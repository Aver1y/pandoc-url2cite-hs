-- default language extensions set in .cabal
module Text.Pandoc.Url2Cite
  (url2cite, url2citeWith,
   -- * Configuration
   LinkOutput(..), AllLinks(..), Cache(..), API(..), AllowDangling(..),
   LogAction(..),
   Configuration(..), configFromMeta,
   linkOutput, allLinks, cache, api, logAction, allowDangling,
   def,
   -- * Internal
   transformAndInsertCSLs, transformAndGetCSLs,
   getCSLsCached, getCSLForURL,
   transformAST, InvalidReferenceFieldException(..)) where

import           Relude -- Alternative prelude
import qualified Relude.Unsafe as U
import           Control.Lens
import           Data.Traversable (for)
import qualified Data.Semigroup as SG

import qualified Data.Map.Strict as M
import qualified Data.Text.IO as T
import qualified Data.Text as T
import           Data.Default
import qualified Data.Typeable as Ty
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Text.Pandoc.Definition
import           Text.Pandoc.Builder (setMeta)
import           Text.Pandoc.Walk
import qualified Text.CSL as CSL
import qualified Text.CSL.Reference as CSL
import qualified Text.CSL.Input.Bibutils as CSL
import qualified Network.HTTP.Req as R
import qualified Data.Aeson.Text as J
import qualified Data.Aeson as J
import qualified Text.URI as URI
import qualified System.Directory as Dir
import qualified System.FilePath as Dir
import           Control.Exception (throwIO)

-- | How to display links converted to citations
data LinkOutput =
    CiteOnly -- ^ [text](href) becomes [@href]
  | Sup      -- ^ [text](href) becomes [text](href)^[@href]
  | Normal   -- ^ [text](href) becomes [text [@href]](href)
  deriving (Show, Eq)
instance Default LinkOutput where def = Normal

data AllLinks =
    AllLinks     -- ^ Convert all links to citations
  | CitationOnly -- ^ Don't convert links unless indicated with {.url-cite}
  deriving (Show, Eq)
instance Default AllLinks where def = CitationOnly

data Cache = NoCache | Cache FilePath
  deriving (Show, Eq)
instance Default Cache where def = Cache "citation-cache.json"

newtype LogAction = LogAction (Text -> IO ())
instance Default LogAction where def = LogAction $ T.hPutStrLn stderr

data API = forall scheme . Typeable scheme => API (R.Url scheme)
instance Default API where
  def = API $ R.https "en.wikipedia.org" R./: "api" R./: "rest_v1" R./: "data"
              R./: "citation" R./: "bibtex"

-- | Whether to allow citations without an accompanying url
data AllowDangling = AllowDangling | WarnDangling | ErrDangling
  deriving (Eq, Show)
instance Default AllowDangling where def = ErrDangling

deriving instance Show API
instance Eq API where
  API (api1 :: R.Url scheme1) == API (api2 :: R.Url scheme2) =
    case Ty.eqT @scheme1 @scheme2 of
      Nothing -> False
      Just Refl -> api1 == api2

data Configuration = Configuration {
    -- | How to display links converted to references /(def: 'Normal')/
    _linkOutput :: LinkOutput
    -- | Whether to convert all links to references /(def: @False@)/
  , _allLinks :: AllLinks
    -- | Cache fetched bibliography entries /(def: @"citation-cache.json"@)/
  , _cache :: Cache
    -- | API server
    --   /def: @"https://en.wikipedia.org/api/rest_v1/data/citation/bibtex"@/
  , _api :: API
    -- | How to log progress /def: @'T.putStrLn' stderr@/
  , _logAction :: LogAction
    -- | Whether to allow citations without an accompanying url
    --   /def: ErrDangling/
  , _allowDangling :: AllowDangling
  }
  deriving (Generic, Default)
makeLenses ''Configuration

pattern MetaString' :: Text -> MetaValue
pattern MetaString' str <- (metaString -> Just str)

-- | Convert MetaStrings and MetaInlines values which are just plain strings to
--   strings
metaString :: MetaValue -> Maybe Text
metaString (MetaString str) = Just str
metaString (MetaInlines inline) = T.concat <$> for inline \case
  Str str -> Just str
  Space -> Just " "
  _ -> Nothing
metaString _ = Nothing

-- | Get 'Configuration' from a 'Meta' and 'Format'.
--
-- [url2cite]: "all-links" | "citation-only"
-- [url2cite-link-output]: "cite-only" | "sup" | "normal"
-- [url2cite-cache]: false | "<filepath>"
-- [url2cite-allow-dangling-citations]: true | false | "allow" | "warn" | "err"
-- [url2cite-api]: "<url>"
-- [url2cite-log]: true | false
configFromMeta :: Maybe Format -> Meta -> Either Text Configuration
configFromMeta format meta =
  pure case format of
         Just (Format "html") -> def & linkOutput .~ Sup
         _ -> def
  <**> fromKey "url2cite" allLinks \case
    MetaString' "all-links" -> Just AllLinks
    MetaString' "citation-only" -> Just CitationOnly
    _ -> Nothing
  <**> fromKey "url2cite-link-output" linkOutput \case
    MetaString' "cite-only" -> Just CiteOnly
    MetaString' "sup" -> Just Sup
    MetaString' "normal" -> Just Normal
    _ -> Nothing
  <**> fromKey "url2cite-cache" cache \case
    MetaString' str -> Just (Cache $ toString str)
    MetaBool False  -> Just NoCache
    _ -> Nothing
  <**> fromKey "url2cite-api" api \case
    MetaString' urlStr
      | Just uri <- URI.mkURI urlStr, Just eurl <- R.useURI uri -> case eurl of
        Left  (url, _) -> Just $ API url
        Right (url, _) -> Just $ API url
    _ -> Nothing
  <**> fromKey "url2cite-log" logAction \case
    MetaBool False -> Just $ LogAction (const pass)
    MetaBool True -> Just def
    _ -> Nothing
  <**> fromKey "url2cite-allow-dangling-citations" allowDangling \case
    MetaBool True  -> Just AllowDangling
    MetaBool False -> Just ErrDangling
    MetaString' "allow" -> Just AllowDangling
    MetaString' "warn" -> Just WarnDangling
    MetaString' "err" -> Just ErrDangling
    _ -> Nothing
  where
    fromKey ::
      Text -> ASetter' s x -> (MetaValue -> Maybe x) -> Either Text (s -> s)
    fromKey key setter val =
      case lookupMeta key meta of
        Nothing -> Right id
        Just mval ->
          maybe (Left $ "invalid value for " <> key) (Right . set setter)
                (val mval)

-- | Walks over the AST and collects all definition of citation keys
transformAST :: Walkable [Inline] p =>
     AllLinks
  -> LinkOutput
  -> p
  -> ([(Text, Text, Bool)], [Text], p)
  -- ^ (defined citation url pairs, used citations, transformed AST)
  -- The Bool signifies whether the pair was explicitly defined or implicitly by
  -- a converted link
transformAST allLinks linkOutput p = walkM inline p
  where
    inline :: [Inline] -> ([(Text, Text, Bool)], [Text], [Inline])
    inline = fmap unlines . traverse line . toList . lines

    -- This is called for every line
    line :: [Inline] -> ([(Text, Text, Bool)], [Text], [Inline])
    -- A citation key definition
    line (removeSpaces -> [Cite [Citation{citationId}] _, Str ":", Str url]) =
      ([(citationId, url, True)], [], [])

    line r = concat <$> for r \case
      -- A citation, add it to the list of used citations
      c@(Cite cs _) -> ([], citationId <$> cs, [c])
      -- A link convert according to settings
      Link (lid, classes, kv) inline (url, title)
        | (mx, classes') <- filterClasses classes
        , fromMaybe (case allLinks of AllLinks -> True; _ -> False) mx
        , isExternal url ->
          let
            newLink = case linkOutput of
              CiteOnly -> [cite]
              Sup -> [link inline, Superscript [cite]]
              Normal -> [link $ inline <> [Space, cite]]
            link inline = Link (lid, classes', kv) inline (url, title)
            cite = Cite [Citation{
              citationId = url,
              citationMode = NormalCitation,
              citationNoteNum = 0,
              citationPrefix = [],
              citationSuffix = [],
              citationHash = 0
              }] []
          in ([(url, url, False)], [], newLink)
      r' -> pure [r']

    isExternal :: Text -> Bool
    isExternal url = case URI.mkURI url of
      Just (URI.uriAuthority -> Right _) -> True
      _ -> False

    -- Look through classes and see if there are any url2cite or no-url2cite
    -- classes and also filter them out
    filterClasses = (first . fmap) getAll . foldMap \case
      "url2cite"    -> (Just $ All True,  [])
      "no-url2cite" -> (Just $ All False, [])
      x             -> (Nothing,          [x])

    removeSpaces = filter \case Space -> False; _ -> True

    lines (SoftBreak:xs) = ([]:|[]) <> lines xs
    lines (x:xs) = case lines xs of
      (l :| ls) -> (x:l) :| ls
    lines [] = [] :| []

    unlines = intercalate [SoftBreak]

getCSLsCached :: API -> Cache -> LogAction -> HashMap Text Text
  -> IO [CSL.Reference]
getCSLsCached api cache (LogAction log) urls = do
  refs <- case cache of
    NoCache -> pure []
    Cache cachePath ->
      ifM (Dir.doesFileExist cachePath)
        do log "Found cache"
           CSL.readBiblioFile (\key -> has (ix key) urls) cachePath
        do log "Cache does not exist yet"
           pure []
  let urlsNotCached =
       foldl' (\hm ref -> hm & sans (CSL.unLiteral . CSL.refId $ ref))
         urls refs
  missingRefs <- R.runReq R.defaultHttpConfig $
    ifor urlsNotCached \citekey url -> do
      liftIO . log $ "Fetching bib entry for " <> url
      getCSLForURLWithKey api citekey url
  let allRefs = refs <> toList missingRefs
  case cache of
    NoCache -> pure ()
    Cache cachePath -> do
      log "Creating cache"
      Dir.createDirectoryIfMissing True $ Dir.takeDirectory cachePath
      writeFileLText cachePath $ J.encodeToLazyText allRefs
  pure $ allRefs

getCSLForURLWithKey :: API -> Text -> Text -> R.Req CSL.Reference
getCSLForURLWithKey api key url =
  getCSLForURL api url <&> \r -> r{CSL.refId = CSL.Literal key}

getCSLForURL :: API -> Text -> R.Req CSL.Reference
getCSLForURL (API api) url =
  liftIO . fmap U.head . CSL.readBiblioString (const True) CSL.Bibtex .
  decodeUtf8 . R.responseBody
  =<< R.req R.GET (api R./: url) R.NoReqBody R.bsResponse mempty

transformAndGetCSLs :: Walkable [Inline] pandoc 
  => (AllLinks, LinkOutput, LogAction, API, Cache)
  -> pandoc
  -> IO (HashSet Text, HashSet Text, pandoc, [CSL.Reference])
transformAndGetCSLs (allLinks, linkOutput, logAction, api, cache) pandoc = do
  csls <- getCSLsCached api cache logAction (fst <$> refs')
  pure (duplicates, dangling, pandoc', csls)
  where
    (refs, (HS.fromList -> cites), pandoc')
      = transformAST allLinks linkOutput pandoc
    refs' :: HashMap Text (Text, Bool)
    refs' =
      fmap (bimap SG.getLast (>1)) . HM.fromListWith (<>)
        $ (\(k,x,e) -> (k,(SG.Last x,if e then Sum 1 else Sum @Int 0))) <$> refs
    duplicates = HM.keysSet $ HM.filter snd refs'
    dangling = cites `HS.difference` HM.keysSet refs'

data InvalidReferenceFieldException = InvalidReferenceFieldException String
  deriving (Show, Exception)

transformAndInsertCSLs ::
  (AllLinks, LinkOutput, LogAction, API, Cache)
  -> Pandoc -> IO (HashSet Text, HashSet Text, Pandoc)
  -- ^ (duplicate urls, dangling urls, result)
transformAndInsertCSLs conf pandoc = do
  (duplicates, dangling, Pandoc meta content, csls)
    <- transformAndGetCSLs conf pandoc
  newRefs <- 
    refToMeta <<$>>
    case CSL.convertRefs (lookupMeta "references" meta) of
      Right existingCsls -> pure $ existingCsls <> toList csls
      Left err -> throwIO $ InvalidReferenceFieldException err
  pure
    (duplicates, dangling, Pandoc (setMeta "references" newRefs meta) content)

refToMeta :: CSL.Reference -> MetaValue
refToMeta ref = valToMeta . J.toJSON $ ref
  where
    valToMeta :: J.Value -> MetaValue
    valToMeta val = case val of
      J.Object obj  -> MetaMap $ M.fromList . HM.toList $ valToMeta <$> obj
      J.Array  vect -> MetaList $ valToMeta <$> toList vect
      J.String text -> MetaString text
      J.Number num  -> MetaString . toText $ J.encodeToLazyText num
      J.Bool   bool -> MetaBool bool
      J.Null        -> MetaString ""

url2citeWith :: Configuration -> Pandoc -> IO Pandoc
url2citeWith Configuration{..} pandoc = do
  (duplicates, dangling, pandoc') <-
    transformAndInsertCSLs (_allLinks,_linkOutput,_logAction,_api,_cache) pandoc
  unless (has _Empty duplicates) $
    log $ "Duplicate citation definitions: "
      <> T.intercalate ", " (toList duplicates)
  case (dangling, _allowDangling) of
    (Empty, _) -> pass
    (_, AllowDangling) -> pass
    _ -> do log $ "Dangling citations: " <> T.intercalate ", " (toList dangling)
            when (_allowDangling == ErrDangling) $ exitFailure
  pure pandoc'
  where LogAction log = _logAction

url2cite :: Maybe Format -> Pandoc -> IO Pandoc
url2cite format pandoc@(Pandoc meta _) = case (configFromMeta format meta) of
  Left err -> T.hPutStrLn stderr err *> exitFailure
  Right conf -> url2citeWith conf pandoc
