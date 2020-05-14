This is a port of [pandoc-url2cite](https://github.com/phiresky/pandoc-url2cite)
to Haskell.

# Example

**test.md:**

```markdown
This is a test [@Hirshburg2016].

[@Hirshburg2016]: https://www.ncbi.nlm.nih.gov/pubmed/27672412
```

```
$ pandoc --filter pandoc-url2cite --filter pandoc-citeproc -t plain test.md 2>/dev/null
This is a test (Hirshburg et al. 2016).

Hirshburg, Jason M., Petra A. Kelsey, Chelsea A. Therrien, A. Carlo
Gavino, and Jason S. Reichenberg. 2016. “Adverse Effects and Safety of
5-Alpha Reductase Inhibitors (Finasteride, Dutasteride): A Systematic
Review.” The Journal of Clinical and Aesthetic Dermatology 9 (7): 56–62.
https://www.ncbi.nlm.nih.gov/pubmed/27672412.
```

# Differences from pandoc-url2cite

- directory for cache is automatically created if it doesn't exist
- no cite-meta attribute is put in converted links
- citation key-url pairs don't have to be in their own paragraph. They can occur
  everywhere as long as they are on their own line.
- `.url2cite` and `.no-url2cite` classes are removed from links
- allows disabling caching by setting `url2cite-cache` to false
- allows changing of the server the citations are fetched from by setting
  `url2cite-api`
- citation keys are not changed to their matching urls
- can be used as a Haskell library
- does not require `pandoc-citeproc` in `PATH`
