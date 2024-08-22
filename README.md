# R Projekt SS 2024 - Thema 6 - Clustering

## Development flow

- Load current package with `devtools::load_all()` or `Ctrl-Shift-L` this loads the package into the current R-Session. AFTERWARDS test the written functions in the console. Don't add `library` and `source` calls to the Codebase. Maybe this [cheat-sheet](https://raw.githubusercontent.com/rstudio/cheatsheets/main/package-development.pdf) is helpful.

## TODO 

- [x] Remove all `requirements` and `library` calls. 
- [x] Remove all `source` calls. 
- [x] Check that the packages lodes, atm something like this happens
  ```
  Error in `load_all()`:
  ! Failed to load R/optics.R
  Caused by error:
  ! object 'irisPetals' not found
  ```
- [ ] Make sure all methods use the same data in- and output format
- [ ] Make sure all`@exported`¸ methods have helpful roxygen comments
- [ ] Make sure all Tests pass successfully
- [ ] Implement hierarchical clustering
- [ ] Replace magrittr pipe calls where possible with native ones
- [ ] Add some helpful `stops`

### At the end

- [ ] Remove all commented code; this is bad style
- [ ] Build package release
- [ ] Check `DESCRIPTION` file: Add authors etc.

### Before each release

- [ ] Check documentation is up to date generate with `devtools::document()`
- [ ] Run tests with `devtools::test()` or `Ctrl-Shift-T`

## Literatur

- [Richter](https://link.springer.com/book/10.1007/978-3-662-59354-7)
- [James](https://link.springer.com/book/10.1007/978-1-0716-1418-1)
