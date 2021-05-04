## Disclaimer: `Rsynthpops` is under active development. Feel free to report bugs, suggest edits, or request additional functionality in the [Issues](https://github.com/cmhoove14/Rsynthpops/issues). Advanced users may also fork the repository and submit pull requests to make direct contributions  

# Overview  

`Rsynthpops` collates functionality from a number of existing R packages to streamline the generation of synthetic agents databases. In particular, it relies on the `tidycensus` package to download census and public use microdata surveys (PUMS) data, and the `ipfr` package to implement iterative proportional updating (ipu) for population synthesis aiming to match both household- and person-level attributes in the target population. `Rsynthpops` adds additional functionality and data tables to streamline the processing of input census and pums data and also contributes methods for adding further fidelity to the synthetic population by placing school-aged children in classes nested within schools based on NCES data and by placing workers in offices nested in workplaces based on ___ data.  

# Setup  
`devtools::install_github("cmhoove14/Rsynthpops")`  