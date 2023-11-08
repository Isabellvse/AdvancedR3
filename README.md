---
editor_options:
  markdown:
    wrap: 72
    canonical: true
---

# Title: Reproducible Research in R Advanced

# AdvancedR3:

This project is used to set up an easy and collaborative workflow in R.

# Brief description of folder and file contents

The following folders contain:

-   `data/`: Clean lipidomics data
-   `doc/`: Learning files
-   `R/`: Functions we use in this project

# Installing project R package dependencies

If dependencies have been managed by using
`usethis::use_package("packagename")` through the `DESCRIPTION` file,
installing dependencies is as easy as opening the `AdvancedR3.Rproj`
file and running this command in the console:

```         
# install.packages("remotes")
remotes::install_deps()
```

You'll need to have remotes installed for this to work.

# Resource

For more information on this folder and file workflow and setup, check
out the [prodigenr](https://rostools.github.io/prodigenr) online
documentation.
