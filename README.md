# Screenmill.grid R package

This package contains helper functions for the _Screenmill_ R package. _Screenmill_ is designed to 
quantify colony growth experiments for Synthetic Genetic Arrays (SGA), Epistatic Mini-Array Profiling (E-MAP), Selective Ploidy Ablation (SPA), or a simple quantitative colony fitness assay. The Screenmill.grid package provides functions for the manipulation of image crop coordinates and grid placements for images that fail automatic processing.

## Installation

This package can be installed from GitHub by following the instructions below.

**Step 1**: Install [R](https://cloud.r-project.org) (>= 3.3.0 recommended).

**Step 2**: Install R package developer tools. Why? This package contains some Rcpp code that must be compiled, so you will need a C++ compiler (e.g. [GCC](https://gcc.gnu.org), or [clang](http://clang.llvm.org)). For more help, checkout this guide for R's [package development prerequisites](https://support.rstudio.com/hc/en-us/articles/200486498-Package-Development-Prerequisites).

- **Mac OS X**: Install [Xcode](https://developer.apple.com/xcode/) developer tools (in terminal: `xcode-select --install`).
- **Windows**: Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/).
- **Debian/Ubuntu**: Install the build-essential package in shell with: `sudo apt-get install build-essential`

**Step 3 (*optional*)**: Install [RStudio](https://www.rstudio.com). Why? RStudio makes programming in R fun!

**Step 4**: Install _Screenmill_ from the [Github page](https://github.com/EricEdwardBryant/screenmill).

**Step 5**: Install the package by running the following commands in R

```r
# Install the latest version of screenmill.grid
devtools::install_github('RobertJDReid/screenmill.grid')
```

# Usage

# Features

