# Quantifying Sources of Estimation Bias in Surplus Production Models Using Sequential Data Replacement Framework

This repository contains the R code used for the analyses presented in:

> *Quantifying Sources of Estimation Bias in Surplus Production Models Using Sequential Data Replacement Framework in Real-World Fisheries*

The study uses a sequential data replacement framework to quantify how different sources of information contribute to estimation bias in surplus production models.

## Repository structure

```text
.
├── README.md
├── LICENSE
└── src/
    ├── functions.r
    ├── run_simulation.r
    └── plot.r
```

* `src/functions.r`
  Functions used for the simulation and analysis.

* `src/run_simulation.r`
  Main script for running the simulation and sequential data replacement analyses.

* `src/plot.r`
  Script for generating a representative figure from the analysis results.

## Requirements

The analyses were conducted in **R**.

The required R packages are specified in the analysis scripts.

## Reproducibility

The main analyses presented in the manuscript can be reproduced using the code provided in this repository.

From the repository root directory, run:

```r
source("./src/run_simulation.r")
```

The required functions are loaded automatically by `run_simulation.r`.

The plotting script can be run separately after the analysis:

```r
source("./src/plot.r")
```

## Data availability

The original data used in this study are not included in this repository.

The data are available from the following source:

* [Data source](https://abchan.fra.go.jp/hyouka/)

Please refer to the data source for information on data access and usage.

## Baseline Data Generation

A reference stock is generated from:

・VPA output (frasyr::vpa)

・Stock-recruitment relationship

・Historical exploitation patterns

## Citation

If you use the code or framework provided in this repository, please cite the associated publication:

> Citation information will be added after publication.

## License

This repository is distributed under the MIT License. See [`LICENSE`](LICENSE) for details.


