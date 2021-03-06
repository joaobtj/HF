
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hf: an R package to calculate head loss in pipelines

<!-- badges: start -->
<!-- badges: end -->

The head loss in pipes has been extensively studied and its precise
calculation is of great importance in hydraulic projects.

In the design and analysis of piping, the three major pipe-flow problems
are the determination of pipe diameter, flow rate and head loss for a
given set of known variables.

The pressure loss due to friction of a liquid flowing in a pipe is
usually calculated by the Universal equation. The friction factor (f)
depends on the Reynolds number (Re) and the relative roughness of the
tube.

For laminar flow (Re &lt; 2000), the friction factor is easily
calculated by a linear relationship with the Reynolds number.

For turbulent flow (Re &gt; 4000), the friction factor can be determined
by the Colebrook-White equation. However, the solution of this equation
requires the use of iterative methods.
<!-- Alternatively, explicit equations can be used for calculating friction factor. There are also found a variety of empirical equations.  -->

Three functions have been implemented covering the three major cases:
pressure drop calculation (`head_loss` function), flow rate or speed
calculation (`flow_rate` function) and diameter calculation (`diameter`
function).

<!-- All problems can be solved with the various equations implemented: Colebrook-White, Swamee-Jain, Blasius, Hazen-Williams, Flamant, among others. -->

## Installation

<!-- You can install the released version of hf from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("hf") -->
<!-- ``` -->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
if (!require(devtools)) {
    install.packages("devtools")
}
devtools::install_github("joaobtj/hf")
```

## Example

These are basic examples showing how to use the functions:

``` r
# load the hf package
library(hf)

# Calculate head loss for laminar flow
head_loss(D = 0.025, Q = 0.000000001, L = 200)
# Calculate head loss for turbulent flow
head_loss(D = 0.050, Q = 0.0006, L = 200, RC = 0.0001)

# Calculate flow rate for laminar flow
flow_rate(hf = 0.97, L = 10, D = 0.008, v = 3e-6)
# Calculate flow rate for turbulent flow
flow_rate(hf = 9.7, L = 10, D = 0.050, RC = 0.0001)

# Calculate diameter for laminar flow
diameter(hf = 19.7, L = 100, Q = 0.0000005)
# Calculate diameter for turbulent flow
diameter(hf = 9.7, L = 10, Q = 0.005, RC = 0.0001)
```
