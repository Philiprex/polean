# polean

**Complex-Encoded Semi-Boolean Extension of the R Logical Type**

`polean` provides a non-propagating extension of R's `logical` type. In addition to `TRUE`, `FALSE`, and `NA`, it introduces `ISH(1)` and `ISH(0)` — values that share *numerical* equivalence with `TRUE` and `FALSE` but not *boolean* equivalence. This lets you distinguish a known-false value from a primary-false value (or analogously for true) without the coercive propagation that `NA` would force.

Internally, each value is represented as a complex number whose real part carries the truth orientation (`0` or `1`) and whose imaginary part records whether the value is strict (`0`) or ish (`1`).

## Installation

```r
# install.packages("devtools")
devtools::install_github("Philiprex/polean")

# To also build the introductory vignette (`vignette("polean")`):
devtools::install_github("Philiprex/polean", build_vignettes = TRUE)
```

## Basic usage

```r
library(polean)

# Construct a polean from a logical-coercible vector.
x <- polean(c(1, 0, 1, 0), treat_logical = c(TRUE, TRUE, FALSE, FALSE))
x

# Or build a length-1 ISH literal directly.
ISH(1)
ISH(0)
ISH(NA)

# Coercion to numeric, complex, logical, or character.
as.numeric(x)    # drops the strict/ish distinction
as.complex(x)    # exposes the full encoding
as.logical(x)    # primary truth orientation only
as.character(x)  # human-readable labels (TRUE/FALSE/ISH(0)/ISH(1))

# Indexing, length, and dim behave as expected for vector and array shapes.
x[1:2]
length(x)
```

## License

MIT. See [LICENSE.md](LICENSE.md).

## Citation

If you use this package in academic work, please cite:

> Eigen, P. (2026). *polean: Complex-Encoded Semi-Boolean Extension of the R Logical Type*. R package version 0.9.0. https://github.com/Philiprex/polean
>
> A `CITATION` file is included in the package; running `citation("polean")` after installation will produce the same reference.