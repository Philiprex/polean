# polean

**Complex-Encoded Semi-Boolean Extension of the R Logical Type**

`polean` provides a non-propagating extension of R's `logical` type. In addition to `POL_TRUE`, `POL_FALSE`, and `POL_NA`, it introduces `POL_1` and `POL_0` — values that share *numerical* equivalence with `TRUE` and `FALSE` but not *boolean* equivalence. This lets you distinguish a known-false value from a primary-false value (or analogously for true) without the coercive propagation that `NA` would force.

Internally, each value is represented as a complex number whose real part carries the truth orientation (`0`, `1`, or `NA`) and whose imaginary part records whether the value is strict (`0`) or not strict (`1`).

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

# The five literals are exported as locked package objects.
POL_TRUE
POL_FALSE
POL_1
POL_0
POL_NA

# Construct a polean from a logical-coercible vector.
# Default strict = FALSE produces not-strict values.
x <- polean(c(1, 0, 1, 0), strict = c(TRUE, TRUE, FALSE, FALSE))
x   # POL_TRUE POL_FALSE POL_1 POL_0

# Or coerce, with strictness inferred from the input type.
as.polean(c(TRUE, FALSE, NA))   # POL_TRUE POL_FALSE POL_NA
as.polean(c(1, 0, NA))          # POL_1 POL_0 POL_NA

# Coercion from polean back to other types.
as.numeric(x)    # drops the strictness distinction
as.complex(x)    # exposes the full encoding
as.logical(x)    # primary truth orientation only
as.character(x)  # human-readable labels

# Accessors.
value(x)         # complex encoding
name(x)          # printed labels
orientation(x)   # truth orientation as logical
strict(x)        # strict (TRUE) vs not strict (FALSE)

# Indexing, length, and dim behave as expected for vector and array shapes.
x[1:2]
length(x)
```

## Documentation

After loading the package:

- `?"polean-package"` — package-level overview
- `?polean` — the `polean` class and its constructor
- `?as.polean` — coercion methods
- `?POL_1` — the five exported literals
- `?"polean-accessors"` — `value()`, `name()`, `orientation()`, `strict()`
- `vignette("polean")` — introductory vignette (requires `build_vignettes = TRUE` at install)

## License

MIT. See [LICENSE.md](LICENSE.md).

## Citation

If you use this package in academic work, please cite:

> Eigen, P. (2026). *polean: Complex-Encoded Semi-Boolean Extension of the R Logical Type*. R package version 0.9.0. https://github.com/Philiprex/polean
>
> A `CITATION` file is included in the package; running `citation("polean")` after installation will produce the same reference.
