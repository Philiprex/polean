#' polean: Complex-Encoded Semi-Boolean Extension of the R Logical Type
#'
#' A non-propagating extension of R's logical type. In addition to
#' \code{POL_TRUE}, \code{POL_FALSE}, and \code{POL_NA}, polean adds
#' two further literals (\code{POL_1} and \code{POL_0}): values that
#' share numerical equivalence with \code{TRUE} and \code{FALSE} but
#' not boolean equivalence. Each value is stored internally as a
#' complex number whose real part carries the truth orientation and
#' whose imaginary part records whether the value is strict
#' (\code{0}) or not (\code{1}).
#'
#' @keywords internal
#' @aliases polean-package
#' @import methods
"_PACKAGE"

# --- Setup
setClassUnion("complexLike", c("complex", "array"))
setClassUnion("characterLike", c("character", "array"))

#' polean class and constructor
#'
#' S4 class for non-propagating, semi-boolean values. A polean takes
#' one of five literal values: \code{POL_TRUE}, \code{POL_FALSE},
#' \code{POL_1}, \code{POL_0}, or \code{POL_NA}. The literals are
#' also available as locked length-1 package objects (see
#' \code{\link{polean-literals}}).
#'
#' @details
#' Internally each element is a complex number. The real part is the
#' orientation (\code{0L}, \code{1L}, or \code{NA_integer_}). The
#' imaginary part is the strictness flag (\code{0L} for strict,
#' \code{1L} for not strict). The \code{name} slot mirrors the shape of
#' \code{value} and holds the printed label.
#'
#' \code{polean()} is the user-facing constructor. The
#' \code{orientation} argument is parsed by \code{as.logical}, so any
#' input for which \code{as.logical(x)} yields \code{TRUE},
#' \code{FALSE}, or \code{NA} is accepted. The \code{strict} argument
#' is taken at face value: \code{TRUE} produces a strict polean
#' (\code{POL_TRUE}/\code{POL_FALSE}), \code{FALSE} produces not strict
#' (\code{POL_1}/\code{POL_0}); the default is \code{FALSE} (not strict).
#' When \code{dim} is supplied, both arguments are recycled to
#' \code{prod(dim)} and the result is shaped to that dimension.
#'
#' For coercion that uses the R type of the input to decide
#' strictness, see \code{\link{as.polean}}.
#'
#' @slot value A \code{complexLike} (complex vector or complex array).
#' @slot name A \code{characterLike} (character vector or character
#'   array) matching the shape of \code{value}. Elements are drawn
#'   from \code{c("POL_TRUE", "POL_FALSE", "POL_1", "POL_0",
#'   "POL_NA")}.
#'
#' @param orientation Anything for which \code{as.logical} yields one
#'   or more of \code{TRUE}, \code{FALSE}, \code{NA}. Length must be
#'   \eqn{\ge 1}.
#' @param strict Anything for which \code{as.logical} yields one
#'   or more of \code{TRUE}, \code{FALSE}, \code{NA}. Length must be
#'   \eqn{\ge 1}, and at least one value must not be \code{NA}. Default \code{FALSE}.
#' @param dim Integer vector of array dimensions, or \code{NULL}. When
#'   non-null, \code{orientation} and \code{strict} are recycled to
#'   \code{prod(dim)}.
#'
#' @return An object of class \code{polean}.
#'
#' @examples
#' polean(TRUE)                       # POL_1   (strict = FALSE by default)
#' polean(TRUE, strict = TRUE)        # POL_TRUE
#' polean(1, strict = TRUE)           # POL_TRUE
#' polean(c(1, 0, NA))                # POL_1 POL_0 POL_NA
#' polean(1, dim = c(2, 3))           # 2x3 array of POL_1
#' polean(c(0, 1), dim = c(2, 3))
#'
#' @name polean
#' @aliases polean-class
#' @seealso \code{\link{as.polean}}, \code{\link{polean-literals}}
#' @export
setClass("polean", slots = list(value="complexLike", name="characterLike"), prototype = list(value=1+1i, name="POL_1"))

setMethod("initialize", "polean", function(.Object, orientation, strict, dim=NULL, ...){
  .Object = callNextMethod(.Object, ...)

  if (is.null(orientation)){
    stop('`orientation` must not be null')
  } else if (!is(orientation, "integer") || length(orientation)==0){
    stop("`orientation` must be an integer with length > 0")
  } else if (!all(orientation %in% c(0,1,NA_integer_))){
    stop("Values of `orientation` must be 0L, 1L or NA_integer_")
  } else if (is.null(strict)){
    stop("`strict` must not be NULL")
  } else if (!is(strict, "integer") || length(strict)==0){
    stop("`strict` must be integer with length > 0")
  } else if (!all(strict %in% c(0,1,NA_integer_))){
    stop("Values of `strict` must be 0L, 1L or NA_integer_")
  } else if (all(is.na(strict))){
    stop("At least one value of `strict` must not be NA")
  } else if (!is.null(dim) & length(orientation)!=prod(dim)){
    warning(paste0("Got input length ", length(orientation), " but dim=c(", paste(dim, collapse=","), "). Input will be recycled to fill dim."), call.=F)
  }

  if (!is.null(dim)){
    n = prod(dim)
    orientation = rep_len(orientation, n)
    strict = rep_len(strict, n)
  }
  .Object@value = complex(real=orientation, imaginary=strict)
  .Object@name = as.character(sapply(.Object@value, function(e) ifelse(is.na(e), "POL_NA", paste0("POL_", ifelse(Im(e)==1, Re(e), as.logical(Re(e)))))))
  if (!is.null(dim)){
    dim(.Object@value) = dim
    dim(.Object@name) = dim
  }

  .Object

})

#' @rdname polean
#' @export
polean = function(orientation, strict=FALSE, dim=NULL){
  orientation=as.integer(sapply(orientation, as.logical))
  strict=as.integer(!sapply(strict, as.logical))
  new("polean",
      orientation=orientation,
      strict=strict,
      dim=dim)
}

# --- Accessors

#' polean accessors
#'
#' Read-only accessors for the slots and derived attributes of a
#' polean.
#'
#' \describe{
#'   \item{\code{value(x)}}{The underlying complex value (slot
#'     \code{@@value}).}
#'   \item{\code{name(x)}}{The printed label (slot \code{@@name}), one
#'     of \code{"POL_TRUE"}, \code{"POL_FALSE"}, \code{"POL_1"},
#'     \code{"POL_0"}, \code{"POL_NA"}.}
#'   \item{\code{orientation(x)}}{The truth orientation as logical:
#'     \code{TRUE} for \code{POL_1}/\code{POL_TRUE}, \code{FALSE} for
#'     \code{POL_0}/\code{POL_FALSE}, \code{NA} for \code{POL_NA}.}
#'   \item{\code{strict(x)}}{\code{TRUE} for strict
#'     (\code{POL_TRUE}/\code{POL_FALSE}), \code{FALSE} for not strict
#'     (\code{POL_1}/\code{POL_0}). For \code{POL_NA} elements the
#'     return value is implementation-defined; treat it as
#'     undefined.}
#' }
#'
#' @param x A polean.
#' @return A vector or array matching the shape of \code{x}; element
#'   type depends on the accessor.
#'
#' @name polean-accessors
NULL

#' @rdname polean-accessors
#' @export
setGeneric("value", function(x) standardGeneric("value"))
setMethod("value", "polean", function(x) x@value)

#' @rdname polean-accessors
#' @export
setGeneric("name", function(x) standardGeneric("name"))
setMethod("name", "polean", function(x) x@name)

#' @rdname polean-accessors
#' @export
setGeneric("orientation", function(x) standardGeneric("orientation"))
setMethod("orientation", "polean", function(x) as.logical(Re(x@value)))

#' @rdname polean-accessors
#' @export
setGeneric("strict", function(x) standardGeneric("strict"))
setMethod("strict", "polean", function(x) !as.logical(Im(x@value)))

# --- Show
setMethod("show", "polean", function(object) {
  print(object@name, quote=F)
})

# --- Literals

#' polean literals
#'
#' The five locked length-1 literals exported by the package.
#'
#' \describe{
#'   \item{\code{POL_1}}{Orientation true, not strict (encoding \code{1+1i}).}
#'   \item{\code{POL_0}}{Orientation false, not strict (encoding \code{0+1i}).}
#'   \item{\code{POL_TRUE}}{Orientation true, strict (encoding \code{1+0i}).}
#'   \item{\code{POL_FALSE}}{Orientation false, strict (encoding \code{0+0i}).}
#'   \item{\code{POL_NA}}{NA orientation. \code{is.na(POL_NA)} is \code{TRUE}.}
#' }
#'
#' Each literal is assigned in the package namespace at load time and
#' has its binding locked. To obtain a multi-element polean, use
#' \code{\link{polean}} or \code{\link{as.polean}}, or concatenate
#' literals with \code{c()}.
#'
#' @section Numerical and boolean equivalence:
#'
#' The literals participate in two distinct equality regimes, chosen
#' by the R type on the other side of \code{==}:
#'
#' \describe{
#'   \item{Numerical (other side is \code{numeric} / \code{integer})}{
#'     Only the orientation (the real part of the encoding) is
#'     compared. Strict and not-strict literals with the same
#'     orientation are both numerically equal to the corresponding
#'     number.}
#'   \item{Boolean (other side is \code{logical}, \code{complex}, or
#'     another \code{polean})}{The full complex encoding is compared,
#'     so both orientation and strictness must agree.}
#' }
#'
#' Examples:
#' \preformatted{
#'   # Numerical: orientation only
#'   POL_TRUE  == 1        # TRUE
#'   POL_1     == 1        # TRUE
#'   POL_FALSE == 0        # TRUE
#'   POL_0     == 0        # TRUE
#'   POL_NA    == 1        # NA
#'
#'   # Boolean: orientation and strictness must both match
#'   POL_TRUE  == TRUE     # TRUE     (1+0i == 1+0i)
#'   POL_1     == TRUE     # FALSE    (1+1i != 1+0i)
#'   POL_FALSE == FALSE    # TRUE     (0+0i == 0+0i)
#'   POL_0     == FALSE    # FALSE    (0+1i != 0+0i)
#'   POL_TRUE  == POL_1    # FALSE    (1+0i != 1+1i)
#'   POL_FALSE == POL_0    # FALSE    (0+0i != 0+1i)
#'   POL_NA    == POL_NA   # NA
#' }
#'
#' The asymmetry preserves the invariant
#' \code{as.polean(x) == x} for numeric \code{x} (since
#' \code{as.polean()} on numeric input produces a not-strict polean,
#' and numerical equivalence ignores strictness), while still letting
#' callers distinguish strict from not-strict whenever a polean is on
#' both sides of \code{==} or the other side is logical or complex.
#'
#' @format A length-1 \code{\link{polean}}.
#' @name polean-literals
#' @aliases POL_1 POL_0 POL_TRUE POL_FALSE POL_NA pol_literals
#' @seealso \code{\link{polean}}, \code{\link{as.polean}}
#' @export POL_1
#' @export POL_0
#' @export POL_TRUE
#' @export POL_FALSE
#' @export POL_NA
NULL

POL_1     <- new("polean", 1L, 1L)
POL_0     <- new("polean", 0L, 1L)
POL_TRUE  <- new("polean", 1L, 0L)
POL_FALSE <- new("polean", 0L, 0L)
POL_NA    <- new("polean", NA_integer_, 0L)

.onLoad = function(libname, pkgname){
  ns = asNamespace(pkgname)
  lockBinding("POL_1", ns)
  lockBinding("POL_0", ns)
  lockBinding("POL_TRUE", ns)
  lockBinding("POL_FALSE", ns)
  lockBinding("POL_NA", ns)
  invisible()
}
.onUnload = function(libname, pkgname){
  ns = asNamespace(pkgname)
  unlockBinding("POL_1", ns)
  unlockBinding("POL_0", ns)
  unlockBinding("POL_TRUE", ns)
  unlockBinding("POL_FALSE", ns)
  unlockBinding("POL_NA", ns)
  invisible()
}

# --- Length, Which, Indexing, Dims
setMethod("length", "polean", function(x) length(x@value))
setMethod("which", "polean", function(x) callNextMethod(as.logical(x)))

setMethod("[", "polean", function(x, i, j, ..., drop=TRUE){
  Nargs = nargs() - !missing(drop)

  if (Nargs <= 2L){
    sliced = x@value[i]
  } else {
    sliced = x@value[i, j, ..., drop=drop]
  }
  polean(Re(sliced), !Im(sliced), dim=dim(sliced))
})
setMethod("[<-", "polean", function(x, i, j, ..., value){
  if (!is(value, "polean")) value = polean(value)
  Nargs = nargs() - 1L

  if (Nargs <= 2L){
    x@value[i] = value@value
    x@name[i] = value@name
  } else {
    x@value[i,j,...] = value@value
    x@name[i,j,...] = value@name
  }
  x
})

setMethod("dim", "polean", function(x) dim(x@value))
setMethod("dim<-", "polean", function(x, value){
  if (!is.null(value) & prod(value) !=length(x@value)){
    stop(paste("dims", prod(value), "do not match length of object", length(x@value)))
  }
  dim(x@value) = value
  dim(x@name) = value
  x
})

# --- Coerce from Polean
setMethod("as.integer", "polean", function(x) Re(x@value))
setMethod("as.numeric", "polean", function(x) Re(x@value))
setMethod("as.complex", "polean", function(x) x@value)
setMethod("as.character", "polean", function(x) x@name)
setMethod("as.logical", "polean", function(x) Re(x@value)==1L)

# --- Coerce to Polean
setGeneric("getOrientation", function(x) standardGeneric("getOrientation"))

setMethod("getOrientation", "ANY", function(x) as.integer(as.logical(x)))
setMethod("getOrientation", "complex", function(x) as.integer(as.logical(Re(x))))
setMethod("getOrientation", "character", function(x) as.integer(sapply(x, function(e) ifelse(e %in% c("T", "TRUE", "F", "FALSE"), as.logical(e), ifelse(e==as.character(as.complex(e)), getOrientation(as.complex(e)), getOrientation(as.numeric(e)))))))
setMethod("getOrientation", "logical", function(x) as.integer(x))
setMethod("getOrientation", "polean", function(x) as.integer(Re(x@value)))

setGeneric("getStrict", function(x) standardGeneric("getStrict"))

setMethod("getStrict", "ANY", function(x) 1L)
setMethod("getStrict", "complex", function(x) as.integer(as.logical(Im(x))))
setMethod("getStrict", "character", function(x) as.integer(sapply(x, function(e) ifelse(e %in% c("T", "TRUE", "F", "FALSE"), 0L, ifelse(e==as.character(as.complex(e)), getStrict(as.complex(e)), getStrict(as.numeric(e)))))))
setMethod("getStrict", "logical", function(x) 0L)
setMethod("getStrict", "polean", function(x) as.integer(Im(x@value)))

#' Coerce an object to a polean
#'
#' Type-dispatched coercion to \code{\link{polean}}. Unlike
#' \code{\link{polean}}, which is parametric over strictness via the
#' \code{strict} argument, \code{as.polean()} reads strictness from
#' the R type of the input.
#'
#' @details
#' Strictness by input type:
#' \describe{
#'   \item{\code{logical}}{strict (\code{POL_TRUE}/\code{POL_FALSE}).}
#'   \item{\code{integer}, \code{double}, \code{numeric} (via the
#'     \code{ANY} method)}{not strict (\code{POL_1}/\code{POL_0}).}
#'   \item{\code{complex}}{strict if \code{Im(x) == 0}, else not strict}
#'   \item{\code{character}}{parsed element-wise: strings in
#'     \code{c("T", "TRUE", "F", "FALSE")} are treated as logical;
#'     otherwise, strings that round-trip through \code{as.complex}
#'     are parsed as complex; otherwise as numeric. The parsed value
#'     is then re-dispatched.}
#'   \item{\code{list}}{coerced element-wise via the rules above.}
#'   \item{\code{polean}}{preserved in encoding.}
#' }
#'
#' @param x Object to coerce.
#' @param ... Further arguments passed to \code{new("polean", ...)}
#'   (e.g. \code{dim}).
#'
#' @return A polean.
#'
#' @examples
#' as.polean(TRUE)                  # POL_TRUE
#' as.polean(1)                     # POL_1
#' as.polean(1+0i)                  # POL_TRUE
#' as.polean(1+1i)                  # POL_1
#' as.polean("TRUE")                # POL_TRUE
#' as.polean("1+0i")                # POL_TRUE
#' as.polean("1.5")                 # POL_1
#' as.polean(list(TRUE, 1, NA))     # POL_TRUE POL_1 POL_NA
#'
#' @seealso \code{\link{polean}}, \code{\link{polean-literals}}
#' @export
setGeneric("as.polean", function(x, ...) standardGeneric("as.polean"))

setMethod("as.polean", "ANY", function(x, ...) new("polean", getOrientation(x), getStrict(x), ...))
setMethod("as.polean", "list", function(x, ...) new("polean", sapply(x, getOrientation), as.integer(sapply(x, getStrict)), ...))

# List with Polean
#' @export
setMethod("c", "polean", function(x, ...) {
  args = c(list(x), list(...))
  values = unlist(lapply(args, getOrientation))
  treat_logicals = unlist(lapply(args, getStrict))

  new("polean", values, treat_logicals)
})

# --- Equality
# General Equality (on polean orientation | polean(1,F)==1)
setMethod("==", signature("polean", "ANY"),
          function(e1, e2) Re(e1@value) == e2)
setMethod("==", signature("ANY", "polean"),
          function(e1, e2) e1 == Re(e2@value))
setMethod("!=", signature("polean", "ANY"),
          function(e1, e2) Re(e1@value) != e2)
setMethod("!=", signature("ANY", "polean"),
          function(e1, e2) e1 != Re(e2@value))

# Complex Equality (on polean orientation + treat_logical | polean(1,F)!=1+0i)
setMethod("==", signature("polean", "complex"),
          function(e1, e2) e1@value == e2)
setMethod("==", signature("complex", "polean"),
          function(e1, e2) e1 == e2@value)
setMethod("!=", signature("polean", "complex"),
          function(e1, e2) e1@value != e2)
setMethod("!=", signature("complex", "polean"),
          function(e1, e2) e1 != e2@value)

# polean Equality (on polean orientation + treat_logical | polean(1,F)!=polean(1,T))
setMethod("==", signature("polean", "polean"),
          function(e1, e2) e1@value == e2@value)
setMethod("!=", signature("polean", "polean"),
          function(e1, e2) e1@value != e2@value)

# Logical Equality (on complex coercion | poleon(1,F)!=TRUE)
setMethod("==", signature("polean", "logical"),
          function(e1, e2) e1@value==e2)
setMethod("==", signature("logical", "polean"),
          function(e1, e2) e2==e1)
setMethod("!=", signature("polean", "logical"),
          function(e1, e2) !(e1==e2))
setMethod("!=", signature("logical", "polean"),
          function(e1, e2) !(e2==e1))
