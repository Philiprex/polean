#' polean: Complex-Encoded Semi-Boolean Extension of the R Logical Type
#'
#' A non-propagating extension of R's logical type that adds two literals
#' (\code{ISH(1)}, \code{ISH(0)}) alongside \code{TRUE}, \code{FALSE},
#' and \code{NA}. Each polean is stored as a complex number whose real
#' part holds orientation and whose imaginary part holds strictness.
#'
#' @keywords internal
#' @aliases polean-package
#' @import methods
"_PACKAGE"

# --- Setup

# definition
setClassUnion("complexLike", c("complex", "array"))
setClassUnion("characterLike", c("character", "array"))

#' polean class and constructor
#'
#' S4 class for non-propagating, semi-boolean values. A polean has five
#' literals: \code{TRUE}, \code{FALSE}, \code{ISH(1)}, \code{ISH(0)},
#' and \code{NA}.
#'
#' @details
#' Internally each element is a complex number. The real part is the
#' orientation (\code{0}, \code{1}, or \code{NA}). The imaginary part
#' is the strictness flag (\code{0} for strict, \code{1} for ish).
#'
#' \code{polean(orientation, treat_logical, dim)} calls the class
#' initializer. The initializer validates that \code{orientation} is
#' coercible to \code{0}, \code{1}, or \code{NA}, and that
#' \code{treat_logical} is coercible to \code{0} or \code{1} (no
#' \code{NA}). When \code{dim} is supplied, both \code{orientation}
#' and \code{treat_logical} are recycled to \code{prod(dim)} and the
#' result is shaped to that array dimension.
#'
#' The \code{value} slot stores
#' \code{complex(real = orientation, imaginary = !treat_logical)}.
#' The \code{name} slot stores the printed name of each element
#' (\code{"TRUE"}, \code{"FALSE"}, \code{"ISH(1)"}, \code{"ISH(0)"},
#' or \code{NA}).
#'
#' @slot value A \code{complexLike} (complex vector or complex array).
#' @slot name A \code{characterLike} (character vector or character array)
#'   matching the shape of \code{value}.
#'
#' @param orientation Logical or numeric, coercible to \code{0},
#'   \code{1}, or \code{NA}.
#' @param treat_logical Logical or numeric, coercible to \code{0} or
#'   \code{1}; \code{NA} not allowed. Default \code{FALSE}: result is
#'   ish. \code{TRUE}: result is strict.
#' @param dim Integer vector of array dimensions, or \code{NULL}. When
#'   non-null, \code{orientation} and \code{treat_logical} are recycled
#'   to \code{prod(dim)}.
#'
#' @return An object of class \code{polean}.
#'
#' @examples
#' polean(TRUE)                  # ISH(1)   (default treat_logical = FALSE)
#' polean(TRUE, TRUE)            # TRUE
#' polean(1, TRUE)               # TRUE
#' polean(c(1, 0, NA))           # ISH(1) ISH(0) NA
#' polean(1, dim = c(2, 3))      # 2x3 array of ISH(1)
#' polean(c(0, 1), dim = c(2, 3))
#'
#' @name polean
#' @aliases polean-class
#' @seealso \code{\link{as.polean}}, \code{\link{ISH}}
#' @export polean
#' @exportClass polean
setClass(Class="polean", slots = list(value="complexLike", name="characterLike"))

# initializer
setMethod("initialize", "polean", function(.Object, orientation, treat_logical, dim, ...){
  .Object = callNextMethod(.Object, ...)
  if (!is.null(dim)){
    n = prod(dim)
    orientation = rep_len(orientation, n)
    treat_logical = rep_len(treat_logical, n)
  }
  if (!(all(as.integer(orientation) %in% c(0,1,NA)))){
    badEntries = unique(orientation[which(!(as.integer(orientation) %in% c(0,1,NA)))])
    stop(paste0("orientation must be logical or coercible to 1 or 0, not ", badEntries))
  } else if (!(all(as.integer(treat_logical) %in% c(0,1))) | anyNA(treat_logical)){
    badEntries = unique(treat_logical[!(as.integer(treat_logical) %in% c(0,1)) | is.na(treat_logical)])
    stop(paste0("treat_logical must be logical or coercible to 1 or 0 and not NA, not ", badEntries))
  } else {
    .Object@value = complex(real=orientation, imaginary=!treat_logical)
    .Object@name = as.character(sapply(.Object@value, function(v) ifelse(is.na(v), NA, ifelse(Im(v)==0, as.logical(Re(v)), paste0("ISH(", Re(v), ")")))))
    if (!is.null(dim)){
      dim(.Object@value) = dim
      dim(.Object@name) = dim
    }
    .Object
  }
})

# constructor
polean = function(orientation, treat_logical=F, dim=NULL) new("polean", orientation, treat_logical=treat_logical, dim=dim)

# --- Coercion from polean
setMethod("as.integer", "polean", function(x) Re(x@value))
setMethod("as.numeric", "polean", function(x) Re(x@value))
setMethod("as.complex", "polean", function(x) x@value)
setMethod("as.character", "polean", function(x) x@name)
setMethod("as.logical", "polean", function(x) Re(x@value)==1L)

# --- Coercion to polean

#' Coerce an object to a polean
#'
#' Generic and methods for coercing R objects to the polean type. Each
#' method delegates to \code{\link{polean}} with arguments derived from
#' the input's R type.
#'
#' @param x Object to coerce.
#' @param ... Further arguments passed to \code{\link{polean}}
#'   (e.g. \code{dim}; \code{treat_logical} where applicable).
#'
#' @section Methods:
#' \describe{
#'   \item{\code{integer}, \code{double}, \code{numeric}}{
#'     \code{polean(as.logical(x), ...)}. Default \code{treat_logical = FALSE}
#'     produces ish values.}
#'   \item{\code{complex}}{
#'     \code{polean(Re(x), treat_logical = !Im(x), ...)}. Real part gives
#'     orientation; imaginary part flips strictness (\code{Im == 0} is
#'     strict, \code{Im != 0} is ish).}
#'   \item{\code{logical}}{
#'     \code{polean(x, treat_logical = TRUE, ...)}. Strict.}
#'   \item{\code{character}}{
#'     Each element is parsed: strings in \code{c("T","TRUE","F","FALSE")}
#'     become logical; otherwise, strings that round-trip through
#'     \code{as.complex} become complex; otherwise, the string is parsed
#'     as numeric. The vector of parsed values is then passed back through
#'     \code{as.polean}.}
#'   \item{\code{list}}{
#'     Each element is coerced via \code{as.polean}; the resulting
#'     \code{value} slots are unlisted and passed back through
#'     \code{as.polean}.}
#' }
#'
#' @return A polean.
#'
#' @examples
#' as.polean(1)        # ISH(1)
#' as.polean(0)        # ISH(0)
#' as.polean(TRUE)     # TRUE
#' as.polean(1+0i)     # TRUE     (Im == 0 -> strict)
#' as.polean(1+1i)     # ISH(1)   (Im != 0 -> ish)
#' as.polean("TRUE")   # TRUE
#' as.polean("1+0i")   # TRUE
#' as.polean("1.5")    # ISH(1)
#' as.polean(list(TRUE, 1, 1+1i))
#'
#' @seealso \code{\link{polean}}, \code{\link{ISH}}
#' @export
setGeneric("as.polean", function(x, ...) standardGeneric("as.polean"))

setMethod("as.polean", "integer", function(x, ...) polean(as.logical(x), ...))
setMethod("as.polean", "double", function(x, ...) polean(as.logical(x), ...))
setMethod("as.polean", "numeric", function(x, ...) polean(as.logical(x), ...))
setMethod("as.polean", "complex", function(x, ...) polean(Re(x), treat_logical=!Im(x), ...))
setMethod("as.polean", "logical", function(x, ...) polean(x, treat_logical=T, ...))
setMethod("as.polean", "list", function(x) as.polean(unlist(sapply(x, function(e) as.polean(e)@value))))
setMethod("as.polean", "character", function(x, ...) as.polean(lapply(x, function(e) ifelse(e %in% c("T", "TRUE", "F", "FALSE"), as.logical(e), ifelse(e==as.character(as.complex(e)), as.complex(e), as.numeric(e))))))

# type object

#' Length-1 polean literal constructor
#'
#' Returns one of the five polean literals (\code{TRUE}, \code{FALSE},
#' \code{ISH(1)}, \code{ISH(0)}, \code{NA}). Input must be length 1 and
#' must coerce to one of \code{0+0i}, \code{1+0i}, \code{0+1i},
#' \code{1+1i}, or \code{NA}; otherwise an error is raised.
#'
#' @details
#' \code{ISH} delegates to \code{\link{as.polean}}: the input's R type
#' determines the result. \code{ISH(1)} returns \code{ISH(1)} (numeric
#' input, default \code{treat_logical = FALSE}). \code{ISH(TRUE)}
#' returns \code{TRUE} (logical input, \code{treat_logical = TRUE}).
#' \code{ISH(1+0i)} returns \code{TRUE}; \code{ISH(1+1i)} returns
#' \code{ISH(1)}.
#'
#' @param val A length-1 input acceptable to \code{\link{as.polean}}.
#' @return A length-1 polean.
#'
#' @examples
#' ISH(1)         # ISH(1)
#' ISH(0)         # ISH(0)
#' ISH(TRUE)      # TRUE
#' ISH(1+0i)      # TRUE
#' ISH(1+1i)      # ISH(1)
#' ISH(NA)        # NA
#'
#' @seealso \code{\link{polean}}, \code{\link{as.polean}}
#' @export
ISH = function(val){
  if (as.complex(val) %in% c(0+0i,1+0i,0+1i,1+1i,NA) & length(val)){
    return(as.polean(val))
  } else {
    stop(paste0("Input for polean literal must be NA or coerce to 0+0i, 1+0i, 0+1i, 1+1i, or NA and be of length 1, not, ", val))
  }
}

# --- Combine multiple poleans into one (non-logical, non-polean input will have treat_logical=F)

#' @export
setMethod("c", "polean", function(x, ...) {
  args = c(list(x), list(...))
  values = unlist(lapply(args, function(a) {
    Re(as.complex((a)))
  }))
  treat_logicals = unlist(lapply(args, function(a) {
    sapply(1:length(a), function(i) ifelse(is(a[i], "polean"), !Im(a@value[i]), !Im(as.polean(a[i])@value)))
  }))

  polean(values, treat_logicals)
})

# --- Length, Index, Which
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

# --- Display
setMethod("show", "polean", function(object) {
  return(print(object@name, quote=F))
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