#' Augmented data frame subsetting. Imagine `dplyr::filter()` that includes neighboring observations.
#'
#' @return
#' A data frame.
#' @export
#'
#'
#' @examples
#' 1
sift <- function(.data, sift.col, scope, ...) {
  UseMethod("sift")
}

#' @export
sift.data.frame <- function(.data, sift.col, scope, ...) {

  # Coerce sift.col to numeric
  if(rlang::is_missing(rlang::enexpr(sift.col))) stop("Must supply sift.col (variable to sift along)")

  tryCatch(
    warning = function(cnd) {
      stop("input sift.col must be coercible to <numeric>")
    },
    x <- as.numeric(dplyr::pull(.data, {{sift.col}}))
  )

  # verify scope
  if(rlang::is_missing(rlang::enexpr(scope))) stop("Must supply scope amount")

  tryCatch(
    warning = function(cnd) {
      stop("input scope must be coercible to <numeric>")
    },
    scope <- as.numeric(scope)
  )

  if (length(scope) == 1) {
    scope[2] <- scope[1]
  }

  # check ... (dplyr::filter will detect invalid expr)
  if(missing(...)) {
    message("No logical expressions supplied.")
    return(.data %>% mutate(.achor = NA))
  }

  loc <- dplyr:::filter_rows(.data, !!!rlang::exprs(...))

  if(rlang::is_empty(x[loc]) | all(is.na(x[loc]))) {
    return(filter(.data, FALSE) %>% mutate(.anchor = logical()))
  }

  gi <- dplyr::group_indices(.data)

  piled <- pile(x, gi, x[loc], gi[loc])

  index <- (piled[[1]] < scope[1]) | (piled[[2]] < scope[2])

  index[is.na(index)] <- FALSE

  dplyr::dplyr_row_slice(.data, index)
}


