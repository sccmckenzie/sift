#' Augmented data frame subsetting. Imagine `dplyr::filter()` that includes neighboring observations.
#'
#' @return
#' A data frame.
#' @export
#' @importFrom dplyr %>%
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

  .data[[".anchor"]] <- loc

  gi <- dplyr::group_indices(.data)

  df <- tibble::tibble(loc, gi, x, i = seq_along(x)) %>%
    dplyr::group_by(gi) %>%
    dplyr::mutate(x1 = dplyr::if_else(loc, x - scope[1], NA_real_),
                  x2 = dplyr::if_else(loc, x + scope[2], NA_real_)) %>%
    # below is opportunity for future Rcpp optimization
    tidyr::fill(x1, .direction = "up") %>%
    tidyr::fill(x2, .direction = "down") %>%
    dplyr::filter(x > x1 | x < x2)

  dplyr::dplyr_row_slice(.data, df$i)
}


