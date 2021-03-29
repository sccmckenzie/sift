#' Augmented data frame subsetting. Imagine `dplyr::filter()` that includes neighboring observations.
#'
#' @return
#' A data frame.
#' @export
#' @import dplyr
#'
#' @examples
#' # Grouped dataframes
#' # Multiple scopes
#' # .cluster/.anchor column
sift <- function(.data, sift.col, scope, ...) {
  UseMethod("sift")
}


#' @export
sift.data.frame <- function(.data, sift.col, scope, ...) {

  x1 <- double()
  x2 <- double()
  .anchor = NA

  # Coerce sift.col to numeric
  if(rlang::is_missing(rlang::enexpr(sift.col))) stop("Must supply sift.col (variable to sift along)")

  tryCatch(
    warning = function(cnd) {
      stop("input sift.col must be coercible to <numeric>")
    },
    x <- as.numeric(pull(.data, {{sift.col}}))
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

  scope <- scope[1:2]

  # check ... (dplyr::filter will detect invalid expr)
  if(missing(...)) {
    message("No logical expressions supplied.")
    return(.data %>% mutate(.cluster = NA_integer_, .anchor = NA))
  }

  .siftIndex <- seq_len(nrow(.data))

  loc <- .data %>%
    tibble::add_column(.siftIndex = .siftIndex) %>%
    ungroup() %>%
    filter(!!!rlang::exprs(...)) %>%
    pull(.siftIndex)

  loc <- .siftIndex %in% loc

  if(rlang::is_empty(x[loc]) | all(is.na(x[loc]))) {
    return(filter(.data, FALSE) %>% mutate(.cluster = integer(), .anchor = logical()))
  }

  .data[[".anchor"]] <- loc

  gi <- group_indices(.data)

  df <- tibble(loc, gi, x, i = seq_along(x)) %>%
    arrange(x) %>%
    group_by(gi) %>%
    mutate(x1 = if_else(loc, x - scope[1], NA_real_),
           x2 = if_else(loc, x + scope[2], NA_real_)) %>%
    # below is opportunity for future Rcpp optimization
    tidyr::fill(x1, .direction = "up") %>%
    tidyr::fill(x2, .direction = "down") %>%
    filter(x > x1 | x < x2)

  dplyr_row_slice(.data, df$i) %>%
    mutate(.cluster = kluster({{sift.col}}, bw = max(scope), fixed = TRUE), .before = .anchor)
}
