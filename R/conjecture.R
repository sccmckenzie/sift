#' conjecture
#'
#' reshaping / obscure / idiosyncratic
#'
#' @param data A data frame to reshape.
#' @param sort_by Column name, as symbol, to serves as sorting dimension.
#' @param names_from Column name, as symbol. Used to differentiate anterior/posterior observations. Columnn must only contain 2 levels (missing values not allowed).
#' @param names_first level in variable specified by \code{names_from} indicating anterior observation.
#'
#' @export
#' @import rlang
#'
#' @examples
#' conjecture(mtcars)
conjecture <- function(data, sort_by, names_from, names_first) {
  UseMethod("conjecture")
}

#' @export
conjecture.data.frame <- function(data, sort_by, names_from, names_first) {

  nms <- validate_name_col(data, enexpr(names_from), names_first)
  x <- validate_sort_col(data, enexpr(sort_by))
  id <- create_group_id(data, enexpr(sort_by), enexpr(names_from))

  df <- tibble(id, x, nm = pull(data, {{names_from}})) %>%
    tidyr::complete(id, nm)

  df_A <- filter(df, nm == nms[1]) %>%
    arrange(id, desc(x))
  df_B <- filter(df, nm == nms[2]) %>%
    arrange(id, x)

  A_index <- which(!duplicated(df_A$id)) - 1L
  A_index[length(A_index) + 1] <- length(df_A$x)

  B_index <- which(!duplicated(df_B$id)) - 1L
  B_index[length(B_index) + 1] <- length(df_B$x)

  interlace(A_index, df_A$x, B_index, df_B$x)
}

validate_name_col <- function(data, names_from, names_first) {
  x <- pull(data, {{names_from}})

  if (!names_first %in% x) abort(glue::glue("Value '{names_first}' not found in {names_from}."))
  if (anyNA(x)) abort(glue::glue("Column '{names_from}' must not contain any missing values."))
  if (n_distinct(x) != 2) abort(glue::glue("Column '{names_from}' must contain exactly 2 levels."))

  return(c(names_first, setdiff(unique(x), names_first)))
}

validate_sort_col <- function(data, sort_by) {
  tryCatch(
    warning = function(cnd) {
      abort("Column '{sort_by}' must be coercible to <double>")
    },
    as.double(pull(data, {{sort_by}}))
  )
}

create_group_id <- function(data, sort_by, names_from) {
  data %>%
    group_by(!!!syms(names(tidyselect::eval_select(expr(-c(!!sort_by, !!names_from)), data)))) %>%
    group_indices()
}
