#' 2020 New York Times Headlines
#'
#' Includes selected headlines and additional metadata for NYT articles throughout 2020. This dataset is not a comprehensive account of all major events from 2020.
#'
#' @format A data frame with 1,830 rows and 6 variables:
#' \describe{
#'   \item{headline}{Article Headline}
#'   \item{abstract}{Brief summary of article}
#'   \item{byline}{Contributing Writers}
#'   \item{pub_date}{Date of Publication}
#'   \item{section_name}{NYT section in which article was published}
#'   \item{web_url}{Article URL}
#'   ...
#' }
#'
#' @source Obtained using \href{https://developer.nytimes.com/}{NYT Developer Portal} (Archive API)
"nyt2020"
