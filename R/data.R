#' Prices of 50,000 round cut diamonds.
#'
#' A simulated dataset of toll booth records for Loop 1 (aka "Mopac") in Austin, Texas.
#' Traffic density & make/model/color frequency based on actual observational data obtained in May 2020.
#'
#' Dataset contains records from 2 toll booths:
#' (1) Plaza (north outskirts of Austin)
#' (2) 45 (south outskirts of Austin)
#'
#' For example, records with direction == "south" & toll_booth == "plaza" indicate vehicles travelling into town.
#'
#' @format A data frame with 109,375 rows and 5 variables:
#' \describe{
#'   \item{t}{dttm, time at which vehicle traverse toll booth}
#'   \item{toll_booth}{character, location of vehicle}
#'   \item{direction}{character, direction of travel}
#'   \item{plate}{character, unique license plate of vehicle}
#'   \item{make}{character, vehicle manufacturer}
#'   \item{model}{character, vehicle model}
#'   \item{color}{character, vehicle color}
#'   ...
#' }
"mopac"