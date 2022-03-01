#' 100 First Name/Last Name Pairing with additional Information
#'
#' A dataset containing Names and other Informations used to predict the race of an individual
#'
#' @format A data frame with 100 rows and 8 variables:
#' \describe{
#'   \item{id}{A unique identifier for the individual}
#'   \item{first_name}{First name of the individual}
#'   \item{last_name}{Last name of the individual}
#'   \item{gender}{Gender of the individual. male/female}
#'   \item{birth_year }{Birth Year of the individuals}
#'   \item{state}{State of the Individual}
#'   \item{county}{County of the Individual}
#'   \item{tract}{Tract of the Individual}
#' }
"name_table"

#' Census Surname List (2010).
#'
#' Census Surname List from 2010 with race/ethnicity probabilities by surname.
#'
#' @format A data frame with 167,613 rows and 6 variables:
#' \describe{
#'   \item{surname}{Surname}
#'   \item{p_whi}{Pr(White | Surname)}
#'   \item{p_bla}{Pr(Black | Surname)}
#'   \item{p_his}{Pr(Hispanic/Latino | Surname)}
#'   \item{p_asi}{Pr(Asian/Pacific Islander | Surname)}
#'   \item{p_oth}{Pr(Other | Surname)}
#' }
"surnames2010"
