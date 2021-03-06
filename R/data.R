#' Regional Employment by U.S. State and Occupation.
#'
#' A dataset containing regional Employment for 51 U.S. States between 2012 and 2015
#' broken down Occupation
#'
#' @format A data frame with 4691 rows and 7 variables:
#' \describe{
#'   \item{ST}{State abbreviation}
#'   \item{STATE}{State name}
#'   \item{OCC_CODE}{The 7-digit Standard Occupational Classification (SOC) code, or OES specific code for the occupation }
#'   \item{OCC_TITLE}{Standard Occupational Classification title or OES specific title for the occupation}
#'   \item{OCC_GROUP}{Shows the SOC occupation level. "total"=Total of all occupations; "major"="SOC major group; "minor"="SOC minor group; "broad"=Broad SOC occupation; "detailed"="Detailed SOC occupation level}
#'   \item{TOT_EMP}{Estimated total employment rounded to the nearest 10 (excludes self-employed)}
#'   \item{Year}{Year Identifier}
#'   ...
#' }
#' @source \url{Beureau of Labor and Statistics. http://www.bls.gov/oes/tables.htm}
"usregions"
