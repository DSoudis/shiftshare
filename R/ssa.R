#' Perform Shift share Analysis
#'
#' @param df A dataframe
#' @param y.var The dependent variable (e.g., employment or growth)
#' @param region.var A factor variable that defines regions
#' @param year.var A number specifying the year variable
#' @param sector.var A factor variable that defines sectors
#' @param gregion.id A string that identifies the greater region (e.g., national employment)
#' @param sector.id A string that identifies the total sector
#' @param start.year A number specifying start year
#' @param end.year A number specifying end year
#'
#' @return A dataframe containing the National Growth effect (\code{NSi}),
#' the Industry Mix effect (\code{IMi}), and the Local Share effect (\code{RSi}) by region and occupation.
#' @export
#'
#' @examples
#' data('usregions')
#' my.ssa <- ssa(usregions, y.var = 'TOT_EMP' ,region.var = 'ST', sector.var = 'OCC_TITLE',
#'   gregion.id = 'US', year.var = 'Year', sector.id = 'All Occupations',
#'   start.year = 2013, end.year = 2015)
#'
 ssa <- function(df, y.var ,region.var, year.var, sector.var, gregion.id, sector.id, start.year, end.year){

  G <- get.G(df, y.var ,region.var, year.var, sector.var, gregion.id, sector.id, start.year, end.year)

  Gi <- get.Gi(df, y.var ,region.var, year.var, sector.var, gregion.id, sector.id, start.year, end.year)

  gi <- get.gi(df, y.var ,region.var, year.var, sector.var, gregion.id, sector.id, start.year, end.year)

  et <- get.et(df, y.var, region.var, year.var, sector.var, gregion.id, sector.id, start.year)

  out <- get.ssa(G, Gi, gi, et, y.var, region.var, sector.var)
  out
}
