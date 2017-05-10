
# Calculates the yearly national change in Y within two points in time
get.Gk <- function(df, y.var , region.var, year.var, sector.var, gregion.id, sector.id, start.year, end.year){
  filter.criteria <- lazyeval::interp(~(dplyr::between(y1, x1, x2)) & (y3 == x3) &
                              (y4 == x4), .values = list( y1 = as.name(year.var), x1 = start.year, x2 = end.year,
                                                          y3 = as.name(region.var), x3 = gregion.id,
                                                          y4 = as.name(sector.var), x4 = sector.id))

  temp.Gk <- df %>% dplyr::filter_(filter.criteria) %>% dplyr::select_(year.var, y.var)

  Gk <- (temp.Gk[[y.var]] - dplyr::lag(temp.Gk[[y.var]])) / dplyr::lag(temp.Gk[[y.var]])
  Gk <- cbind(start.year:end.year, Gk)
}


# Calculates the national yearly change in Y for each sector within two points in time
get.Gik <- function(df, y.var, region.var, year.var, sector.var, gregion.id, sector.id, start.year, end.year){
  filter.criteria <- lazyeval::interp(~(dplyr::between(y1, x1, x2)) & (y3 == x3) &
                              (y4 != x4), .values = list( y1 = as.name(year.var), x1 = start.year, x2 = end.year,
                                                          y3 = as.name(region.var), x3 = gregion.id,
                                                          y4 = as.name(sector.var), x4 = sector.id))

  temp.Gik <- df %>% dplyr::filter_(filter.criteria) %>% dplyr::select_(region.var, year.var ,sector.var, y.var)

  mutate.call <- lazyeval::interp(~((a - dplyr::lag(a))/dplyr::lag(a)), a = as.name(y.var))
  Gik <- temp.Gik %>% dplyr::group_by_(sector.var) %>% dplyr::mutate_(.dots = stats::setNames(list(mutate.call), 'change')) %>% dplyr::select_(sector.var, year.var ,'change')
  Gik
}


# Calculates yearly change in Y by region and sector within two points in time
get.gik <- function(df, y.var ,region.var, year.var, sector.var, gregion.id, sector.id, start.year, end.year){
  filter.criteria <- lazyeval::interp(~(dplyr::between(y1, x1, x2)) & (y3 != x3) &
                              (y4 != x4), .values = list( y1 = as.name(year.var), x1 = start.year, x2 = end.year,
                                                          y3 = as.name(region.var), x3 = gregion.id,
                                                          y4 = as.name(sector.var), x4 = sector.id))

  temp.gik <- df %>% dplyr::filter_(filter.criteria) %>% dplyr::select_(region.var, year.var ,sector.var, y.var)
  temp.gik

  mutate.call <- lazyeval::interp(~((a - dplyr::lag(a)) / dplyr::lag(a)), a = as.name(y.var))
  gik <- temp.gik %>% dplyr::group_by_(region.var, sector.var) %>% dplyr::mutate_(.dots = stats::setNames(list(mutate.call), 'change')) %>%
    dplyr::select_(sector.var, year.var ,'change') %>% dplyr::arrange_(region.var, sector.var, year.var)
  gik
}


# Extracts yearly employment by sector and region
get.etk <- function(df, y.var, region.var, year.var, sector.var, gregion.id, sector.id, start.year, end.year){
  filter.criteria <- lazyeval::interp(~(dplyr::between(y1, x1, x2)) & y3 != x3 & y4 != x4, .values = list(y1 = as.name(year.var), x1 = start.year, x2 = end.year,
                                                                                         y3 = as.name(sector.var), x3 = sector.id,
                                                                                         y4 = as.name(region.var), x4 = gregion.id))

  etk <- df %>% dplyr::filter_(filter.criteria) %>% dplyr::select_(region.var, sector.var, year.var ,y.var) %>% dplyr::arrange_(region.var, sector.var, year.var)
  etk
}


# Calculates dynamic shift share analysis
get.dssa <- function(Gk, Gik, gik, etk, y.var, region.var, sector.var, year.var){
  NSik <- dplyr::lag(etk[[y.var]]) * Gk[,2]
  IMik <- dplyr::lag(etk[[y.var]]) * (Gik[['change']] - Gk[ ,2])
  RSik <- dplyr::lag(etk[[y.var]]) * (gik[['change']] - Gik[['change']])
  temp.dssa <- as.data.frame(cbind(etk[, c(region.var, sector.var, year.var)],NSik, IMik, RSik))

  results <- temp.dssa %>% dplyr::group_by_(region.var, sector.var) %>% dplyr::summarise(sum(NSik, na.rm = T),
                                                                       sum(IMik, na.rm = T),
                                                                       sum(RSik, na.rm = T))
  names(results) <- c(region.var, sector.var, 'NSik', 'IMik', 'RSik')
  results
}
