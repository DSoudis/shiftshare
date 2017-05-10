
# Calculates the national change in Y between two points in time
get.G <- function(df, y.var ,region.var, year.var, sector.var, gregion.id, sector.id, start.year, end.year){
  filter.criteria <- lazyeval::interp(~(y1 == x1 | y2 == x2) & (y3 == x3) &
                                        (y4 == x4), .values = list( y1 = as.name(year.var), x1 = start.year,
                                                                    y2 = as.name(year.var), x2 = end.year,
                                                                    y3 = as.name(region.var), x3 = gregion.id,
                                                                    y4 = as.name(sector.var), x4 = sector.id))

  temp.G <- df %>% dplyr::filter_(filter.criteria) %>% dplyr::select_(y.var) %>% dplyr::slice(c(1, n()))

  G <- diff(temp.G[[y.var]])/temp.G[[1,y.var]]
  G
}


# Calculates the national change in Y for each sector between two points in time
get.Gi <- function(df, y.var ,region.var, year.var, sector.var, gregion.id, sector.id, start.year, end.year){
  filter.criteria <- lazyeval::interp(~(y1 == x1 | y2 == x2) & (y3 == x3) &
                                        (y4 != x4), .values = list( y1 = as.name(year.var), x1 = start.year,
                                                                    y2 = as.name(year.var), x2 = end.year,
                                                                    y3 = as.name(region.var), x3 = gregion.id,
                                                                    y4 = as.name(sector.var), x4 = sector.id))

  temp.Gi <- df %>% dplyr::filter_(filter.criteria) %>% dplyr::select_(region.var, sector.var, y.var) %>%
    dplyr::group_by_(sector.var) %>%  dplyr::slice(c(1, n()))

  mutate.call <- lazyeval::interp(~((a - dplyr::lag(a))/dplyr::lag(a)), a = as.name(y.var))
  Gi <- temp.Gi %>% dplyr::group_by_(sector.var) %>% dplyr::mutate_(.dots = stats::setNames(list(mutate.call), 'change')) %>%
    dplyr::select_(sector.var, 'change') %>% dplyr::slice(n())
  Gi
}


# Calculates change in Y by region and sector between two points in time
get.gi <- function(df, y.var ,region.var, year.var, sector.var, gregion.id, sector.id, start.year, end.year){
  filter.criteria <- lazyeval::interp(~(y1 == x1 | y2 == x2) & (y3 != x3) &
                                        (y4 != x4), .values = list( y1 = as.name(year.var), x1 = start.year,
                                                                    y2 = as.name(year.var), x2 = end.year,
                                                                    y3 = as.name(region.var), x3 = gregion.id,
                                                                    y4 = as.name(sector.var), x4 = sector.id))

  temp.gi <- df %>% dplyr::filter_(filter.criteria) %>% dplyr::select_(region.var, sector.var, y.var) %>%
    dplyr::group_by_(sector.var, region.var) %>%  dplyr::slice(c(1, n()))

  mutate.call <- lazyeval::interp(~((a - dplyr::lag(a))/dplyr::lag(a)), a = as.name(y.var))
  gi <- temp.gi %>% dplyr::group_by_(sector.var, region.var) %>% dplyr::mutate_(.dots = stats::setNames(list(mutate.call), 'change')) %>%
    dplyr::select_(sector.var, 'change') %>% dplyr::slice(n())
  gi
}

# Extracts initial employment by sector and region
get.et <- function(df, y.var, region.var, year.var, sector.var, gregion.id, sector.id, start.year){
  filter.criteria <- lazyeval::interp(~(y1 == x1) & y2 != x2 & y3 != x3, .values = list(y1 = as.name(year.var), x1 = start.year,
                                                                                        y2 = as.name(sector.var), x2 = sector.id,
                                                                                        y3 = as.name(region.var), x3 = gregion.id))

  et <- df %>% dplyr::filter_(filter.criteria) %>% dplyr::select_(region.var, sector.var, y.var)
  et
}


# Calculates simple shift share analysis
get.ssa <- function(G, Gi, gi, et, y.var, region.var, sector.var){
  et <- et %>% dplyr::arrange_(region.var, sector.var)
  gi <- gi %>% dplyr::arrange_(region.var, sector.var)
  Gi <- Gi %>% dplyr::arrange_(sector.var)
  NSi <- et[[y.var]] * G
  IMi <- et[[y.var]] * (Gi[['change']] - G)
  RSi <- et[[y.var]] * (gi[['change']] - Gi[['change']])
  results <- as.data.frame(cbind(et[, c(region.var, sector.var)],NSi, IMi, RSi))
  results
}
