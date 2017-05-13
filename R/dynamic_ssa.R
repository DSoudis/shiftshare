
# Calculates the yearly national change in Y within two points in time
get.Gk <- function(df, y.var ,region.var, year.var, sector.var, gregion.id, sector.id, start.year, end.year){
  filter.criteria <- lazyeval::interp(~(dplyr::between(y1, x1, x2)) & (y3 == x3) &
                              (y4 == x4), .values = list( y1 = as.name(year.var), x1 = start.year, x2 = end.year,
                                                          y3 = as.name(region.var), x3 = gregion.id,
                                                          y4 = as.name(sector.var), x4 = sector.id))
  
  temp.Gk <- df %>% dplyr::filter_(filter.criteria) %>% dplyr::select_(year.var, y.var)
  
  Gk <- (temp.Gk[[y.var]] - dplyr::lag(temp.Gk[[y.var]])) / dplyr::lag(temp.Gk[[y.var]])
  Gk <- data.frame(cbind(start.year:end.year, Gk))
  names(Gk) <- c(year.var, 'Gk')
  Gk
}


# Calculates the national yearly change in Y for each sector within two points in time
get.Gik <- function(df, y.var ,region.var, year.var, sector.var, gregion.id, sector.id, start.year, end.year){
  filter.criteria <- lazyeval::interp(~(dplyr::between(y1, x1, x2)) & (y3 == x3) &
                              (y4 != x4), .values = list( y1 = as.name(year.var), x1 = start.year, x2 = end.year,
                                                          y3 = as.name(region.var), x3 = gregion.id,
                                                          y4 = as.name(sector.var), x4 = sector.id))
  
  temp.Gik <- df %>% dplyr::filter_(filter.criteria) %>% dplyr::select_(region.var, year.var ,sector.var, y.var)
  
  mutate.call <- lazyeval::interp(~((a - lag(a))/lag(a)), a = as.name(y.var))
  Gik <- temp.Gik %>% dplyr::group_by_(sector.var) %>% dplyr::mutate_(.dots = setNames(list(mutate.call), 'changeGik')) %>% dplyr::select_(sector.var, year.var ,'changeGik')
  Gik
}


# Calculates yearly change in Y by region and sector within two points in time
get.gik <- function(df, y.var, region.var, year.var, sector.var, gregion.id, sector.id, start.year, end.year){
  filter.criteria <- lazyeval::interp(~(dplyr::between(y1, x1, x2)) & (y3 != x3) &
                              (y4 != x4), .values = list( y1 = as.name(year.var), x1 = start.year, x2 = end.year,
                                                          y3 = as.name(region.var), x3 = gregion.id,
                                                          y4 = as.name(sector.var), x4 = sector.id))
  
  temp.gik <- df %>% dplyr::filter_(filter.criteria) %>% dplyr::select_(region.var, year.var ,sector.var, y.var)
  temp.gik
  
  mutate.call <- lazyeval::interp(~((a - lag(a))/lag(a)), a = as.name(y.var))
  gik <- temp.gik %>% dplyr::group_by_(region.var, sector.var) %>% dplyr::mutate_(.dots = setNames(list(mutate.call), 'changegik')) %>%
    dplyr::select_(sector.var, year.var ,'changegik') %>% dplyr::arrange_(region.var, sector.var, year.var)
  gik
}


# Extracts yearly employment by sector and region
get.etk <- function(df, y.var, region.var, year.var, sector.var, gregion.id, sector.id, start.year, end.year){
  filter.criteria <- lazyeval::interp(~(dplyr::between(y1, x1, x2)) & y3 != x3 & y4 != x4, .values = list(y1 = as.name(year.var), x1 = start.year, x2 = end.year,
                                                                                         y3 = as.name(sector.var), x3 = sector.id,
                                                                                         y4 = as.name(region.var), x4 = gregion.id))
  mutate.call <- lazyeval::interp(~lag(a, 1), a = as.name(y.var))
  
  etk <- df %>% dplyr::filter_(filter.criteria) %>% dplyr::select_(region.var, sector.var, year.var ,y.var) %>% dplyr::arrange_(region.var, sector.var, year.var)
  etk <- etk %>% dplyr::group_by_(region.var, sector.var) %>%  dplyr::mutate_(.dots = setNames(list(mutate.call), 'laged'))
  etk
}


#Merges previous results
get.merged <- function(etk, Gik, gik, Gk, region.var, sector.var, year.var){
  merged <- dplyr::left_join(etk, Gik, by = c(sector.var, year.var)) %>%
    dplyr::left_join(.,gik, by = c(region.var, sector.var, year.var)) %>%
    dplyr::left_join(.,Gk)
  merged
}


# Calculates dynamic shift share analysis
get.dssa <- function(merged, y.var, region.var, sector.var, year.var){
  NSik <- merged[,5]*merged[,8]
  IMik <-  merged[,5] * (merged[,6] - merged[,8])
  RSik <- merged[,5] * (merged[,7] - merged[,6]) 
  merged$NSik <- NSik[,1]
  merged$IMik <- IMik[,1]
  merged$RSik <- RSik[,1]
  
  results <- merged %>% dplyr::group_by_(region.var, sector.var) %>% dplyr::summarise(sum(NSik, na.rm = T),
                                                                        sum(IMik, na.rm = T),
                                                                        sum(RSik, na.rm = T))
  names(results) <- c(region.var, sector.var, 'NSik', 'IMik', 'RSik')
  results
}
