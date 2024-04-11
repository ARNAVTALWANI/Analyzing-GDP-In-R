#Problem 1(a)
range_len <- function(vec) {
  range(vec)[2] - range(vec)[1]
}

library(gapminder)
colnames(gapminder)

#Problem 2
compute_gdp_col <- function(df) {
  df$gdp <- df$gdpPercap * df$pop
  return(df)
}

#Problem 3(a)
cont_gdp <- function(df) {
  df <- compute_gdp_col(df)
  continent_gdp <- aggregate(gdp ~ continent + year, data = df, sum)
  continent_gdp <- continent_gdp[order(continent_gdp$continent, continent_gdp$year), ]
  return(continent_gdp)
}

#Problem 5
median_gdppc_range <- function(df) {
  ranges <- tapply(df$gdpPercap, df$country, function(x) diff(range(x)))
  med_range <- median(ranges)
  names(which.min(abs(ranges - med_range)))
}