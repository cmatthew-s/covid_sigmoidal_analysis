
library(dplyr)

# define datasets
covid_data = read.csv("C:/Users/matthew/Downloads/owid-covid-data.csv")
scaled_covid_data = covid_data[, c('total_cases', 'new_cases', 'total_vaccinations', 'population', 'total_deaths', 'new_deaths')] %>% mutate_all(~(scale(.) %>% as.vector))
df = cbind(covid_data[, c( 'location', 'life_expectancy', 'date')], scaled_covid_data)
df$date = as.Date(df$date,"%d-%m-%y")

# get the country data
dataJapan = df[which(df$location=='Japan', arr.ind = TRUE), ]
dataAmerica = df[which(df$location=='United States', arr.ind = TRUE), ]
dataChina = df[which(df$location=='China', arr.ind = TRUE), ]
dataIsrael = df[which(df$location=='Israel', arr.ind = TRUE), ]

# seasons between country
plotSeasons(dataAmerica, c(
    c("2020-09-01", "2020-11-30", "fall"),
    c("2021-06-01", '2021-08-31', "summer"),
    c("2021-12-01", "2022-02-28", "winter"),
    c("2022-03-01", "2022-05-31", "spring")
))

# plot before and after vaciness effect
plotVaccinesEffect(dataAmerica, c(0, 400, 601, 850))






