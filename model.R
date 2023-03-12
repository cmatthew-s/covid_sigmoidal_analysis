library(growthcurver)
library(nlstools)

covid_data = read.csv("C:/Users/matthew/Downloads/owid-covid-data.csv")
scaled_covid_data = covid_data[, c('total_cases', 'new_cases', 'total_vaccinations', 'population', 'total_deaths', 'new_deaths')] %>% mutate_all(~(scale(.) %>% as.vector))
df = cbind(covid_data[, c( 'location', 'life_expectancy', 'date')], scaled_covid_data)
df$date = as.Date(df$date,"%d-%m-%y")

dataAmerica = df[which(df$location=='United States', arr.ind = TRUE), ]
dataAmerica$date = as.Date(dataAmerica$date,"%d-%m-%y")

# INCREASE RATE COMPARISON BETWEEN CATEGORIES
model_result = generateModel(dataJapan, c(
    c("2021-12-01", "2022-02-28", "winter"),
    c("2021-06-01", "2021-08-31", "summer"),
    c("2022-03-01", "2022-05-31", "spring"),
    c("2020-09-01", "2020-11-30", "fall"),
    c(1, 400, 601, 850)
))
print(model_result)

