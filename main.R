
library(dplyr)
library(psych)
library(GGally)

`%!in%` <- Negate(`%in%`)

covid_data = read.csv("C:/Users/matthew/Downloads/owid-covid-data.csv")
scaled_covid_data = covid_data[, c('total_cases', 'new_cases', 'total_vaccinations', 'population', 'total_deaths', 'new_deaths', 'new_vaccinations', 'positive_rate', 'icu_patients', 'hosp_patients', 'total_boosters')] %>% mutate_all(~(scale(.) %>% as.vector))
unscaled_covid_data = covid_data[, c('total_cases', 'new_cases', 'total_vaccinations', 'population', 'total_deaths', 'new_deaths', 'new_vaccinations', 'positive_rate', 'icu_patients', 'hosp_patients', 'total_boosters')]

# switch between scaled and unscaled data
df = cbind(covid_data[, c( 'location', 'life_expectancy', 'date')], scaled_covid_data)
df$date = as.Date(df$date,"%d-%m-%y")
df = cbind(df, strftime(df$date, '%m'))
names(df)[length(df)] = 'months'

unique(df$location)
dataAmerica = df[which(covid_data$location=='United States', arr.ind = TRUE), ]
dataIsrael = df[which(df$location=='Israel', arr.ind = TRUE), ]
dataJapan = df[which(df$location=='Japan', arr.ind = TRUE), ]
dataLuxembourg = df[which(df$location=='Luxembourg', arr.ind = TRUE), ]

winterMonth = c("01", "02", "12")
winterAmerica = dataAmerica[which(dataAmerica$month %in% winterMonth, arr.ind = TRUE), ]
winterJapan = dataJapan[which(dataJapan$month %in% winterMonth, arr.ind = TRUE), ]
winterIsrael = dataIsrael[which(dataIsrael$month %in% winterMonth, arr.ind = TRUE), ]
winterLuxembourg = dataLuxembourg[which(dataLuxembourg$month %in% winterMonth, arr.ind = TRUE), ]
winterData = rbind(winterAmerica, winterJapan, winterIsrael, winterLuxembourg)

notWinterAmerica = dataAmerica[which(dataAmerica$month %!in% winterMonth, arr.ind = TRUE), ]
notWinterJapan = dataJapan[which(dataJapan$month %!in% winterMonth, arr.ind = TRUE), ]
notWinterIsrael = dataIsrael[which(dataIsrael$month %!in% winterMonth, arr.ind = TRUE), ]
notWinterLuxembourg = dataLuxembourg[which(dataLuxembourg$month %!in% winterMonth, arr.ind = TRUE), ]
notWinterData = rbind(notWinterAmerica, notWinterJapan, notWinterIsrael, notWinterLuxembourg)

# statistical analysis
describe(winterData$new_deaths)
describe(notWinterData$new_deaths)

# correlation matrix
# NOTES:
# Dataframe explanation:
# - months: extracted month from the d-date
# - winter_cats: if winter then 1, else 0
country_data = rbind(dataAmerica, dataJapan, dataIsrael, dataLuxembourg)
corr_data = data.frame(cbind(country_data$new_cases, country_data$new_deaths, country_data$new_vaccinations, as.numeric(country_data$months)))
colnames(corr_data) = c('new_cases', 'new_deaths', 'new_vaccinations', 'months')
corr_data$winter_cats = ifelse(corr_data$months %in% winterMonth, 1, 0)
ggcorr(corr_data)

# analyze the total deaths growth after and before vaccines
total_deaths_israel = totalDeathsModel(dataIsrael, c(0, 300, 501, 650))
total_deaths_japan = totalDeathsModel(dataJapan, c(0, 400, 401, 650))
total_deaths_luxembourg = totalDeathsModel(dataLuxembourg, c(1, 400, 601, 850))
total_deaths_america = totalDeathsModel(dataAmerica, c(1, 400, 601, 850))

before_vaccines = rbind(dataIsrael[0:300,], dataJapan[0:400, ], dataLuxembourg[1:400,], dataAmerica[1:400,])
after_vaccines = rbind(dataIsrael[501:650,], dataJapan[401:650,], dataLuxembourg[601:850,], dataAmerica[601:860,])

# check the exact date when vaccines happens
iv = checkVaccinesDay(dataIsrael)
av = checkVaccinesDay(dataAmerica)
jv = checkVaccinesDay(dataJapan)
lv = checkVaccinesDay(dataLuxembourg)

describeNewDeaths(dataIsrael, c(0, iv-1, iv, dim(dataIsrael)[1]))
describeNewDeaths(dataJapan, c(0, jv-1, jv, dim(dataJapan)[1]))
describeNewDeaths(dataAmerica, c(0, av-1, av, dim(dataAmerica)[1]))
describeNewDeaths(dataLuxembourg, c(0, lv-1, lv, dim(dataIsrael)[1]))

# plot positive rate
par(mfrow=c(4, 1))
plot(dataIsrael$new_deaths[0:(iv-1)], main="dataIsrael", col="blue", type='l', ylab="positive rate")
lines(dataIsrael$new_deaths[iv:dim(dataIsrael)[1]], type='l', col="green")
plot(dataAmerica$new_deaths[0:(av-1)], main="dataAmerica", col="blue", type='l', ylab="positive rate")
lines(dataAmerica$new_deaths[av:dim(dataAmerica)[1]], type='l', col="green")
plot(dataJapan$new_deaths[0:(jv-1)], main="dataJapan", col="blue", type='l', ylab="positive rate")
lines(dataJapan$new_deaths[jv:dim(dataJapan)[1]], type='l', col="green")
plot(dataLuxembourg$new_deaths[0:(lv-1)], main="dataLuxembourg", col="blue", type='l', ylab="positive rate")
lines(dataLuxembourg$new_deaths[lv:dim(dataLuxembourg)[1]], type='l', col="green")




