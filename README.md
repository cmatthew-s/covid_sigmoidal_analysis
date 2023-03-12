# Dispelling Misconceptions about the Effectiveness of COVID-19 Vaccines: A Statistical Analysis

## Overview
This statistical analysis aims to prove the benefits of vaccines towards the covid-19 virus. We found that the vacciness indeed is beneficiary towards the covid-19 virus. We utilize the sigmoidal analysis to determine the benefits of vaccines and we found that the covid-19 growth rate is significantly hihger after vaccination than before vaccination in the four country we choose (America, Japan, Israel, and Luxembourg)

## Data Description
The data itself we obtainned it from GitHub: https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv

## Analysis Results
### The number of new cases (after and before vaccines)
We observed that the number of new cases in each of the four countries increased significantly following the discovery of the Omicron variant. Therefore, we decided to utilize only the data before the emergence of Omicron to eliminate any potential bias in our model.
![israel_data](https://github.com/cmatthew-s/covid_sigmoidal_analysis/blob/main/images/Israel%20Data.png)
![japan_data](https://github.com/cmatthew-s/covid_sigmoidal_analysis/blob/main/images/Japan%20Data.png)
![luxembourg_data](https://github.com/cmatthew-s/covid_sigmoidal_analysis/blob/main/images/Luxembourg%20Data.png)
![united_states_data](https://github.com/cmatthew-s/covid_sigmoidal_analysis/blob/main/images/United%20States%20Data.png)

### The positives rate post omicron (green = after vaccines, blue = before vaccines)
Interestingly the positives rates (the number of postive tests divided by the total test taken daily) are significantly lower after vaccination than before vacination in all countries except for Japan
![positive_rates](https://github.com/cmatthew-s/covid_sigmoidal_analysis/blob/main/images/positives_rate.png)
