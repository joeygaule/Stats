# Load necessary libraries
library(tidycensus)
library(dplyr)
library(ggplot2)
library(sf)
library(MASS)
library(lmtest)
library(sandwich)
library(tidyverse)
library(car)

# Set the Census API key
census_api_key('5e6ad1def43ec9f863aeb1ce3690e000014f095d', install = TRUE)

# Fetch the ACS data for Cook County, IL, with specified variables
cook_county_data <- get_acs(
  geography = "tract", 
  variables = c('DP05_0001E','DP05_0018E','DP03_0062E','DP02_0065PE','DP03_0096PE','DP03_0128PE','DP04_0047PE'), 
  year = 2019, output = "wide", geometry = TRUE,
  state = "IL", county = "Cook County", survey = "acs5"
)

# Remove margin of error columns and rename for clarity while preserving the geometry column
geometry_col <- st_geometry(cook_county_data)
cook_county_data <- subset(cook_county_data, select = -grep("M$", names(cook_county_data)))
names(cook_county_data)[names(cook_county_data) %in% c('DP05_0001E','DP05_0018E','DP03_0062E',
                                                       'DP02_0065PE','DP03_0096PE','DP03_0128PE',
                                                       'DP04_0047PE')] <- c('Population', 'MedianAge', 
                                                                            'HouseholdIncome', 'BachelorsRate', 
                                                                            'InsuranceCoverage', 'PovertyRate', 
                                                                            'RentalRate')
st_geometry(cook_county_data) <- geometry_col



# Q5: Build a base model
base_model <- lm(BachelorsRate ~ log(HouseholdIncome) + I(MedianAge^2) + PovertyRate, data = cook_county_data)
summary(base_model)

# Q6: Create scatterplots for each predictor variable in the base model
# Example for Household Income
ggplot(data = cook_county_data, aes(x = log(HouseholdIncome), y = BachelorsRate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Household Income and Baccalaureate Attainment Rate") +
  theme_minimal()

# Q7: Perform diagnostic tests for the base model
# Normality Test
shapiro.test(residuals(base_model))

# Autocorrelation Test
durbinWatsonTest(base_model)

# Heteroskedasticity Test
bptest(base_model)

# Q8: Use stepwise regression to select variables (forward selection)
step_model <- step(lm(BachelorsRate ~ 1, data = cook_county_data), 
                   direction = 'forward', 
                   scope = ~ log(Population) + I(MedianAge^2) + log(HouseholdIncome) +
                     InsuranceCoverage + log(PovertyRate) + RentalRate)

# Build the final model with selected variables and an interaction term
final_model <- lm(BachelorsRate ~ log(InsuranceCoverage) + I(MedianAge^2) * log(PovertyRate) + RentalRate, 
                  data = cook_county_data)
summary(final_model)

# Q9: Apply a hypothetical 'Robin Hood' tax policy effect and predict with the final model
cook_county_data$adjustedHouseholdIncome <- with(cook_county_data, ifelse(HouseholdIncome > quantile(HouseholdIncome, 0.9), HouseholdIncome - 10000, ifelse(HouseholdIncome < quantile(HouseholdIncome, 0.1), HouseholdIncome + 10000, HouseholdIncome)))
cook_county_data$log_adjustedHouseholdIncome <- log(cook_county_data$adjustedHouseholdIncome)

# Fit the model with adjusted income
adjusted_final_model <- update(final_model, data = cook_county_data)

# Predict the BachelorsRate using the adjusted model
predicted_bachelors_rate <- predict(adjusted_final_model, newdata = cook_county_data)
mean(predicted_bachelors_rate)
