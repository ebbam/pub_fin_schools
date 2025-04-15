# Obtain data from the Urban Institute Education Data Portal API

library(educationdata)
data <- get_education_data(level = "school-districts",
                           source = "edfacts",
                           topic = "assessments",
                           filters = list(year = 2014, grade_edfacts = 8))

