# Obtain data from the Urban Institute Education Data Portal API
# Documentation: https://educationdata.urban.org/documentation/#r
# When citing data from the Education Data Portal in any research reports, briefs, or similar products, please cite both the portal and the dataset. For example:
#   
#   [dataset names], Education Data Portal (Version 0.23.0), Urban Institute, accessed Month, DD, YYYY, https://educationdata.urban.org/documentation/, made available under the ODC Attribution License.
# 
# Be sure to replace “0.23.0” with the version of the data you used for the research. When citing data from the Education Data Portal in blog posts, data visualizations, or other works in which the full citation above may unduly constrain available space, we recommend the following citation:
#   
#   [dataset names], via Education Data Portal v. 0.23.0, Urban Institute, under ODC Attribution License.
# 
# Again, be sure to replace “0.23.0” with the version of the data you used for the project. Datasets included in this portal are the US Department of Education Common Core of Data, the US Department of Education Civil Rights Data Collection, the US Census Bureau Small Area Income and Poverty Estimates, the US Department of Education Integrated Postsecondary Education Data System, the US Department of Education College Scorecard, and the National Historical Geographic Information System.
# 
# We highly encourage users to reach out to educationdata@urban.org to notify us of any public projects or research using these data, including providing information on the title and link to the published work, which can elevate the visibility of your work and help us make the case for continued funding.

library(educationdata)
data_08_18 <- get_education_data(level = "school-districts",
                           source = "edfacts",
                           topic = "assessments",
                           filters = list(year = 2009:2018, grade_edfacts = 8))


# Data only goes up to 2020 and data is missing for 2019 (likely Covid)
data <- get_education_data(level = "school-districts",
                                 source = "edfacts",
                                 topic = "assessments",
                                 filters = list(year = 2020, grade_edfacts = 8)) %>% 
  rbind(data_08_18)

