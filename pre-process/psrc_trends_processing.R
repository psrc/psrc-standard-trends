library(dplyr)
library(psrctrends)

transit_data <- process_ntd_data()
airport_data <- summarize_airport_data(yr1=2018)
jobs_data <- summarize_jobs_data(yr=2023, mo=12)
population_data <- get_population_data()
housing_data <- get_housing_data()
population_housing_near_hct_data <- pop_hsg_near_hct()
vehicle_registration_data <- process_vehicle_registration_data(dol_registration_file="C:/coding/Vehicle_Title_Transactions.csv")
census_data <- summarize_census_data(years=c(2022))

trend_data <- bind_rows(transit_data, airport_data, jobs_data, 
                        population_data, housing_data, population_housing_near_hct_data, 
                        vehicle_registration_data, census_data)

saveRDS(trend_data, "C:\\coding\\psrc-standard-trends\\data\\trend_data.rds")
