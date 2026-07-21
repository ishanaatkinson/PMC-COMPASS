
##### READ IN DATA #####


##### READ IN DATA #####
full_data <- readRDS(here::here(country_main_page, paste0(country_main_page, "_calibrated_scaled_site.rds")))


# convert all "-" and spaces into "_" to ease in analysis later on and remove any accents from admin-1 area names 
full_data$sites$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$sites$name_1), id = "Latin-ASCII")
full_data$prevalence$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$prevalence$name_1), id = "Latin-ASCII") 
full_data$interventions$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$interventions$name_1), id = "Latin-ASCII")
full_data$population$population_total$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$population$population_total$name_1), id = "Latin-ASCII") 
full_data$population$population_by_age$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$population$population_by_age$name_1), id = "Latin-ASCII") 
full_data$vectors$vector_species$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$vectors$vector_species$name_1), id = "Latin-ASCII")
full_data$vectors$pyrethroid_resistance$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$vectors$pyrethroid_resistance$name_1), id = "Latin-ASCII")
full_data$seasonality$seasonality_parameters$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$seasonality$seasonality_parameters$name_1), id = "Latin-ASCII")
full_data$seasonality$monthly_rainfall$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$seasonality$monthly_rainfall$name_1), id = "Latin-ASCII")
full_data$seasonality$fourier_prediction$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$seasonality$fourier_prediction$name_1), id = "Latin-ASCII")
full_data$eir$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$eir$name_1), id = "Latin-ASCII") 
full_data$blood_disorders$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$blood_disorders$name_1), id = "Latin-ASCII") 
full_data$accessibility$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$accessibility$name_1), id = "Latin-ASCII") 

full_data$sites$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$sites$name_2), id = "Latin-ASCII")
full_data$prevalence$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$prevalence$name_2), id = "Latin-ASCII") 
full_data$interventions$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$interventions$name_2), id = "Latin-ASCII")
full_data$population$population_by_age$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$population$population_by_age$name_2), id = "Latin-ASCII") 
full_data$population$population_total$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$population$population_total$name_2), id = "Latin-ASCII") 
full_data$vectors$vector_species$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$vectors$vector_species$name_2), id = "Latin-ASCII")
full_data$vectors$pyrethroid_resistance$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$vectors$pyrethroid_resistance$name_2), id = "Latin-ASCII")
full_data$pyrethroid_resistance$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$pyrethroid_resistance$name_2), id = "Latin-ASCII")
full_data$seasonality$seasonality_parameters$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$seasonality$seasonality_parameters$name_2), id = "Latin-ASCII")
full_data$seasonality$monthly_rainfall$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$seasonality$monthly_rainfall$name_2), id = "Latin-ASCII")
full_data$seasonality$fourier_prediction$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$seasonality$fourier_prediction$name_2), id = "Latin-ASCII")
full_data$eir$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$eir$name_2), id = "Latin-ASCII") 
full_data$blood_disorders$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$blood_disorders$name_2), id = "Latin-ASCII") 
full_data$accessibility$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$accessibility$name_2), id = "Latin-ASCII") 



list_of_possible_country_codes <- c("CMR", "MOZ", "CIV", "BEN")
area_names <- unique(full_data$sites$name_2)
admin1<-area_names


population_weights <- c()
rural_weights <- c()
urban_weights <- c()

pop_size_across_admin1 <- sum((full_data$population$population_total %>% filter(year==2023, name_2 %in% admin1))$pop)


for (i in 1:length(admin1)){
  
  pop <- sum((full_data$population$population_total %>% filter(year == 2023, name_2 == admin1[i]))$pop)
  
  pop_weight <- pop / pop_size_across_admin1
  population_weights <- c(population_weights, pop_weight)
  
  
  rural_pop <- (full_data$population$population_total %>% filter(year == 2023, name_2 == admin1[i], urban_rural == "rural"))$pop
  urban_pop <- (full_data$population$population_total %>% filter(year == 2023, name_2 == admin1[i], urban_rural == "urban"))$pop
  
  if (length(rural_pop) == 0) {
    rural_pop = 0
  }
  
  if (length(urban_pop) == 0) {
    urban_pop = 0
  }
  
  rural_weights <- c(rural_weights, rural_pop/pop)
  urban_weights <- c(urban_weights, urban_pop/pop)
}


##### GET DHS COVERAGE AND SCHEDULE DATA #####

# for 4,8,10,12 doses coverages 
#coverage_data <- read_xlsx("plusproject_countries_coverage_dhs.xlsx", sheet = "Ishana's edits - add'l doses")

# for default + comparison coverages
coverage_data <- read_xlsx("plusproject_countries_coverage_dhs.xlsx", sheet = "focus_countries")

# coverage data for given country
cov <- coverage_data %>% filter(country == country_main_page, default == "yes")

# vaccine schedule for given country
schedule <- unique((coverage_data %>% filter(country == country_main_page, default == "yes"))$vaccine)


vaccines <- c() # vaccines given 
vaccine_weeks <- c() # target age for each vaccine
vaccine_cov_rural <- vaccine_cov_urban <- c() # coverage of vaccine by rurality

coverage_df <- c()


for (i in 1:length(area_names)) {
  
  # same for each rural and urban
  vaccines <- unique((cov %>% filter(area == area_names[i]))$vaccine)
  vaccine_weeks <- as.numeric(unique((cov %>% filter(area == area_names[i]))$vaccine_weeks))
  
  
  # coverage of vaccine by rurality
  
  # assume 90% of children to uptake co-delivered vaccine will uptake PMC dose 
  
  if (length((cov %>% filter(area == area_names[i], rural_urban == "rural"))$coverage) == 0) {
    vaccine_cov_rural <- as.numeric(rep(0, times=length(unique(vaccine_weeks))))
  } else {
    # assumed 90% of DHS co-intervention coverage for PMC dose 
    vaccine_cov_rural <- 0.9*as.numeric((cov %>% filter(area == area_names[i], rural_urban == "rural"))$coverage)
  }
  
  if (length((cov %>% filter(area == area_names[i], rural_urban == "urban"))$coverage) == 0) {
    vaccine_cov_urban <- as.numeric(rep(0, times=length(unique(vaccine_weeks))))
  } else {
    # assumed 90% of DHS co-intervention coverage for PMC dose 
    vaccine_cov_urban <- 0.9*as.numeric((cov %>% filter(area == area_names[i], rural_urban == "urban"))$coverage)
  }
  
  
  # when no DHS data for a given rurality, assume same cov for the whole admin-1 area 
  if (all(vaccine_cov_rural) == 0) {
    vaccine_cov_rural <- vaccine_cov_urban
  }
  
  if (all(vaccine_cov_urban) == 0) {
    vaccine_cov_urban <- vaccine_cov_rural
  }
  
  for (j in 1:length(unique(vaccine_weeks))) {
    coverage_df <- rbind(coverage_df, c(area_names[i], vaccines[j], as.numeric(vaccine_weeks[j]), as.numeric(vaccine_cov_rural[j]), as.numeric(vaccine_cov_urban[j])))
  }
  
}


colnames(coverage_df) <- c("area", "vaccine", "vaccine_weeks", "cov_rural", "cov_urban")
coverage_df <- as.data.frame(coverage_df)
coverage_df$vaccine_weeks <- as.numeric(coverage_df$vaccine_weeks)
coverage_df$vaccine_days <- 7 * coverage_df$vaccine_weeks 


vaccine_timing <- unique(coverage_df$vaccine_weeks)

merged_admin1_cov <- c()


for (i in 1:length(area_names)){
  rural_cov <- as.numeric(((coverage_df %>% filter(area == area_names[i]))$cov_rural))
  urban_cov <- as.numeric(((coverage_df %>% filter(area == area_names[i]))$cov_urban))
  
  # when no DHS data for a given rurality, assume same cov for the whole admin-1 area 
  if (all(rural_cov) == 0) {
    rural_cov <- urban_cov
  }
  
  if (all(urban_cov) == 0) {
    urban_cov <- rural_cov
  }
  
  
  merged_admin1_cov <- c(merged_admin1_cov, ((rural_cov*rural_weights[i]) + (urban_cov*urban_weights[i])))
  
  
}


coverage_df$cov_merged <- rep(merged_admin1_cov)


overall_country_cov <- c()

for (i in 1:length(unique(coverage_df$vaccine_weeks))) {
  current_vaccine_data <- (coverage_df %>% filter(vaccine_weeks == vaccine_timing[i]))$cov_merged
  
  overall_country_cov <- c(overall_country_cov, sum(population_weights*current_vaccine_data))
  
}

overall_country_cov <- data.frame(cov = overall_country_cov)
overall_country_cov$country <- rep(country_main_page, times = length(unique(coverage_df$vaccine_weeks)))
overall_country_cov$vaccine <- unique(coverage_df$vaccine)
overall_country_cov$vaccine_weeks <- unique(coverage_df$vaccine_weeks)
overall_country_cov$vaccine_days <- overall_country_cov$vaccine_weeks * 7 

