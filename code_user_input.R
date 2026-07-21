starttime<-Sys.time()


df <- dose_data()

schedule <- round(as.numeric(df$`Age (months`) * 30.4167, 0)
cov <- as.numeric(df$`Coverage (0-100%)`) / 100


# Get the order of indices that would sort schedule_additional_doses
sorted_indices <- order(schedule)

# Use sorted_indices to reorder both vectors
sorted_schedule <- schedule[sorted_indices]
sorted_cov <- cov[sorted_indices]


schedule <- sorted_schedule
cov <- sorted_cov

incProgress(1/10)


country_lookup <- c(
  AGO = "Angola",
  BDI = "Burundi",
  BEN = "Benin",
  BFA = "Burkina Faso",
  CAF = "Central African Republic",
  CIV = "Cote d'Ivoire",
  CMR = "Cameroon",
  COD = "DR Congo",
  COG = "Congo-Brazzaville",
  GAB = "Gabon",
  GHA = "Ghana",
  GIN = "Guinea",
  GNQ = "Equatorial Guinea",
  KEN = "Kenya",
  LBR = "Liberia",
  MDG = "Madagascar",
  MLI = "Mali",
  MOZ = "Mozambique",
  MWI = "Malawi",
  NER = "Niger",
  NGA = "Nigeria",
  SLE = "Sierra Leone",
  SSD = "South Sudan",
  TCD = "Chad",
  TGO = "Togo",
  TZA = "Tanzania",
  UGA = "Uganda",
  ZMB = "Zambia"
)

get_iso3 <- function(country_name, lookup = country_lookup) {
  names(lookup)[lookup == country_name]
}

# 
# 
# ##### READ IN DATA #####
# full_data <- readRDS(here::here(country, paste0(country, "_calibrated_scaled_site.rds")))
# 
# # convert all "-" and spaces into "_" to ease in analysis later on and remove any accents from admin-1 area names 
# full_data$sites$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$sites$name_1), id = "Latin-ASCII")
# full_data$prevalence$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$prevalence$name_1), id = "Latin-ASCII") 
# full_data$interventions$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$interventions$name_1), id = "Latin-ASCII")
# full_data$population$population_total$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$population$population_total$name_1), id = "Latin-ASCII") 
# full_data$population$population_by_age$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$population$population_by_age$name_1), id = "Latin-ASCII") 
# full_data$vectors$vector_species$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$vectors$vector_species$name_1), id = "Latin-ASCII")
# full_data$vectors$pyrethroid_resistance$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$vectors$pyrethroid_resistance$name_1), id = "Latin-ASCII")
# full_data$seasonality$seasonality_parameters$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$seasonality$seasonality_parameters$name_1), id = "Latin-ASCII")
# full_data$seasonality$monthly_rainfall$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$seasonality$monthly_rainfall$name_1), id = "Latin-ASCII")
# full_data$seasonality$fourier_prediction$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$seasonality$fourier_prediction$name_1), id = "Latin-ASCII")
# full_data$eir$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$eir$name_1), id = "Latin-ASCII") 
# full_data$blood_disorders$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$blood_disorders$name_1), id = "Latin-ASCII") 
# full_data$accessibility$name_2 <- stri_trans_general(str=gsub("-", "_", full_data$accessibility$name_1), id = "Latin-ASCII") 
# 
# full_data$sites$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$sites$name_2), id = "Latin-ASCII")
# full_data$prevalence$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$prevalence$name_2), id = "Latin-ASCII") 
# full_data$interventions$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$interventions$name_2), id = "Latin-ASCII")
# full_data$population$population_by_age$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$population$population_by_age$name_2), id = "Latin-ASCII") 
# full_data$population$population_total$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$population$population_total$name_2), id = "Latin-ASCII") 
# full_data$vectors$vector_species$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$vectors$vector_species$name_2), id = "Latin-ASCII")
# full_data$vectors$pyrethroid_resistance$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$vectors$pyrethroid_resistance$name_2), id = "Latin-ASCII")
# full_data$pyrethroid_resistance$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$pyrethroid_resistance$name_2), id = "Latin-ASCII")
# full_data$seasonality$seasonality_parameters$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$seasonality$seasonality_parameters$name_2), id = "Latin-ASCII")
# full_data$seasonality$monthly_rainfall$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$seasonality$monthly_rainfall$name_2), id = "Latin-ASCII")
# full_data$seasonality$fourier_prediction$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$seasonality$fourier_prediction$name_2), id = "Latin-ASCII")
# full_data$eir$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$eir$name_2), id = "Latin-ASCII") 
# full_data$blood_disorders$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$blood_disorders$name_2), id = "Latin-ASCII") 
# full_data$accessibility$name_2 <- stri_trans_general(str=gsub(" ", "_", full_data$accessibility$name_2), id = "Latin-ASCII") 
# 
# area_names <- unique(full_data$sites$name_2)

##### PRINT ERROR MESSAGES #####
if (length(schedule) != length(cov)) {
  print("Number of PMC doses does not equal number of coverage values inputted.")
}



#incProgress(1/10)

##### 7. COMMON PARAMETERS/VARIABLES ##### 

human_population <- 5e5 # population size in model 
min_age_to_model <- 0 # minimum age to model (in days)
max_age_to_model <- 2.5*365 # enter in multiples of 0.5, maximum age to model (in days)

# set the time span over which to simulate
# NOTE: currently 23 years is the max as we only have ITN/IRS data for this length
year <- 365; years <- 25; sim_length_for_data <- year * years #24 years for total, 1 year for daily

years_proj_forward <- 0 # number of years to project forward following known data
years_of_simulation <- years + years_proj_forward # simulation length (years) 
sim_length <- (years + years_proj_forward) * year # simulation length (days)

# interval between modeled age groups (1 = days, 7 = weeks etc)
step_length <- 7

# vector of min and max ages to be used in each age bracket
age_min <- seq(min_age_to_model, max_age_to_model, step_length)
age_max <- age_min + 6

#seq(min_age_to_model+2, max_age_to_model+2, step_length) - 1

# age_min <- c(0, 0)
# age_max <- c(913, 1825)

# six month ages (in days)
sixmonth_intervals <- c(0, 183, 365, 548, 730, 913, 1095, 1278, 1460, 1643,
                        1825, 2008, 2190, 2373, 2555, 2738, 2920, 3285, 3468,
                        3650, 4015)

# time steps used in model (number of rows in the simulations output data frame)
timesteps<-seq(0,round(sim_length),1)

# vector for the midpoint age for each age bracket (useful for graphing)
age_in_days <- seq(from=3, to=max(age_max), by=7)
age_in_days_midpoint <- age_in_days # age_in_days[-length(age_in_days)] + diff(age_in_days)/2

#age_in_days_midpoint <- c(456, 913)


# number of 6 month interval age groups to model 
no_sixmonth_intervals <- round(max(age_max)/182.5)

# empty vectors for the column names of interest 
age_group_names <- c() # age groups
age_group_names_sixmonth <- c() # 6 month age groups
sixmonth_intervals_midpoint <- c() # 6 month age group midpoints
clin_inc_cols <- c() # clinical incidence column names
sev_inc_cols <- c() # severe incidence column names
tot_inc_cols <- c() # total incidence column names
asym_inc_cols <- c() # asymptomatic incidence column names

# fill empty vectors
for (i in 1:(length(age_min))) {
  age_group_names <- append(age_group_names, paste0("n_age_", as.character(age_min[i]), "_", as.character(age_max[i])))
}

for (i in 1:(length(sixmonth_intervals) - 1)) {
  
  if (i == (length(sixmonth_intervals) - 1)) {
    age_group_names_sixmonth <- append(age_group_names_sixmonth, paste0("n_age_", as.character(sixmonth_intervals[i]), "_", as.character(sixmonth_intervals[i+1])))
    sixmonth_intervals_midpoint <- append(sixmonth_intervals_midpoint, (sixmonth_intervals[i]+sixmonth_intervals[i+1])/2)
  }
  
  if (i != (length(sixmonth_intervals) - 1)) {
    age_group_names_sixmonth <- append(age_group_names_sixmonth, paste0("n_age_", as.character(sixmonth_intervals[i]), "_", as.character(sixmonth_intervals[i+1] - 1)))
    sixmonth_intervals_midpoint <- append(sixmonth_intervals_midpoint, (sixmonth_intervals[i]+(sixmonth_intervals[i+1] - 1))/2)
  }
  
}

# write column names 
for (i in 1:length(age_min)) {
  clin_inc_cols <- append(clin_inc_cols, paste0("n_inc_clinical_", as.character(age_min[i]), "_", as.character(age_max[i])))
  sev_inc_cols <- append(sev_inc_cols, paste0("n_inc_severe_", as.character(age_min[i]), "_", as.character(age_max[i])))
  tot_inc_cols <- append(tot_inc_cols, paste0("n_inc_", as.character(age_min[i]), "_", as.character(age_max[i])))
  asym_inc_cols <- append(asym_inc_cols, paste0("n_inc_asym_", as.character(age_min[i]), "_", as.character(age_max[i])))
}

##### READ IN IMPERIAL MODEL OUTPUT #####

all_sites_for_DT <- vroom::vroom(("files_needed/all_sites_for_DT.csv")) 
incidence_ppy_df <- vroom::vroom(paste0("", get_iso3(country), "/sim_results_DHS_coverage_levels/incidence_ppy_df_merged.csv"))

#incProgress(1/10)

incidence_ppy_df_whole_country <- vroom::vroom(paste0("", get_iso3(country), "/sim_results_DHS_coverage_levels/incidence_ppy_df_whole_country.csv"))

population_df_age_structure <- vroom::vroom(paste0("", get_iso3(country), "/sim_results_DHS_coverage_levels/population_df_age_structure.csv"))

starttime2 <- Sys.time()



##### READ IN PROPORTIONS OF HAPLOTYPES #####

if (input$country_or_area == "Whole country") {
  area_names<-unique(incidence_ppy_df$`Admin-1 unit`)
} else {
  area_names <- chosen_area
}

incProgress(1/10)
##### EFFICACY CURVES WEIBULL PARAMETERS #####

# weibull scale parameters for each haplotype
lambda_trip<-59.57659
lambda_quadr<-33.05391
lambda_quint<-18.55328
lambda_sext<-12.81186
lambda_other<-23
lambda_VAGKAA<-22 # assumed to be 20.1 days protection
lambda_VAGKGS<-18.55328 # assumed to be same as the QUINT

# weibull shape parameters for each haplotype
w_trip<- 8.435971
w_quadr<-4.862126
w_quint<-2.4840752
w_sext<-3.691953
w_other<-4.5
w_VAGKAA<-4.5
w_VAGKGS<- 2.4840752


##### SET UP EFFICACY CURVED (NORMAL PMC SCHEDULE) #####

# # set up df with the timesteps
# if (sim_length > length(age_in_days_midpoint)) {
#   df<- data.frame(time=1:sim_length)
# }

#if (sim_length < length(age_in_days_midpoint)) {
  df<- data.frame(time=1:913)
#}



# initalise empty columns
df$prot_trip<-NA
df$prot_quadr<-NA
df$prot_quint<-NA
df$prot_sext<-NA
df$prot_other<-NA
df$prot_VAGKAA<-NA
df$prot_VAGKGS<-NA


#incProgress(1/10)

# construct weibull curves to model the efficacy of SP through time for
# each haplotype

if (length(schedule) > 0) {
  for (t in 1: schedule[1]) {  # day 0 to dose 1 on day 70
    df$prot_trip[t]<-0
    df$prot_quadr[t]<-0
    df$prot_quint[t]<-0
    df$prot_sext[t]<- 0
    df$prot_other[t]<-0
    df$prot_VAGKAA[t]<- 0
    df$prot_VAGKGS[t]<- 0
  }
} else {
  for (t in (schedule[length(schedule)]+1) : nrow(df))  {
    df$prot_trip[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov[length(cov)]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov[length(cov)]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov[length(cov)]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov[length(cov)]
    df$prot_other[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov[length(cov)]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov[length(cov)]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov[length(cov)]
  }
}


if (length(schedule) > 1) {
  for (t in (schedule[1]+1) : schedule[2])  {   # day 71 to dose 2 on day 98
    df$prot_trip[t]<- exp(-(df$time[t-schedule[1]]/lambda_trip)^w_trip) * cov[1]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[1]]/lambda_quadr)^w_quadr)* cov[1]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[1]]/lambda_quint)^w_quint)* cov[1]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[1]]/lambda_sext)^w_sext)* cov[1]
    df$prot_other[t]<- exp(-(df$time[t-schedule[1]]/lambda_other)^w_other)* cov[1]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[1]]/lambda_VAGKAA)^w_VAGKAA)* cov[1]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[1]]/lambda_VAGKGS)^w_VAGKGS)* cov[1]
  }
} else {
  for (t in (schedule[length(schedule)]+1) : nrow(df))  {
    df$prot_trip[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov[length(cov)]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov[length(cov)]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov[length(cov)]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov[length(cov)]
    df$prot_other[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov[length(cov)]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov[length(cov)]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov[length(cov)]
  }
}

if (length(schedule) > 2) {
  for (t in (schedule[2]+1) : schedule[3])  {  #  day 99 to dose 3 on day 180
    df$prot_trip[t]<- exp(-(df$time[t-schedule[2]]/lambda_trip)^w_trip)* cov[2]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[2]]/lambda_quadr)^w_quadr)* cov[2]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[2]]/lambda_quint)^w_quint)* cov[2]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[2]]/lambda_sext)^w_sext)* cov[2]
    df$prot_other[t]<- exp(-(df$time[t-schedule[2]]/lambda_other)^w_other)* cov[2]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[2]]/lambda_VAGKAA)^w_VAGKAA)* cov[2]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[2]]/lambda_VAGKGS)^w_VAGKGS)* cov[2]
  }
} else {
  for (t in (schedule[length(schedule)]+1) : nrow(df))  {
    df$prot_trip[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov[length(cov)]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov[length(cov)]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov[length(cov)]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov[length(cov)]
    df$prot_other[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov[length(cov)]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov[length(cov)]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov[length(cov)]
  }
}



if (length(schedule) > 3) {
  for (t in (schedule[3]+1) : schedule[4])  {  # day 181 to dose 4 on day 270
    df$prot_trip[t]<- exp(-(df$time[t-schedule[3]]/lambda_trip)^w_trip)* cov[3]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[3]]/lambda_quadr)^w_quadr)* cov[3]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[3]]/lambda_quint)^w_quint)* cov[3]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[3]]/lambda_sext)^w_sext)* cov[3]
    df$prot_other[t]<- exp(-(df$time[t-schedule[3]]/lambda_other)^w_other)* cov[3]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[3]]/lambda_VAGKAA)^w_VAGKAA)* cov[3]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[3]]/lambda_VAGKGS)^w_VAGKGS)* cov[3]
  }
} else {
  for (t in (schedule[length(schedule)]+1) : nrow(df))  {
    df$prot_trip[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov[length(cov)]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov[length(cov)]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov[length(cov)]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov[length(cov)]
    df$prot_other[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov[length(cov)]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov[length(cov)]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov[length(cov)]
  }
}

if (length(schedule) > 4) {
  for (t in (schedule[4]+1) : schedule[5])  {  # day 271 to dose 5 on day 360
    df$prot_trip[t]<- exp(-(df$time[t-schedule[4]]/lambda_trip)^w_trip)* cov[4]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[4]]/lambda_quadr)^w_quadr)* cov[4]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[4]]/lambda_quint)^w_quint)* cov[4]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[4]]/lambda_sext)^w_sext)* cov[4]
    df$prot_other[t]<- exp(-(df$time[t-schedule[4]]/lambda_other)^w_other)* cov[4]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[4]]/lambda_VAGKAA)^w_VAGKAA)* cov[4]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[4]]/lambda_VAGKGS)^w_VAGKGS)* cov[4]
  }
} else {
  for (t in (schedule[length(schedule)]+1) : nrow(df))  {
    df$prot_trip[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov[length(cov)]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov[length(cov)]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov[length(cov)]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov[length(cov)]
    df$prot_other[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov[length(cov)]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov[length(cov)]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov[length(cov)]
  }
}

if (length(schedule) > 5) {
  for (t in (schedule[5]+1) : schedule[6])  {  # day 361 to dose 6 on day 450
    df$prot_trip[t]<- exp(-(df$time[t-schedule[5]]/lambda_trip)^w_trip)* cov[5]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[5]]/lambda_quadr)^w_quadr)* cov[5]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[5]]/lambda_quint)^w_quint)* cov[5]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[5]]/lambda_sext)^w_sext)* cov[5]
    df$prot_other[t]<- exp(-(df$time[t-schedule[5]]/lambda_other)^w_other)* cov[5]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[5]]/lambda_VAGKAA)^w_VAGKAA)* cov[5]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[5]]/lambda_VAGKGS)^w_VAGKGS)* cov[5]
  }
} else {
  for (t in (schedule[length(schedule)]+1) : nrow(df))  {
    df$prot_trip[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov[length(cov)]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov[length(cov)]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov[length(cov)]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov[length(cov)]
    df$prot_other[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov[length(cov)]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov[length(cov)]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov[length(cov)]
  }
}



if (length(schedule) > 6) {
  for (t in (schedule[6]+1) : schedule[7])  {  # day 451 to dose 7 on day 540
    df$prot_trip[t]<- exp(-(df$time[t-schedule[6]]/lambda_trip)^w_trip)* cov[6]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[6]]/lambda_quadr)^w_quadr)* cov[6]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[6]]/lambda_quint)^w_quint)* cov[6]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[6]]/lambda_sext)^w_sext)* cov[6]
    df$prot_other[t]<- exp(-(df$time[t-schedule[6]]/lambda_other)^w_other)* cov[6]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[6]]/lambda_VAGKAA)^w_VAGKAA)* cov[6]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[6]]/lambda_VAGKGS)^w_VAGKGS)* cov[6]
  }
} else {
  for (t in (schedule[length(schedule)]+1) : nrow(df))  {
    df$prot_trip[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov[length(cov)]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov[length(cov)]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov[length(cov)]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov[length(cov)]
    df$prot_other[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov[length(cov)]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov[length(cov)]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov[length(cov)]
  }
}

if (length(schedule) > 7) {
  for (t in (schedule[7]+1) : schedule[8])  {  # day 541 to dose 8 on day 720
    df$prot_trip[t]<- exp(-(df$time[t-schedule[7]]/lambda_trip)^w_trip)* cov[7]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[7]]/lambda_quadr)^w_quadr)* cov[7]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[7]]/lambda_quint)^w_quint)* cov[7]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[7]]/lambda_sext)^w_sext)* cov[7]
    df$prot_other[t]<- exp(-(df$time[t-schedule[7]]/lambda_other)^w_other)* cov[7]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[7]]/lambda_VAGKAA)^w_VAGKAA)* cov[7]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[7]]/lambda_VAGKGS)^w_VAGKGS)* cov[7]
  }
} else {
  for (t in (schedule[length(schedule)]+1) : nrow(df))  {
    df$prot_trip[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov[length(cov)]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov[length(cov)]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov[length(cov)]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov[length(cov)]
    df$prot_other[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov[length(cov)]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov[length(cov)]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov[length(cov)]
  }
}

incProgress(1/10)

if (length(schedule) > 8) {
  for (t in (schedule[8]+1) : nrow(df))  {  # day 721 to end of simulation
    df$prot_trip[t]<- exp(-(df$time[t-schedule[8]]/lambda_trip)^w_trip)* cov[8]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[8]]/lambda_quadr)^w_quadr)* cov[8]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[8]]/lambda_quint)^w_quint)* cov[8]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[8]]/lambda_sext)^w_sext)* cov[8]
    df$prot_other[t]<- exp(-(df$time[t-schedule[8]]/lambda_other)^w_other)* cov[8]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[8]]/lambda_VAGKAA)^w_VAGKAA)* cov[8]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[8]]/lambda_VAGKGS)^w_VAGKGS)* cov[8]
  }
} else {
  for (t in (schedule[length(schedule)]+1) : nrow(df))  {
    df$prot_trip[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov[length(cov)]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov[length(cov)]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov[length(cov)]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov[length(cov)]
    df$prot_other[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov[length(cov)]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov[length(cov)]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov[length(cov)]
  }
}

if (length(schedule) > 9) {
  for (t in (schedule[9]+1) : nrow(df))  {  # day 721 to end of simulation
    df$prot_trip[t]<- exp(-(df$time[t-schedule[9]]/lambda_trip)^w_trip)* cov[9]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[9]]/lambda_quadr)^w_quadr)* cov[9]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[9]]/lambda_quint)^w_quint)* cov[9]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[9]]/lambda_sext)^w_sext)* cov[9]
    df$prot_other[t]<- exp(-(df$time[t-schedule[9]]/lambda_other)^w_other)* cov[9]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[9]]/lambda_VAGKAA)^w_VAGKAA)* cov[9]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[9]]/lambda_VAGKGS)^w_VAGKGS)* cov[9]
  }
} else {
  for (t in (schedule[length(schedule)]+1) : nrow(df))  {
    df$prot_trip[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov[length(cov)]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov[length(cov)]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov[length(cov)]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov[length(cov)]
    df$prot_other[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov[length(cov)]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov[length(cov)]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov[length(cov)]
  }
}

if (length(schedule) > 10) {
  for (t in (schedule[10]+1) : nrow(df))  {  # day 721 to end of simulation
    df$prot_trip[t]<- exp(-(df$time[t-schedule[10]]/lambda_trip)^w_trip)* cov[10]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[10]]/lambda_quadr)^w_quadr)* cov[10]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[10]]/lambda_quint)^w_quint)* cov[10]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[10]]/lambda_sext)^w_sext)* cov[10]
    df$prot_other[t]<- exp(-(df$time[t-schedule[10]]/lambda_other)^w_other)* cov[10]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[10]]/lambda_VAGKAA)^w_VAGKAA)* cov[10]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[10]]/lambda_VAGKGS)^w_VAGKGS)* cov[10]
  }
} else {
  for (t in (schedule[length(schedule)]+1) : nrow(df))  {
    df$prot_trip[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov[length(cov)]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov[length(cov)]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov[length(cov)]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov[length(cov)]
    df$prot_other[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov[length(cov)]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov[length(cov)]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov[length(cov)]
  }
}

if (length(schedule) > 11) {
  for (t in (schedule[11]+1) : nrow(df))  {  # day 721 to end of simulation
    df$prot_trip[t]<- exp(-(df$time[t-schedule[11]]/lambda_trip)^w_trip)* cov[11]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[11]]/lambda_quadr)^w_quadr)* cov[11]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[11]]/lambda_quint)^w_quint)* cov[11]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[11]]/lambda_sext)^w_sext)* cov[11]
    df$prot_other[t]<- exp(-(df$time[t-schedule[11]]/lambda_other)^w_other)* cov[11]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[11]]/lambda_VAGKAA)^w_VAGKAA)* cov[11]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[11]]/lambda_VAGKGS)^w_VAGKGS)* cov[11]
  }
} else {
  for (t in (schedule[length(schedule)]+1) : nrow(df))  {
    df$prot_trip[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov[length(cov)]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov[length(cov)]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov[length(cov)]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov[length(cov)]
    df$prot_other[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov[length(cov)]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov[length(cov)]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov[length(cov)]
  }
}

if (length(schedule) > 12) {
  for (t in (schedule[12]+1) : nrow(df))  {  # day 721 to end of simulation
    df$prot_trip[t]<- exp(-(df$time[t-schedule[12]]/lambda_trip)^w_trip)* cov[12]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[12]]/lambda_quadr)^w_quadr)* cov[12]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[12]]/lambda_quint)^w_quint)* cov[12]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[12]]/lambda_sext)^w_sext)* cov[12]
    df$prot_other[t]<- exp(-(df$time[t-schedule[12]]/lambda_other)^w_other)* cov[12]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[12]]/lambda_VAGKAA)^w_VAGKAA)* cov[12]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[12]]/lambda_VAGKGS)^w_VAGKGS)* cov[12]
  }
} else {
  for (t in (schedule[length(schedule)]+1) : nrow(df))  {
    df$prot_trip[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov[length(cov)]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov[length(cov)]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov[length(cov)]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov[length(cov)]
    df$prot_other[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov[length(cov)]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov[length(cov)]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov[length(cov)]
  }
}



if (length(schedule) > 13) {
  for (t in (schedule[13]+1) : nrow(df))  {  # day 721 to end of simulation
    df$prot_trip[t]<- exp(-(df$time[t-schedule[13]]/lambda_trip)^w_trip)* cov[13]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[13]]/lambda_quadr)^w_quadr)* cov[13]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[13]]/lambda_quint)^w_quint)* cov[13]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[13]]/lambda_sext)^w_sext)* cov[13]
    df$prot_other[t]<- exp(-(df$time[t-schedule[13]]/lambda_other)^w_other)* cov[13]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[13]]/lambda_VAGKAA)^w_VAGKAA)* cov[13]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[13]]/lambda_VAGKGS)^w_VAGKGS)* cov[13]
  }
} else {
  for (t in (schedule[length(schedule)]+1) : nrow(df))  {
    df$prot_trip[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov[length(cov)]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov[length(cov)]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov[length(cov)]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov[length(cov)]
    df$prot_other[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov[length(cov)]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov[length(cov)]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov[length(cov)]
  }
}

if (length(schedule) > 14) {
  for (t in (schedule[14]+1) : nrow(df))  {  # day 721 to end of simulation
    df$prot_trip[t]<- exp(-(df$time[t-schedule[14]]/lambda_trip)^w_trip)* cov[14]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[14]]/lambda_quadr)^w_quadr)* cov[14]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[14]]/lambda_quint)^w_quint)* cov[14]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[14]]/lambda_sext)^w_sext)* cov[14]
    df$prot_other[t]<- exp(-(df$time[t-schedule[14]]/lambda_other)^w_other)* cov[14]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[14]]/lambda_VAGKAA)^w_VAGKAA)* cov[14]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[14]]/lambda_VAGKGS)^w_VAGKGS)* cov[14]
  }
} else {
  for (t in (schedule[length(schedule)]+1) : nrow(df))  {
    df$prot_trip[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov[length(cov)]
    df$prot_quadr[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov[length(cov)]
    df$prot_quint[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov[length(cov)]
    df$prot_sext[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov[length(cov)]
    df$prot_other[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov[length(cov)]
    df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov[length(cov)]
    df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov[length(cov)]
  }
}
# 
# if (length(schedule) > 15) {
#   for (t in (schedule[15]+1) : nrow(df))  {  # day 721 to end of simulation
#     df$prot_trip[t]<- exp(-(df$time[t-schedule[15]]/lambda_trip)^w_trip)* cov[15]
#     df$prot_quadr[t]<- exp(-(df$time[t-schedule[15]]/lambda_quadr)^w_quadr)* cov[15]
#     df$prot_quint[t]<- exp(-(df$time[t-schedule[15]]/lambda_quint)^w_quint)* cov[15]
#     df$prot_sext[t]<- exp(-(df$time[t-schedule[15]]/lambda_sext)^w_sext)* cov[15]
#     df$prot_other[t]<- exp(-(df$time[t-schedule[15]]/lambda_other)^w_other)* cov[15]
#     df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[15]]/lambda_VAGKAA)^w_VAGKAA)* cov[15]
#     df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[15]]/lambda_VAGKGS)^w_VAGKGS)* cov[15]
#   }
# } else {
#   for (t in (schedule[length(schedule)]+1) : nrow(df))  {
#     df$prot_trip[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov[length(cov)]
#     df$prot_quadr[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov[length(cov)]
#     df$prot_quint[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov[length(cov)]
#     df$prot_sext[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov[length(cov)]
#     df$prot_other[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov[length(cov)]
#     df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov[length(cov)]
#     df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov[length(cov)]
#   }
# }
# 
# if (length(schedule) > 16) {
#   for (t in (schedule[16]+1) : nrow(df))  {  # day 721 to end of simulation
#     df$prot_trip[t]<- exp(-(df$time[t-schedule[16]]/lambda_trip)^w_trip)* cov[16]
#     df$prot_quadr[t]<- exp(-(df$time[t-schedule[16]]/lambda_quadr)^w_quadr)* cov[16]
#     df$prot_quint[t]<- exp(-(df$time[t-schedule[16]]/lambda_quint)^w_quint)* cov[16]
#     df$prot_sext[t]<- exp(-(df$time[t-schedule[16]]/lambda_sext)^w_sext)* cov[16]
#     df$prot_other[t]<- exp(-(df$time[t-schedule[16]]/lambda_other)^w_other)* cov[16]
#     df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[16]]/lambda_VAGKAA)^w_VAGKAA)* cov[16]
#     df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[16]]/lambda_VAGKGS)^w_VAGKGS)* cov[16]
#   }
# } else {
#   for (t in (schedule[length(schedule)]+1) : nrow(df))  {
#     df$prot_trip[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_trip)^w_trip)* cov[length(cov)]
#     df$prot_quadr[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quadr)^w_quadr)* cov[length(cov)]
#     df$prot_quint[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_quint)^w_quint)* cov[length(cov)]
#     df$prot_sext[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_sext)^w_sext)* cov[length(cov)]
#     df$prot_other[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_other)^w_other)* cov[length(cov)]
#     df$prot_VAGKAA[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKAA)^w_VAGKAA)* cov[length(cov)]
#     df$prot_VAGKGS[t]<- exp(-(df$time[t-schedule[length(schedule)]]/lambda_VAGKGS)^w_VAGKGS)* cov[length(cov)]
#   }
# }


endtime2 <- Sys.time()

print(endtime2 - starttime2)


incProgress(1/10)

##### EFFICACY CURVE GRAPHS #####

colors <- c("I_AKA_" = "#00C094", "I_GKA_" = "#00B6EB", "I_GEA_" = "#FFA500",
            "I_GEG_" = "#F8766D" , "V_GKA_"= "#b7a1ff", "V_GKG_"= "#7361b3", "Other" = "#D2B48C")



##### CALCULATE OVERALL EFFICACY FOR EACH ADMIN-1 AREA #####

# loop over each admin-1 area and calculate weighted mean for overall efficacy
# based on proportion of each haplotype present

if (input$change_haplotype_data=="Yes") {
  haplotype_data_final$`Admin-1 unit` <- gsub(" ", "_", haplotype_data_final$`Admin-1 unit`)
}


for (i in 1:length(area_names)){
  
  proportions <- haplotype_data_final %>% filter(Country == country, `Admin-1 unit` == area_names[i])
  
  if (dim(proportions)[1] != 0) {
    
    df[paste0("prot_overall_", area_names[i])] <- as.double(proportions$I_AKA_)*df$prot_trip +
      as.double(proportions$I_GKA_)*df$prot_quadr +
      as.double(proportions$I_GEA_)*df$prot_quint +
      as.double(proportions$I_GEG_)*df$prot_sext +
      as.double(proportions$V_GKA_)*df$prot_VAGKAA +
      as.double(proportions$V_GKG_)*df$prot_VAGKGS +
      as.double(proportions$Other)*df$prot_other
  }
  
}

#incProgress(1/10)

##### CALCULATE INCIDENCE FOR WHOLE COUNTRY ##### 


if (input$country_or_area == "Whole country") {

population_weights <- c()

#pop_size_across_admin1 <- sum((full_data$population$population_total %>% filter(year==2023, name_2 %in% area_names))$pop)
pop_size_across_admin1 <- sum((all_sites_for_DT %>% filter(Country==input$country))$population_total)



for (i in 1:length(area_names)){

  pop <- (all_sites_for_DT %>% filter(Country==input$country, `Admin-1 unit`==area_names[i]))$population_total

  pop_weight <- pop / pop_size_across_admin1
  population_weights <- c(population_weights, pop_weight)
  
  
}

#population_weights <- 1/sum(population_weights) * (population_weights)

population_weights <- rep(population_weights, each=2*length(age_in_days_midpoint))

SP_protection_whole_country <- c()

# Identify the admin-1 protection columns
admin_cols <- (ncol(df) - length(area_names) + 1):ncol(df)

SP_protection_whole_country <- df %>%
  rowwise() %>%
  mutate(
    weighted_sum = sum(
      unique(population_weights) * 
        c_across(all_of(admin_cols))
    )
  ) %>%
  ungroup() %>%
  pull(weighted_sum)

# for (i in 1:913) {
#   
#   # SP protection (sum of weighted mean (protection in each admin-1 area with population size weights) at each time point)
#   
#   SP_protection_whole_country <- c(SP_protection_whole_country, sum(unique(population_weights) * df[i,(dim(df)[2] - length(area_names) + 1):dim(df)[2]])) # only admin-1 area protections
# 
# }

#population_weights <- 1/sum(population_weights) * (population_weights) # proportion relative to the population in admin1 units which have malaria transmission 

SP_protection_whole_country <- tapply(
  SP_protection_whole_country,
  (seq_along(SP_protection_whole_country)) %/% 7,
  mean,
  na.rm = TRUE
)

PMC_impact_ppy_whole_country <- data.frame(age_in_days_midpoint)

PMC_impact_ppy_whole_country$clinical <- incidence_ppy_df_whole_country$clinical * (1-SP_protection_whole_country)
PMC_impact_ppy_whole_country$severe <- incidence_ppy_df_whole_country$severe * (1-SP_protection_whole_country)


##### CALCULATE PMC IMPACT FOR EACH ADMIN-1 AREA (both PMC schedules) #####

PMC_impact_ppy <- data.frame()

 


for (i in 1:length(area_names)){
  
  # PMC impact on incidence (ppy)
  new_PMC_impact_ppy_df <- (incidence_ppy_df %>% filter(`Admin-1 unit` == area_names[i]))
  
  ages_needed <- unique(new_PMC_impact_ppy_df$age_in_days_midpoint)
  
  PMC_impact_ppy_cases <- as.double(new_PMC_impact_ppy_df$value) * rep(1-df[[paste0("prot_overall_", area_names[i])]][ages_needed], times=2)
  
  
  new_PMC_impact_ppy_df$value <- PMC_impact_ppy_cases
  
  PMC_impact_ppy <- rbind(PMC_impact_ppy, new_PMC_impact_ppy_df)
  
}

 

##### GRAPHS AND ANALYSIS RESULTS #####

number_of_doses <- length(schedule)

merged_df_by_age_COMPLETE <- c()
merged_df_annual_sixmonths_COMPLETE <- c()
merged_df_annual_COMPLETE <- c()
whole_country_COMPLETE <- c()
whole_country_by_age_COMPLETE <- c()



##### MERGED RESULTS AND WHOLE COUNTRY #####

for (k in 1:length(number_of_doses)) {
  
  rur_or_urb <- "merged"
  
  
  
  #area_names <- unique(PMC_impact_ppy$area)
  
  ##### SET UP CALCULATION DATAFRAMES  #####
  
  # raw baseline cases (no PMC) for every age group
  results_by_age<- data.frame(Country=rep(country, length(area_names)*length(age_in_days_midpoint)),
                              `Age group` = rep(age_group_names,length(area_names)),
                              `Admin-1 unit` = rep(area_names, each=length(age_in_days_midpoint)),
                              `N doses` = rep(number_of_doses[k], length(area_names)*length(age_in_days_midpoint)), check.names=F)
  
  # average baseline annual cases (no PMC) for whole age group
  annual_results<- data.frame(Country=rep(country, length(area_names)),
                              `Age group` = paste0("n_age_0_", max(age_max)),
                              `Admin-1 unit` = area_names,
                              `N doses` =rep(number_of_doses[k], length(area_names)), check.names=F)
  
  
  ##### INITIALISE VECTORS FOR CALCULATIONS #####
  
  # raw cases
  current_cases_no_PMC_clin <- current_cases_no_PMC_sev <- current_cases_no_PMC_tot <- current_cases_no_PMC_asym <- c()
  current_cases_averted_with_PMC_clin <- current_cases_averted_with_PMC_sev <- current_cases_averted_with_PMC_tot <- current_cases_averted_with_PMC_asym <- c()
  current_cases_with_PMC_clin <- current_cases_with_PMC_sev <- current_cases_with_PMC_tot <- current_cases_with_PMC_asym <- c()
  
  # cases per 1000 children in that age group
  current_cases_no_PMC_clin_per1000 <- current_cases_no_PMC_sev_per1000 <- current_cases_no_PMC_tot_per1000 <- current_cases_no_PMC_asym_per1000 <- c()
  current_cases_averted_with_PMC_clin_per1000 <- current_cases_averted_with_PMC_sev_per1000 <- current_cases_averted_with_PMC_tot_per1000 <- current_cases_averted_with_PMC_asym_per1000 <- c()
  current_cases_with_PMC_clin_per1000 <- current_cases_with_PMC_sev_per1000 <- current_cases_with_PMC_tot_per1000 <- current_cases_with_PMC_asym_per1000 <- c()
  
  # % reduction with PMC 
  current_cases_reduction_with_PMC_clin <- current_cases_reduction_with_PMC_sev <- current_cases_reduction_with_PMC_tot <- current_cases_reduction_with_PMC_asym <- c()
  
  # annual cases
  annual_current_cases_no_PMC_clin <- annual_current_cases_no_PMC_sev <- annual_current_cases_no_PMC_tot <- annual_current_cases_no_PMC_asym <- c()
  annual_current_cases_with_PMC_clin <- annual_current_cases_with_PMC_sev <- annual_current_cases_with_PMC_tot <- annual_current_cases_with_PMC_asym <- c()
  annual_current_cases_averted_with_PMC_clin <- annual_current_cases_averted_with_PMC_sev <- annual_current_cases_averted_with_PMC_tot <- annual_current_cases_averted_with_PMC_asym <- c()
  
  # annual cases (per 1000 children in that age group)
  annual_current_cases_no_PMC_clin_per1000 <- annual_current_cases_no_PMC_sev_per1000 <- annual_current_cases_no_PMC_tot_per1000 <- annual_current_cases_no_PMC_asym_per1000 <- c()
  annual_current_cases_with_PMC_clin_per1000 <- annual_current_cases_with_PMC_sev_per1000 <- annual_current_cases_with_PMC_tot_per1000 <- annual_current_cases_with_PMC_asym_per1000 <- c()
  annual_current_cases_averted_with_PMC_clin_per1000 <- annual_current_cases_averted_with_PMC_sev_per1000 <- annual_current_cases_averted_with_PMC_tot_per1000 <- annual_current_cases_averted_with_PMC_asym_per1000 <- c()
  
  
  
  
  ##### RUN CALCULATIONS ACROSS WHOLE COUNTRY #####
  
  for (i in 1:length(area_names)){
    
    # extracting average age group proportions from the imperial model age distributions 
    age_distribution <- as.numeric(rep(unlist((population_df_age_structure %>% filter(Admin.1.unit == area_names[i]))[,4:dim(population_df_age_structure)[2]]),2))
    
    
    # sum of rural and urban areas as weighted mean is used 
    
    area_population <- (all_sites_for_DT %>% filter(Country==input$country, `Admin-1 unit`==area_names[i]))$population_total
    
    
    
    # number of cases in each age group per year (no PMC)
    current_cases_no_PMC <- (incidence_ppy_df %>% filter(`Admin-1 unit`==area_names[i]))$value * age_distribution * area_population
    
    # number of cases in each age group per year (no PMC) by infection class
    current_cases_no_PMC_clin <- c(current_cases_no_PMC_clin, current_cases_no_PMC[1:131])
    current_cases_no_PMC_sev <- c(current_cases_no_PMC_sev, current_cases_no_PMC[(132):262])
    # current_cases_no_PMC_tot <- c(current_cases_no_PMC_tot, current_cases_no_PMC[(1+2*max(age_max)):(3*max(age_max))])
    # current_cases_no_PMC_asym <- c(current_cases_no_PMC_asym, current_cases_no_PMC[(1+3*max(age_max)):(4*max(age_max))])
    # 
    # annual number of cases in each age group per year (no PMC) by infection class
    annual_current_cases_no_PMC_clin <- c(annual_current_cases_no_PMC_clin, sum(current_cases_no_PMC[1:131]))
    annual_current_cases_no_PMC_sev <- c(annual_current_cases_no_PMC_sev, sum(current_cases_no_PMC[(132):262]))
    # annual_current_cases_no_PMC_tot <- c(annual_current_cases_no_PMC_tot, sum(current_cases_no_PMC[(1+2*max(age_max)):(3*max(age_max))]))
    # annual_current_cases_no_PMC_asym <- c(annual_current_cases_no_PMC_asym, sum(current_cases_no_PMC[(1+3*max(age_max)):(4*max(age_max))]))
    # 
    # number of cases in each age group per year (no PMC) (per 1000 children in that age group)
    current_cases_no_PMC_per1000 <- (incidence_ppy_df %>% filter(`Admin-1 unit`==area_names[i]))$value * 1000
    
    # number of cases in each age group per year (no PMC) by infection class (per 1000 children in that age group)
    current_cases_no_PMC_clin_per1000 <- c(current_cases_no_PMC_clin_per1000, current_cases_no_PMC_per1000[1:131])
    current_cases_no_PMC_sev_per1000 <- c(current_cases_no_PMC_sev_per1000, current_cases_no_PMC_per1000[(132):262])
    # current_cases_no_PMC_tot_per1000 <- c(current_cases_no_PMC_tot_per1000, current_cases_no_PMC_per1000[(1+2*max(age_max)):(3*max(age_max))])
    # current_cases_no_PMC_asym_per1000 <- c(current_cases_no_PMC_asym_per1000, current_cases_no_PMC_per1000[(1+3*max(age_max)):(4*max(age_max))])
    # 
    # annual number of cases in each age group per year (no PMC) by infection class (per 1000 children in that age group)
    annual_current_cases_no_PMC_clin_per1000 <- c(annual_current_cases_no_PMC_clin_per1000, mean(current_cases_no_PMC_per1000[1:131]))
    annual_current_cases_no_PMC_sev_per1000 <- c(annual_current_cases_no_PMC_sev_per1000, mean(current_cases_no_PMC_per1000[(132):262]))
    # annual_current_cases_no_PMC_tot_per1000 <- c(annual_current_cases_no_PMC_tot_per1000, mean(current_cases_no_PMC_per1000[(1+2*max(age_max)):(3*max(age_max))]))
    # annual_current_cases_no_PMC_asym_per1000 <- c(annual_current_cases_no_PMC_asym_per1000, mean(current_cases_no_PMC_per1000[(1+3*max(age_max)):(4*max(age_max))]))
    # 
    
    
    # number of cases in each age group per year (with PMC)
    current_cases_with_PMC <- (PMC_impact_ppy %>% filter(`Admin-1 unit`==area_names[i]))$value * age_distribution * area_population
    
    # number of cases in each age group per year (with PMC) (per 1000 children in that age group)
    current_cases_with_PMC_per1000 <- (PMC_impact_ppy %>% filter(`Admin-1 unit`==area_names[i]))$value * 1000
    
    # number of cases in each age group per year (with PMC) by infection class
    current_cases_with_PMC_clin <- c(current_cases_with_PMC_clin, current_cases_with_PMC[1:131])
    current_cases_with_PMC_sev <- c(current_cases_with_PMC_sev, current_cases_with_PMC[(132):262])
    # current_cases_with_PMC_tot <- c(current_cases_with_PMC_tot, current_cases_with_PMC[(1+2*max(age_max)):(3*max(age_max))])
    # current_cases_with_PMC_asym <- c(current_cases_with_PMC_asym, current_cases_with_PMC[(1+3*max(age_max)):(4*max(age_max))])
    # 
    
    # number of cases in each age group per year (with PMC) by infection class (per 1000 children in that age group)
    current_cases_with_PMC_clin_per1000 <- c(current_cases_with_PMC_clin_per1000, current_cases_with_PMC_per1000[1:131])
    current_cases_with_PMC_sev_per1000 <- c(current_cases_with_PMC_sev_per1000, current_cases_with_PMC_per1000[(132):262])
    # current_cases_with_PMC_tot_per1000 <- c(current_cases_with_PMC_tot_per1000, current_cases_with_PMC_per1000[(1+2*max(age_max)):(3*max(age_max))])
    # current_cases_with_PMC_asym_per1000 <- c(current_cases_with_PMC_asym_per1000, current_cases_with_PMC_per1000[(1+3*max(age_max)):(4*max(age_max))])
    # 
    
    # annual number of cases in each age group per year (with PMC) by infection class
    annual_current_cases_with_PMC_clin <- c(annual_current_cases_with_PMC_clin, sum(current_cases_with_PMC[1:131]))
    annual_current_cases_with_PMC_sev <- c(annual_current_cases_with_PMC_sev, sum(current_cases_with_PMC[(132):262]))
    # annual_current_cases_with_PMC_tot <- c(annual_current_cases_with_PMC_tot, sum(current_cases_with_PMC[(1+2*max(age_max)):(3*max(age_max))]))
    # annual_current_cases_with_PMC_asym <- c(annual_current_cases_with_PMC_asym, sum(current_cases_with_PMC[(1+3*max(age_max)):(4*max(age_max))]))
    # 
    
    # annual number of cases in each age group per year (with PMC) by infection class (per 1000 children in that age group)
    annual_current_cases_with_PMC_clin_per1000 <- c(annual_current_cases_with_PMC_clin_per1000, mean(current_cases_with_PMC_per1000[1:131]))
    annual_current_cases_with_PMC_sev_per1000 <- c(annual_current_cases_with_PMC_sev_per1000, mean(current_cases_with_PMC_per1000[(132):262]))
    # annual_current_cases_with_PMC_tot_per1000 <- c(annual_current_cases_with_PMC_tot_per1000, mean(current_cases_with_PMC_per1000[(1+2*max(age_max)):(3*max(age_max))]))
    # annual_current_cases_with_PMC_asym_per1000 <- c(annual_current_cases_with_PMC_asym_per1000, mean(current_cases_with_PMC_per1000[(1+3*max(age_max)):(4*max(age_max))]))
    # 
    
    # average reduction in cases (with PMC) by infection class 
    current_cases_reduction_with_PMC_clin <- c(current_cases_reduction_with_PMC_clin, (mean(current_cases_no_PMC[1:131]) - mean(current_cases_with_PMC[1:131])) / mean(current_cases_no_PMC[1:131]) * 100)
    current_cases_reduction_with_PMC_sev <- c(current_cases_reduction_with_PMC_sev, (mean(current_cases_no_PMC[(132):262]) - mean(current_cases_with_PMC[(132):262])) / mean(current_cases_no_PMC[(132):262]) * 100)
    # current_cases_reduction_with_PMC_tot <- c(current_cases_reduction_with_PMC_tot, (mean(current_cases_no_PMC[(1+2*max(age_max)):(3*max(age_max))]) - mean(current_cases_with_PMC[(1+2*max(age_max)):(3*max(age_max))])) / mean(current_cases_no_PMC[(1+2*max(age_max)):(3*max(age_max))]) * 100)
    # current_cases_reduction_with_PMC_asym <- c(current_cases_reduction_with_PMC_asym, (mean(current_cases_no_PMC[(1+3*max(age_max)):(4*max(age_max))]) - mean(current_cases_with_PMC[(1+3*max(age_max)):(4*max(age_max))])) / mean(current_cases_no_PMC[(1+3*max(age_max)):(4*max(age_max))]) * 100)
    # 
    
  }
  
  
   
  
  # raw cases averted (with PMC) by infection class
  cases_averted_with_PMC_clin <- (current_cases_no_PMC_clin - current_cases_with_PMC_clin)
  cases_averted_with_PMC_sev <- (current_cases_no_PMC_sev - current_cases_with_PMC_sev)
  # cases_averted_with_PMC_tot <- (current_cases_no_PMC_tot - current_cases_with_PMC_tot)
  # cases_averted_with_PMC_asym <- (current_cases_no_PMC_asym - current_cases_with_PMC_asym)
  # 
  
  # raw cases averted (with PMC) by infection class (per 1000 children in that age group)
  cases_averted_with_PMC_clin_per1000 <- (current_cases_no_PMC_clin_per1000 - current_cases_with_PMC_clin_per1000)
  cases_averted_with_PMC_sev_per1000 <- (current_cases_no_PMC_sev_per1000 - current_cases_with_PMC_sev_per1000)
  # cases_averted_with_PMC_tot_per1000 <- (current_cases_no_PMC_tot_per1000 - current_cases_with_PMC_tot_per1000)
  # cases_averted_with_PMC_asym_per1000 <- (current_cases_no_PMC_asym_per1000 - current_cases_with_PMC_asym_per1000)
  # 
  # annual cases averted (with PMC) by infection class
  annual_cases_averted_with_PMC_clin <- (annual_current_cases_no_PMC_clin - annual_current_cases_with_PMC_clin)
  annual_cases_averted_with_PMC_sev <- (annual_current_cases_no_PMC_sev - annual_current_cases_with_PMC_sev)
  # annual_cases_averted_with_PMC_tot <- (annual_current_cases_no_PMC_tot - annual_current_cases_with_PMC_tot)
  # annual_cases_averted_with_PMC_asym <- (annual_current_cases_no_PMC_asym - annual_current_cases_with_PMC_asym)
  # 
  
  # annual cases averted (with PMC) by infection class (per 1000 children in that age group)
  annual_cases_averted_with_PMC_clin_per1000 <- (annual_current_cases_no_PMC_clin_per1000 - annual_current_cases_with_PMC_clin_per1000)
  annual_cases_averted_with_PMC_sev_per1000 <- (annual_current_cases_no_PMC_sev_per1000 - annual_current_cases_with_PMC_sev_per1000)
  # annual_cases_averted_with_PMC_tot_per1000 <- (annual_current_cases_no_PMC_tot_per1000 - annual_current_cases_with_PMC_tot_per1000)
  # annual_cases_averted_with_PMC_asym_per1000 <- (annual_current_cases_no_PMC_asym_per1000 - annual_current_cases_with_PMC_asym_per1000)
  # 
  
  # average reduction in cases by infection class 
  cases_reduction_with_PMC_clin <- (current_cases_no_PMC_clin - current_cases_with_PMC_clin)/current_cases_no_PMC_clin * 100
  cases_reduction_with_PMC_sev <- (current_cases_no_PMC_sev - current_cases_with_PMC_sev)/current_cases_no_PMC_sev * 100
  # cases_reduction_with_PMC_tot <- (current_cases_no_PMC_tot - current_cases_with_PMC_tot)/current_cases_no_PMC_tot * 100
  # cases_reduction_with_PMC_asym <- (current_cases_no_PMC_asym - current_cases_with_PMC_asym)/current_cases_no_PMC_asym * 100
  # 
  
  ##### APPEND VECTORS TO CALCULATION DATAFRAMES #####
  
  results_by_age$clinical_cases_no_PMC <- current_cases_no_PMC_clin
  results_by_age$severe_cases_no_PMC <- current_cases_no_PMC_sev
  # results_by_age$total_cases_no_PMC <- current_cases_no_PMC_tot
  # results_by_age$asymptomatic_cases_no_PMC <- current_cases_no_PMC_asym
  
  
  results_by_age$clinical_cases_no_PMC_per1000 <- current_cases_no_PMC_clin_per1000
  results_by_age$severe_cases_no_PMC_per1000 <- current_cases_no_PMC_sev_per1000
  # results_by_age$total_cases_no_PMC_per1000 <- current_cases_no_PMC_tot_per1000
  # results_by_age$asymptomatic_cases_no_PMC_per1000 <- current_cases_no_PMC_asym_per1000
  # 
  annual_results$clinical_cases_no_PMC <- annual_current_cases_no_PMC_clin
  annual_results$severe_cases_no_PMC <- annual_current_cases_no_PMC_sev
  # annual_results$total_cases_no_PMC <- annual_current_cases_no_PMC_tot
  # annual_results$asymptomatic_cases_no_PMC <- annual_current_cases_no_PMC_asym
  
  annual_results$clinical_cases_no_PMC_per1000 <- annual_current_cases_no_PMC_clin_per1000
  annual_results$severe_cases_no_PMC_per1000 <- annual_current_cases_no_PMC_sev_per1000
  # annual_results$total_cases_no_PMC_per1000 <- annual_current_cases_no_PMC_tot_per1000
  # annual_results$asymptomatic_cases_no_PMC_per1000 <- annual_current_cases_no_PMC_asym_per1000
  # 
  results_by_age$clinical_cases_averted_with_PMC <- cases_averted_with_PMC_clin
  results_by_age$severe_cases_averted_with_PMC <- cases_averted_with_PMC_sev
  # results_by_age$total_cases_averted_with_PMC <- cases_averted_with_PMC_tot
  # results_by_age$asymptomatic_cases_averted_with_PMC <- cases_averted_with_PMC_asym
  
  results_by_age$clinical_cases_averted_with_PMC_per1000 <- cases_averted_with_PMC_clin_per1000
  results_by_age$severe_cases_averted_with_PMC_per1000 <- cases_averted_with_PMC_sev_per1000
  # results_by_age$total_cases_averted_with_PMC_per1000 <- cases_averted_with_PMC_tot_per1000
  # results_by_age$asymptomatic_cases_averted_with_PMC_per1000<- cases_averted_with_PMC_asym_per1000
  # 
  annual_results$clinical_cases_averted_with_PMC  <- annual_cases_averted_with_PMC_clin
  annual_results$severe_cases_averted_with_PMC  <- annual_cases_averted_with_PMC_sev
  # annual_results$total_cases_averted_with_PMC  <- annual_cases_averted_with_PMC_tot
  # annual_results$asymptomatic_cases_averted_with_PMC  <- annual_cases_averted_with_PMC_asym
  
  annual_results$clinical_cases_averted_with_PMC_per1000 <- annual_cases_averted_with_PMC_clin_per1000
  annual_results$severe_cases_averted_with_PMC_per1000 <- annual_cases_averted_with_PMC_sev_per1000
  # annual_results$total_cases_averted_with_PMC_per1000 <- annual_cases_averted_with_PMC_tot_per1000
  # annual_results$asymptomatic_cases_averted_with_PMC_per1000 <- annual_cases_averted_with_PMC_asym_per1000
  # 
  results_by_age$clinical_cases_with_PMC <- current_cases_with_PMC_clin
  results_by_age$severe_cases_with_PMC <- current_cases_with_PMC_sev
  # results_by_age$total_cases_with_PMC <- current_cases_with_PMC_tot
  # results_by_age$asymptomatic_cases_with_PMC <- current_cases_with_PMC_asym
  
  results_by_age$clinical_cases_with_PMC_per1000 <- current_cases_with_PMC_clin_per1000
  results_by_age$severe_cases_with_PMC_per1000 <- current_cases_with_PMC_sev_per1000
  # results_by_age$total_cases_with_PMC_per1000 <- current_cases_with_PMC_tot_per1000
  # results_by_age$asymptomatic_cases_with_PMC_per1000 <- current_cases_with_PMC_asym_per1000
  # 
  annual_results$clinical_cases_with_PMC <- annual_current_cases_with_PMC_clin
  annual_results$severe_cases_with_PMC <- annual_current_cases_with_PMC_sev
  # annual_results$total_cases_with_PMC <- annual_current_cases_with_PMC_tot
  # annual_results$asymptomatic_cases_with_PMC <- annual_current_cases_with_PMC_asym
  
  annual_results$clinical_cases_with_PMC_per1000 <- annual_current_cases_with_PMC_clin_per1000
  annual_results$severe_cases_with_PMC_per1000 <- annual_current_cases_with_PMC_sev_per1000
  # annual_results$total_cases_with_PMC_per1000 <- annual_current_cases_with_PMC_tot_per1000
  # annual_results$asymptomatic_cases_with_PMC_per1000 <- annual_current_cases_with_PMC_asym_per1000
  # 
  annual_results$clinical_cases_reduction <- current_cases_reduction_with_PMC_clin
  annual_results$severe_cases_reduction <- current_cases_reduction_with_PMC_sev
  # annual_results$total_cases_reduction <- current_cases_reduction_with_PMC_tot
  # annual_results$asymptomatic_cases_reduction <- current_cases_reduction_with_PMC_asym
  # 
  ##### SET UP CALCULATION DATAFRAMES (6 MONTH AGE GROUP INTERVALS) #####
  
  # raw baseline cases (no PMC)
  annual_results_sixmonth<- data.frame(Country=rep(country, length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                       `Age group` = rep(age_group_names_sixmonth[1:no_sixmonth_intervals], times=length(area_names)),
                                       `Admin-1 unit` = rep(area_names, each=length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                       `N doses` = rep(number_of_doses[k], length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])), check.names = F)
  
  
  ##### INITIALISE VECTORS FOR CALCULATIONS (6 MONTH AGE GROUP INTERVALS) #####
  
  # raw cases
  cases_no_PMC_clin <- cases_no_PMC_sev <- cases_no_PMC_tot <- cases_no_PMC_asym <- c()
  cases_with_PMC_clin <- cases_with_PMC_sev <- cases_with_PMC_tot <- cases_with_PMC_asym <- c()
  
  # cases per 1000 children in that age group
  cases_no_PMC_clin_per1000 <- cases_no_PMC_sev_per1000 <- cases_no_PMC_tot_per1000 <- cases_no_PMC_asym_per1000 <- c()
  cases_with_PMC_clin_per1000 <- cases_with_PMC_sev_per1000 <- cases_with_PMC_tot_per1000 <- cases_with_PMC_asym_per1000 <- c()
  
  # cases averted (with PMC) 
  cases_averted_with_PMC_clin <- cases_averted_with_PMC_sev <- cases_averted_with_PMC_tot <- cases_averted_with_PMC_asym <- c()
  
  # cases averted (per 1000 children in that age group)
  cases_averted_with_PMC_clin_per1000 <- cases_averted_with_PMC_sev_per1000 <- cases_averted_with_PMC_tot_per1000 <- cases_averted_with_PMC_asym_per1000 <- c()
  
  # cases reduction 
  cases_reduction_with_PMC_clin <- cases_reduction_with_PMC_sev <- cases_reduction_with_PMC_tot <- cases_reduction_with_PMC_asym <- c()
  
  ##### RUN CALCULATIONS ACROSS WHOLE COUNTRY (6 MONTH AGE GROUP INTERVALS) #####
  
  for (i in 1:length(area_names)) {
    
    # raw cases (no PMC) by infection class
    current_cases_no_PMC_clin <- (results_by_age %>% filter(`Admin-1 unit` == area_names[i]))$clinical_cases_no_PMC
    current_cases_no_PMC_sev <- (results_by_age %>% filter(`Admin-1 unit` == area_names[i]))$severe_cases_no_PMC
    # current_cases_no_PMC_tot <- (results_by_age %>% filter(`Admin-1 unit` == area_names[i]))$total_cases_no_PMC
    # current_cases_no_PMC_asym <- (results_by_age %>% filter(`Admin-1 unit` == area_names[i]))$asymptomatic_cases_no_PMC
    # 
    # raw cases (with PMC) by infection class
    current_cases_with_PMC_clin <- (results_by_age %>% filter(`Admin-1 unit` == area_names[i]))$clinical_cases_with_PMC
    current_cases_with_PMC_sev <- (results_by_age %>% filter(`Admin-1 unit` == area_names[i]))$severe_cases_with_PMC
    # current_cases_with_PMC_tot <- (results_by_age %>% filter(`Admin-1 unit` == area_names[i]))$total_cases_with_PMC
    # current_cases_with_PMC_asym <- (results_by_age %>% filter(`Admin-1 unit` == area_names[i]))$asymptomatic_cases_with_PMC
    # 
    
    # cases (no PMC) by infection class (per 1000 children in that age group)
    current_cases_no_PMC_clin_per1000 <- (incidence_ppy_df %>% filter(`Admin-1 unit` == area_names[i], infection_class == "clinical"))$value * 1000
    current_cases_no_PMC_sev_per1000 <- (incidence_ppy_df %>% filter(`Admin-1 unit` == area_names[i], infection_class == "severe"))$value * 1000
    # current_cases_no_PMC_tot_per1000 <- (incidence_ppy_df %>% filter(`Admin-1 unit` == area_names[i], infection_class == "total"))$value * 1000
    # current_cases_no_PMC_asym_per1000 <- (incidence_ppy_df %>% filter(`Admin-1 unit` == area_names[i], infection_class == "asymptomatic"))$value * 1000
    # 
    # cases (with PMC) by infection class (per 1000 children in that age group)
    current_cases_with_PMC_clin_per1000 <- (PMC_impact_ppy %>% filter(`Admin-1 unit` == area_names[i], infection_class == "clinical"))$value * 1000
    current_cases_with_PMC_sev_per1000 <- (PMC_impact_ppy %>% filter(`Admin-1 unit` == area_names[i], infection_class == "severe"))$value * 1000
    # current_cases_with_PMC_tot_per1000 <- (PMC_impact_ppy %>% filter(`Admin-1 unit` == area_names[i], infection_class == "total"))$value * 1000
    # current_cases_with_PMC_asym_per1000 <- (PMC_impact_ppy %>% filter(`Admin-1 unit` == area_names[i], infection_class == "asymptomatic"))$value * 1000
    # 
    
    
    # split data into each 6 month age group for each infection class (no PMC)
    split_current_cases_no_PMC_clin <- split(current_cases_no_PMC_clin, c(rep(1, times=26),rep(2, times=26), rep(3, times=26), rep(4, times=26), rep(5, times=27)))
    names(split_current_cases_no_PMC_clin) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    split_current_cases_no_PMC_sev <- split(current_cases_no_PMC_sev, c(rep(1, times=26),rep(2, times=26), rep(3, times=26), rep(4, times=26), rep(5, times=27)))
    names(split_current_cases_no_PMC_sev) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    # 
    # split_current_cases_no_PMC_tot <- split(current_cases_no_PMC_tot, ceiling(seq_along(current_cases_no_PMC_tot)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    # names(split_current_cases_no_PMC_tot) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    # 
    # split_current_cases_no_PMC_asym <- split(current_cases_no_PMC_asym, ceiling(seq_along(current_cases_no_PMC_asym)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    # names(split_current_cases_no_PMC_asym) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    # 
    
    # split data into each 6 month age group for each infection class (with PMC)
    split_current_cases_with_PMC_clin <- split(current_cases_with_PMC_clin,  c(rep(1, times=26),rep(2, times=26), rep(3, times=26), rep(4, times=26), rep(5, times=27)))
    names(split_current_cases_with_PMC_clin) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    split_current_cases_with_PMC_sev <- split(current_cases_with_PMC_sev,  c(rep(1, times=26),rep(2, times=26), rep(3, times=26), rep(4, times=26), rep(5, times=27)))
    names(split_current_cases_with_PMC_sev) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    # split_current_cases_with_PMC_tot <- split(current_cases_with_PMC_tot, ceiling(seq_along(current_cases_with_PMC_tot)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    # names(split_current_cases_with_PMC_tot) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    # 
    # split_current_cases_with_PMC_asym <- split(current_cases_with_PMC_asym, ceiling(seq_along(current_cases_with_PMC_asym)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    # names(split_current_cases_with_PMC_asym) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    # 
    
    # split data into each 6 month age group for each infection class (no PMC) (per 1000 children in that age group)
    split_current_cases_no_PMC_clin_per1000 <- split(current_cases_no_PMC_clin_per1000,  c(rep(1, times=26),rep(2, times=26), rep(3, times=26), rep(4, times=26), rep(5, times=27)))
    names(split_current_cases_no_PMC_clin_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    split_current_cases_no_PMC_sev_per1000 <- split(current_cases_no_PMC_sev_per1000,  c(rep(1, times=26),rep(2, times=26), rep(3, times=26), rep(4, times=26), rep(5, times=27)))
    names(split_current_cases_no_PMC_sev_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    # split_current_cases_no_PMC_tot_per1000 <- split(current_cases_no_PMC_tot_per1000, ceiling(seq_along(current_cases_no_PMC_tot_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    # names(split_current_cases_no_PMC_tot_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    # 
    # split_current_cases_no_PMC_asym_per1000 <- split(current_cases_no_PMC_asym_per1000, ceiling(seq_along(current_cases_no_PMC_asym_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    # names(split_current_cases_no_PMC_asym_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    # 
    
    # split data into each 6 month age group for each infection class (with PMC) (per 1000 children in that age group)
    split_current_cases_with_PMC_clin_per1000 <- split(current_cases_with_PMC_clin_per1000,  c(rep(1, times=26),rep(2, times=26), rep(3, times=26), rep(4, times=26), rep(5, times=27)))
    names(split_current_cases_with_PMC_clin_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    split_current_cases_with_PMC_sev_per1000 <- split(current_cases_with_PMC_sev_per1000,  c(rep(1, times=26),rep(2, times=26), rep(3, times=26), rep(4, times=26), rep(5, times=27)))
    names(split_current_cases_with_PMC_sev_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    
    # split_current_cases_with_PMC_tot_per1000 <- split(current_cases_with_PMC_tot_per1000, ceiling(seq_along(current_cases_with_PMC_tot_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    # names(split_current_cases_with_PMC_tot_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    # 
    # split_current_cases_with_PMC_asym_per1000 <- split(current_cases_with_PMC_asym_per1000, ceiling(seq_along(current_cases_with_PMC_asym_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
    # names(split_current_cases_with_PMC_asym_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
    # 
     
    
    
    for (j in 1:length(sixmonth_intervals[1:no_sixmonth_intervals])) {
      
      # sum of cases (no PMC) for each 6 month age group
      cases_no_PMC_clin<- c(cases_no_PMC_clin, sum(split_current_cases_no_PMC_clin[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      cases_no_PMC_sev<- c(cases_no_PMC_sev, sum(split_current_cases_no_PMC_sev[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      #cases_no_PMC_tot<- c(cases_no_PMC_tot, sum(split_current_cases_no_PMC_tot[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      #cases_no_PMC_asym<- c(cases_no_PMC_asym, sum(split_current_cases_no_PMC_asym[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      
      # sum of cases (with PMC) for each 6 month age group
      cases_with_PMC_clin<- c(cases_with_PMC_clin, sum(split_current_cases_with_PMC_clin[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      cases_with_PMC_sev<- c(cases_with_PMC_sev, sum(split_current_cases_with_PMC_sev[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      #cases_with_PMC_tot<- c(cases_with_PMC_tot, sum(split_current_cases_with_PMC_tot[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      #cases_with_PMC_asym<- c(cases_with_PMC_asym, sum(split_current_cases_with_PMC_asym[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      
      # sum of cases (no PMC) for each 6 month age group (per 1000 children in that age group)
      cases_no_PMC_clin_per1000<- c(cases_no_PMC_clin_per1000, mean(split_current_cases_no_PMC_clin_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      cases_no_PMC_sev_per1000<- c(cases_no_PMC_sev_per1000, mean(split_current_cases_no_PMC_sev_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      #cases_no_PMC_tot_per1000<- c(cases_no_PMC_tot_per1000, mean(split_current_cases_no_PMC_tot_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      #cases_no_PMC_asym_per1000<- c(cases_no_PMC_asym_per1000, mean(split_current_cases_no_PMC_asym_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      
      # sum of cases (with PMC) for each 6 month age group (per 1000 children in that age group)
      cases_with_PMC_clin_per1000<- c(cases_with_PMC_clin_per1000, mean(split_current_cases_with_PMC_clin_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      cases_with_PMC_sev_per1000<- c(cases_with_PMC_sev_per1000, mean(split_current_cases_with_PMC_sev_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      #cases_with_PMC_tot_per1000<- c(cases_with_PMC_tot_per1000, mean(split_current_cases_with_PMC_tot_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      #cases_with_PMC_asym_per1000<- c(cases_with_PMC_asym_per1000, mean(split_current_cases_with_PMC_asym_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
      
    }
    
  }  
  
  
  
  # cases averted (with PMC) by infection class (per 1000 children in that age group)
  cases_averted_with_PMC_clin <- (cases_no_PMC_clin - cases_with_PMC_clin)
  cases_averted_with_PMC_sev <- (cases_no_PMC_sev - cases_with_PMC_sev)
  # cases_averted_with_PMC_tot <- (cases_no_PMC_tot - cases_with_PMC_tot)
  # cases_averted_with_PMC_asym <- (cases_no_PMC_asym - cases_with_PMC_asym)
  # 
  # cases averted (with PMC) by infection class (per 1000 children in that age group)
  cases_averted_with_PMC_clin_per1000 <- (cases_no_PMC_clin_per1000 - cases_with_PMC_clin_per1000)
  cases_averted_with_PMC_sev_per1000 <- (cases_no_PMC_sev_per1000 - cases_with_PMC_sev_per1000)
  # cases_averted_with_PMC_tot_per1000 <- (cases_no_PMC_tot_per1000 - cases_with_PMC_tot_per1000)
  # cases_averted_with_PMC_asym_per1000 <- (cases_no_PMC_asym_per1000 - cases_with_PMC_asym_per1000)
  # 
  # reduction in cases (with PMC) by infection class
  cases_reduction_with_PMC_clin <- (cases_no_PMC_clin - cases_with_PMC_clin)/cases_no_PMC_clin * 100
  cases_reduction_with_PMC_sev <- (cases_no_PMC_sev - cases_with_PMC_sev)/cases_no_PMC_sev * 100
  # cases_reduction_with_PMC_tot <- (cases_no_PMC_tot - cases_with_PMC_tot)/cases_no_PMC_tot * 100
  # cases_reduction_with_PMC_asym <- (cases_no_PMC_asym - cases_with_PMC_asym)/cases_no_PMC_asym * 100
  # 
  
  ##### APPEND VECTORS TO CALCULATION DATAFRAMES (6 MONTH AGE GROUP INTERVALS) #####
  
  annual_results_sixmonth$clinical_cases_no_PMC <- cases_no_PMC_clin
  annual_results_sixmonth$severe_cases_no_PMC <- cases_no_PMC_sev
  # annual_results_sixmonth$total_cases_no_PMC <- cases_no_PMC_tot
  # annual_results_sixmonth$asymptomatic_cases_no_PMC<- cases_no_PMC_asym
  # 
  
  annual_results_sixmonth$clinical_cases_no_PMC_per1000 <- cases_no_PMC_clin_per1000
  annual_results_sixmonth$severe_cases_no_PMC_per1000 <- cases_no_PMC_sev_per1000
  # annual_results_sixmonth$total_cases_no_PMC_per1000 <- cases_no_PMC_tot_per1000
  # annual_results_sixmonth$asymptomatic_cases_no_PMC_per1000<- cases_no_PMC_asym_per1000
  # 
  annual_results_sixmonth$clinical_cases_with_PMC<- cases_with_PMC_clin
  annual_results_sixmonth$severe_cases_with_PMC<- cases_with_PMC_sev
  # annual_results_sixmonth$total_cases_with_PMC<- cases_with_PMC_tot
  # annual_results_sixmonth$asymptomatic_cases_with_PMC<- cases_with_PMC_asym
  # 
  annual_results_sixmonth$clinical_cases_with_PMC_per1000<- cases_with_PMC_clin_per1000
  annual_results_sixmonth$severe_cases_with_PMC_per1000<- cases_with_PMC_sev_per1000
  # annual_results_sixmonth$total_cases_with_PMC_per1000<- cases_with_PMC_tot_per1000
  # annual_results_sixmonth$asymptomatic_cases_with_PMC_per1000<- cases_with_PMC_asym_per1000
  # 
  annual_results_sixmonth$clinical_cases_averted_with_PMC <- cases_averted_with_PMC_clin
  annual_results_sixmonth$severe_cases_averted_with_PMC <- cases_averted_with_PMC_sev
  # annual_results_sixmonth$total_cases_averted_with_PMC <- cases_averted_with_PMC_tot
  # annual_results_sixmonth$asymptomatic_cases_averted_with_PMC <- cases_averted_with_PMC_asym
  # 
  annual_results_sixmonth$clinical_cases_averted_with_PMC_per1000 <- cases_averted_with_PMC_clin_per1000
  annual_results_sixmonth$severe_cases_averted_with_PMC_per1000 <- cases_averted_with_PMC_sev_per1000
  # annual_results_sixmonth$total_cases_averted_with_PMC_per1000 <- cases_averted_with_PMC_tot_per1000
  # annual_results_sixmonth$asymptomatic_cases_averted_with_PMC_per1000 <- cases_averted_with_PMC_asym_per1000
  # 
  annual_results_sixmonth$clinical_cases_reduction <- cases_reduction_with_PMC_clin
  annual_results_sixmonth$severe_cases_reduction <- cases_reduction_with_PMC_sev
  # annual_results_sixmonth$total_cases_reduction <- cases_reduction_with_PMC_tot
  # annual_results_sixmonth$asymptomatic_cases_reduction <- cases_reduction_with_PMC_asym
  # 
  
  
  
  PMC_impact_ppy_whole_country_weighted <- PMC_impact_ppy
  incidence_ppy_df_whole_country_weighted <- incidence_ppy_df
  
  
  population_weights <- c()
  
  #pop_size_across_admin1 <- sum((full_data$population$population_total %>% filter(year==2023, name_2 %in% area_names))$pop)
  pop_size_across_admin1 <- sum((all_sites_for_DT %>% filter(Country==input$country))$population_total)
  
  
  
  for (i in 1:length(area_names)){
    
    pop <- (all_sites_for_DT %>% filter(Country==input$country, `Admin-1 unit`==area_names[i]))$population_total
    
    pop_weight <- pop / pop_size_across_admin1
    population_weights <- c(population_weights, pop_weight)
    
    
  }
  
  
  population_weights <- rep(population_weights, each=2*length(age_in_days_midpoint))
  
  
   
  
  
  PMC_impact_ppy_whole_country_weighted$value <- PMC_impact_ppy_whole_country_weighted$value * population_weights
  incidence_ppy_df_whole_country_weighted$value <- incidence_ppy_df_whole_country_weighted$value * population_weights
  
  
  results_by_age_whole_country<- data.frame(Country = rep(country, length(age_in_days_midpoint)),
                                            `Age group`=rep(age_group_names),
                                            `N doses` = rep(number_of_doses[k], each = length(age_in_days_midpoint)), check.names=F)
  
  incidence_ppy_df_whole_country <- data.frame(Country = rep(country, length(age_in_days_midpoint)),
                                               `Age group`=age_group_names, check.names=F)
  
  annual_results_whole_country<- data.frame(Country = rep(country), `Age group` = paste0("n_age_0_", max(age_max)),
                                            `N doses` = rep(number_of_doses[k]), check.names=F)
  
  annual_results_sixmonths_whole_country<- data.frame(Country = rep(country, length(age_group_names_sixmonth[1:no_sixmonth_intervals])),
                                                      `Age group` = rep(age_group_names_sixmonth[1:no_sixmonth_intervals]),
                                                      `N doses` = rep(number_of_doses[k], length(age_group_names_sixmonth[1:no_sixmonth_intervals])), check.names=F)
  
  
  
  PMC_impact_ppy_whole_country_age_sum_clin <- incidence_ppy_df_whole_country_age_sum_clin <- c()
  PMC_impact_ppy_whole_country_age_sum_sev <- incidence_ppy_df_whole_country_age_sum_sev <- c()
  PMC_impact_ppy_whole_country_age_sum_tot <- incidence_ppy_df_whole_country_age_sum_tot <- c()
  PMC_impact_ppy_whole_country_age_sum_asym <- incidence_ppy_df_whole_country_age_sum_asym <- c()
  
  
  
  for (i in 1:length(age_in_days_midpoint)) {
    
    PMC_impact_ppy_whole_country_age_sum_clin <- c(PMC_impact_ppy_whole_country_age_sum_clin, sum((PMC_impact_ppy_whole_country_weighted %>% filter(infection_class == "clinical", age_in_days_midpoint == age_in_days_midpoint[i]))$value))
    incidence_ppy_df_whole_country_age_sum_clin <- c(incidence_ppy_df_whole_country_age_sum_clin, sum((incidence_ppy_df_whole_country_weighted %>% filter(infection_class == "clinical", age_in_days_midpoint == age_in_days_midpoint[i]))$value))
    
    PMC_impact_ppy_whole_country_age_sum_sev <- c(PMC_impact_ppy_whole_country_age_sum_sev, sum((PMC_impact_ppy_whole_country_weighted %>% filter(infection_class == "severe", age_in_days_midpoint == age_in_days_midpoint[i]))$value))
    incidence_ppy_df_whole_country_age_sum_sev <- c(incidence_ppy_df_whole_country_age_sum_sev, sum((incidence_ppy_df_whole_country_weighted %>% filter(infection_class == "severe", age_in_days_midpoint == age_in_days_midpoint[i]))$value))
    
    # PMC_impact_ppy_whole_country_age_sum_tot <- c(PMC_impact_ppy_whole_country_age_sum_tot, sum((PMC_impact_ppy_whole_country_weighted %>% filter(infection_class == "total", age_in_days_midpoint == age_in_days_midpoint[i]))$value))
    # incidence_ppy_df_whole_country_age_sum_tot <- c(incidence_ppy_df_whole_country_age_sum_tot, sum((incidence_ppy_df_whole_country_weighted %>% filter(infection_class == "total", age_in_days_midpoint == age_in_days_midpoint[i]))$value))
    # 
    # PMC_impact_ppy_whole_country_age_sum_asym <- c(PMC_impact_ppy_whole_country_age_sum_asym, sum((PMC_impact_ppy_whole_country_weighted %>% filter(infection_class == "asymptomatic", age_in_days_midpoint == age_in_days_midpoint[i]))$value))
    # incidence_ppy_df_whole_country_age_sum_asym <- c(incidence_ppy_df_whole_country_age_sum_asym, sum((incidence_ppy_df_whole_country_weighted %>% filter(infection_class == "asymptomatic", age_in_days_midpoint == age_in_days_midpoint[i]))$value))
    # 
  }
  
  
  results_by_age_whole_country$clinical_ppy_with_PMC <- PMC_impact_ppy_whole_country_age_sum_clin
  incidence_ppy_df_whole_country$clinical <- incidence_ppy_df_whole_country_age_sum_clin
  
  results_by_age_whole_country$severe_ppy_with_PMC <- PMC_impact_ppy_whole_country_age_sum_sev
  incidence_ppy_df_whole_country$severe <- incidence_ppy_df_whole_country_age_sum_sev
  
  # results_by_age_whole_country$total_ppy_with_PMC <- PMC_impact_ppy_whole_country_age_sum_tot
  # incidence_ppy_df_whole_country$total <- incidence_ppy_df_whole_country_age_sum_tot
  # 
  # results_by_age_whole_country$asymptomatic_ppy_with_PMC <- PMC_impact_ppy_whole_country_age_sum_asym
  # incidence_ppy_df_whole_country$asymptomatic <- incidence_ppy_df_whole_country_age_sum_asym
  # 
  annual_results_whole_country$clinical_cases_no_PMC <- sum(annual_results$clinical_cases_no_PMC )
  annual_results_whole_country$severe_cases_no_PMC <- sum(annual_results$severe_cases_no_PMC )
  # annual_results_whole_country$total_cases_no_PMC <- sum(annual_results$total_cases_no_PMC )
  # annual_results_whole_country$asymptomatic_cases_no_PMC <- sum(annual_results$asymptomatic_cases_no_PMC )
  # 
  annual_results_whole_country$clinical_cases_no_PMC_per1000 <- sum(annual_results$clinical_cases_no_PMC_per1000 * unique(population_weights))
  annual_results_whole_country$severe_cases_no_PMC_per1000 <- sum(annual_results$severe_cases_no_PMC_per1000 * unique(population_weights))
  # annual_results_whole_country$total_cases_no_PMC_per1000 <- sum(annual_results$total_cases_no_PMC_per1000 * unique(population_weights))
  # annual_results_whole_country$asymptomatic_cases_no_PMC_per1000 <- sum(annual_results$asymptomatic_cases_no_PMC_per1000 * unique(population_weights))
  # 
  annual_results_whole_country$clinical_cases_averted_with_PMC <- sum(annual_results$clinical_cases_averted_with_PMC )
  annual_results_whole_country$severe_cases_averted_with_PMC <- sum(annual_results$severe_cases_averted_with_PMC )
  # annual_results_whole_country$total_cases_averted_with_PMC <- sum(annual_results$total_cases_averted_with_PMC )
  # annual_results_whole_country$asymptomatic_cases_averted_with_PMC <- sum(annual_results$asymptomatic_cases_averted_with_PMC )
  # 
  annual_results_whole_country$clinical_cases_averted_with_PMC_per1000 <- sum(annual_results$clinical_cases_averted_with_PMC_per1000 * unique(population_weights))
  annual_results_whole_country$severe_cases_averted_with_PMC_per1000 <- sum(annual_results$severe_cases_averted_with_PMC_per1000 * unique(population_weights))
  # annual_results_whole_country$total_cases_averted_with_PMC_per1000 <- sum(annual_results$total_cases_averted_with_PMC_per1000 * unique(population_weights))
  # annual_results_whole_country$asymptomatic_cases_averted_with_PMC_per1000 <- sum(annual_results$asymptomatic_cases_averted_with_PMC_per1000 * unique(population_weights))
  # 
  annual_results_whole_country$clinical_cases_with_PMC <- sum(annual_results$clinical_cases_with_PMC  )
  annual_results_whole_country$severe_cases_with_PMC  <- sum(annual_results$severe_cases_with_PMC )
  # annual_results_whole_country$total_cases_with_PMC  <- sum(annual_results$total_cases_with_PMC  )
  # annual_results_whole_country$asymptomatic_cases_with_PMC  <- sum(annual_results$asymptomatic_cases_with_PMC  )
  # 
  annual_results_whole_country$clinical_cases_with_PMC_per1000 <- sum(annual_results$clinical_cases_with_PMC_per1000  * unique(population_weights))
  annual_results_whole_country$severe_cases_with_PMC_per1000  <- sum(annual_results$severe_cases_with_PMC_per1000  * unique(population_weights))
  # annual_results_whole_country$total_cases_with_PMC_per1000  <- sum(annual_results$total_cases_with_PMC_per1000  * unique(population_weights))
  # annual_results_whole_country$asymptomatic_cases_with_PMC_per1000  <- sum(annual_results$asymptomatic_cases_with_PMC_per1000  * unique(population_weights))
  
  annual_results_whole_country$clinical_cases_reduction <- sum(annual_results$clinical_cases_reduction  * unique(population_weights))
  annual_results_whole_country$severe_cases_reduction  <- sum(annual_results$severe_cases_reduction  * unique(population_weights))
  # annual_results_whole_country$total_cases_reduction  <- sum(annual_results$total_cases_reduction  * unique(population_weights))
  # annual_results_whole_country$asymptomatic_cases_reduction  <- sum(annual_results$asymptomatic_cases_reduction  * unique(population_weights))
  # 
  
  whole_country_COMPLETE <- rbind(whole_country_COMPLETE, annual_results_whole_country)
  whole_country_by_age_COMPLETE <- rbind(whole_country_by_age_COMPLETE, results_by_age_whole_country)
  
  
  
  # Whole country by 6 month age group
  
  # raw cases
  cases_no_PMC_clin <- cases_no_PMC_sev <- cases_no_PMC_tot <- cases_no_PMC_asym <- c()
  cases_with_PMC_clin <- cases_with_PMC_sev <- cases_with_PMC_tot <- cases_with_PMC_asym <- c()
  
  # cases per 1000 children in that age group
  cases_no_PMC_clin_per1000 <- cases_no_PMC_sev_per1000 <- cases_no_PMC_tot_per1000 <- cases_no_PMC_asym_per1000 <- c()
  cases_with_PMC_clin_per1000 <- cases_with_PMC_sev_per1000 <- cases_with_PMC_tot_per1000 <- cases_with_PMC_asym_per1000 <- c()
  
  # cases averted (with PMC) 
  cases_averted_with_PMC_clin <- cases_averted_with_PMC_sev <- cases_averted_with_PMC_tot <- cases_averted_with_PMC_asym <- c()
  
  # cases averted (per 1000 children in that age group)
  cases_averted_with_PMC_clin_per1000 <- cases_averted_with_PMC_sev_per1000 <- cases_averted_with_PMC_tot_per1000 <- cases_averted_with_PMC_asym_per1000 <- c()
  
  # cases reduction 
  cases_reduction_with_PMC_clin <- cases_reduction_with_PMC_sev <- cases_reduction_with_PMC_tot <- cases_reduction_with_PMC_asym <- c()
  
  
  for (j in 1:length(unique(annual_results_sixmonth$`Age group`))) {
    
    # sum of cases (no PMC) for each 6 month age group
    cases_no_PMC_clin<- c(cases_no_PMC_clin, sum((annual_results_sixmonth %>% filter(`Age group` == annual_results_sixmonth$`Age group`[j]))$clinical_cases_no_PMC))
    cases_no_PMC_sev<- c(cases_no_PMC_sev, sum((annual_results_sixmonth %>% filter(`Age group` == annual_results_sixmonth$`Age group`[j]))$severe_cases_no_PMC))
    # cases_no_PMC_tot<- c(cases_no_PMC_tot, sum((annual_results_sixmonth %>% filter(`Age group` == annual_results_sixmonth$`Age group`[j]))$total_cases_no_PMC))
    # cases_no_PMC_asym<- c(cases_no_PMC_asym, sum((annual_results_sixmonth %>% filter(`Age group` == annual_results_sixmonth$`Age group`[j]))$asymptomatic_cases_no_PMC))
    # 
    # sum of cases (with PMC) for each 6 month age group
    cases_with_PMC_clin<- c(cases_with_PMC_clin, sum((annual_results_sixmonth %>% filter(`Age group` == annual_results_sixmonth$`Age group`[j]))$clinical_cases_with_PMC))
    cases_with_PMC_sev<- c(cases_with_PMC_sev, sum((annual_results_sixmonth %>% filter(`Age group` == annual_results_sixmonth$`Age group`[j]))$severe_cases_with_PMC))
    # cases_with_PMC_tot<- c(cases_with_PMC_tot, sum((annual_results_sixmonth %>% filter(`Age group` == annual_results_sixmonth$`Age group`[j]))$total_cases_with_PMC))
    # cases_with_PMC_asym<- c(cases_with_PMC_asym, sum((annual_results_sixmonth %>% filter(`Age group` == annual_results_sixmonth$`Age group`[j]))$asymptomatic_cases_with_PMC))
    # 
    # sum of cases (no PMC) for each 6 month age group (per 1000 children in that age group)
    cases_no_PMC_clin_per1000<- c(cases_no_PMC_clin_per1000, sum((annual_results_sixmonth %>% filter(`Age group` == annual_results_sixmonth$`Age group`[j]))$clinical_cases_no_PMC_per1000 *unique(population_weights)))
    cases_no_PMC_sev_per1000<- c(cases_no_PMC_sev_per1000, sum((annual_results_sixmonth %>% filter(`Age group` == annual_results_sixmonth$`Age group`[j]))$severe_cases_no_PMC_per1000 *unique(population_weights)))
    # cases_no_PMC_tot_per1000<- c(cases_no_PMC_tot_per1000, sum((annual_results_sixmonth %>% filter(`Age group` == annual_results_sixmonth$`Age group`[j]))$total_cases_no_PMC_per1000 *unique(population_weights)))
    # cases_no_PMC_asym_per1000<- c(cases_no_PMC_asym_per1000, sum((annual_results_sixmonth %>% filter(`Age group` == annual_results_sixmonth$`Age group`[j]))$asymptomatic_cases_no_PMC_per1000 *unique(population_weights)))
    # 
    # sum of cases (with PMC) for each 6 month age group (per 1000 children in that age group)
    cases_with_PMC_clin_per1000<- c(cases_with_PMC_clin_per1000, sum((annual_results_sixmonth %>% filter(`Age group` == annual_results_sixmonth$`Age group`[j]))$clinical_cases_with_PMC_per1000 *unique(population_weights)))
    cases_with_PMC_sev_per1000<- c(cases_with_PMC_sev_per1000, sum((annual_results_sixmonth %>% filter(`Age group` == annual_results_sixmonth$`Age group`[j]))$severe_cases_with_PMC_per1000 *unique(population_weights)))
    # cases_with_PMC_tot_per1000<- c(cases_with_PMC_tot_per1000, sum((annual_results_sixmonth %>% filter(`Age group` == annual_results_sixmonth$`Age group`[j]))$total_cases_with_PMC_per1000 *unique(population_weights)))
    # cases_with_PMC_asym_per1000<- c(cases_with_PMC_asym_per1000, sum((annual_results_sixmonth %>% filter(`Age group` == annual_results_sixmonth$`Age group`[j]))$asymptomatic_cases_with_PMC_per1000 *unique(population_weights)))
    # 
    
  }
  
   
  
  # cases averted (with PMC) by infection class (per 1000 children in that age group)
  cases_averted_with_PMC_clin <- (cases_no_PMC_clin - cases_with_PMC_clin)
  cases_averted_with_PMC_sev <- (cases_no_PMC_sev - cases_with_PMC_sev)
  # cases_averted_with_PMC_tot <- (cases_no_PMC_tot - cases_with_PMC_tot)
  # cases_averted_with_PMC_asym <- (cases_no_PMC_asym - cases_with_PMC_asym)
  # 
  # cases averted (with PMC) by infection class (per 1000 children in that age group)
  cases_averted_with_PMC_clin_per1000 <- (cases_no_PMC_clin_per1000 - cases_with_PMC_clin_per1000)
  cases_averted_with_PMC_sev_per1000 <- (cases_no_PMC_sev_per1000 - cases_with_PMC_sev_per1000)
  # cases_averted_with_PMC_tot_per1000 <- (cases_no_PMC_tot_per1000 - cases_with_PMC_tot_per1000)
  # cases_averted_with_PMC_asym_per1000 <- (cases_no_PMC_asym_per1000 - cases_with_PMC_asym_per1000)
  # 
  # reduction in cases (with PMC) by infection class
  cases_reduction_with_PMC_clin <- (cases_no_PMC_clin - cases_with_PMC_clin)/cases_no_PMC_clin * 100
  cases_reduction_with_PMC_sev <- (cases_no_PMC_sev - cases_with_PMC_sev)/cases_no_PMC_sev * 100
  # cases_reduction_with_PMC_tot <- (cases_no_PMC_tot - cases_with_PMC_tot)/cases_no_PMC_tot * 100
  # cases_reduction_with_PMC_asym <- (cases_no_PMC_asym - cases_with_PMC_asym)/cases_no_PMC_asym * 100
  # 
  
  ##### APPEND VECTORS TO CALCULATION DATAFRAMES (6 MONTH AGE GROUP INTERVALS) #####
  
  annual_results_sixmonths_whole_country$clinical_cases_no_PMC <- cases_no_PMC_clin
  annual_results_sixmonths_whole_country$severe_cases_no_PMC <- cases_no_PMC_sev
  # annual_results_sixmonths_whole_country$total_cases_no_PMC <- cases_no_PMC_tot
  # annual_results_sixmonths_whole_country$asymptomatic_cases_no_PMC<- cases_no_PMC_asym
  # 
  
  annual_results_sixmonths_whole_country$clinical_cases_no_PMC_per1000 <- cases_no_PMC_clin_per1000
  annual_results_sixmonths_whole_country$severe_cases_no_PMC_per1000 <- cases_no_PMC_sev_per1000
  # annual_results_sixmonths_whole_country$total_cases_no_PMC_per1000 <- cases_no_PMC_tot_per1000
  # annual_results_sixmonths_whole_country$asymptomatic_cases_no_PMC_per1000<- cases_no_PMC_asym_per1000
  # 
  annual_results_sixmonths_whole_country$clinical_cases_with_PMC<- cases_with_PMC_clin
  annual_results_sixmonths_whole_country$severe_cases_with_PMC<- cases_with_PMC_sev
  # annual_results_sixmonths_whole_country$total_cases_with_PMC<- cases_with_PMC_tot
  # annual_results_sixmonths_whole_country$asymptomatic_cases_with_PMC<- cases_with_PMC_asym
  # 
  annual_results_sixmonths_whole_country$clinical_cases_with_PMC_per1000<- cases_with_PMC_clin_per1000
  annual_results_sixmonths_whole_country$severe_cases_with_PMC_per1000<- cases_with_PMC_sev_per1000
  # annual_results_sixmonths_whole_country$total_cases_with_PMC_per1000<- cases_with_PMC_tot_per1000
  # annual_results_sixmonths_whole_country$asymptomatic_cases_with_PMC_per1000<- cases_with_PMC_asym_per1000
  # 
  annual_results_sixmonths_whole_country$clinical_cases_averted_with_PMC <- cases_averted_with_PMC_clin
  annual_results_sixmonths_whole_country$severe_cases_averted_with_PMC <- cases_averted_with_PMC_sev
  # annual_results_sixmonths_whole_country$total_cases_averted_with_PMC <- cases_averted_with_PMC_tot
  # annual_results_sixmonths_whole_country$asymptomatic_cases_averted_with_PMC <- cases_averted_with_PMC_asym
  # 
  annual_results_sixmonths_whole_country$clinical_cases_averted_with_PMC_per1000 <- cases_averted_with_PMC_clin_per1000
  annual_results_sixmonths_whole_country$severe_cases_averted_with_PMC_per1000 <- cases_averted_with_PMC_sev_per1000
  # annual_results_sixmonths_whole_country$total_cases_averted_with_PMC_per1000 <- cases_averted_with_PMC_tot_per1000
  # annual_results_sixmonths_whole_country$asymptomatic_cases_averted_with_PMC_per1000 <- cases_averted_with_PMC_asym_per1000
  # 
  annual_results_sixmonths_whole_country$clinical_cases_reduction <- cases_reduction_with_PMC_clin
  annual_results_sixmonths_whole_country$severe_cases_reduction <- cases_reduction_with_PMC_sev
  # annual_results_sixmonths_whole_country$total_cases_reduction <- cases_reduction_with_PMC_tot
  # annual_results_sixmonths_whole_country$asymptomatic_cases_reduction <- cases_reduction_with_PMC_asym
  # 
  

}




} else {
  
  
  ##### CALCULATE PMC IMPACT FOR EACH ADMIN-1 AREA (both PMC schedules) #####
   
  
  PMC_impact_ppy <- data.frame()
  
  for (i in 1:length(area_names)){
    
    # PMC impact on incidence (ppy)
    new_PMC_impact_ppy_df <- (incidence_ppy_df %>% filter(`Admin-1 unit` == area_names[i]))
    
    ages_needed <- unique(new_PMC_impact_ppy_df$age_in_days_midpoint)
    
    PMC_impact_ppy_cases <- as.double(new_PMC_impact_ppy_df$value) * rep(1-df[[paste0("prot_overall_", area_names[i])]][ages_needed], times=2)
    
    
    new_PMC_impact_ppy_df$value <- PMC_impact_ppy_cases
    
    PMC_impact_ppy <- rbind(PMC_impact_ppy, new_PMC_impact_ppy_df)
    
  }
  
  
  
  ##### GRAPHS AND ANALYSIS RESULTS #####
  
  number_of_doses <- length(schedule)
  
  merged_df_by_age_COMPLETE <- c()
  merged_df_annual_sixmonths_COMPLETE <- c()
  merged_df_annual_COMPLETE <- c()
  

  for (k in 1:length(number_of_doses)) {
    
    rur_or_urb <- "merged"
    
    
    
    #area_names <- unique(PMC_impact_ppy$area)
    
    ##### SET UP CALCULATION DATAFRAMES  #####
    
    # raw baseline cases (no PMC) for every age group
    results_by_age<- data.frame(Country=rep(country, length(area_names)*length(age_in_days_midpoint)),
                                `Age group` = rep(age_group_names,length(area_names)),
                                `Admin-1 unit` = rep(area_names, each=length(age_in_days_midpoint)),
                                `N doses` = rep(number_of_doses[k], length(area_names)*length(age_in_days_midpoint)), check.names=F)
    
    # average baseline annual cases (no PMC) for whole age group
    annual_results<- data.frame(Country=rep(country, length(area_names)),
                                `Age group` = paste0("n_age_0_", max(age_max)),
                                `Admin-1 unit` = area_names,
                                `N doses` =rep(number_of_doses[k], length(area_names)), check.names=F)
    
    
    ##### INITIALISE VECTORS FOR CALCULATIONS #####
    
    # raw cases
    current_cases_no_PMC_clin <- current_cases_no_PMC_sev <- current_cases_no_PMC_tot <- current_cases_no_PMC_asym <- c()
    current_cases_averted_with_PMC_clin <- current_cases_averted_with_PMC_sev <- current_cases_averted_with_PMC_tot <- current_cases_averted_with_PMC_asym <- c()
    current_cases_with_PMC_clin <- current_cases_with_PMC_sev <- current_cases_with_PMC_tot <- current_cases_with_PMC_asym <- c()
    
    # cases per 1000 children in that age group
    current_cases_no_PMC_clin_per1000 <- current_cases_no_PMC_sev_per1000 <- current_cases_no_PMC_tot_per1000 <- current_cases_no_PMC_asym_per1000 <- c()
    current_cases_averted_with_PMC_clin_per1000 <- current_cases_averted_with_PMC_sev_per1000 <- current_cases_averted_with_PMC_tot_per1000 <- current_cases_averted_with_PMC_asym_per1000 <- c()
    current_cases_with_PMC_clin_per1000 <- current_cases_with_PMC_sev_per1000 <- current_cases_with_PMC_tot_per1000 <- current_cases_with_PMC_asym_per1000 <- c()
    
    # % reduction with PMC 
    current_cases_reduction_with_PMC_clin <- current_cases_reduction_with_PMC_sev <- current_cases_reduction_with_PMC_tot <- current_cases_reduction_with_PMC_asym <- c()
    
    # annual cases
    annual_current_cases_no_PMC_clin <- annual_current_cases_no_PMC_sev <- annual_current_cases_no_PMC_tot <- annual_current_cases_no_PMC_asym <- c()
    annual_current_cases_with_PMC_clin <- annual_current_cases_with_PMC_sev <- annual_current_cases_with_PMC_tot <- annual_current_cases_with_PMC_asym <- c()
    annual_current_cases_averted_with_PMC_clin <- annual_current_cases_averted_with_PMC_sev <- annual_current_cases_averted_with_PMC_tot <- annual_current_cases_averted_with_PMC_asym <- c()
    
    # annual cases (per 1000 children in that age group)
    annual_current_cases_no_PMC_clin_per1000 <- annual_current_cases_no_PMC_sev_per1000 <- annual_current_cases_no_PMC_tot_per1000 <- annual_current_cases_no_PMC_asym_per1000 <- c()
    annual_current_cases_with_PMC_clin_per1000 <- annual_current_cases_with_PMC_sev_per1000 <- annual_current_cases_with_PMC_tot_per1000 <- annual_current_cases_with_PMC_asym_per1000 <- c()
    annual_current_cases_averted_with_PMC_clin_per1000 <- annual_current_cases_averted_with_PMC_sev_per1000 <- annual_current_cases_averted_with_PMC_tot_per1000 <- annual_current_cases_averted_with_PMC_asym_per1000 <- c()
    
    
    
    
    ##### RUN CALCULATIONS ACROSS WHOLE COUNTRY #####
    
    for (i in 1:length(area_names)){
      
      # extracting average age group proportions from the imperial model age distributions 
      age_distribution <- as.numeric(rep(unlist((population_df_age_structure %>% filter(Admin.1.unit == area_names[i]))[,4:dim(population_df_age_structure)[2]]),2))
      
      
      # sum of rural and urban areas as weighted mean is used 
      
      area_population <- (all_sites_for_DT %>% filter(Country==input$country, `Admin-1 unit`==area_names[i]))$population_total
      
      
      
      # number of cases in each age group per year (no PMC)
      current_cases_no_PMC <- (incidence_ppy_df %>% filter(`Admin-1 unit`==area_names[i]))$value * age_distribution * area_population
      
      # number of cases in each age group per year (no PMC) by infection class
      current_cases_no_PMC_clin <- c(current_cases_no_PMC_clin, current_cases_no_PMC[1:131])
      current_cases_no_PMC_sev <- c(current_cases_no_PMC_sev, current_cases_no_PMC[(132):262])
      # current_cases_no_PMC_tot <- c(current_cases_no_PMC_tot, current_cases_no_PMC[(1+2*max(age_max)):(3*max(age_max))])
      # current_cases_no_PMC_asym <- c(current_cases_no_PMC_asym, current_cases_no_PMC[(1+3*max(age_max)):(4*max(age_max))])
      # 
      # annual number of cases in each age group per year (no PMC) by infection class
      annual_current_cases_no_PMC_clin <- c(annual_current_cases_no_PMC_clin, sum(current_cases_no_PMC[1:131]))
      annual_current_cases_no_PMC_sev <- c(annual_current_cases_no_PMC_sev, sum(current_cases_no_PMC[(132):262]))
      # annual_current_cases_no_PMC_tot <- c(annual_current_cases_no_PMC_tot, sum(current_cases_no_PMC[(1+2*max(age_max)):(3*max(age_max))]))
      # annual_current_cases_no_PMC_asym <- c(annual_current_cases_no_PMC_asym, sum(current_cases_no_PMC[(1+3*max(age_max)):(4*max(age_max))]))
      # 
      # number of cases in each age group per year (no PMC) (per 1000 children in that age group)
      current_cases_no_PMC_per1000 <- (incidence_ppy_df %>% filter(`Admin-1 unit`==area_names[i]))$value * 1000
      
      # number of cases in each age group per year (no PMC) by infection class (per 1000 children in that age group)
      current_cases_no_PMC_clin_per1000 <- c(current_cases_no_PMC_clin_per1000, current_cases_no_PMC_per1000[1:131])
      current_cases_no_PMC_sev_per1000 <- c(current_cases_no_PMC_sev_per1000, current_cases_no_PMC_per1000[(132):262])
      # current_cases_no_PMC_tot_per1000 <- c(current_cases_no_PMC_tot_per1000, current_cases_no_PMC_per1000[(1+2*max(age_max)):(3*max(age_max))])
      # current_cases_no_PMC_asym_per1000 <- c(current_cases_no_PMC_asym_per1000, current_cases_no_PMC_per1000[(1+3*max(age_max)):(4*max(age_max))])
      # 
      # annual number of cases in each age group per year (no PMC) by infection class (per 1000 children in that age group)
      annual_current_cases_no_PMC_clin_per1000 <- c(annual_current_cases_no_PMC_clin_per1000, mean(current_cases_no_PMC_per1000[1:131]))
      annual_current_cases_no_PMC_sev_per1000 <- c(annual_current_cases_no_PMC_sev_per1000, mean(current_cases_no_PMC_per1000[(132):262]))
      # annual_current_cases_no_PMC_tot_per1000 <- c(annual_current_cases_no_PMC_tot_per1000, mean(current_cases_no_PMC_per1000[(1+2*max(age_max)):(3*max(age_max))]))
      # annual_current_cases_no_PMC_asym_per1000 <- c(annual_current_cases_no_PMC_asym_per1000, mean(current_cases_no_PMC_per1000[(1+3*max(age_max)):(4*max(age_max))]))
      # 
      
      
      # number of cases in each age group per year (with PMC)
      current_cases_with_PMC <- (PMC_impact_ppy %>% filter(`Admin-1 unit`==area_names[i]))$value * age_distribution * area_population
      
      # number of cases in each age group per year (with PMC) (per 1000 children in that age group)
      current_cases_with_PMC_per1000 <- (PMC_impact_ppy %>% filter(`Admin-1 unit`==area_names[i]))$value * 1000
      
      # number of cases in each age group per year (with PMC) by infection class
      current_cases_with_PMC_clin <- c(current_cases_with_PMC_clin, current_cases_with_PMC[1:131])
      current_cases_with_PMC_sev <- c(current_cases_with_PMC_sev, current_cases_with_PMC[(132):262])
      # current_cases_with_PMC_tot <- c(current_cases_with_PMC_tot, current_cases_with_PMC[(1+2*max(age_max)):(3*max(age_max))])
      # current_cases_with_PMC_asym <- c(current_cases_with_PMC_asym, current_cases_with_PMC[(1+3*max(age_max)):(4*max(age_max))])
      # 
      
      # number of cases in each age group per year (with PMC) by infection class (per 1000 children in that age group)
      current_cases_with_PMC_clin_per1000 <- c(current_cases_with_PMC_clin_per1000, current_cases_with_PMC_per1000[1:131])
      current_cases_with_PMC_sev_per1000 <- c(current_cases_with_PMC_sev_per1000, current_cases_with_PMC_per1000[(132):262])
      # current_cases_with_PMC_tot_per1000 <- c(current_cases_with_PMC_tot_per1000, current_cases_with_PMC_per1000[(1+2*max(age_max)):(3*max(age_max))])
      # current_cases_with_PMC_asym_per1000 <- c(current_cases_with_PMC_asym_per1000, current_cases_with_PMC_per1000[(1+3*max(age_max)):(4*max(age_max))])
      # 
      
      # annual number of cases in each age group per year (with PMC) by infection class
      annual_current_cases_with_PMC_clin <- c(annual_current_cases_with_PMC_clin, sum(current_cases_with_PMC[1:131]))
      annual_current_cases_with_PMC_sev <- c(annual_current_cases_with_PMC_sev, sum(current_cases_with_PMC[(132):262]))
      # annual_current_cases_with_PMC_tot <- c(annual_current_cases_with_PMC_tot, sum(current_cases_with_PMC[(1+2*max(age_max)):(3*max(age_max))]))
      # annual_current_cases_with_PMC_asym <- c(annual_current_cases_with_PMC_asym, sum(current_cases_with_PMC[(1+3*max(age_max)):(4*max(age_max))]))
      # 
      
      # annual number of cases in each age group per year (with PMC) by infection class (per 1000 children in that age group)
      annual_current_cases_with_PMC_clin_per1000 <- c(annual_current_cases_with_PMC_clin_per1000, mean(current_cases_with_PMC_per1000[1:131]))
      annual_current_cases_with_PMC_sev_per1000 <- c(annual_current_cases_with_PMC_sev_per1000, mean(current_cases_with_PMC_per1000[(132):262]))
      # annual_current_cases_with_PMC_tot_per1000 <- c(annual_current_cases_with_PMC_tot_per1000, mean(current_cases_with_PMC_per1000[(1+2*max(age_max)):(3*max(age_max))]))
      # annual_current_cases_with_PMC_asym_per1000 <- c(annual_current_cases_with_PMC_asym_per1000, mean(current_cases_with_PMC_per1000[(1+3*max(age_max)):(4*max(age_max))]))
      # 
      
      # average reduction in cases (with PMC) by infection class 
      current_cases_reduction_with_PMC_clin <- c(current_cases_reduction_with_PMC_clin, (mean(current_cases_no_PMC[1:131]) - mean(current_cases_with_PMC[1:131])) / mean(current_cases_no_PMC[1:131]) * 100)
      current_cases_reduction_with_PMC_sev <- c(current_cases_reduction_with_PMC_sev, (mean(current_cases_no_PMC[(132):262]) - mean(current_cases_with_PMC[(132):262])) / mean(current_cases_no_PMC[(132):262]) * 100)
      # current_cases_reduction_with_PMC_tot <- c(current_cases_reduction_with_PMC_tot, (mean(current_cases_no_PMC[(1+2*max(age_max)):(3*max(age_max))]) - mean(current_cases_with_PMC[(1+2*max(age_max)):(3*max(age_max))])) / mean(current_cases_no_PMC[(1+2*max(age_max)):(3*max(age_max))]) * 100)
      # current_cases_reduction_with_PMC_asym <- c(current_cases_reduction_with_PMC_asym, (mean(current_cases_no_PMC[(1+3*max(age_max)):(4*max(age_max))]) - mean(current_cases_with_PMC[(1+3*max(age_max)):(4*max(age_max))])) / mean(current_cases_no_PMC[(1+3*max(age_max)):(4*max(age_max))]) * 100)
      # 
      
    }
    
    # raw cases averted (with PMC) by infection class
    cases_averted_with_PMC_clin <- (current_cases_no_PMC_clin - current_cases_with_PMC_clin)
    cases_averted_with_PMC_sev <- (current_cases_no_PMC_sev - current_cases_with_PMC_sev)
    # cases_averted_with_PMC_tot <- (current_cases_no_PMC_tot - current_cases_with_PMC_tot)
    # cases_averted_with_PMC_asym <- (current_cases_no_PMC_asym - current_cases_with_PMC_asym)
    # 
    
    # raw cases averted (with PMC) by infection class (per 1000 children in that age group)
    cases_averted_with_PMC_clin_per1000 <- (current_cases_no_PMC_clin_per1000 - current_cases_with_PMC_clin_per1000)
    cases_averted_with_PMC_sev_per1000 <- (current_cases_no_PMC_sev_per1000 - current_cases_with_PMC_sev_per1000)
    # cases_averted_with_PMC_tot_per1000 <- (current_cases_no_PMC_tot_per1000 - current_cases_with_PMC_tot_per1000)
    # cases_averted_with_PMC_asym_per1000 <- (current_cases_no_PMC_asym_per1000 - current_cases_with_PMC_asym_per1000)
    # 
    # annual cases averted (with PMC) by infection class
    annual_cases_averted_with_PMC_clin <- (annual_current_cases_no_PMC_clin - annual_current_cases_with_PMC_clin)
    annual_cases_averted_with_PMC_sev <- (annual_current_cases_no_PMC_sev - annual_current_cases_with_PMC_sev)
    # annual_cases_averted_with_PMC_tot <- (annual_current_cases_no_PMC_tot - annual_current_cases_with_PMC_tot)
    # annual_cases_averted_with_PMC_asym <- (annual_current_cases_no_PMC_asym - annual_current_cases_with_PMC_asym)
    # 
    
    # annual cases averted (with PMC) by infection class (per 1000 children in that age group)
    annual_cases_averted_with_PMC_clin_per1000 <- (annual_current_cases_no_PMC_clin_per1000 - annual_current_cases_with_PMC_clin_per1000)
    annual_cases_averted_with_PMC_sev_per1000 <- (annual_current_cases_no_PMC_sev_per1000 - annual_current_cases_with_PMC_sev_per1000)
    # annual_cases_averted_with_PMC_tot_per1000 <- (annual_current_cases_no_PMC_tot_per1000 - annual_current_cases_with_PMC_tot_per1000)
    # annual_cases_averted_with_PMC_asym_per1000 <- (annual_current_cases_no_PMC_asym_per1000 - annual_current_cases_with_PMC_asym_per1000)
    # 
    
    # average reduction in cases by infection class 
    cases_reduction_with_PMC_clin <- (current_cases_no_PMC_clin - current_cases_with_PMC_clin)/current_cases_no_PMC_clin * 100
    cases_reduction_with_PMC_sev <- (current_cases_no_PMC_sev - current_cases_with_PMC_sev)/current_cases_no_PMC_sev * 100
    # cases_reduction_with_PMC_tot <- (current_cases_no_PMC_tot - current_cases_with_PMC_tot)/current_cases_no_PMC_tot * 100
    # cases_reduction_with_PMC_asym <- (current_cases_no_PMC_asym - current_cases_with_PMC_asym)/current_cases_no_PMC_asym * 100
    # 
    
    ##### APPEND VECTORS TO CALCULATION DATAFRAMES #####
    
    results_by_age$clinical_cases_no_PMC <- current_cases_no_PMC_clin
    results_by_age$severe_cases_no_PMC <- current_cases_no_PMC_sev
    # results_by_age$total_cases_no_PMC <- current_cases_no_PMC_tot
    # results_by_age$asymptomatic_cases_no_PMC <- current_cases_no_PMC_asym
    
    
    results_by_age$clinical_cases_no_PMC_per1000 <- current_cases_no_PMC_clin_per1000
    results_by_age$severe_cases_no_PMC_per1000 <- current_cases_no_PMC_sev_per1000
    # results_by_age$total_cases_no_PMC_per1000 <- current_cases_no_PMC_tot_per1000
    # results_by_age$asymptomatic_cases_no_PMC_per1000 <- current_cases_no_PMC_asym_per1000
    # 
    annual_results$clinical_cases_no_PMC <- annual_current_cases_no_PMC_clin
    annual_results$severe_cases_no_PMC <- annual_current_cases_no_PMC_sev
    # annual_results$total_cases_no_PMC <- annual_current_cases_no_PMC_tot
    # annual_results$asymptomatic_cases_no_PMC <- annual_current_cases_no_PMC_asym
    
    annual_results$clinical_cases_no_PMC_per1000 <- annual_current_cases_no_PMC_clin_per1000
    annual_results$severe_cases_no_PMC_per1000 <- annual_current_cases_no_PMC_sev_per1000
    # annual_results$total_cases_no_PMC_per1000 <- annual_current_cases_no_PMC_tot_per1000
    # annual_results$asymptomatic_cases_no_PMC_per1000 <- annual_current_cases_no_PMC_asym_per1000
    # 
    results_by_age$clinical_cases_averted_with_PMC <- cases_averted_with_PMC_clin
    results_by_age$severe_cases_averted_with_PMC <- cases_averted_with_PMC_sev
    # results_by_age$total_cases_averted_with_PMC <- cases_averted_with_PMC_tot
    # results_by_age$asymptomatic_cases_averted_with_PMC <- cases_averted_with_PMC_asym
    
    results_by_age$clinical_cases_averted_with_PMC_per1000 <- cases_averted_with_PMC_clin_per1000
    results_by_age$severe_cases_averted_with_PMC_per1000 <- cases_averted_with_PMC_sev_per1000
    # results_by_age$total_cases_averted_with_PMC_per1000 <- cases_averted_with_PMC_tot_per1000
    # results_by_age$asymptomatic_cases_averted_with_PMC_per1000<- cases_averted_with_PMC_asym_per1000
    # 
    annual_results$clinical_cases_averted_with_PMC  <- annual_cases_averted_with_PMC_clin
    annual_results$severe_cases_averted_with_PMC  <- annual_cases_averted_with_PMC_sev
    # annual_results$total_cases_averted_with_PMC  <- annual_cases_averted_with_PMC_tot
    # annual_results$asymptomatic_cases_averted_with_PMC  <- annual_cases_averted_with_PMC_asym
    
    annual_results$clinical_cases_averted_with_PMC_per1000 <- annual_cases_averted_with_PMC_clin_per1000
    annual_results$severe_cases_averted_with_PMC_per1000 <- annual_cases_averted_with_PMC_sev_per1000
    # annual_results$total_cases_averted_with_PMC_per1000 <- annual_cases_averted_with_PMC_tot_per1000
    # annual_results$asymptomatic_cases_averted_with_PMC_per1000 <- annual_cases_averted_with_PMC_asym_per1000
    # 
    results_by_age$clinical_cases_with_PMC <- current_cases_with_PMC_clin
    results_by_age$severe_cases_with_PMC <- current_cases_with_PMC_sev
    # results_by_age$total_cases_with_PMC <- current_cases_with_PMC_tot
    # results_by_age$asymptomatic_cases_with_PMC <- current_cases_with_PMC_asym
    
    results_by_age$clinical_cases_with_PMC_per1000 <- current_cases_with_PMC_clin_per1000
    results_by_age$severe_cases_with_PMC_per1000 <- current_cases_with_PMC_sev_per1000
    # results_by_age$total_cases_with_PMC_per1000 <- current_cases_with_PMC_tot_per1000
    # results_by_age$asymptomatic_cases_with_PMC_per1000 <- current_cases_with_PMC_asym_per1000
    # 
    annual_results$clinical_cases_with_PMC <- annual_current_cases_with_PMC_clin
    annual_results$severe_cases_with_PMC <- annual_current_cases_with_PMC_sev
    # annual_results$total_cases_with_PMC <- annual_current_cases_with_PMC_tot
    # annual_results$asymptomatic_cases_with_PMC <- annual_current_cases_with_PMC_asym
    
    annual_results$clinical_cases_with_PMC_per1000 <- annual_current_cases_with_PMC_clin_per1000
    annual_results$severe_cases_with_PMC_per1000 <- annual_current_cases_with_PMC_sev_per1000
    # annual_results$total_cases_with_PMC_per1000 <- annual_current_cases_with_PMC_tot_per1000
    # annual_results$asymptomatic_cases_with_PMC_per1000 <- annual_current_cases_with_PMC_asym_per1000
    # 
    annual_results$clinical_cases_reduction <- current_cases_reduction_with_PMC_clin
    annual_results$severe_cases_reduction <- current_cases_reduction_with_PMC_sev
    # annual_results$total_cases_reduction <- current_cases_reduction_with_PMC_tot
    # annual_results$asymptomatic_cases_reduction <- current_cases_reduction_with_PMC_asym
    # 
    ##### SET UP CALCULATION DATAFRAMES (6 MONTH AGE GROUP INTERVALS) #####
    
    # raw baseline cases (no PMC)
    annual_results_sixmonth<- data.frame(Country=rep(country, length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                         `Age group` = rep(age_group_names_sixmonth[1:no_sixmonth_intervals], times=length(area_names)),
                                         `Admin-1 unit` = rep(area_names, each=length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])),
                                         `N doses` = rep(number_of_doses[k], length(area_names)*length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals])), check.names = F)
    
    
    ##### INITIALISE VECTORS FOR CALCULATIONS (6 MONTH AGE GROUP INTERVALS) #####
    
    # raw cases
    cases_no_PMC_clin <- cases_no_PMC_sev <- cases_no_PMC_tot <- cases_no_PMC_asym <- c()
    cases_with_PMC_clin <- cases_with_PMC_sev <- cases_with_PMC_tot <- cases_with_PMC_asym <- c()
    
    # cases per 1000 children in that age group
    cases_no_PMC_clin_per1000 <- cases_no_PMC_sev_per1000 <- cases_no_PMC_tot_per1000 <- cases_no_PMC_asym_per1000 <- c()
    cases_with_PMC_clin_per1000 <- cases_with_PMC_sev_per1000 <- cases_with_PMC_tot_per1000 <- cases_with_PMC_asym_per1000 <- c()
    
    # cases averted (with PMC) 
    cases_averted_with_PMC_clin <- cases_averted_with_PMC_sev <- cases_averted_with_PMC_tot <- cases_averted_with_PMC_asym <- c()
    
    # cases averted (per 1000 children in that age group)
    cases_averted_with_PMC_clin_per1000 <- cases_averted_with_PMC_sev_per1000 <- cases_averted_with_PMC_tot_per1000 <- cases_averted_with_PMC_asym_per1000 <- c()
    
    # cases reduction 
    cases_reduction_with_PMC_clin <- cases_reduction_with_PMC_sev <- cases_reduction_with_PMC_tot <- cases_reduction_with_PMC_asym <- c()
    
    ##### RUN CALCULATIONS ACROSS WHOLE COUNTRY (6 MONTH AGE GROUP INTERVALS) #####
    
    for (i in 1:length(area_names)) {
      
      # raw cases (no PMC) by infection class
      current_cases_no_PMC_clin <- (results_by_age %>% filter(`Admin-1 unit` == area_names[i]))$clinical_cases_no_PMC
      current_cases_no_PMC_sev <- (results_by_age %>% filter(`Admin-1 unit` == area_names[i]))$severe_cases_no_PMC
      # current_cases_no_PMC_tot <- (results_by_age %>% filter(`Admin-1 unit` == area_names[i]))$total_cases_no_PMC
      # current_cases_no_PMC_asym <- (results_by_age %>% filter(`Admin-1 unit` == area_names[i]))$asymptomatic_cases_no_PMC
      # 
      # raw cases (with PMC) by infection class
      current_cases_with_PMC_clin <- (results_by_age %>% filter(`Admin-1 unit` == area_names[i]))$clinical_cases_with_PMC
      current_cases_with_PMC_sev <- (results_by_age %>% filter(`Admin-1 unit` == area_names[i]))$severe_cases_with_PMC
      # current_cases_with_PMC_tot <- (results_by_age %>% filter(`Admin-1 unit` == area_names[i]))$total_cases_with_PMC
      # current_cases_with_PMC_asym <- (results_by_age %>% filter(`Admin-1 unit` == area_names[i]))$asymptomatic_cases_with_PMC
      # 
      
      # cases (no PMC) by infection class (per 1000 children in that age group)
      current_cases_no_PMC_clin_per1000 <- (incidence_ppy_df %>% filter(`Admin-1 unit` == area_names[i], infection_class == "clinical"))$value * 1000
      current_cases_no_PMC_sev_per1000 <- (incidence_ppy_df %>% filter(`Admin-1 unit` == area_names[i], infection_class == "severe"))$value * 1000
      # current_cases_no_PMC_tot_per1000 <- (incidence_ppy_df %>% filter(`Admin-1 unit` == area_names[i], infection_class == "total"))$value * 1000
      # current_cases_no_PMC_asym_per1000 <- (incidence_ppy_df %>% filter(`Admin-1 unit` == area_names[i], infection_class == "asymptomatic"))$value * 1000
      # 
      # cases (with PMC) by infection class (per 1000 children in that age group)
      current_cases_with_PMC_clin_per1000 <- (PMC_impact_ppy %>% filter(`Admin-1 unit` == area_names[i], infection_class == "clinical"))$value * 1000
      current_cases_with_PMC_sev_per1000 <- (PMC_impact_ppy %>% filter(`Admin-1 unit` == area_names[i], infection_class == "severe"))$value * 1000
      # current_cases_with_PMC_tot_per1000 <- (PMC_impact_ppy %>% filter(`Admin-1 unit` == area_names[i], infection_class == "total"))$value * 1000
      # current_cases_with_PMC_asym_per1000 <- (PMC_impact_ppy %>% filter(`Admin-1 unit` == area_names[i], infection_class == "asymptomatic"))$value * 1000
      # 
      
      
      # split data into each 6 month age group for each infection class (no PMC)
      split_current_cases_no_PMC_clin <- split(current_cases_no_PMC_clin, c(rep(1, times=26),rep(2, times=26), rep(3, times=26), rep(4, times=26), rep(5, times=27)))
      names(split_current_cases_no_PMC_clin) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
      
      split_current_cases_no_PMC_sev <- split(current_cases_no_PMC_sev, c(rep(1, times=26),rep(2, times=26), rep(3, times=26), rep(4, times=26), rep(5, times=27)))
      names(split_current_cases_no_PMC_sev) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
      # 
      # split_current_cases_no_PMC_tot <- split(current_cases_no_PMC_tot, ceiling(seq_along(current_cases_no_PMC_tot)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
      # names(split_current_cases_no_PMC_tot) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
      # 
      # split_current_cases_no_PMC_asym <- split(current_cases_no_PMC_asym, ceiling(seq_along(current_cases_no_PMC_asym)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
      # names(split_current_cases_no_PMC_asym) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
      # 
      
      # split data into each 6 month age group for each infection class (with PMC)
      split_current_cases_with_PMC_clin <- split(current_cases_with_PMC_clin,  c(rep(1, times=26),rep(2, times=26), rep(3, times=26), rep(4, times=26), rep(5, times=27)))
      names(split_current_cases_with_PMC_clin) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
      
      split_current_cases_with_PMC_sev <- split(current_cases_with_PMC_sev,  c(rep(1, times=26),rep(2, times=26), rep(3, times=26), rep(4, times=26), rep(5, times=27)))
      names(split_current_cases_with_PMC_sev) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
      
      # split_current_cases_with_PMC_tot <- split(current_cases_with_PMC_tot, ceiling(seq_along(current_cases_with_PMC_tot)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
      # names(split_current_cases_with_PMC_tot) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
      # 
      # split_current_cases_with_PMC_asym <- split(current_cases_with_PMC_asym, ceiling(seq_along(current_cases_with_PMC_asym)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
      # names(split_current_cases_with_PMC_asym) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
      # 
      
      # split data into each 6 month age group for each infection class (no PMC) (per 1000 children in that age group)
      split_current_cases_no_PMC_clin_per1000 <- split(current_cases_no_PMC_clin_per1000,  c(rep(1, times=26),rep(2, times=26), rep(3, times=26), rep(4, times=26), rep(5, times=27)))
      names(split_current_cases_no_PMC_clin_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
      
      split_current_cases_no_PMC_sev_per1000 <- split(current_cases_no_PMC_sev_per1000,  c(rep(1, times=26),rep(2, times=26), rep(3, times=26), rep(4, times=26), rep(5, times=27)))
      names(split_current_cases_no_PMC_sev_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
      
      # split_current_cases_no_PMC_tot_per1000 <- split(current_cases_no_PMC_tot_per1000, ceiling(seq_along(current_cases_no_PMC_tot_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
      # names(split_current_cases_no_PMC_tot_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
      # 
      # split_current_cases_no_PMC_asym_per1000 <- split(current_cases_no_PMC_asym_per1000, ceiling(seq_along(current_cases_no_PMC_asym_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
      # names(split_current_cases_no_PMC_asym_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
      # 
      
      # split data into each 6 month age group for each infection class (with PMC) (per 1000 children in that age group)
      split_current_cases_with_PMC_clin_per1000 <- split(current_cases_with_PMC_clin_per1000,  c(rep(1, times=26),rep(2, times=26), rep(3, times=26), rep(4, times=26), rep(5, times=27)))
      names(split_current_cases_with_PMC_clin_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
      
      split_current_cases_with_PMC_sev_per1000 <- split(current_cases_with_PMC_sev_per1000,  c(rep(1, times=26),rep(2, times=26), rep(3, times=26), rep(4, times=26), rep(5, times=27)))
      names(split_current_cases_with_PMC_sev_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
      
      # split_current_cases_with_PMC_tot_per1000 <- split(current_cases_with_PMC_tot_per1000, ceiling(seq_along(current_cases_with_PMC_tot_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
      # names(split_current_cases_with_PMC_tot_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
      # 
      # split_current_cases_with_PMC_asym_per1000 <- split(current_cases_with_PMC_asym_per1000, ceiling(seq_along(current_cases_with_PMC_asym_per1000)/(max(age_max)/length(sixmonth_intervals_midpoint[1:no_sixmonth_intervals]))))
      # names(split_current_cases_with_PMC_asym_per1000) <- age_group_names_sixmonth[1:no_sixmonth_intervals]
      # 
      
      
      for (j in 1:length(sixmonth_intervals[1:no_sixmonth_intervals])) {
        
        # sum of cases (no PMC) for each 6 month age group
        cases_no_PMC_clin<- c(cases_no_PMC_clin, sum(split_current_cases_no_PMC_clin[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
        cases_no_PMC_sev<- c(cases_no_PMC_sev, sum(split_current_cases_no_PMC_sev[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
        #cases_no_PMC_tot<- c(cases_no_PMC_tot, sum(split_current_cases_no_PMC_tot[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
        #cases_no_PMC_asym<- c(cases_no_PMC_asym, sum(split_current_cases_no_PMC_asym[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
        
        # sum of cases (with PMC) for each 6 month age group
        cases_with_PMC_clin<- c(cases_with_PMC_clin, sum(split_current_cases_with_PMC_clin[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
        cases_with_PMC_sev<- c(cases_with_PMC_sev, sum(split_current_cases_with_PMC_sev[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
        #cases_with_PMC_tot<- c(cases_with_PMC_tot, sum(split_current_cases_with_PMC_tot[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
        #cases_with_PMC_asym<- c(cases_with_PMC_asym, sum(split_current_cases_with_PMC_asym[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
        
        # sum of cases (no PMC) for each 6 month age group (per 1000 children in that age group)
        cases_no_PMC_clin_per1000<- c(cases_no_PMC_clin_per1000, mean(split_current_cases_no_PMC_clin_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
        cases_no_PMC_sev_per1000<- c(cases_no_PMC_sev_per1000, mean(split_current_cases_no_PMC_sev_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
        #cases_no_PMC_tot_per1000<- c(cases_no_PMC_tot_per1000, mean(split_current_cases_no_PMC_tot_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
        #cases_no_PMC_asym_per1000<- c(cases_no_PMC_asym_per1000, mean(split_current_cases_no_PMC_asym_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
        
        # sum of cases (with PMC) for each 6 month age group (per 1000 children in that age group)
        cases_with_PMC_clin_per1000<- c(cases_with_PMC_clin_per1000, mean(split_current_cases_with_PMC_clin_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
        cases_with_PMC_sev_per1000<- c(cases_with_PMC_sev_per1000, mean(split_current_cases_with_PMC_sev_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
        #cases_with_PMC_tot_per1000<- c(cases_with_PMC_tot_per1000, mean(split_current_cases_with_PMC_tot_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
        #cases_with_PMC_asym_per1000<- c(cases_with_PMC_asym_per1000, mean(split_current_cases_with_PMC_asym_per1000[[age_group_names_sixmonth[1:no_sixmonth_intervals][j]]]))
        
      }
      
    }  
    
    
    
    # cases averted (with PMC) by infection class (per 1000 children in that age group)
    cases_averted_with_PMC_clin <- (cases_no_PMC_clin - cases_with_PMC_clin)
    cases_averted_with_PMC_sev <- (cases_no_PMC_sev - cases_with_PMC_sev)
    # cases_averted_with_PMC_tot <- (cases_no_PMC_tot - cases_with_PMC_tot)
    # cases_averted_with_PMC_asym <- (cases_no_PMC_asym - cases_with_PMC_asym)
    # 
    # cases averted (with PMC) by infection class (per 1000 children in that age group)
    cases_averted_with_PMC_clin_per1000 <- (cases_no_PMC_clin_per1000 - cases_with_PMC_clin_per1000)
    cases_averted_with_PMC_sev_per1000 <- (cases_no_PMC_sev_per1000 - cases_with_PMC_sev_per1000)
    # cases_averted_with_PMC_tot_per1000 <- (cases_no_PMC_tot_per1000 - cases_with_PMC_tot_per1000)
    # cases_averted_with_PMC_asym_per1000 <- (cases_no_PMC_asym_per1000 - cases_with_PMC_asym_per1000)
    # 
    # reduction in cases (with PMC) by infection class
    cases_reduction_with_PMC_clin <- (cases_no_PMC_clin - cases_with_PMC_clin)/cases_no_PMC_clin * 100
    cases_reduction_with_PMC_sev <- (cases_no_PMC_sev - cases_with_PMC_sev)/cases_no_PMC_sev * 100
    # cases_reduction_with_PMC_tot <- (cases_no_PMC_tot - cases_with_PMC_tot)/cases_no_PMC_tot * 100
    # cases_reduction_with_PMC_asym <- (cases_no_PMC_asym - cases_with_PMC_asym)/cases_no_PMC_asym * 100
    # 
    
    ##### APPEND VECTORS TO CALCULATION DATAFRAMES (6 MONTH AGE GROUP INTERVALS) #####
    
    annual_results_sixmonth$clinical_cases_no_PMC <- cases_no_PMC_clin
    annual_results_sixmonth$severe_cases_no_PMC <- cases_no_PMC_sev
    # annual_results_sixmonth$total_cases_no_PMC <- cases_no_PMC_tot
    # annual_results_sixmonth$asymptomatic_cases_no_PMC<- cases_no_PMC_asym
    # 
    
    annual_results_sixmonth$clinical_cases_no_PMC_per1000 <- cases_no_PMC_clin_per1000
    annual_results_sixmonth$severe_cases_no_PMC_per1000 <- cases_no_PMC_sev_per1000
    # annual_results_sixmonth$total_cases_no_PMC_per1000 <- cases_no_PMC_tot_per1000
    # annual_results_sixmonth$asymptomatic_cases_no_PMC_per1000<- cases_no_PMC_asym_per1000
    # 
    annual_results_sixmonth$clinical_cases_with_PMC<- cases_with_PMC_clin
    annual_results_sixmonth$severe_cases_with_PMC<- cases_with_PMC_sev
    # annual_results_sixmonth$total_cases_with_PMC<- cases_with_PMC_tot
    # annual_results_sixmonth$asymptomatic_cases_with_PMC<- cases_with_PMC_asym
    # 
    annual_results_sixmonth$clinical_cases_with_PMC_per1000<- cases_with_PMC_clin_per1000
    annual_results_sixmonth$severe_cases_with_PMC_per1000<- cases_with_PMC_sev_per1000
    # annual_results_sixmonth$total_cases_with_PMC_per1000<- cases_with_PMC_tot_per1000
    # annual_results_sixmonth$asymptomatic_cases_with_PMC_per1000<- cases_with_PMC_asym_per1000
    # 
    annual_results_sixmonth$clinical_cases_averted_with_PMC <- cases_averted_with_PMC_clin
    annual_results_sixmonth$severe_cases_averted_with_PMC <- cases_averted_with_PMC_sev
    # annual_results_sixmonth$total_cases_averted_with_PMC <- cases_averted_with_PMC_tot
    # annual_results_sixmonth$asymptomatic_cases_averted_with_PMC <- cases_averted_with_PMC_asym
    # 
    annual_results_sixmonth$clinical_cases_averted_with_PMC_per1000 <- cases_averted_with_PMC_clin_per1000
    annual_results_sixmonth$severe_cases_averted_with_PMC_per1000 <- cases_averted_with_PMC_sev_per1000
    # annual_results_sixmonth$total_cases_averted_with_PMC_per1000 <- cases_averted_with_PMC_tot_per1000
    # annual_results_sixmonth$asymptomatic_cases_averted_with_PMC_per1000 <- cases_averted_with_PMC_asym_per1000
    # 
    annual_results_sixmonth$clinical_cases_reduction <- cases_reduction_with_PMC_clin
    annual_results_sixmonth$severe_cases_reduction <- cases_reduction_with_PMC_sev
    # annual_results_sixmonth$total_cases_reduction <- cases_reduction_with_PMC_tot
    # annual_results_sixmonth$asymptomatic_cases_reduction <- cases_reduction_with_PMC_asym
    # 
    
    
    

    
    
  }
  
  
  
  
  
  
}



  
 

#incProgress(1/10)






##### SHAPEFILES FOR MAP MAKING #####

# shape file for country 
adm1<- sf::st_read(paste0("", get_iso3(country), "/shp/gadm41_", get_iso3(country), "_1.shp"))
adm1$`Admin-1 unit` <- stri_trans_general(str=gsub("-", "_", adm1$NAME_1), id = "Latin-ASCII")


schedule_temp <- c(0)

df_drug_protect<- data.frame(time=1:70)

# initalise empty columns
df_drug_protect$prot_trip<-NA
df_drug_protect$prot_quadr<-NA
df_drug_protect$prot_quint<-NA
df_drug_protect$prot_sext<-NA
df_drug_protect$prot_other<-NA
df_drug_protect$prot_VAGKAA<-NA
df_drug_protect$prot_VAGKGS<-NA

for (t in 1:70) {  # day 0 to dose 1 on day 70 
  df_drug_protect$prot_trip[t]<- exp(-(df_drug_protect$time[t-schedule_temp[length(schedule_temp)]]/lambda_trip)^w_trip)
  df_drug_protect$prot_quadr[t]<- exp(-(df_drug_protect$time[t-schedule_temp[length(schedule_temp)]]/lambda_quadr)^w_quadr)
  df_drug_protect$prot_quint[t]<- exp(-(df_drug_protect$time[t-schedule_temp[length(schedule_temp)]]/lambda_quint)^w_quint)
  df_drug_protect$prot_sext[t]<- exp(-(df_drug_protect$time[t-schedule_temp[length(schedule_temp)]]/lambda_sext)^w_sext)
  df_drug_protect$prot_other[t]<- exp(-(df_drug_protect$time[t-schedule_temp[length(schedule_temp)]]/lambda_other)^w_other)
  df_drug_protect$prot_VAGKAA[t]<- exp(-(df_drug_protect$time[t-schedule_temp[length(schedule_temp)]]/lambda_VAGKAA)^w_VAGKAA)
  df_drug_protect$prot_VAGKGS[t]<- exp(-(df_drug_protect$time[t-schedule_temp[length(schedule_temp)]]/lambda_VAGKGS)^w_VAGKGS)
}


for (i in 1:length(area_names)){
  proportions <- haplotype_data_final %>% filter(Country == country, `Admin-1 unit` == area_names[i])
  
  if (dim(proportions)[1] != 0) {
    
    df_drug_protect[paste0("prot_overall_", area_names[i])] <- as.double(proportions$I_AKA_)*df_drug_protect$prot_trip +
      as.double(proportions$I_GKA_)*df_drug_protect$prot_quadr +
      as.double(proportions$I_GEA_)*df_drug_protect$prot_quint +
      as.double(proportions$I_GEG_)*df_drug_protect$prot_sext +
      as.double(proportions$V_GKA_)*df_drug_protect$prot_VAGKAA +
      as.double(proportions$V_GKG_)*df_drug_protect$prot_VAGKGS +
      as.double(proportions$Other)*df_drug_protect$prot_other
    
  }
  
}

incProgress(1/10)

merged_df_by_age_COMPLETE <- results_by_age
merged_df_annual_COMPLETE <- annual_results
merged_df_annual_sixmonths_COMPLETE <- annual_results_sixmonth

endtime <- Sys.time()



data_protective_efficacy <- data.frame(
  Country = character(),
  `Admin-1 unit` = character(),
  day_30_protect_efficacy = double(),
  stringsAsFactors = FALSE
)
# weibull scale parameters for each haplotype
lambda_trip<-59.57659
lambda_quadr<-33.05391
lambda_quint<-18.55328
lambda_sext<-12.81186
lambda_other<-23
lambda_VAGKAA<-22 # assumed to be 20.1 days protection
lambda_VAGKGS<-18.55328 # assumed to be same as the QUINT

# weibull shape parameters for each haplotype
w_trip<- 8.435971
w_quadr<-4.862126
w_quint<-2.4840752
w_sext<-3.691953
w_other<-4.5
w_VAGKAA<-4.5
w_VAGKGS<- 2.4840752


for (i in 1:length(area_names)) {
  
  haplotypes_admin1 <- haplotype_data_final %>% filter(Country == country, `Admin-1 unit` == area_names[i])
  
  print(area_names[i])
  
  # set up df with the timesteps
  
  time<- seq(from=0,to=30,by=1) 
  
  p_protect_trip<- exp(-(time/lambda_trip)^w_trip)
  p_protect_quadr<- exp(-(time/lambda_quadr)^w_quadr)
  p_protect_quint<- exp(-(time/lambda_quint)^w_quint)
  p_protect_sext<- exp(-(time/lambda_sext)^w_sext)
  p_protect_w_VAGKAA<- exp(-(time/lambda_VAGKAA)^w_VAGKAA)
  p_protect_w_VAGKGS<- exp(-(time/lambda_VAGKGS)^w_VAGKGS)
  p_protect_other<- exp(-(time/lambda_other)^w_other)
  
  
  mean_trip<-mean(p_protect_trip)
  mean_quadr<-mean(p_protect_quadr)
  mean_quint<-mean(p_protect_quint)
  mean_sext<-mean(p_protect_sext)
  mean_w_VAGKAA<-mean(p_protect_w_VAGKAA)
  mean_w_VAGKGS<-mean(p_protect_w_VAGKGS)
  mean_other<-mean(p_protect_other)
  
  
  protective_efficacy<- mean_trip*as.numeric(haplotypes_admin1[,"I_AKA_"][[1]]) +
    mean_quadr* as.numeric(haplotypes_admin1[,"I_GKA_"][[1]])  +
    mean_quint* as.numeric(haplotypes_admin1[,"I_GEA_"][[1]]) +
    mean_sext* as.numeric(haplotypes_admin1[,"I_GEG_"][[1]]) +
    mean_w_VAGKAA* as.numeric(haplotypes_admin1[,"V_GKA_"][[1]]) +
    mean_w_VAGKGS* as.numeric(haplotypes_admin1[,"V_GKG_"][[1]]) +
    mean_other*as.numeric(haplotypes_admin1[,"Other"][[1]])
  
  
  
  data_protective_efficacy <- rbind(data_protective_efficacy,
                                    data.frame(Country = country,
                                              `Admin-1 unit` = area_names[i],
                                               day_30_protect_efficacy = protective_efficacy))
  
}

