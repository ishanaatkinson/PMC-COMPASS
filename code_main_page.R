
starttime<-Sys.time()


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


##### COMMON PARAMETERS/VARIABLES #####
# 
# human_population <- 1e5 # population size in model
# min_age_to_model <- 0 # minimum age to model (in days)
# max_age_to_model <- 2.5*365 # enter in multiples of 0.5, maximum age to model (in days)
# 
# # set the time span over which to simulate
# # NOTE: currently 23 years is the max as we only have ITN/IRS data for this length
# year <- 365; years <- 1; sim_length_for_data <- year * years
# 
# years_proj_forward <- 1 # number of years to project forward following known data
# years_of_simulation <- years + years_proj_forward # simulation length (years)
# sim_length <- (years + years_proj_forward) * year # simulation length (days)
# 
# # interval between modeled age groups (1 = days, 7 = weeks etc)
# step_length <- 1
# 
# # vector of min and max ages to be used in each age bracket
# age_min <- seq(min_age_to_model, max_age_to_model, step_length)
# age_max <- seq(min_age_to_model, max_age_to_model, step_length) + step_length
# 
# # six month ages (in days)
# sixmonth_intervals <- c(0, 183, 365, 548, 730, 913, 1095, 1278, 1460, 1643,
#                         1825, 2008, 2190, 2373, 2555, 2738, 2920, 3285, 3468,
#                         3650, 4015)
# 
# # time steps used in model (number of rows in the simulations output data frame)
# timesteps<-seq(0,round(sim_length),1)
# 
# # vector for the midpoint age for each age bracket (useful for graphing)
# age_in_days <- seq(age_min[1], age_max[length(age_max)], length.out=length(age_min) + 1)
# age_in_days_midpoint <- age_in_days[-length(age_in_days)] + diff(age_in_days)/2
# 
# # number of 6 month interval age groups to model
# no_sixmonth_intervals <- round(max(age_max)/182.5)
# 
# # empty vectors for the column names of interest
# age_group_names <- c() # age groups
# age_group_names_sixmonth <- c() # 6 month age groups
# sixmonth_intervals_midpoint <- c() # 6 month age group midpoints
# clin_inc_cols <- c() # clinical incidence column names
# sev_inc_cols <- c() # severe incidence column names
# tot_inc_cols <- c() # total incidence column names
# asym_inc_cols <- c() # asymptomatic incidence column names
# 
# # fill empty vectors
# for (i in 1:(length(age_min))) {
#   age_group_names <- append(age_group_names, paste0("n_age_", as.character(age_min[i]), "_", as.character(age_max[i])))
# }
# 
# for (i in 1:(length(sixmonth_intervals) - 1)) {
#   
#   if (i == (length(sixmonth_intervals) - 1)) {
#     age_group_names_sixmonth <- append(age_group_names_sixmonth, paste0("n_age_", as.character(sixmonth_intervals[i]), "_", as.character(sixmonth_intervals[i+1])))
#     sixmonth_intervals_midpoint <- append(sixmonth_intervals_midpoint, (sixmonth_intervals[i]+sixmonth_intervals[i+1])/2)
#   }
#   
#   if (i != (length(sixmonth_intervals) - 1)) {
#     age_group_names_sixmonth <- append(age_group_names_sixmonth, paste0("n_age_", as.character(sixmonth_intervals[i]), "_", as.character(sixmonth_intervals[i+1] - 1)))
#     sixmonth_intervals_midpoint <- append(sixmonth_intervals_midpoint, (sixmonth_intervals[i]+(sixmonth_intervals[i+1] - 1))/2)
#   }
#   
# }
# 
# # write column names
# for (i in 1:length(age_min)) {
#   clin_inc_cols <- append(clin_inc_cols, paste0("n_inc_clinical_", as.character(age_min[i]), "_", as.character(age_max[i])))
#   sev_inc_cols <- append(sev_inc_cols, paste0("n_inc_severe_", as.character(age_min[i]), "_", as.character(age_max[i])))
#   tot_inc_cols <- append(tot_inc_cols, paste0("n_inc_", as.character(age_min[i]), "_", as.character(age_max[i])))
#   asym_inc_cols <- append(asym_inc_cols, paste0("n_inc_asym_", as.character(age_min[i]), "_", as.character(age_max[i])))
# }

##### READ IN IMPERIAL MODEL OUTPUT #####

file_names <- list.files(paste0("", get_iso3(country_main_page), "/sim_results_DHS_coverage_levels"))

file_names <- file_names[
  !file_names %in% c(
    "population_df_merged.csv",
    "population_df_rural.csv"
  )
]


file_names_no_ext <- gsub("\\.(csv|xlsx)$", "", file_names)

incProgress(1/10)

for (i in 1:length(file_names)) {
  
  ext <- tolower(file_ext(file_names[i]))
  
  if (ext == "csv") {
    
    assign(file_names_no_ext[i], vroom::vroom(paste0("", get_iso3(country_main_page), "/sim_results_DHS_coverage_levels/", file_names[i])))
    
    
  }  
  
  if (ext == "xlsx") {
    
    assign(file_names_no_ext[i], readxl::read_xlsx(paste0("", get_iso3(country_main_page), "/sim_results_DHS_coverage_levels/", file_names[i])))
    
    
  } 
  
  if (ext == "png") {
    next
  }
    
}


incProgress(1/10)

##### SHAPEFILES FOR MAP MAKING #####


# shape file for country 
adm1<- sf::st_read(paste0("", get_iso3(country_main_page), paste0("/shp/gadm41_", get_iso3(country_main_page), "_1.shp")))
adm1$NAME_2 <- stri_trans_general(str=gsub("-", "_", adm1$NAME_1), id = "Latin-ASCII")
adm1$NAME_2 <- stri_trans_general(str=gsub(" ", "_", adm1$NAME_2), id = "Latin-ASCII")

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


admin1_with_haplotype_data <- c()



for (i in 1:length(area_names)){
  proportions <- haplotype_data_final %>% filter(`Country` == country_main_page, `Admin-1 unit` == area_names[i])
  
  if (dim(proportions)[1] != 0) {
    
    admin1_with_haplotype_data <- c(admin1_with_haplotype_data, area_names[i])
    
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


endtime<-Sys.time()

