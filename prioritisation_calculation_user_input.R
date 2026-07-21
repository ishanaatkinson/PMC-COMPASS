##### LOAD PACKAGES ##### 


population_df <- vroom::vroom(paste0("", get_iso3(country), "/sim_results_DHS_coverage_levels/population_df_merged_shortened.csv"))
  

##### 7. COUNT NUMBER OF PMC DOSES #####

number_of_doses <- length(schedule)
country_code <- get_iso3(input$country)

#population_df_age_structure

##### 7. COUNT NUMBER OF PMC DOSES #####


PMC_doses_all <- tibble()


  
for (k in seq_along(number_of_doses)) {
  
  nd <- number_of_doses[k]

  for (i in seq_along(area_names)) {
    
    area <- area_names[i]
    
    for (j in seq_along(schedule)) {
      
      if (schedule[j] < max(age_in_days_midpoint)) {
        
        age_bins <- c(0, 7, 14, 21, 28, 35, 42, 49, 56, 63, 70, 77, 84, 91, 98, 105, 112)
        age_cols <- grep("^n_age_", names(population_df_age_structure)[4:134], value = TRUE)

        get_age_column <- function(age_value, cols) {
          cols[findInterval(age_value, age_bins)]
        }
        
        col <- get_age_column(schedule[j], age_cols)

        pop <- mean((population_df %>% filter(`Admin-1 unit` == area))[[col]]) / 7

        pop_real <- (pop / human_population) *
          sum((all_sites_for_DT %>% filter(Country==input$country, `Admin-1 unit` == area))$population_total)
        
        
        PMC_doses <- pop_real * cov[j] * 365 # annually

        if (schedule[j] > 365) {
          PMC_doses <- PMC_doses
        }
        
        age_band <- dplyr::case_when(
          schedule[j] < 365 ~ "0-1yr",
          schedule[j] < 730 ~ "1-2yr",
          schedule[j] < 1095 ~ "2-3yr",
          TRUE ~ NA_character_
        )
        
        PMC_doses_all <- bind_rows(
          PMC_doses_all,
          tibble(
            `Iso code` = country_code,
            `N doses` = nd,
            `Admin-1 unit` = area,
            scenario = paste0("User selected ", nd, " dose delivery strategy"),
            age = age_band,
            merged = PMC_doses
          )
        )
      }
    }
  }
}






coverage_data_full <- read.csv("files_needed/final_coverage_lookup_data.csv")
cost_data_full <- read.csv("files_needed/PMC_cost_data.csv")
health_facility_data_full <- readxl::read_xlsx("files_needed/AFRO_health_facility_data_clean_v3.xlsx")
# cost_and_incidence_data_full <- readxl::read_xlsx("files_needed/cost_and_incidence_data.xlsx")
cet_thresholds <- readxl::read_xlsx("files_needed/cost_effectiveness_thresholds.xlsx")
districts_data_full <- readxl::read_xlsx("files_needed/africa_admin2_per_admin1_gadm.xlsx")



DALY_uncomp <- 0.051
DALY_sev <- 0.133
DALY_death <- 1

# Death rate as a proportion of severe cases (can change)
death_rate <- 0.215

country_code <- get_iso3(input$country)


economic_summary_df <- tibble::tibble()


#for (i in 1:length(countries)) {
  
  
  #print(countries[i])
  

  ##### 1. EPI VACCINE COVERAGE DATA #####
  
  # for the default vs comparison coverage levels 
  #coverage_data <- read_xlsx("plusproject_countries_coverage_dhs.xlsx", sheet = "focus_countries")
  
  coverage_data <- cov
  
  ##### 2. READ IN IMPERIAL MODEL OUTPUT #####
  # 
  # incidence_ppy_df <- vroom::vroom(paste0("C:/Users/ishan/OneDrive - London School of Hygiene and Tropical Medicine/Documents/R files/RUN_MODEL/", country_code, "/sim_results_DHS_coverage_levels/incidence_ppy_df_merged.csv"))
  # PMC_impact_ppy <- vroom::vroom(paste0("C:/Users/ishan/OneDrive - London School of Hygiene and Tropical Medicine/Documents/R files/RUN_MODEL/", country_code, "/sim_results_DHS_coverage_levels/PMC_impact_ppy_merged_FINAL.csv"))
  # 
  # 
  # incidence_ppy_df$`Admin-1 unit` <- stri_trans_general(str=gsub("-", "_", incidence_ppy_df$`Admin-1 unit`), id = "Latin-ASCII") 
  # incidence_ppy_df$`Admin-1 unit`  <- stri_trans_general(str=gsub(" ", "_", incidence_ppy_df$`Admin-1 unit` ), id = "Latin-ASCII")
  # PMC_impact_ppy$`Admin-1 unit` <- stri_trans_general(str=gsub("-", "_", PMC_impact_ppy$`Admin-1 unit`), id = "Latin-ASCII") 
  # PMC_impact_ppy$`Admin-1 unit`  <- stri_trans_general(str=gsub(" ", "_", PMC_impact_ppy$`Admin-1 unit` ), id = "Latin-ASCII")
  # 
  # 
  
  annual_data <- merged_df_annual_COMPLETE
  

  no_PMC <- annual_data %>% dplyr::select(`Country`, `Age group`, `Admin-1 unit`,  `N doses`, clinical_cases_no_PMC, severe_cases_no_PMC)
  
  with_PMC <- annual_data %>% dplyr::select(`Country`, `Age group`, `Admin-1 unit`, `N doses`, clinical_cases_with_PMC, severe_cases_with_PMC)
  
  PMC_doses_given <- PMC_doses_all
  
  subset_cost_and_inc_table <- (cost_data_full %>% filter(iso3c == country_code))
  
  
  
  # ---- FINAL OUTPUT DATAFRAME ----
  
  for (j in seq_along(area_names)) {
    
    current_area <- area_names[j]
    source_of_cost_estimates <- "Plus Project countries"
    
    # ---- Countries with Plus Project ----
    pp_countries <- c("CMR", "BEN", "CIV", "MOZ")
    
    # ---- Implementation regions by country ----
    pp_regions <- list(
      CMR = c("Centre"),
      BEN = c("Borgou", "Kouffo", "Zou"),
      CIV = c("Comoe", "Sassandra_Marahoue", "Woroba"),
      MOZ = c("Sofala")
    )
    
    # ---- Country-level override ----
    if (country_code %in% pp_countries) {
      source_of_cost_estimates <- "Other regions in this country"
    }
    
    # ---- Region-level override ----
    if (
      country_code %in% names(pp_regions) &&
      area_names[j] %in% pp_regions[[country_code]]
    ) {
      source_of_cost_estimates <- "This region"
    }
    
    
    
    scenario <- unique(PMC_doses_given$scenario[PMC_doses_given$`Admin-1 unit` == current_area])[1]
    
    number_of_doses <- unique(PMC_doses_given$`N doses`[PMC_doses_given$`Admin-1 unit` == current_area])[1]
    
    # ---- CASES ----
    clinical_cases_no_PMC <- round(as.double(
      sum((no_PMC %>% filter(`Admin-1 unit` == current_area))$clinical_cases_no_PMC, na.rm = TRUE)
    ),0)
    
    clinical_cases_no_PMC_sought_care <- round(clinical_cases_no_PMC * 0.5,0)
    
    severe_cases_no_PMC <- round(as.double(
      sum((no_PMC %>% filter(`Admin-1 unit` == current_area))$severe_cases_no_PMC, na.rm = TRUE)
    ),0)
    
    clinical_cases_with_PMC <- round(as.double(
      sum((with_PMC %>% filter(`Admin-1 unit` == current_area))$clinical_cases_with_PMC, na.rm = TRUE)
    ),0)
    
    clinical_cases_with_PMC_sought_care <- round(clinical_cases_with_PMC * 0.5,0)
    
    
    severe_cases_with_PMC <- round(as.double(
      sum((with_PMC %>% filter(`Admin-1 unit` == current_area))$severe_cases_with_PMC, na.rm = TRUE)
    ),0)
    
    
    # ---- NUMBER OF SP TABLETS DELIVERED ----
    n_tablets <- round(as.double(
      sum(c(
        (PMC_doses_given %>% filter(`Admin-1 unit` == current_area, age == "0-1yr"))$merged,
        (PMC_doses_given %>% filter(`Admin-1 unit` == current_area, age == "1-2yr"))$merged * 2,
        (PMC_doses_given %>% filter(`Admin-1 unit` == current_area, age == "2-3yr"))$merged * 2
        
      ), na.rm = TRUE)
    ),0)
    
    
    # ---- NUMBER OF SP TABLETS DELIVERED ----
    total_n_pmc_doses_delivered <- round(as.double(
      sum(c(
        (PMC_doses_given %>% filter(`Admin-1 unit` == current_area, age == "0-1yr"))$merged,
        (PMC_doses_given %>% filter(`Admin-1 unit` == current_area, age == "1-2yr"))$merged,
        (PMC_doses_given %>% filter(`Admin-1 unit` == current_area, age == "2-3yr"))$merged
        
      ), na.rm = TRUE)
    ),0)
    
    n_pmc_doses_delivered_0_1 = round(sum((PMC_doses_given %>% filter(`Admin-1 unit` == current_area, age == "0-1yr"))$merged),0)
    n_pmc_doses_delivered_1_2 = round(sum((PMC_doses_given %>% filter(`Admin-1 unit` == current_area, age == "1-2yr"))$merged),0)
    n_pmc_doses_delivered_2_25 =  round(sum((PMC_doses_given %>% filter(`Admin-1 unit` == current_area, age == "2-3yr"))$merged),0)
    
    if(length(n_pmc_doses_delivered_0_1) == 0) {n_pmc_doses_delivered_0_1 = 0}
    if(length(n_pmc_doses_delivered_1_2) == 0) {n_pmc_doses_delivered_1_2 = 0}
    if(length(n_pmc_doses_delivered_2_25) == 0) {n_pmc_doses_delivered_2_25 = 0}
    
    # ---- COST PER TABLET ----
    cost_per_tablet <- round(as.double((cost_data_full %>% filter(iso3c == country_code))$cost_per_tablet),2)
    
    # ---- COST PER CONSUMABLES ----
    cost_per_consumables <- round(as.double((cost_data_full %>% filter(iso3c == country_code))$cost_per_consumables),2)
    
    # cost_per_tablet_used_country_average <- ifelse(nrow(dose_cost_data) == 0, "yes", "no")
    # 
    # cost_per_tablet <- as.double(
    #   ifelse(
    #     nrow(dose_cost_data) == 0,
    #     (cost_data_full %>% filter(iso3c == "COUNTRY_AVERAGE", level == "doses"))$total_cost_per_unit,
    #     dose_cost_data$total_cost_per_unit
    #   )
    # )
    
    cost_sp_with_pmc <- round(as.double(cost_per_tablet * n_tablets) + as.double(cost_per_consumables * total_n_pmc_doses_delivered),0)
    
    
    # ---- NUMBER OF health facilities ----
    
    health_facility_data <- health_facility_data_full %>%
      filter(country == country_code, admin1_check == "yes", name_2 == current_area)
    
    facilities_used_country_average <- ifelse(nrow(health_facility_data) == 0, "yes", "no")
    
    n_facilities <- as.double(
      ifelse(
        nrow(health_facility_data) == 0,
        (health_facility_data_full %>% filter(country == "ADMIN_AVERAGE", admin1_check == "yes"))$NATURE_count,
        health_facility_data$NATURE_count
      )
    )
    
    
    # ---- COST PER health facility ----
    cost_per_facility <- round(as.double((cost_data_full %>% filter(iso3c == country_code))$HF),0)
    
    # cost_per_facility_used_country_average <- ifelse(nrow(facility_cost_data) == 0, "yes", "no")
    # 
    # cost_per_facility <- as.double(
    #   ifelse(
    #     nrow(facility_cost_data) == 0,
    #     (cost_data_full %>% filter(iso3c == "COUNTRY_AVERAGE", level == "HF"))$total_cost_per_unit,
    #     facility_cost_data$total_cost_per_unit
    #   )
    # )
    
    # ---- NUMBER OF districts ----
    
    districts_data <- districts_data_full %>%
      filter(`Iso code` == country_code, `Admin-1 unit` == current_area)
    
    districts_data_used_country_average <- ifelse(nrow(districts_data) == 0, "yes", "no")
    
    n_districts <- as.double(
      ifelse(
        nrow(districts_data) == 0,
        mean(districts_data_full$n_admin2, na.rm=TRUE),
        districts_data$n_admin2
      )
    )
    
    
    # ---- COST PER district ----
    cost_per_district <- round(as.double((cost_data_full %>% filter(iso3c == country_code))$district),0)
    
    # cost_per_district_used_country_average <- ifelse(nrow(district_cost_data) == 0, "yes", "no")
    # 
    # cost_per_district <- as.double(
    #   ifelse(
    #     nrow(district_cost_data) == 0,
    #     (cost_data_full %>% filter(iso3c == "COUNTRY_AVERAGE", level == "district"))$total_cost_per_unit,
    #     district_cost_data$total_cost_per_unit
    #   )
    # )
    
    
    cost_pmc_per_area <- round(as.double(n_facilities * cost_per_facility) + as.double(n_districts * cost_per_district),0)
    
    # ---- COST PER country ----
    
    cost_per_country <- round(as.double((cost_data_full %>% filter(iso3c == country_code))$national),0)
    
    # cost_per_country_used_average <- ifelse(nrow(country_cost_data) == 0, "yes", "no")
    # 
    # cost_per_country <- as.double(
    #   ifelse(
    #     nrow(country_cost_data) == 0,
    #     (cost_data_full %>% filter(iso3c == "COUNTRY_AVERAGE", level == "national"))$total_cost_per_unit,
    #     country_cost_data$total_cost_per_unit
    #   )
    # )
    
    # ---- TREATMENT COSTS ----
    treatment_cost_data <- (cost_data_full %>% filter(iso3c == country_code))
    
    # cost_uncomplicated_used_country_average <- ifelse(nrow(treatment_cost_data) == 0, "yes", "no")
    # cost_severe_used_country_average <- ifelse(nrow(treatment_cost_data) == 0, "yes", "no")
    # 
    # cost_uncomplicated_case <- as.double(
    #   ifelse(
    #     nrow(treatment_cost_data) == 0,
    #     (cost_data_full %>% filter(iso3c == "COUNTRY_AVERAGE", level == "district"))$Econ_cost_to_providers_uncomplicated,
    #     treatment_cost_data$Econ_cost_to_providers_uncomplicated
    #   )
    # )
    # 
    # cost_severe_case <- as.double(
    #   ifelse(
    #     nrow(treatment_cost_data) == 0,
    #     (cost_data_full %>% filter(iso3c == "COUNTRY_AVERAGE", level == "district"))$Econ_cost_to_providers_hospitalisation,
    #     treatment_cost_data$Econ_cost_to_providers_hospitalisation
    #   )
    # )
    
    cost_uncomplicated_case <- round(as.double(treatment_cost_data$outpatient),2)
    
    cost_severe_case <- round(as.double(treatment_cost_data$inpatient),2)
    
    cost_death <- round(as.double(cost_severe_case),2)
    
    cost_treatment_no_pmc <- round(as.double(
      (clinical_cases_no_PMC_sought_care * cost_uncomplicated_case) +
        (severe_cases_no_PMC * cost_severe_case) + 
        ((severe_cases_no_PMC * death_rate) * cost_death) # cost per death 
    ),0)
    
    cost_treatment_with_pmc <- round(as.double(
      (clinical_cases_with_PMC_sought_care * cost_uncomplicated_case) +
        (severe_cases_with_PMC * cost_severe_case) + 
        ((severe_cases_with_PMC * death_rate) * cost_death) # cost per death 
    ),0)
    
    
    # ---- DALY estimates 
    
    # without PMC
    DALYs_no_PMC_uncomplicated <- round(as.double(
      (clinical_cases_no_PMC * DALY_uncomp)),0)
    
    DALYs_no_PMC_severe <- round(as.double(
      (severe_cases_no_PMC * DALY_sev)),0)
    
    DALYs_no_PMC_deaths <- round(as.double(
      (severe_cases_no_PMC * death_rate * DALY_death)),0)
    
    total_DALYs_no_PMC <- round(DALYs_no_PMC_uncomplicated + DALYs_no_PMC_severe + DALYs_no_PMC_deaths,0)
    
    # with PMC 
    DALYs_with_PMC_uncomplicated <- round(as.double(
      (clinical_cases_with_PMC * DALY_uncomp)),0)
    
    DALYs_with_PMC_severe <- round(as.double(
      (severe_cases_with_PMC * DALY_sev)),0)
    
    DALYs_with_PMC_deaths <- round(as.double(
      (severe_cases_with_PMC * death_rate * DALY_death)),0)
    
    total_DALYs_with_PMC <- round(DALYs_with_PMC_uncomplicated + DALYs_with_PMC_severe + DALYs_with_PMC_deaths,0)
    
    
    
    pmc_implementation_cost_incl_national_costs <- round(cost_per_country + as.double(n_facilities * cost_per_facility) + as.double(n_districts * cost_per_district) + as.double(cost_per_tablet * n_tablets),0)
    pmc_implementation_cost <- round(as.double(n_facilities * cost_per_facility) + as.double(n_districts * cost_per_district) + as.double(cost_per_tablet * n_tablets),0)
    
    
    
    # ---- TOTALS ----
    total_cost_no_pmc <- round(as.double(cost_treatment_no_pmc),0)
    
    total_cost_with_pmc <- round(as.double(
      pmc_implementation_cost + cost_treatment_with_pmc
    ),0)
    
    total_cost_pmc_implementation <- round(as.double(
      pmc_implementation_cost
    ),0)
    
    # Total population size in this region 
    
    total_population_all_ages <- round((all_sites_for_DT %>% filter(`Iso code` == country_code, `Admin-1 unit` == current_area))$population_total,0)
    
    
    
    # --------------------------------------------------
    # APPEND TWO ROWS PER AREA:
    #   1) No PMC
    #   2) PMC scenario
    # --------------------------------------------------
    
    # ---- ROW 1: NO PMC ----
    economic_summary_df <- bind_rows(
      economic_summary_df,
      data.frame(
        `Iso code` = country_code,
        `Region (admin-1 unit)` = current_area,
        `Population (all ages)` = total_population_all_ages, 
        
        `Scenario` = "No PMC",
        `N doses in PMC schedule` = 0,
        `Data source for cost modelling` = source_of_cost_estimates,
        
        `Total cost of PMC implementation (excl national costs)` = 0,
        `Sub-total: Cost of treatment` = cost_treatment_no_pmc,
        `Total cost of PMC implementation + treatment (excluding national costs)` = total_cost_no_pmc,
        `Total cost of PMC implementation + treatment (including full national costs for every region)` = total_cost_no_pmc,
        
        
        `Clinical cases (N, 0-30mo)` = clinical_cases_no_PMC,
        `Clinical cases for whom care sought` = clinical_cases_no_PMC_sought_care,
        `Hospitalisations (N, 0-30mo)` = severe_cases_no_PMC,
        `Deaths (N, 0-30mo)` = round(as.double(severe_cases_no_PMC * death_rate),0),
        
        `Total DALYs` = total_DALYs_no_PMC,
        `DALYs from clinical cases` = DALYs_no_PMC_uncomplicated,
        `DALYs from hospitalisations` = DALYs_no_PMC_severe,
        `DALYs from deaths` = DALYs_no_PMC_deaths,
        
        `PMC cost input: Implementation cost at national level` = 0,
        `PMC cost input: PMC implementation cost per district` = 0,
        `PMC cost input: PMC implementation cost per health facility` = 0,
        
        `PMC cost input: Cost of PMC administration consumables per dose` = 0,
        `PMC Cost input: Cost per SP tablet (includes wastage)` = 0,
        
        
        `N districts in region` = n_districts,
        `N health facilities in region` = n_facilities,
        
        
        `N expected PMC doses delivered (age 0-1yr)` = 0,
        `N expected PMC doses delivered (age 1-2yr)` = 0,
        `N expected PMC doses delivered (age 2-2.5yr)` = 0,
        `N total expected PMC doses delivered (age 0-2.5yr)` = 0,
        
        `N SP tablets delivered` = 0,
        `Sub-total: Cost of PMC implementation at district and HFs` = 0,
        `Sub-total: Cost of PMC administration (SP+consumables)` = 0,
        
        `Treatment cost per clinical case` = cost_uncomplicated_case,
        `Treatment cost per hospitalisation` = cost_severe_case,
        `Treatment cost per death` = cost_death, 
        
        
        stringsAsFactors = FALSE,
        check.names=FALSE
      )
    )
    
    # ---- ROW 2: WITH PMC ----
    economic_summary_df <- bind_rows(
      economic_summary_df,
      data.frame(
        `Iso code` = country_code,
        `Region (admin-1 unit)` = current_area,
        `Population (all ages)` = total_population_all_ages, 
        `Scenario` = scenario,
        `N doses in PMC schedule` = as.double(number_of_doses),
        `Data source for cost modelling` = source_of_cost_estimates,
        
        `Total cost of PMC implementation (excl national costs)` = total_cost_pmc_implementation,
        `Sub-total: Cost of treatment` = cost_treatment_with_pmc,
        `Total cost of PMC implementation + treatment (excluding national costs)` = total_cost_with_pmc,
        `Total cost of PMC implementation + treatment (including full national costs for every region)` = pmc_implementation_cost_incl_national_costs + cost_treatment_with_pmc,
        
        
        
        
        `Clinical cases (N, 0-30mo)` = clinical_cases_with_PMC,
        `Clinical cases for whom care sought` = clinical_cases_with_PMC_sought_care,
        
        `Hospitalisations (N, 0-30mo)` = severe_cases_with_PMC,
        `Deaths (N, 0-30mo)` = round(as.double(severe_cases_with_PMC * death_rate),0),
        
        `Total DALYs` = total_DALYs_with_PMC,
        `DALYs from clinical cases` = DALYs_with_PMC_uncomplicated,
        `DALYs from hospitalisations` = DALYs_with_PMC_severe,
        `DALYs from deaths` = DALYs_with_PMC_deaths,
        
        
        `PMC cost input: Implementation cost at national level` = cost_per_country,
        `PMC cost input: PMC implementation cost per district` = cost_per_district,
        `PMC cost input: PMC implementation cost per health facility` = cost_per_facility,
        
        `PMC cost input: Cost of PMC administration consumables per dose` = cost_per_consumables,
        `PMC Cost input: Cost per SP tablet (includes wastage)` = cost_per_tablet,
        
        
        `N districts in region` = n_districts,
        `N health facilities in region` = n_facilities,
        
        `N expected PMC doses delivered (age 0-1yr)` = n_pmc_doses_delivered_0_1,
        `N expected PMC doses delivered (age 1-2yr)` = n_pmc_doses_delivered_1_2,
        `N expected PMC doses delivered (age 2-2.5yr)` = n_pmc_doses_delivered_2_25,
        `N total expected PMC doses delivered (age 0-2.5yr)` = total_n_pmc_doses_delivered,
        
        `N SP tablets delivered` = n_tablets,
        `Sub-total: Cost of PMC implementation at district and HFs` = cost_pmc_per_area,
        `Sub-total: Cost of PMC administration (SP+consumables)` = cost_sp_with_pmc,
        
        `Treatment cost per clinical case` = cost_uncomplicated_case,
        `Treatment cost per hospitalisation` = cost_severe_case,
        `Treatment cost per death` = cost_death, 
        
        
        
        stringsAsFactors = FALSE,
        check.names=FALSE
        
      )
    )
    
    
  }
  


economic_summary_df <- unique(economic_summary_df)




ICER_comparison_table <- data.frame()

#for (i in 1:length(countries)) {
  
  #print(countries[i])
  
  #country_code <- get_iso3(input$country)
  
  ##### 1. EPI VACCINE COVERAGE DATA #####
  
  # for the default vs comparison coverage levels 
  
  coverage_data <- cov
  
  #area_names <- unique(full_data$eir$name_2)
  
  
  ##### 2. READ IN IMPERIAL MODEL OUTPUT #####

  annual_data <- merged_df_annual_COMPLETE
  

  no_PMC <- annual_data %>% dplyr::select(`Country`, `Age group`, `Admin-1 unit`,  `N doses`, clinical_cases_no_PMC, severe_cases_no_PMC)
  
  with_PMC <- annual_data %>% dplyr::select(`Country`, `Age group`, `Admin-1 unit`, `N doses`, clinical_cases_with_PMC, severe_cases_with_PMC)
  
  PMC_doses_given <- PMC_doses_all
  
  cost_and_incidence_data <- economic_summary_df
  
  scenarios <- unique(PMC_doses_given$scenario)
  
  # Explicitly add No PMC
  scenarios <- unique(c("No PMC", scenarios))
  
  # Create ALL ORDERED pairwise combinations (A vs B, B vs A)
  scenario_pairs <- expand.grid(
    scenario_1 = scenarios,
    scenario_2 = scenarios,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::filter(scenario_1 != scenario_2) %>%
    dplyr::mutate(
      scenario_comparison = paste0(scenario_1, " vs ", scenario_2)
    )
  
  # ------------------------------------------------------------------
  # FINAL OUTPUT TABLE
  # ------------------------------------------------------------------
  
  scenario_comparison_table <- data.frame()
  
  for (area in area_names) {
    
    area_df <- data.frame(
      country = country_code,
      area = area,
      scenario_comparison = scenario_pairs$scenario_comparison,
      stringsAsFactors = FALSE
    )
    
    scenario_comparison_table <- rbind(
      scenario_comparison_table,
      area_df
    )
  }
  
  
  
  # -------------------------------------------------------
  # FINAL ICER COMPARISON TABLE
  # -------------------------------------------------------
  
  scenario_comparisons <- unique(scenario_comparison_table$scenario_comparison)
  
  ICER_base_table <- cost_and_incidence_data %>%
    dplyr::select(
      `Iso code`,
      `Region (admin-1 unit)`,
      `Population (all ages)`,
      `Scenario`,
      `N doses in PMC schedule`,
      `Data source for cost modelling`,
      `Total cost of PMC implementation (excl national costs)`,
      `Sub-total: Cost of treatment`,
      `Total cost of PMC implementation + treatment (excluding national costs)`,
      `Total cost of PMC implementation + treatment (including full national costs for every region)`,
      `Clinical cases (N, 0-30mo)`,
      `Clinical cases for whom care sought`,
      `Hospitalisations (N, 0-30mo)`,
      `Deaths (N, 0-30mo)`,
      `Total DALYs`,
      `DALYs from clinical cases`,
      `DALYs from hospitalisations`,
      `DALYs from deaths`,
      `PMC cost input: Implementation cost at national level`,
      `PMC cost input: PMC implementation cost per district`,
      `PMC cost input: PMC implementation cost per health facility`,
      `PMC cost input: Cost of PMC administration consumables per dose`,
      `PMC Cost input: Cost per SP tablet (includes wastage)`,
      `N districts in region`,
      `N health facilities in region`,
      `N expected PMC doses delivered (age 0-1yr)`,
      `N expected PMC doses delivered (age 1-2yr)`,
      `N expected PMC doses delivered (age 2-2.5yr)`,
      `N total expected PMC doses delivered (age 0-2.5yr)`,
      `N SP tablets delivered`,
      `Sub-total: Cost of PMC implementation at district and HFs`,
      `Sub-total: Cost of PMC administration (SP+consumables)`,
      `Treatment cost per clinical case`,
      `Treatment cost per hospitalisation`,
      `Treatment cost per death`
    ) 
  
  
  # ---- CET values (can be replaced later) ----
  
  CET_lower_bound  <- (cet_thresholds %>% filter(Country == country_code))$Min_CET
  CET_upper_bound <- (cet_thresholds %>% filter(Country == country_code))$Max_CET
  
  
  
  scenario_comparisons <- unique(scenario_comparison_table$scenario_comparison)
  
  
  for (j in seq_along(area_names)) {
    
    current_area <- area_names[j]
    
    for (k in seq_along(scenario_comparisons)) {
      
      current_comparison <- scenario_comparisons[k]
      
      scen_pair <- strsplit(current_comparison, " vs ")[[1]]
      scen_A <- scen_pair[1]   # comparator (baseline)
      scen_B <- scen_pair[2]   # intervention
      
      row_A <- ICER_base_table[
        ICER_base_table$`Iso code` == country_code &
          ICER_base_table$`Region (admin-1 unit)` == current_area &
          ICER_base_table$Scenario == scen_A,
      ]
      
      row_B <- ICER_base_table[
        ICER_base_table$`Iso code` == country_code &
          ICER_base_table$`Region (admin-1 unit)` == current_area &
          ICER_base_table$Scenario == scen_B,
      ]
      
      if (nrow(row_A) == 0 || nrow(row_B) == 0) next
      
      
      # --------------------------------------------------
      # COST DIFFERENCES
      # --------------------------------------------------
      
      delta_cost_total <- as.double(
        row_B$`Total cost of PMC implementation + treatment (excluding national costs)` - row_A$`Total cost of PMC implementation + treatment (excluding national costs)`
      )
      
      delta_total_cost_incl_national_costs <- as.double(
        row_B$`Total cost of PMC implementation + treatment (including full national costs for every region)` - row_A$`Total cost of PMC implementation + treatment (including full national costs for every region)`
      )
      
      delta_cost_implementation <- as.double(
        (row_B$`Total cost of PMC implementation (excl national costs)`) -
          (row_A$`Total cost of PMC implementation (excl national costs)`)
      )
      
      delta_cost_treatment <- as.double(
        (row_B$`Sub-total: Cost of treatment`) -
          (row_A$`Sub-total: Cost of treatment`)
      )
      
      # National cost only applies to PMC scenarios
      national_cost_A <- ifelse(scen_A == "No PMC", 0, row_A$`PMC cost input: Implementation cost at national level`)
      national_cost_B <- ifelse(scen_B == "No PMC", 0, row_B$`PMC cost input: Implementation cost at national level`)
      
      delta_cost_implementation_incl_national_costs <- as.double(
        (row_B$`Total cost of PMC implementation (excl national costs)` + national_cost_B) -
          (row_A$`Total cost of PMC implementation (excl national costs)` + national_cost_A)
      )
      
      
      delta_cost_sp <- as.double(
        row_B$`Sub-total: Cost of PMC administration (SP+consumables)` -
          row_A$`Sub-total: Cost of PMC administration (SP+consumables)`
      )
      
      
      # --------------------------------------------------
      # HEALTH EFFECTS
      # --------------------------------------------------
      
      dalys_averted <- as.double(
        row_A$`Total DALYs` - row_B$`Total DALYs`
      )
      
      delta_clinical_cases <- as.double(
        row_A$`Clinical cases (N, 0-30mo)` - row_B$`Clinical cases (N, 0-30mo)`
      )
      
      delta_severe_cases <- as.double(
        row_A$`Hospitalisations (N, 0-30mo)` - row_B$`Hospitalisations (N, 0-30mo)`
      )
      
      delta_deaths <- as.double(
        row_A$`Deaths (N, 0-30mo)` - row_B$`Deaths (N, 0-30mo)`
      )
      
      
      # --------------------------------------------------
      # ICERS
      # --------------------------------------------------
      
      icer_value <- ifelse(
        dalys_averted == 0,
        NA,
        delta_cost_total / dalys_averted
      )
      
      icer_value_incl_national_costs <- ifelse(
        dalys_averted == 0,
        NA,
        delta_total_cost_incl_national_costs / dalys_averted
      )
      
      
      # --------------------------------------------------
      # CET FLAGS
      # --------------------------------------------------
      
      ICER_below_CET_upper_bound <- ifelse(
        !is.na(icer_value) & icer_value < CET_upper_bound,
        "yes", "no"
      )
      
      ICER_below_CET_lower_bound <- ifelse(
        !is.na(icer_value) & icer_value < CET_lower_bound,
        "yes", "no"
      )
      
      ICER_below_250 <- ifelse(
        !is.na(icer_value) & icer_value < 250,
        "yes", "no"
      )
      
      
      
      
      
      
      ICER_below_CET_upper_bound_incl_national_costs <- ifelse(
        !is.na(icer_value_incl_national_costs) &
          icer_value_incl_national_costs < CET_upper_bound,
        "yes", "no"
      )
      
      ICER_below_CET_lower_bound_incl_national_costs <- ifelse(
        !is.na(icer_value_incl_national_costs) &
          icer_value_incl_national_costs < CET_lower_bound,
        "yes", "no"
      )
      
      ICER_below_250_incl_national_costs <- ifelse(
        !is.na(icer_value_incl_national_costs) &
          icer_value_incl_national_costs < 250,
        "yes", "no"
      )
      
      n_pmc_doses_delivered_0_1 <- as.double(
        row_A$`N expected PMC doses delivered (age 0-1yr)` - row_B$`N expected PMC doses delivered (age 0-1yr)`
      )
      
      n_pmc_doses_delivered_1_2 <- as.double(
        row_A$`N expected PMC doses delivered (age 1-2yr)` - row_B$`N expected PMC doses delivered (age 1-2yr)`
      ) 
      
      n_pmc_doses_delivered_2_25 <- as.double(
        row_A$`N expected PMC doses delivered (age 2-2.5yr)` - row_B$`N expected PMC doses delivered (age 2-2.5yr)`
      )
      
      total_population_all_ages <- as.double(
        row_A$`Population (all ages)`
      )
      
      source_of_cost_estimates <- row_A$`Data source for cost modelling`
      
      n_doses <- row_A$`N doses in PMC schedule`
      
      # --------------------------------------------------
      # APPEND ROW
      # --------------------------------------------------
      
      ICER_comparison_table <- rbind(
        ICER_comparison_table,
        data.frame(
          `Country` = country_code,
          `Region (admin-1)` = current_area,
          `Population (all ages)` = total_population_all_ages,
          `PMC delivery strategy (vs. comparator)` = current_comparison,
          `N doses in PMC schedule` = n_doses,
          
          `Expected PMC doses delivered (age 0-1yr)` = n_pmc_doses_delivered_0_1, 
          `Expected PMC doses delivered (age 1-2yr)` = n_pmc_doses_delivered_1_2, 
          `Expected PMC doses delivered (age 2-2.5yr)` = n_pmc_doses_delivered_2_25, 
          
          
          `Cost-effectiveness threshold (lower)` = CET_lower_bound,
          `Cost-effectiveness threshold (upper)` = CET_upper_bound,
          
          `Additional clinical malaria cases averted` =
            -delta_clinical_cases,
          
          `Additional hospitalisations averted` =
            -delta_severe_cases,
          
          `Additional deaths averted` =
            -delta_deaths,
          
          `Additional DALYs averted` = -dalys_averted,
          
          
          `Sub-total: Cost of PMC administration (SP+consumables)` = -delta_cost_sp,
          
          `Difference in treatment costs` =
            -delta_cost_treatment,
          
          `Difference in implementation costs` =
            -delta_cost_implementation,
          
          `Difference in implementation costs (incl. national costs)` =
            -delta_cost_implementation_incl_national_costs,
          
          `Difference in total costs` =
            -delta_cost_total,
          
          `Difference in total costs (incl. national costs)` =
            -delta_total_cost_incl_national_costs,
          
          `ICER based on regional costs only (no national costs)` = icer_value,
          `Cost-effective? (lower country-specific threshold)` = ICER_below_CET_lower_bound,
          `Cost-effective? (higher country-specific threshold)` = ICER_below_CET_upper_bound,
          `Cost-effective? ($250 / DALY averted threshold)` = ICER_below_250,
          
          
          
          `ICER including national costs for every individual region (i.e. double-counting, just done for calculation purposes)` =
            icer_value_incl_national_costs,
          
          `ICER (incl. national costs) below CET lower bound (Y/N)` =
            ICER_below_CET_lower_bound_incl_national_costs,
          
          `ICER (incl. national costs) below CET upper bound (Y/N)` =
            ICER_below_CET_upper_bound_incl_national_costs,
          
          `ICER (incl. national costs) below $250 / DALY averted threshold (Y/N)` =
            ICER_below_250_incl_national_costs,
          
          
          `Current scenario chosen` = NA,
          `Data source for cost modelling` = source_of_cost_estimates,
          
          stringsAsFactors = FALSE,
          check.names = FALSE
          
        )
      )
      
      
      
      
      
    }
  }
  
  
  
ICER_comparison_table <- unique(ICER_comparison_table)

#write.csv(ICER_comparison_table, "C:/Users/ishan/OneDrive - London School of Hygiene and Tropical Medicine/Documents/Economic Evaluation/ICER_comparison_table.csv")


# UNITS OF ICER = cost per Additional DALYs averted

# remove any rows in the prioritisation table which have negative health impact in relation to the comparison 

# ICER_comparison_table <- read.csv("C:/Users/ishan/OneDrive - London School of Hygiene and Tropical Medicine/Documents/Economic Evaluation/ICER_comparison_table.csv", check.names = FALSE)
# 
# ICER_comparison_table <- ICER_comparison_table[,-1] 


ICER_comparison_table_clean <- ICER_comparison_table %>%
  filter(`Additional DALYs averted` > 0) 


# start with baseline of no PMC 
ICER_comparison_table_clean$`Current scenario chosen` <- rep("No PMC", dim(ICER_comparison_table_clean)[1])

# remove any rows in the prioritisation table which have ICERs above the CET_country
# prioritisation_table <- prioritisation_table %>%
#   filter(ICER_below_CET_country == "Yes")

remaining_ICER_comparison_table_clean <- ICER_comparison_table_clean
final_ranked_table_full <- c()

selected_countries <- character(0)


for (i in 1:1200) {
  
  if (i>1000) {print(i)}
  
  # --------------------------------------------------
  # 1. Keep only valid comparisons
  # --------------------------------------------------
  
  df_candidates <- remaining_ICER_comparison_table_clean %>%
    mutate(
      `Comparator scenario` = sub("^.* vs ", "", `PMC delivery strategy (vs. comparator)`)
    ) %>%
    filter(`Comparator scenario` == `Current scenario chosen`)
  
  if (nrow(df_candidates) == 0) break
  
  
  # --------------------------------------------------
  # 2. Decide which ICER to use for ranking
  # --------------------------------------------------
  
  df_candidates <- df_candidates %>%
    mutate(
      `Incremental economic cost per DALY averted vs. next higher option in ranking` = ifelse(
        `Country` %in% final_ranked_table_full$`Country`,
        `ICER based on regional costs only (no national costs)`,
        `ICER including national costs for every individual region (i.e. double-counting, just done for calculation purposes)`
      )
    )
  
  df_candidates <- df_candidates %>%
    filter(!is.na(`Incremental economic cost per DALY averted vs. next higher option in ranking`))
  
  if (nrow(df_candidates) == 0) break
  
  
  # --------------------------------------------------
  # 3. Select globally lowest ICER
  # --------------------------------------------------
  
  current_ranked <- df_candidates[
    which.min(df_candidates$`Incremental economic cost per DALY averted vs. next higher option in ranking`),
  ]
  
  
  
  # --------------------------------------------------
  # 5. Update chosen scenario
  # --------------------------------------------------
  
  scen_pair <- strsplit(current_ranked$`PMC delivery strategy (vs. comparator)`, " vs ")[[1]]
  new_chosen_scenario <- scen_pair[1]
  
  remaining_ICER_comparison_table_clean[
    remaining_ICER_comparison_table_clean$`Country` == current_ranked$`Country` &
      remaining_ICER_comparison_table_clean$`Region (admin-1)` == current_ranked$`Region (admin-1)`,
    "Current scenario chosen"
  ] <- new_chosen_scenario
  
  current_ranked$`Current scenario chosen` <- new_chosen_scenario
  current_ranked$`Rank` <- i
  
  
  # --------------------------------------------------
  # 6. Cost contribution (incl treatment costs)
  # --------------------------------------------------
  
  
  first_time_country <- !(current_ranked$`Country` %in% final_ranked_table_full$`Country`)
  
  incremental_total_cost_used <- ifelse(
    first_time_country,
    current_ranked$`Difference in total costs (incl. national costs)`,
    current_ranked$`Difference in total costs`
  )
  
  current_ranked$`Additional net economic costs of PMC implementation and treatment` <- incremental_total_cost_used
  
  
  
  incremental_implementation_cost_used <- ifelse(
    first_time_country,
    current_ranked$`Difference in implementation costs (incl. national costs)`,
    current_ranked$`Difference in implementation costs`
  )
  
  current_ranked$`Additional financial costs of PMC implementation` <- incremental_implementation_cost_used
  
  
  
  
  # --------------------------------------------------
  # 7. CET flags
  # --------------------------------------------------
  
  current_ranked$`Cost-effective? (higher country-specific threshold)` <- ifelse(
    !is.na(current_ranked$`Incremental economic cost per DALY averted vs. next higher option in ranking`) &
      current_ranked$`Incremental economic cost per DALY averted vs. next higher option in ranking` < current_ranked$`Cost-effectiveness threshold (upper)`,
    "yes", "no"
  )
  
  current_ranked$`Cost-effective? (lower country-specific threshold)` <- ifelse(
    !is.na(current_ranked$`Incremental economic cost per DALY averted vs. next higher option in ranking`) &
      current_ranked$`Incremental economic cost per DALY averted vs. next higher option in ranking` < current_ranked$`Cost-effectiveness threshold (lower)`,
    "yes", "no"
  )
  
  
  current_ranked$`Cost-effective? ($250 / DALY averted threshold)`  <- ifelse(
    !is.na(current_ranked$`Incremental economic cost per DALY averted vs. next higher option in ranking`) &
      current_ranked$`Incremental economic cost per DALY averted vs. next higher option in ranking` < 250,
    "yes", "no"
  )
  
  
  
  
  # --------------------------------------------------
  # 8. Append
  # --------------------------------------------------
  
  final_ranked_table_full <- rbind(final_ranked_table_full, current_ranked)
  
  
  # --------------------------------------------------
  # 9. Remove used comparison
  # --------------------------------------------------
  
  remaining_ICER_comparison_table_clean <- remaining_ICER_comparison_table_clean %>%
    filter(
      !(
        `Country` == current_ranked$`Country` &
          `Region (admin-1)` == current_ranked$`Region (admin-1)` &
          `PMC delivery strategy (vs. comparator)` == current_ranked$`PMC delivery strategy (vs. comparator)`
      )
    )
}



## --------------------------------------------------
## Post-processing
## --------------------------------------------------

final_ranked_table_full <- final_ranked_table_full %>%
  mutate(

    # `Cost-effectiveness threshold (lower)` = round(`Cost-effectiveness threshold (lower)`, 0),
    # `Cost-effectiveness threshold (upper)` = round(`Cost-effectiveness threshold (upper)`, 0),
    # 
    ## round all incidence & cost metrics to 1 dp
    `Additional clinical malaria cases averted` = round(`Additional clinical malaria cases averted`, 0),
    `Additional hospitalisations averted` = round(`Additional hospitalisations averted`, 0),
    `Additional deaths averted` = round(`Additional deaths averted`, 0),
    `Additional DALYs averted` = round(`Additional DALYs averted`, 0),
    
    
    
    `Financial cost of SP and administration consumables` = round(`Sub-total: Cost of PMC administration (SP+consumables)`, 0),
    `Cumulative cost of SP and administration consumables` = round(cumsum(`Financial cost of SP and administration consumables`)),
    
    
    
    `PMC implementation cost in this region (excludes any national costs)` =
      round(`Difference in implementation costs`, 0),
    
    `PMC implementation cost in this region (adding national costs for every region even though national costs should only be incurred in the first region)` =
      round(`Difference in implementation costs (incl. national costs)`, 0),
    
    `Additional financial costs of PMC implementation` =
      round(`Additional financial costs of PMC implementation`, 0),
    
    `Additional economic cost savings to public providers from reduced treatment` =
      round(`Difference in treatment costs`, 0),
    
    `PMC implementation costs less cost savings from avoided treatment (excludes national costs)` =
      round(`Difference in total costs`, 0),
    
    `PMC implementation costs less cost savings from avoided treatment (including national costs)` =
      round(`Difference in total costs (incl. national costs)`, 0),
    
    `Additional net economic costs of PMC implementation and treatment` =
      round(`Additional net economic costs of PMC implementation and treatment`, 0),
    
    `ICER based on regional costs only (no national costs)` =
      round(`ICER based on regional costs only (no national costs)`, 0),
    
    `ICER including national costs for every individual region (i.e. double-counting, just done for calculation purposes)` =
      round(`ICER including national costs for every individual region (i.e. double-counting, just done for calculation purposes)`, 0),
    
    `Incremental economic cost per DALY averted vs. next higher option in ranking` =
      round(`Incremental economic cost per DALY averted vs. next higher option in ranking`, 0),
    
    `Population (all ages)` = round(`Population (all ages)`,0),
    
    `Expected PMC doses delivered (age 0-1yr)`= round(`Expected PMC doses delivered (age 0-1yr)`,0),
    `Expected PMC doses delivered (age 1-2yr)`= round(`Expected PMC doses delivered (age 1-2yr)`,0),
    `Expected PMC doses delivered (age 2-2.5yr)` = round(`Expected PMC doses delivered (age 2-2.5yr)`,0),
    
    `Cumulative net costs of PMC implementation and treatment` = round(cumsum(`Additional net economic costs of PMC implementation and treatment`)),
    `Cumulative economic cost savings to public providers from reduced treatment` = round(cumsum(`Additional economic cost savings to public providers from reduced treatment`)),
    `Cumulative financial costs of PMC implementation` = round(cumsum(`Additional financial costs of PMC implementation`))
    
  )


## --------------------------------------------------
## Final column order
## --------------------------------------------------

final_ranked_table_full <- final_ranked_table_full %>%
  dplyr::select(
    `Country`,
    `Region (admin-1)`,
    `PMC delivery strategy (vs. comparator)`,
    `N doses in PMC schedule`,
    
    #`Cost-effectiveness threshold (lower)`,
    #`Cost-effectiveness threshold (upper)`,
    
    `Additional clinical malaria cases averted`,
    `Additional hospitalisations averted`,
    `Additional deaths averted`,
    `Additional DALYs averted`,
    
    
    `Financial cost of SP and administration consumables`,
    `Cumulative cost of SP and administration consumables`,
    
    
    
    `Additional financial costs of PMC implementation`,
    `Cumulative financial costs of PMC implementation`,
    
    
    `Additional economic cost savings to public providers from reduced treatment`,
    `Cumulative economic cost savings to public providers from reduced treatment`,
    
    
    # `PMC implementation cost in this region (excludes any national costs)`,
    # `PMC implementation cost in this region (adding national costs for every region even though national costs should only be incurred in the first region)`,
    
    `Additional net economic costs of PMC implementation and treatment`,
    `Cumulative net costs of PMC implementation and treatment`,
    
    
    `Incremental economic cost per DALY averted vs. next higher option in ranking`,
    
    `Cost-effective? (lower country-specific threshold)`,
    `Cost-effective? (higher country-specific threshold)`,
    `Cost-effective? ($250 / DALY averted threshold)`,
    
    
    `Population (all ages)`,
    
    `Expected PMC doses delivered (age 0-1yr)`,
    `Expected PMC doses delivered (age 1-2yr)`,
    `Expected PMC doses delivered (age 2-2.5yr)`,     
    
    `Data source for cost modelling`,
    
    
    
    # 
    # `PMC implementation costs less cost savings from avoided treatment (excludes national costs)`,
    # 
    `ICER based on regional costs only (no national costs)`
    # `ICER including national costs for every individual region (i.e. double-counting, just done for calculation purposes)`,
    
    
    
  )

