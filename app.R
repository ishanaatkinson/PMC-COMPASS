library(pacman)

p_load(
  shiny,
  shinydashboard,
  shinyBS,
  tidyr,
  dplyr,
  ggplot2,
  reshape2,
  ggpubr,
  gridExtra,
  readxl,
  stringi,
  XML,
  maps,
  readr,
  here,
  sf,
  magrittr,
  scatterpie,
  bslib,
  tidyverse,
  plotly,
  leaflet,
  viridis,
  htmltools,
  ggrepel,
  ggforce,
  lme4,
  tools,
  vroom,
  scales,
  ggnewscale
)


# CHANGE
# setwd(
#   "C:/Users/ishan/OneDrive - London School of Hygiene and Tropical Medicine/Documents/Decision tool app - test v2"
# )


#options(rsconnect.max.bundle.size = 6 * 1024^3)

#project_root <- here::here()



c("#E16E21", "#8FC7C7")

# Define UI for application that draws a histogram
ui <- fluidPage(


  tags$head(
    tags$style(HTML("
      /* Define your custom styles here */
      .custom-header-container {

        /*align-items: center;*/
        text-align: center

      }
      .custom-header {
        background-color: white; /* Example background color */
        color: grey; /* Example text color */
        font-size: 20px; /* Example font size */
        font-family: 'Roboto';
        text-align: center;
        padding: 10px;
        border-radius: 5px
      }
      
      .more-info {
        cursor: pointer;
        color: grey;
        margin-top: 10px;
        display: flex;
        align-items: center;
        justify-content: center;
        font-style: italic;
      }
      
      .additional-text {
        display: none;
        margin-top: 10px;
        font-size: 12px;
        color: grey;
      }
      
      .more-info i {
        margin-right: 5px;
      }
      
    "))
  ),
  
  
  
  
  # UI elements
  tags$div(class = "custom-header-container",
           tags$div(class = "custom-header",
                    h2("PMC Compass",  style = "color: #E16E21; font-weight: bold;"),
                    h4("Beta testing mode. Last updated: 25-02-26",  style = "font-style: italic; font-weight: bold;"),
                    h6("Informing choices to maximise the health impact of perennial malaria chemoprevention"),
                    tags$div(class = "more-info",
                             icon("info-circle"),
                             "Click here for more information on how to use this app"),
                    tags$div(class = "additional-text", 
                             "Use the main page to explore choices to maximise the health impact of perennial malaria chemoprevention assuming PMC is co-delivered with EPI. Additional information on economics, health impact and SP efficacy and resistance will be present on their respective pages. To explore other PMC delivery strategies, use the user input page and input the target ages and respective estimated coverages for your desired strategy. You can also edit our assumed dhps frequencies here which influence the efficacy of SP.")
                    
                    )),
  
  tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function() {
      var moreInfo = document.querySelector('.more-info');
      var additionalText = document.querySelector('.additional-text');
      
      moreInfo.addEventListener('click', function() {
        if (additionalText.style.display === 'none' || additionalText.style.display === '') {
          additionalText.style.display = 'block';
        } else {
          additionalText.style.display = 'none';
        }
      });
    });
  ")),
  
  
  
  page_navbar(
  theme = bs_theme(
    bg = "#FFF",
    fg = "black",
    primary = "#8FC7C7",
    secondary = "#E16E21",
    success = "limegreen",
    warning = "yellow",
    danger = "red",
    base_font = font_google("Roboto"),
    code_font = font_google("Source Code Pro"),
    bootswatch = "cosmo"
  ),
  
  nav_panel(title="Main page",
            fluidPage(
              sidebarLayout(
                sidebarPanel(width=3,
                             title = "Inputs:",
                             selectInput(
                               inputId = "country_main_page",
                               label = "Select PMC-eligible country",
                               choices = c(
                                 "Angola",
                                 "Burundi",
                                 "Benin",
                                 "Burkina Faso",
                                 "Central African Republic",
                                 "Cote d'Ivoire",
                                 "Cameroon",
                                 "DR Congo",
                                 "Congo-Brazzaville",
                                 "Gabon",
                                 "Ghana",
                                 "Guinea",
                                 "Equatorial Guinea",
                                 "Kenya",
                                 "Liberia",
                                 "Madagascar",
                                 "Mali",
                                 "Mozambique",
                                 "Malawi",
                                 "Niger",
                                 "Nigeria",
                                 "Sierra Leone",
                                 "South Sudan",
                                 "Chad",
                                 "Togo",
                                 "Tanzania",
                                 "Uganda",
                                 "Zambia"
                               ),
                               selected = "Angola"
                             ),
                             selectInput(
                               inputId = "country_or_area_main_page",
                               label = "Model whole country or a specific admin-1 unit?",
                               choices = c("Whole country", "Admin-1 unit"),
                               selected = "Whole country"
                             ),
                             conditionalPanel(
                               "input.country_or_area_main_page!='Whole country'",
                               selectInput(
                                 inputId = "area_main_page",
                                 label = "Admin-1 unit",
                                 choices = c(),
                                 selected = c()
                               )
                             ),
                             
                             actionButton("show_results_main_page", "Generate results")
                ),
                
                
                
                
                mainPanel(
                  width = 9,
                  
                  ## ---- DOWNLOAD BUTTON + WARNINGS ----
                  conditionalPanel(
                    condition = "input.show_results_main_page != 0",
                    div(
                      style = "margin-bottom: 15px; display: flex; align-items: center; gap: 15px;",
                      
                      # Download button
                      downloadButton(
                        outputId = "download_outputs",
                        label = "Download outputs"
                      ),
                      
                      # ---- Warning message 1 ----
                      conditionalPanel(
                        condition = "
          input.country_main_page == 'Burundi' ||
          input.country_main_page == 'DR Congo' ||
          input.country_main_page == 'Kenya' ||
          input.country_main_page == 'Mozambique' ||
          input.country_main_page == 'Malawi' ||
          input.country_main_page == 'South Sudan' ||
          input.country_main_page == 'Tanzania' ||
          input.country_main_page == 'Uganda' ||
          input.country_main_page == 'Zambia'
        ",
                        div(
                          style = "
            background-color: #f8d7da;
            color: #842029;
            padding: 8px 12px;
            border: 2px solid #dc3545;
            border-radius: 5px;
            font-weight: bold;
          ",
                          "SP protection in some areas of this country are SUBJECT TO CHANGE"
                        )
                      ),
                      
                      # ---- Warning message 2 ----
                      conditionalPanel(
                        condition = "
          input.country_main_page == 'Cameroon' ||
          input.country_main_page == 'Nigeria'
        ",
                        div(
                          style = "
            background-color: #f8d7da;
            color: #842029;
            padding: 8px 12px;
            border: 2px solid #dc3545;
            border-radius: 5px;
            font-weight: bold;
          ",
                          "Emerging resistance, SP protection in some areas of this country are SUBJECT TO CHANGE"
                        )
                      )
                      
                    )
                  ),
                  
                  ## ---- RESULTS ----
                  conditionalPanel(
                    condition = "input.show_results_main_page != 0",
                    uiOutput("output_navset_tabs_main_page")
                  )
                )
                
                
                
                # mainPanel(
                #   width = 9,
                #   
                #   ## ---- DOWNLOAD BUTTON ----
                #   conditionalPanel(
                #     condition = "input.show_results_main_page != 0",
                #     div(
                #       style = "margin-bottom: 15px;",
                #       downloadButton(
                #         outputId = "download_outputs",
                #         label = "Download outputs"
                #       )
                #     )
                #   ),
                #   
                #   ## ---- RESULTS ----
                #   conditionalPanel(
                #     condition = "input.show_results_main_page != 0",
                #     uiOutput("output_navset_tabs_main_page")
                #   )
                # )
                
                
                
                
                
              )
            )
  ),
  
  ##### HEALTH IMPACT TAB #####
  
  nav_panel(title = "Health impact",
            
            page_fluid(
              conditionalPanel(condition = "(input.show_results_main_page != 0)",
                               uiOutput("output_health_impact_tab"))
              
            )),
  
  ##### ECONOMIC EVALUATION TAB #####
  
  nav_panel(title = "Economics",
            
            page_fluid(
              conditionalPanel(condition = "(input.show_results_main_page != 0)",
                               uiOutput("output_cost_tab"))
              
            )),
  

  
  ##### SP RESISTANCE AND EFFICACY TAB #####
  
  nav_panel(title = "SP resistance and efficacy",
            page_fluid(
              
              conditionalPanel(condition = "(input.show_results_main_page != 0)",
                               uiOutput("output_SP_tab"))
              
              
            )),
  
  nav_panel(
    title = "User input",
    
    page_fluid(
      id = "user-input-tab",
      
      sidebarLayout(
        
        # ---- SIDEBAR ----
        sidebarPanel(
          width = 3,
          title = "Inputs",
          
          selectInput(
            inputId = "country",
            label = "Select PMC-eligible country",
            choices = c(
              "Angola","Burundi","Benin","Burkina Faso",
              "Central African Republic","Cote d'Ivoire",
              "Cameroon","DR Congo","Congo-Brazzaville",
              "Gabon","Ghana","Guinea","Equatorial Guinea",
              "Kenya","Liberia","Madagascar","Mali",
              "Mozambique","Malawi","Niger","Nigeria",
              "Sierra Leone","South Sudan","Chad",
              "Togo","Tanzania","Uganda","Zambia"
            ),
            selected = "Angola"
          ),
          
          selectInput(
            inputId = "country_or_area",
            label = "Model whole country or a specific admin-1 unit?",
            choices = c("Whole country", "Admin-1 unit"),
            selected = "Whole country"
          ),
          
          conditionalPanel(
            condition = "input.country_or_area != 'Whole country'",
            selectInput(
              inputId = "area",
              label = "Admin-1 unit",
              choices = NULL
            )
          ),
          
          selectInput(
            inputId = "change_haplotype_data",
            label = "Edit our estimated frequencies of molecular markers associated with SP resistance?",
            choices = c("No", "Yes"),
            selected = "No"
          ),
          
          numericInput(
            inputId = "number_of_doses",
            label = "Select the number of PMC doses in the schedule to model (maximum 15)",
            value = 1,
            min = 1,
            max = 15
          ),
          
          conditionalPanel(
            condition = "input.number_of_doses > 0",
            actionButton(
              inputId = "add_table",
              label = "Input data"
            )
          )
          
        ),
        
        
        # ---- MAIN PANEL ----
        mainPanel(
          width = 9,
          
          # Coverage + schedule only
          conditionalPanel(
            condition = "input.add_table > 0 && input.change_haplotype_data == 'No'",
            uiOutput("cov_and_schedule_card_only")
          ),
          
          # Coverage + schedule + haplotypes
          conditionalPanel(
            condition = "input.add_table > 0 && input.change_haplotype_data == 'Yes'",
            uiOutput("cov_and_schedule_and_haplotype_card")
          ),
          
          # Results
          conditionalPanel(
            condition = "input.show_results > 0",
            uiOutput("output_navset_tabs")
          )
          
        )
        
      )
    )
  )
  
  
  
  
  ))


##### SERVER #####

server <- function(input, output, session) {
  

  dose_data <- reactiveVal(NULL)
  
  # add admin-1 unit names for selected country to input
  observeEvent(input$country, {
    country <- input$country
    chosen_area <- input$area
    
    
    
    # ISHANA TO DO: when you select a country and then change the country, the admin1 units do not update with that so edit
    
    
    area_names <- unique((vroom::vroom(("files_needed/all_sites_for_DT.csv")) %>% filter(Country==country))$`Admin-1 unit`)

    updateSelectInput(session, "area", choices = unique(stri_trans_general(str = gsub("_", " ", stri_trans_general(str = gsub("-", "_", area_names), id = "Latin-ASCII")), id = "Latin-ASCII")))

    
  })
  
  
  # add admin-1 unit names for selected country to input
  observeEvent(input$country_main_page, {
    country_main_page <- input$country_main_page
    chosen_area_main_page <- input$area_main_page
    
    
    area_names <- unique((vroom::vroom(("files_needed/all_sites_for_DT.csv")) %>% filter(Country==country_main_page))$`Admin-1 unit`)
    
    #if (input$country_or_area == "Admin 1 unit") {
    updateSelectInput(session, "area_main_page", choices = unique(stri_trans_general(str = gsub("_", " ", stri_trans_general(str = gsub("-", "_", area_names), id = "Latin-ASCII")), id = "Latin-ASCII")))
    #}
    
  })
  
  haplotype_data <- reactiveVal(NULL)
  edited_haplotype_data <- reactiveVal(NULL)
  
  haplotype_data_for_table <- reactiveVal(NULL)
  edited_haplotype_data_for_table <- reactiveVal(NULL)
  
  data <- reactiveVal(NULL)
  edited_data <- reactiveVal(NULL)
  
  
  # get data from the table a user has edited when "show results" button is pressed
  sourced_data_main_page <- eventReactive(input$show_results_main_page, {

    country_main_page <- input$country_main_page
    chosen_area_main_page <- gsub(" ", "_", input$area_main_page)
    
    coverage_df_admin1 <- vroom::vroom(("files_needed/all_sites_admin1_cov_weighted_average.csv"))
    coverage_df_country <- vroom::vroom(("files_needed/all_countries_cov_weighted_average.csv"))
  
    coverage_df_admin1$unique_ages_n <- lapply(coverage_df_admin1$unique_ages, 
                                             function(x) {
                                               # Split the string, take the first element (if multiple)
                                               split_result <- strsplit(x, ",\\s*")[[1]]
                                               # Convert to numeric, handling potential errors/NAs
                                               as.numeric(split_result)
                                             })
    
    coverage_df_country$unique_ages_n <- lapply(coverage_df_country$unique_ages, 
                                             function(x) {
                                               # Split the string, take the first element (if multiple)
                                               split_result <- strsplit(x, ",\\s*")[[1]]
                                               # Convert to numeric, handling potential errors/NAs
                                               as.numeric(split_result)
                                             })
    
    coverage_df_admin1$avg_coverage_by_age_n <- lapply(coverage_df_admin1$avg_coverage_by_age, 
                                             function(x) {
                                               # Split the string, take the first element (if multiple)
                                               split_result <- strsplit(x, ",\\s*")[[1]]
                                               # Convert to numeric, handling potential errors/NAs
                                               as.numeric(split_result)
                                             })
    
    coverage_df_country$avg_coverage_by_age_n <- lapply(coverage_df_country$avg_coverage_by_age, 
                                             function(x) {
                                               # Split the string, take the first element (if multiple)
                                               split_result <- strsplit(x, ",\\s*")[[1]]
                                               # Convert to numeric, handling potential errors/NAs
                                               as.numeric(split_result)
                                             })
    
    
    # ISHANA NOTE: add in a "run epi only or with vit a" opion at the beginning when we have data
    # and allow this as a filtering scenario here based off what a user decides
    
    if (input$country_or_area_main_page == "Admin-1 unit") {
      schedule <- unlist((coverage_df_admin1 %>% filter(Country == country_main_page,  `Admin-1 unit` == chosen_area_main_page))$unique_ages_n)
      cov <- unlist((coverage_df_admin1 %>% filter(Country == country_main_page, `Admin-1 unit` == chosen_area_main_page))$avg_coverage_by_age_n)
      area_names <- unique((vroom::vroom(("files_needed/all_sites_for_DT.csv")) %>% filter(Country==country_main_page, `Admin-1 unit` == chosen_area_main_page))$`Admin-1 unit`)
      number_of_doses <- length(schedule)
    }
    
    if (input$country_or_area_main_page == "Whole country") {
      schedule <- unlist((coverage_df_country %>% filter(Country == country_main_page))$unique_ages_n)
      cov <- unlist((coverage_df_country %>% filter(Country == country_main_page))$avg_coverage_by_age_n)
      area_names <- unique((vroom::vroom(("files_needed/all_sites_for_DT.csv")) %>% filter(Country==country_main_page))$`Admin-1 unit`)
      number_of_doses <- length(schedule)
      
    }
    
    
    haplotype_data_final <- read_xlsx(("files_needed/haplotype_data_final.xlsx"))
    final_ranked_table_full <- read_xlsx(("files_needed/final_ranked_table_full.xlsx"))
    cost_data <- read_xlsx(("files_needed/cost_and_incidence_data.xlsx"))
    protective_efficacy_30_days <- read_xlsx(("files_needed/protective_efficacy_30_days.xlsx"))
    cet_thresholds <- read_xlsx(("files_needed/cost_effectiveness_thresholds.xlsx"))

    source("code_main_page.R", local = TRUE)
    
    

    saved <- ls()
    list_items<-list() 
    
    for (i in 1:length(saved)) {
      list_items[[saved[i]]] <- get(saved[i])
    }
    
    list_items
    
  })
  
  
  # create incidence graph for either the default values or the inputted PMC schedule
  
  main_page_output_generation <- eventReactive(input$show_results_main_page, {
    
    
    data <- sourced_data_main_page()
    
    for (i in 1:length(data)) {
      assign(names(data[i]), data[[i]])
    }
  
    colours <- c("#FB61D7", "#00B6EB", "#FFA500", "#F8766D", "#00C094")
    
    main_page_outputs <- list()
    

    output$download_outputs <- downloadHandler(
      filename = function() {
        paste0(get_iso3(input$country_main_page),
               "/PMC_Compass_Modelling_Outputs.pdf"
        )
      },
      content = function(file) {
        
        src <- file.path(
          get_iso3(input$country_main_page),
          "sim_results_DHS_coverage_levels/PMC_Compass_Modelling_Outputs.pdf"
        )
        
        if (!file.exists(src)) {
          stop("Requested file does not exist.")
        }
        
        file.copy(src, file)
      }
    )
    

    if (input$country_or_area_main_page == "Admin-1 unit") {
      
      withProgress(message = "Preparing results, please wait", {
      # Clinical incidence graph

      # Initialize the ggplot object
      clinical_inc_graphs <- ggplot()
      incProgress(1/10)
      names_in_plot <- c()
      

      # Iterate over columns starting with number_of_doses
      for (i in 1:length(number_of_doses)) {
        
        PMC_impact_ppy <- PMC_impact_ppy_merged_FINAL %>% filter(`N doses` == number_of_doses[i], `Admin-1 unit`==chosen_area_main_page, infection_class=="clinical")
        incidence_ppy_df <- incidence_ppy_df_merged %>% filter(`Admin-1 unit`==chosen_area_main_page, infection_class == "clinical")
        
        # get first 5 letters from each in 
        vaccines_text <- unique(
          (coverage_df_admin1 %>% filter(Country == country_main_page, `Admin-1 unit`==chosen_area_main_page))$vaccines_by_age
        )
        
        vaccines_cov <- unlist(unique(
               (coverage_df_admin1 %>% filter(Country == country_main_page, `Admin-1 unit`==chosen_area_main_page))$avg_coverage_by_age_n
          ))
        
        vaccines_vec <- trimws(unlist(strsplit(vaccines_text, ",")))
        
        vaccines_dosed <- paste0(
          "Dose ",
          seq_along(vaccines_vec),
          ": ",
          vaccines_vec,
          " (",
          round(vaccines_cov * 100,0), 
          "% coverage)",
          collapse = "\n"
        )
        
        legend_label <- paste0(
          unique((coverage_df_admin1 %>% filter(Country == country_main_page, `Admin-1 unit`==chosen_area_main_page))$Scenario),
          ":\n",
          vaccines_dosed
        )
        
        names_in_plot <- c(names_in_plot, legend_label)
        
        clinical_inc_graphs <- clinical_inc_graphs +
          geom_line(aes_string(x = PMC_impact_ppy$age_in_days_midpoint, 
                               y = 1000 * PMC_impact_ppy$value, 
                               colour = paste0("\"", legend_label, "\"")), 
                    linewidth = 0.6) 
        
        legend_label2 <- paste0(legend_label) #paste0("Cases averted, ", number_of_doses[i], " doses")
        names_in_plot <- c(names_in_plot, legend_label2)
        
        clinical_inc_graphs <- clinical_inc_graphs + 
          geom_ribbon(aes_string(x = PMC_impact_ppy$age_in_days_midpoint, 
                                 ymin = 1000 * PMC_impact_ppy$value, 
                                 ymax = 1000 * incidence_ppy_df$value, 
                                 fill = paste0("\"", legend_label2, "\"")), 
                      alpha = 0.3, show.legend = TRUE)
      }
      
      
      clinical_inc_graphs <- clinical_inc_graphs + 
        geom_line(aes_string(x = incidence_ppy_df$age_in_days_midpoint, 
                             y = 1000 * incidence_ppy_df$value, 
                             colour = "\"No PMC\""))
      
      incProgress(1/10)
      
      
      names(clinical_inc_graphs$layers) <- c(names_in_plot, "No PMC")
      
      clinical_inc_graphs <- ggplot() + clinical_inc_graphs$layers +
        labs(x = "Age (months)", y = "New clinical infections per 1000 children", colour = "Incidence by delivery model [select/deselect]", fill = "Cases averted", title = paste0(chosen_area_main_page, ", ", country_main_page)) +
        ylim(0, max((1000*incidence_ppy_df$value) + 500)) +

        scale_x_continuous(
          breaks = c(0, 183, 365, 549, 730, 913),
          labels = c("0", "6", "12", "18", "24", "30")
        ) + 
        scale_fill_manual("Cases averted", breaks = names_in_plot[c(seq(from=2, to=length(names_in_plot), by=2))], values = colours[2:5]) +
        scale_color_manual("Incidence by delivery model [select/deselect]", breaks = c("No PMC", names_in_plot[c(seq(from=1, to=length(names_in_plot), by=2))]), values = colours[1:5]) + 
        theme(panel.grid.major = element_line(colour = "gray", linewidth = 0.2), 
              panel.background = element_rect(fill = 'transparent'), 
              plot.background = element_rect(fill = 'transparent', color = NA), 
              legend.background = element_rect(fill = 'transparent')) 
      
      
      clinical_inc_graphs_plotly <- ggplotly(clinical_inc_graphs)
      
      
      for (i in 1:length(clinical_inc_graphs_plotly$x$data)){
        if (!is.null(clinical_inc_graphs_plotly$x$data[[i]]$name)){
          clinical_inc_graphs_plotly$x$data[[i]]$name <- gsub("^\\(", "", clinical_inc_graphs_plotly$x$data[[i]]$name)
          clinical_inc_graphs_plotly$x$data[[i]]$name <- gsub(",1)", "", clinical_inc_graphs_plotly$x$data[[i]]$name)
          
        }
      }
      
      text_x <- paste0("Age: ", clinical_inc_graphs_plotly$x$data[[i]]$x)
      text_y <- paste0("Clinical incidence per 1000: ", round(clinical_inc_graphs_plotly$x$data[[i]]$y, digits=1))
      text_z <- Map(function(x, y) paste0("", x, " days\n", y, ""), text_x, text_y)
      

      clinical_inc_graphs_plotly <- clinical_inc_graphs_plotly %>%
        style(hoverinfo = "skip", traces = c(3, 5, 7, 9)) %>% 
        style(text = unlist(text_z))
      
      # Assign the plot to the main_page_outputs
      main_page_outputs$clinical_incidence_graph <- clinical_inc_graphs_plotly
      
      ##########################################################################
      
      # severe incidence graph 
      incProgress(1/10)
      # Initialize the ggplot object
      severe_inc_graphs <- ggplot()
      
      names_in_plot <- c()
      
      # Iterate over columns starting with number_of_doses
      for (i in 1:length(number_of_doses)) {
        
        PMC_impact_ppy <- PMC_impact_ppy_merged_FINAL %>% filter(`N doses` == number_of_doses[i], `Admin-1 unit`==chosen_area_main_page, infection_class=="severe")
        incidence_ppy_df <- incidence_ppy_df_merged %>% filter(`Admin-1 unit`==chosen_area_main_page, infection_class == "severe")
        
        # get first 5 letters from each in 
        vaccines_text <- unique(
          (coverage_df_admin1 %>% filter(Country == country_main_page, `Admin-1 unit`==chosen_area_main_page))$vaccines_by_age
        )
        
        vaccines_cov <- unlist(unique(
          (coverage_df_admin1 %>% filter(Country == country_main_page, `Admin-1 unit`==chosen_area_main_page))$avg_coverage_by_age_n
        ))
        
        vaccines_vec <- trimws(unlist(strsplit(vaccines_text, ",")))
        
        vaccines_dosed <- paste0(
          "Dose ",
          seq_along(vaccines_vec),
          ": ",
          vaccines_vec,
          " (",
          round(vaccines_cov * 100,0), 
          "% coverage)",
          collapse = "\n"
        )
        
        legend_label <- paste0(
          unique((coverage_df_admin1 %>% filter(Country == country_main_page, `Admin-1 unit`==chosen_area_main_page))$Scenario),
          ":\n",
          vaccines_dosed
        )
        

        
        names_in_plot <- c(names_in_plot, legend_label)
        
        severe_inc_graphs <- severe_inc_graphs +
          geom_line(aes_string(x = PMC_impact_ppy$age_in_days_midpoint, 
                               y = 1000 * PMC_impact_ppy$value, 
                               colour = paste0("\"", legend_label, "\"")), 
                    linewidth = 0.6) 
        
        legend_label2 <- paste0(legend_label) #paste0("Cases averted, ", number_of_doses[i], " doses")
        names_in_plot <- c(names_in_plot, legend_label2)
        
        severe_inc_graphs <- severe_inc_graphs + 
          geom_ribbon(aes_string(x = PMC_impact_ppy$age_in_days_midpoint, 
                                 ymin = 1000 * PMC_impact_ppy$value, 
                                 ymax = 1000 * incidence_ppy_df$value, 
                                 fill = paste0("\"", legend_label2, "\"")), 
                      alpha = 0.3, show.legend = TRUE)
      }
      
      
      severe_inc_graphs <- severe_inc_graphs + 
        geom_line(aes_string(x = incidence_ppy_df$age_in_days_midpoint, 
                             y = 1000 * incidence_ppy_df$value, 
                             colour = "\"No PMC\""))
      
      
      
      names(severe_inc_graphs$layers) <- c(names_in_plot, "No PMC")
      
      severe_inc_graphs <- ggplot() + severe_inc_graphs$layers +
        labs(x = "Age (months)", y = "New hospitalisations per 1000 children", colour = "Hospitalisations by delivery model [select/deselect]", fill = "Hospitalisations averted", title = paste0(chosen_area_main_page, ", ", country_main_page)) +
        ylim(0, max((1000*incidence_ppy_df$value)) + 100) +
        
        scale_x_continuous(
          breaks = c(0, 183, 365, 549, 730, 913),
          labels = c("0", "6", "12", "18", "24", "30")
        ) + 
        scale_fill_manual("Hospitalisations averted", breaks = names_in_plot[c(seq(from=2, to=length(names_in_plot), by=2))], values = colours[2:5]) +
        scale_color_manual("Hospitalisations by delivery model [select/deselect]", breaks = c("No PMC", names_in_plot[c(seq(from=1, to=length(names_in_plot), by=2))]), values = colours[1:5]) + 
        theme(panel.grid.major = element_line(colour = "gray", linewidth = 0.2), 
              panel.background = element_rect(fill = 'transparent'), 
              plot.background = element_rect(fill = 'transparent', color = NA), 
              legend.background = element_rect(fill = 'transparent')) 
      
      
      severe_inc_graphs_plotly <- ggplotly(severe_inc_graphs)
      incProgress(1/10)
      
      for (i in 1:length(severe_inc_graphs_plotly$x$data)){
        if (!is.null(severe_inc_graphs_plotly$x$data[[i]]$name)){
          severe_inc_graphs_plotly$x$data[[i]]$name <- gsub("^\\(", "", severe_inc_graphs_plotly$x$data[[i]]$name)
          severe_inc_graphs_plotly$x$data[[i]]$name <- gsub(",1)", "", severe_inc_graphs_plotly$x$data[[i]]$name)
          
        }
      }
      
      text_x <- paste0("Age: ", severe_inc_graphs_plotly$x$data[[i]]$x)
      text_y <- paste0("Hospitalisations per 1000: ", round(severe_inc_graphs_plotly$x$data[[i]]$y, digits=1))
      text_z <- Map(function(x, y) paste0("", x, " days\n", y, ""), text_x, text_y)
      
      
      severe_inc_graphs_plotly <- severe_inc_graphs_plotly %>%
        style(hoverinfo = "skip", traces = c(3, 5, 7, 9)) %>% 
        style(text = unlist(text_z))
      
      # Assign the plot to the main_page_outputs
      main_page_outputs$severe_incidence_graph <- severe_inc_graphs_plotly
      
      ##########################################################################

      dosing_schedules <- data.frame(Dose = paste0("Dose ",seq_along(vaccines_vec)), 
        `Co-delivered intervention` = vaccines_vec, 
        `Coverage (%)` = paste0(round(vaccines_cov * 100,0), "%"),
        check.names=FALSE)
      
      main_page_outputs$dosing_schedules <- dosing_schedules
      
      # Haplotype data table 
      
      haplotype_data_df <- haplotype_data_final %>% filter(Country == country_main_page, `Admin-1 unit` == chosen_area_main_page)
      
      haplotype_data_df <- haplotype_data_df[c("Country", "Admin-1 unit",  "latitude", "longitude", "I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other")]
      
      colnames(haplotype_data_df) <- c("Country", "Admin-1 unit",  "latitude", "longitude", "I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other")
      
      haplotype_data_df[, c("I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other")] <- lapply(haplotype_data_df[, c( "I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other")], function(x) round(as.numeric(x), digits = 3))
      
      for (i in 1:dim(haplotype_data_df)[1]) {
        haplotype_data_df[i, "Sum of proportions"] <- sum(unlist(c(haplotype_data_df[i, "I_AKA_"], haplotype_data_df[i, "I_GKA_"], haplotype_data_df[i, "I_GEA_"], haplotype_data_df[i, "I_GEG_"], haplotype_data_df[i, "V_GKA_"], haplotype_data_df[i, "V_GKG_"], haplotype_data_df[i, "Other"])), na.rm = TRUE)
      }
      
      haplotype_data_df$`Admin-1 unit` <- gsub("_", " ", haplotype_data_df$`Admin-1 unit`)
      main_page_outputs$haplotype_table <- haplotype_data_df[, c("Country", "Admin-1 unit", "I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other")]
      
      ##########################################################################
      incProgress(1/10)
      
      
      #########################################################################
      
      # Haplotype map 
      
      # names(adm1)[names(adm1) == "NAME_2"] <- "Region"
      # adm1$NAME_2 <- adm1$Region
      # 
      # 
      # haplotype_proportions_map <- ggplot(adm1) + 
      #   geom_sf() + 
      #   theme_bw() +
      #   theme(
      #     legend.text = element_text(size = 12),
      #     legend.title = element_text(size = 15),
      #     title = element_text(size = 20)
      #   ) +
      #   scatterpie::geom_scatterpie(
      #     data = haplotype_data_df,
      #     cols = c("I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other"),
      #     aes(x = longitude, y = latitude, group = Region, r = 0.13 * min(diff(range(haplotype_data_df$longitude)), diff(range(haplotype_data_df$latitude)))) # Reduced radius from 0.8 to 0.3
      #   ) +
      #   scale_fill_manual(
      #     values = c(  "#00C094", "#00B6EB", "#FFA500","#F8766D","#b7a1ff", "#7361b3","#D2B48C"),
      #     name = "Haplotypes",
      #     breaks = c("I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other"),
      #     labels = c("I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other")
      #   ) +
      #   theme(
      #     axis.title.x = element_blank(),
      #     axis.text.x = element_blank(),
      #     axis.ticks.x = element_blank(),
      #     axis.title.y = element_blank(),
      #     axis.text.y = element_blank(),
      #     axis.ticks.y = element_blank(),
      #     panel.background = element_blank(),
      #     panel.border = element_blank(),
      #     panel.grid.major = element_blank(),
      #     panel.grid.minor = element_blank(),
      #     plot.background = element_blank()
      #   )
      
      adm1$NAME_2 <- stri_trans_general(str = gsub(" ", "_", adm1$NAME_1), id = "Latin-ASCII")
      adm1$NAME_2 <- stri_trans_general(str = gsub("-", "_", adm1$NAME_2), id = "Latin-ASCII")
      
      adm1$`Admin-1 unit` <- adm1$NAME_2
      combined_protective_efficacy_30_days <- merge(protective_efficacy_30_days %>% filter(Country == country_main_page, `Admin-1 unit`== chosen_area_main_page), adm1, by="Admin-1 unit") 
      
      
      haplotype_proportions_map <- ggplot(combined_protective_efficacy_30_days) + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
        theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
        geom_sf(data=adm1)+
        geom_sf(aes(fill=day_30_protect_efficacy, geometry=geometry), size=0.5)  +
        geom_sf_label(aes(label = paste0(`Admin-1 unit`), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
        ggtitle(paste0("Average 30 day protective efficacy following a SP dose: ", stri_trans_general(str = gsub("_", " ", chosen_area_main_page), id = "Latin-ASCII"), ", ", country_main_page)) + labs(fill="30 day protective efficacy") +
        scale_fill_viridis_c(limits=c(min(combined_protective_efficacy_30_days$day_30_protect_efficacy),max(combined_protective_efficacy_30_days$day_30_protect_efficacy))) + 
        theme(text=element_text(size=14),legend.title =element_text(size=14), legend.text = element_text(size=14),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank())
      
      
      
      main_page_outputs$haplotype_map <- haplotype_proportions_map 
      
      #########################################################################
      
      # Probability of drug protection
      incProgress(1/10)
      
      prob_drug_protect_plot <- ggplot()
      names_in_plot <- c()
      
      # Iterate over columns starting with 'prot_overall_'
      for (col in names(df_drug_protect)) {
        if (startsWith(col, 'prot_overall_')) {
          
          legend_label <- gsub('prot_overall_', '', col)
          legend_label <- gsub('_', ' ', legend_label)
          names_in_plot <- c(names_in_plot, legend_label)
          
          # Create geom_line for each column
          print(legend_label)
          prob_drug_protect_plot <- prob_drug_protect_plot + geom_line(aes_string(x = df_drug_protect$time, y = paste0("df_drug_protect[[\"", col, "\"]]"), colour = paste0("\"", legend_label, "\"")), linewidth=1)
          
        }
        
        
      }
      
      
      names(prob_drug_protect_plot$layers) <- names_in_plot
      
      layer_index <- which(names(prob_drug_protect_plot$layers) == gsub("_", " ", chosen_area_main_page))
      
      # Customize legend and labels
      prob_drug_protect_plot <- ggplot() + prob_drug_protect_plot$layers[[layer_index]] +
        labs(x = "Days since SP dose", y = "Probability of drug protection", colour= "Admin-1 unit",
             title = "Probability of drug protection following an SP dose") +
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 14),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 14))
      

      
      main_page_outputs$prob_drug_protect_graph <- ggplotly(prob_drug_protect_plot) 
      
      
      prob_drug_protect_plotly <- main_page_outputs$prob_drug_protect_graph
      
      
      for (i in 1:length(prob_drug_protect_plotly$x$data)) {
        
        if (!is.null(prob_drug_protect_plotly$x$data[[i]]$name)) {
          
          prob_drug_protect_plotly$x$data[[i]]$name <-
            gsub("^\\(", "", prob_drug_protect_plotly$x$data[[i]]$name)
          
          prob_drug_protect_plotly$x$data[[i]]$name <-
            gsub(",1\\)$", "", prob_drug_protect_plotly$x$data[[i]]$name)
          
        }
      }
      
      for (i in 1:length(prob_drug_protect_plotly$x$data)) {
        
        # Build hover text
        text_x <- paste0(
          "Days since SP dose: ",
          prob_drug_protect_plotly$x$data[[i]]$x
        )
        
        text_y <- paste0(
          "Probability of protection: ",
          round(prob_drug_protect_plotly$x$data[[i]]$y, 3)
        )
        
        text_xy <- Map(
          function(x, y) paste0(x, "\n", y),
          text_x,
          text_y
        )
        
        # Attach hover text to trace
        prob_drug_protect_plotly$x$data[[i]]$text <- unlist(text_xy)
        prob_drug_protect_plotly$x$data[[i]]$hoverinfo <- "text"
      }
      
      main_page_outputs$prob_drug_protect_graph <-
        prob_drug_protect_plotly
    
      
      
      
      #########################################################################
      
      ## ----------------------------------------------------
      ## ICERS map
      ## ----------------------------------------------------
      
    
      adm1$`Region (admin-1)` <- adm1$NAME_2
      
      final_ranked_table_full <- final_ranked_table_full %>%
        mutate(
          `Rank of PMC option by cost-effectiveness` = row_number()
        )
      
      adm1_merged <- left_join(adm1, final_ranked_table_full, by = "Region (admin-1)")
      
      
      # Extract values
      icer_vals <- adm1_merged$`ICER based on regional costs only (no national costs)`
      
      min_val <- floor(min(icer_vals, na.rm = TRUE) / 10) * 10
      max_val <- ceiling(max(icer_vals, na.rm = TRUE) / 10) * 10
      mid_val <- round((min_val + max_val) / 2)
      
      # icer_map <- ggplot(adm1_merged) +
      #   geom_sf(data = adm1) +
      #   geom_sf(aes(fill = `ICER based on regional costs only (no national costs)`,
      #               geometry = geometry),
      #           size = 0.5) +
      #   geom_sf_label(
      #     aes(label = paste0("Rank: ", `Rank of PMC option by cost-effectiveness`),
      #         geometry = geometry),
      #     fun.geometry = sf::st_point_on_surface,
      #     size = 3,
      #     alpha = 0.2
      #   ) +
      #   theme_void() +
      #   theme(
      #     legend.title = element_text(hjust = 0.5)
      #   ) +
      #   scale_fill_gradient2(
      #     low = "#008631",
      #     mid = "#cefad0",
      #     high = "#ffffff",
      #     midpoint = mid_val,
      #     limits = c(min_val, max_val),   # IMPORTANT
      #     breaks = c(min_val, mid_val, max_val),
      #     labels = c(
      #       paste0(comma(min_val), " (most cost-effective, rank=1)"),
      #       comma(mid_val),
      #       paste0(comma(max_val), " (least cost-effective, rank=", max(adm1_merged$`Rank of PMC option by cost-effectiveness`), ")")
      #     ),
      #     guide = guide_colorbar(reverse = TRUE)
      #   ) +
      #   labs(fill = "Cost-effectiveness\n(incremental cost-effectiveness ratio)\n")  + 
      #   theme(text=element_text(size=14),legend.title =element_text(size=14), legend.text = element_text(size=14))
      # 
      
      icer_map <- ggplot(adm1_merged) +
        
        # Base map
        geom_sf(data = adm1, fill = NA, color = "grey60") +
        
        # Filled regions
        geom_sf(aes(fill = `ICER based on regional costs only (no national costs)`,
                    geometry = geometry),
                size = 0.5) +
        
        # Thick border for selected region
        geom_sf(
          data = subset(adm1_merged, `Region (admin-1)` == chosen_area_main_page),  # <-- replace admin1_name
          fill = NA,
          color = "black",
          size = 1.5
        ) +
        
        # Labels
        geom_sf_label(
          aes(label = paste0("Rank: ", `Rank of PMC option by cost-effectiveness`),
              geometry = geometry),
          fun.geometry = sf::st_point_on_surface,
          size = 3,
          alpha = 0.2
        ) +
        
        theme_void() +
        theme(
          legend.title = element_text(hjust = 0.5)
        ) +
        
        scale_fill_gradient2(
          low = "#008631",
          mid = "#cefad0",
          high = "#ffffff",
          midpoint = mid_val,
          limits = c(min_val, max_val),
          breaks = c(min_val, mid_val, max_val),
          labels = c(
            paste0(comma(min_val), " (most cost-effective, rank=1)"),
            comma(mid_val),
            paste0(comma(max_val), " (least cost-effective, rank=",
                   max(adm1_merged$`Rank of PMC option by cost-effectiveness`), ")")
          ),
          guide = guide_colorbar(reverse = TRUE)
        ) +
        
        labs(fill = "Cost-effectiveness\n(incremental cost-effectiveness ratio)\n") +
        
        theme(
          text = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14)
        )
      
      main_page_outputs$icer_map <- icer_map 
      
      ##########################################################################
      
      # Prioritisation table 
      
      
      
      final_ranked_table_full <- final_ranked_table_full %>% filter(Country==get_iso3(country_main_page), `Region (admin-1)`==chosen_area_main_page)
      final_ranked_table_full$Country <- country_lookup[final_ranked_table_full$Country] 
      
      final_ranked_table_full <- final_ranked_table_full %>% dplyr::select(-c(`ICER based on regional costs only (no national costs)`, `Cost-effective? (lower country-specific threshold)`, `Cost-effective? (higher country-specific threshold)`, `Cost-effective? ($250 / DALY averted threshold)`)) %>% 
        rename("Financial (budget) cost of SP+consumables" = "Financial cost of SP and administration consumables" ) %>% 
        rename("Cumulative financial (budget) cost of SP+consumables" = "Cumulative cost of SP and administration consumables")  %>%
        relocate(`Rank of PMC option by cost-effectiveness`, .after = 2)
      
        # final_ranked_table$`Cumulative cost of SP and administration consumables` <- cumsum(final_ranked_table$`Financial cost of SP and administration consumables`)
        # final_ranked_table$`Cumulative economic cost savings to public providers from reduced treatment` <- cumsum(final_ranked_table$`Additional economic cost savings to public providers from reduced treatment`)
        # final_ranked_table$`Cumulative net costs of PMC implementation and treatment` <- cumsum(final_ranked_table$`Additional economic costs of PMC implementation` + final_ranked_table$`Additional net economic costs of PMC implementation and treatment`)
        # 
        
        main_page_outputs$prioritisation_table <- data.frame(final_ranked_table_full, check.names = FALSE) 
      
      
      
      incProgress(1/10)
      # All health outputs 
      
      # Define the new order of columns
      new_column_order <- c("Country", 
                            "Admin-1 unit", 
                            "Age group", 
                            "N doses", 
                            "clinical_cases_no_PMC", 
                            "clinical_cases_no_PMC_per1000", 
                            "clinical_cases_with_PMC", 
                            "clinical_cases_with_PMC_per1000", 
                            "clinical_cases_averted_with_PMC", 
                            "clinical_cases_averted_with_PMC_per1000", 
                            "clinical_cases_reduction", 
                            "severe_cases_no_PMC", 
                            "severe_cases_no_PMC_per1000", 
                            "severe_cases_with_PMC", 
                            "severe_cases_with_PMC_per1000", 
                            "severe_cases_averted_with_PMC", 
                            "severe_cases_averted_with_PMC_per1000", 
                            "severe_cases_reduction" 
                            
      )
      
      new_column_names <- c("Country",                          
                            "Admin-1 unit", 
                            "Age group",
                            "N doses",                 
                            "Clinical cases (no PMC)",             
                            "Clinical cases (no PMC, per 1000)",    
                            "Clinical cases (with PMC)",           
                            "Clinical cases (with PMC, per 1000)",  
                            "Clinical cases averted (with PMC)",   
                            "Clinical cases averted (with PMC, per 1000)",
                            "Clinical cases reduction (%)",          
                            "Severe cases (no PMC)",               
                            "Severe cases (no PMC, per 1000)",      
                            "Severe cases (with PMC)",             
                            "Severe cases (with PMC, per 1000)",    
                            "Severe cases averted (with PMC)",     
                            "Severe cases averted (with PMC, per 1000)",
                            "Severe cases reduction (%)"            
                            
      )
      
      
      #########################################################################
      
      # All health outputs (annual, all admin-1 areas, total age group)
      
      #merged_df_annual_COMPLETE$`Age group` <- rep(c("0-30mo"), times= dim(merged_df_annual_COMPLETE)[1])
      
      merged_df_annual_COMPLETE <- merged_df_annual_COMPLETE %>% filter(`Admin-1 unit` == chosen_area_main_page)
      
      merged_df_annual_COMPLETE[, c("clinical_cases_no_PMC", 
                                    "clinical_cases_no_PMC_per1000", 
                                    "clinical_cases_with_PMC", 
                                    "clinical_cases_with_PMC_per1000", 
                                    "clinical_cases_averted_with_PMC", 
                                    "clinical_cases_averted_with_PMC_per1000", 
                                    "clinical_cases_reduction", 
                                    "severe_cases_no_PMC", 
                                    "severe_cases_no_PMC_per1000", 
                                    "severe_cases_with_PMC", 
                                    "severe_cases_with_PMC_per1000", 
                                    "severe_cases_averted_with_PMC", 
                                    "severe_cases_averted_with_PMC_per1000", 
                                    "severe_cases_reduction" 
                                    )] <- lapply(merged_df_annual_COMPLETE[, c("clinical_cases_no_PMC", 
                                                                                                      "clinical_cases_no_PMC_per1000", 
                                                                                                      "clinical_cases_with_PMC", 
                                                                                                      "clinical_cases_with_PMC_per1000", 
                                                                                                      "clinical_cases_averted_with_PMC", 
                                                                                                      "clinical_cases_averted_with_PMC_per1000", 
                                                                                                      "clinical_cases_reduction", 
                                                                                                      "severe_cases_no_PMC", 
                                                                                                      "severe_cases_no_PMC_per1000", 
                                                                                                      "severe_cases_with_PMC", 
                                                                                                      "severe_cases_with_PMC_per1000", 
                                                                                                      "severe_cases_averted_with_PMC", 
                                                                                                      "severe_cases_averted_with_PMC_per1000", 
                                                                                                      "severe_cases_reduction" 
                                                                                                      )], function(x) round(as.numeric(x), digits = 1))
      
      
      
      annual_health_output_table <- merged_df_annual_COMPLETE[, new_column_order]
      colnames(annual_health_output_table) <- new_column_names
      annual_health_output_table$`Age group` <- rep(c("0-30mo"), times= dim(annual_health_output_table)[1])
      
      main_page_outputs$annual_health_output_table <- data.frame(annual_health_output_table, check.names = FALSE) 
      
      
      #########################################################################
      incProgress(1/10)
      
      # All health outputs (annual, 6 month age groups)
      
      #merged_df_annual_sixmonths_COMPLETE$`Age group` <- rep(c("0-6mo", "6-12mo", "12-18mo", "18-24mo", "24-30mo"), times=dim(merged_df_annual_sixmonths_COMPLETE)[1]/5)
      merged_df_annual_sixmonths_COMPLETE <- merged_df_annual_sixmonths_COMPLETE %>% filter(`Admin-1 unit` == chosen_area_main_page)
      
      merged_df_annual_sixmonths_COMPLETE[, c("clinical_cases_no_PMC", 
                                              "clinical_cases_no_PMC_per1000", 
                                              "clinical_cases_with_PMC", 
                                              "clinical_cases_with_PMC_per1000", 
                                              "clinical_cases_averted_with_PMC", 
                                              "clinical_cases_averted_with_PMC_per1000", 
                                              "clinical_cases_reduction", 
                                              "severe_cases_no_PMC", 
                                              "severe_cases_no_PMC_per1000", 
                                              "severe_cases_with_PMC", 
                                              "severe_cases_with_PMC_per1000", 
                                              "severe_cases_averted_with_PMC", 
                                              "severe_cases_averted_with_PMC_per1000", 
                                              "severe_cases_reduction" 

                                              )] <- lapply(merged_df_annual_sixmonths_COMPLETE[, c("clinical_cases_no_PMC", 
                                                                                                                          "clinical_cases_no_PMC_per1000", 
                                                                                                                          "clinical_cases_with_PMC", 
                                                                                                                          "clinical_cases_with_PMC_per1000", 
                                                                                                                          "clinical_cases_averted_with_PMC", 
                                                                                                                          "clinical_cases_averted_with_PMC_per1000", 
                                                                                                                          "clinical_cases_reduction", 
                                                                                                                          "severe_cases_no_PMC", 
                                                                                                                          "severe_cases_no_PMC_per1000", 
                                                                                                                          "severe_cases_with_PMC", 
                                                                                                                          "severe_cases_with_PMC_per1000", 
                                                                                                                          "severe_cases_averted_with_PMC", 
                                                                                                                          "severe_cases_averted_with_PMC_per1000", 
                                                                                                                          "severe_cases_reduction" 
                                                          
                                                                                                                          )], function(x) round(as.numeric(x), digits = 1))
      
      
      
      annual_health_output_sixmonths_table <-  merged_df_annual_sixmonths_COMPLETE[, new_column_order]
      colnames(annual_health_output_sixmonths_table) <- new_column_names
      annual_health_output_sixmonths_table$`Age group` <- rep(c("0-6mo", "6-12mo", "12-18mo", "18-24mo", "24-30mo"), times=dim(annual_health_output_sixmonths_table)[1]/5)
      
      main_page_outputs$annual_health_output_sixmonths_table <- data.frame(annual_health_output_sixmonths_table, check.names = FALSE) 
      
      #########################################################################
      
      # Cost data 
    
      cost_table <- (cost_data %>% filter(`Iso code` == get_iso3(country_main_page),  `Region (admin-1 unit)`== chosen_area_main_page))[,-c(11:18, 30)]
      
      cost_table$`Iso code` <- country_lookup[cost_table$`Iso code`]
      rename(cost_table, `Country` = `Iso code`)
      
      
      
      main_page_outputs$cost_data_table <- data.frame(cost_table,check.names = FALSE)  
      
      #########################################################################
      
      
      # DALYS averted table 
      
      # dalys_table <- (cost_data %>% filter(`Iso code` == get_iso3(country_main_page),  `Region (admin-1 unit)`== chosen_area_main_page))[,c(1,2,3,4,5,6,15:18)]
      # 
      # 
      # dalys_table <- (cost_data %>% filter(`Iso code` == get_iso3(country_main_page)))[,c(1,2,3,4,5,6,15:18)]
      # 
      
      dalys_table <- cost_data %>%
        filter(`Iso code` == get_iso3(country_main_page),  `Region (admin-1 unit)`== chosen_area_main_page) %>%
        dplyr::select(1, 2, 3, 4, 5, 15:18) %>%
        
        mutate(
          Country = country_lookup[`Iso code`]
        ) %>%
        dplyr::select(-`Iso code`) %>%
        relocate(Country) %>%
        
        group_by(`Region (admin-1 unit)`) %>%
        
        summarise(
          Country = first(Country),
          
          `Total DALYs averted` =
            diff(`Total DALYs`),
          
          `DALYs averted from clinical cases` =
            diff(`DALYs from clinical cases`),
          
          `DALYs averted from hospitalisations` =
            diff(`DALYs from hospitalisations`),
          
          `DALYs averted from deaths` =
            diff(`DALYs from deaths`),
          
          .groups = "drop"
        )
      
      dalys_table <- dalys_table %>% relocate(`Country`)
      # 
      # dalys_table$`Iso code` <- country_lookup[dalys_table$`Iso code`]
      # dalys_table$`Country` <- dalys_table$`Iso code`
      # dalys_table <- dalys_table %>% dplyr::select(-`Iso code`) %>% relocate(`Country`)
      # 
    
      main_page_outputs$dalys_table <- data.frame(dalys_table,check.names = FALSE)   
      
      
      # cost effectiveness thresholds 
      
      cet_table <- (cet_thresholds %>% filter(Name == country_main_page))[,c(1,3,4)]
      names(cet_table) <- c("Country", "Cost-effectiveness threshold (lower)", "Cost-effectiveness threshold (upper)")
      
      main_page_outputs$cet_table <- data.frame(cet_table,check.names = FALSE)
      
      
      ################################################################
      # bar plot
      ################################################################
      
      # Reshape data to long format
      plot_df <- annual_health_output_table %>%
        select(`Admin-1 unit`,
               `Clinical cases (no PMC)`,
               `Clinical cases (with PMC)`) %>%
        pivot_longer(
          cols = c(`Clinical cases (no PMC)`, `Clinical cases (with PMC)`),
          names_to = "Scenario",
          values_to = "Cases"
        ) %>%
        mutate(
          Scenario = recode(Scenario,
                            "Clinical cases (no PMC)" = "No PMC",
                            "Clinical cases (with PMC)" = "PMC co-delivered with EPI")
        )
      
      # Plot: two bars per Admin-1
      bar_plot <- ggplot(plot_df, aes(x = `Admin-1 unit`, y = Cases, fill = Scenario)) +
        geom_col(position = position_dodge(width = 0.7), width = 0.6) +
        labs(
          x = "Admin-1 unit",
          y = "Estimated new clinical cases in age group (0-30mo)",
          fill = "Scenario"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      main_page_outputs$bar_plot <- bar_plot 
      
      
      
      
})
    }
    
    
   
    
    
    
    
    
    
    
    
    
    
    
    
    
    ##### GRAPH FOR WHOLE COUNTRY #####
    
    if (input$country_or_area_main_page == "Whole country") {
      
      withProgress(message = "Preparing results, please wait", {
      
        number_of_doses <- max(unique(whole_country_by_age_COMPLETE$`N doses`))

      # Clinical incidence graph
      # Initialize the ggplot object
      clinical_inc_graphs <- ggplot()
      
      names_in_plot <- c()
      

      # Iterate over columns starting with number_of_doses
      for (i in 1:length(number_of_doses)) {
        
        PMC_impact_ppy <- whole_country_by_age_COMPLETE %>% filter(`N doses` == number_of_doses[i])
        incidence_ppy_df <- incidence_ppy_df_whole_country
        
        # get first 5 letters from each in 
        vaccines_text <- unique(
          (coverage_df_country %>% filter(Country == country_main_page))$vaccines_by_age
        )
        
        vaccines_cov <- unlist(unique(
          (coverage_df_country %>% filter(Country == country_main_page))$avg_coverage_by_age_n
        ))
        
        vaccines_vec <- trimws(unlist(strsplit(vaccines_text, ",")))
        
        vaccines_dosed <- paste0(
          "Dose ",
          seq_along(vaccines_vec),
          ": ",
          vaccines_vec,
          " (",
          round(vaccines_cov * 100,0), 
          "% coverage)",
          collapse = "\n"
        )
        
        legend_label <- paste0(
          unique((coverage_df_country %>% filter(Country == country_main_page))$Scenario),
          ":\n",
          vaccines_dosed
        )
        
        incProgress(1/10)
        
        names_in_plot <- c(names_in_plot, legend_label)
        
        clinical_inc_graphs <- clinical_inc_graphs +
          geom_line(aes_string(x = PMC_impact_ppy$age_in_days_midpoint, 
                               y = 1000 * PMC_impact_ppy$clinical_ppy_with_PMC, 
                               colour = paste0("\"", legend_label, "\"")), 
                    linewidth = 0.6) 
        
        legend_label2 <- paste0(legend_label) #paste0("Cases averted, ", number_of_doses[i], " doses")
        names_in_plot <- c(names_in_plot, legend_label2)
        
        clinical_inc_graphs <- clinical_inc_graphs + 
          geom_ribbon(aes_string(x = PMC_impact_ppy$age_in_days_midpoint, 
                                 ymin = 1000 * PMC_impact_ppy$clinical_ppy_with_PMC, 
                                 ymax = 1000 * incidence_ppy_df$clinical, 
                                 fill = paste0("\"", legend_label2, "\"")), 
                      alpha = 0.3, show.legend = TRUE)
      }
      
      
      clinical_inc_graphs <- clinical_inc_graphs + 
        geom_line(aes_string(x = incidence_ppy_df$age_in_days_midpoint, 
                             y = 1000 * incidence_ppy_df$clinical, 
                             colour = "\"No PMC\""))
      
      
      
      names(clinical_inc_graphs$layers) <- c(names_in_plot, "No PMC")
      
      clinical_inc_graphs <- ggplot() + clinical_inc_graphs$layers +
        labs(x = "Age (months)", y = "New clinical infections per 1000 children", colour = "Incidence by delivery model [select/deselect]", fill = "Cases averted", title = paste0(country_main_page)) +
        ylim(0, max((1000*incidence_ppy_df$clinical)) + 500) +
        
        scale_x_continuous(
          breaks = c(0, 183, 365, 549, 730, 913),
          labels = c("0", "6", "12", "18", "24", "30")
        ) + 
        scale_fill_manual("Cases averted", breaks = names_in_plot[c(seq(from=2, to=length(names_in_plot), by=2))], values = colours[2:5]) +
        scale_color_manual("Incidence by delivery model [select/deselect]", breaks = c("No PMC", names_in_plot[c(seq(from=1, to=length(names_in_plot), by=2))]), values = colours[1:5]) + 
        theme(panel.grid.major = element_line(colour = "gray", linewidth = 0.2), 
              panel.background = element_rect(fill = 'transparent'), 
              plot.background = element_rect(fill = 'transparent', color = NA), 
              legend.background = element_rect(fill = 'transparent')) 
      
      
      clinical_inc_graphs_plotly <- ggplotly(clinical_inc_graphs)
      
      
      for (i in 1:length(clinical_inc_graphs_plotly$x$data)){
        if (!is.null(clinical_inc_graphs_plotly$x$data[[i]]$name)){
          clinical_inc_graphs_plotly$x$data[[i]]$name <- gsub("^\\(", "", clinical_inc_graphs_plotly$x$data[[i]]$name)
          clinical_inc_graphs_plotly$x$data[[i]]$name <- gsub(",1)", "", clinical_inc_graphs_plotly$x$data[[i]]$name)
          
        }
      }
      
      text_x <- paste0("Age: ", clinical_inc_graphs_plotly$x$data[[i]]$x)
      text_y <- paste0("Clinical incidence per 1000: ", round(clinical_inc_graphs_plotly$x$data[[i]]$y, digits=1))
      text_z <- Map(function(x, y) paste0("", x, " days\n", y, ""), text_x, text_y)
      

      
      clinical_inc_graphs_plotly <- clinical_inc_graphs_plotly %>%
        style(hoverinfo = "skip", traces = c(3, 5, 7, 9)) %>% 
        style(text = unlist(text_z))
      
      # Assign the plot to the main_page_outputs
      main_page_outputs$clinical_incidence_graph <- clinical_inc_graphs_plotly

            ##########################################################################
      incProgress(1/10)
      # severe incidence graph 
      
      # Initialize the ggplot object
      severe_inc_graphs <- ggplot()
      
      names_in_plot <- c()
      
      # Iterate over columns starting with number_of_doses
      for (i in 1:length(number_of_doses)) {
        
        PMC_impact_ppy <- whole_country_by_age_COMPLETE %>% filter(`N doses` == number_of_doses[i])
        incidence_ppy_df <- incidence_ppy_df_whole_country
        
        # get first 5 letters from each in 
        vaccines_text <- unique(
          (coverage_df_country %>% filter(Country == country_main_page))$vaccines_by_age
        )
        
        vaccines_cov <- unlist(unique(
          (coverage_df_country %>% filter(Country == country_main_page))$avg_coverage_by_age_n
        ))
        
        vaccines_vec <- trimws(unlist(strsplit(vaccines_text, ",")))
        
        vaccines_dosed <- paste0(
          "Dose ",
          seq_along(vaccines_vec),
          ": ",
          vaccines_vec,
          " (",
          round(vaccines_cov * 100,0), 
          "% coverage)",
          collapse = "\n"
        )
        
        legend_label <- paste0(
          unique((coverage_df_country %>% filter(Country == country_main_page))$Scenario),
          ":\n",
          vaccines_dosed
        )
        
        names_in_plot <- c(names_in_plot, legend_label)
        
        severe_inc_graphs <- severe_inc_graphs +
          geom_line(aes_string(x = PMC_impact_ppy$age_in_days_midpoint, 
                               y = 1000 * PMC_impact_ppy$severe_ppy_with_PMC, 
                               colour = paste0("\"", legend_label, "\"")), 
                    linewidth = 0.6) 
        
        legend_label2 <- paste0(legend_label) #paste0("Cases averted, ", number_of_doses[i], " doses")
        names_in_plot <- c(names_in_plot, legend_label2)
        
        severe_inc_graphs <- severe_inc_graphs + 
          geom_ribbon(aes_string(x = PMC_impact_ppy$age_in_days_midpoint, 
                                 ymin = 1000 * PMC_impact_ppy$severe_ppy_with_PMC, 
                                 ymax = 1000 * incidence_ppy_df$severe, 
                                 fill = paste0("\"", legend_label2, "\"")), 
                      alpha = 0.3, show.legend = TRUE)
      }
      
      
      severe_inc_graphs <- severe_inc_graphs + 
        geom_line(aes_string(x = incidence_ppy_df$age_in_days_midpoint, 
                             y = 1000 * incidence_ppy_df$severe, 
                             colour = "\"No PMC\""))
      
      
      
      names(severe_inc_graphs$layers) <- c(names_in_plot, "No PMC")
      
      severe_inc_graphs <- ggplot() + severe_inc_graphs$layers +
        labs(x = "Age (months)", y = "New hospitalisations per 1000 children", colour = "Hospitalisations by delivery model [select/deselect]", fill = "Hospitalisations averted", title = paste0(country_main_page)) +
        ylim(0, max((1000*incidence_ppy_df$severe)) + 100) +
        
        scale_x_continuous(
          breaks = c(0, 183, 365, 549, 730, 913),
          labels = c("0", "6", "12", "18", "24", "30")
        ) + 
        scale_fill_manual("Hospitalisations averted", breaks = names_in_plot[c(seq(from=2, to=length(names_in_plot), by=2))], values = colours[2:5]) +
        scale_color_manual("Hospitalisations by delivery model [select/deselect]", breaks = c("No PMC", names_in_plot[c(seq(from=1, to=length(names_in_plot), by=2))]), values = colours[1:5]) + 
        theme(panel.grid.major = element_line(colour = "gray", linewidth = 0.2), 
              panel.background = element_rect(fill = 'transparent'), 
              plot.background = element_rect(fill = 'transparent', color = NA), 
              legend.background = element_rect(fill = 'transparent')) 
      
      
      severe_inc_graphs_plotly <- ggplotly(severe_inc_graphs)
      incProgress(1/10)
      
      
      for (i in 1:length(severe_inc_graphs_plotly$x$data)){
        if (!is.null(severe_inc_graphs_plotly$x$data[[i]]$name)){
          severe_inc_graphs_plotly$x$data[[i]]$name <- gsub("^\\(", "", severe_inc_graphs_plotly$x$data[[i]]$name)
          severe_inc_graphs_plotly$x$data[[i]]$name <- gsub(",1)", "", severe_inc_graphs_plotly$x$data[[i]]$name)
          
        }
      }
      
      text_x <- paste0("Age: ", severe_inc_graphs_plotly$x$data[[i]]$x)
      text_y <- paste0("Hospitalisations per 1000: ", round(severe_inc_graphs_plotly$x$data[[i]]$y, digits=1))
      text_z <- Map(function(x, y) paste0("", x, " days\n", y, ""), text_x, text_y)
      
      
      severe_inc_graphs_plotly <- severe_inc_graphs_plotly %>%
        style(hoverinfo = "skip", traces = c(3, 5, 7, 9)) %>% 
        style(text = unlist(text_z))
      
      # Assign the plot to the main_page_outputs
      main_page_outputs$severe_incidence_graph <- severe_inc_graphs_plotly
      
      #########################################################################

      dosing_schedules <- data.frame(Dose = paste0("Dose ",seq_along(vaccines_vec)), 
                                     `Co-delivered intervention` = vaccines_vec, 
                                     `Coverage (%)` = paste0(round(vaccines_cov * 100,0), "%"),
                                     check.names=FALSE)
      
      main_page_outputs$dosing_schedules <- dosing_schedules
      
      
      incProgress(1/10)
      # Haplotype data table
      

      haplotype_data_df <- haplotype_data_final %>% filter(Country == country_main_page)
      
      haplotype_data_df <- haplotype_data_df[c("Country", "Admin-1 unit",  "latitude", "longitude", "I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other")]
      
      colnames(haplotype_data_df) <- c("Country", "Admin-1 unit",  "latitude", "longitude", "I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other")
      
      haplotype_data_df[, c("I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other")] <- lapply(haplotype_data_df[, c( "I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other")], function(x) round(as.numeric(x), digits = 3))
      
      for (i in 1:dim(haplotype_data_df)[1]) {
        haplotype_data_df[i, "Sum of proportions"] <- sum(unlist(c(haplotype_data_df[i, "I_AKA_"], haplotype_data_df[i, "I_GKA_"], haplotype_data_df[i, "I_GEA_"], haplotype_data_df[i, "I_GEG_"], haplotype_data_df[i, "V_GKA_"], haplotype_data_df[i, "V_GKG_"], haplotype_data_df[i, "Other"])), na.rm = TRUE)
      }
      
      haplotype_data_df$`Admin-1 unit` <- gsub("_", " ", haplotype_data_df$`Admin-1 unit`)
      main_page_outputs$haplotype_table <- haplotype_data_df[, c("Country", "Admin-1 unit", "I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other")]
      
      ##########################################################################
      

      # Haplotype map 
      # 
      # names(adm1)[names(adm1) == "NAME_2"] <- "Region"
      # adm1$NAME_2 <- adm1$Region
      # 
      # 
      # haplotype_proportions_map <- ggplot(adm1) + 
      #   geom_sf() + 
      #   theme_bw() +
      #   theme(
      #     legend.text = element_text(size = 12),
      #     legend.title = element_text(size = 15),
      #     title = element_text(size = 20)
      #   ) +
      #   scatterpie::geom_scatterpie(
      #     data = haplotype_data_df,
      #     cols = c("I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other"),
      #     aes(x = longitude, y = latitude, group = Region, r = 0.13 * min(diff(range(haplotype_data_df$longitude)), diff(range(haplotype_data_df$latitude)))) # Reduced radius from 0.8 to 0.3
      #   ) +
      #   scale_fill_manual(
      #     values = c(  "#00C094", "#00B6EB", "#FFA500","#F8766D","#b7a1ff", "#7361b3","#D2B48C"),
      #     name = "Haplotypes",
      #     breaks = c("I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other"),
      #     labels = c("I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other")
      #   ) +
      #   theme(
      #     axis.title.x = element_blank(),
      #     axis.text.x = element_blank(),
      #     axis.ticks.x = element_blank(),
      #     axis.title.y = element_blank(),
      #     axis.text.y = element_blank(),
      #     axis.ticks.y = element_blank(),
      #     panel.background = element_blank(),
      #     panel.border = element_blank(),
      #     panel.grid.major = element_blank(),
      #     panel.grid.minor = element_blank(),
      #     plot.background = element_blank()
      #   )
      # 

      adm1$NAME_2 <- stri_trans_general(str = gsub(" ", "_", adm1$NAME_1), id = "Latin-ASCII")
      adm1$NAME_2 <- stri_trans_general(str = gsub("-", "_", adm1$NAME_2), id = "Latin-ASCII")
      
      adm1$`Admin-1 unit` <- adm1$NAME_2
      combined_protective_efficacy_30_days <- merge(protective_efficacy_30_days %>% filter(Country == country_main_page), adm1, by="Admin-1 unit") 
      
      
      haplotype_proportions_map <- ggplot(combined_protective_efficacy_30_days) + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
        theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
        geom_sf(data=adm1)+
        geom_sf(aes(fill=day_30_protect_efficacy, geometry=geometry), size=0.5)  +
        geom_sf_label(aes(label = paste0(`Admin-1 unit`), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
        ggtitle(paste0("Average 30 day protective efficacy following a SP dose: ", country_main_page)) + labs(fill="30 day protective efficacy") +
        scale_fill_viridis_c(limits=c(min(combined_protective_efficacy_30_days$day_30_protect_efficacy),max(combined_protective_efficacy_30_days$day_30_protect_efficacy))) + 
        theme(text=element_text(size=14),legend.title =element_text(size=14), legend.text = element_text(size=14),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank())
      

      main_page_outputs$haplotype_map <- haplotype_proportions_map 

      
      #########################################################################
      incProgress(1/10)
      # Probability of drug protection
    
      # Initialize the ggplot object
      prob_drug_protect_plot <- ggplot()
      names_in_plot <- c()
      
      # Iterate over columns starting with 'prot_overall_'
      for (col in names(df_drug_protect)) {
        if (startsWith(col, 'prot_overall_')) {
          
          legend_label <- gsub('prot_overall_', '', col)
          legend_label <- gsub('_', ' ', legend_label)
          names_in_plot <- c(names_in_plot, legend_label)
          
          # Create geom_line for each column
          print(legend_label)
          prob_drug_protect_plot <- prob_drug_protect_plot + geom_line(aes_string(x = df_drug_protect$time, y = paste0("df_drug_protect[[\"", col, "\"]]"), colour = paste0("\"", legend_label, "\"")), linewidth=1)
          
        }
        
        
      }
      
      # Customize legend and labels
      prob_drug_protect_plot <- prob_drug_protect_plot + 
        labs(x = "Days since SP dose", y = "Probability of drug protection", colour= "Admin-1 unit",
             title = "Probability of drug protection following an SP dose") +
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 14),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 14))
      
      names(prob_drug_protect_plot$layers) <- names_in_plot
      
    
      main_page_outputs$prob_drug_protect_graph <- ggplotly(prob_drug_protect_plot) 
      
    
      prob_drug_protect_plotly <- main_page_outputs$prob_drug_protect_graph
      
      
      for (i in 1:length(prob_drug_protect_plotly$x$data)) {
        
        if (!is.null(prob_drug_protect_plotly$x$data[[i]]$name)) {
          
          prob_drug_protect_plotly$x$data[[i]]$name <-
            gsub("^\\(", "", prob_drug_protect_plotly$x$data[[i]]$name)
          
          prob_drug_protect_plotly$x$data[[i]]$name <-
            gsub(",1\\)$", "", prob_drug_protect_plotly$x$data[[i]]$name)
          
        }
      }
      
      for (i in 1:length(prob_drug_protect_plotly$x$data)) {
        
        # Build hover text
        text_x <- paste0(
          "Days since SP dose: ",
          prob_drug_protect_plotly$x$data[[i]]$x
        )
        
        text_y <- paste0(
          "Probability of protection: ",
          round(prob_drug_protect_plotly$x$data[[i]]$y, 3)
        )
        
        text_xy <- Map(
          function(x, y) paste0(x, "\n", y),
          text_x,
          text_y
        )
        
        # Attach hover text to trace
        prob_drug_protect_plotly$x$data[[i]]$text <- unlist(text_xy)
        prob_drug_protect_plotly$x$data[[i]]$hoverinfo <- "text"
      }
      
      main_page_outputs$prob_drug_protect_graph <-
        prob_drug_protect_plotly
      
      #########################################################################
      
      
      adm1$`Region (admin-1)` <- adm1$NAME_2
      
      
      final_ranked_table_full <- final_ranked_table_full %>%
        mutate(
          `Rank of PMC option by cost-effectiveness` = row_number()
        )
      
      adm1_merged <- left_join(adm1, final_ranked_table_full, by = "Region (admin-1)")
      
      
      #adm1_merged <- sf::st_make_valid(adm1_merged)
      #adm1 <- sf::st_make_valid(adm1)
      
      # Extract values
      icer_vals <- adm1_merged$`ICER based on regional costs only (no national costs)`
      
      min_val <- floor(min(icer_vals, na.rm = TRUE) / 10) * 10
      max_val <- ceiling(max(icer_vals, na.rm = TRUE) / 10) * 10
      mid_val <- round((min_val + max_val) / 2)
      
      icer_map <- ggplot(adm1_merged) +
        geom_sf(data = adm1) +
        geom_sf(aes(fill = `ICER based on regional costs only (no national costs)`,
                    geometry = geometry),
                size = 0.5) +
        geom_sf_label(
          aes(label = paste0("Rank: ", `Rank of PMC option by cost-effectiveness` ),
              geometry = geometry),
          fun.geometry = sf::st_point_on_surface,
          size = 3,
          alpha = 0.2
        ) +
        theme_void() +
        theme(
          legend.title = element_text(hjust = 0.5)
        ) +
        scale_fill_gradient2(
          low = "#008631",
          mid = "#cefad0",
          high = "#ffffff",
          midpoint = mid_val,
          limits = c(min_val, max_val),   # IMPORTANT
          breaks = c(min_val, mid_val, max_val),
          labels = c(
            paste0(comma(min_val), " (most cost-effective, rank=1)"),
            comma(mid_val),
            paste0(comma(max_val), " (least cost-effective, rank=", max(adm1_merged$`Rank of PMC option by cost-effectiveness` ), ")")
          ),
          guide = guide_colorbar(reverse = TRUE)
        ) +
        labs(fill = "Cost-effectiveness\n(incremental cost-effectiveness ratio)\n") + 
        theme(text=element_text(size=14),legend.title =element_text(size=14), legend.text = element_text(size=14))
      
      
      
      main_page_outputs$icer_map <- icer_map
      
      
      # Prioritisation table 
  
      final_ranked_table_full <- final_ranked_table_full %>% filter(Country==get_iso3(country_main_page))
      final_ranked_table_full$Country <- country_lookup[final_ranked_table_full$Country] 
      final_ranked_table_full <- final_ranked_table_full %>% dplyr::select(-c( `ICER based on regional costs only (no national costs)`, `Cost-effective? (lower country-specific threshold)`, `Cost-effective? (higher country-specific threshold)`, `Cost-effective? ($250 / DALY averted threshold)`)) %>% 
        rename("Financial (budget) cost of SP+consumables" = "Financial cost of SP and administration consumables" ) %>% 
        rename("Cumulative financial (budget) cost of SP+consumables" = "Cumulative cost of SP and administration consumables")  %>%
        relocate(`Rank of PMC option by cost-effectiveness`, .after = 2)
      
      # final_ranked_table$`Cumulative economic costs of PMC implementation` <- cumsum(final_ranked_table$`Additional economic costs of PMC implementation`)
      # final_ranked_table$`Cumulative treatment cost savings (economic) to public providers` <- cumsum(final_ranked_table$`Additional economic cost savings to public providers from reduced treatment`)
      # final_ranked_table$`Cumulative economic costs of implementation and treatment` <- cumsum(final_ranked_table$`Additional economic costs of PMC implementation` + final_ranked_table$`Additional economic cost savings to public providers from reduced treatment`)
      # 
      
      main_page_outputs$prioritisation_table <- data.frame(final_ranked_table_full, check.names = FALSE) 
      
      #########################################################################
      incProgress(1/10)
      
      
      
      # All health outputs 
      
      # Define the new order of columns
      new_column_order <- c("Country", 
                            "Admin-1 unit", 
                            "Age group", 
                            "N doses", 
                            "clinical_cases_no_PMC", 
                            "clinical_cases_no_PMC_per1000", 
                            "clinical_cases_with_PMC", 
                            "clinical_cases_with_PMC_per1000", 
                            "clinical_cases_averted_with_PMC", 
                            "clinical_cases_averted_with_PMC_per1000", 
                            "clinical_cases_reduction", 
                            "severe_cases_no_PMC", 
                            "severe_cases_no_PMC_per1000", 
                            "severe_cases_with_PMC", 
                            "severe_cases_with_PMC_per1000", 
                            "severe_cases_averted_with_PMC", 
                            "severe_cases_averted_with_PMC_per1000", 
                            "severe_cases_reduction" 

                            )
      
      new_column_names <- c("Country",                          
                            "Admin-1 unit", 
                            "Age group",
                            "N doses",                 
                            "Clinical cases (no PMC)",             
                            "Clinical cases (no PMC, per 1000)",    
                            "Clinical cases (with PMC)",           
                            "Clinical cases (with PMC, per 1000)",  
                            "Clinical cases averted (with PMC)",   
                            "Clinical cases averted (with PMC, per 1000)",
                            "Clinical cases reduction (%)",          
                            "Severe cases (no PMC)",               
                            "Severe cases (no PMC, per 1000)",      
                            "Severe cases (with PMC)",             
                            "Severe cases (with PMC, per 1000)",    
                            "Severe cases averted (with PMC)",     
                            "Severe cases averted (with PMC, per 1000)",
                            "Severe cases reduction (%)"            

                            )
      
      
      #########################################################################
      
      # All health outputs (annual, all admin-1 areas, total age group)

      #merged_df_annual_COMPLETE$`Age group` <- rep(c("0-30mo"), times= dim(merged_df_annual_COMPLETE)[1])
      merged_df_annual_COMPLETE[, c("clinical_cases_no_PMC", 
                                    "clinical_cases_no_PMC_per1000", 
                                    "clinical_cases_with_PMC", 
                                    "clinical_cases_with_PMC_per1000", 
                                    "clinical_cases_averted_with_PMC", 
                                    "clinical_cases_averted_with_PMC_per1000", 
                                    "clinical_cases_reduction", 
                                    "severe_cases_no_PMC", 
                                    "severe_cases_no_PMC_per1000", 
                                    "severe_cases_with_PMC", 
                                    "severe_cases_with_PMC_per1000", 
                                    "severe_cases_averted_with_PMC", 
                                    "severe_cases_averted_with_PMC_per1000", 
                                    "severe_cases_reduction" 

                                    )] <- lapply(merged_df_annual_COMPLETE[, c("clinical_cases_no_PMC", 
                                                                                                      "clinical_cases_no_PMC_per1000", 
                                                                                                      "clinical_cases_with_PMC", 
                                                                                                      "clinical_cases_with_PMC_per1000", 
                                                                                                      "clinical_cases_averted_with_PMC", 
                                                                                                      "clinical_cases_averted_with_PMC_per1000", 
                                                                                                      "clinical_cases_reduction", 
                                                                                                      "severe_cases_no_PMC", 
                                                                                                      "severe_cases_no_PMC_per1000", 
                                                                                                      "severe_cases_with_PMC", 
                                                                                                      "severe_cases_with_PMC_per1000", 
                                                                                                      "severe_cases_averted_with_PMC", 
                                                                                                      "severe_cases_averted_with_PMC_per1000", 
                                                                                                      "severe_cases_reduction" 
               
                                                                               )], function(x) round(as.numeric(x), digits = 1))
      
      annual_health_output_table <- (merged_df_annual_COMPLETE %>% dplyr::select(!units))[, new_column_order]
      colnames(annual_health_output_table) <- new_column_names
      annual_health_output_table$`Age group` <- rep(c("0-30mo"), times= dim(annual_health_output_table)[1])
      main_page_outputs$annual_health_output_table <- data.frame(annual_health_output_table, check.names = FALSE) 
      
      
      #########################################################################
      
      # All health outputs (annual, 6 month age groups)
      
      #merged_df_annual_sixmonths_COMPLETE$`Age group` <- rep(c("0-6mo", "6-12mo", "12-18mo", "18-24mo", "24-30mo"), times=dim(merged_df_annual_sixmonths_COMPLETE)[1]/5)
      
      merged_df_annual_sixmonths_COMPLETE[, c("clinical_cases_no_PMC", 
                                    "clinical_cases_no_PMC_per1000", 
                                    "clinical_cases_with_PMC", 
                                    "clinical_cases_with_PMC_per1000", 
                                    "clinical_cases_averted_with_PMC", 
                                    "clinical_cases_averted_with_PMC_per1000", 
                                    "clinical_cases_reduction", 
                                    "severe_cases_no_PMC", 
                                    "severe_cases_no_PMC_per1000", 
                                    "severe_cases_with_PMC", 
                                    "severe_cases_with_PMC_per1000", 
                                    "severe_cases_averted_with_PMC", 
                                    "severe_cases_averted_with_PMC_per1000", 
                                    "severe_cases_reduction" 

                                    )] <- lapply(merged_df_annual_sixmonths_COMPLETE[, c("clinical_cases_no_PMC", 
                                                                                                      "clinical_cases_no_PMC_per1000", 
                                                                                                      "clinical_cases_with_PMC", 
                                                                                                      "clinical_cases_with_PMC_per1000", 
                                                                                                      "clinical_cases_averted_with_PMC", 
                                                                                                      "clinical_cases_averted_with_PMC_per1000", 
                                                                                                      "clinical_cases_reduction", 
                                                                                                      "severe_cases_no_PMC", 
                                                                                                      "severe_cases_no_PMC_per1000", 
                                                                                                      "severe_cases_with_PMC", 
                                                                                                      "severe_cases_with_PMC_per1000", 
                                                                                                      "severe_cases_averted_with_PMC", 
                                                                                                      "severe_cases_averted_with_PMC_per1000", 
                                                                                                      "severe_cases_reduction" 
 
                                                                                         )], function(x) round(as.numeric(x), digits = 1))
      
      
      
      annual_health_output_sixmonths_table <-  merged_df_annual_sixmonths_COMPLETE[, new_column_order]
      colnames(annual_health_output_sixmonths_table) <- new_column_names
      annual_health_output_sixmonths_table$`Age group` <- rep(c("0-6mo", "6-12mo", "12-18mo", "18-24mo", "24-30mo"), times=dim(annual_health_output_sixmonths_table)[1]/5)
      main_page_outputs$annual_health_output_sixmonths_table <- data.frame(annual_health_output_sixmonths_table, check.names = FALSE) 
      incProgress(1/10)
      
      #########################################################################
      
      # All health outputs (annual, whole country)
      
      #whole_country_COMPLETE$`Age group` <- rep(c("0-30mo"), times= dim(whole_country_COMPLETE)[1])
      whole_country_COMPLETE[, c("clinical_cases_no_PMC", 
                                    "clinical_cases_no_PMC_per1000", 
                                    "clinical_cases_with_PMC", 
                                    "clinical_cases_with_PMC_per1000", 
                                    "clinical_cases_averted_with_PMC", 
                                    "clinical_cases_averted_with_PMC_per1000", 
                                    "clinical_cases_reduction", 
                                    "severe_cases_no_PMC", 
                                    "severe_cases_no_PMC_per1000", 
                                    "severe_cases_with_PMC", 
                                    "severe_cases_with_PMC_per1000", 
                                    "severe_cases_averted_with_PMC", 
                                    "severe_cases_averted_with_PMC_per1000", 
                                    "severe_cases_reduction" 

                                 )] <- lapply(whole_country_COMPLETE[, c("clinical_cases_no_PMC", 
                                                                                                      "clinical_cases_no_PMC_per1000", 
                                                                                                      "clinical_cases_with_PMC", 
                                                                                                      "clinical_cases_with_PMC_per1000", 
                                                                                                      "clinical_cases_averted_with_PMC", 
                                                                                                      "clinical_cases_averted_with_PMC_per1000", 
                                                                                                      "clinical_cases_reduction", 
                                                                                                      "severe_cases_no_PMC", 
                                                                                                      "severe_cases_no_PMC_per1000", 
                                                                                                      "severe_cases_with_PMC", 
                                                                                                      "severe_cases_with_PMC_per1000", 
                                                                                                      "severe_cases_averted_with_PMC", 
                                                                                                      "severe_cases_averted_with_PMC_per1000", 
                                                                                                      "severe_cases_reduction" 
            
                                                                         )], function(x) round(as.numeric(x), digits = 1))
      
      annual_health_output_whole_country_table <- whole_country_COMPLETE[, new_column_order[-(which(new_column_order == "Admin-1 unit"))]]
      colnames(annual_health_output_whole_country_table) <- new_column_names[-(which(new_column_order == "Admin-1 unit"))]
      annual_health_output_whole_country_table$`Age group` <- rep(c("0-30mo"), times= dim(annual_health_output_whole_country_table)[1])      
      main_page_outputs$annual_health_output_whole_country_table <- data.frame(annual_health_output_whole_country_table, check.names = FALSE) 
      

      #########################################################################
      
      # Cost data 
      cost_table <- (cost_data %>% filter(`Iso code` == get_iso3(country_main_page)))[,-c(11:18, 30)]
      
      cost_table$`Iso code` <- country_lookup[cost_table$`Iso code`]
      rename(cost_table, `Country` = `Iso code`)
      
      main_page_outputs$cost_data_table <- data.frame(cost_table,check.names = FALSE)  
      
      
      # DALYS table 
      
      #dalys_table <- (cost_data %>% filter(`Iso code` == get_iso3(country_main_page)))[,c(1,2,3,4,5,6,15:18)]
      
      
      dalys_table <- cost_data %>%
        filter(`Iso code` == get_iso3(country_main_page)) %>%
        dplyr::select(1, 2, 3, 4, 5, 15:18) %>%
        
        mutate(
          Country = country_lookup[`Iso code`]
        ) %>%
        dplyr::select(-`Iso code`) %>%
        relocate(Country) %>%
        
        group_by(`Region (admin-1 unit)`) %>%
        
        summarise(
          Country = first(Country),
          
          `Total DALYs averted` =
            diff(`Total DALYs`),
          
          `DALYs averted from clinical cases` =
            diff(`DALYs from clinical cases`),
          
          `DALYs averted from hospitalisations` =
            diff(`DALYs from hospitalisations`),
          
          `DALYs averted from deaths` =
            diff(`DALYs from deaths`),
          
          .groups = "drop"
        )
      
      dalys_table <- dalys_table %>% relocate(`Country`)
      
      
      # dalys_table$`Iso code` <- country_lookup[dalys_table$`Iso code`]
      # dalys_table$`Country` <- dalys_table$`Iso code`
      # dalys_table <- dalys_table %>% dplyr::select(-`Iso code`) %>% relocate(`Country`)
      
      
      
      main_page_outputs$dalys_table <- data.frame(dalys_table,check.names = FALSE)     
      
      
      #########################################################################
      
      
      # cost effectiveness thresholds 
    
      
      cet_table <- (cet_thresholds %>% filter(Name == country_main_page))[,c(1,3,4)]
      names(cet_table) <- c("Country", "Cost-effectiveness threshold (lower)", "Cost-effectiveness threshold (upper)")
      
      main_page_outputs$cet_table <- data.frame(cet_table, check.names = FALSE)  
      
      
      #########################################################################
      
      # Impact on clinical cases  map
      
      adm1$`Admin-1 unit` <- adm1$NAME_2
    
      # link data with shapefile
      combined_df_with_PMC_reduction <- merge(merged_df_annual_COMPLETE %>% filter(`N doses` == number_of_doses), adm1, by="Admin-1 unit") 

      clinical_impact_graph_with_PMC <- ggplot(combined_df_with_PMC_reduction) + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
        theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
        geom_sf(data=adm1)+
        geom_sf(aes(fill=clinical_cases_reduction, geometry=geometry), size=0.5)  +
        geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
        ggtitle(paste0(length(unique(schedule)), " DOSE")) + labs(fill="% reduction in\nclinical cases") +
        scale_fill_viridis_c(limits=c(min(combined_df_with_PMC_reduction$clinical_cases_reduction),max(combined_df_with_PMC_reduction$clinical_cases_reduction))) + 
        theme(text=element_text(size=14),legend.title =element_text(size=14), legend.text = element_text(size=14),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank())
      
      main_page_outputs$clinical_reduction_map <- clinical_impact_graph_with_PMC
      
      incProgress(1/10)
      
      ##########################################################################
      
      # Impact on hospitalisations map 
      
      severe_impact_graph_with_PMC <- ggplot(combined_df_with_PMC_reduction) + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
        theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
        geom_sf(data=adm1)+
        geom_sf(aes(fill=severe_cases_reduction, geometry=geometry), size=0.5)  +
        geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
        ggtitle(paste0(length(schedule), " DOSE")) + labs(fill="% reduction in\nhospitalisations") +
        scale_fill_viridis_c(limits=c(min(combined_df_with_PMC_reduction$severe_cases_reduction),max(combined_df_with_PMC_reduction$severe_cases_reduction))) + 
        theme(text=element_text(size=14),legend.title =element_text(size=14), legend.text = element_text(size=14),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank())
      
      main_page_outputs$severe_reduction_map <- severe_impact_graph_with_PMC
      
      
      #########################################################################
      
      # Impact on clinical cases map (cases averted)
      incProgress(1/10)
      

      
      #scale_fill_viridis_c(limits=c(65,215)) + 

      clinical_cases_averted_graph_with_PMC <- ggplot(combined_df_with_PMC_reduction) + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
        theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
        geom_sf(data=adm1)+
        geom_sf(aes(fill=clinical_cases_averted_with_PMC_per1000, geometry=geometry), size=0.5)  +
        #geom_sf(data = subset(adm1, (NAME_2 %in% c("Nampula"))), fill = "grey", color = "black", size = 0.5) +
        geom_sf_label(aes(label = paste0(NAME_2), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
        ggtitle(paste0(length(schedule), " DOSE")) + labs(fill="Cases averted per 1000") +
        scale_fill_viridis_c(limits=c(min(combined_df_with_PMC_reduction$clinical_cases_averted_with_PMC_per1000),max(combined_df_with_PMC_reduction$clinical_cases_averted_with_PMC_per1000))) + 
        #scale_fill_viridis_c(limits=c(10,300)) + 
        theme(text=element_text(size=14),legend.title =element_text(size=14), legend.text = element_text(size=14),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank())
      
      main_page_outputs$clinical_cases_averted_map <- clinical_cases_averted_graph_with_PMC    
      

      #########################################################################
      
      # Impact on severe cases map (cases averted)
      
      
      severe_cases_averted_graph_with_PMC <- ggplot(combined_df_with_PMC_reduction) + theme_bw()+ theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
        theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
        geom_sf(data=adm1)+
        geom_sf(aes(fill=severe_cases_averted_with_PMC_per1000, geometry=geometry), size=0.5)  +
        geom_sf_label(aes(label = paste0(NAME_2), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
        ggtitle(paste0(length(schedule), " DOSE")) + labs(fill="Cases averted per 1000") +
        scale_fill_viridis_c(limits=c(min(combined_df_with_PMC_reduction$severe_cases_averted_with_PMC_per1000),max(combined_df_with_PMC_reduction$severe_cases_averted_with_PMC_per1000))) + 
        theme(text=element_text(size=14),legend.title =element_text(size=14), legend.text = element_text(size=14),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank())
      
      main_page_outputs$severe_cases_averted_map <- severe_cases_averted_graph_with_PMC    
      
      ################################################################
      # bar plot
      ################################################################
      
      # Reshape data to long format
      plot_df <- annual_health_output_table %>%
        select(`Admin-1 unit`,
               `Clinical cases (no PMC)`,
               `Clinical cases (with PMC)`) %>%
        pivot_longer(
          cols = c(`Clinical cases (no PMC)`, `Clinical cases (with PMC)`),
          names_to = "Scenario",
          values_to = "Cases"
        ) %>%
        mutate(
          Scenario = recode(Scenario,
                            "Clinical cases (no PMC)" = "No PMC",
                            "Clinical cases (with PMC)" = "PMC co-delivered with EPI")
        )
      
      # Plot: two bars per Admin-1
      bar_plot <- ggplot(plot_df, aes(x = `Admin-1 unit`, y = Cases, fill = Scenario)) +
        geom_col(position = position_dodge(width = 0.7), width = 0.6) +
        labs(
          x = "Admin-1 unit",
          y = "Estimated new clinical cases in age group (0-30mo)",
          fill = "Scenario"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      main_page_outputs$bar_plot <- bar_plot 
      
      })
    
    }
    
    return(main_page_outputs)
    
  })
  
  output$clinical_incidence_graph_main_page <- renderPlotly({
    main_page_output_generation()$clinical_incidence_graph
  })
  
 
  
  # render table for dhps haplotype frequencies
  output$haplotype_table_main_page <- DT::renderDT(DT::datatable(main_page_output_generation()$haplotype_table,
                                                                    filter = "top", options = list(dom = 'Bftsp', paging = TRUE, pageLength = 10, ordering = TRUE, searching = TRUE, fixedColumns = TRUE, scrollX = TRUE, autoWidth = TRUE, responsive = TRUE, rownames = FALSE, escape = FALSE)))
  
  
  # render map of dhps haplotype frequencies 
  output$haplotype_map_main_page <- renderPlot({
    main_page_output_generation()$haplotype_map
  })
  
  output$prob_drug_protect_graph_main_page <- renderPlotly({
    main_page_output_generation()$prob_drug_protect_graph
  })
  
  output$severe_incidence_graph_main_page <- renderPlotly({
    main_page_output_generation()$severe_incidence_graph
  })
  
  
  # Common DT options for compact, information-dense tables
  dt_options_compact <- list(
    dom = 'Bftsp',
    paging = TRUE,
    pageLength = 25,
    lengthMenu = c(10, 25, 50, 100),
    ordering = TRUE,
    searching = TRUE,
    fixedColumns = TRUE,
    scrollX = TRUE,
    autoWidth = TRUE,
    responsive = TRUE
  )
  
  # % reduction in clinical incidence by 6-month age groups
  output$prioritisation_table_main_page <- DT::renderDT(
    DT::datatable(
      main_page_output_generation()$prioritisation_table,
      filter = "top",
      options = dt_options_compact,
      rownames = FALSE,
      escape = FALSE,
      class = "compact stripe hover"
    )
  )
  
  # Health impact (total age group, all admin1 areas, annual)
  output$annual_health_output_table_main_page <- DT::renderDT(
    DT::datatable(
      main_page_output_generation()$annual_health_output_table,
      filter = "top",
      options = dt_options_compact,
      rownames = FALSE,
      escape = FALSE,
      class = "compact stripe hover"
    )
  )
  
  # Health impact (6-month age groups)
  output$annual_health_output_sixmonths_table_main_page <- DT::renderDT(
    DT::datatable(
      main_page_output_generation()$annual_health_output_sixmonths_table,
      filter = "top",
      options = dt_options_compact,
      rownames = FALSE,
      escape = FALSE,
      class = "compact stripe hover"
    )
  )
  
  # Health impact (whole country summary)
  output$annual_health_output_whole_country_table_main_page <- DT::renderDT(
    DT::datatable(
      main_page_output_generation()$annual_health_output_whole_country_table,
      filter = "top",
      options = dt_options_compact,
      rownames = FALSE,
      escape = FALSE,
      class = "compact stripe hover"
    )
  )
  
  output$dosing_schedules_main_page <- DT::renderDT(
    DT::datatable(
      main_page_output_generation()$dosing_schedules,
      rownames = FALSE,
      filter = "top",
      options = list(
        dom = "ftip",
        paging = FALSE,
        ordering = TRUE,
        searching = TRUE,
        scrollX = FALSE,
        autoWidth = TRUE,
        responsive = FALSE
      )
    )
  )
  
  # # render table for % reduction in clinical incidence by 6 month age groups
  # output$prioritisation_table_main_page <- DT::renderDT(DT::datatable(main_page_output_generation()$prioritisation_table,
  #                                                                          filter = "top", options = list(dom = 'Bftsp', paging = TRUE, pageLength = 10, ordering = TRUE, searching = TRUE, fixedColumns = TRUE, scrollX = TRUE, autoWidth = TRUE, responsive = TRUE, rownames = FALSE, escape = FALSE)))
  # 
  # 
  # # render table for health impact (total age group, all admin1 areas, annual)
  # output$annual_health_output_table_main_page <- DT::renderDT(DT::datatable(main_page_output_generation()$annual_health_output_table,
  #                                                                filter = "top", options = list(dom = 'Bftsp', paging = TRUE, pageLength = 10, ordering = TRUE, searching = TRUE, fixedColumns = TRUE, scrollX = TRUE, autoWidth = TRUE, responsive = TRUE, rownames = FALSE, escape = FALSE)))
  # 
  # 
  # # render table for health impact (total age group, all admin1 areas, annual)
  # output$annual_health_output_sixmonths_table_main_page <- DT::renderDT(DT::datatable(main_page_output_generation()$annual_health_output_sixmonths_table,
  #                                                                 filter = "top", options = list(dom = 'Bftsp', paging = TRUE, pageLength = 10, ordering = TRUE, searching = TRUE, fixedColumns = TRUE, scrollX = TRUE, autoWidth = TRUE, responsive = TRUE, rownames = FALSE, escape = FALSE)))
  # 
  # 
  # # render table for health impact (total age group, all admin1 areas, annual)
  # output$annual_health_output_whole_country_table_main_page <- DT::renderDT(DT::datatable(main_page_output_generation()$annual_health_output_whole_country_table,
  #                                                                 filter = "top", options = list(dom = 'Bftsp', paging = TRUE, pageLength = 10, ordering = TRUE, searching = TRUE, fixedColumns = TRUE, scrollX = TRUE, autoWidth = TRUE, responsive = TRUE, rownames = FALSE, escape = FALSE)))
  # 
  # 
  # render table for cost data
  # output$cost_data_table_main_page <- DT::renderDT(DT::datatable(main_page_output_generation()$cost_data_table,
  #                                                                                         filter = "top", options = list(dom = 'Bftsp', paging = TRUE, pageLength = 10, ordering = TRUE, searching = TRUE, fixedColumns = TRUE, scrollX = TRUE, autoWidth = TRUE, responsive = TRUE, rownames = FALSE, escape = FALSE)))
  # 
  
  
  # render table for cost data
  output$cost_data_table_main_page <- DT::renderDT(
    DT::datatable(
      main_page_output_generation()$cost_data_table,
      filter = "top",
      options = list(
        dom = 'Bftsp',
        paging = TRUE,
        pageLength = 25,        # show more rows
        lengthMenu = c(10, 25, 50, 100),
        ordering = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        scrollX = TRUE,
        autoWidth = TRUE,
        responsive = TRUE
      ),
      rownames = FALSE,
      escape = FALSE,
      class = "compact stripe hover"   # makes rows tighter
    )
  )
  
  
  # render table for DALYS
  output$dalys_table_main_page <-     DT::renderDT(DT::datatable(main_page_output_generation()$dalys_table,
                                                                                          filter = "top", options = list(dom = 'Bftsp', paging = TRUE, pageLength = 10, ordering = TRUE, searching = TRUE, fixedColumns = TRUE, scrollX = TRUE, autoWidth = TRUE, responsive = TRUE, rownames = FALSE, escape = FALSE)))
  
  
  # render table for CET
  output$cet_table_main_page <- DT::renderDT(
    DT::datatable(
      main_page_output_generation()$cet_table,
      rownames = FALSE,
      filter = "top",
      options = list(
        dom = "ftip",
        paging = FALSE,
        ordering = TRUE,
        searching = TRUE,
        scrollX = FALSE,
        autoWidth = TRUE,
        responsive = FALSE
      )
    )
  )
  
  
  
  output$clinical_reduction_map_main_page <- renderPlot({
    main_page_output_generation()$clinical_reduction_map
  })
  
  output$severe_reduction_map_main_page <- renderPlot({
    main_page_output_generation()$severe_reduction_map
  })
  
  output$clinical_cases_averted_map_main_page <- renderPlot({
    main_page_output_generation()$clinical_cases_averted_map
  })
  
  output$severe_cases_averted_map_main_page <- renderPlot({
    main_page_output_generation()$severe_cases_averted_map
  })
  
  output$icer_map_main_page <- renderPlot({
    main_page_output_generation()$icer_map
  })
  
  # output$icer_map_main_page <- renderPlot({
  #   
  #   req(input$main_tabs == "Map: cost-effectiveness by admin-1 unit")
  #   
  #   # your plotting code here
  #   
  #   main_page_output_generation()$icer_map
  #   
  # })
  
  output$bar_plot_main_page <- renderPlot({
    main_page_output_generation()$bar_plot
  })
  
  output$output_navset_tabs_main_page <- renderUI({
    if (input$show_results_main_page != 0) {
      tagList(
        h2("Ranked prioritisation of PMC delivery options by admin-1 unit"),
        tags$style(HTML(".description-text { font-size: 12px; }")), 
        
        
        p(class = "description-text",
          "This table shows the ranked prioritisation of PMC options. The first option is the most cost-effective choice. For a given investment, the first option will avert the most deaths. The second option is less efficient than the first option. We compare different delivery schedules to the currently implemented package of interventions in the selected country, and to each other. We then rank the interventions in order in which they should be added to existing malaria control activities in the selected country. This ranking is based on its cost-effectiveness which incorporates health impact and implementation costs. All costs are presented in 2024 USD.",
          br(), br(),
          "ICERs include economic costs of implementation, with start-up costs annualised at 3% over 7 years. The costs of co-designing a PMC implementation approach specific to country context has not been included.",
          br(), br(),
          "Interpreting ICERs: If implementation in the first region in a country is shown as not cost-effective, but other regions in the same country are shown as cost-effective lower in the ranking, then it would not be cost effective to implement only in the first region in the country, but it would be cost-effective to implement in the first and second (and potentially more regions) simultaneously."
        ),
        
        
        
        
        
        navset_card_tab(height = 800,
                        
    
          nav_panel(
            title = "Table: ranked prioritisation of PMC delivery options", 
            DT::DTOutput("prioritisation_table_main_page")
          ),
          
          nav_panel(
            title = "Map: cost-effectiveness by admin-1 unit", 
            plotOutput("icer_map_main_page")
          ),
          
          nav_panel(
            title = "Table: delivery schedule and coverage assumptions", 
            DT::DTOutput("dosing_schedules_main_page")
          )

          

        )
      )
    }
  })
  
  
  
  
  
  # add table containing target age and coverage data that a user inputs
  observeEvent(input$add_table, {
    
    # ---- Guards ----
    req(input$country)
    req(input$number_of_doses)
    req(input$change_haplotype_data)
    
    if (input$country_or_area == "Admin-1 unit") {
      req(input$area)
    }
    
    # ---- Show results button ----
    output$show_results_button <- renderUI({
      actionButton("show_results", "Show results")
    })
    
    # ---- Country lookup ----
    country_lookup <- c(
      AGO="Angola",BDI="Burundi",BEN="Benin",BFA="Burkina Faso",
      CAF="Central African Republic",CIV="Cote d'Ivoire",
      CMR="Cameroon",COD="DR Congo",COG="Congo-Brazzaville",
      GAB="Gabon",GHA="Ghana",GIN="Guinea",GNQ="Equatorial Guinea",
      KEN="Kenya",LBR="Liberia",MDG="Madagascar",MLI="Mali",
      MOZ="Mozambique",MWI="Malawi",NER="Niger",NGA="Nigeria",
      SLE="Sierra Leone",SSD="South Sudan",TCD="Chad",
      TGO="Togo",TZA="Tanzania",UGA="Uganda",ZMB="Zambia"
    )
    
    get_iso3 <- function(country_name) {
      names(country_lookup)[country_lookup == country_name]
    }
    
    # ============================================================
    # ---- HAPLOTYPE DATA ----
    # ============================================================
    if (!is.null(input$change_haplotype_data) &&
        input$change_haplotype_data == "Yes") {
      
      country <- input$country
      iso3 <- get_iso3(country)
      
      chosen_area <- NULL
      if (!is.null(input$area)) {
        chosen_area <- gsub(" ", "_", input$area)
      }
      
      haplotype_data_final <-
        readxl::read_xlsx(
          "files_needed/haplotype_data_final.xlsx"
        )
      
      haplotype_data_df <-
        haplotype_data_final %>%
        dplyr::filter(`Iso code` == iso3)
      
      haplotype_data_df$Country <- country
      
      haplotype_data_df <-
        haplotype_data_df %>%
        dplyr::select(-`Iso code`)
      
      if (!is.null(chosen_area) &&
          input$country_or_area == "Admin-1 unit") {
        
        haplotype_data_df <-
          haplotype_data_df %>%
          dplyr::filter(`Admin-1 unit` == chosen_area)
      }
      
      # ---- Rename ----
      colnames(haplotype_data_df) <- c(
        "Country","Admin-1 unit","latitude","longitude",
        "I_AKA_","I_GKA_","I_GEA_","I_GEG_",
        "V_GKA_","V_GKG_","Other","Sum of proportions"
      )
      
      # ---- Keep editable cols ----
      haplotype_data_df <-
        haplotype_data_df[
          c("Country","Admin-1 unit","latitude","longitude",
            "I_AKA_","I_GKA_","I_GEA_","I_GEG_",
            "V_GKA_","V_GKG_","Other")
        ]
      
      num_cols <- c(
        "latitude","longitude",
        "I_AKA_","I_GKA_","I_GEA_","I_GEG_",
        "V_GKA_","V_GKG_","Other"
      )
      
      haplotype_data_df[, num_cols] <-
        lapply(
          haplotype_data_df[, num_cols],
          function(x) round(as.numeric(x), 3)
        )
      
      # ---- Sum proportions ----
      haplotype_data_df$`Sum of proportions` <-
        rowSums(
          haplotype_data_df[, c(
            "I_AKA_","I_GKA_","I_GEA_",
            "I_GEG_","V_GKA_","V_GKG_","Other"
          )],
          na.rm = TRUE
        )
      
      haplotype_data_df$`Admin-1 unit` <-
        gsub("_"," ", haplotype_data_df$`Admin-1 unit`)
      
      haplotype_data(haplotype_data_df)
      
      # haplotype_data_for_table(
      #   haplotype_data_df[, c(
      #     "Country","Admin-1 unit","latitude","longitude",
      #     "I_AKA_","I_GKA_","I_GEA_",
      #     "I_GEG_","V_GKA_","V_GKG_","Other"
      #   )]
      # )
      
      # Store FULL data (with lat/long) internally
      haplotype_data(haplotype_data_df)
      
      # Create EDITABLE version WITHOUT lat/long
      haplotype_data_for_table(
        haplotype_data_df[, c(
          "Country","Admin-1 unit",
          "I_AKA_","I_GKA_","I_GEA_",
          "I_GEG_","V_GKA_","V_GKG_","Other"
        )]
      )
      
      output$dynamic_haplotype_table <- DT::renderDT({
        DT::datatable(
          haplotype_data_for_table(),
          editable = TRUE,
          caption =
            "Please edit the haplotypes given in this table for your country."
        )
      })
      
      error_displayed <- reactiveVal(FALSE)
      
      observeEvent(
        input$dynamic_haplotype_table_cell_edit,
        {
          
          info <- input$dynamic_haplotype_table_cell_edit
          req(info$value)
          
          df_edit <- haplotype_data_for_table()
          
          df_edit[info$row, info$col] <-
            round(as.numeric(info$value), 3)
          
          # Recalculate sum
          df_edit$`Sum of proportions` <-
            rowSums(
              df_edit[, c(
                "I_AKA_","I_GKA_","I_GEA_",
                "I_GEG_","V_GKA_","V_GKG_","Other"
              )],
              na.rm = TRUE
            )
          
          haplotype_data_for_table(df_edit)
          
          # ---- MERGE BACK WITH LAT/LONG ----
          full_df <- haplotype_data()
          
          full_df_updated <-
            full_df %>%
            dplyr::select(Country, `Admin-1 unit`, latitude, longitude) %>%
            dplyr::left_join(
              df_edit,
              by = c("Country", "Admin-1 unit")
            )
          
          haplotype_data(full_df_updated)
          
          # ---- Validation ----
          if (!error_displayed() &&
              any(abs(df_edit$`Sum of proportions` - 1) > 0.001,
                  na.rm = TRUE)) {
            
            showModal(
              modalDialog(
                title = "Error",
                "Please check that all the haplotype frequencies sum to 1 before continuing.",
                easyClose = TRUE
              )
            )
            
            error_displayed(TRUE)
          }
        }
      )
      
      # observeEvent(
      #   input$dynamic_haplotype_table_cell_edit,
      #   {
      #     
      #     info <- input$dynamic_haplotype_table_cell_edit
      #     req(info$value)
      #     
      #     df_edit <- haplotype_data_for_table()
      #     
      #     df_edit[info$row, info$col] <-
      #       round(as.numeric(info$value), 3)
      #     
      #     df_edit$`Sum of proportions` <-
      #       rowSums(
      #         df_edit[, c(
      #           "I_AKA_","I_GKA_","I_GEA_",
      #           "I_GEG_","V_GKA_","V_GKG_","Other"
      #         )],
      #         na.rm = TRUE
      #       )
      #     
      #     haplotype_data_for_table(df_edit)
      #     
      #     if (!error_displayed() &&
      #         any(abs(df_edit$`Sum of proportions` - 1) > 0.001,
      #             na.rm = TRUE)) {
      #       
      #       showModal(
      #         modalDialog(
      #           title = "Error",
      #           "Please check that all the haplotype frequencies sum to 1 before continuing.",
      #           easyClose = TRUE
      #         )
      #       )
      #       
      #       error_displayed(TRUE)
      #     }
      #   }
      # )
      
      
      
      
      
    }
    
    # ============================================================
    # ---- DOSE / COVERAGE TABLE ----
    # ============================================================
    
    
    # data_df <- data.frame(
    #   Dose = paste0("Dose ", seq_len(input$number_of_doses)),
    #   Age = NA,
    #   Coverage = NA
    # )
    # 
    # output$dynamic_table <- DT::renderDT({
    #   DT::datatable(
    #     data_df,
    #     editable = TRUE,
    #     caption =
    #       "Please list the age at which PMC doses are given in weeks and the associated coverage values given within a range 0–1."
    #   ) %>%
    #     DT::formatStyle(
    #       names(data_df),
    #       backgroundColor = "#F7F7F7"
    #     )
    # })
    
    
    # ---- Initialise dose table ----
    init_df <- data.frame(
      Dose = paste0("Dose ", seq_len(input$number_of_doses)),
      `Age (months)` = NA,
      `Coverage (0-100%)` = NA,
      check.names=FALSE
    )
    
    dose_data(init_df)
    
    # ---- Render table ----
    output$dynamic_table <- DT::renderDT({
      
      DT::datatable(
        dose_data(),
        editable = TRUE,
        caption =
          "Input the age at which PMC doses are given in months and the expected coverage of PMC by dose given within a range 0–100."
      ) %>%
        DT::formatStyle(
          names(dose_data()),
          backgroundColor = "#F7F7F7"
        )
      
    })
    
    
    
    # ============================================================
    # ---- UI CARDS ----
    # ============================================================
    
    if (input$change_haplotype_data == "No") {
      
      output$cov_and_schedule_card_only <- renderUI({
        
        tagList(
          
          card(
            card_header(
              class = "bg-dark",
              "PMC delivery schedule and coverage data"
            ),
            DT::DTOutput("dynamic_table")
          ),
          
          card(
            uiOutput("show_results_button")
          )
          
        )
      })
    }
    
    if (input$change_haplotype_data == "Yes") {
      
      output$cov_and_schedule_and_haplotype_card <- renderUI({
        
        tagList(
          
          card(
            card_header(
              class = "bg-dark",
              "dhps haplotype frequency data:"
            ),
            DT::DTOutput("dynamic_haplotype_table")
          ),
          
          card(
            card_header(
              class = "bg-dark",
              "PMC delivery schedule and coverage data:"
            ),
            DT::DTOutput("dynamic_table")
          ),
          
          card(
            uiOutput("show_results_button")
          )
          
        )
      })
    }
    
  })
  
  
  
  
  
  
  
  
  # update results when the schedule and coverage table is edited by user
  # observeEvent(input$dynamic_table_cell_edit, {
  #   info <- input$dynamic_table_cell_edit
  #   if (!is.null(info$value)) {
  #     data_df <- data()
  #     
  #     data_df[info$row, info$col] <- info$value
  #     data(data_df)
  #     edited_data(data_df)
  #     
  #     
  #   }
  # })
  
  observeEvent(input$dynamic_table_cell_edit, {
    
    info <- input$dynamic_table_cell_edit
    req(info)
    
    df <- dose_data()
    
    # ---- Apply edit safely ----
    df[info$row, info$col] <-
      suppressWarnings(
        as.numeric(info$value)
      )
    
    dose_data(df)
    
  })
  
  
  
  # get data from the table a user has edited when "show results" button is pressed
  sourced_data <- eventReactive(input$show_results, {
    
    
    showModal(
      modalDialog(
        title = tagList(icon("clock"), "Generating results"),
        tags$p("This may take approximately"),
        tags$strong("~1 minute"),
        tags$p("Please do not refresh or navigate away."),
        easyClose = TRUE,   # 👈 key line
        footer = NULL
      )
    )

    withProgress(message = "Collecting data, please wait", {
    country <- input$country
    
    
    # if (input$default_or_choose == "Run PMC co-delivery with EPI only") {
    # 
    #   #country_main_page <- country
    # 
    #   #source("code_for_DHS_coverage_main_page.R", local = TRUE)
    #   
    #   coverage_df_admin1 <- vroom::vroom(("files_needed/all_sites_admin1_cov_weighted_average.csv"))
    #   coverage_df_country <- vroom::vroom(("files_needed/all_countries_cov_weighted_average.csv"))
    #   
    #   coverage_df_admin1$unique_ages_n <- lapply(coverage_df_admin1$unique_ages, 
    #                                              function(x) {
    #                                                # Split the string, take the first element (if multiple)
    #                                                split_result <- strsplit(x, ",\\s*")[[1]]
    #                                                # Convert to numeric, handling potential errors/NAs
    #                                                as.numeric(split_result)
    #                                              })
    #   
    #   coverage_df_country$unique_ages_n <- lapply(coverage_df_country$unique_ages, 
    #                                               function(x) {
    #                                                 # Split the string, take the first element (if multiple)
    #                                                 split_result <- strsplit(x, ",\\s*")[[1]]
    #                                                 # Convert to numeric, handling potential errors/NAs
    #                                                 as.numeric(split_result)
    #                                               })
    #   
    #   coverage_df_admin1$avg_coverage_by_age_n <- lapply(coverage_df_admin1$avg_coverage_by_age, 
    #                                                      function(x) {
    #                                                        # Split the string, take the first element (if multiple)
    #                                                        split_result <- strsplit(x, ",\\s*")[[1]]
    #                                                        # Convert to numeric, handling potential errors/NAs
    #                                                        as.numeric(split_result)
    #                                                      })
    #   
    #   coverage_df_country$avg_coverage_by_age_n <- lapply(coverage_df_country$avg_coverage_by_age, 
    #                                                       function(x) {
    #                                                         # Split the string, take the first element (if multiple)
    #                                                         split_result <- strsplit(x, ",\\s*")[[1]]
    #                                                         # Convert to numeric, handling potential errors/NAs
    #                                                         as.numeric(split_result)
    #                                                       })
    #   
    #   
    # 
    #   if (input$country_or_area == "Admin-1 unit") {
    # 
    #     chosen_area <- gsub(" ", "_", input$area)
    # 
    #     schedule <- unlist((coverage_df_admin1 %>% filter(Country == country,  `Admin-1 unit` == chosen_area))$unique_ages_n)
    #     cov <- unlist((coverage_df_admin1 %>% filter(Country == country, `Admin-1 unit` == chosen_area))$avg_coverage_by_age_n)
    #     area_names <- unique((vroom::vroom(("files_needed/all_sites_for_DT.csv")) %>% filter(Country==country, `Admin-1 unit` == chosen_area))$`Admin-1 unit`)
    #     number_of_doses <- length(schedule)
    # 
    #   }
    # 
    #   if (input$country_or_area == "Whole country") {
    #     schedule <- unlist((coverage_df_country %>% filter(Country == country))$unique_ages_n)
    #     cov <- unlist((coverage_df_country %>% filter(Country == country))$avg_coverage_by_age_n)
    #     area_names <- unique((vroom::vroom(("files_needed/all_sites_for_DT.csv")) %>% filter(Country==country))$`Admin-1 unit`)
    #     number_of_doses <- length(schedule)
    # 
    #   }
    # } else {

      if (input$country_or_area == "Admin-1 unit") {

        chosen_area <- gsub(" ", "_", input$area)

      }

      if (any(edited_data()$`Coverage (0-100%)` > 100) | any(edited_data()$`Coverage (0-100%)` < 0)) {
        showModal(modalDialog(title = "Error", "Please check that the coverage values lie in the range 0-1.", easyClose = TRUE))
        return()
      }
      
      #schedule <- round(as.numeric(edited_data()$`Age (months)`) * 30.4167, 0)
      #cov <- round(as.numeric(edited_data()$`Coverage (0-100%)`) / 100, 0)

    #}

 
    if (input$change_haplotype_data == "Yes") {
      haplotype_data_final <- haplotype_data_for_table()} else {
        haplotype_data_final <- read_xlsx(("files_needed/haplotype_data_final.xlsx"))

    }


    #final_ranked_table_full <- read_xlsx(("files_needed/final_ranked_table_full.xlsx"))
    cost_data <- read_xlsx(("files_needed/cost_and_incidence_data.xlsx"))
    protective_efficacy_30_days <- read_xlsx(("files_needed/protective_efficacy_30_days.xlsx"))
    cet_thresholds <- read_xlsx(("files_needed/cost_effectiveness_thresholds.xlsx"))
    

    # if (input$change_haplotype_data == "No" && input$default_or_choose == "Run PMC co-delivery with EPI only") {
    # 
    #   country_main_page <- country
    # 
    #   #withProgress(message = "Generating results, please wait", {source("code_main_page.R", local = TRUE)})
    #   source("code_main_page.R", local = TRUE)
    #   
    # } else {

      #withProgress(message = "Generating results, please wait", {source("code_user_input.R", local = TRUE)})
      source("code_user_input.R", local = TRUE)
      
    #}

    source("prioritisation_calculation_user_input.R", local = TRUE)

    saved <- ls()
    list_items<-list()

    for (i in 1:length(saved)) {
      list_items[[saved[i]]] <- get(saved[i])
    }

    list_items
    
  })

  }
  
  )
  
  
  
  
  # create incidence graph for either the default values or the inputted PMC schedule
  
  user_input_output_generation <- eventReactive(input$show_results, {

    data <- sourced_data()

    for (i in 1:length(data)) {
      assign(names(data[i]), data[[i]])
    }


    colours <- c("red", "green", "yellow", "blue", "pink")

    user_input_outputs <- list()

    number_of_doses <- length(schedule)
    
    
    if (input$country_or_area == "Admin-1 unit") {
      
      withProgress(message = "Preparing results, please wait", {
        # Clinical incidence graph

        # Initialize the ggplot object
        clinical_inc_graphs <- ggplot()
        incProgress(1/10)
        names_in_plot <- c()

        # Iterate over columns starting with number_of_doses
        for (i in 1:length(number_of_doses)) {
          
          PMC_impact_ppy_temp <- PMC_impact_ppy %>% filter(infection_class=="clinical")
          incidence_ppy_df_temp <- incidence_ppy_df %>% filter(infection_class == "clinical", `Admin-1 unit`==chosen_area)
          
          # get first 5 letters from each in 
          # vaccines_text <- unique(
          #   (coverage_df_admin1 %>% filter(Country == country, `Admin-1 unit`==chosen_area))$vaccines_by_age
          # )
          # 
          # vaccines_vec <- trimws(unlist(strsplit(vaccines_text, ",")))
          # 
          # vaccines_dosed <- paste0(
          #   "Dose ",
          #   seq_along(vaccines_vec),
          #   ": ",
          #   vaccines_vec,
          #   collapse = "\n"
          # )
          # 
          # legend_label <- paste0(
          #   unique((coverage_df_admin1 %>% filter(Country == country, `Admin-1 unit`==chosen_area))$Scenario),
          #   ":\n",
          #   vaccines_dosed
          # )
          
          legend_label <- paste0(
            "Dose ",
            seq_along(schedule),
            ": ",
            round(schedule / 30.4167, 1),
            " months",
            " (",
            cov * 100,
            "% coverage)"
          ) |> 
            paste(collapse = "\n")
          
          names_in_plot <- c(names_in_plot, legend_label)
          
          clinical_inc_graphs <- clinical_inc_graphs +
            geom_line(aes_string(x = PMC_impact_ppy_temp$age_in_days_midpoint, 
                                 y = 1000 * PMC_impact_ppy_temp$value, 
                                 colour = paste0("\"", legend_label, "\"")), 
                      linewidth = 0.6) 
          
          legend_label2 <- paste0(legend_label) #paste0("Cases averted, ", number_of_doses[i], " doses")
          names_in_plot <- c(names_in_plot, legend_label2)
          
          clinical_inc_graphs <- clinical_inc_graphs + 
            geom_ribbon(aes_string(x = PMC_impact_ppy_temp$age_in_days_midpoint, 
                                   ymin = 1000 * PMC_impact_ppy_temp$value, 
                                   ymax = 1000 * incidence_ppy_df_temp$value, 
                                   fill = paste0("\"", legend_label2, "\"")), 
                        alpha = 0.3, show.legend = TRUE)
        }
        
        
        clinical_inc_graphs <- clinical_inc_graphs + 
          geom_line(aes_string(x = incidence_ppy_df_temp$age_in_days_midpoint, 
                               y = 1000 * incidence_ppy_df_temp$value, 
                               colour = "\"No PMC\""))
        
        incProgress(1/10)
        
        
        names(clinical_inc_graphs$layers) <- c(names_in_plot, "No PMC")
        
        clinical_inc_graphs <- ggplot() + clinical_inc_graphs$layers +
          labs(x = "Age (months)", y = "New clinical infections per 1000 children", colour = "Incidence by delivery model [select/deselect]", fill = "Cases averted", title = paste0(chosen_area, ", ", country)) +
          ylim(0, max((1000*incidence_ppy_df_temp$value) + 500)) +
          
          scale_x_continuous(
            breaks = c(0, 183, 365, 549, 730, 913),
            labels = c("0", "6", "12", "18", "24", "30")
          ) + 
          scale_fill_manual("Cases averted", breaks = names_in_plot[c(seq(from=2, to=length(names_in_plot), by=2))], values = colours[2:5]) +
          scale_color_manual("Incidence by delivery model [select/deselect]", breaks = c("No PMC", names_in_plot[c(seq(from=1, to=length(names_in_plot), by=2))]), values = colours[1:5]) + 
          theme(panel.grid.major = element_line(colour = "gray", linewidth = 0.2), 
                panel.background = element_rect(fill = 'transparent'), 
                plot.background = element_rect(fill = 'transparent', color = NA), 
                legend.background = element_rect(fill = 'transparent')) 
        
        
        clinical_inc_graphs_plotly <- ggplotly(clinical_inc_graphs)
        
        
        for (i in 1:length(clinical_inc_graphs_plotly$x$data)){
          if (!is.null(clinical_inc_graphs_plotly$x$data[[i]]$name)){
            clinical_inc_graphs_plotly$x$data[[i]]$name <- gsub("^\\(", "", clinical_inc_graphs_plotly$x$data[[i]]$name)
            clinical_inc_graphs_plotly$x$data[[i]]$name <- gsub(",1)", "", clinical_inc_graphs_plotly$x$data[[i]]$name)
            
          }
        }
        
        text_x <- paste0("Age: ", clinical_inc_graphs_plotly$x$data[[i]]$x)
        text_y <- paste0("Clinical incidence per 1000: ", round(clinical_inc_graphs_plotly$x$data[[i]]$y, digits=1))
        text_z <- Map(function(x, y) paste0("", x, " days\n", y, ""), text_x, text_y)
        
        
        clinical_inc_graphs_plotly <- clinical_inc_graphs_plotly %>%
          style(hoverinfo = "skip", traces = c(3, 5, 7, 9)) %>% 
          style(text = unlist(text_z))
        
        # Assign the plot to the user_input_outputs
        user_input_outputs$clinical_incidence_graph <- clinical_inc_graphs_plotly
        
        ##########################################################################
        
        # severe incidence graph 
        incProgress(1/10)
        # Initialize the ggplot object
        severe_inc_graphs <- ggplot()
        
        names_in_plot <- c()
        

        # Iterate over columns starting with number_of_doses
        for (i in 1:length(number_of_doses)) {
          
          PMC_impact_ppy_temp <- PMC_impact_ppy %>% filter(infection_class=="severe")
          incidence_ppy_df_temp <- incidence_ppy_df %>% filter(infection_class == "severe", `Admin-1 unit`==chosen_area)
          
          # get first 5 letters from each in 
          
          
          legend_label <- paste0(
            "Dose ",
            seq_along(schedule),
            ": ",
            round(schedule / 30.4167, 1),
            " months",
            " (",
            cov * 100,
            "% coverage)"
          ) |> 
            paste(collapse = "\n")
          
          
          # vaccines_text <- unique(
          #   (coverage_df_admin1 %>% filter(Country == country, `Admin-1 unit`==chosen_area))$vaccines_by_age
          # )
          # 
          # vaccines_vec <- trimws(unlist(strsplit(vaccines_text, ",")))
          # 
          # vaccines_dosed <- paste0(
          #   "Dose ",
          #   seq_along(vaccines_vec),
          #   ": ",
          #   vaccines_vec,
          #   collapse = "\n"
          # )
          # 
          # legend_label <- paste0(
          #   unique((coverage_df_admin1 %>% filter(Country == country, `Admin-1 unit`==chosen_area))$Scenario),
          #   ":\n",
          #   vaccines_dosed
          # )
          
          
          
          
          
          names_in_plot <- c(names_in_plot, legend_label)
          
          severe_inc_graphs <- severe_inc_graphs +
            geom_line(aes_string(x = PMC_impact_ppy_temp$age_in_days_midpoint, 
                                 y = 1000 * PMC_impact_ppy_temp$value, 
                                 colour = paste0("\"", legend_label, "\"")), 
                      linewidth = 0.6) 
          
          legend_label2 <- paste0(legend_label) #paste0("Cases averted, ", number_of_doses[i], " doses")
          names_in_plot <- c(names_in_plot, legend_label2)
          
          severe_inc_graphs <- severe_inc_graphs + 
            geom_ribbon(aes_string(x = PMC_impact_ppy_temp$age_in_days_midpoint, 
                                   ymin = 1000 * PMC_impact_ppy_temp$value, 
                                   ymax = 1000 * incidence_ppy_df_temp$value, 
                                   fill = paste0("\"", legend_label2, "\"")), 
                        alpha = 0.3, show.legend = TRUE)
        }
        
        
        severe_inc_graphs <- severe_inc_graphs + 
          geom_line(aes_string(x = incidence_ppy_df_temp$age_in_days_midpoint, 
                               y = 1000 * incidence_ppy_df_temp$value, 
                               colour = "\"No PMC\""))
        
        
        
        names(severe_inc_graphs$layers) <- c(names_in_plot, "No PMC")
        
        severe_inc_graphs <- ggplot() + severe_inc_graphs$layers +
          labs(x = "Age (months)", y = "New hospitalisations per 1000 children", colour = "Hospitalisations by delivery model [select/deselect]", fill = "Hospitalisations averted", title = paste0(chosen_area, ", ", country)) +
          ylim(0, max((1000*incidence_ppy_df_temp$value)) + 100) +
          
          scale_x_continuous(
            breaks = c(0, 183, 365, 549, 730, 913),
            labels = c("0", "6", "12", "18", "24", "30")
          ) + 
          scale_fill_manual("Hospitalisations averted", breaks = names_in_plot[c(seq(from=2, to=length(names_in_plot), by=2))], values = colours[2:5]) +
          scale_color_manual("Hospitalisations by delivery model [select/deselect]", breaks = c("No PMC", names_in_plot[c(seq(from=1, to=length(names_in_plot), by=2))]), values = colours[1:5]) + 
          theme(panel.grid.major = element_line(colour = "gray", linewidth = 0.2), 
                panel.background = element_rect(fill = 'transparent'), 
                plot.background = element_rect(fill = 'transparent', color = NA), 
                legend.background = element_rect(fill = 'transparent')) 
        
        
        severe_inc_graphs_plotly <- ggplotly(severe_inc_graphs)
        incProgress(1/10)
        
        for (i in 1:length(severe_inc_graphs_plotly$x$data)){
          if (!is.null(severe_inc_graphs_plotly$x$data[[i]]$name)){
            severe_inc_graphs_plotly$x$data[[i]]$name <- gsub("^\\(", "", severe_inc_graphs_plotly$x$data[[i]]$name)
            severe_inc_graphs_plotly$x$data[[i]]$name <- gsub(",1)", "", severe_inc_graphs_plotly$x$data[[i]]$name)
            
          }
        }
        
        text_x <- paste0("Age: ", severe_inc_graphs_plotly$x$data[[i]]$x)
        text_y <- paste0("Hospitalisations per 1000: ", round(severe_inc_graphs_plotly$x$data[[i]]$y, digits=1))
        text_z <- Map(function(x, y) paste0("", x, " days\n", y, ""), text_x, text_y)
        
        
        severe_inc_graphs_plotly <- severe_inc_graphs_plotly %>%
          style(hoverinfo = "skip", traces = c(3, 5, 7, 9)) %>% 
          style(text = unlist(text_z))
        
        # Assign the plot to the user_input_outputs
        user_input_outputs$severe_incidence_graph <- severe_inc_graphs_plotly
        
        ##########################################################################
        
        
        # Haplotype data table 
        # if (input$change_haplotype_data == "Yes") {
        #   
        #   haplotype_data_df <- haplotype_data_for_table()
        #   
        # } else {
        #   
        #   haplotype_data_df <- haplotype_data_final %>% filter(Country == country, `Admin-1 unit` == chosen_area)
        #   haplotype_data_df <- haplotype_data_df[c("Country", "Admin-1 unit",  "latitude", "longitude", "I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other")]
        #   
        #   
        # }
        # 
        # #haplotype_data_df <- haplotype_data_df[c("Country", "Admin-1 unit",  "latitude", "longitude", "I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "other")]
        # 
        # colnames(haplotype_data_df) <- c("Country", "Admin-1 unit",  "latitude", "longitude", "I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other")
        # 
        # haplotype_data_df[, c("I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other")] <- lapply(haplotype_data_df[, c( "I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other")], function(x) round(as.numeric(x), digits = 3))
        # 
        # for (i in 1:dim(haplotype_data_df)[1]) {
        #   haplotype_data_df[i, "Sum of proportions"] <- sum(unlist(c(haplotype_data_df[i, "I_AKA_"], haplotype_data_df[i, "I_GKA_"], haplotype_data_df[i, "I_GEA_"], haplotype_data_df[i, "I_GEG_"], haplotype_data_df[i, "V_GKA_"], haplotype_data_df[i, "V_GKG_"], haplotype_data_df[i, "Other"])), na.rm = TRUE)
        # }
        # 
        # haplotype_data_df$`Admin-1 unit` <- gsub("_", " ", haplotype_data_df$`Admin-1 unit`)
        # user_input_outputs$haplotype_table <- haplotype_data_df[, c("Country", "Admin-1 unit", "I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other")]
        # 
        

        hap_cols <- c(
          "I_AKA_", "I_GKA_", "I_GEA_",
          "I_GEG_", "V_GKA_", "V_GKG_", "Other"
        )
        
        if (input$change_haplotype_data == "Yes") {
          
          # ---- Get edited table (NO lat/long in it) ----
          df_edit <- haplotype_data_for_table()
          
          # Ensure correct Admin-1 filtering
          if (!is.null(chosen_area)) {
            df_edit <- df_edit %>%
              dplyr::filter(`Admin-1 unit` == chosen_area)
          }
          
          # Ensure numeric rounding
          df_edit[, hap_cols] <- lapply(
            df_edit[, hap_cols],
            function(x) round(as.numeric(x), 3)
          )
          
          # Recalculate Sum
          df_edit$`Sum of proportions` <-
            rowSums(df_edit[, hap_cols], na.rm = TRUE)
          
          # ---- Reattach latitude & longitude safely ----
          latlong_df <- haplotype_data() %>%
            dplyr::select(Country, `Admin-1 unit`, latitude, longitude)
          
          haplotype_data_df <- latlong_df %>%
            dplyr::filter(Country == country,
                          `Admin-1 unit` == chosen_area) %>%
            dplyr::left_join(
              df_edit,
              by = c("Country", "Admin-1 unit")
            )
          
        } else {
          
          # ---- Original filtered data ----
          haplotype_data_df <- haplotype_data_final %>%
            dplyr::filter(Country == country,
                          `Admin-1 unit` == chosen_area) %>%
            dplyr::select(
              Country, `Admin-1 unit`,
              latitude, longitude,
              all_of(hap_cols)
            )
          
          # Ensure numeric rounding
          haplotype_data_df[, hap_cols] <- lapply(
            haplotype_data_df[, hap_cols],
            function(x) round(as.numeric(x), 3)
          )
          
          # Calculate Sum
          haplotype_data_df$`Sum of proportions` <-
            rowSums(haplotype_data_df[, hap_cols], na.rm = TRUE)
        }
        
        # ---- Clean formatting ----
        haplotype_data_df$`Admin-1 unit` <-
          gsub("_", " ", haplotype_data_df$`Admin-1 unit`)
        
        # ---- Final object used downstream ----
        user_input_outputs$haplotype_table <- haplotype_data_df[
          c("Country", "Admin-1 unit",
          
            hap_cols)
        ]
        

        
        ##########################################################################
        incProgress(1/10)
        
        
        #########################################################################
        
        # Haplotype map 
        
        # names(adm1)[names(adm1) == "NAME_2"] <- "Region"
        # adm1$NAME_2 <- adm1$Region
        # 
        # 
        # haplotype_proportions_map <- ggplot(adm1) + 
        #   geom_sf() + 
        #   theme_bw() +
        #   theme(
        #     legend.text = element_text(size = 12),
        #     legend.title = element_text(size = 15),
        #     title = element_text(size = 20)
        #   ) +
        #   scatterpie::geom_scatterpie(
        #     data = haplotype_data_df,
        #     cols = c("I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other"),
        #     aes(x = longitude, y = latitude, group = Region, r = 0.13 * min(diff(range(haplotype_data_df$longitude)), diff(range(haplotype_data_df$latitude)))) # Reduced radius from 0.8 to 0.3
        #   ) +
        #   scale_fill_manual(
        #     values = c(  "#00C094", "#00B6EB", "#FFA500","#F8766D","#b7a1ff", "#7361b3","#D2B48C"),
        #     name = "Haplotypes",
        #     breaks = c("I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other"),
        #     labels = c("I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other")
        #   ) +
        #   theme(
        #     axis.title.x = element_blank(),
        #     axis.text.x = element_blank(),
        #     axis.ticks.x = element_blank(),
        #     axis.title.y = element_blank(),
        #     axis.text.y = element_blank(),
        #     axis.ticks.y = element_blank(),
        #     panel.background = element_blank(),
        #     panel.border = element_blank(),
        #     panel.grid.major = element_blank(),
        #     panel.grid.minor = element_blank(),
        #     plot.background = element_blank()
        #   )
        
        #adm1$NAME_2 <- stri_trans_general(str = gsub(" ", "_", adm1$NAME_2), id = "Latin-ASCII")
        adm1$NAME_2 <- stri_trans_general(str = gsub(" ", "_", adm1$NAME_1), id = "Latin-ASCII")
        adm1$NAME_2 <- stri_trans_general(str = gsub("-", "_", adm1$NAME_2), id = "Latin-ASCII")
        adm1$`Admin-1 unit` <- adm1$NAME_2
        combined_protective_efficacy_30_days <- merge(protective_efficacy_30_days %>% filter(Country == country, `Admin-1 unit` == chosen_area), adm1, by="Admin-1 unit") 
        
        
        haplotype_proportions_map <- ggplot(combined_protective_efficacy_30_days) + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
          theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
          geom_sf(data=adm1)+
          geom_sf(aes(fill=day_30_protect_efficacy, geometry=geometry), size=0.5)  +
          geom_sf_label(aes(label = paste0(`Admin-1 unit`), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
          ggtitle(paste0("Average 30 day protective efficacy following a SP dose: ", country)) + labs(fill="30 day protective efficacy") +
          scale_fill_viridis_c(limits=c(min(combined_protective_efficacy_30_days$day_30_protect_efficacy),max(combined_protective_efficacy_30_days$day_30_protect_efficacy))) + 
          theme(text=element_text(size=14),legend.title =element_text(size=14), legend.text = element_text(size=14),
                axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                panel.background=element_blank(),
                panel.border=element_blank(),
                panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),
                plot.background=element_blank())
        
        
        
        user_input_outputs$haplotype_map <- haplotype_proportions_map 
        
        #########################################################################
        
        # Probability of drug protection
        incProgress(1/10)
        
        
        
        prob_drug_protect_plot <- ggplot()
        names_in_plot <- c()
        
        # Iterate over columns starting with 'prot_overall_'
        for (col in names(df_drug_protect)) {
          if (startsWith(col, 'prot_overall_')) {
            
            legend_label <- gsub('prot_overall_', '', col)
            legend_label <- gsub('_', ' ', legend_label)
            names_in_plot <- c(names_in_plot, legend_label)
            
            # Create geom_line for each column
            print(legend_label)
            prob_drug_protect_plot <- prob_drug_protect_plot + geom_line(aes_string(x = df_drug_protect$time, y = paste0("df_drug_protect[[\"", col, "\"]]"), colour = paste0("\"", legend_label, "\"")), linewidth=1)
            
          }
          
          
        }
        
        
        names(prob_drug_protect_plot$layers) <- names_in_plot
        
        layer_index <- which(names(prob_drug_protect_plot$layers) == gsub("_", " ", chosen_area))
        
        # Customize legend and labels
        prob_drug_protect_plot <- ggplot() + prob_drug_protect_plot$layers[[layer_index]] +
          labs(x = "Days since SP dose", y = "Probability of drug protection", colour= "Admin-1 unit",
               title = "Probability of drug protection following an SP dose") +
          theme(axis.text = element_text(size = 12),
                axis.title = element_text(size = 14),
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))
        
        
        
        user_input_outputs$prob_drug_protect_graph <- ggplotly(prob_drug_protect_plot) 
        
        
        prob_drug_protect_plotly <- user_input_outputs$prob_drug_protect_graph
        
        
        for (i in 1:length(prob_drug_protect_plotly$x$data)) {
          
          if (!is.null(prob_drug_protect_plotly$x$data[[i]]$name)) {
            
            prob_drug_protect_plotly$x$data[[i]]$name <-
              gsub("^\\(", "", prob_drug_protect_plotly$x$data[[i]]$name)
            
            prob_drug_protect_plotly$x$data[[i]]$name <-
              gsub(",1\\)$", "", prob_drug_protect_plotly$x$data[[i]]$name)
            
          }
        }
        
        for (i in 1:length(prob_drug_protect_plotly$x$data)) {
          
          # Build hover text
          text_x <- paste0(
            "Days since SP dose: ",
            prob_drug_protect_plotly$x$data[[i]]$x
          )
          
          text_y <- paste0(
            "Probability of protection: ",
            round(prob_drug_protect_plotly$x$data[[i]]$y, 3)
          )
          
          text_xy <- Map(
            function(x, y) paste0(x, "\n", y),
            text_x,
            text_y
          )
          
          # Attach hover text to trace
          prob_drug_protect_plotly$x$data[[i]]$text <- unlist(text_xy)
          prob_drug_protect_plotly$x$data[[i]]$hoverinfo <- "text"
        }
        
        user_input_outputs$prob_drug_protect_graph <-
          prob_drug_protect_plotly
        
        ## ----------------------------------------------------
        ## ICERS map
        ## ----------------------------------------------------
      
        
        adm1$`Region (admin-1)` <- adm1$`Admin-1 unit`
        
        final_ranked_table_full <- final_ranked_table_full %>%
          mutate(
            `Rank of PMC option by cost-effectiveness` = row_number()
          )
        
        adm1_merged <- left_join(adm1, final_ranked_table_full, by = "Region (admin-1)")
        
        # Extract values
        icer_vals <- adm1_merged$`ICER based on regional costs only (no national costs)`
        
        min_val <- floor(min(icer_vals, na.rm = TRUE) / 10) * 10
        max_val <- ceiling(max(icer_vals, na.rm = TRUE) / 10) * 10
        mid_val <- round((min_val+max_val) / 2)
        
        
        icer_map <- ggplot(adm1_merged) +
          
          # Base map
          geom_sf(data = adm1, fill = NA, color = "grey60") +
          
          # Filled regions
          geom_sf(aes(fill = `ICER based on regional costs only (no national costs)`,
                      geometry = geometry),
                  size = 0.5) +
          
          # Thick border for selected region
          geom_sf(
            data = subset(adm1_merged, `Region (admin-1)` == chosen_area),  # <-- replace admin1_name
            fill = NA,
            color = "black",
            size = 1.5
          ) +
          
          # Labels
          # geom_sf_label(
          #   aes(label = paste0("Rank: ", `Rank of PMC option by cost-effectiveness`),
          #       geometry = geometry),
          #   fun.geometry = sf::st_point_on_surface,
          #   size = 3,
          #   alpha = 0.2
          # ) +
          
          theme_void() +
          theme(
            legend.title = element_text(hjust = 0.5)
          ) +
          
          scale_fill_gradient2(
            low = "#008631",
            mid = "#cefad0",
            high = "#ffffff",
            midpoint = mid_val,
            limits = c(min_val, max_val),
            breaks = c(min_val, mid_val, max_val),
            labels = c(
              paste0(comma(min_val), ""),
              comma(mid_val),
              paste0(comma(max_val), "")
            ),
            guide = guide_colorbar(reverse = TRUE)
          ) +
          
          labs(fill = "Cost-effectiveness\n(incremental cost-effectiveness ratio)\n") +
          
          theme(
            text = element_text(size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 14)
          )
        # icer_map <- ggplot(adm1_merged) +
        #   geom_sf(data = adm1) +
        #   geom_sf(aes(fill = `ICER based on regional costs only (no national costs)`,
        #               geometry = geometry),
        #           size = 0.5) +
        #   # geom_sf_label(
        #   #   aes(label = paste0("Rank: ", `Rank of PMC option by cost-effectiveness` ),
        #   #       geometry = geometry),
        #   #   fun.geometry = sf::st_point_on_surface,
        #   #   size = 3,
        #   #   alpha = 0.2
        #   # ) +
        #   theme_void() +
        #   theme(
        #     legend.title = element_text(hjust = 0.5)
        #   ) +
        #   scale_fill_gradient2(
        #     low = "#008631",
        #     mid = "#cefad0",
        #     high = "#ffffff",
        #     midpoint = mid_val,
        #     #limits = c(min_val, max_val),   # IMPORTANT
        #     #breaks = c(min_val, mid_val, max_val),
        #     breaks = c(mid_val),
        #     guide = guide_colorbar(reverse = TRUE)
        #   ) +
        #   labs(fill = "Cost-effectiveness\n(incremental cost-effectiveness ratio)\n") + 
        #   theme(text=element_text(size=14),legend.title =element_text(size=14), legend.text = element_text(size=14))
        # 
        
        
        user_input_outputs$icer_map <- icer_map
        
      
        ##########################################################################
        
        # Prioritisation table 
        
        
        
        final_ranked_table_full <- final_ranked_table_full %>% filter(Country==get_iso3(country), `Region (admin-1)`==chosen_area)
        final_ranked_table_full$Country <- country_lookup[final_ranked_table_full$Country] 
        final_ranked_table_full <- final_ranked_table_full %>% dplyr::select(-c( `ICER based on regional costs only (no national costs)`, `Cost-effective? (lower country-specific threshold)`, `Cost-effective? (higher country-specific threshold)`, `Cost-effective? ($250 / DALY averted threshold)`)) %>% 
          rename("Financial (budget) cost of SP+consumables" = "Financial cost of SP and administration consumables" ) %>% 
          rename("Cumulative financial (budget) cost of SP+consumables" = "Cumulative cost of SP and administration consumables")  %>%
          relocate(`Rank of PMC option by cost-effectiveness`, .after = 2)
        
        
        
        
        # final_ranked_table$`Cumulative cost of SP and administration consumables` <- cumsum(final_ranked_table$`Financial cost of SP and administration consumables`)
        # final_ranked_table$`Cumulative economic cost savings to public providers from reduced treatment` <- cumsum(final_ranked_table$`Additional economic cost savings to public providers from reduced treatment`)
        # final_ranked_table$`Cumulative net costs of PMC implementation and treatment` <- cumsum(final_ranked_table$`Additional economic costs of PMC implementation` + final_ranked_table$`Additional net economic costs of PMC implementation and treatment`)
        # 
        
        user_input_outputs$prioritisation_table <- data.frame(final_ranked_table_full, check.names = FALSE) 
        
        #########################################################################
        
        
        
        
        incProgress(1/10)
        # All health outputs 
        
        # Define the new order of columns
        new_column_order <- c("Country", 
                              "Admin-1 unit", 
                              "Age group", 
                              "N doses", 
                              "clinical_cases_no_PMC", 
                              "clinical_cases_no_PMC_per1000", 
                              "clinical_cases_with_PMC", 
                              "clinical_cases_with_PMC_per1000", 
                              "clinical_cases_averted_with_PMC", 
                              "clinical_cases_averted_with_PMC_per1000", 
                              "clinical_cases_reduction", 
                              "severe_cases_no_PMC", 
                              "severe_cases_no_PMC_per1000", 
                              "severe_cases_with_PMC", 
                              "severe_cases_with_PMC_per1000", 
                              "severe_cases_averted_with_PMC", 
                              "severe_cases_averted_with_PMC_per1000", 
                              "severe_cases_reduction" 
                              
        )
        
        new_column_names <- c("Country",                          
                              "Admin-1 unit", 
                              "Age group",
                              "N doses",                 
                              "Clinical cases (no PMC)",             
                              "Clinical cases (no PMC, per 1000)",    
                              "Clinical cases (with PMC)",           
                              "Clinical cases (with PMC, per 1000)",  
                              "Clinical cases averted (with PMC)",   
                              "Clinical cases averted (with PMC, per 1000)",
                              "Clinical cases reduction (%)",          
                              "Severe cases (no PMC)",               
                              "Severe cases (no PMC, per 1000)",      
                              "Severe cases (with PMC)",             
                              "Severe cases (with PMC, per 1000)",    
                              "Severe cases averted (with PMC)",     
                              "Severe cases averted (with PMC, per 1000)",
                              "Severe cases reduction (%)"            
                              
        )
        
        
        #########################################################################
        
        # All health outputs (annual, all admin-1 areas, total age group)
        
        #merged_df_annual_COMPLETE$`Age group` <- rep(c("0-30mo"), times= dim(merged_df_annual_COMPLETE)[1])
        
        merged_df_annual_COMPLETE <- merged_df_annual_COMPLETE %>% filter(`Admin-1 unit` == chosen_area)
        
        merged_df_annual_COMPLETE[, c("clinical_cases_no_PMC", 
                                      "clinical_cases_no_PMC_per1000", 
                                      "clinical_cases_with_PMC", 
                                      "clinical_cases_with_PMC_per1000", 
                                      "clinical_cases_averted_with_PMC", 
                                      "clinical_cases_averted_with_PMC_per1000", 
                                      "clinical_cases_reduction", 
                                      "severe_cases_no_PMC", 
                                      "severe_cases_no_PMC_per1000", 
                                      "severe_cases_with_PMC", 
                                      "severe_cases_with_PMC_per1000", 
                                      "severe_cases_averted_with_PMC", 
                                      "severe_cases_averted_with_PMC_per1000", 
                                      "severe_cases_reduction" 
        )] <- lapply(merged_df_annual_COMPLETE[, c("clinical_cases_no_PMC", 
                                                   "clinical_cases_no_PMC_per1000", 
                                                   "clinical_cases_with_PMC", 
                                                   "clinical_cases_with_PMC_per1000", 
                                                   "clinical_cases_averted_with_PMC", 
                                                   "clinical_cases_averted_with_PMC_per1000", 
                                                   "clinical_cases_reduction", 
                                                   "severe_cases_no_PMC", 
                                                   "severe_cases_no_PMC_per1000", 
                                                   "severe_cases_with_PMC", 
                                                   "severe_cases_with_PMC_per1000", 
                                                   "severe_cases_averted_with_PMC", 
                                                   "severe_cases_averted_with_PMC_per1000", 
                                                   "severe_cases_reduction" 
        )], function(x) round(as.numeric(x), digits = 1))
        
        
        
        annual_health_output_table <- merged_df_annual_COMPLETE[, new_column_order]
        colnames(annual_health_output_table) <- new_column_names
        annual_health_output_table$`Age group` <- rep(c("0-30mo"), times= dim(annual_health_output_table)[1])
        
        user_input_outputs$annual_health_output_table <- data.frame(annual_health_output_table, check.names = FALSE) 
        
        
        #########################################################################
        incProgress(1/10)
        
        # All health outputs (annual, 6 month age groups)
        
        #merged_df_annual_sixmonths_COMPLETE$`Age group` <- rep(c("0-6mo", "6-12mo", "12-18mo", "18-24mo", "24-30mo"), times=dim(merged_df_annual_sixmonths_COMPLETE)[1]/5)
        merged_df_annual_sixmonths_COMPLETE <- merged_df_annual_sixmonths_COMPLETE %>% filter(`Admin-1 unit` == chosen_area)
        
        merged_df_annual_sixmonths_COMPLETE[, c("clinical_cases_no_PMC", 
                                                "clinical_cases_no_PMC_per1000", 
                                                "clinical_cases_with_PMC", 
                                                "clinical_cases_with_PMC_per1000", 
                                                "clinical_cases_averted_with_PMC", 
                                                "clinical_cases_averted_with_PMC_per1000", 
                                                "clinical_cases_reduction", 
                                                "severe_cases_no_PMC", 
                                                "severe_cases_no_PMC_per1000", 
                                                "severe_cases_with_PMC", 
                                                "severe_cases_with_PMC_per1000", 
                                                "severe_cases_averted_with_PMC", 
                                                "severe_cases_averted_with_PMC_per1000", 
                                                "severe_cases_reduction" 
                                                
        )] <- lapply(merged_df_annual_sixmonths_COMPLETE[, c("clinical_cases_no_PMC", 
                                                             "clinical_cases_no_PMC_per1000", 
                                                             "clinical_cases_with_PMC", 
                                                             "clinical_cases_with_PMC_per1000", 
                                                             "clinical_cases_averted_with_PMC", 
                                                             "clinical_cases_averted_with_PMC_per1000", 
                                                             "clinical_cases_reduction", 
                                                             "severe_cases_no_PMC", 
                                                             "severe_cases_no_PMC_per1000", 
                                                             "severe_cases_with_PMC", 
                                                             "severe_cases_with_PMC_per1000", 
                                                             "severe_cases_averted_with_PMC", 
                                                             "severe_cases_averted_with_PMC_per1000", 
                                                             "severe_cases_reduction" 
                                                             
        )], function(x) round(as.numeric(x), digits = 1))
        
        
        
        annual_health_output_sixmonths_table <-  merged_df_annual_sixmonths_COMPLETE[, new_column_order]
        colnames(annual_health_output_sixmonths_table) <- new_column_names
        annual_health_output_sixmonths_table$`Age group` <- rep(c("0-6mo", "6-12mo", "12-18mo", "18-24mo", "24-30mo"), times=dim(annual_health_output_sixmonths_table)[1]/5)
        user_input_outputs$annual_health_output_sixmonths_table <- data.frame(annual_health_output_sixmonths_table, check.names = FALSE) 
        
        #########################################################################
        
        # Cost data 
        
        cost_table <- (economic_summary_df %>% filter(`Iso code` == get_iso3(country),  `Region (admin-1 unit)`== chosen_area))[,-c(11:18, 30)]
        
        cost_table$`Iso code` <- country_lookup[cost_table$`Iso code`]
        cost_table$`Country` <- cost_table$`Iso code`
        cost_table <- cost_table %>% dplyr::select(-`Iso code`) %>% relocate(`Country`)
        
        
        
        user_input_outputs$cost_data_table <- data.frame(cost_table,check.names = FALSE)  
        
        #########################################################################
        
        
        # DALYS averted table 
      
        #dalys_table <- (economic_summary_df %>% filter(`Iso code` == get_iso3(country),  `Region (admin-1 unit)`== chosen_area))[,c(1,2,3,4,5,15:18)]
        
        
        dalys_table <- economic_summary_df %>%
          filter(`Iso code` == get_iso3(country),  `Region (admin-1 unit)`== chosen_area) %>%
          dplyr::select(1, 2, 3, 4, 5, 15:18) %>%
          
          mutate(
            Country = country_lookup[`Iso code`]
          ) %>%
          dplyr::select(-`Iso code`) %>%
          relocate(Country) %>%
          
          group_by(`Region (admin-1 unit)`) %>%
          
          summarise(
            Country = first(Country),
            
            `Total DALYs averted` =
              diff(`Total DALYs`),
            
            `DALYs averted from clinical cases` =
              diff(`DALYs from clinical cases`),
            
            `DALYs averted from hospitalisations` =
              diff(`DALYs from hospitalisations`),
            
            `DALYs averted from deaths` =
              diff(`DALYs from deaths`),
            
            .groups = "drop"
          )
        
        dalys_table <- dalys_table%>% relocate(`Country`)
        
        # dalys_table$`Iso code` <- country_lookup[dalys_table$`Iso code`]
        # dalys_table$`Country` <- dalys_table$`Iso code`
        # dalys_table <- dalys_table %>% dplyr::select(-`Iso code`) %>% relocate(`Country`)
        
        
        
        user_input_outputs$dalys_table <- data.frame(dalys_table ,check.names = FALSE) 
        
        
        
        # cost effectiveness thresholds 
        
        cet_table <- (cet_thresholds %>% filter(Name == country))[,c(1,3,4)]
        names(cet_table) <- c("Country", "Cost-effectiveness threshold (lower)", "Cost-effectiveness threshold (upper)")
        
        user_input_outputs$cet_table <- data.frame(cet_table, check.names = FALSE)  
        
        ################################################################
        # bar plot
        ################################################################
        
        # Reshape data to long format
        plot_df <- annual_health_output_table %>%
          select(`Admin-1 unit`,
                 `Clinical cases (no PMC)`,
                 `Clinical cases (with PMC)`) %>%
          pivot_longer(
            cols = c(`Clinical cases (no PMC)`, `Clinical cases (with PMC)`),
            names_to = "Scenario",
            values_to = "Cases"
          ) %>%
          mutate(
            Scenario = recode(Scenario,
                              "Clinical cases (no PMC)" = "No PMC",
                              "Clinical cases (with PMC)" = "PMC co-delivered with EPI")
          )
        
        # Plot: two bars per Admin-1
        bar_plot <- ggplot(plot_df, aes(x = `Admin-1 unit`, y = Cases, fill = Scenario)) +
          geom_col(position = position_dodge(width = 0.7), width = 0.6) +
          labs(
            x = "Admin-1 unit",
            y = "Estimated new clinical cases in age group (0-30mo)",
            fill = "Scenario"
          ) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1)
          )
        
        user_input_outputs$bar_plot <- bar_plot 
        
      })
    }



    ##### GRAPH FOR WHOLE COUNTRY #####
    
    if (input$country_or_area == "Whole country") {
      
      withProgress(message = "Preparing results, please wait", {
        
        

        # Clinical incidence graph
        
        # Initialize the ggplot object
        clinical_inc_graphs <- ggplot()
        
        names_in_plot <- c()
        

        
        # Iterate over columns starting with number_of_doses
        for (i in 1:length(number_of_doses)) {
          
          PMC_impact_ppy <- PMC_impact_ppy_whole_country 
          incidence_ppy_df <- incidence_ppy_df_whole_country
          
          # get first 5 letters from each in 
          # vaccines_text <- unique(
          #   (coverage_df_country %>% filter(Country == country))$vaccines_by_age
          # )
          # 
          # vaccines_vec <- trimws(unlist(strsplit(vaccines_text, ",")))
          # 
          # vaccines_dosed <- paste0(
          #   "Dose ",
          #   seq_along(vaccines_vec),
          #   ": ",
          #   vaccines_vec,
          #   collapse = "\n"
          # )
          # 
          legend_label <- paste0(
            "Dose ",
            seq_along(schedule),
            ": ",
            round(schedule / 30.4167, 1),
            " months",
            " (",
            cov * 100,
            "% coverage)"
          ) |> 
            paste(collapse = "\n")
          
          
          incProgress(1/10)
          
          names_in_plot <- c(names_in_plot, legend_label)
          
          clinical_inc_graphs <- clinical_inc_graphs +
            geom_line(aes_string(x = PMC_impact_ppy$age_in_days_midpoint, 
                                 y = 1000 * PMC_impact_ppy$clinical, 
                                 colour = paste0("\"", legend_label, "\"")), 
                      linewidth = 0.6) 
          
          legend_label2 <- paste0(legend_label) #paste0("Cases averted, ", number_of_doses[i], " doses")
          names_in_plot <- c(names_in_plot, legend_label2)
          
          clinical_inc_graphs <- clinical_inc_graphs + 
            geom_ribbon(aes_string(x = PMC_impact_ppy$age_in_days_midpoint, 
                                   ymin = 1000 * PMC_impact_ppy$clinical, 
                                   ymax = 1000 * incidence_ppy_df$clinical, 
                                   fill = paste0("\"", legend_label2, "\"")), 
                        alpha = 0.3, show.legend = TRUE)
        }
        
        
        clinical_inc_graphs <- clinical_inc_graphs + 
          geom_line(aes_string(x = PMC_impact_ppy$age_in_days_midpoint, 
                               y = 1000 * incidence_ppy_df$clinical, 
                               colour = "\"No PMC\""))
        
        
        
        names(clinical_inc_graphs$layers) <- c(names_in_plot, "No PMC")
        
        clinical_inc_graphs <- ggplot() + clinical_inc_graphs$layers +
          labs(x = "Age (months)", y = "New clinical infections per 1000 children", colour = "Incidence by delivery model [select/deselect]", fill = "Cases averted", title = paste0(country)) +
          ylim(0, max((1000*incidence_ppy_df$clinical)) + 500) +
          
          scale_x_continuous(
            breaks = c(0, 183, 365, 549, 730, 913),
            labels = c("0", "6", "12", "18", "24", "30")
          ) + 
          scale_fill_manual("Cases averted", breaks = names_in_plot[c(seq(from=2, to=length(names_in_plot), by=2))], values = colours[2:5]) +
          scale_color_manual("Incidence by delivery model [select/deselect]", breaks = c("No PMC", names_in_plot[c(seq(from=1, to=length(names_in_plot), by=2))]), values = colours[1:5]) + 
          theme(panel.grid.major = element_line(colour = "gray", linewidth = 0.2), 
                panel.background = element_rect(fill = 'transparent'), 
                plot.background = element_rect(fill = 'transparent', color = NA), 
                legend.background = element_rect(fill = 'transparent')) 
        
        
        clinical_inc_graphs_plotly <- ggplotly(clinical_inc_graphs)
        
        
        for (i in 1:length(clinical_inc_graphs_plotly$x$data)){
          if (!is.null(clinical_inc_graphs_plotly$x$data[[i]]$name)){
            clinical_inc_graphs_plotly$x$data[[i]]$name <- gsub("^\\(", "", clinical_inc_graphs_plotly$x$data[[i]]$name)
            clinical_inc_graphs_plotly$x$data[[i]]$name <- gsub(",1)", "", clinical_inc_graphs_plotly$x$data[[i]]$name)
            
          }
        }
        
        text_x <- paste0("Age: ", clinical_inc_graphs_plotly$x$data[[i]]$x)
        text_y <- paste0("Clinical incidence per 1000: ", round(clinical_inc_graphs_plotly$x$data[[i]]$y, digits=1))
        text_z <- Map(function(x, y) paste0("", x, " days\n", y, ""), text_x, text_y)
        
        
        
        clinical_inc_graphs_plotly <- clinical_inc_graphs_plotly %>%
          style(hoverinfo = "skip", traces = c(3, 5, 7, 9)) %>% 
          style(text = unlist(text_z))
        
        # Assign the plot to the user_input_outputs
        user_input_outputs$clinical_incidence_graph <- clinical_inc_graphs_plotly
        
        ##########################################################################
        incProgress(1/10)
        # severe incidence graph 
        
        # Initialize the ggplot object
        severe_inc_graphs <- ggplot()
        
        names_in_plot <- c()
        
        # Iterate over columns starting with number_of_doses
        for (i in 1:length(number_of_doses)) {
          
          PMC_impact_ppy <- PMC_impact_ppy_whole_country 
          incidence_ppy_df <- incidence_ppy_df_whole_country
          
          # get first 5 letters from each in 
          # vaccines_text <- unique(
          #   (coverage_df_country %>% filter(Country == country))$vaccines_by_age
          # )
          # 
          # vaccines_vec <- trimws(unlist(strsplit(vaccines_text, ",")))
          # 
          # vaccines_dosed <- paste0(
          #   "Dose ",
          #   seq_along(vaccines_vec),
          #   ": ",
          #   vaccines_vec,
          #   collapse = "\n"
          # )
          # 
          # legend_label <- paste0(
          #   unique((coverage_df_country %>% filter(Country == country))$Scenario),
          #   ":\n",
          #   vaccines_dosed
          # )
          
          legend_label <- paste0(
            "Dose ",
            seq_along(schedule),
            ": ",
            round(schedule / 30.4167, 1),
            " months",
            " (",
            cov * 100,
            "% coverage)"
          ) |> 
            paste(collapse = "\n")
          
          names_in_plot <- c(names_in_plot, legend_label)
          
          severe_inc_graphs <- severe_inc_graphs +
            geom_line(aes_string(x = PMC_impact_ppy$age_in_days_midpoint, 
                                 y = 1000 * PMC_impact_ppy$severe, 
                                 colour = paste0("\"", legend_label, "\"")), 
                      linewidth = 0.6) 
          
          legend_label2 <- paste0(legend_label) #paste0("Cases averted, ", number_of_doses[i], " doses")
          names_in_plot <- c(names_in_plot, legend_label2)
          
          severe_inc_graphs <- severe_inc_graphs + 
            geom_ribbon(aes_string(x = PMC_impact_ppy$age_in_days_midpoint, 
                                   ymin = 1000 * PMC_impact_ppy$severe, 
                                   ymax = 1000 * incidence_ppy_df$severe, 
                                   fill = paste0("\"", legend_label2, "\"")), 
                        alpha = 0.3, show.legend = TRUE)
        }
        
        
        severe_inc_graphs <- severe_inc_graphs + 
          geom_line(aes_string(x = PMC_impact_ppy$age_in_days_midpoint, 
                               y = 1000 * incidence_ppy_df$severe, 
                               colour = "\"No PMC\""))
        
        
        
        names(severe_inc_graphs$layers) <- c(names_in_plot, "No PMC")
        
        severe_inc_graphs <- ggplot() + severe_inc_graphs$layers +
          labs(x = "Age (months)", y = "New hospitalisations per 1000 children", colour = "Hospitalisations by delivery model [select/deselect]", fill = "Hospitalisations averted", title = paste0(country)) +
          ylim(0, max((1000*incidence_ppy_df$severe)) + 100) +
          
          scale_x_continuous(
            breaks = c(0, 183, 365, 549, 730, 913),
            labels = c("0", "6", "12", "18", "24", "30")
          ) + 
          scale_fill_manual("Hospitalisations averted", breaks = names_in_plot[c(seq(from=2, to=length(names_in_plot), by=2))], values = colours[2:5]) +
          scale_color_manual("Hospitalisations by delivery model [select/deselect]", breaks = c("No PMC", names_in_plot[c(seq(from=1, to=length(names_in_plot), by=2))]), values = colours[1:5]) + 
          theme(panel.grid.major = element_line(colour = "gray", linewidth = 0.2), 
                panel.background = element_rect(fill = 'transparent'), 
                plot.background = element_rect(fill = 'transparent', color = NA), 
                legend.background = element_rect(fill = 'transparent')) 
        
        
        severe_inc_graphs_plotly <- ggplotly(severe_inc_graphs)
        incProgress(1/10)
        
        
        for (i in 1:length(severe_inc_graphs_plotly$x$data)){
          if (!is.null(severe_inc_graphs_plotly$x$data[[i]]$name)){
            severe_inc_graphs_plotly$x$data[[i]]$name <- gsub("^\\(", "", severe_inc_graphs_plotly$x$data[[i]]$name)
            severe_inc_graphs_plotly$x$data[[i]]$name <- gsub(",1)", "", severe_inc_graphs_plotly$x$data[[i]]$name)
            
          }
        }
        
        text_x <- paste0("Age: ", severe_inc_graphs_plotly$x$data[[i]]$x)
        text_y <- paste0("Hospitalisations per 1000: ", round(severe_inc_graphs_plotly$x$data[[i]]$y, digits=1))
        text_z <- Map(function(x, y) paste0("", x, " days\n", y, ""), text_x, text_y)
        
        
        severe_inc_graphs_plotly <- severe_inc_graphs_plotly %>%
          style(hoverinfo = "skip", traces = c(3, 5, 7, 9)) %>% 
          style(text = unlist(text_z))
        
        # Assign the plot to the user_input_outputs
        user_input_outputs$severe_incidence_graph <- severe_inc_graphs_plotly
        

        #########################################################################
        
        incProgress(1/10)
        # Haplotype data table
        
        # if (input$change_haplotype_data == "Yes") {
        #   
        #   haplotype_data_df <- haplotype_data_for_table()
        #   
        # } else {
        #   
        # haplotype_data_df <- haplotype_data_final %>% filter(Country == country)
        # haplotype_data_df <- haplotype_data_df[c("Country", "Admin-1 unit",  "latitude", "longitude", "I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other")]
        # 
        # 
        # }
        # 
        # 
        # 
        # #haplotype_data_df <- haplotype_data_df[c("Country", "Admin-1 unit",  "latitude", "longitude", "I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "other")]
        # 
        # colnames(haplotype_data_df) <- c("Country", "Admin-1 unit",  "latitude", "longitude", "I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other")
        # 
        # haplotype_data_df[, c("I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other")] <- lapply(haplotype_data_df[, c( "I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other")], function(x) round(as.numeric(x), digits = 3))
        # 
        # for (i in 1:dim(haplotype_data_df)[1]) {
        #   haplotype_data_df[i, "Sum of proportions"] <- sum(unlist(c(haplotype_data_df[i, "I_AKA_"], haplotype_data_df[i, "I_GKA_"], haplotype_data_df[i, "I_GEA_"], haplotype_data_df[i, "I_GEG_"], haplotype_data_df[i, "V_GKA_"], haplotype_data_df[i, "V_GKG_"], haplotype_data_df[i, "Other"])), na.rm = TRUE)
        # }
        # 
        # haplotype_data_df$`Admin-1 unit` <- gsub("_", " ", haplotype_data_df$`Admin-1 unit`)
        # user_input_outputs$haplotype_table <- haplotype_data_df[, c("Country", "Admin-1 unit", "I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other")]
        # 
        
        if (input$change_haplotype_data == "Yes") {
          
          # ---- Get user-edited table (NO lat/long inside it) ----
          df_edit <- haplotype_data_for_table()
          
          # Ensure numeric rounding
          hap_cols <- c(
            "I_AKA_", "I_GKA_", "I_GEA_", 
            "I_GEG_", "V_GKA_", "V_GKG_", "Other"
          )
          
          df_edit[, hap_cols] <- lapply(
            df_edit[, hap_cols],
            function(x) round(as.numeric(x), 3)
          )
          
          # Recalculate Sum of proportions
          df_edit$`Sum of proportions` <-
            rowSums(df_edit[, hap_cols], na.rm = TRUE)
          
          # ---- Reattach latitude & longitude ----
          latlong_df <- haplotype_data() %>%
            dplyr::select(Country, `Admin-1 unit`, latitude, longitude)
          
          haplotype_data_df <- latlong_df %>%
            dplyr::left_join(
              df_edit,
              by = c("Country", "Admin-1 unit")
            )
          
        } else {
          
          haplotype_data_df <- haplotype_data_final %>% 
            dplyr::filter(Country == country)
          
          haplotype_data_df <- haplotype_data_df[
            c("Country", "Admin-1 unit",
              "latitude", "longitude",
              "I_AKA_", "I_GKA_", "I_GEA_",
              "I_GEG_", "V_GKA_", "V_GKG_", "Other")
          ]
          
          hap_cols <- c(
            "I_AKA_", "I_GKA_", "I_GEA_", 
            "I_GEG_", "V_GKA_", "V_GKG_", "Other"
          )
          
          haplotype_data_df[, hap_cols] <- lapply(
            haplotype_data_df[, hap_cols],
            function(x) round(as.numeric(x), 3)
          )
          
          haplotype_data_df$`Sum of proportions` <-
            rowSums(haplotype_data_df[, hap_cols], na.rm = TRUE)
        }
        
        # ---- Clean Admin-1 formatting ----
        haplotype_data_df$`Admin-1 unit` <-
          gsub("_", " ", haplotype_data_df$`Admin-1 unit`)
        
        # ---- Final object used downstream ----
        user_input_outputs$haplotype_table <- haplotype_data_df[
          c("Country", "Admin-1 unit",
            "I_AKA_", "I_GKA_", "I_GEA_",
            "I_GEG_", "V_GKA_", "V_GKG_",
            "Other")
        ]
        
        
        ##########################################################################
        
        
        # Haplotype map 
        # 
        # names(adm1)[names(adm1) == "NAME_2"] <- "Region"
        # adm1$NAME_2 <- adm1$Region
        # 
        # 
        # haplotype_proportions_map <- ggplot(adm1) + 
        #   geom_sf() + 
        #   theme_bw() +
        #   theme(
        #     legend.text = element_text(size = 12),
        #     legend.title = element_text(size = 15),
        #     title = element_text(size = 20)
        #   ) +
        #   scatterpie::geom_scatterpie(
        #     data = haplotype_data_df,
        #     cols = c("I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other"),
        #     aes(x = longitude, y = latitude, group = Region, r = 0.13 * min(diff(range(haplotype_data_df$longitude)), diff(range(haplotype_data_df$latitude)))) # Reduced radius from 0.8 to 0.3
        #   ) +
        #   scale_fill_manual(
        #     values = c(  "#00C094", "#00B6EB", "#FFA500","#F8766D","#b7a1ff", "#7361b3","#D2B48C"),
        #     name = "Haplotypes",
        #     breaks = c("I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other"),
        #     labels = c("I_AKA_", "I_GKA_", "I_GEA_", "I_GEG_", "V_GKA_", "V_GKG_", "Other")
        #   ) +
        #   theme(
        #     axis.title.x = element_blank(),
        #     axis.text.x = element_blank(),
        #     axis.ticks.x = element_blank(),
        #     axis.title.y = element_blank(),
        #     axis.text.y = element_blank(),
        #     axis.ticks.y = element_blank(),
        #     panel.background = element_blank(),
        #     panel.border = element_blank(),
        #     panel.grid.major = element_blank(),
        #     panel.grid.minor = element_blank(),
        #     plot.background = element_blank()
        #   )
        # 
        
        adm1<- sf::st_read(paste0("", get_iso3(country), "/shp/gadm41_", get_iso3(country), "_1.shp"))
        adm1$`Admin-1 unit` <- stri_trans_general(str=gsub("-", "_", adm1$NAME_1), id = "Latin-ASCII")
        adm1$`Admin-1 unit` <- stri_trans_general(str = gsub(" ", "_", adm1$`Admin-1 unit`), id = "Latin-ASCII")
        
        #adm1$`Admin-1 unit` <- adm1$NAME_2
        # adm1$NAME_2 <- stri_trans_general(str = gsub(" ", "_", adm1$NAME_2), id = "Latin-ASCII")
        # adm1$`Admin-1 unit` <- adm1$NAME_2
        combined_protective_efficacy_30_days <- merge(protective_efficacy_30_days %>% filter(Country == country), adm1, by="Admin-1 unit") 
        
        
        haplotype_proportions_map <- ggplot(combined_protective_efficacy_30_days) + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
          theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
          geom_sf(data=adm1)+
          geom_sf(aes(fill=day_30_protect_efficacy, geometry=geometry), size=0.5)  +
          geom_sf_label(aes(label = paste0(`Admin-1 unit`), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
          ggtitle(paste0("Average 30 day protective efficacy following a SP dose: ", country)) + labs(fill="30 day protective efficacy") +
          scale_fill_viridis_c(limits=c(min(combined_protective_efficacy_30_days$day_30_protect_efficacy),max(combined_protective_efficacy_30_days$day_30_protect_efficacy))) + 
          theme(text=element_text(size=14),legend.title =element_text(size=14), legend.text = element_text(size=14),
                axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                panel.background=element_blank(),
                panel.border=element_blank(),
                panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),
                plot.background=element_blank())
        
        
        user_input_outputs$haplotype_map <- haplotype_proportions_map 
        
        
        #########################################################################
        incProgress(1/10)
        # Probability of drug protection
        
        # Initialize the ggplot object
        prob_drug_protect_plot <- ggplot()
        names_in_plot <- c()
        
        # Iterate over columns starting with 'prot_overall_'
        for (col in names(df_drug_protect)) {
          if (startsWith(col, 'prot_overall_')) {
            
            legend_label <- gsub('prot_overall_', '', col)
            legend_label <- gsub('_', ' ', legend_label)
            names_in_plot <- c(names_in_plot, legend_label)
            
            # Create geom_line for each column
            print(legend_label)
            prob_drug_protect_plot <- prob_drug_protect_plot + geom_line(aes_string(x = df_drug_protect$time, y = paste0("df_drug_protect[[\"", col, "\"]]"), colour = paste0("\"", legend_label, "\"")), linewidth=1)
            
          }
          
          
        }
        
        # Customize legend and labels
        prob_drug_protect_plot <- prob_drug_protect_plot + 
          labs(x = "Days since SP dose", y = "Probability of drug protection", colour= "Admin-1 unit",
               title = "Probability of drug protection following an SP dose") +
          theme(axis.text = element_text(size = 12),
                axis.title = element_text(size = 14),
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))
        
        names(prob_drug_protect_plot$layers) <- names_in_plot
        
        
        user_input_outputs$prob_drug_protect_graph <- ggplotly(prob_drug_protect_plot) 
        
        
        prob_drug_protect_plotly <- user_input_outputs$prob_drug_protect_graph
        
        
        for (i in 1:length(prob_drug_protect_plotly$x$data)) {
          
          if (!is.null(prob_drug_protect_plotly$x$data[[i]]$name)) {
            
            prob_drug_protect_plotly$x$data[[i]]$name <-
              gsub("^\\(", "", prob_drug_protect_plotly$x$data[[i]]$name)
            
            prob_drug_protect_plotly$x$data[[i]]$name <-
              gsub(",1\\)$", "", prob_drug_protect_plotly$x$data[[i]]$name)
            
          }
        }
        
        for (i in 1:length(prob_drug_protect_plotly$x$data)) {
          
          # Build hover text
          text_x <- paste0(
            "Days since SP dose: ",
            prob_drug_protect_plotly$x$data[[i]]$x
          )
          
          text_y <- paste0(
            "Probability of protection: ",
            round(prob_drug_protect_plotly$x$data[[i]]$y, 3)
          )
          
          text_xy <- Map(
            function(x, y) paste0(x, "\n", y),
            text_x,
            text_y
          )
          
          # Attach hover text to trace
          prob_drug_protect_plotly$x$data[[i]]$text <- unlist(text_xy)
          prob_drug_protect_plotly$x$data[[i]]$hoverinfo <- "text"
        }
        
        user_input_outputs$prob_drug_protect_graph <-
          prob_drug_protect_plotly
        
        #########################################################################
        ## ----------------------------------------------------
        ## ICERS map
        ## ----------------------------------------------------
        
        
        adm1$`Region (admin-1)` <- adm1$`Admin-1 unit`
        
        final_ranked_table_full <- final_ranked_table_full %>%
          mutate(
            `Rank of PMC option by cost-effectiveness` = row_number()
          )
        
        final_ranked_table_full_EDITABLE <- final_ranked_table_full
        
        adm1_merged <- left_join(adm1, final_ranked_table_full , by = "Region (admin-1)")
        
      
        # Extract values
        icer_vals <- adm1_merged$`ICER based on regional costs only (no national costs)`
        
        min_val <- floor(min(icer_vals, na.rm = TRUE) / 10) * 10
        max_val <- ceiling(max(icer_vals, na.rm = TRUE) / 10) * 10
        mid_val <- round((min_val + max_val) / 2)
        


        icer_map <- ggplot(adm1_merged) +
          geom_sf(data = adm1) +
          geom_sf(aes(fill = `ICER based on regional costs only (no national costs)`,
                      geometry = geometry),
                  size = 0.5) +
          geom_sf_label(
            aes(label = paste0("Rank: ", `Rank of PMC option by cost-effectiveness` ),
                geometry = geometry),
            fun.geometry = sf::st_point_on_surface,
            size = 3,
            alpha = 0.2
          ) +
          theme_void() +
          theme(
            legend.title = element_text(hjust = 0.5)
          ) +
          scale_fill_gradient2(
            low = "#008631",
            mid = "#cefad0",
            high = "#ffffff",
            midpoint = mid_val,
            limits = c(min_val, max_val),   # IMPORTANT
            breaks = c(min_val, mid_val, max_val),
            labels = c(
              paste0(comma(min_val), " (most cost-effective, rank=1)"),
              comma(mid_val),
              paste0(comma(max_val), " (least cost-effective, rank=", max(adm1_merged$`Rank of PMC option by cost-effectiveness` ), ")")
            ),
            guide = guide_colorbar(reverse = TRUE)
          ) +
          labs(fill = "Cost-effectiveness\n(incremental cost-effectiveness ratio)\n") + 
          theme(text=element_text(size=14),legend.title =element_text(size=14), legend.text = element_text(size=14))
        
        
        
        user_input_outputs$icer_map <- icer_map 
        
        # Prioritisation table 
        
        final_ranked_table_full <- final_ranked_table_full %>% filter(Country==get_iso3(country))
        final_ranked_table_full$Country <- country_lookup[final_ranked_table_full$Country] 
        final_ranked_table_full <- final_ranked_table_full %>% dplyr::select(-c( `ICER based on regional costs only (no national costs)`, `Cost-effective? (lower country-specific threshold)`, `Cost-effective? (higher country-specific threshold)`, `Cost-effective? ($250 / DALY averted threshold)`)) %>% 
          rename("Financial (budget) cost of SP+consumables" = "Financial cost of SP and administration consumables" ) %>% 
          rename("Cumulative financial (budget) cost of SP+consumables" = "Cumulative cost of SP and administration consumables")  %>%
          relocate(`Rank of PMC option by cost-effectiveness`, .after = 2)
        
        
        # final_ranked_table$`Cumulative economic costs of PMC implementation` <- cumsum(final_ranked_table$`Additional economic costs of PMC implementation`)
        # final_ranked_table$`Cumulative treatment cost savings (economic) to public providers` <- cumsum(final_ranked_table$`Additional economic cost savings to public providers from reduced treatment`)
        # final_ranked_table$`Cumulative economic costs of implementation and treatment` <- cumsum(final_ranked_table$`Additional economic costs of PMC implementation` + final_ranked_table$`Additional economic cost savings to public providers from reduced treatment`)
        # 
        
        user_input_outputs$prioritisation_table <- data.frame(final_ranked_table_full, check.names = FALSE) 
        
        #########################################################################
        incProgress(1/10)
        
        
        # All health outputs 
        
        # Define the new order of columns
        new_column_order <- c("Country", 
                              "Admin-1 unit", 
                              "Age group", 
                              "N doses", 
                              "clinical_cases_no_PMC", 
                              "clinical_cases_no_PMC_per1000", 
                              "clinical_cases_with_PMC", 
                              "clinical_cases_with_PMC_per1000", 
                              "clinical_cases_averted_with_PMC", 
                              "clinical_cases_averted_with_PMC_per1000", 
                              "clinical_cases_reduction", 
                              "severe_cases_no_PMC", 
                              "severe_cases_no_PMC_per1000", 
                              "severe_cases_with_PMC", 
                              "severe_cases_with_PMC_per1000", 
                              "severe_cases_averted_with_PMC", 
                              "severe_cases_averted_with_PMC_per1000", 
                              "severe_cases_reduction" 
                              
        )
        
        new_column_names <- c("Country",                          
                              "Admin-1 unit", 
                              "Age group",
                              "N doses",                 
                              "Clinical cases (no PMC)",             
                              "Clinical cases (no PMC, per 1000)",    
                              "Clinical cases (with PMC)",           
                              "Clinical cases (with PMC, per 1000)",  
                              "Clinical cases averted (with PMC)",   
                              "Clinical cases averted (with PMC, per 1000)",
                              "Clinical cases reduction (%)",          
                              "Severe cases (no PMC)",               
                              "Severe cases (no PMC, per 1000)",      
                              "Severe cases (with PMC)",             
                              "Severe cases (with PMC, per 1000)",    
                              "Severe cases averted (with PMC)",     
                              "Severe cases averted (with PMC, per 1000)",
                              "Severe cases reduction (%)"            
                              
        )
        
        
        #########################################################################
        
        # All health outputs (annual, all admin-1 areas, total age group)
        
        #merged_df_annual_COMPLETE$`Age group` <- rep(c("0-30mo"), times= dim(merged_df_annual_COMPLETE)[1])
        merged_df_annual_COMPLETE[, c("clinical_cases_no_PMC", 
                                      "clinical_cases_no_PMC_per1000", 
                                      "clinical_cases_with_PMC", 
                                      "clinical_cases_with_PMC_per1000", 
                                      "clinical_cases_averted_with_PMC", 
                                      "clinical_cases_averted_with_PMC_per1000", 
                                      "clinical_cases_reduction", 
                                      "severe_cases_no_PMC", 
                                      "severe_cases_no_PMC_per1000", 
                                      "severe_cases_with_PMC", 
                                      "severe_cases_with_PMC_per1000", 
                                      "severe_cases_averted_with_PMC", 
                                      "severe_cases_averted_with_PMC_per1000", 
                                      "severe_cases_reduction" 
                                      
        )] <- lapply(merged_df_annual_COMPLETE[, c("clinical_cases_no_PMC", 
                                                   "clinical_cases_no_PMC_per1000", 
                                                   "clinical_cases_with_PMC", 
                                                   "clinical_cases_with_PMC_per1000", 
                                                   "clinical_cases_averted_with_PMC", 
                                                   "clinical_cases_averted_with_PMC_per1000", 
                                                   "clinical_cases_reduction", 
                                                   "severe_cases_no_PMC", 
                                                   "severe_cases_no_PMC_per1000", 
                                                   "severe_cases_with_PMC", 
                                                   "severe_cases_with_PMC_per1000", 
                                                   "severe_cases_averted_with_PMC", 
                                                   "severe_cases_averted_with_PMC_per1000", 
                                                   "severe_cases_reduction" 
                                                   
        )], function(x) round(as.numeric(x), digits = 1))
        
        annual_health_output_table <- merged_df_annual_COMPLETE[, new_column_order]
        colnames(annual_health_output_table) <- new_column_names
        annual_health_output_table$`Age group` <- rep(c("0-30mo"), times= dim(annual_health_output_table)[1])
        
        user_input_outputs$annual_health_output_table <- data.frame(annual_health_output_table, check.names = FALSE) 
        
        
        #########################################################################
        
        # All health outputs (annual, 6 month age groups)
        
        #merged_df_annual_sixmonths_COMPLETE$`Age group` <- rep(c("0-6mo", "6-12mo", "12-18mo", "18-24mo", "24-30mo"), times=dim(merged_df_annual_sixmonths_COMPLETE)[1]/5)
        
        merged_df_annual_sixmonths_COMPLETE[, c("clinical_cases_no_PMC", 
                                                "clinical_cases_no_PMC_per1000", 
                                                "clinical_cases_with_PMC", 
                                                "clinical_cases_with_PMC_per1000", 
                                                "clinical_cases_averted_with_PMC", 
                                                "clinical_cases_averted_with_PMC_per1000", 
                                                "clinical_cases_reduction", 
                                                "severe_cases_no_PMC", 
                                                "severe_cases_no_PMC_per1000", 
                                                "severe_cases_with_PMC", 
                                                "severe_cases_with_PMC_per1000", 
                                                "severe_cases_averted_with_PMC", 
                                                "severe_cases_averted_with_PMC_per1000", 
                                                "severe_cases_reduction" 
                                                
        )] <- lapply(merged_df_annual_sixmonths_COMPLETE[, c("clinical_cases_no_PMC", 
                                                             "clinical_cases_no_PMC_per1000", 
                                                             "clinical_cases_with_PMC", 
                                                             "clinical_cases_with_PMC_per1000", 
                                                             "clinical_cases_averted_with_PMC", 
                                                             "clinical_cases_averted_with_PMC_per1000", 
                                                             "clinical_cases_reduction", 
                                                             "severe_cases_no_PMC", 
                                                             "severe_cases_no_PMC_per1000", 
                                                             "severe_cases_with_PMC", 
                                                             "severe_cases_with_PMC_per1000", 
                                                             "severe_cases_averted_with_PMC", 
                                                             "severe_cases_averted_with_PMC_per1000", 
                                                             "severe_cases_reduction" 
                                                             
        )], function(x) round(as.numeric(x), digits = 1))
        
        
        
        annual_health_output_sixmonths_table <-  merged_df_annual_sixmonths_COMPLETE[, new_column_order]
        colnames(annual_health_output_sixmonths_table) <- new_column_names
        annual_health_output_sixmonths_table$`Age group` <- rep(c("0-6mo", "6-12mo", "12-18mo", "18-24mo", "24-30mo"), times=dim(annual_health_output_sixmonths_table)[1]/5)
        
        user_input_outputs$annual_health_output_sixmonths_table <- data.frame(annual_health_output_sixmonths_table, check.names = FALSE) 
        incProgress(1/10)
        
        #########################################################################
        
        # All health outputs (annual, whole country)
        
        #whole_country_COMPLETE$`Age group` <- rep(c("0-30mo"), times= dim(whole_country_COMPLETE)[1])
        whole_country_COMPLETE[, c("clinical_cases_no_PMC", 
                                   "clinical_cases_no_PMC_per1000", 
                                   "clinical_cases_with_PMC", 
                                   "clinical_cases_with_PMC_per1000", 
                                   "clinical_cases_averted_with_PMC", 
                                   "clinical_cases_averted_with_PMC_per1000", 
                                   "clinical_cases_reduction", 
                                   "severe_cases_no_PMC", 
                                   "severe_cases_no_PMC_per1000", 
                                   "severe_cases_with_PMC", 
                                   "severe_cases_with_PMC_per1000", 
                                   "severe_cases_averted_with_PMC", 
                                   "severe_cases_averted_with_PMC_per1000", 
                                   "severe_cases_reduction" 
                                   
        )] <- lapply(whole_country_COMPLETE[, c("clinical_cases_no_PMC", 
                                                "clinical_cases_no_PMC_per1000", 
                                                "clinical_cases_with_PMC", 
                                                "clinical_cases_with_PMC_per1000", 
                                                "clinical_cases_averted_with_PMC", 
                                                "clinical_cases_averted_with_PMC_per1000", 
                                                "clinical_cases_reduction", 
                                                "severe_cases_no_PMC", 
                                                "severe_cases_no_PMC_per1000", 
                                                "severe_cases_with_PMC", 
                                                "severe_cases_with_PMC_per1000", 
                                                "severe_cases_averted_with_PMC", 
                                                "severe_cases_averted_with_PMC_per1000", 
                                                "severe_cases_reduction" 
                                                
        )], function(x) round(as.numeric(x), digits = 1))
        
        annual_health_output_whole_country_table <- whole_country_COMPLETE[, new_column_order[-(which(new_column_order == "Admin-1 unit"))]]
        colnames(annual_health_output_whole_country_table) <- new_column_names[-(which(new_column_order == "Admin-1 unit"))]
        annual_health_output_whole_country_table$`Age group` <- rep(c("0-30mo"), times= dim(annual_health_output_whole_country_table)[1])
        
        user_input_outputs$annual_health_output_whole_country_table <- data.frame(annual_health_output_whole_country_table, check.names = FALSE) 
        
        
        #########################################################################
        
        # Cost data 
        
        cost_table <- (economic_summary_df %>% filter(`Iso code` == get_iso3(country)))[,-c(11:18, 30)]
        
        cost_table$`Iso code` <- country_lookup[cost_table$`Iso code`]
        
        cost_table$`Country` <- cost_table$`Iso code`
        cost_table <- cost_table %>% dplyr::select(-`Iso code`) %>% relocate(`Country`)
        
        
        user_input_outputs$cost_data_table <- data.frame(cost_table,check.names = FALSE)  
        
        
        # DALYS table 
        
        # DALYS averted table 
        

        dalys_table <- economic_summary_df %>%
          filter(`Iso code` == get_iso3(country)) %>%
          dplyr::select(1, 2, 3, 4, 5, 15:18) %>%
          
          mutate(
            Country = country_lookup[`Iso code`]
          ) %>%
          dplyr::select(-`Iso code`) %>%
          relocate(Country) %>%
          
          group_by(`Region (admin-1 unit)`) %>%
          
          summarise(
            Country = first(Country),
            
            `Total DALYs averted` =
              diff(`Total DALYs`),
            
            `DALYs averted from clinical cases` =
              diff(`DALYs from clinical cases`),
            
            `DALYs averted from hospitalisations` =
              diff(`DALYs from hospitalisations`),
            
            `DALYs averted from deaths` =
              diff(`DALYs from deaths`),
            
            .groups = "drop"
          )
        
        dalys_table <- dalys_table%>% relocate(`Country`)
        
        
        
        user_input_outputs$dalys_table <- data.frame(dalys_table,check.names = FALSE)  
        
        
        #########################################################################
        
        
        # cost effectiveness thresholds 
        
        cet_table <- (cet_thresholds %>% filter(Name == country))[,c(1,3,4)]
        names(cet_table) <- c("Country", "Cost-effectiveness threshold (lower)", "Cost-effectiveness threshold (upper)")
        
        user_input_outputs$cet_table <- data.frame(cet_table  ,check.names = FALSE) 
        
        
        #########################################################################
        

        # Impact on clinical cases  map
        
        #adm1$`Admin-1 unit` <- adm1$NAME_2
        
        # link data with shapefile
        combined_df_with_PMC_reduction <- merge(merged_df_annual_COMPLETE %>% filter(`N doses` == number_of_doses), adm1, by="Admin-1 unit") 
        
        clinical_impact_graph_with_PMC <- ggplot(combined_df_with_PMC_reduction) + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
          theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
          geom_sf(data=adm1)+
          geom_sf(aes(fill=clinical_cases_reduction, geometry=geometry), size=0.5)  +
          geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
          ggtitle(paste0(length(unique(schedule)), " DOSE")) + labs(fill="% reduction in\nclinical cases") +
          scale_fill_viridis_c(limits=c(min(combined_df_with_PMC_reduction$clinical_cases_reduction),max(combined_df_with_PMC_reduction$clinical_cases_reduction))) + 
          theme(text=element_text(size=14),legend.title =element_text(size=14), legend.text = element_text(size=14),
                axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                panel.background=element_blank(),
                panel.border=element_blank(),
                panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),
                plot.background=element_blank())
        
        user_input_outputs$clinical_reduction_map <- clinical_impact_graph_with_PMC
        
        incProgress(1/10)
        
        ##########################################################################
        
        # Impact on hospitalisations map 
        
        severe_impact_graph_with_PMC <- ggplot(combined_df_with_PMC_reduction) + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
          theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
          geom_sf(data=adm1)+
          geom_sf(aes(fill=severe_cases_reduction, geometry=geometry), size=0.5)  +
          geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
          ggtitle(paste0(length(schedule), " DOSE")) + labs(fill="% reduction in\nhospitalisations") +
          scale_fill_viridis_c(limits=c(min(combined_df_with_PMC_reduction$severe_cases_reduction),max(combined_df_with_PMC_reduction$severe_cases_reduction))) + 
          theme(text=element_text(size=14),legend.title =element_text(size=14), legend.text = element_text(size=14),
                axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                panel.background=element_blank(),
                panel.border=element_blank(),
                panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),
                plot.background=element_blank())
        
        user_input_outputs$severe_reduction_map <- severe_impact_graph_with_PMC
        
        
        #########################################################################
        
        
        
        
        clinical_cases_averted_graph_with_PMC <- ggplot(combined_df_with_PMC_reduction) + theme_bw()+  theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
          theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
          geom_sf(data=adm1)+
          geom_sf(aes(fill=clinical_cases_averted_with_PMC_per1000, geometry=geometry), size=0.5)  +
          #geom_sf(data = subset(adm1, (NAME_2 %in% c("Nampula"))), fill = "grey", color = "black", size = 0.5) +
          geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
          ggtitle(paste0(length(schedule), " DOSE")) + labs(fill="Cases averted per 1000") +
          scale_fill_viridis_c(limits=c(min(combined_df_with_PMC_reduction$clinical_cases_averted_with_PMC_per1000),max(combined_df_with_PMC_reduction$clinical_cases_averted_with_PMC_per1000))) + 
          theme(text=element_text(size=14),legend.title =element_text(size=14), legend.text = element_text(size=14),
                axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                panel.background=element_blank(),
                panel.border=element_blank(),
                panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),
                plot.background=element_blank())
        
        user_input_outputs$clinical_cases_averted_map <- clinical_cases_averted_graph_with_PMC    
        
        
        #########################################################################
        
        # Impact on severe cases map (cases averted)
        
        
        severe_cases_averted_graph_with_PMC <- ggplot(combined_df_with_PMC_reduction) + theme_bw()+ theme(legend.text=element_text(size=8), legend.title = element_text(size=10), title = element_text(size=10))+
          theme(legend.key.height = unit(1.5, "cm"),legend.key.width = unit(0.5, "cm")) +
          geom_sf(data=adm1)+
          geom_sf(aes(fill=severe_cases_averted_with_PMC_per1000, geometry=geometry), size=0.5)  +
          geom_sf_label(aes(label = paste0(NAME_1), geometry=geometry),fun.geometry = st_centroid, size=3, alpha = 0.2) +
          ggtitle(paste0(length(schedule), " DOSE")) + labs(fill="Cases averted per 1000") +
          scale_fill_viridis_c(limits=c(min(combined_df_with_PMC_reduction$severe_cases_averted_with_PMC_per1000),max(combined_df_with_PMC_reduction$severe_cases_averted_with_PMC_per1000))) + 
          theme(text=element_text(size=14),legend.title =element_text(size=14), legend.text = element_text(size=14),
                axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                panel.background=element_blank(),
                panel.border=element_blank(),
                panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),
                plot.background=element_blank())
        
        user_input_outputs$severe_cases_averted_map <- severe_cases_averted_graph_with_PMC    
        
        
        
        ################################################################
        # bar plot
        ################################################################
        
        # Reshape data to long format
        plot_df <- annual_health_output_table %>%
          select(`Admin-1 unit`,
                 `Clinical cases (no PMC)`,
                 `Clinical cases (with PMC)`) %>%
          pivot_longer(
            cols = c(`Clinical cases (no PMC)`, `Clinical cases (with PMC)`),
            names_to = "Scenario",
            values_to = "Cases"
          ) %>%
          mutate(
            Scenario = recode(Scenario,
                              "Clinical cases (no PMC)" = "No PMC",
                              "Clinical cases (with PMC)" = "PMC co-delivered with EPI")
          )
        
        # Plot: two bars per Admin-1
        bar_plot <- ggplot(plot_df, aes(x = `Admin-1 unit`, y = Cases, fill = Scenario)) +
          geom_col(position = position_dodge(width = 0.7), width = 0.6) +
          labs(
            x = "Admin-1 unit",
            y = "Estimated new clinical cases in age group (0-30mo)",
            fill = "Scenario"
          ) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1)
          )
        
        user_input_outputs$bar_plot <- bar_plot 
        
        
      })
      
    }



    return(user_input_outputs)

  })


  
  
  
  
  
  
  
  # 
  # output$clinical_incidence_graph_user_input <- renderPlotly({
  #   user_input_output_generation()$clinical_incidence_graph
  # })
  # 
  # 
  # 
  # # render table for dhps haplotype frequencies
  # output$haplotype_table_user_input <- DT::renderDT(DT::datatable(user_input_output_generation()$haplotype_table,
  #                                                                filter = "top", options = list(dom = 'Bftsp', paging = TRUE, pageLength = 10, ordering = TRUE, searching = TRUE, fixedColumns = TRUE, scrollX = TRUE, autoWidth = TRUE, responsive = TRUE, rownames = FALSE, escape = FALSE)))
  # 
  # 
  # # render map of dhps haplotype frequencies
  # output$haplotype_map_user_input <- renderPlot({
  #   user_input_output_generation()$haplotype_map
  # })
  # 
  # output$prob_drug_protect_graph_user_input <- renderPlotly({
  #   user_input_output_generation()$prob_drug_protect_graph
  # })
  # 
  # output$severe_incidence_graph_user_input <- renderPlotly({
  #   user_input_output_generation()$severe_incidence_graph
  # })
  # 
  # # render table for % reduction in clinical incidence by 6 month age groups
  # output$prioritisation_table_user_input <- DT::renderDT(DT::datatable(user_input_output_generation()$prioritisation_table,
  #                                                           filter = "top", options = list(dom = 'Bftsp', paging = TRUE, pageLength = 10, ordering = TRUE, searching = TRUE, fixedColumns = TRUE, scrollX = TRUE, autoWidth = TRUE, responsive = TRUE, rownames = FALSE, escape = FALSE)))
  # 
  # 
  # # render table for health impact (total age group, all admin1 areas, annual)
  # output$annual_health_output_table_user_input <- DT::renderDT(DT::datatable(user_input_output_generation()$annual_health_output_table,
  #                                                                 filter = "top", options = list(dom = 'Bftsp', paging = TRUE, pageLength = 10, ordering = TRUE, searching = TRUE, fixedColumns = TRUE, scrollX = TRUE, autoWidth = TRUE, responsive = TRUE, rownames = FALSE, escape = FALSE)))
  # 
  # 
  # # render table for health impact (total age group, all admin1 areas, annual)
  # output$annual_health_output_sixmonths_table_user_input <- DT::renderDT(DT::datatable(user_input_output_generation()$annual_health_output_sixmonths_table,
  #                                                                           filter = "top", options = list(dom = 'Bftsp', paging = TRUE, pageLength = 10, ordering = TRUE, searching = TRUE, fixedColumns = TRUE, scrollX = TRUE, autoWidth = TRUE, responsive = TRUE, rownames = FALSE, escape = FALSE)))
  # 
  # 
  # # render table for health impact (total age group, all admin1 areas, annual)
  # output$annual_health_output_whole_country_table_user_input <- DT::renderDT(DT::datatable(user_input_output_generation()$annual_health_output_whole_country_table,
  #                                                                               filter = "top", options = list(dom = 'Bftsp', paging = TRUE, pageLength = 10, ordering = TRUE, searching = TRUE, fixedColumns = TRUE, scrollX = TRUE, autoWidth = TRUE, responsive = TRUE, rownames = FALSE, escape = FALSE)))
  # 
  # 
  # # render table for health impact (6month age group, whole country, annual)
  # output$annual_health_output_whole_country_sixmonths_table_user_input <- DT::renderDT(DT::datatable(user_input_output_generation()$annual_health_output_whole_country_sixmonths_table,
  #                                                                                          filter = "top", options = list(dom = 'Bftsp', paging = TRUE, pageLength = 10, ordering = TRUE, searching = TRUE, fixedColumns = TRUE, scrollX = TRUE, autoWidth = TRUE, responsive = TRUE, rownames = FALSE, escape = FALSE)))
  # 
  # 
  # # render table for cost data
  # output$cost_data_table_user_input <- DT::renderDT(DT::datatable(user_input_output_generation()$cost_data_table,
  #                                                                filter = "top", options = list(dom = 'Bftsp', paging = TRUE, pageLength = 10, ordering = TRUE, searching = TRUE, fixedColumns = TRUE, scrollX = TRUE, autoWidth = TRUE, responsive = TRUE, rownames = FALSE, escape = FALSE)))
  # 
  # 
  # output$clinical_reduction_map_user_input <- renderPlot({
  #   user_input_output_generation()$clinical_reduction_map
  # })
  # 
  # output$severe_reduction_map_user_input <- renderPlot({
  #   user_input_output_generation()$severe_reduction_map
  # })
  # 
  # 
  # output$clinical_cases_averted_map_user_input <- renderPlot({
  #   user_input_output_generation()$clinical_cases_averted_map
  # })
  # 
  # output$severe_cases_averted_map_user_input <- renderPlot({
  #   user_input_output_generation()$severe_cases_averted_map
  # })
  
  
  output$clinical_incidence_graph_user_input  <- renderPlotly({
    user_input_output_generation()$clinical_incidence_graph
  })
  
  
  
  # render table for dhps haplotype frequencies
  output$haplotype_table_user_input  <- DT::renderDT(DT::datatable(user_input_output_generation()$haplotype_table,
                                                                 filter = "top", options = list(dom = 'Bftsp', paging = TRUE, pageLength = 10, ordering = TRUE, searching = TRUE, fixedColumns = TRUE, scrollX = TRUE, autoWidth = TRUE, responsive = TRUE, rownames = FALSE, escape = FALSE)))
  
  
  # render map of dhps haplotype frequencies 
  output$haplotype_map_user_input  <- renderPlot({
    user_input_output_generation()$haplotype_map
  })
  
  output$prob_drug_protect_graph_user_input  <- renderPlotly({
    user_input_output_generation()$prob_drug_protect_graph
  })
  
  output$severe_incidence_graph_user_input  <- renderPlotly({
    user_input_output_generation()$severe_incidence_graph
  })
  
  # # render table for % reduction in clinical incidence by 6 month age groups
  # output$prioritisation_table_user_input  <- DT::renderDT(DT::datatable(user_input_output_generation()$prioritisation_table,
  #                                                                     filter = "top", options = list(dom = 'Bftsp', paging = TRUE, pageLength = 10, ordering = TRUE, searching = TRUE, fixedColumns = TRUE, scrollX = TRUE, autoWidth = TRUE, responsive = TRUE, rownames = FALSE, escape = FALSE)))
  # 
  # 
  # # render table for health impact (total age group, all admin1 areas, annual)
  # output$annual_health_output_table_user_input  <- DT::renderDT(DT::datatable(user_input_output_generation()$annual_health_output_table,
  #                                                                           filter = "top", options = list(dom = 'Bftsp', paging = TRUE, pageLength = 10, ordering = TRUE, searching = TRUE, fixedColumns = TRUE, scrollX = TRUE, autoWidth = TRUE, responsive = TRUE, rownames = FALSE, escape = FALSE)))
  # 
  # 
  # # render table for health impact (total age group, all admin1 areas, annual)
  # output$annual_health_output_sixmonths_table_user_input  <- DT::renderDT(DT::datatable(user_input_output_generation()$annual_health_output_sixmonths_table,
  #                                                                                     filter = "top", options = list(dom = 'Bftsp', paging = TRUE, pageLength = 10, ordering = TRUE, searching = TRUE, fixedColumns = TRUE, scrollX = TRUE, autoWidth = TRUE, responsive = TRUE, rownames = FALSE, escape = FALSE)))
  # 
  # 
  # # render table for health impact (total age group, all admin1 areas, annual)
  # output$annual_health_output_whole_country_table_user_input  <- DT::renderDT(DT::datatable(user_input_output_generation()$annual_health_output_whole_country_table,
  #                                                                                         filter = "top", options = list(dom = 'Bftsp', paging = TRUE, pageLength = 10, ordering = TRUE, searching = TRUE, fixedColumns = TRUE, scrollX = TRUE, autoWidth = TRUE, responsive = TRUE, rownames = FALSE, escape = FALSE)))
  # 
  # 
  # 
  # Common compact DT options
  dt_options_compact <- list(
    dom = 'Bftsp',
    paging = TRUE,
    pageLength = 25,
    lengthMenu = c(10, 25, 50, 100),
    ordering = TRUE,
    searching = TRUE,
    fixedColumns = TRUE,
    scrollX = TRUE,
    autoWidth = TRUE,
    responsive = TRUE
  )
  
  # % reduction in clinical incidence by 6-month age groups
  output$prioritisation_table_user_input <- DT::renderDT(
    DT::datatable(
      user_input_output_generation()$prioritisation_table,
      filter = "top",
      options = dt_options_compact,
      rownames = FALSE,
      escape = FALSE,
      class = "compact stripe hover"
    )
  )
  
  # Health impact (total age group, all admin1 areas, annual)
  output$annual_health_output_table_user_input <- DT::renderDT(
    DT::datatable(
      user_input_output_generation()$annual_health_output_table,
      filter = "top",
      options = dt_options_compact,
      rownames = FALSE,
      escape = FALSE,
      class = "compact stripe hover"
    )
  )
  
  # Health impact (6-month age groups)
  output$annual_health_output_sixmonths_table_user_input <- DT::renderDT(
    DT::datatable(
      user_input_output_generation()$annual_health_output_sixmonths_table,
      filter = "top",
      options = dt_options_compact,
      rownames = FALSE,
      escape = FALSE,
      class = "compact stripe hover"
    )
  )
  
  # Health impact (whole country summary)
  output$annual_health_output_whole_country_table_user_input <- DT::renderDT(
    DT::datatable(
      user_input_output_generation()$annual_health_output_whole_country_table,
      filter = "top",
      options = dt_options_compact,
      rownames = FALSE,
      escape = FALSE,
      class = "compact stripe hover"
    )
  )
  
  # render table for cost data
  # output$cost_data_table_user_input  <- DT::renderDT(DT::datatable(user_input_output_generation()$cost_data_table,
  #                                                                filter = "top", options = list(dom = 'Bftsp', paging = TRUE, pageLength = 10, ordering = TRUE, searching = TRUE, fixedColumns = TRUE, scrollX = TRUE, autoWidth = TRUE, responsive = TRUE, rownames = FALSE, escape = FALSE)))
  # 
  
  # Compact DT options
  dt_options_compact <- list(
    dom = 'Bftsp',
    paging = TRUE,
    pageLength = 25,
    lengthMenu = c(10, 25, 50, 100),
    ordering = TRUE,
    searching = TRUE,
    fixedColumns = TRUE,
    scrollX = TRUE,
    autoWidth = TRUE,
    responsive = TRUE
  )
  
  # Render table for cost data (user input page)
  output$cost_data_table_user_input <- DT::renderDT(
    DT::datatable(
      user_input_output_generation()$cost_data_table,
      filter = "top",
      options = dt_options_compact,
      rownames = FALSE,
      escape = FALSE,
      class = "compact stripe hover"
    )
  )
  
  # render table for DALYS
  output$dalys_table_user_input  <- DT::renderDT(DT::datatable(user_input_output_generation()$dalys_table,
                                                             filter = "top", options = list(dom = 'Bftsp', paging = TRUE, pageLength = 10, ordering = TRUE, searching = TRUE, fixedColumns = TRUE, scrollX = TRUE, autoWidth = TRUE, responsive = TRUE, rownames = FALSE, escape = FALSE)))
  
  
  # render table for CET
  output$cet_table_user_input <- DT::renderDT(
    DT::datatable(
      user_input_output_generation()$cet_table,
      rownames = FALSE,
      filter = "top",
      options = list(
        dom = "ftip",
        paging = FALSE,
        ordering = TRUE,
        searching = TRUE,
        scrollX = FALSE,
        autoWidth = TRUE,
        responsive = FALSE
      )
    )
  )
  
  
  output$clinical_reduction_map_user_input  <- renderPlot({
    user_input_output_generation()$clinical_reduction_map
  })
  
  output$severe_reduction_map_user_input  <- renderPlot({
    user_input_output_generation()$severe_reduction_map
  })
  
  output$clinical_cases_averted_map_user_input  <- renderPlot({
    user_input_output_generation()$clinical_cases_averted_map
  })
  
  output$severe_cases_averted_map_user_input  <- renderPlot({
    user_input_output_generation()$severe_cases_averted_map
  })
  
  
  output$icer_map_user_input  <- renderPlot({
    user_input_output_generation()$icer_map
  })
  
  output$bar_plot_user_input  <- renderPlot({
    user_input_output_generation()$bar_plot
  })
  
  
  
  output$output_navset_tabs <- renderUI({
    
    req(input$show_results)
    
    # =====================================================
    # HEALTH PANELS (build list safely)
    # =====================================================
    # ---- Add country-only panels ----
    
    health_panels <- c()
    
    if (input$country_or_area == "Whole country") {
        
      health_panels <- c(health_panels, 
                         list(
          
          nav_panel(
            title = "Map: clinical cases averted",
            plotOutput(
              "clinical_cases_averted_map_user_input"
            )
          ),
          
          nav_panel(
            title = "Map: hospitalisations averted",
            plotOutput(
              "severe_cases_averted_map_user_input"
            )
          ),
          
          nav_panel(
            title = "Map: reduction in clinical cases (%)",
            plotOutput(
              "clinical_reduction_map_user_input"
            )
          ),
          
          nav_panel(
            title = "Map: reduction in hospitalisations (%)",
            plotOutput(
              "severe_reduction_map_user_input"
            )
          ),
          
          nav_panel(
            title = "Table: average national incidence, age 0-30mo",
            DT::DTOutput(
              "annual_health_output_whole_country_table_user_input"
            )
          )
          
        )
        
      )
        
      
    }
    
    
    health_panels <- c(health_panels, 
                       list(
                         
       nav_panel(
         title = "Graph: clinical cases with and without PMC (0-30mo)",
         plotOutput(
           "bar_plot_user_input"
         )
       ), 
    
      nav_panel(
        title = "Graph: clinical cases averted by age",
        plotlyOutput(
          "clinical_incidence_graph_user_input"
        )
      ), 
      
      nav_panel(
        title = "Graph: hospitalisations averted by age",
        plotlyOutput(
          "severe_incidence_graph_user_input"
        )
      ),
      
      nav_panel(
        title = "Table: incidence, age 0-30mo",
        DT::DTOutput(
          "annual_health_output_table_user_input"
        )
      ),
      
      nav_panel(
        title = "Table: incidence, 6mo age groups",
        DT::DTOutput(
          "annual_health_output_sixmonths_table_user_input"
        )
      ),
      
      
      nav_panel(
        title = "Table: DALYs averted",
        DT::DTOutput("dalys_table_user_input")
      )
      
    )
    
    )
    
    
    
    # =====================================================
    # FULL PAGE UI
    # =====================================================
    fluidPage(
      
      # ---------------- ECONOMICS ----------------
      titlePanel(tags$h3("Economics")),
      
      navset_card_tab(
        height = 800,
        
        nav_panel(
          title = "Map: cost-effectiveness by admin-1 unit",
          plotOutput(
            "icer_map_user_input"
          )
        ),
        
        nav_panel(
          title = "Table: ranked list of options",
          DT::DTOutput(
            "prioritisation_table_user_input"
          )
        ),
        

        
        nav_panel(
          title = "Table: cost assumptions",
          DT::DTOutput(
            "cost_data_table_user_input"
          )
        )
        
      ),
    
      
      # ---------------- HEALTH ----------------
      titlePanel(tags$h3("Health Impact")),
      
      do.call(
        navset_card_tab,
        c(
          list(height = 800),
          health_panels
        )
      ),
      
      
      # ---------------- ECONOMICS ----------------
      titlePanel(tags$h3("SP resistance and efficacy")),
      
      navset_card_tab(height = 800,
                      nav_panel(title="Country-specific dhps haplotype frequencies",
                                DT::DTOutput("haplotype_table_user_input")),
                      
                      nav_panel(
                        title = "Country-specific map of estimated 30-day protective efficacy",
                        plotOutput("haplotype_map_user_input")),
                      
                      nav_panel(
                        title="Probability of drug protection following a dose of SP",
                        plotlyOutput("prob_drug_protect_graph_user_input")
                      )
                      
      )
      
      
      
    )
    
  })
  
  


  output$output_cost_tab <- renderUI({
    if (input$show_results_main_page != 0) {

      fluidPage(
        #titlePanel(tags$h3("Cost assumptions")),
        
        # ---- Caption text ----
        tags$style(HTML(".description-text { font-size: 12px; }")),
        
        p(
          class = "description-text",
          "Click on the tabs below to see the detailed information on the assumptions used in estimating costs, DALYs, and cost-effectiveness used in the main ranking of options. Options are NOT ranked in these tables, each row represents an option involving no PMC, or a specific PMC delivery schedule by admin-1 unit.  "
        ),
        
        # ---- Table card ----
        navset_card_tab(
          height = 800,
          
          nav_panel(
            title = "Table: cost assumptions",
            DT::DTOutput("cost_data_table_main_page")
          ),
          
          nav_panel(
            title = "Table: country-specific cost effectiveness thresholds",
            DT::DTOutput("cet_table_main_page")
          )
        )
      )



    }
  })

  output$output_SP_tab <- renderUI({
    if (input$show_results_main_page != 0) {


      fluidPage(
        #titlePanel(tags$h3("Molecular markers associated with SP efficacy and resistance")),
        
        # ---- Caption text ----
        tags$style(HTML(".description-text { font-size: 12px; }")),
        
        p(
          class = "description-text",
          "Mutations in the dihydropteroate synthase (dhps) gene vary geographically. The following table and maps show the frequency of dhps haplotypes by subnational area, based on P. falciparum positive samples collected in each region. The haplotypes consist of amino acid changes at positions 431-436 (as “_”)-437-540-581. We define frequency as the number with each haplotype divided by the total number of samples successfully genotyped and which are not mixed at any of the above loci. Sources include a comprehensive database held at LSHTM, including data from samples collected within the Plus Project. In areas where data were not available, modelled estimates were used.
          \nProtective efficacy wanes over time since drug administration, and is dependent on the parasite genotypes that are present. The plot below shows the probability of drug protection against new P. falciparum infection since receiving an SP dose. Subnational area-specific genotype frequencies were used to weigh previously estimated protective efficacy curves against parasites with each dhps genotype that is commonly found in Africa."
        ),
        
        navset_card_tab(height = 800,
          nav_panel(title="Country-specific dhps haplotype frequencies",
                    DT::DTOutput("haplotype_table_main_page")),

          nav_panel(
             title = "Country-specific map of estimated 30-day protective efficacy",
             plotOutput("haplotype_map_main_page")),
          
          nav_panel(
            title="Probability of drug protection following a dose of SP",
            plotlyOutput("prob_drug_protect_graph_main_page")
          )

        )

        )



    }
  })

  output$output_health_impact_tab <- renderUI({
    if (input$show_results_main_page != 0) {


      fluidPage(
                
                
                # ---- Caption text ----
                tags$style(HTML(".description-text { font-size: 12px; }")),
                
                p(
                  class = "description-text",
                  "Click on the tabs below to see figures and tables showing the potential health impact of PMC in greater depth. Results are only shown for the region selected and “run” on the main page. Return to the main page to produce results for a different geographic area. In the tables, options are NOT ranked, each row represents the incidence or health impact of PMC by the specified geography and age group. "
                ),
                
                navset_card_tab(height = 800,
                                
                  if (input$country_or_area_main_page == "Whole country") {
                    nav_panel(
                      title = "Map: clinical cases averted",
                      plotOutput("clinical_cases_averted_map_main_page")
                    )},
                  
                  if (input$country_or_area_main_page == "Whole country") {
                    nav_panel(
                      title = "Map: hospitalisations averted",
                      plotOutput("severe_cases_averted_map_main_page")
                    )},
                  
                  if (input$country_or_area_main_page == "Whole country") {
                    nav_panel(
                      title = "Map: reduction in clinical cases (%)",
                      plotOutput("clinical_reduction_map_main_page")
                    )},
                  
                  if (input$country_or_area_main_page == "Whole country") {
                    nav_panel(
                      title = "Map: reduction in hospitalisations (%)",
                      plotOutput("severe_reduction_map_main_page")
                    )},
                  
                  if (input$country_or_area_main_page == "Whole country") {
                    nav_panel(
                      title = "Table: average national incidence, age 0-30mo",
                      DT::DTOutput("annual_health_output_whole_country_table_main_page")
                    )
                    
                  },
                  
                  nav_panel(
                    title = "Graph: clinical cases with and without PMC (0-30mo)",
                    plotOutput("bar_plot_main_page")
                  ),

                  
                  nav_panel(
                    title = "Graph: clinical cases averted by age",
                    plotlyOutput("clinical_incidence_graph_main_page")
                  ),

                  nav_panel(
                    title = "Graph: hospitalisations averted by age",
                    plotlyOutput("severe_incidence_graph_main_page")
                  ),
                  
                  
                  nav_panel(
                    title = "Table: incidence, age 0-30mo",
                    DT::DTOutput("annual_health_output_table_main_page")
                  ),

                  nav_panel(
                    title = "Table: incidence, 6mo age groups",
                    DT::DTOutput("annual_health_output_sixmonths_table_main_page")
                  ),
                  
                  nav_panel(
                    title = "Table: DALYs averted",
                    DT::DTOutput("dalys_table_main_page")
                  )
                  

                )

                

                )



    }
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)