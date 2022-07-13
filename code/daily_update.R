#daily update
run_daily_update = function(current_day, from_cache="None") { #in daily_update.R
  
  message("Running Scotland model")
  if("proportions" %in% from_cache)
    proportions = readRDS("Outputs/raw/proportions.rds")
  else if(from_cache == "None") {
    message("Running Scotland model")
    model = run_model(Age_group, SIMD, Vulnerable, In_care)
    proportions = run_timeline(Age_group, In_care) %>% 
      get_proportions(model)
    
    proportions %>% saveRDS("Outputs/raw/proportions.rds")
  }
  
  if("scotland_estimates" %in% from_cache)
    scotland_estimates = readRDS("Outputs/raw/scotland_estimates.rds")
  else {
    scotland_estimates = proportions %>% 
      get_totals(get_population(Age_group, SIMD, Vulnerable, In_care))
    
    scotland_estimates %>% saveRDS("Outputs/raw/scotland_estimates.rds")
  }
}

create_excel_output <- function(end_date, start_date = end_date + 1 - 7*16) {
  proportions = readRDS("Outputs/raw/proportions.rds")
  scotland_estimates = readRDS("Outputs/raw/scotland_estimates.rds")
  
  proportions = proportions %>% 
    distinct(Date) %>% 
    arrange(Date) %>% 
    mutate(Day = (Date %>% as.character %>% as_factor %>% as.integer)-1) %>% 
    full_join(proportions)
  
  scotland_estimates = scotland_estimates %>% 
    distinct(Date) %>% 
    arrange(Date) %>% 
    mutate(Day = (Date %>% as.character %>% as_factor %>% as.integer)-1) %>% 
    full_join(scotland_estimates)
  
  message("Creating mobilisation spreadsheet")
  scotland_estimates %>% 
    mutate(ID = ID %>% recode(`1`=2, `2`=1)) %>% 
    filter(ID %in% 0:3) %>% 
    create_current_estimates_spreadsheet(end_date, start_date)
  
  #medical equipment spreadsheet
  message("Creating medical equipment spreadsheet")
  scotland_estimates %>% 
    mutate(ID = ID %>% recode(`1`=2, `2`=1)) %>% 
    filter(ID %in% 0:3) %>% 
    create_equipment_spreadsheet(end_date, start_date)
  
  #bed days spreadsheet
  message("Creating bed days spreadsheet")
  scotland_estimates %>% 
    filter(ID %in% 0:3) %>% 
    create_bed_days_spreadsheet(end_date, start_date)
  
  #NHS workforce spreadsheet
  message("Creating NHS workforce spreadsheet")
  proportions %>% 
    filter(ID %in% 0:3) %>% 
    create_nhs_workforce_proportions(end_date)
  
  #symptomatic by health board spreadsheet
  message("Creating symptomatic by health board spreadsheet")
  proportions %>% 
    filter(ID %in% 0:3) %>% 
    create_symptomatic_by_health_board(end_date)
  
}
  
create_slidepack_output <- function(end_date, start_date = end_date + 1 - 7*16, concurrent_or_admissions = "concurrent") {
  scotland_estimates = readRDS("Outputs/raw/scotland_estimates.rds")
  
  scotland_estimates = scotland_estimates %>% 
    distinct(Date) %>% 
    arrange(Date) %>% 
    mutate(Day = (Date %>% as.character %>% as_factor %>% as.integer)-1) %>% 
    full_join(scotland_estimates)
  
  # #hospital demand curves for slidepack
  # message("Creating bed demand charts")
  # scotland_estimates %>% 
  #   filter(ID %in% 0:3) %>% 
  #   create_demand_curves(current_day, included_equipment = c("Beds", "Ventilators"))
  
  # #vulnerable and resilient curves for slidepack
  # message("Creating vulnerable and resilient charts")
  # scotland_estimates %>% 
  #   create_vulnerable_recovered_and_resilient
  
  #demand curve comparison for slidepack
  message("Creating demand curve comparison charts")
  scotland_estimates %>%
    create_demand_comparison(end_date,
                             start_date, 
                             concurrent_or_admissions)
  
  
  #Create TTIS demand comparison for slide pack
  scotland_estimates %>%
    filter(ID %in% 1:3) %>% 
    get_contact_tracing() %>%
    create_TTIS_demand_comparison("phase2",
                                  end_date,
                                  start_date, 
                                  concurrent_or_admissions)
  
  # #death curves for slidepack
  # message("Creating death charts")
  # scotland_estimates %>% 
  #   filter(ID %in% c(1,2,3)) %>% 
  #   create_deaths_curves(current_day)
}

run_weekly_update = function(scenarios, levels, filepath = "Outputs/raw/") { #in daily_update.R
  
  scenario_df = crossing(Scenario_name = scenarios, Level = levels) %>% 
    mutate(ID = 1:NROW(.))
  
  message("Running Scotland model")
  # Add immune-compromised.
  model = scenario_df %>% 
    run_model(Age_group, SIMD, Immune_compromised)
  
  proportions = scenario_df %>% 
    mutate(scenario = Scenario_name) %>% 
    nest(scenario_info = c(Scenario_name, Level, ID)) %>% 
    pmap_dfr(transform_epi_curve) %>% 
    run_timeline(Age_group, Immune_compromised) %>% 
    get_proportions(model)
  
  proportions %>% saveRDS(filepath %>% str_c("proportions.rds"))
  
  # Make sure population is split by immune-compromised status.
  scotland_estimates = proportions %>% 
    get_totals(get_population(Age_group, SIMD, Immune_compromised))
  
  scotland_estimates %>% saveRDS(filepath %>% str_c("scotland_estimates.rds"))
}

create_products = function() { #in daily_update.R
  export_SPI_forecasts(1.0)
}