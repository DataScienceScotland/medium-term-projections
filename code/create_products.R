# Hospitalisation and ICU demand graphs
create_demand_curves = function(df, current_date = today(), included_equipment = NULL) { #in create_products.R
  ###produce estimates
  df = df %>%
    nest(-ID, -`Scenario_name`, .key = "estimates") %>% 
    ###create the charts
    select(intervention_id = ID, intervention = `Scenario_name`, estimates)
  
  df%>% 
    pmap(output_charts, max_week=75, include_lines=FALSE, current_date=current_date)
  df%>% 
    pmap(output_charts, 
         max_week=23, 
         suffix = "_windowed",
         included_equipment=included_equipment, 
         current_date=current_date)
}

create_demand_comparison = function(df, end_date, start_date = end_date + 1 - 7*16, concurrent_or_admissions) { #in create_products.R
  
  df %>%
    nest(-Scenario_name, .key = "df") %>%
    pmap(compare_curves,
         type = "Hospital",
         start_date,
         end_date,
         concurrent_or_admissions)
  
  df %>%
    nest(-Scenario_name, .key = "df") %>%
    pmap(compare_curves,
         type = "ICU",
         start_date,
         end_date,
         concurrent_or_admissions)
}

create_deaths_curves = function(estimates, end_date, start_date = end_date + 1 - 7*16) { #in create_products.R 
  estimates %>% 
    filter(ID %in% 1:3, Date >= start_date, Date <= end_date) %>% 
    deaths_curves
  ggsave("Outputs/curves/deaths.png",
         height = 10.97, 
         width = 14.65, 
         units = "cm")
}

#############################################
#Spreadsheet for demand led items and services, saved to erdm

create_equipment_spreadsheet = function(df, end_date, start_date = end_date + 1 - 7*16) { #in create_products.R
  workbook = "Current estimates of equipment and medicine template.xlsx" %>% 
    get_filepath("Output templates") %>% 
    loadWorkbook %>% 
    add_ppe_sheets(df, start_date, end_date, type = "weekly") %>% 
    add_ppe_sheets(df, start_date, end_date, type = "daily", first_row = 9) %>% 
    add_testing_sheets(df, start_date, end_date, type="weekly") %>%
    add_testing_sheets(df, start_date, end_date, type="daily") %>% 
    add_tracing_sheets(df, start_date, end_date, type="weekly") %>%
    add_tracing_sheets(df, start_date, end_date, type="daily") %>% 
    add_medicine_sheets(df, start_date, end_date, type = "weekly") %>% 
    add_medicine_sheets(df, start_date, end_date, type="daily")
  
  saveWorkbook(workbook, 
               "Outputs/Current estimates spreadsheets/Current estimates of equipment and medicine.xlsx",
               overwrite = TRUE)
}

add_testing_sheets = function(template,
                              df,
                              start_date,
                              end_date,
                              type = "weekly", 
                              first_row = 9) { #in create_products.R
  
  sheet = "Testing " %>% str_c(type)
  period = switch(type, weekly = "weeks", daily = "days")
  column = switch(type, weekly = "Week", daily = "Day")
  column_name = switch(type, weekly = "Week_commencing", daily = "Date")     #added column_name
  column_name_quo = rlang::sym(column_name)
  
  today() %>% 
    format.Date("%d/%m/%Y") %>% 
    str_c("Date produced: ", .) %>% 
    writeData(template, sheet = sheet, ., startCol = 2, startRow = 2, colNames = FALSE)
  
  df %>%
    get_tests(period = period) %>% 
    filter(!!column_name_quo >= start_date,
           !!column_name_quo <= end_date) %>% 
    mutate(ID = ID %>% as.integer) %>% 
    select(-column) %>%                                 #line added to remove Week/Day column before spreading
    mutate(!!column_name_quo := !!column_name_quo %>% as_excel_date) %>% 
    spread(column_name, "Number of cases") %>%          #spread by column_name
    arrange(ID, Type_of_case) %>%
    select(-Type_of_case) %>% 
    nest(-ID) %>% 
    mutate(row_number = first_row + 6*ID) %>% 
    pmap(function(ID, data, row_number, ...) {
      writeData(template, sheet = sheet, data, startCol = 3, startRow = row_number, colNames = TRUE)
    })
  
  template
}

add_ppe_sheets = function(template,
                          df,
                          start_date,
                          end_date,
                          type = "weekly", 
                          first_row = 10) { #in create_products.R
  
  sheet = "PPE " %>% str_c(type)
  period = switch(type, weekly = "weeks", daily = "days")
  column = switch(type, weekly = "Week", daily = "Day")
  column_name = switch(type, weekly = "Week_commencing", daily = "Date")     #added column_name
  column_name_quo = rlang::sym(column_name)
  
  today() %>% 
    format.Date("%d/%m/%Y") %>% 
    str_c("Date produced: ", .) %>% 
    writeData(template, sheet = sheet, ., startCol = 2, startRow = 2, colNames = FALSE)
  
  df %>%
    get_hospitalisations(period = period) %>% 
    filter(!!column_name_quo >= start_date,
           !!column_name_quo <= end_date) %>% 
    mutate(ID = ID %>% as.integer) %>% 
    select(-column) %>%                                 #line added to remove Week/Day column before spreading
    mutate(!!column_name_quo := !!column_name_quo %>% as_excel_date) %>% 
    spread(column_name, "Number of cases") %>% 
    arrange(ID, Type_of_case) %>% 
    select(-Type_of_case) %>%
    bind_rows(df %>%
                get_ppe(period = period) %>% 
                filter(!!column_name_quo >= start_date,
                       !!column_name_quo <= end_date) %>% 
                mutate(ID = ID %>% as.integer) %>% 
                select(-column) %>%                                 #line added to remove Week/Day column before spreading
                mutate(!!column_name_quo := !!column_name_quo %>% as_excel_date) %>% 
                spread(column_name, "Number") %>%                   #spread by column_name
                arrange(ID, Equipment, desc(Stat)) %>%
                select(-Equipment, -Stat,-`Scenario_name`)) %>%
    arrange(ID) %>% 
    nest(-ID) %>% 
    mutate(row_number = first_row + 17*ID) %>% 
    pmap(function(ID, data, row_number, ...) {
      writeData(template, sheet = sheet, data, startCol = 3, startRow = row_number, colNames = TRUE)
    })
  
  template
} 

add_tracing_sheets = function(template,
                              df,
                              start_date,
                              end_date,
                              type = "weekly", 
                              first_row = 10) { #in create_products.R
  
  sheet = "Tracing " %>% str_c(type)
  period = switch(type, weekly = "weeks", daily = "days")
  column = switch(type, weekly = "Week", daily = "Day")
  column_name = switch(type, weekly = "Week_commencing", daily = "Date")     #added column_name
  column_name_quo = rlang::sym(column_name)
  
  today() %>% 
    format.Date("%d/%m/%Y") %>% 
    str_c("Date produced: ", .) %>% 
    writeData(template, sheet = sheet, ., startCol = 2, startRow = 2, colNames = FALSE)
  
  df %>%
    get_tracing_for_ss(period = period) %>% 
    filter(!!column_name_quo >= start_date,
           !!column_name_quo <= end_date) %>% 
    mutate(ID = ID %>% as.integer) %>% 
    select(-column) %>%                                 #line added to remove Week/Day column before spreading
    mutate(!!column_name_quo := !!column_name_quo %>% as_excel_date) %>% 
    spread(column_name, "Number of cases") %>%          #spread by column_name
    arrange(ID, Type_of_case) %>%
    select(-Type_of_case) %>% 
    nest(-ID) %>% 
    mutate(row_number = first_row + 6*ID) %>% 
    pmap(function(ID, data, row_number, ...) {
      writeData(template, sheet = sheet, data, startCol = 3, startRow = row_number, colNames = TRUE)
    })
  
  template
}

add_medicine_sheets = function(template,
                               df,
                               start_date,
                               end_date,
                               type = "weekly", 
                               first_row = 9) { #in create_products.R
  
  sheet = "Medicine " %>% str_c(type)
  period = switch(type, weekly = "weeks", daily = "days")
  column = switch(type, weekly = "Week", daily = "Day")
  column_name = switch(type, weekly = "Week_commencing", daily = "Date")     #added column_name
  column_name_quo = rlang::sym(column_name)
  
  today() %>% 
    format.Date("%d/%m/%Y") %>% 
    str_c("Date produced: ", .) %>% 
    writeData(template, sheet = sheet, ., startCol = 2, startRow = 2, colNames = FALSE)
  
  df %>% 
    get_medicine(period = period) %>% 
    filter(!!column_name_quo >= start_date,
           !!column_name_quo <= end_date) %>% 
    mutate(ID = ID %>% as.integer) %>% 
    select(-column) %>%                                 #line added to remove Week/Day column before spreading
    mutate(!!column_name_quo := !!column_name_quo %>% as_excel_date) %>% 
    spread(column_name, "Requirement") %>%              #spread by column_name
    arrange(ID, `Meds Group`, desc(Measure)) %>% 
    select(ID, `Meds Group`, Measure, everything()) %>% 
    rename("Medicine Group" = "Meds Group") %>%
    select(-`Comparator product`) %>%
    nest(-ID) %>% 
    mutate(row_number = first_row + 21*ID) %>% 
    pmap(function(ID, data, row_number, ...) {
      writeData(template, sheet = sheet, data, startCol = 2, startRow = row_number, colNames = TRUE)
    })
  
  template
}

##############################
#spreadsheet for demand, saved to erdm

create_current_estimates_spreadsheet = function(df, end_date, start_date = end_date + 1 - 7*16) { #in create_products.R
  workbook ="Current Estimates of cases template.xlsx" %>% 
    get_filepath("Output templates") %>% 
    loadWorkbook %>% 
    add_current_estimates_sheets(df, start_date, end_date, type = "weekly") %>% 
    add_current_estimates_sheets(df, start_date, end_date, type = "daily")
  
  saveWorkbook(workbook, "Outputs/Current estimates spreadsheets/Current Estimates of cases.xlsx", overwrite = TRUE)
}

add_current_estimates_sheets = function(template,
                                        df, 
                                        start_date,
                                        end_date,
                                        type = "weekly",
                                        first_row = 24) { #in create_products.R
  
  sheet = "Estimates " %>% str_c(type)
  period = switch(type, weekly = "weeks", daily = "days")
  column = switch(type, weekly = "Week", daily = "Day")
  column_name = switch(type, weekly = "Week_commencing", daily = "Date")      #added column_name
  column_name_quo = rlang::sym(column_name)
  
  today() %>% 
    format.Date("%d/%m/%Y") %>% 
    str_c("Date produced: ", .) %>% 
    writeData(template, sheet = sheet, ., startCol = 2, startRow = 2, colNames = FALSE)
  
  df %>% 
    get_current_estimates(period = period) %>% 
    filter(!!column_name_quo >= start_date,
           !!column_name_quo <= end_date) %>% 
    mutate(ID = ID %>% as.integer) %>% 
    select(-column) %>%                                 #line added to remove Week/Day column before spreading
    mutate(!!column_name_quo := !!column_name_quo %>% as_excel_date) %>% 
    spread(column_name, "Number of cases") %>% 
    arrange(ID, Type_of_case) %>% 
    select(-Type_of_case) %>% 
    nest(-ID) %>% 
    mutate(row_number = first_row + 9*ID) %>% 
    pmap(function(ID, data, row_number, ...) {
      writeData(template, sheet = sheet, data, startCol = 3, startRow = row_number, colNames = TRUE,
                headerStyle = createStyle(numFmt = "DATE", textDecoration = "bold"))
    })
  
  template
}

##########################################
#spreadsheets for use by HSCA

create_bed_days_spreadsheet = function(df, end_date, start_date = end_date + 1 - 7*16) { #in create_products.R
  workbook ="Bed usage days template.xlsx" %>% 
    get_filepath("Output templates") %>% 
    loadWorkbook %>% 
    add_bed_days_sheets(df, start_date, end_date, type = "weekly")
  
  saveWorkbook(workbook, "Outputs/Bed usage days.xlsx", overwrite = TRUE)
}

add_bed_days_sheets = function(template,
                               df, 
                               start_date,
                               end_date,
                               type = "weekly",
                               first_row = 5) { #in create_products.R
  
  sheet = "Estimates " %>% str_c(type)
  period = switch(type, weekly = "weeks", daily = "days")
  column = switch(type, weekly = "Week", daily = "Day")
  column_name = switch(type, weekly = "Week_commencing", daily = "Date")      #added column_name
  column_name_quo = rlang::sym(column_name)
  
  today() %>% 
    format.Date("%d/%m/%Y") %>% 
    str_c("Date produced: ", .) %>% 
    writeData(template, sheet = sheet, ., startCol = 1, startRow = 1, colNames = FALSE)
  
  df %>% 
    get_bed_days(period = period) %>% 
    filter(!!column_name_quo >= start_date,
           !!column_name_quo <= end_date) %>% 
    mutate(ID = ID %>% as.integer) %>% 
    select(-column) %>%                                 #line added to remove Week/Day column before spreading
    mutate(!!column_name_quo := !!column_name_quo %>% as_excel_date) %>% 
    spread(column_name, "Number of cases") %>% 
    arrange(ID, Type_of_case) %>% 
    select(-Type_of_case) %>% 
    nest(-ID) %>% 
    mutate(row_number = first_row + 7*ID) %>% 
    pmap(function(ID, data, row_number, ...) {
      writeData(template, sheet = sheet, data, startCol = 2, startRow = row_number, colNames = TRUE,
                headerStyle = createStyle(numFmt = "DATE", textDecoration = "bold"))
    })
  
  template
}

# create_assumptions_table = function(...) { #in create_products.R
#   "Model Assumptions.xlsx" %>% 
#     get_filepath("Data") %>% 
#     get_scenarios %>% 
#     mutate(data = ID %>%
#              map(prepare_assumptions, ...)) %>%
#     unnest %>% 
#     select(-`Curve file`, `Curve method`, -ID) %>% 
#     write_csv("Data/Assumptions Table.csv")
# }

create_nhs_workforce_proportions = function(proportions, end_date) { #in create_products.R
  working_age_population = get_population(Age) %>% 
    filter(Age >= 16, Age <= 64) %>% 
    mutate(Age_group = (Age %/% 10) * 10) %>% 
    group_by(Age_group) %>% 
    summarise(Population = sum(Population))
  
  proportions = proportions %>% 
    filter(Level == "Median")
  
  staff = proportions %>% 
    filter(severity != "Mild", 
           Status %in% c("Infected", "Recovering"),
           SIMD=="U",
           Age_group >=10 , 
           Age_group <= 60,
           Date <= end_date) %>% 
    select(-severity, -SIMD) %>% 
    mutate(Status = ifelse(Status=="Recovering", 
                           "Proportion of working age recovering", 
                           "Proportion of working age symptomatic")) %>% 
    get_totals(working_age_population) %>% 
    group_by(Scenario_name, Age_group, Status, Date) %>% 
    summarise(Number_at = sum(Number_at),
              Population = first(Population)) %>% 
    group_by(Scenario_name, Status, Date) %>% 
    summarise(Proportion_at = sum(Number_at)/sum(Population)) %>% 
    mutate(`Week commencing` = Date %>% floor_date("weeks", week_start = 1)) %>%  
    group_by(Scenario_name, Status, `Week commencing`) %>% 
    summarise(Proportion_at = max(Proportion_at)) %>% 
    spread(Status, Proportion_at)
  
  household = proportions %>% 
    filter(severity != "Mild", 
           Status %in% c("Infected"),
           Date <= end_date) %>% 
    mutate(Status = "Proportion of population newly symptomatic") %>% 
    get_totals(get_population(Age_group, SIMD)) %>% 
    group_by(Scenario_name, Age_group, Status, Date, SIMD) %>% 
    summarise(Number_starting = sum(Number_starting),
              Population = first(Population)) %>%  
    group_by(Scenario_name, Age_group, Status, Date) %>% 
    summarise(Number_starting = sum(Number_starting),
              Population = sum(Population)) %>% 
    group_by(Scenario_name, Status, Date) %>% 
    summarise(Proportion_starting = sum(Number_starting)/sum(Population)) %>% 
    mutate(`Week commencing` = Date %>% floor_date("weeks", week_start = 1)) %>% 
    group_by(Scenario_name, Status, `Week commencing`) %>% 
    summarise(Proportion_starting = sum(Proportion_starting)) %>% 
    spread(Status, Proportion_starting)
  
  template = "NHS workforce proportions template.xlsx" %>% 
    get_filepath("Output templates") %>% 
    loadWorkbook
  
  writeData(template, sheet = 1, staff %>% left_join(household), colNames = TRUE)
  
  saveWorkbook(template, "Outputs/NHS workforce proportions.xlsx", overwrite = TRUE)
}

#for NHS calls modelling
create_symptomatic_by_health_board = function(proportions, end_date) { #in create_products.R
  proportions %>% 
    filter(Status=="Infected", 
           severity!="Mild",
           Date <= end_date) %>% 
    select(-Status, -severity) %>% 
    get_totals(get_population(Health_board, Age_group, In_care, SIMD, Vulnerable)) %>% 
    mutate(Week = Day %/% 7) %>% 
    group_by(ID, `Scenario_name`, Health_board, Week) %>% 
    summarise(Number_starting = sum(Number_starting)) %>% 
    spread(Week, Number_starting) %>% 
    ungroup %>% 
    select(-ID) %>% 
    write_csv("Outputs/Symptomatic by Health board.csv")
}

##################################
#charts

create_vulnerable_recovered_and_resilient = function(df, end_date) { #in create_products.R
  susceptible = df %>% 
    filter(Status %in% c("Oxygen", "Non-invasive ventilation", "")) %>% 
    group_by(ID, `Scenario_name`, Age_group, Vulnerable, In_care, Date) %>% 
    summarise(Number_at = sum(Number_at, na.rm=TRUE)) %>%
    left_join(get_population(Age_group, Vulnerable, In_care)) %>% 
    mutate(Number_at = Population - Number_at)
  
  resilient = susceptible %>% 
    filter(Age_group == 20, In_care == "No", Vulnerable == "No") %>% 
    mutate(Status = "Resilient")
  
  vulnerable = susceptible %>% 
    filter(Age_group >= 70 | Vulnerable == "Yes",
           In_care == "No") %>% 
    mutate(Status = "Needing support")
  
  recovered = df %>% 
    filter(Status == "Recovered")
  
  bind_rows(resilient, vulnerable, recovered) %>% 
    ungroup %>% 
    nest(-ID, -`Scenario_name`, .key = "df") %>% 
    pmap(vulnerable_recovered_and_resilient_curves, end_date)
}

create_range_comparison = function(estimates, end_date, start_date = end_date + 1 - 16*7) { #in create_products.R
  estimates %>% compare_ranges("Hospital", start_date, end_date)
  
  ggsave("Outputs/curves/hospital_range_comparison.png", 
         height = 10.97, 
         width = 14.65, 
         units = "cm")
  
  estimates %>% compare_ranges("ICU", start_date, end_date)
  
  ggsave("Outputs/curves/icu_range_comparison.png", 
         height = 10.97, 
         width = 14.65, 
         units = "cm")
}

create_short_term_forecasts_chart = function (filename, audience = "internal") {
  
  filename %>% short_term_forecasts(audience)
  
  ggsave("Outputs/curves/short_term_forecasts.png", 
         height = 10, 
         width = 23, 
         units = "cm")
}


create_TTIS_demand_comparison = function(df, 
                                         scenario_group, 
                                         end_date, 
                                         start_date = end_date + 1 - 7*16, 
                                         concurrent_or_admissions="concurrent") { #in create_products.R
  
  df %>% 
    compare_curves_TP("Contacts", start_date, end_date, concurrent_or_admissions)
  ggsave("Outputs/curves/contacts_" %>% str_c(scenario_group, ".png"), 
         height = 10.97, 
         width = 14.65, 
         units = "cm")
  
  df %>% 
    compare_curves_TP("Tracer", start_date, end_date, concurrent_or_admissions)
  ggsave("Outputs/curves/tracer_" %>% str_c(scenario_group, ".png"),
         height = 10.97, 
         width = 14.65, 
         units = "cm")
}

####Spreadsheets of forecasts to go to SPI M
export_SPI_forecast = function(model=NULL, table=NULL) { #in create_products.R
  table %>% 
    write_csv("Outputs/SPI M/8weeks " %>%
                str_c(today() %>% format("%Y%m%d"),
                      " SG",
                      ".csv"))
}

export_SPI_forecasts = function(model_version,
                                base_date = today() %>% floor_date("weeks"),
                                estimates = NULL) { #in create_products.R
  
  if(estimates %>% is_null) {
    estimates = readRDS("Outputs/raw/scotland_estimates.rds")
  }
  
  forecasts = estimates %>% 
    prep_SPI_forecast(base_date-10, base_date+55, model_version = model_version)
  
  export_SPI_forecast(table = forecasts)
}