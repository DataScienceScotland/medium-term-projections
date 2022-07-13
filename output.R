#outputs tables
#pivotable output, mainly for GIS. Specify the required categorical columns
output_tables = function(results, ..., .filename = "output") { #in output.R
  
  infections = results %>% 
    group_by(severity, ...) %>% 
    summarise(Infections = sum(Infections)) %>% 
    ungroup %>% 
    mutate(severity = "Potential " %>% str_c(severity, " cases"),
           Infections = Infections %>% round) %>% 
    spread(severity, Infections)
  
  deaths = results %>% 
    group_by(...) %>% 
    summarise(Fatalities = sum(Fatalities)) %>% 
    mutate(Fatalities = Fatalities %>% round)
  
  population = results %>% 
    filter(severity=="Mild") %>% 
    group_by(...) %>% 
    summarise(Population = sum(Population)) %>% 
    mutate(Population = Population %>% round)
  
  infections %>% 
    left_join(deaths) %>% 
    left_join(population) %>% 
    rename(`Local authority` = Local_authority,
           `Potential Fatalities` = Fatalities) %>% 
    left_join(get_age_group_dict()) %>% 
    mutate(Age_group = `Age group`) %>% 
    select(-`Age group`) %>% 
    rename(`Age group` = Age_group) %>% 
    write_csv("Outputs/" %>% str_c(.filename,".csv"))
}

#bar charts of estimated (total) hospital bed and icu bed demand, for slide pack
output_charts = function(estimates, 
                         intervention_id, 
                         intervention,
                         end_date,
                         start_date = end_date + 1 - 16*7, 
                         suffix="",
                         include_lines=TRUE,
                         included_equipment = NULL, 
                         current_date = today()) { #in output.R
  equipment_data = get_equipment_data(included_equipment, 1000) %>% 
    filter(Date >= start_date, Date <= end_date)
  
  hosp_beds = equipment_data %>% 
    filter(Equipment == "Beds")
  
  hosp = estimates %>% 
    filter(Status %in% c("Oxygen", "Non-invasive ventilation", "Invasive ventilation")) %>% 
    group_by(Date, Age_group) %>% 
    summarise(Number_at = sum(Number_at)) %>% 
    filter(Date >= start_date,Date <= end_date)
  
  ymax = hosp %>% 
    group_by(Date) %>% 
    summarise(Number_at = sum(Number_at)) %>% 
    .$Number_at %>% 
    c(hosp_beds$Capacity) %>% 
    max
  
  hosp %>% 
    bed_demand_curve("Hospital",
                     intervention, 
                     hosp_beds, 
                     #min_week=min_week, 
                     #max_week=max_week, 
                     current_date = current_date) +
      coord_cartesian(ylim = c(0, 10000))
  ggsave("Outputs/curves/" %>% 
           str_c(intervention_id, " ", intervention, "_hospital", suffix, ".png"), 
         height = 10.97, 
         width = 14.65, 
         units = "cm")
  
  icu_equipment = equipment_data %>% 
    filter(Equipment != "Beds")
  
  estimates %>% 
    filter(Status == "Invasive ventilation") %>% 
    group_by(Date, Age_group) %>% 
    summarise(Number_at = sum(Number_at)) %>% 
    filter(Date >= start_date, Date <= end_date) %>% 
    bed_demand_curve("ICU", 
                     intervention, 
                     icu_equipment,
                     #min_week=min_week, 
                     #max_week=max_week, 
                     include_lines=include_lines, 
                     current_date = current_date) +
      coord_cartesian(ylim = c(0, 1000))
  ggsave("Outputs/curves/" %>% 
           str_c(intervention_id, " ", intervention, "_intensive", suffix, ".png"), 
         height = 10.97, 
         width = 14.65, 
         units = "cm")
  
  
}

get_age_group_dict = function() {  #in output.R
  
  tibble(Age_group = c(0,10,20,30,40,50,60,70,80),
         `Age group` = c("Under 10", "10 to 19", "20 to 29", "30 to 39", "40 to 49", "50 to 59", "60 to 69", "70 to 79", "80 and over"))
}
