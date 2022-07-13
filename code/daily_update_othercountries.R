
get_population_othercountries=function(...) {#in daily_update_othercountries.R
  
  if("~Health_board" %in% enquos(...) & "~Local_authority" %in% enquos(...))
    stop("SIMD can't be combined with Health_board")
  
  pop_data = get_pop_data_england()
  
  if("~Age" %in% enquos(...)) 
    population = pop_data %>% group_by(Age) %>% summarise(Population = sum(Persons))
  else {
    temp = pop_data %>% 
      select(-`Area code`) %>% 
      rename(Population = Persons) %>% 
      mutate(Age_group = Age %>% map_dbl(get_age_group) %>% as.integer) %>% 
      group_by(Area1,Age_group) %>% 
      summarise(Population = sum(Population))
    
    if("~In_care" %in% enquos(...)) {
      if("~Health_board" %in% enquos(...)) 
        stop("In_care can't be combined with Health_board")
      
      care_data = "Data/care_homes.xlsx" %>% 
        read_excel(sheet = "raw9", skip=1) %>% 
        filter(KeyStatistic %>% str_detect("Male and Female"),
               MainClientGroup == "All Adults") %>% 
        mutate(Age_group = KeyStatistic %>% map_int(convert_KeyStatistic_to_Age_group)) %>% 
        select(Area1 = LocalAuthority, Age_group, In_care = `2017`) %>% 
        mutate(In_care = In_care %>% as.integer,
               Area1 = Area1 %>% str_replace("&", "and"),
               Area1 = Area1 %>% str_replace("Edinburgh, City of", "City of Edinburgh")) %>% 
        replace_na(list(In_care = 0)) %>% 
        group_by(Area1, Age_group) %>% 
        summarise(In_care = sum(In_care)) %>% 
        filter(Area1 != "Scotland")
      
      temp = temp %>%
        left_join(care_data, 
                  by = c("Area1", "Age_group")) %>% 
        replace_na(list(In_care = 0)) %>% 
        mutate(No = Population - In_care) %>% 
        rename(Yes = In_care) %>% 
        select(-Population) %>% 
        gather(In_care, Population, No, Yes)
    }
    
    if("~Vulnerable" %in% enquos(...)) {
      denoms = pop_data %>% 
        mutate(Category = ifelse(Age>=65,"65 and over","Under 65")) %>% 
        group_by(Category) %>% 
        summarise(Population = sum(Persons)) %>% 
        bind_rows(pop_data %>% filter(Age<40, Age>20) %>% summarise(Population = sum(Persons)) %>% mutate(Category="Pregnant no risk"),
                  pop_data %>% filter(Age<40, Age>20) %>% summarise(Population = sum(Persons)) %>% mutate(Category="Pregnant at risk"))
      
      vulnerable = "Data/flu_vaccine.csv" %>% 
        read_csv %>% 
        left_join(denoms) %>% 
        mutate(proportion_at_risk = (Total/Population) %>% map_dbl(min,1)) %>% 
        select(Category, proportion_at_risk)
      vulnerable = vulnerable %>% 
        bind_rows(vulnerable %>% 
                    filter(Category %in% c("65 and over", "Under 65")) %>%
                    summarise(proportion_at_risk = mean(proportion_at_risk)) %>% 
                    mutate(Category = "60 to 69")) %>% 
        right_join(tibble(Age_group = c(0,10,20,30,40,50,60,70,80,90,20,30),
                          Category = c("Under 65","Under 65","Under 65","Under 65","Under 65","Under 65",
                                       "60 to 69",
                                       "65 and over","65 and over","65 and over",
                                       "Pregnant at risk", "Pregnant at risk"))) %>% 
        group_by(Age_group) %>% 
        summarise(proportion_at_risk = sum(proportion_at_risk))
      
      temp = temp %>% 
        left_join(vulnerable) %>% 
        mutate(Yes = Population*proportion_at_risk,
               No = Population - Yes) %>% 
        select(-Population,-proportion_at_risk) %>% 
        gather(Vulnerable, Population, Yes, No)
    }
    
    if("~SIMD" %in% enquos(...)) {
      if("~Health_board" %in% enquos(...)) stop("SIMD can't be combined with Health_board")
      
      simd = "Data/simd.csv" %>% 
        read_csv %>% 
        get_simd %>% 
        select(Area1 = Council_area,
               Upper4fifths_proportion,
               Lowerfifth_proportion)
      
      temp = temp %>% 
        left_join(simd) %>% 
        mutate(Upper4fifths = Upper4fifths_proportion*Population,
               Lowerfifth = Lowerfifth_proportion*Population) %>% 
        select(-Upper4fifths_proportion, -Lowerfifth_proportion, -Population) %>% 
        gather(SIMD, Population, Upper4fifths, Lowerfifth)
    }
    
    if("~Health_board" %in% enquos(...)) 
      temp = temp %>% rename(Health_board = Area1)
    if("~Local_authority" %in% enquos(...)) 
      temp = temp %>% rename(Local_authority = Area1)
    
    population = temp %>% 
      group_by(...) %>% 
      summarise(Population = sum(Population)) 
  }
  
  population
}



run_model_othercountries = function(..., .scenario = 0) {#in daily_update_othercountries.R
  
  scenarios = "Data/Model Assumptions.xlsx" %>% get_scenarios
  ###read in population data
  population = get_population_othercountries(...)
  
  ###read in assumptions
  scenarios %>% 
    filter(ID %in% .scenario) %>% 
    mutate(assumptions = ID %>%
             map(prepare_assumptions,...)) %>% 
    ###calculate infections by Age_group
    mutate(results = assumptions %>% map(get_results, .population = population)) %>% 
    ###produce estimates
    mutate(results_by_day = results %>% map(set_to_curve, get_curve("days")))
}


#daily update
run_daily_update_2 = function() {#in daily_update_othercountries.R
  
  results_by_day_table = run_model_othercountries(Age_group, .scenario = 0:2) #change .scenario as appropriate
  
  #mobilisation spreadsheet
  results_by_day_table %>% create_mobilisation_spreadsheet
  #hospital demand curves for slidepack
  results_by_day_table %>% create_demand_curves
  
  #unavailability ready reckoner
  run_model_simple(0:2) %>% create_unavailability_for_work
}
