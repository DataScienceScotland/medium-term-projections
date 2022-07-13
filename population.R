#find proportions of each council area with lowest quintile SIMD (most deprived)
get_simd = function(simd_data) { #in population.R
  #calculate the total population in each council area
  total_population = simd_data %>% 
    #within each Council, sum the datazone populations
    group_by(Council_area) %>% 
    summarise(total_population = sum(Total_population))
  #calculate the deprived population in each council area
  deprived_population = simd_data %>% 
    #remove all the SIMD quintiles except for the lowest quintile
    filter(SIMD2020v2_Quintile == 1) %>% 
    #within each Council, sum the deprived datazone populations
    group_by(Council_area) %>% 
    summarise(deprived_population = sum(Total_population))
  #calculate the proportion of each council area that is most deprived
  total_population %>% 
    left_join(deprived_population) %>% 
    #set missing values to zero (where the council has no datazones that are most deprived)
    replace_na(list(deprived_population=0)) %>% 
    #calculate the proportions
    mutate(Lowerfifth_proportion = deprived_population/total_population,
           Upper4fifths_proportion = 1-Lowerfifth_proportion,
           Council_area = Council_area %>% str_replace("an Iar", "Siar"))
}

#gets the total population for a given age range
get_population = function(...) { #in population.R
  
  if("~Health_board" %in% enquos(...) & "~Local_authority" %in% enquos(...))
    stop("Local_authority can't be combined with Health_board")
  
  pop_data = get_pop_data()
  
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
      care_numbers = "care_homes.xlsx" %>% 
        get_filepath("Data") %>% 
        read_excel(sheet = "raw9", skip=1) %>% 
        filter(KeyStatistic == "Number of Long Stay Residents",
               MainClientGroup == "All Adults") %>% 
        select(Area1 = LocalAuthority, area_total = `2017`)
      
      care_data = "care_homes.xlsx" %>% 
        get_filepath("Data") %>% 
        read_excel(sheet = "raw9", skip=1) %>% 
        filter(KeyStatistic %>% str_detect("Male and Female"),
               MainClientGroup == "All Adults",
               LocalAuthority != "Scotland") %>% 
        mutate(Age_group = KeyStatistic %>% map_int(convert_KeyStatistic_to_Age_group)) %>% 
        select(Area1 = LocalAuthority, Age_group, percentage = `2017`) %>% 
        left_join(care_numbers) %>% 
        mutate(area_total = area_total %>% as.integer,
               percentage = percentage %>% as.double,
               Area1 = Area1 %>% str_replace("&", "and"),
               Area1 = Area1 %>% str_replace("Edinburgh, City of", "City of Edinburgh")) %>% 
        group_by(Area1, Age_group) %>% 
        summarise(In_care = sum(area_total*percentage/100, na.rm=TRUE))
      
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
      
      vulnerable = "flu_vaccine.csv" %>% 
        get_filepath("Data") %>% 
        read_csv(col_types = cols(
          Category = col_character(),
          Total = col_double()
        )) %>% 
        left_join(denoms) %>% 
        mutate(proportion_at_risk = (Total/Population) %>% map_dbl(min,1)) %>% 
        select(Category, proportion_at_risk)
      vulnerable = vulnerable %>% 
        bind_rows(vulnerable %>% 
                    filter(Category %in% c("65 and over", "Under 65")) %>%
                    summarise(proportion_at_risk = mean(proportion_at_risk)) %>% 
                    mutate(Category = "60 to 69")) %>% 
        right_join(tibble(Age_group = c(0,10,20,30,40,50,60,70,80,20,30),
                          Category = c("Under 65","Under 65","Under 65","Under 65","Under 65","Under 65",
                                       "60 to 69",
                                       "65 and over","65 and over",
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
      simd = "simd.csv" %>% 
        get_filepath("Data") %>% 
        read_csv(col_types = cols(
          .default = col_double(),
          Data_Zone = col_character(),
          Intermediate_Zone = col_character(),
          Council_area = col_character(),
          income_rate = col_character(),
          employment_rate = col_character(),
          DEPRESS = col_character(),
          LBWT = col_character(),
          Attendance = col_character(),
          Attainment = col_character(),
          not_participating = col_character(),
          University = col_character(),
          crime_count = col_character(),
          crime_rate = col_character(),
          overcrowded_rate = col_character(),
          nocentralheating_rate = col_character(),
          broadband = col_character()
        )) %>% 
        get_simd %>% 
        select(Area1 = Council_area,
               Upper4fifths_proportion,
               Lowerfifth_proportion)
      
      temp = temp %>% 
        left_join(simd) %>% 
        mutate(U = Upper4fifths_proportion*Population,
               L = Lowerfifth_proportion*Population) %>% 
        select(-Upper4fifths_proportion, -Lowerfifth_proportion, -Population) %>% 
        gather(SIMD, Population, U, L)
    }
    
    if("~Immune_compromised" %in% enquos(...) & "~SIMD" %in% enquos(...)) {
      
      # If temp includes SIMD:
      
      # Read in the immune-compromised population by SIMD quintile and split between lower (quintile 1) and upper (everyone else).
      immune_compromised_simd = "Assumptions - Immune-compromised - Sources.xlsx" %>%
        get_filepath("Data") %>%
        read_excel(sheet = "Immune-Compromised Population", range = "A36:F45") %>% 
        group_by(Age_group) %>% 
        mutate(L = `1`, U = sum(`2`, `3`, `4`, `5`) ) %>% 
        ungroup %>% 
        select(Age_group, U, L) %>% 
        gather(SIMD, Immune_compromised_population, U, L)
      
      # Get the total population by age group and SIMD and calculate the proportion who are immune-compromised.
      immune_compromised_simd_proportions = temp %>% 
        group_by(Age_group, SIMD) %>% 
        summarise(Population = sum(Population) ) %>% 
        ungroup %>% 
        left_join(immune_compromised_simd) %>% 
        mutate(Immune_compromised_proportion = Immune_compromised_population / Population) %>% 
        select(Age_group, SIMD, Immune_compromised_proportion)
      
      # Apply the proportion immune-compromised to the population broken down by local authority, care status, etc.
      temp = temp %>% 
        left_join(immune_compromised_simd_proportions) %>% 
        mutate(Yes = Population * Immune_compromised_proportion, No = Population - Yes) %>% 
        select(-Population, -Immune_compromised_proportion) %>% 
        gather(Immune_compromised, Population, No, Yes)
      
    } else if("~Immune_compromised" %in% enquos(...)) {
      
      # If temp doesn't include SIMD:
      
      # Read in the immune-compromised population.
      immune_compromised = "Assumptions - Immune-compromised - Sources.xlsx" %>%
        get_filepath("Data") %>%
        read_excel(sheet = "Immune-Compromised Population", range = "A22:B31")
      
      # Get the total population by age group and calculate the proportion who are immune-compromised.
      immune_compromised_proportions = temp %>% 
        group_by(Age_group) %>% 
        summarise(Population = sum(Population) ) %>% 
        ungroup %>% 
        left_join(immune_compromised) %>% 
        mutate(Immune_compromised_proportion = Immune_compromised_population / Population) %>% 
        select(Age_group, Immune_compromised_proportion)
      
      # Apply the proportion immune-compromised to the population broken down by local authority, care status, etc.
      temp = temp %>% 
        left_join(immune_compromised_proportions) %>% 
        mutate(Yes = Population * Immune_compromised_proportion, No = Population - Yes) %>% 
        select(-Population, -Immune_compromised_proportion) %>% 
        gather(Immune_compromised, Population, No, Yes)
    }
    
    if("~Health_board" %in% enquos(...)) {
      temp = "LA_to_HB_lookup.xlsx" %>% 
        get_filepath("Data") %>% 
        read_excel %>% 
        select(Area1 = LA_Name, Health_board = HBName) %>% 
        full_join(temp)
    }
    if("~Local_authority" %in% enquos(...)) 
      temp = temp %>% rename(Local_authority = Area1)
    
    population = temp %>% 
      group_by(...) %>% 
      summarise(Population = sum(Population))
  }
  
  population %>% 
    ungroup
}

convert_KeyStatistic_to_Age_group = function(label) { #in population.R
  
  dict = tibble(KeyStatistic = c("18-64", "65-74", "75-84", "85-94", "95 and Older"),
                Age_group = c(60L,70L,80L,80L,80L)) %>% 
    filter(KeyStatistic == label %>% str_remove("Male and Female ")) %>% 
    .$Age_group
}

