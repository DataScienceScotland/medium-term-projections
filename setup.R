library(zoo)
library(furrr)

# get_curve = function(cutoff=0, filename = "Scenario setup.xlsx") { #in setup.R
#   scenarios = filename %>% 
#     get_filepath("Data") %>% 
#     get_scenarios %>% 
#     nest(-`Curve method`, -`Curve file`, .key = scenario_info)
#   
#   pmap_dfr(scenarios, 
#            function(scenario_info, `Curve method`, `Curve file`) do.call(`Curve method`, list(scenario_info, `Curve file`)))
# }

# get_curve_sg = function(scenario_info, file) { #in setup.R
#   id_root = min(scenario_info$ID)
#   horizon = max(scenario_info$Horizon)
#   
#   file %>% 
#     str_c(".csv") %>% 
#     get_filepath("Data") %>% 
#     get_bayesian_estimate %>% 
#     forecast_new_cases(horizon = horizon, testing=FALSE) %>% 
#     select(-Day) %>%
#     rename(`0` = predicted_max,
#            `1` = predicted_cases,
#            `2` = predicted_min) %>% 
#     unnest %>% 
#     get_curve_scenario %>% 
#     mutate(ID = ID + id_root)
# }

get_bayesian_estimate = function(filename) { #in setup.R
  filename %>% 
    read_csv(col_types=c(
    time = col_character(),
    country = col_character(),
    reported_cases = col_double(),
    reported_cases_cum = col_double(),
    predicted_cases_cum = col_double(),
    predicted_min_cum = col_double(),
    predicted_max_cum = col_double(),
    predicted_cases = col_double(),
    predicted_min = col_double(),
    predicted_max = col_double(),
    reported_deaths = col_double(),
    deaths_cum = col_double(),
    estimated_deaths_cum = col_double(),
    death_min_c = col_double(),
    death_max_c = col_double(),
    estimated_deaths = col_double(),
    death_min = col_double(),
    death_max = col_double(),
    rt = col_double(),
    rt_min = col_double(),
    rt_max = col_double()
  )) %>% 
    mutate(X1 = 1:NROW(.))
}

# run_curve_epi = function(scenario_info, scenario) { #in setup.R
#   
#   id_root = min(scenario_info$ID)
#   
#   levels = scenario_info %>% 
#     pull(Level) %>% 
#     str_replace("Median", "50") %>% 
#     str_remove("th percentile") %>% 
#     as.double %>% 
#     `/`(100)
#   
#   phases = "Forecast scenarios.xlsx" %>%
#     get_filepath("Data") %>% 
#     read_excel(sheet = scenario)
#   
#   sim_output = phases %>% 
#     add_vaccine %>% 
#     run_simulation_from_scenario(100)
#   
#   sim_output %>% 
#     saveRDS(scenario %>% str_c("SCHEMa outputs/", ., " sim_output.rds"))
#   
#   efficacy_adjustments = phases %>% 
#     transmute(Date = ymd(start_date), efficacy_adjustment)
#   
#   sim_output %>%
#     nest(-Date) %>% 
#     mutate(ID = list(0:(length(levels)-1)),
#            Level = list(levels),
#            Cases = map2(data,
#                         Level,
#                         ~quantile(.x$Cases, probs = .y), names=FALSE)) %>% 
#     select(-data) %>% 
#     unnest %>% 
#     select(-Level) %>% 
#     get_curve_scenario(efficacy_adjustments) %>% 
#     mutate(ID = ID + id_root)
# }

# get_curve_epi = function(scenario_info, scenario) { #in setup.R
#   id_root = min(scenario_info$ID)
#   
#   levels = scenario_info %>% 
#     pull(Level) %>% 
#     str_replace("Median", "50") %>% 
#     str_remove("th percentile") %>% 
#     as.double %>% 
#     `/`(100)
#   
#   phases = "Forecast scenarios.xlsx" %>%
#     get_filepath("Data") %>% 
#     read_excel(sheet = scenario)
#   
#   efficacy_adjustments = phases %>% 
#     transmute(Date = ymd(start_date), efficacy_adjustment)
#   
#   scenario %>% 
#     str_c(" sim_output.rds") %>% 
#     get_filepath("SCHEMa outputs") %>% 
#     readRDS %>% 
#     nest(-Date) %>% 
#     mutate(ID = list(0:(length(levels)-1)),
#            Level = list(levels),
#            Cases = map2(data,
#                         Level,
#                         ~quantile(.x$Cases, probs = .y), names=FALSE)) %>% 
#     select(-data) %>% 
#     unnest %>% 
#     select(-Level) %>% 
#     get_curve_scenario(efficacy_adjustments) %>% 
#     mutate(ID = ID + id_root)
# }

##new version based on projecting forward historical age group breakdown
get_curve_scenario = function(df, efficacy_adjustments) { #in setup.R
  population = get_population(Age_group)
  
  past_vaccine_data = get_vaccine_history()
  future_vaccine_data = get_vaccine_roll_out(max(past_vaccine_data$Date) + 1)
  
  all_vaccine_data = estimate_roll_out_by_age(past_vaccine_data, future_vaccine_data)
  
  susceptibility = get_immunity_data(all_vaccine_data, efficacy_adjustments) %>%
    # Keep the immune-compromised flag.
    select(Date, Age_group, Immune_compromised, Immune, Safe) %>% 
    complete(Date = unique(df$Date), Age_group = 10*(0:8), Immune_compromised = c("No", "Yes") ) %>% 
    filter(!(Date < "2021-07-01" & Immune_compromised == "Yes") ) %>% 
    group_by(Age_group, Immune_compromised) %>% 
    fill(Immune, Safe) %>% 
    replace_na(list(Immune = 0, Safe = 0)) %>%
    ungroup %>% 
    left_join(population) %>% 
    # Separate the immune-compromised population.
    left_join("Assumptions - Immune-compromised - Sources.xlsx" %>%
                get_filepath("Data") %>%
                read_excel(sheet = "Immune-Compromised Population", range = "A22:B31")
             )
  
  historic_proportions = get_proportions_from_historic(df$Date %>% max)
  
  if(! "apply_vaccine" %>% exists) apply_vaccine = FALSE
  
  if(apply_vaccine) {
    proportions = susceptibility %>%
      # For immune-compromised people, calculate using the immune-compromised population.
      mutate(Susceptible = ifelse(Immune_compromised == "Yes",
                                  Immune_compromised_population - Immune,
                                  (Population - Immune_compromised_population) - Immune
                                 ),
             Proportion_unsafe = ifelse(Immune_compromised == "Yes",
                                        (Immune_compromised_population - Safe) / Susceptible,
                                        (Population - Immune_compromised_population - Safe) / Susceptible
                                       ) 
            ) %>% 
      group_by(Date) %>% 
      mutate(Proportion_of_cases = Susceptible / sum(Susceptible) ) %>% 
      ungroup %>% 
      left_join(historic_proportions) %>% 
      mutate(Proportion_of_cases = Proportion) %>% 
      right_join(df) %>% 
      # Keep the immune-compromised flag.  Don't use the immune-compromised population in the denominator as this gives
      # ridiculously high proportions because Cases in the numerator are not broken down by immune-compromised status. 
      transmute(Date,
                ID,
                Age_group,
                Immune_compromised,
                proportion_in_period = Cases * Proportion_of_cases * Proportion_unsafe / Population / 0.8)
  }
  else {
    proportions = susceptibility %>%
      mutate(Susceptible = Population - Immune) %>% 
      group_by(Date) %>% 
      mutate(Proportion_of_cases = Susceptible / sum(Susceptible) ) %>% 
      ungroup %>% 
      left_join(historic_proportions) %>% 
      mutate(Proportion_of_cases = Proportion) %>% 
      right_join(df) %>% 
      # Keep the immune-compromised flag.
      transmute(Date,
                ID,
                Age_group,
                Immune_compromised,
                proportion_in_period = Cases * Proportion_of_cases / Population / 0.8)
  }
  
  proportions
}

get_proportions_from_historic = function(end_date) { #in setup.R
  age_group_dict = tibble(Age_group = c(0, 10, 10, 20, 20, 30,
                                        40, 40, 50, 60, 60,
                                        70, 70, 80, 80),
                          AgeGroup = c("0 to 14", "0 to 14", "15 to 19", "20 to 24", "25 to 44", "25 to 44", 
                                       "25 to 44", "45 to 64", "45 to 64", "45 to 64", "65 to 74",
                                       "65 to 74", "75 to 84",  "75 to 84", "85plus"),
                          Contribution = c(2/3, 1/3, 1, 1, 1/4, 1/2,
                                           1/4, 1/4, 1/2, 1/4, 1/2,
                                           1/2, 1/2, 1/2, 1))
  
  
  
  read_csv("Data/trend_agesex.csv") %>% 
    filter(! AgeGroup %in% c("0 to 59", "60+", "Total")) %>% 
    mutate(Date = Date %>% ymd) %>% 
    left_join(age_group_dict) %>% 
    group_by(Date, Age_group) %>%
    summarise(Cases = sum(DailyPositive*Contribution)) %>% 
    group_by(Date) %>% 
    mutate(Proportion = Cases/sum(Cases)) %>% 
    group_by(Age_group) %>% 
    mutate(Proportion = ifelse(Proportion %>% is.nan, NA, Proportion) %>% 
             ma(30)) %>% 
    select(-Cases) %>% 
    ungroup %>% 
    complete(Date = seq(ymd("2020-01-01"), end_date, by = 1),
             Age_group = 10*(0:8)) %>% 
    group_by(Age_group) %>% 
    fill(Proportion) %>% 
    fill(Proportion, .direction = "up") %>% 
    ungroup
}

run_simulation_from_scenario = function(phases, 
                                        n_simulations, 
                                        history = "history.rds" %>%
                                          get_filepath("Data") %>%
                                          readRDS(), 
                                        var = 20,
                                        population = NULL,
                                        forecast_start_date = min(phases$start_date),
                                        natural_immunity_time = get_immunity_time("natural"),
                                        vaccine_immunity_time = get_immunity_time("vaccine")) { #in setup.R
  oplan <- plan()
  on.exit(plan(oplan), add = TRUE)
  plan(multisession)
  
  if(population %>% is_null)  population = get_population() %>% pull(Population)
  
  1:n_simulations %>%
    future_map_dfr(run_sim,
                   history %>% filter(Date < forecast_start_date),
                   phases %>% add_vaccine,
                   var = var,
                   population = population,
                   natural_immunity_time = natural_immunity_time,
                   vaccine_immunity_time = vaccine_immunity_time)
}

# read_imperial = function(file,ID,Horizon,cutoff,...) { #in setup.R
#   curve = file %>% 
#     str_c(".csv") %>% 
#     get_filepath("Data") %>% 
#     read_csv(col_types = c(
#       .default = col_double()
#     )) %>%
#     mutate(ID = ID,
#            infection_start_date=t,
#            `0` = (`I0-5` + `I5-10`),
#            `10` = (`I10-15` + `I15-20`),
#            `20` = (`I20-25` + `I25-30`),
#            `30` = (`I30-35` + `I35-40`),
#            `40` = (`I40-45` + `I45-50`),
#            `50` = (`I50-55` + `I55-60`),
#            `60` = (`I60-65` + `I65-70`),
#            `70` = (`I70-75` + `I75-80`),
#            `80` = (`I80-85`)) %>%
#     select(ID, infection_start_date,`0`,`10`,`20`,`30`,`40`,`50`,`60`,`70`,`80`) %>%
#     gather(Age_group,cumulative,3:11) %>%  
#     filter(! infection_start_date %>% is.na) %>% 
#     group_by(ID, Age_group) %>%
#     mutate(new_in_period = cumulative %>% diff %>% c(0,.),
#            proportion_in_period = new_in_period/sum(new_in_period)) %>%
#     ungroup %>%
#     filter(proportion_in_period < 0.75) %>% 
#     select(ID, infection_start_date, Age_group, proportion_in_period)
#   
#   low_end_cutoff = cutoff
#   cutoff_value = curve %>%
#     filter(Age_group=="40",
#            infection_start_date == cutoff) %>%
#     .$proportion_in_period
#   high_end_cutoff = curve %>%
#     filter(Age_group=="40",
#            infection_start_date > cutoff,
#            proportion_in_period > cutoff_value) %>%
#     .$infection_start_date %>%
#     min
#   
#   if(length(high_end_cutoff) == 0) high_end_cutoff = cutoff
#   else if(is.infinite(high_end_cutoff)) high_end_cutoff = cutoff
#   
#   #Cut out the extra months of intervention
#   # bind_rows(curve %>%
#   #             spread(Age_group, proportion_in_period) %>%
#   #             filter(infection_start_date<=cutoff),
#   #           curve %>%
#   #             spread(Age_group, proportion_in_period) %>%
#   #             filter(infection_start_date>cutoff & `0`>=`0`[cutoff+1] & infection_start_date<(curve %>% filter(proportion_in_period==max(proportion_in_period)))$infection_start_date),
#   #           curve %>%
#   #             spread(Age_group, proportion_in_period) %>%
#   #             filter(infection_start_date>=(curve %>% filter(proportion_in_period==max(proportion_in_period)))$infection_start_date)) %>%
#   #   gather(Age_group,proportion_in_period,3:12) %>%
#   #   group_by(ID, Age_group) %>%
#   #   mutate(infection_start_date = seq(1,NROW(Age_group))) %>%
#   #   ungroup
#   curve %>%
#     filter(infection_start_date <= low_end_cutoff | infection_start_date >= high_end_cutoff) %>%
#     group_by(ID, Age_group) %>%
#     mutate(infection_start_date = seq(0,NROW(Age_group)-1)) %>%
#     ungroup %>% 
#     mutate(Date = infection_start_date + ymd("2020-01-20")) %>%
#     mutate(Age_group = as.integer(Age_group)) %>% 
#     filter(Date <= today() + Horizon)
# }

# get_curve_imperial = function(scenario_info, file, cutoff=0){ #in setup.R
#   scenario_info %>% 
#     #rename(file=`Curve file`) %>% 
#     pmap_dfr(read_imperial, file, cutoff) %>% 
#     select(-infection_start_date)
# }


get_scenarios = function(assumptions_file) { #in setup.R
    assumptions_file %>% 
    read_excel("Scenario key")
}

read_in_assumption_file = function(filename){ #in setup.R
  filename %>% 
    get_filepath("Data") %>% 
    read_csv(col_types=cols(
      Assumption = col_character(),
      Value = col_double()
    ))
}


#munge the assumptions data into the format we want
prepare_assumptions = function(scenario=0, ...){#in setup.R
  
  if("~In_care" %in% enquos(...)) 
    In_care_include = c("Yes", "No")
  else 
    In_care_include = c("No")
  
  if("~Age_group" %in% enquos(...)) 
    Age_group_include = 10*(0:9)
  else 
    Age_group_include = as.integer(40)
  
  if("~Vulnerable" %in% enquos(...)) 
    Vulnerable_include = c("No","Yes")
  else 
    Vulnerable_include = c("No")
  
  # Add immune-compromised.
  if("~Immune_compromised" %in% enquos(...)) 
    Immune_compromised_include = c("No","Yes")
  else 
    Immune_compromised_include = c("No")
  
  x=map(c("Assumptions - Infection rates.csv",
          "Assumptions - Generic.csv",
          "Assumptions - Hospitalisation rates.csv",
          "Assumptions - Ventilation rates.csv",
          "Assumptions - Fatality rates.csv",
          "Assumptions - SIMD factor.csv"),
        read_in_assumption_file) %>%
    bind_rows %>%
    mutate(Assumption = Assumption %>% 
             recode(`Proportion of infected people requiring hospitalisation` = "Requiring hospitalisation rate",
                    `Ventilation rate by age group and in-care` = "Ventilation rate",
                    `Proportion of ventilation which is invasive` = "Ventilation proportion invasive")) %>%
    # Add immune-compromised.
    transmute(Assumption, Value, Age_group=`Lower end age`, Vulnerable, In_care, Immune_compromised) %>%
    replace_na(list(Age_group = list(Age_group_include))) %>%
    unnest %>%
    replace_na(list(Vulnerable = list(Vulnerable_include))) %>%
    unnest %>%
    replace_na(list(In_care = list(In_care_include))) %>%
    unnest %>%
    # Add immune-compromised.
    replace_na(list(Immune_compromised = list(Immune_compromised_include))) %>%
    unnest %>%
    filter(Age_group %in% Age_group_include,
           Vulnerable %in% Vulnerable_include,
           In_care %in% In_care_include,
           # Add immune-compromised.
           Immune_compromised %in% Immune_compromised_include)
  
  if("~SIMD" %in% enquos(...)){
    x %>%
      filter(Assumption!="SIMD factor") %>%
      # Add immune-compromised.
      left_join(x %>%
                  filter(Assumption=="SIMD factor"),
                by=c("Age_group", "Vulnerable", "In_care", "Immune_compromised")) %>%
      transmute(Assumption = Assumption.x,
                Value = ifelse(Assumption.x == "Requiring hospitalisation rate",
                               Value.x * Value.y,
                               Value.x),
                Age_group,
                Vulnerable,
                In_care,
                Immune_compromised,
                SIMD = "L") %>%
      bind_rows(x %>%
                  filter(Assumption!="SIMD factor")) %>%
      replace_na(list(SIMD = "U")) %>%
      # Add immune-compromised.
      arrange(Assumption, Value, Age_group, SIMD, Vulnerable, In_care, Immune_compromised)
  }
  
  else{
    x %>%
      filter(Assumption!="SIMD factor") %>%
      mutate(SIMD = "U") %>%
      # Add immune-compromised.
      arrange(Assumption, Value, Age_group, SIMD, Vulnerable, In_care, Immune_compromised)
  }
}

convert_range = function(range) { #in setup.R
  if(range %>% str_detect("~")) {
    temp = range %>% 
      str_split("~") %>% 
      unlist
    seq.int(temp[1],temp[2]) %>% 
      as.character
  }
  else range
}

#converts a character definition of a demographic to an age range
get_age_group = function(age) { #in setup.R
  if(age<10) 0
  else if(age<20) 10
  else if(age<30) 20
  else if(age<40) 30
  else if(age<50) 40
  else if(age<60) 50
  else if(age<70) 60
  else if(age<80) 70
  else 80
}

#pull in population data by local authority. Uses 2019 estimates
get_pop_data = function(area_type="Local_authority") { #in setup.R
  selected_rows = switch(area_type,
                         Local_authority = 3:34,
                         Health_board = 37:50)
  withCallingHandlers({
    #code
    data="mid-year-pop-est-20-data_Table 2.csv" %>% 
      get_filepath("Data") %>% 
      read_csv(skip = 3,
               col_types= c(.default = col_character()))
  },
  warning=function(w){
    #what to do with warning
    if(conditionMessage(w) %>% str_detect("Missing column names filled in:"))
      invokeRestart("muffleWarning")
  })
  data %>%
    select(c("Area code", "Area name",   "0",          "1",          "2",          "3",          "4",         
             "5",          "6",          "7",          "8",          "9",          "10",         "11",         "12",        
             "13",         "14",         "15",         "16",         "17",         "18",         "19",         "20",        
             "21",         "22",         "23",         "24",         "25",         "26",         "27",         "28",        
             "29",         "30",         "31",         "32",         "33",         "34",         "35",         "36",        
             "37",         "38",         "39",         "40",         "41",         "42",         "43",         "44",        
             "45",         "46",         "47",         "48",         "49",         "50",         "51",         "52",        
             "53",         "54",         "55",         "56",         "57",         "58",         "59",         "60",        
             "61",         "62",         "63",         "64",         "65",         "66",         "67",         "68",        
             "69",         "70",         "71",         "72",         "73",         "74",         "75",         "76",        
             "77",         "78",         "79",         "80",         "81",         "82",         "83",         "84",        
             "85",         "86",         "87",         "88",         "89",         "90+" )) %>%
    slice(selected_rows) %>% 
    rename(#`Area code`="Area code1",
           Area1 =`Area name`) %>%
    gather(Age, Persons, -`Area code`, -Area1) %>% 
    filter(Age!="All Ages") %>% 
    mutate(Age = Age %>% 
             str_remove_all("\\+") %>% 
             as.integer, 
           Persons = Persons %>% 
             str_remove_all(",") %>% 
             as.integer) %>% 
    filter(! Age %>% is.na)
}



#converts the assumptions table in excel to the required parameters for the model
prepare_model = function(df, ..., percentile_multiplier = NA) { #in setup.R
  
  # AS: Added an option to multiply the hospitalisation rate by a given percentage for the 5th and 95th percentiles of the actuals.
  
  #critical - needs ventilation
  #severe - needs hospital, but not ventilation
  #moderate - symptomatic but not severe
  #mild - not symptomatic
  if (!is.na(percentile_multiplier) ) {
    temp = df %>% 
      spread(Assumption, Value) %>% 
      mutate(Mild_infection_rate = `Infection rate`*(1 - `Symptomatic rate`),
             Critical_infection_rate = ifelse(Level == "5th percentile",
                                              `Infection rate` * `Requiring hospitalisation rate` * (1 - percentile_multiplier) * `Ventilation rate` * `Ventilation proportion invasive`,
                                              ifelse(Level == "95th percentile",
                                                     `Infection rate` * `Requiring hospitalisation rate` * (1 + percentile_multiplier) * `Ventilation rate` * `Ventilation proportion invasive`,
                                                     `Infection rate` * `Requiring hospitalisation rate` * `Ventilation rate` * `Ventilation proportion invasive`
                                                    )
                                             ),
             Severe_infection_rate = ifelse(Level == "5th percentile",
                                            `Infection rate` * `Requiring hospitalisation rate` * (1 - percentile_multiplier) * `Ventilation rate` * (1-`Ventilation proportion invasive`),
                                            ifelse(Level == "95th percentile",
                                                   `Infection rate` * `Requiring hospitalisation rate` * (1 + percentile_multiplier) * `Ventilation rate` * (1-`Ventilation proportion invasive`),
                                                   `Infection rate` * `Requiring hospitalisation rate` * `Ventilation rate` * (1-`Ventilation proportion invasive`)
                                                  )
                                           ),
             Serious_infection_rate = ifelse(Level == "5th percentile",
                                             `Infection rate` * `Requiring hospitalisation rate` * (1 - percentile_multiplier) - Critical_infection_rate - Severe_infection_rate,
                                             ifelse(Level == "95th percentile",
                                                    `Infection rate` * `Requiring hospitalisation rate` * (1 + percentile_multiplier) - Critical_infection_rate - Severe_infection_rate,
                                                    `Infection rate` * `Requiring hospitalisation rate` - Critical_infection_rate - Severe_infection_rate
                                                   )
                                            ),
             Moderate_infection_rate = ifelse(Level == "5th percentile",
                                              `Infection rate` * (`Symptomatic rate` - `Requiring hospitalisation rate` * (1 - percentile_multiplier) ),
                                              ifelse(Level == "95th percentile",
                                                     `Infection rate` * (`Symptomatic rate` - `Requiring hospitalisation rate` * (1 + percentile_multiplier) ),
                                                     `Infection rate` * (`Symptomatic rate` - `Requiring hospitalisation rate`)
                                                    )
                                             ),
             Critical_mortality_rate_before_Dexamethasone = `Fatality rate hospitalised (ventilated before Dexamethasone)`,
             Critical_mortality_rate_after_Dexamethasone = `Fatality rate hospitalised (ventilated after Dexamethasone)`
            )
  } else {
    temp = df %>% 
      spread(Assumption, Value) %>% 
      mutate(Mild_infection_rate = `Infection rate`*(1 - `Symptomatic rate`),
             Critical_infection_rate = `Infection rate` * `Requiring hospitalisation rate` * `Ventilation rate` * `Ventilation proportion invasive`,
             Severe_infection_rate = `Infection rate` * `Requiring hospitalisation rate` * `Ventilation rate` * (1-`Ventilation proportion invasive`),
             Serious_infection_rate = `Infection rate` * `Requiring hospitalisation rate` - Critical_infection_rate - Severe_infection_rate,
             Moderate_infection_rate = `Infection rate` * (`Symptomatic rate` - `Requiring hospitalisation rate`),
             Critical_mortality_rate_before_Dexamethasone = `Fatality rate hospitalised (ventilated before Dexamethasone)`,
             Critical_mortality_rate_after_Dexamethasone = `Fatality rate hospitalised (ventilated after Dexamethasone)`
            )
  }
  
  temp %>% 
    select(Critical_infection_rate,
           Severe_infection_rate,
           Serious_infection_rate,
           Moderate_infection_rate,
           Mild_infection_rate,
           Critical_mortality_rate_before_Dexamethasone,
           Critical_mortality_rate_after_Dexamethasone,
           Severe_mortality_rate_before_Dexamethasone = `Fatality rate hospitalised (ventilated before Dexamethasone)`,
           Severe_mortality_rate_after_Dexamethasone = `Fatality rate hospitalised (ventilated after Dexamethasone)`,
           Serious_mortality_rate_before_Dexamethasone = `Fatality rate hospitalised (not ventilated before Dexamethasone)`,
           Serious_mortality_rate_after_Dexamethasone = `Fatality rate hospitalised (not ventilated after Dexamethasone)`,
           ...) %>% 
    gather(assumption, 
           value,
           Critical_infection_rate,
           Severe_infection_rate,
           Serious_infection_rate,
           Moderate_infection_rate,
           Mild_infection_rate,
           Critical_mortality_rate_before_Dexamethasone,
           Critical_mortality_rate_after_Dexamethasone,
           Severe_mortality_rate_before_Dexamethasone,
           Severe_mortality_rate_after_Dexamethasone,
           Serious_mortality_rate_before_Dexamethasone,
           Serious_mortality_rate_after_Dexamethasone) %>% 
    separate(assumption, c("severity", "assumption"), sep="_", extra="merge") %>% 
    spread(assumption, value, fill=0)
}



# get_dates_of_stay = function(date_of_admission_from_infection_start = 9, added_days=4) { #in setup.R
#   "AssumptionControl_LoSProfiles.csv" %>% 
#     get_filepath("Data") %>% 
#     read_csv(col_types = c(
#       .default = col_double())) %>%
#     reduce(1:added_days, ~bind_rows(slice(.,1), .), .init = .) %>% 
#     mutate(date_from_infection = date_of_admission_from_infection_start:(date_of_admission_from_infection_start-1+NROW(.))) %>% 
#     gather(pathway, proportion, -date_from_infection) %>% 
#     filter(proportion != 0) %>% 
#     separate(pathway, c("pathway", "Age_group"), "_", extra = "merge") %>% 
#     separate(pathway, c("severity", "Status"), 1) %>% 
#     mutate(Age_group = Age_group %>% str_extract("\\d*") %>% as.double,
#            severity = severity %>% recode(`2` = "Serious",
#                                           `3` = "Severe",
#                                           `4` = "Critical"),
#            Status = Status %>% recode(O = "Oxygen",
#                                       `O+` = "Non-invasive ventilation",
#                                       V = "Invasive ventilation")) %>% 
#     filter(Age_group %% 10 == 0) %>% 
#     bind_rows(filter(., Age_group==80) %>% mutate(Age_group=90))
# }

get_dates_of_stay = function(date_of_admission_from_infection_start = 9, Level = "Median") { #in setup.R
  
  # AS: Modified version to remove X and Y (which created the distribution of days of hospitalisation) and added_days.
  # Also changed to read in the new Excel version of the length of stay assumptions spreadsheet,
  # with a separate sheet for Median, 5th percentile and 95th percentile.
  
  "AssumptionControl_LoSProfiles.xlsx" %>% 
    get_filepath("Data") %>% 
    # read_csv(col_types = c(
    #   .default = col_double())) %>%
    read_excel(sheet = Level) %>%
    mutate(date_from_infection = 0:(NROW(.)-1)) %>% 
    gather(pathway, proportion, -date_from_infection) %>% 
    mutate(date_from_infection = date_from_infection + date_of_admission_from_infection_start) %>% 
    group_by(date_from_infection, pathway) %>% 
    summarise(proportion = sum(proportion) ) %>% 
    ungroup %>% 
    filter(proportion != 0) %>% 
    separate(pathway, c("pathway", "Age_group"), "_", extra = "merge") %>% 
    separate(pathway, c("severity", "Status"), 1) %>% 
    mutate(Age_group = Age_group %>% str_extract("\\d*") %>% as.double,
           severity = severity %>% recode(`2` = "Serious",
                                          `3` = "Severe",
                                          `4` = "Critical"),
           Status = Status %>% recode(O = "Oxygen",
                                      `O+` = "Non-invasive ventilation",
                                      V = "Invasive ventilation")) %>% 
    filter(Age_group %% 10 == 0)
}

get_dates_of_infection = function(length_symptomatic = 8,
                                  length_of_infection = 13,
                                  date_of_admission_from_infection_start = 9) { #in setup.R
  asymptomatic = tibble(date_from_infection = 0:(length_of_infection - length_symptomatic - 1),
                        Status = "Asymptomatic")
  symptomatic = tibble(date_from_infection = (length_of_infection - length_symptomatic):(length_of_infection-1),
                       Status = "Symptomatic")
  still_in_hospital = get_dates_of_stay() %>% 
    group_by(date_from_infection, severity, Age_group) %>% 
    summarise(proportion = sum(proportion)) %>% 
    mutate(Status = "Symptomatic")
  
  bind_rows(asymptomatic, symptomatic) %>% 
    crossing(Age_group = (0:9)*10, 
             severity = c("Mild", "Moderate", "Serious", "Severe", "Critical")) %>% 
    mutate(proportion = 1) %>% 
    filter(date_from_infection < date_of_admission_from_infection_start | severity %in% c("Mild", "Moderate")) %>% 
    bind_rows(still_in_hospital)
}

get_dates_recovering = function(dates_from_infection, ..., date_of_admission_from_infection_start=9, hospital_recovery_factor=1) {
  dates_from_infection %>%
    group_by(date_from_infection, severity, ...) %>%
    summarise(proportion=sum(proportion)) %>%
    group_by(severity, Age_group, ...) %>%
    mutate(proportion_leaving = proportion %>% diff %>% abs %>% c(0,.)) %>%
    ungroup %>% 
    filter(date_from_infection>date_of_admission_from_infection_start) %>% 
    mutate(date_from_infection = date_from_infection %>% 
             map(~seq(from = ., 
                      length.out = (.- date_of_admission_from_infection_start-1) * hospital_recovery_factor))) %>% 
    unnest %>%
    group_by(severity, date_from_infection, ...) %>%
    summarise(proportion = sum(proportion_leaving)) %>% 
    ungroup %>% 
    mutate(Status = "Recovering")
}

get_dates_removed = function(dates_from_infection, Status) {
  dates_from_infection %>% 
    group_by(In_care, date_from_infection, Age_group, severity) %>% 
    summarise(proportion = 1-sum(proportion)) %>% 
    mutate(Status = Status)
}

get_equipment_data = function(included_equipment = NULL, max_week=100) { #in setup.R
  
  if(included_equipment %>% is_null) 
    included_equipment = c()
  
  withCallingHandlers({
    temp = "Equipment.xlsx" %>% 
      get_filepath("Data") %>% 
      read_excel(sheet = "COVID1 - Equipment Delivery",
                 skip=69)},
    warning=function(w){
      #what to do with warning
      if(conditionMessage(w) %>% str_detect("Expecting numeric in"))
        invokeRestart("muffleWarning")
    })
  temp=temp %>% 
    rename(Equipment = `Effective capacity`) %>% 
    gather(Week, Capacity, -Equipment) %>%
    mutate(Week = Week %>% str_remove("Week ") %>% as.integer) %>%  
    spread(Equipment, Capacity) %>% 
    gather(Equipment, Capacity, -Week, -Date) %>% 
    mutate(Date = Date + ymd("1899-12-30")) %>% 
    bind_rows(tibble(Week = 1:6), .) %>% 
    complete(Equipment, Week, fill=list(Capacity=NA)) %>% 
    mutate(Capacity = ifelse(Equipment=="Beds" & Week<7, 3000, Capacity)) %>% 
    filter(Equipment %in% included_equipment)
  
  if(max_week > temp$Week %>% max & NROW(temp) > 0) {
    addition = temp %>% 
      filter(Week == max(Week)) %>% 
      select(-Week)
    temp = tibble(Week = max(temp$Week+1):max_week) %>% 
      mutate(data = list(addition)) %>% 
      unnest %>% 
      bind_rows(temp) %>% 
      arrange(Equipment, Week)
  }
  temp               
}

get_actuals = function(concurrent_or_admissions = "concurrent", frequency = "7-day Moving Average") { #in setup.R
  warning("Deprecated. Use read_actual_concurrent or read_actual_admissions")
  
  if(concurrent_or_admissions=="concurrent"){
    withCallingHandlers({
      temp = "Dashboard - Time series.xlsx" %>% 
        get_filepath("Data") %>% 
        read_excel },
      warning=function(w){
        #what to do with warning
        if(conditionMessage(w) %>% str_detect("Expecting numeric in"))
          invokeRestart("muffleWarning")
      })
    
    temp %>% 
      gather(Date, Value, -Slide, -Data) %>% 
      filter(Date > 43890,
             Data %>% str_detect("confirmed and suspected C19 Inpatients In Hospital at Midnight") |
               Data %in% c("Total Number of confirmed and suspected C19 Inpatients In Hospital at Midnight",
                           "Total confirmed C19 patients in hospital", 
                           "ICU beds occupied by confirmed or suspected COVID-19 patients")) %>% 
      transmute(Date = ymd("1899-12-30") + as.integer(Date),
                Location = ifelse(Data %>% str_detect("ICU"), "ICU", ifelse(Data %>% str_detect("suspected"),"Hospital - confirmed and suspected", "Hospital - confirmed")),
                Number = as.integer(Value))
  }
  
  else if(concurrent_or_admissions=="admissions"){
    withCallingHandlers({
      hospital_temp = "daily_covid_admissions_22092020.csv" %>% 
        get_filepath("Data") %>% 
        read_csv },
      warning=function(w){
        #what to do with warning
        if(conditionMessage(w) %>% str_detect("Expecting numeric in"))
          invokeRestart("muffleWarning")
      })
    
    if(frequency == "Daily") type = "NumberAdmitted"
    else type = "SevenDayAverage"
    
    hospital = hospital_temp %>%
      select(Date, 
             Number = type) %>%
      transmute(Location = "Hospital",
                Date = Date %>% ymd,
                Number)
    
    withCallingHandlers({
      icu_temp = "daily_icu_admissions_22092020.csv" %>% 
        get_filepath("Data") %>% 
        read_csv },
      warning=function(w){
        #what to do with warning
        if(conditionMessage(w) %>% str_detect("Expecting numeric in"))
          invokeRestart("muffleWarning")
      })
    
    if(frequency == "Daily") type = "NumberAdmitted"
    else type = "SevenDayAverage"
    
    icu = icu_temp %>%
      select(Date, 
             Number = type) %>%
      transmute(Location = "ICU",
                Date = Date %>% ymd,
                Number)
    
    bind_rows(hospital, icu)
  }
}

#pulls in the data on positive tests by LA
# get_LA_data = function(result) { # in setup.R
#   
#   raw_data = "LocalAuth Time Series.xlsx" %>% 
#     get_filepath("Data") %>% 
#     read_excel(sheet = result %>% str_c(" Local Auth"),
#                col_types = c("text", rep("numeric", 34)))
#   
#   if("Specimen Date" %in% names(raw_data)) 
#     raw_data = raw_data %>% rename(SpecimenDate = `Specimen Date`)
#   
#   data = raw_data %>% 
#     gather(Local_authority, Tests, -`SpecimenDate`) %>% 
#     filter(! Local_authority %in% c("..2", "Total")) %>% 
#     transmute(Date = `SpecimenDate` %>% ymd,
#               Result = result,
#               Local_authority = Local_authority %>% recode(`..2` = "Unknown"),
#               Tests) %>% 
#     replace_na(list(Tests = 0)) %>% 
#     filter(! Date %>% is.na)
#   
#   date_of_data = max(data$Date)
#   
#   data = data %>% 
#     filter(Date < date_of_data)
#   
#   date_of_data = max(data$Date)
#   
#   data %>% 
#     filter(Date < date_of_data)
# }

get_LA_data = function(result) { # in setup.R
  
  raw_data = "LocalAuth Time Series.xlsx" %>% 
    get_filepath("Data") %>% 
    read_excel(sheet = result %>% str_c(" Local Auth"),
               col_types = c("text", rep("numeric", 34)))
  
  if("Specimen Date" %in% names(raw_data)) 
    raw_data = raw_data %>% rename(SpecimenDate = `Specimen Date`)
  
  data = raw_data %>% 
    gather(Local_authority, Tests, -`SpecimenDate`) %>% 
    filter(! Local_authority %in% c("..2", "Total")) %>% 
    transmute(Date = `SpecimenDate` %>% from_excel_date,
              Result = result,
              Local_authority = Local_authority %>% recode(`..2` = "Unknown"),
              Tests) %>% 
    replace_na(list(Tests = 0)) %>% 
    filter(! Date %>% is.na)
  
  date_of_data = max(data$Date)
  
  data = data %>% 
    filter(Date < date_of_data)
  
  date_of_data = max(data$Date)
  
  data %>% 
    filter(Date < date_of_data)
}

get_s_gene_proportion = function(result) { # in setup.R
  
  target_failure = "s_gene_time_series.xlsx" %>% 
    get_filepath("Data") %>% 
    read_excel(sheet = "S Gene target failure (LA)") %>% 
    transmute(Date = date_ecoss_specimen %>% from_excel_date(),
              TF = `Grand Total`)
  
  positive = "s_gene_time_series.xlsx" %>% 
    get_filepath("Data") %>% 
    read_excel(sheet = "S Gene Positive (LA)") %>% 
    transmute(Date = date_ecoss_specimen %>% from_excel_date(),
              Positive = `Grand Total`)
  
  if(result %in% c("dropouts", "target failure")) {
    full_join(target_failure, positive, y, by = "Date") %>% 
      mutate(Proportion = (TF / (TF + Positive)) %>% replace_na(0)) %>% 
      select(Date, Proportion)
  }
  else if(result == "positive") {
    full_join(target_failure, positive, y, by = "Date") %>% 
      mutate(Proportion = (Positive / (TF + Positive)) %>% replace_na(0)) %>% 
      select(Date, Proportion)
  }
  else stop("result must be 'positive' or 'target failure'")
}

#converts positive cases to infections
get_historic_infections = function(method = "cases",
                                   lag = 1,
                                   detection_rate = 0.5, 
                                   baseline = 20000,
                                   s_gene_status = "all") { # in setup.R
  if(method == "positivity") {
    infections = get_LA_data("Negative") %>% 
      bind_rows(get_LA_data("Positive")) %>% 
      group_by(Date, Result) %>% 
      summarise(Tests = sum(Tests)) %>% 
      ungroup %>% 
      spread(Result, Tests) %>% 
      mutate(Prop_pos = Positive/(Positive+Negative)) %>% 
      filter(! Positive %>% is.na) %>% 
      select(-Negative) %>% 
      transmute(Date = Date - lag,
                Level = "Actual",
                Cases = Prop_pos*baseline)
  }
  else if(method == "cases") {
    infections = get_LA_data("Positive") %>% 
      group_by(Date) %>% 
      summarise(Cases = sum(Tests)) %>% 
      filter(Date >= ymd("2020-08-01")) %>% 
      mutate(Date = Date - lag,
             Level = "Actual",
             Cases = Cases %>% 
               adjust_for_weekends %>% 
               `/`(detection_rate))
  }
  else stop("method must be either 'cases' or 'positivity'")
  
  if(s_gene_status == "all") infections
  else if(s_gene_status %in% c("target failure", "positive")) {
    get_s_gene_proportion(s_gene_status) %>% 
      mutate(Date = Date - lag) %>% 
      right_join(infections) %>% 
      mutate(Proportion = Proportion %>% replace_na(0),
             Cases = Cases*Proportion) %>% 
      select(-Proportion)
  }
  else stop("s_gene_status must be either 'all', 'target failure' or 'positive'")
}

#attempts to adjust for the weekend effect of fewer tests
adjust_for_weekends = function(series) { # in setup.R
  log_series = log(series)
  
  decomposition = log_series %>% ts(frequency = 7) %>% stl(s.window = "periodic")
  
  (decomposition$time.series[,"trend"] + decomposition$time.series[,"remainder"]) %>% 
    as.vector %>% 
    exp
}

#reads in assumptions on how long immunity lasts
get_immunity_time = function(type) { #in setup.R
  assumption = type %>% 
    str_to_lower %>% 
    str_c(" immunity time")
  
  if(! assumption %in% c("natural immunity time", "vaccine immunity time")) 
    stop("type must equal 'natural' or 'vaccine'")
  
  "Assumptions - Other.csv" %>% 
    get_filepath("Data") %>% 
    read_csv(col_types = cols("c", "d")) %>%
    filter(Assumption %>% str_to_lower %>% str_detect(assumption)) %>% 
    pull(Value)
}

#' @title Project infections and Rt for given scenario
#'
#' @description \code{simulate_epi_curve} returns a tibble on a number of epidemiological
#' simulations for the path of the epidemic.
#'
#' @param scenario Name of the scenario
#'
#' @return A tibble including infections, Rt, and other values, by date and simulation.
#'
#' @examples
#' # Run the "Better" scenario.
#' simulate_epi_curve("Better")
#'
#' @export

simulate_epi_curve = function(scenario, start_date_of_forecast = ymd("20200101") ) { #in setup.R
  
  # AS: Modified version to filter the forecasts so they don't estimate actuals.
  
  sim_output = "Forecast scenarios.xlsx" %>%
    get_filepath("Data") %>% 
    read_excel(sheet = scenario) %>% 
    add_vaccine %>% 
    run_simulation_from_scenario(100)
  
  sim_output %>%
    # AS: Filter to only the forecast period.
    filter(Date >= start_date_of_forecast) %>% 
    saveRDS(scenario %>% str_c("SCHEMa outputs/", ., " sim_output.rds"))
  
  sim_output
}

transform_epi_curve = function(sim_output = NULL, scenario_info = NULL, scenario = NULL) { #in setup.R
  id_root = min(scenario_info$ID)
  
  levels = scenario_info %>% 
    pull(Level) %>% 
    str_replace("Median", "50") %>% 
    str_remove("th percentile") %>% 
    as.double %>% 
    `/`(100)
  
  phases = "Forecast scenarios.xlsx" %>%
    get_filepath("Data") %>% 
    read_excel(sheet = scenario)
  
  efficacy_adjustments = phases %>% 
    transmute(Date = ymd(start_date), efficacy_adjustment)
  
  if(sim_output %>% is_null) {
    sim_output = scenario %>% 
      str_c(" sim_output.rds") %>% 
      get_filepath("SCHEMa outputs") %>% 
      readRDS
  }
  
  sim_output %>%
    nest(-Date) %>% 
    mutate(ID = list(0:(length(levels)-1)),
           Level = list(levels),
           Cases = map2(data,
                        Level,
                        ~quantile(.x$Cases, probs = .y), names=FALSE)) %>% 
    select(-data) %>% 
    unnest %>% 
    select(-Level) %>% 
    get_curve_scenario(efficacy_adjustments) %>% 
    mutate(ID = ID + id_root)
}



get_curve_scenario_actuals = function(df, efficacy_adjustments, input) { #in setup.R
  
  # AS: As apply_vaccine isn't defined in the global environment in include_totals_from_actuals (and we need a different version for hospitalisations anyway),
  # I've added the input argument to determine which formula to use:
  # "asymp" = Use only the historical proportions of infections by age (i.e. don't apply the effect of vaccinations).
  # "other" = Multiply the historical proportions of infections by age by the proportion unsafe (i.e. apply the effect of vaccinations).
  # "hosp"  = Multiply the historical proportions of infections by age by the hospitalisation rate and rescale so they sum to 1 (i.e. break down the hospitalisations by age).
  
  population = get_population(Age_group)
  
  past_vaccine_data = get_vaccine_history()
  future_vaccine_data = get_vaccine_roll_out(max(past_vaccine_data$Date) + 1)
  
  all_vaccine_data = estimate_roll_out_by_age(past_vaccine_data, future_vaccine_data)
  
  susceptibility = get_immunity_data(all_vaccine_data, efficacy_adjustments) %>%
    # Keep the immune-compromised flag.
    select(Date, Age_group, Immune_compromised, Immune, Safe) %>% 
    complete(Date = unique(df$Date), Age_group = 10*(0:8), Immune_compromised = c("No", "Yes") ) %>% 
    filter(!(Date < "2021-07-01" & Immune_compromised == "Yes") ) %>% 
    group_by(Age_group, Immune_compromised) %>% 
    fill(Immune, Safe) %>% 
    replace_na(list(Immune = 0, Safe = 0)) %>%
    ungroup %>% 
    left_join(population) %>% 
    # Separate the immune-compromised population.
    left_join("Assumptions - Immune-compromised - Sources.xlsx" %>%
                get_filepath("Data") %>%
                read_excel(sheet = "Immune-Compromised Population", range = "A22:B31")
             )
  
  historic_proportions = get_proportions_from_historic(df$Date %>% max)
  
  if (input == "asymp") {
    proportions = susceptibility %>%
      left_join(historic_proportions) %>% 
      mutate(Proportion_of_cases = Proportion) %>% 
      right_join(df) %>% 
      # Keep the immune-compromised flag.
      transmute(Date,
                ID,
                Age_group,
                Immune_compromised,
                proportion_in_period = Cases * Proportion_of_cases / Population / 0.8)
    
  } else if (input == "other") {
    proportions = susceptibility %>%
      # For immune-compromised people, calculate using the immune-compromised population.
      mutate(Susceptible = ifelse(Immune_compromised == "Yes",
                                  Immune_compromised_population - Immune,
                                  (Population - Immune_compromised_population) - Immune
                                 ),
             Proportion_unsafe = ifelse(Immune_compromised == "Yes",
                                        (Immune_compromised_population - Safe) / Susceptible,
                                        (Population - Immune_compromised_population - Safe) / Susceptible
                                       )
            ) %>% 
      left_join(historic_proportions) %>% 
      mutate(Proportion_of_cases = Proportion) %>% 
      right_join(df) %>% 
      # Keep the immune-compromised flag.  Don't use the immune-compromised population in the denominator as this gives
      # ridiculously high proportions because Cases in the numerator are not broken down by immune-compromised status. 
      transmute(Date,
                ID,
                Age_group,
                Immune_compromised,
                proportion_in_period = Cases * Proportion_of_cases * Proportion_unsafe / Population / 0.8)
    
  } else if (input == "hosp") {
    # AS: To work out the age breakdown of hospitalisations, we need to apply the hospitalisation rate to the proportion
    # of infections for each age group, and rescale the resulting proportions so that they sum to 1.
    hospitalisation_rates = "Assumptions - Hospitalisation rates.csv" %>% 
      get_filepath("Data") %>% 
      read_csv %>% 
      filter(In_care == "No") %>% 
      select(Age_group = `Lower end age`, Immune_compromised, Hospitalisation_rate = Value)
    
    proportions = susceptibility %>%
      left_join(historic_proportions) %>% 
      left_join(hospitalisation_rates) %>% 
      mutate(Proportion_of_cases = Proportion * Hospitalisation_rate) %>% 
      group_by(Date, Immune_compromised) %>% 
      mutate(Proportion_of_cases = Proportion_of_cases / sum(Proportion_of_cases) ) %>% 
      ungroup %>% 
      right_join(df) %>% 
      # Keep the immune-compromised flag.  Don't use the immune-compromised population in the denominator as this gives
      # ridiculously high proportions because Cases in the numerator are not broken down by immune-compromised status. 
      transmute(Date,
                ID,
                Age_group,
                severity,
                Immune_compromised,
                proportion_in_period = Cases * Proportion_of_cases / Population / 0.8)
  }
  
  proportions
}