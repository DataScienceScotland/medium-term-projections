
#calculates the proportions of each demographic group that will be affected, and how severely
run_model = function(scenario_df, ...) { #in model_functions.R
  scenario_df %>% 
    mutate(data = ID %>%
             map(prepare_assumptions, ...)) %>%
    unnest %>% 
    prepare_model(ID, `Scenario_name`, Level, ...)
}

#sets the total wave proportions in the model to the proportions by time in the timeline
get_proportions = function(timeline, model) { #in model_functions.R
  timeline %>%
    left_join(model) %>%
    mutate(mult_factor = infection_rate * ifelse(Status == "Dead",
                                                 ifelse(Date<as.Date("2020-07-01"),
                                                        mortality_rate_before_Dexamethasone,
                                                        mortality_rate_after_Dexamethasone),
                                                 ifelse(Status %>% 
                                                          str_detect("Recover"),
                                                        ifelse(Date<as.Date("2020-07-01"),
                                                               1-mortality_rate_before_Dexamethasone,
                                                               1-mortality_rate_after_Dexamethasone),
                                                        1))) %>%
    mutate_at(c("Proportion_starting", "Proportion_at"), ~.* mult_factor) %>%
    select(-infection_rate, -mortality_rate_before_Dexamethasone, -mortality_rate_after_Dexamethasone, -mult_factor)
}

#calculates the consequences of a given scenario in total infections and fatalities
get_totals = function(proportions, population, date_shift=0) { #in model_functions.R
  proportions %>%
    mutate(Date = Date + date_shift) %>%
    left_join(population) %>% 
    mutate(Number_starting = Proportion_starting * Population,
           Number_at = Proportion_at * Population) %>%
    select(-Proportion_starting, -Proportion_at)
}

#procuces a timeline based on a given infection curve
set_to_curve = function(totals, curve, time_to_death=9) { #in model_functions.R
  
  if(curve %>% NCOL == 2) {
    curve = tibble(temp = 1,
                   data = list(curve)) %>% 
      unnest
  }
  
  message("Setting to curve")
  totals %>% 
    mutate(temp = 1) %>% 
    left_join(curve) %>% 
    select(-temp) %>% 
    unnest %>% 
    mutate(infection_rate = infection_rate*proportion_in_period) %>% 
    left_join(get_timeline())
}

#wrapper around prepare_scenario and get_totals for convenience
get_results = function(assumptions,...,.population=NULL) { #in model_functions.R
  
  if(.population %>% is_null) {
    .population = get_population(...) %>%
      left_join(care_data) %>% 
      mutate(Population = ifelse(Age_group == 81, 
                                 Population - In_care, 
                                 Population))
  }
  
  assumptions  %>% 
    nest(Assumption, Value, .key = assumptions) %>% 
    mutate(scenario = assumptions %>% map(prepare_scenario)) %>% 
    left_join(.population) %>% 
    mutate(totals = pmap(., get_totals)) %>% 
    unnest(totals) %>% 
    filter(! Population %>% is.na) 
}

#randomises parameters within given ranges and runs the model multiple times
run_simulation = function(assumptions, demo, n_runs, periods) { #in model_functions.R
  
  assumptions %>%
    filter(Demo == demo) %>%
    mutate(Value = Value %>% as.double) %>%
    filter(! Value %>% is.na) %>%
    spread(Limit, Value) %>%
    rename(min = Lower, mode = Central, max = Upper) %>%
    mutate(runs = pmap(., function(min, mode, max, ...) tibble(run=1:n_runs, Value=rtri(n_runs, min, mode, max)))) %>%
    unnest %>%
    nest(-run, .key = "assumptions") %>%
    mutate(results = assumptions %>%
             map(get_results,
                 demo %>% get_age_range %>% get_population)) %>%
    select(-assumptions) %>%
    unnest %>% 
    set_to_curve
}



get_timeline = function(length_symptomatic = 8,
                        length_of_infection = 13, 
                        date_of_admission_from_infection_start = 9,
                        treatment_recovery_factor = 1,
                        available_treatments = tibble(In_care = c("No", "Yes"),
                                                      Status = list(c("Oxygen", 
                                                                      "Non-invasive ventilation", 
                                                                      "Invasive ventilation"),
                                                                    c("Oxygen"))),
                        Levels = "Median"
                       ) { #in model_functions.R
  
  # AS: Modified version to change the hard coded dates from infection.
  # Also added Level to allow the calculation of the 5th and 95th percentile for actuals occupancy.
  
  #Everyone starts symptomatic phase on day 0
  asymptomatic_start = crossing(severity=c("Mild","Moderate","Serious","Severe","Critical"),
                                Age_group = (0:9)*10,
                                In_care = c("Yes", "No"),
                                Level = Levels) %>% 
    mutate(date_from_infection = 0, 
           proportion = 1)
  
  #People with moderate or worse infections develop symptoms on a set day from infection date
  symptomatic_start = crossing(severity=c("Moderate","Serious","Severe","Critical"),
                               Age_group = (0:9)*10,
                               In_care = c("Yes", "No"),
                               Level = Levels) %>% 
    mutate(date_from_infection = length_of_infection-length_symptomatic,
           proportion = 1)
  
  #People with serious or worse infections enter treatment on a set day from infection date
  treatment_start = crossing(severity=c("Serious","Severe","Critical"),
                             Age_group = (0:9)*10,
                             In_care = c("Yes", "No"),
                             Level = Levels) %>% 
    mutate(date_from_infection = date_of_admission_from_infection_start,
           proportion = 1,
           Status = severity %>% 
             recode(`Serious` = "Oxygen",
                    `Severe` = "Non-invasive ventilation",
                    `Critical` = "Invasive ventilation"))
  
  treatment_at = available_treatments %>% 
    crossing(Level = Levels) %>% 
    mutate(data = Status %>% 
             map2(Level, function(x, y) {
               filter(get_dates_of_stay(date_of_admission_from_infection_start, Level = y), 
                      Status %in% x | date_from_infection == date_of_admission_from_infection_start
                     ) %>% 
                 bind_rows(filter(get_dates_of_stay(date_of_admission_from_infection_start, Level = y), 
                                  ! Status %in% x & date_from_infection == date_of_admission_from_infection_start+1
                                 ) %>% 
                             mutate(proportion=0)
                          )
             })) %>% 
    select(-Status) %>% 
    unnest
  
  #People with mild and moderate infections start recovering on a set day from infection date
  recovering_start_no_treatment = crossing(severity=c("Mild","Moderate"),
                                           Age_group = (0:9)*10,
                                           In_care = c("Yes", "No"),
                                           Level = Levels) %>% 
    mutate(date_from_infection = length_of_infection,         
           proportion = 1)
  
  #People with serious or worse infections start recovering a number of days from infection start based on pathways
  recovering_start_treatment = treatment_at %>% 
    group_by(date_from_infection, severity, Age_group, In_care, Level) %>% 
    summarise(proportion = sum(proportion)) %>% 
    group_by(severity, Age_group, In_care, Level) %>% 
    mutate(proportion = -(proportion %>% diff %>% c(0,.)))
  
  recovering_start = bind_rows(recovering_start_no_treatment, recovering_start_treatment)
  
  #People with mild and moderate infections fully recover the same day they start recovering
  recovered_start_no_treatment = recovering_start_no_treatment
  
  #People with serious or worse infections fully recover after the number of days they were in treatment multiplied by a factor
  recovered_start_treatment = recovering_start_treatment %>% 
    mutate(date_from_infection = date_of_admission_from_infection_start + 
             ((date_from_infection-date_of_admission_from_infection_start)*(1+treatment_recovery_factor)) %>% as.integer)
  
  recovered_start = bind_rows(recovered_start_no_treatment, recovered_start_treatment)
  
  #Anyone that dies instead of recovering does so on the day they would have started recovering
  dead_start = crossing(severity=c("Mild","Moderate","Serious","Severe","Critical"),
                        Age_group = (0:9)*10,
                        In_care = c("Yes", "No"),
                        Level = Levels) %>% 
    mutate(date_from_infection = 16,
           proportion = 1)
  
  start = bind_rows(Asymptomatic = asymptomatic_start,
                    Symptomatic = symptomatic_start,
                    Recovering = recovering_start,
                    Recovered = recovered_start,
                    Dead = dead_start,
                    .id = "Status")
  
  #Mild infections are asymptomatic until recovery starts
  asymptomatic_end_no_symptoms = recovering_start %>% filter(severity=="Mild")
  
  #Moderate and worse infections are asymptomatic until symptoms start
  asymptomatic_end_symptoms = symptomatic_start %>% filter(severity!="Mild")
  
  asymptomatic_end = bind_rows(asymptomatic_end_no_symptoms, asymptomatic_end_symptoms)
  
  #Moderate and worse infections are symptomatic until recovery starts
  symptomatic_end = recovering_start %>% filter(severity!="Mild")
  
  #Recovery ends when the patient is fully recovered
  recovering_end = recovered_start
  
  end = bind_rows(Asymptomatic = asymptomatic_end,
                  Symptomatic = symptomatic_end,
                  Recovering = recovering_end,
                  .id = "Status")
  
  #Number of cases that are at a status is the number that have started minus the number that have ended
  at = bind_rows(Start = start, End = end, .id = "Event") %>%
    complete(date_from_infection = min(date_from_infection):(1+max(date_from_infection)),
             Status = c("Asymptomatic","Symptomatic","Recovering","Recovered","Dead"),
             severity = c("Mild","Moderate","Serious","Severe","Critical"),
             Age_group = (0:9)*10,
             In_care = c("Yes", "No"),
             Event = c("Start", "End"),
             Level = Levels,
             fill = list(proportion = 0)) %>% 
    group_by(Status, severity, Age_group, In_care, Event, Level) %>% 
    mutate(proportion = cumsum(proportion)) %>% 
    ungroup %>% 
    spread(Event, proportion) %>% 
    transmute(date_from_infection, Status, severity, Age_group, In_care, proportion = Start-End, Level)
  
  bind_rows(Proportion_starting = bind_rows(start, treatment_start),
            Proportion_at = bind_rows(at, treatment_at),
            .id = "Stat") %>% 
    filter(proportion>0)
}



run_single_timeline = function(curve, timeline, ...) {
  curve %>% 
    left_join(timeline) %>% 
    mutate(Date = Date + date_from_infection) %>% 
    group_by(severity, Status, Stat, Date, ...) %>% 
    summarise(proportion = sum(proportion_in_period*proportion))
}



run_timeline = function(curve,
                        ...,
                        length_symptomatic = 8,             
                        length_of_infection=13,
                        date_of_admission_from_infection_start=9, 
                        treatment_recovery_factor = 1,
                        cutoff=0,
                        timeline = NULL) { #in model_functions.R
  
  if(timeline %>% is_null) {
    if("~In_care" %in% enquos(...)) {
      timeline = get_timeline(length_of_infection=length_of_infection,
                              date_of_admission_from_infection_start=date_of_admission_from_infection_start,
                              treatment_recovery_factor=treatment_recovery_factor)
    }
    else {
      timeline = get_timeline(length_of_infection=length_of_infection,
                              date_of_admission_from_infection_start=date_of_admission_from_infection_start,
                              treatment_recovery_factor=treatment_recovery_factor) %>% 
        filter(In_care == "No") %>% 
        select(-In_care)
    }
  }
  
  curve %>% 
    nest(-ID) %>% 
    mutate(data = data %>% map(run_single_timeline, timeline, ...)) %>% 
    unnest %>% 
    spread(Stat, proportion, fill=0) %>% 
    mutate(Status = Status %>% recode(Symptomatic = "Infected"))
}



include_totals_from_actuals = function(start_date_of_forecast, start_date_of_hospitalisations, start_date_of_ICU){ #in model_functions.R
  
  #Take the historical infections and hospitalisations and run them through the 
  #timeline to get estimates of hospitalisations (from infections), deaths
  #and hospital occupancy
  
  date_of_admission_from_infection_start = 9
  
  # AS: This is the date that was used for the start_date_of... arguments when creating the history_actuals.rds file.
  # To speed up the running of the actuals code, we cut off data earlier than this date before running the get_totals_from_actuals functions.
  # We then attach the data from history_actuals.rds at the end.  When history_actuals.rds is updated this date will need to be updated too.
  history_date = ymd("2021-11-01")
  
  
  
  # AS: Get the infection rate.  As every scenario and level will be the same while they're based on
  # the actuals, we can save a lot of time and memory by only calculating one and copying it later.
  model = run_model_actuals(tibble(ID = 1:3, Scenario_name = "Central", Level = c("5th percentile", "Median", "95th percentile") ),
                            Age_group, SIMD, Immune_compromised
                           ) %>% 
    # AS: Get the mortality rate.  These are applied at the end.
    left_join("Assumptions - Hospitalisation rates.csv" %>% 
                get_filepath("Data") %>% 
                read_csv %>% 
                filter(In_care == "No") %>% 
                select(Age_group = `Lower end age`, Immune_compromised, Hospitalisation_rate = Value)
             ) %>% 
    mutate(Death_rate = mortality_rate_after_Dexamethasone)
  
  # AS: Get the proportion of those infected on day 0 who are in each state on day X.
  timeline = get_timeline(Levels = c("5th percentile", "Median", "95th percentile") ) %>%
    filter(In_care == "No") %>% 
    select(-In_care)
  
  # AS: Get the proportion of those hospitalised on day 0 who are in each state on day X.
  # We assume hospitalisation occurs 12 days after infection so the dates here are 12 days 
  # behind those in the infections timeline.
  hospitalisations_timeline = get_timeline(Levels = c("5th percentile", "Median", "95th percentile") ) %>%
    filter(In_care == "No") %>% 
    select(-In_care) %>%
    #use date from hospitalisation as date_from_infection to work with future functions
    mutate(date_from_infection = date_from_infection - date_of_admission_from_infection_start) %>%
    filter(severity %in% c("Serious",
                           "Severe",
                           "Critical"))
  
  
  
  # AS: Break down the historical infections without vaccination by age, SIMD, immune-compromised status and severity.
  message("Breaking down infections curve for asymptomatic")
  infections_curve_asymp = get_historic_infections() %>%
    select(-Level) %>%
    mutate(ID = 0) %>%
    #breakdown by age and SIMD
    # AS: Use the new get_curve_scenario_actuals function.
    get_curve_scenario_actuals(input = "asymp", efficacy_adjustments = tibble(Date = ymd("2000-01-01"), efficacy_adjustment = as.double(NA))) %>%
    select(-ID) %>%
    left_join(get_population(Age_group, SIMD, Immune_compromised)) %>%
    mutate(proportion_in_period=proportion_in_period*Population*0.8) %>%
    #breakdown by severity based on assumed infection rates
    left_join(model) %>%
    group_by(Date, ID, Scenario_name, Level, Age_group, SIMD, Immune_compromised) %>%
    mutate(proportion_in_period = proportion_in_period*(infection_rate/sum(infection_rate))) %>%
    ungroup %>% 
    select("Date", 
           "proportion_in_period",
           "Age_group",
           "ID",
           "Scenario_name",
           "Level",
           "SIMD",
           "severity",
           "Immune_compromised",
           "Population"
          ) %>%
    # AS: Subset dates to after the last date in history_actuals.rds and before the actuals run out.
    filter(Date >= history_date,
           Date < start_date_of_forecast)
  
  
  
  # AS: Break down the historical infections with vaccination by age, SIMD, immune-compromised status and severity.
  # These are used to calculate admissions (once we run out of actual admissions), occupancy and deaths.
  message("Breaking down infections curve for other")
  infections_curve_other = get_historic_infections() %>%
    select(-Level) %>%
    mutate(ID = 0) %>%
    #breakdown by age and SIMD
    # AS: Use the new get_curve_scenario_actuals function.
    get_curve_scenario_actuals(input = "other", efficacy_adjustments = tibble(Date = ymd("2000-01-01"), efficacy_adjustment = as.double(NA))) %>%
    select(-ID) %>%
    left_join(get_population(Age_group, SIMD, Immune_compromised)) %>%
    mutate(proportion_in_period=proportion_in_period*Population*0.8) %>%
    #breakdown by severity based on assumed infection rates
    left_join(model) %>%
    group_by(Date, ID, Scenario_name, Level, Age_group, SIMD, Immune_compromised) %>%
    mutate(proportion_in_period = proportion_in_period*(infection_rate/sum(infection_rate))) %>%
    ungroup %>%
    select("Date", 
           "proportion_in_period",
           "Age_group",
           "ID",
           "Scenario_name",
           "Level",
           "SIMD",
           "severity",
           "Immune_compromised",
           "Population",
           "Death_rate"
          ) %>%
    # AS: Subset dates to after the last date in history_actuals.rds and before the actuals run out.
    filter(Date >= history_date,
           Date < start_date_of_forecast)
  
  
  
  # AS: Break down the historical hospitalisations by age, SIMD, immune-compromised status and severity.
  message("Breaking down hospitalisations curve")
  
  ICU = read_sicsag_ICU(Age_group) %>%
    filter(ValueType=="Incidence") %>%
    mutate(ValueType = "ICU admissions",
           severity="Critical") %>%
    transmute(Date,
              Age_group,
              Number,
              severity) %>%
    left_join(get_population(Age_group)) %>%
    #breakdown by SIMD by assuming admissions are split proportionally between SIMD 
    #groups as we don't have data on this
    mutate(proportion_in_period = Number/Population) %>%
    select(-Population, -Number) %>%
    # AS: Added immune-compromised as we don't have SICSAG data for this either.
    left_join(get_population(Age_group, SIMD, Immune_compromised) ) %>%
    mutate(proportion_in_period = proportion_in_period*Population)
  
  # AS: We don't add the immune-compromised until 2021-07-01 so sum the numbers before this date.
  ICU = ICU %>% 
    filter(!Date >= "2021-07-01") %>% 
    group_by(Date, Age_group, severity, SIMD) %>% 
    summarise(proportion_in_period = sum(proportion_in_period) ) %>% 
    ungroup %>% 
    mutate(Immune_compromised = "No") %>% 
    left_join(get_population(Age_group, SIMD, Immune_compromised) ) %>% 
    bind_rows(ICU %>% filter(Date >= "2021-07-01") ) %>% 
    filter(Date < start_date_of_ICU)
  
  
  # AS: read_actual_admissions needs to read Sicsag data as the published ICU admissions end in June.
  hospitalisations_curve = read_actual_admissions("Daily", ICU_source = "sicsag") %>%
    filter(!(Location == "Hospital" & Date >= start_date_of_hospitalisations) ) %>% 
    spread(Location, Number) %>%
    mutate(Critical = ICU,
           Severe = ICU,
           Serious = Hospital - (2*ICU)) %>%
    select(-Hospital, -ICU) %>%
    #Name hospitalisations as "Cases" so that the get_curve_scenario function works
    mutate(ID = 0) %>%
    gather(severity, Cases, Critical,Severe,Serious) %>%
    # AS: Use the new get_curve_scenario_actuals function.
    get_curve_scenario_actuals(input = "hosp", efficacy_adjustments = tibble(Date = ymd("2000-01-01"), efficacy_adjustment = as.double(NA))) %>%
    select(-ID) %>%
    #Multiply by population to get breakdown by SIMD
    left_join(get_population(Age_group, SIMD, Immune_compromised)) %>%
    mutate(proportion_in_period = proportion_in_period*Population*0.8) %>%
    #Replace ICU admissions where we have admissions broken down by age
    filter(!(severity=="Critical" & Date>=ymd("20210401"))) %>%
    bind_rows(ICU) %>%
    #Join to model to get ID numbers
    left_join(model) %>%
    select("Date", 
           "proportion_in_period",
           "Age_group",
           "ID",
           "Scenario_name",
           "Level",
           "SIMD",
           "severity",
           "Immune_compromised",
           "Population",
           "Death_rate"
          ) %>%
    # AS: Subset dates to after the last date in history_actuals.rds and before the actuals run out.
    filter(Date >= history_date,
           !is.na(proportion_in_period),
           Date < start_date_of_forecast + date_of_admission_from_infection_start)
  
  
  
  # AS: Multiply the broken down infections without vaccination by the timeline proportions
  # to get the number of people in each state on day X.
  message("Getting totals from asymptomatic infections")
  totals_from_infections_asymp = infections_curve_asymp %>%
    get_totals_from_actuals(timeline,
                            fill=0,
                            Age_group, SIMD, Level, Immune_compromised, Population
                           ) %>%
    filter(Status == "Asymptomatic") %>%
    # Uncomment this if you need to see where a row in totals_from_actuals comes from.
    mutate(source = "asymp")
  
  
  # AS: Multiply the broken down infections with vaccination by the timeline proportions
  # to get the number of people in each state on day X.
  message("Getting totals from other infections")
  # AS: Use the new get_totals_from_actuals_other function.
  # This means we only calculate admissions etc. using infections once the actual admissions run out.
  totals_from_infections_other = infections_curve_other %>% 
    get_totals_from_actuals_other(timeline,
                                  fill=0,
                                  start_date_of_hospitalisations,
                                  start_date_of_ICU,
                                  date_of_admission_from_infection_start,
                                  Age_group, SIMD, Level, Immune_compromised, Population, Death_rate
                                 ) %>% 
    filter(Status != "Asymptomatic") %>% 
    # Uncomment this if you need to see where a row in totals_from_actuals comes from.
    mutate(source = "other")
  
  
  totals_from_infections = totals_from_infections_asymp %>%
    bind_rows(totals_from_infections_other)
  
  
  
  # AS: Multiply the broken down hospitalisations by the timeline proportions
  # to get the number of people in each state on day X.
  message("Getting totals from hospitalisations")
  totals_from_hospitalisations = hospitalisations_curve %>% 
    get_totals_from_actuals(hospitalisations_timeline,
                            fill=0,
                            Age_group, SIMD, Level, Immune_compromised, Population, Death_rate
                           ) %>% 
    # Uncomment this if you need to see where a row in totals_from_actuals comes from.
    mutate(source = "hosp")
  
  
  
  #Join totals estimated from infections actuals with totals estimated from 
  #hospitalisation actuals. Use totals_from_hospitalisations where these exist, and 
  #fill in any blanks from totals_from_infections. There will be hospitalisations 
  #resulting from the most recent infections we have actuals for, which have not yet 
  #gone to hospital and so do not appear yet in the totals_from_hospitalisations
  totals_from_actuals = totals_from_infections %>%  
    bind_rows(totals_from_hospitalisations %>% 
                filter(!(Status %in% c("Asymptomatic", "Infected") ) )
             ) %>% 
    # Apply mortality rates.
    mutate(Number_at = ifelse(Status == "Dead",
                              Number_at * Death_rate,
                              ifelse(Status %>% str_detect("Recover"),
                                     Number_at * (1 - Death_rate),
                                     Number_at
                                    )
                             ),
           Number_starting = ifelse(Status == "Dead",
                                    Number_starting * Death_rate,
                                    ifelse(Status %>% str_detect("Recover"),
                                           Number_starting * (1 - Death_rate),
                                           Number_starting
                                          )
                                   )
          ) %>% 
    select(-Death_rate) %>% 
    # AS: Attach the data from history_actuals.rds.  To avoid double counting, we need to remove the admissions after history_date;
    # and the deaths and occupancy calculated from infections after history_date.
    bind_rows(readRDS("Data/history_actuals.rds") %>% 
                mutate(Number_starting = ifelse(Status %in% c("Oxygen", "Non-invasive ventilation", "Invasive ventilation") & Date >= history_date, 0, Number_starting),
                       Number_starting = ifelse(Status %in% c("Dead", "Recovering", "Recovered") & Date >= history_date & source == "other", 0, Number_starting),
                       Number_at = ifelse(Status %in% c("Oxygen", "Non-invasive ventilation", "Invasive ventilation") & Date >= history_date & source == "other", 0, Number_at)
                      ),
              .
             )
  
}



get_totals_from_actuals = function(curve, timeline, fill, ...){ #in model_functions.R
  
  # AS: Function to combine the broken down infections/hospitalisations with the timeline proportions.
  
  curve %>% 
    nest(-c(ID, Scenario_name)) %>% 
    mutate(data = data %>% map(run_single_timeline, timeline, ...)) %>% 
    unnest %>% 
    spread(Stat, proportion, fill=fill) %>% 
    mutate(Status = Status %>% recode(Symptomatic = "Infected")) %>%
    rename(Number_at = Proportion_at,
           Number_starting = Proportion_starting)
}



get_totals_from_actuals_other = function(curve,
                                         timeline,
                                         fill,
                                         start_date_of_hospitalisations,
                                         start_date_of_ICU,
                                         date_of_admission_from_infection_start,
                                         ...
                                        ){ #in model_functions.R
  
  # AS: Version to use for totals_from_infections_other so it only starts 
  # calculating admissions after we run out of actual admissions.
  
  curve %>% 
    nest(-c(ID, Scenario_name)) %>% 
    mutate(data = data %>% 
             map(
               # AS: This is just run_single_timeline except for the filter.
               function(curve, timeline, ...) {
                 curve %>% 
                   left_join(timeline) %>% 
                   filter(Status %in% c("Asymptomatic", "Symptomatic")
                          | (!severity %in% c("Critical", "Severe") & Date >= start_date_of_hospitalisations - date_of_admission_from_infection_start)
                          | (severity %in% c("Critical", "Severe") & Date >= start_date_of_ICU - date_of_admission_from_infection_start)
                         ) %>% 
                   mutate(Date = Date + date_from_infection) %>% 
                   group_by(severity, Status, Stat, Date, ...) %>% 
                   summarise(proportion = sum(proportion_in_period*proportion))
               }
               , timeline, ...
             ) 
    ) %>% 
    unnest %>% 
    spread(Stat, proportion, fill=fill) %>% 
    mutate(Status = Status %>% recode(Symptomatic = "Infected")) %>%
    rename(Number_at = Proportion_at,
           Number_starting = Proportion_starting)
}



run_model_actuals = function(scenario_df, ...) { #in model_functions.R
  
  # AS: In this version the call to prepare_model now uses the option to multiply the hospitalisation rate by a given percentage for the 5th and 95th percentiles.
  # We need the 5th and 95th percentiles in include_totals_from_actuals in order to get a ribbbon for the bits that are estimated (occupancy, deaths and admissions calculated from actual infections).
  # As there's no random element in include_totals_from_actuals we instead decrease/increase hospitalisation rates by 50% to create the 5th/95th percentile.
  # The multiplier of 50% comes from comparing the 5th and 95th percentiles to the Median for the first forecast point.
  # Note that this multiplier is used to create the ribbons for hospitalisations, but not occupancy; the multiplier for occupancy is defined in the "LoS.R" code.
  
  scenario_df %>% 
    mutate(data = ID %>%
             map(prepare_assumptions, ...)) %>%
    unnest %>% 
    prepare_model(ID, `Scenario_name`, Level, ..., percentile_multiplier = 0.5)
}


