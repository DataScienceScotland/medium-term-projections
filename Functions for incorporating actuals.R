include_totals_from_actuals = function(df, start_date_of_forecast){
  
  date_of_admission_from_infection_start = 12
  
  model = run_model(Age_group, SIMD) 
  
  timeline = get_timeline(start_date_dist = 5,
                          end_date_dist = 28) %>%
    filter(In_care == "No") %>% 
    select(-In_care)
  
  hospitalisations_timeline = get_timeline(start_date_dist = 12,
                                           end_date_dist = 12) %>%
    filter(In_care == "No") %>% 
    select(-In_care) %>%
    #use date from hospitalisation as date_from_infection to work with future functions
    mutate(date_from_infection=date_from_infection-date_of_admission_from_infection_start) %>%
    filter(severity %in% c("Serious",
                           "Severe",
                           "Critical")) 
  
  apply_vaccine = TRUE
  
  message("Breaking down infections curve for asymptomatic")
  infections_curve_asymp = get_historic_infections() %>%
    select(-Level) %>%
    mutate(ID = 0) %>%
    #breakdown by age and SIMD
    get_curve_scenario(efficacy_adjustments = tibble(Date = ymd("2000-01-01"), efficacy_adjustment = as.double(NA))) %>%
    select(-ID) %>%
    left_join(get_population(Age_group, SIMD)) %>%
    mutate(proportion_in_period=proportion_in_period*Population*0.8) %>%
    #breakdown by severity based on assumed infection rates
    left_join(model) %>%
    group_by(Date, ID, Scenario_name, Level, Age_group, SIMD) %>%
    mutate(proportion_in_period = proportion_in_period*(infection_rate/sum(infection_rate))) %>%
    ungroup %>%
    select("Date", 
           "proportion_in_period",
           "Age_group",
           "ID",
           "Scenario_name",
           "Level",
           "SIMD",
           "severity") %>%
    filter(Date > ymd("20200801"),
           Date < start_date_of_forecast-7)
  
  apply_vaccine = FALSE
  
  message("Breaking down infections curve for other")
  infections_curve_other = get_historic_infections() %>%
    select(-Level) %>%
    mutate(ID = 0) %>%
    #breakdown by age and SIMD
    get_curve_scenario(efficacy_adjustments = tibble(Date = ymd("2000-01-01"), efficacy_adjustment = as.double(NA))) %>%
    select(-ID) %>%
    left_join(get_population(Age_group, SIMD)) %>%
    mutate(proportion_in_period=proportion_in_period*Population*0.8) %>%
    #breakdown by severity based on assumed infection rates
    left_join(model) %>%
    group_by(Date, ID, Scenario_name, Level, Age_group, SIMD) %>%
    mutate(proportion_in_period = proportion_in_period*(infection_rate/sum(infection_rate))) %>%
    ungroup %>%
    select("Date", 
           "proportion_in_period",
           "Age_group",
           "ID",
           "Scenario_name",
           "Level",
           "SIMD",
           "severity") %>%
    filter(Date > ymd("20200801"),
           Date < start_date_of_forecast-7)
  
  message("Breaking down hospitalisations curve")
  hospitalisations_curve = read_actual_admissions("Daily") %>%
    spread(Location, Number) %>%
    mutate(Critical = ICU,
           Severe = ICU,
           Serious = Hospital - (2*ICU)) %>%
    select(-Hospital, -ICU) %>%
    #Name hospitalisations as "Cases" so that the get_curve_scenario function works
    mutate(ID = 0) %>%
    gather(severity, Cases, Critical,Severe,Serious) %>%
    get_curve_scenario_hosp(efficacy_adjustments = tibble(Date = ymd("2000-01-01"), efficacy_adjustment = as.double(NA))) %>%
    select(-ID) %>%
    #Multiply by population to get breakdonw by SIMD
    left_join(get_population(Age_group, SIMD)) %>%
    mutate(proportion_in_period = proportion_in_period*Population*0.8) %>%
    #Join to model to get ID numbers
    left_join(model) %>%
    select("Date", 
           "proportion_in_period",
           "Age_group",
           "ID",
           "Scenario_name",
           "Level",
           "SIMD",
           "severity") %>%
    filter(Date > ymd("20200801"),
           !is.na(proportion_in_period),
           Date < start_date_of_forecast + date_of_admission_from_infection_start)
  
  # A = read_actual_admissions("Daily") %>%
  #   filter(Date>ymd("20200801")) %>%
  #   group_by(Date) %>%
  #   summarize(Number=sum(Number))
  # 
  # factors = hospitalisations_curve %>% 
  #   group_by(ID,Date) %>%
  #   summarise(proportion_in_period=sum(proportion_in_period)) %>%
  #   left_join(A,by="Date") %>%
  #   mutate(factor=if_else(proportion_in_period==0,0,Number/proportion_in_period)) %>%
  #   select(Date, factor)
  # 
  # hospitalisations_curve = hospitalisations_curve %>%
  #   left_join(factors) %>%
  #   mutate(proportion_in_period = factor * proportion_in_period)
  
  message("Getting totals from asymptomatic infections")
  totals_from_infections_asymp = get_totals_from_actuals(infections_curve_asymp, timeline, fill=0, Age_group, SIMD, Level) %>% 
    filter(Status == "Asymptomatic")
  
  message("Getting totals from other infections")
  totals_from_infections_other = get_totals_from_actuals(infections_curve_other, timeline, fill=0, Age_group, SIMD, Level) %>% 
    filter(Status != "Asymptomatic")
  
  totals_from_infections = totals_from_infections_asymp %>%
    bind_rows(totals_from_infections_other)
  
  message("Getting totals from hospitalisations")
  totals_from_hospitalisations = get_totals_from_actuals(hospitalisations_curve, hospitalisations_timeline, fill=NA, Age_group, SIMD,Level)
  
  totals_from_actuals = totals_from_infections %>%
    gather(Measure, Number, Number_at, Number_starting) %>% 
    anti_join(totals_from_hospitalisations %>%
                gather(Measure, Number, Number_at, Number_starting) %>%
                filter(!is.na(Number)), 
              by=c("ID",
                   "severity",
                   "Status",
                   "Date",
                   "Age_group",
                   "Measure")) %>%
    #mutate(where = "totals_from_infections") %>%
    bind_rows(totals_from_hospitalisations %>%
                gather(Measure, Number, Number_at, Number_starting) %>%
                filter(!is.na(Number)) #%>%
                #mutate(where = "totals_from_hospitalisations")
               ) %>%
    spread(Measure, Number, fill=0) 
  
  message("Joining forecasts to actuals")
  #Join the forecasts to the actuals
  # df %>%
  #   nest(-c(Scenario_name)) %>%
  #   mutate(data = data %>% map(function(data){
  #     bind_rows(data, totals_from_actuals)
  #   })) %>%
  #   unnest %>%
  #   arrange(ID, Scenario_name, Level,Date)
  
  df %>% 
    mutate(where="df") %>%
    bind_rows(totals_from_actuals) %>%
    arrange(ID, Scenario_name, Level,Date)
}

get_totals_from_actuals = function(curve, timeline, fill,...){
  curve %>% 
    nest(-c(ID, Scenario_name)) %>% 
    mutate(data = data %>% map(run_single_timeline, timeline, ...)) %>% 
    unnest %>% 
    spread(Stat, proportion, fill=fill) %>% 
    mutate(Status = Status %>% recode(Symptomatic = "Infected")) %>%
    rename(Number_at = Proportion_at,
           Number_starting = Proportion_starting)
}


get_curve_scenario_hosp = function(df, efficacy_adjustments) { #in setup.R
  population = get_population(Age_group)
  
  past_vaccine_data = get_vaccine_history()
  future_vaccine_data = get_vaccine_roll_out(max(past_vaccine_data$Date) + 1)
  
  all_vaccine_data = estimate_roll_out_by_age(past_vaccine_data, future_vaccine_data)
  
  susceptibility = get_immunity_data(all_vaccine_data, efficacy_adjustments) %>%
    select(Date, Age_group, Immune, Safe) %>% 
    complete(Date = unique(df$Date), Age_group = 10*(0:8)) %>% 
    group_by(Age_group) %>% 
    fill(Immune, Safe) %>% 
    replace_na(list(Immune = 0, Safe = 0)) %>% 
    left_join(population)
  
  susceptibility %>%
    mutate(Susceptible = Population - Safe) %>% 
    group_by(Date) %>% 
    mutate(Proportion_of_cases = Susceptible / sum(Susceptible)) %>% 
    ungroup %>% 
    full_join(df) %>% 
    transmute(Date,
              ID,
              Age_group,
              severity,
              proportion_in_period = Cases * Proportion_of_cases / Population / 0.8)
  
}

get_timeline = function(length_symptomatic = 8,
                        length_of_infection = 13, 
                        date_of_admission_from_infection_start = 16,
                        treatment_recovery_factor = 1,
                        available_treatments = tibble(In_care = c("No", "Yes"),
                                                      Status = list(c("Oxygen", 
                                                                      "Non-invasive ventilation", 
                                                                      "Invasive ventilation"),
                                                                    c("Oxygen"))),
                        start_date_dist = 5,
                        end_date_dist = 28) {
  #Everyone starts symptomatic phase on day 0
  asymptomatic_start = crossing(severity=c("Mild","Moderate","Serious","Severe","Critical"),
                                Age_group = (0:9)*10,
                                In_care = c("Yes", "No")) %>% 
    mutate(date_from_infection = 0, 
           proportion = 1)
  
  #People with moderate or worse infections develop symptoms on a set day from infection date
  symptomatic_start = crossing(severity=c("Moderate","Serious","Severe","Critical"),
                               Age_group = (0:9)*10,
                               In_care = c("Yes", "No")) %>% 
    mutate(date_from_infection = length_of_infection-length_symptomatic,
           proportion = 1)
  
  #People with serious or worse infections enter treatment on a set day from infection date
  # treatment_start = crossing(severity=c("Serious","Severe","Critical"),
  #                            Age_group = (0:9)*10,
  #                            In_care = c("Yes", "No")) %>% 
  #   mutate(date_from_infection = date_of_admission_from_infection_start,
  #          proportion = 1)
  
  treatment_start = crossing(severity=c("Serious","Severe","Critical"),
                             Age_group = (0:9)*10,
                             In_care = c("Yes", "No")) %>% 
    mutate(date_from_infection = date_of_admission_from_infection_start,
           proportion = 1,
           Status = severity %>% 
             recode(`Serious` = "Oxygen",
                    `Severe` = "Non-invasive ventilation",
                    `Critical` = "Invasive ventilation"))
  
  treatment_at = available_treatments %>% 
    mutate(data = Status %>% 
             map(function(x) {
               filter(get_dates_of_stay(date_of_admission_from_infection_start,
                                        start_date_dist = start_date_dist,
                                        end_date_dist = end_date_dist), 
                      Status %in% x | date_from_infection == date_of_admission_from_infection_start) %>% 
                 bind_rows(filter(get_dates_of_stay(date_of_admission_from_infection_start,
                                                    start_date_dist = start_date_dist,
                                                    end_date_dist = end_date_dist), 
                                  ! Status %in% x & date_from_infection == date_of_admission_from_infection_start+1) %>% 
                             mutate(proportion=0))
             })) %>% 
    select(-Status) %>% 
    unnest
  
  #People with mild and moderate infections start recovering on a set day from infection date
  recovering_start_no_treatment = crossing(severity=c("Mild","Moderate"),
                                           Age_group = (0:9)*10,
                                           In_care = c("Yes", "No")) %>% 
    mutate(date_from_infection = length_of_infection,         
           proportion = 1)
  
  #People with serious or worse infections start recovering a number of days from infection start based on pathways
  recovering_start_treatment = treatment_at %>% 
    group_by(date_from_infection, severity, Age_group, In_care) %>% 
    summarise(proportion = sum(proportion)) %>% 
    group_by(severity, Age_group, In_care) %>% 
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
  #dead_start = recovering_start
  dead_start = crossing(severity=c("Mild","Moderate","Serious","Severe","Critical"),
                        Age_group = (0:9)*10,
                        In_care = c("Yes", "No")) %>% 
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
             fill = list(proportion = 0)) %>% 
    group_by(Status, severity, Age_group, In_care, Event) %>% 
    mutate(proportion = cumsum(proportion)) %>% 
    ungroup %>% 
    spread(Event, proportion) %>% 
    transmute(date_from_infection, Status, severity, Age_group, In_care, proportion = Start-End)
  
  bind_rows(Proportion_starting = bind_rows(start, treatment_start),
            Proportion_at = bind_rows(at, treatment_at),
            .id = "Stat") %>% 
    filter(proportion>0)
}



get_dates_of_stay = function(date_of_admission_from_infection_start = 16, 
                             added_days=0,
                             start_date_dist = 5,
                             end_date_dist = 28) { #in setup.R
  "AssumptionControl_LoSProfiles.csv" %>% 
    get_filepath("Data") %>% 
    read_csv(col_types = c(
      .default = col_double())) %>%
    reduce(1:added_days, ~bind_rows(slice(.,1), .), .init = .) %>% 
    mutate(date_from_infection = 0:(NROW(.)-1)) %>% 
    gather(pathway, proportion, -date_from_infection) %>% 
    crossing(X = start_date_dist:end_date_dist,
             Y = 1/(end_date_dist - (start_date_dist - 1))) %>% 
    mutate(date_from_infection = date_from_infection+X) %>% 
    group_by(date_from_infection, pathway) %>% 
    summarise(proportion = sum(proportion*Y)) %>% 
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


run_curve_epi = function(scenario_info, scenario) { #in setup.R
  
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
  
  sim_output = phases %>% 
    add_vaccine %>% 
    run_simulation_from_scenario(100) %>%
    filter(Date>=start_date_of_forecast)
  
  sim_output %>% 
    saveRDS(scenario %>% str_c("SCHEMa outputs/", ., " sim_output.rds"))
  
  efficacy_adjustments = phases %>% 
    transmute(Date = ymd(start_date), efficacy_adjustment)
  
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

