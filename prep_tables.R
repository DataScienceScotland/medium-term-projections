library(forecast)
library(gridExtra)

make_ppe = function(totals, period = "weeks") { #in prep_tables.R
  
  PPE_requirement = "PPE requirements.xlsx" %>% 
    get_filepath("Data") %>% 
    read_excel %>% 
    left_join("Staff requirement.xlsx" %>% get_filepath("Data") %>% read_excel) %>% 
    mutate(Items_per_bed = Items * `Per day` * `Staff per bed`) %>% 
    select(`PPE ensemble group`, Equipment = `PPE guidance`, Items_per_bed)
  
  ppe = totals %>% 
    ungroup %>% 
    filter(Status %in% c("Non-invasive ventilation", "Invasive ventilation")) %>%
    mutate(Status = "Covid ventilation") %>% 
    bind_rows(totals %>% filter(Status == "Oxygen") %>% mutate(Status = "Covid non vent")) %>% 
    group_by(ID, Scenario_name, Day, Status) %>% 
    summarise(Number_at = sum(Number_at)) %>% 
    bind_rows(tibble(ID = .$ID,
                     Scenario_name = .$Scenario_name,
                     Day = .$Day, 
                     Status = "Unsuspected covid", 
                     Number_at = 13000)) %>% 
    distinct %>% 
    left_join(PPE_requirement, by = c("Status" = "PPE ensemble group")) %>% 
    mutate(Items = Number_at*Items_per_bed) %>% 
    group_by(ID, Scenario_name, Day, Equipment) %>% 
    summarise(Number_required = sum(Items)) %>% 
    arrange(Day) %>% 
    group_by(ID, Scenario_name, Equipment) %>% 
    mutate(Number_by = cumsum(Number_required)) %>% 
    ungroup
  
  if(period == "weeks"){
    ppe %>% 
      mutate(Week = Day %/% 7) %>% 
      group_by(ID, Scenario_name, Equipment, Week) %>% 
      summarise(Number_required = sum(Number_required),
                Number_by = max(Number_by)) %>% 
      gather(Stat, Number, Number_by, Number_required) %>% 
      ungroup
  }
  else if(period == "days"){
    ppe %>% 
      gather(Stat, Number, Number_by, Number_required) %>% 
      ungroup
  }
  
  else stop("Invalid period. Use either weeks or days")
}

extract_cases = function(estimates) { #in prep_tables.R
  estimates %>% 
    filter(Status %in% c("Asymptomatic", "Infected")) %>% 
    gather(Measure, Number, Number_at, Number_starting) %>% 
    filter(Status == "Asymptomatic" | Measure == "Number_at") %>% 
    group_by(ID, Scenario_name, Level, Date, Measure) %>%  
    summarise(`Number of cases` = sum(Number)) %>% 
    ungroup
}

extract_cases_with_symptoms = function(estimates) { #in prep_tables.R
  estimates %>% 
    filter(Status == "Infected") %>% 
    gather(Measure, Number, Number_at, Number_starting) %>% 
    group_by(ID, Scenario_name, Level, Date, Measure) %>% 
    summarise(`Number of cases` = sum(Number)) %>% 
    ungroup
}

extract_hospitalisations = function(estimates, ...) { #in prep_tables.R
  if("In_care" %in% names(estimates)) 
    estimates = estimates %>% filter(In_care == "No")
  
  estimates %>% 
    filter(Status %in% c("Oxygen", "Non-invasive ventilation", "Invasive ventilation")) %>% 
    gather(Measure, Number, Number_at, Number_starting) %>% 
    group_by(ID, Scenario_name, Level, Date, Measure, ...) %>% 
    summarise(`Number of cases` = sum(Number)) %>% 
    ungroup
}

extract_gen_hospitalisations = function(estimates) { #in prep_tables.R
  estimates %>% 
    extract_hospitalisations(Status) %>% 
    filter(Status=="Oxygen") %>% 
    select(-Status)
}

extract_non_inv_vent = function(estimates) { #in prep_tables.R
  estimates %>% 
    extract_hospitalisations(Status) %>% 
    filter(Status=="Non-invasive ventilation") %>% 
    select(-Status)
}

extract_icu = function(estimates) { #in prep_tables.R
  estimates %>% 
    extract_hospitalisations(Status) %>% 
    filter(Status=="Invasive ventilation") %>% 
    select(-Status)
}

extract_deaths = function(estimates) { #in prep_tables.R
  estimates %>% 
    filter(Status == "Dead") %>% 
    group_by(ID, Scenario_name, Level, Date) %>% 
    summarise(`Number of cases` = sum(Number_starting)) %>%
    ungroup %>% 
    mutate(Measure = "Number_starting")
}

extract_recovering  = function(estimates) { #in prep_tables.R
  if("In_care" %in% names(estimates)) 
    estimates = estimates %>% filter(In_care == "No")
  
  estimates %>% 
    filter(Status %in% c("Recovering")) %>% 
    gather(Measure, Number, Number_at, Number_starting) %>% 
    group_by(ID, Scenario_name, Level, Date, Measure) %>% 
    summarise(`Number of cases` = sum(Number)) %>% 
    ungroup
}

extract_long_table = function(statuses, estimates, status_names = names(statuses)) {
  if(status_names %>% is_null) status_names = statuses
  
  statuses %>% 
    str_c("extract_", .) %>% 
    map(do.call, list(estimates = estimates)) %>% 
    set_names(status_names) %>% 
    bind_rows(.id = "Status")
}

get_tests = function(estimates, period = "weeks") { #in prep_tables.R
  
  Hosp = estimates %>% 
    extract_hospitalisations %>% 
    filter(Measure == "Number_starting") %>% 
    select(-Measure) %>% 
    mutate(Status = "Hospital")
  
  Recovering = estimates %>%
    filter(Status=="Recovering", severity %in% c("Serious", "Severe", "Critical")) %>% 
    group_by(ID, Day, Date) %>% 
    summarise(`Number of cases` = sum(Number_starting)) %>% 
    mutate(Status = "Recovering")
  
  #People are tested twice when they start to recover from being seriously, severely, or critically ill
  # and once when they first enter hospital
  Tests = Recovering %>%
    mutate(`Number of cases` = `Number of cases`*2) %>% 
    bind_rows(Hosp) %>% 
    group_by(ID, Day, Date) %>% 
    summarise(`Number of cases` = 600+sum(`Number of cases`)) %>% 
    mutate(Status = "Test")
  
  current_est = list(Hosp, Recovering, Tests) %>% 
    bind_rows %>%
    ungroup %>% 
    filter(! `Number of cases` %>% is.na)
  
  if(period == "weeks") {
    temp = current_est %>% 
      spread(Status, `Number of cases`) %>%
      mutate(week_day = Day %% 7,
             Week = Day %/% 7) %>%
      group_by(Week) %>%
      mutate(Week_commencing = min(Date)) %>%
      group_by(Week_commencing, Week, ID) %>%                           #added -Week_commencing
      summarise(`1 Hospitalisation admissions` = sum(Hospital),
                `2 Patients recovering` = sum(Recovering),
                `3 Tests required` = sum(Test)) %>%
      arrange(Week_commencing) %>% 
      group_by(ID) %>%
      mutate(`4 Cumulative tests` = cumsum(`3 Tests required`)) %>%
      ungroup %>%
      gather(`Type_of_case`, `Number of cases`, -Week, -Week_commencing, -ID) %>%             #added -Week_commencing
      replace_na(list(`Number of cases`=0)) %>% 
      arrange(ID, `Type_of_case`, Week_commencing)                           #added -Week_commencing
    
  }
  
  else if(period == "days") {  
    temp = current_est %>% 
      spread(Status, `Number of cases`) %>%
      rename(`1 Hospitalisation admissions` = Hospital,
             `2 Patients recovering` = Recovering,
             `3 Tests required` = Test) %>%
      arrange(Day) %>% 
      group_by(ID) %>%
      mutate(`4 Cumulative tests` = cumsum(`3 Tests required`)) %>%
      ungroup %>%
      gather(`Type_of_case`, `Number of cases`, -Day, -Date, -ID) %>%
      select(ID, `Type_of_case`, Day, Date, `Number of cases`) %>%                      # added in Date
      replace_na(list(`Number of cases`=0)) %>% 
      arrange(ID, `Type_of_case`, Date)                   # added in Date
  }
  
  else stop("Invalid period. Use either weeks or days")
}

get_ppe = function(totals, period = "weeks") { #in prep_tables.R
  
  PPE_requirement = read_excel("PPE requirements.xlsx" %>% get_filepath("Data")) %>% 
    left_join(read_excel("Staff requirement.xlsx" %>% get_filepath("Data"))) %>% 
    mutate(Items_per_bed = Items * `Per day` * `Staff per bed`) %>% 
    select(`PPE ensemble group`, Equipment = `PPE guidance`, Items_per_bed)
  
  ppe = totals %>% 
    ungroup %>% 
    filter(Status %in% c("Non-invasive ventilation", "Invasive ventilation")) %>%
    mutate(Status = "Covid ventilation") %>% 
    bind_rows(totals %>% filter(Status == "Oxygen") %>% mutate(Status = "Covid non vent")) %>% 
    group_by(ID, Scenario_name, Day, Date, Status) %>%                           #added Date
    summarise(Number_at = sum(Number_at)) %>% 
    bind_rows(tibble(ID = .$ID,
                     Scenario_name = .$Scenario_name,
                     Day = .$Day, 
                     Date = .$Date,                                               #added Date
                     Status = "Unsuspected covid", 
                     Number_at = 13000)) %>% 
    distinct %>% 
    left_join(PPE_requirement, by = c("Status" = "PPE ensemble group")) %>% 
    mutate(Items = Number_at*Items_per_bed) %>% 
    group_by(ID, Scenario_name, Day, Date, Equipment) %>%                             #added Date
    summarise(Number_required = sum(Items)) %>% 
    arrange(Date) %>% 
    group_by(ID, Scenario_name, Equipment) %>% 
    mutate(Number_by = cumsum(Number_required)) %>% 
    ungroup
  
  if(period == "weeks"){
    ppe %>% 
      mutate(Week = Day %/% 7) %>%
      group_by(Week) %>%
      mutate(Week_commencing = min(Date)) %>%                              #added Date
      ungroup %>%
      group_by(ID, Scenario_name, Equipment, Week_commencing, Week) %>% 
      summarise(Number_required = sum(Number_required),
                Number_by = max(Number_by)) %>% 
      gather(Stat, Number, Number_by, Number_required) %>% 
      ungroup
  }
  else if(period == "days"){
    ppe %>% 
      gather(Stat, Number, Number_by, Number_required) %>% 
      ungroup
  }
  
  else stop("Invalid period. Use either weeks or days")
}

get_medicine = function(totals, period = "weeks") { #in prep_tables.R
  meds_data = read_excel("Covid-19_RegionalMedsNeedsITUList_20200331v0.4_5.xlsx" %>% 
                           get_filepath("Data"),
                         sheet = "InputData_RefSheet") %>% 
    select(`Meds Group`, `Comparator product`, `Avg daily dose`)
  
  meds = totals %>% 
    filter(Status %in% c("Invasive ventilation", "Non-invasive ventilation")) %>% 
    group_by(ID, Day, Date) %>%                                                       #added Date
    summarise(Patients = sum(Number_at)) %>% 
    mutate(data = list(meds_data)) %>% 
    unnest %>% 
    mutate(Demand = Patients*`Avg daily dose`) %>% 
    group_by(ID, Day, Date, `Meds Group`, `Comparator product`) %>%                  #added Date
    summarise(Daily = sum(Demand)) %>%
    arrange(Date) %>% 
    group_by(ID, `Meds Group`, `Comparator product`) %>% 
    mutate(Cumulative = cumsum(Daily))
  
  if(period == "days") {
    meds %>% 
      ungroup %>% 
      gather(Measure, Requirement, Daily, Cumulative) %>% 
      mutate(Measure = Measure %>% str_c(" required doses"))
  }
  
  else if(period == "weeks") {  
    meds %>% 
      mutate(Week = Day %/% 7) %>%
      group_by(Week) %>%
      mutate(Week_commencing = min(Date)) %>%
      ungroup %>%
      group_by(ID, Week, Week_commencing, `Meds Group`, `Comparator product`) %>%      #added Week_commencing
      summarise(Weekly = sum(Daily),
                Cumulative = max(Cumulative)) %>% 
      ungroup %>% 
      gather(Measure, Requirement, Weekly, Cumulative) %>% 
      mutate(Measure = Measure %>% str_c(" required doses"))
  }
  
  else stop("Invalid period. Use either weeks or days")
}


get_current_estimates = function(estimates, period = "weeks") { #in prep_tables.R
  
  Cases = estimates %>% 
    extract_cases
  
  `Cases with symptoms` = estimates %>% 
    extract_cases_with_symptoms
  
  Hosp = estimates %>% 
    extract_hospitalisations %>%
    mutate(Status = Measure %>% recode(Number_at = "Hospital_at",
                                       Number_starting = "Hospital_new")) %>%
    select(-Measure)
  
  icu = estimates %>% 
    extract_icu %>%
    mutate(Status = Measure %>% recode(Number_at = "icu_at",
                                       Number_starting = "icu_new")) %>%
    select(-Measure)
  
  Mort = estimates %>% 
    extract_deaths
  
  current_est = list(Cases, `Cases with symptoms`, Hosp, icu, Mort) %>% 
    bind_rows %>%
    ungroup %>% 
    filter(! `Number of cases` %>% is.na) %>%
    filter(! Status %in% c("Invasive ventilation", "Non-invasive ventilation"))
  
  if(period == "weeks") {
    current_est %>% 
      spread(Status, `Number of cases`) %>%
      mutate(week_day = Day %% 7,
             Week = Day %/% 7) %>%
      group_by(Week) %>%
      mutate(Week_commencing = min(Date)) %>%      
      ungroup %>%
      group_by(Week_commencing, Week, ID) %>%           
      summarise(`1 Cases` = max(Infected),
                `2 Cases with symptoms` = max(Symptomatic),
                `3 Patients requiring admission to hospital` = sum(Hospital_new),
                `4 Peak number of beds required that week` = max(Hospital_at),
                `5 Patients requiring admission to ICU` = sum(`icu_new`),
                `6 Peak number of ICU beds required that week` = max(icu_at),
                `7 Deaths` = sum(Dead)) %>%
      ungroup %>%
      gather(`Type_of_case`, `Number of cases`, -Week_commencing, -Week, -ID) %>%  
      replace_na(list(`Number of cases`=0)) %>% 
      arrange(ID, `Type_of_case`, Week_commencing)                                
  }
  
  else if(period == "days") {  
    current_est %>% 
      mutate(`Type_of_case` = Status %>% 
               recode(Infected = "1 Cases",
                      Symptomatic = "2 Cases with symptoms",
                      Hospital_new = "3 Patients requiring admission to hospital",
                      Hospital_at = "4 Peak number of beds required that week",
                      icu_new = "5 Patients requiring admission to ICU",
                      icu_at = "6 Peak number of ICU beds required that week",
                      Dead = "7 Deaths")) %>%
      select(ID, `Type_of_case`, Day, Date, `Number of cases`) %>%    
      replace_na(list(`Number of cases`=0)) %>% 
      arrange(ID, `Type_of_case`, Date)                   
  }
  
  else stop("Invalid period. Use either weeks or days")
}


get_hospitalisations = function(estimates, period = "weeks") { #in prep_tables.R
  
  Gen_hosp = estimates %>%
    extract_gen_hospitalisations %>%
    mutate(Status = Measure %>% recode(Number_at = "Gen_hospital_at",
                                       Number_starting = "Gen_hospital_new")) %>%
    select(-Measure)
  
  Non_inv_vent = estimates %>%
    extract_non_inv_vent %>%
    mutate(Status = Measure %>% recode(Number_at = "Non_inv_vent_at",
                                       Number_starting = "Non_inv_vent_new")) %>%
    select(-Measure)
  
  icu = estimates %>%
    extract_icu %>%
    mutate(Status = Measure %>% recode(Number_at = "icu_at",
                                       Number_starting = "icu_new")) %>%
    select(-Measure)
  
  current_hosp_est = list(Gen_hosp, Non_inv_vent, icu) %>%
    bind_rows %>%
    ungroup %>%
    filter(! `Number of cases` %>% is.na,
           Status %in% c("Gen_hospital_at", "Non_inv_vent_at", "icu_at"))
  
  if(period == "weeks") {
    current_hosp_est %>% 
      spread(Status, `Number of cases`) %>%
      mutate(week_day = Day %% 7,
             Week = Day %/% 7) %>%
      group_by(Week) %>%
      mutate(Week_commencing = min(Date)) %>%   #added line to calculate start date for each week
      ungroup %>%   
      group_by(Week_commencing, Week, ID) %>%                #added -Week_commencing
      summarise(`1 General hospitalisation (not including ventilation)` = max(Gen_hospital_at),
                `2 Numbers requiring non-invasive ventilation` = max(Non_inv_vent_at),
                `3 Numbers in ICU on invasive ventilation` = max(icu_at)) %>%
      ungroup %>%
      gather(`Type_of_case`, `Number of cases`, -Week_commencing, -Week, -ID) %>%   #added -Week_commencing
      replace_na(list(`Number of cases`=0)) %>% 
      arrange(ID, `Type_of_case`, Week_commencing)                                  #arranged by Week_commencing
  }
  
  else if(period == "days") {  
    current_hosp_est %>% 
      mutate(`Type_of_case` = Status %>% 
               recode(Gen_hospital_at = "1 General hospitalisation (not including ventilation)",
                      Non_inv_vent_at = "2 Numbers requiring non-invasive ventilation",
                      icu_at = "3 Numbers in ICU on invasive ventilation")) %>%
      select(ID, `Type_of_case`, Day, Date, `Number of cases`) %>%                      #included Date
      replace_na(list(`Number of cases`=0)) %>% 
      arrange(ID, `Type_of_case`, Date)                       #arranged by Date
  }
  
  else stop("Invalid period. Use either weeks or days")
  
  
}

get_bed_days = function(estimates, period = "weeks") { #in prep_tables.R
  
  Gen_hosp = estimates %>%
    extract_gen_hospitalisations %>%
    mutate(Status = Measure %>% recode(Number_at = "Gen_hospital_at",
                                       Number_starting = "Gen_hospital_new")) %>%
    select(-Measure)
  
  Non_inv_vent = estimates %>%
    extract_non_inv_vent %>%
    mutate(Status = Measure %>% recode(Number_at = "Non_inv_vent_at",
                                       Number_starting = "Non_inv_vent_new")) %>%
    select(-Measure)
  
  icu = estimates %>%
    extract_icu %>%
    mutate(Status = Measure %>% recode(Number_at = "icu_at",
                                       Number_starting = "icu_new")) %>%
    select(-Measure)
  
  total = estimates %>%
    extract_hospitalisations %>%
    mutate(Status = Measure %>% recode(Number_at = "total_at",
                                       Number_starting = "total_new")) %>%
    select(-Measure)
  
  current_hosp_est = list(Gen_hosp, Non_inv_vent, icu, total) %>%
    bind_rows %>%
    ungroup %>%
    filter(! `Number of cases` %>% is.na,
           Status %in% c("Gen_hospital_at", "Non_inv_vent_at", "icu_at", "total_at"))
  
  if(period == "weeks") {
    current_hosp_est %>% 
      spread(Status, `Number of cases`) %>%
      mutate(week_day = Day %% 7,
             Week = Day %/% 7) %>%
      group_by(Week) %>%
      mutate(Week_commencing = min(Date)) %>%   #added line to calculate start date for each week
      ungroup %>%   
      group_by(Week_commencing, Week, ID) %>%                #added -Week_commencing
      summarise(`1 General hospitalisation (not including ventilation)` = sum(Gen_hospital_at), #sum used as we want bed days
                `2 Numbers requiring non-invasive ventilation` = sum(Non_inv_vent_at),
                `3 Numbers in ICU on invasive ventilation` = sum(icu_at),
                `4 Total numbers in hospital` = sum(total_at)) %>%
      ungroup %>%
      gather(`Type_of_case`, `Number of cases`, -Week_commencing, -Week, -ID) %>%   #added -Week_commencing
      replace_na(list(`Number of cases`=0)) %>% 
      arrange(ID, `Type_of_case`, Week_commencing)                                  #arranged by Week_commencing
  }
  
  else if(period == "days") {  
    current_hosp_est %>% 
      mutate(`Type_of_case` = Status %>% 
               recode(Gen_hospital_at = "1 General hospitalisation (not including ventilation)",
                      Non_inv_vent_at = "2 Numbers requiring non-invasive ventilation",
                      icu_at = "3 Numbers in ICU on invasive ventilation",
                      total_at = "4 Total numbers in hospital")) %>%
      select(ID, `Type_of_case`, Day, Date, `Number of cases`) %>%                      #included Date
      replace_na(list(`Number of cases`=0)) %>% 
      arrange(ID, `Type_of_case`, Date)                       #arranged by Date
  }
  
  else stop("Invalid period. Use either weeks or days")
  
  
}


get_contact_tracing = function(df){#in prep_tables.R
  
  #get_contact_tracing calculates demand for tests and contact tracing
  #Contacts is the number of contacts needing to be traced
  #Tracer is the number of tracers required to trace these contacts, taking into consideration the time it takes
  #a tracer to trace each index case, and additional time per contact, active tracing time per shift, efficiency 
  #saving, and a tracer_expansion to avoid queues
  
  #Assuming no backlog - number of tracers required to trace all contacts on the day they are first needing to be traced
  assumptions = get_tracing_assumptions() %>%
    spread(Assumption, Value)
  
  df %>%
    filter(Status == "Infected") %>%
    mutate(Day = Day + assumptions$test_turnaround_period,
           Date = Date + assumptions$test_turnaround_period,
           Contacts = Number_starting*assumptions$number_of_contacts,
           Efficiency_s = predict(get_ES_parameters(),
                                  list(x=Day-131)),
           Efficiency_s=ifelse(Efficiency_s<0,0,ifelse(Efficiency_s>0.5,0.5,Efficiency_s)),
           Tracer = assumptions$tracer_expansion*Number_starting*(1-Efficiency_s)*((assumptions$tracer_time_per_case+assumptions$tracer_time_per_contact*assumptions$number_of_contacts)/assumptions$active_tracer_time)) %>%
    select(-Number_at, -Number_starting, -Status) %>%
    gather(Status, Number_at, Contacts, Tracer)
  
}

get_tracing_for_ss = function(df,
                              period = "days") {#in prep_tables.R
  
  New_infections = df %>% 
    filter(Status %in% c("Asymptomatic")) %>% 
    mutate(Status = "Infected") %>% 
    group_by(ID, Day, Date, Status) %>%                                                     
    summarise(`Number of cases` = sum(Number_starting))
  
  New_symptomatic = df %>% 
    filter(Status == "Infected", severity != "Mild") %>% 
    mutate(Status = "Symptomatic") %>%
    group_by(ID, Day, Date,Status) %>%                                                      
    summarise(`Number of cases` = sum(Number_starting))
  
  Tracing = df %>%
    get_contact_tracing() %>%
    group_by(ID, Date, Day, Status) %>% 
    summarise(Number_at = sum(Number_at)) %>% 
    ungroup %>%
    filter(Status != "Test") %>%
    arrange(Status) %>%
    rename("Number of cases"=Number_at)
  
  current_est_tracing = list(New_infections, New_symptomatic, Tracing) %>% 
    bind_rows %>%
    ungroup %>% 
    filter(! `Number of cases` %>% is.na)
  
  if(period == "weeks") {
    temp = current_est_tracing %>% 
      spread(Status, `Number of cases`) %>%
      mutate(week_day = Day %% 7,
             Week = Day %/% 7) %>%
      group_by(Week) %>%
      mutate(Week_commencing = min(Date)) %>%
      group_by(Week_commencing, Week, ID) %>%                       
      summarise(`1 New infections` = sum(Infected),
                `2 New symptomatic cases` = sum(Symptomatic),
                `3 Contact tracing required` = sum(Contacts),
                `4 Tracers required` = max(Tracer)) %>%
      ungroup %>%
      gather(`Type_of_case`, `Number of cases`, -Week, -Week_commencing, -ID) %>%
      replace_na(list(`Number of cases`=0)) %>% 
      arrange(ID, `Type_of_case`, Week_commencing)                        
    
  }
  
  else if(period == "days") {  
    temp = current_est_tracing %>% 
      spread(Status, `Number of cases`) %>%
      rename(`1 New infections` = Infected,
             `2 New symptomatic cases` = Symptomatic,
             `3 Contact tracing required` = Contacts,
             `4 Tracers required` = Tracer) %>%
      gather(`Type_of_case`, `Number of cases`, -Day, -Date, -ID) %>%
      select(ID, `Type_of_case`, Day, Date, `Number of cases`) %>%                      
      replace_na(list(`Number of cases`=0)) %>% 
      arrange(ID, `Type_of_case`, Date)                 
  }
  
  else stop("Invalid period. Use either weeks or days")
  
}


get_tracing_assumptions = function(){ #in prep_tables.R
  "Assumptions - Tracing.csv" %>%
    get_filepath("Data") %>%
    read_csv(col_types = c(
      Assumption = col_character(),
      Value = col_double())) 
}

get_ES_parameters= function(){ #in prep_tables.R
  temp = get_tracing_assumptions() %>%
    filter(Assumption %>% str_detect("ES_Milestone_")) %>%
    separate(Assumption, c("Assumption", "order"), "-", extra = "merge")%>%
    group_by(Assumption) %>%
    arrange(order, .by_group=TRUE) %>%
    ungroup %>%
    spread(Assumption, Value) %>%
    select(-order) %>%
    transmute(x = ES_Milestone_date-ES_Milestone_date[1],
              y=ES_Milestone_value)
  
  temp %>% nls(y ~ 1 - 1/(x/A + B),
               data = .,
               start = list(A = 1, B=1))
  
}

prep_SPI_forecast = function(estimates, start_date, end_date, model_version) { #in prep_tables.R
  c(infections = "cases", hospital = "hospitalisations", icu = "icu", death = "deaths",
    recovering = "recovering") %>% 
    extract_long_table(estimates) %>% 
    filter(Date >= start_date, 
           Date <= end_date) %>% 
    filter(Status != "recovering" | Measure == "Number_at") %>% 
    mutate(Status = Status %>% recode(recovering = "hospital")) %>% 
    group_by(Status, ID, Scenario_name, Level, Date, Measure) %>% 
    summarise(`Number of cases` = sum(`Number of cases`)) %>% 
    ungroup %>% 
    transmute(Date, 
              Level = Level %>% 
                str_replace("Median", "50") %>% 
                str_extract("\\d*") %>% 
                as.integer %>% 
                `/`(100) %>% 
                str_c("Quantile ", .),
              Scenario_name,
              ValueType = Measure %>% 
                recode(Number_at = "prev", 
                       Number_starting = "inc") %>% 
                str_c(Status, ., sep="_") %>% 
                recode(death_inc = "type28_death_inc_line"),
              Number = `Number of cases`) %>% 
    filter(ValueType %in% c("infections_inc", 
                            "hospital_prev", 
                            "hospital_inc", 
                            "icu_prev", 
                            "icu_inc",
                            "type28_death_inc_line"))%>% 
    spread(Level, Number) %>% 
    transmute(Group = "ScottishGovernment",
              Model = Scenario_name %>% 
                str_to_lower %>% 
                str_c("Mechanistic_SCIRe_", .),
              ModelType = "Multiple",
              Version = model_version,
              Scenario = "MTP",
              `Creation Day` = today() %>% day(),
              `Creation Month` = today() %>% month(),
              `Creation Year` = today() %>% year(),
              `Day of Value` = day(Date),
              `Month of Value` = month(Date),
              `Year of Value` = year(Date),
              AgeBand = "All",
              Geography = "Scotland",
              ValueType,
              Value = NA,
              `Quantile 0.05`,
              `Quantile 0.1` = NA,
              `Quantile 0.15` = NA,
              `Quantile 0.2` = NA,
              `Quantile 0.25`,
              `Quantile 0.3` = NA,
              `Quantile 0.35` = NA,
              `Quantile 0.4` = NA,
              `Quantile 0.45` = NA,
              `Quantile 0.5`,
              `Quantile 0.55` = NA,
              `Quantile 0.6` = NA,
              `Quantile 0.65` = NA,
              `Quantile 0.7` = NA,
              `Quantile 0.75`,
              `Quantile 0.8` = NA,
              `Quantile 0.85` = NA,
              `Quantile 0.9` = NA,
              `Quantile 0.95`)
}

#prepares the table to be exported for the PHS 12 week scenarios
prep_PHS_forecast = function(estimates, base_date = today() %>% floor_date("weeks")) { #in prep_tables.R
  c(home = "cases", symptomatic = "cases_with_symptoms", hospital = "hospitalisations", critical_care = "icu", deaths = "deaths") %>%
    extract_long_table(estimates) %>%
    filter(Date >= base_date - 38, Date <= base_date + 95) %>%
    transmute(seir_date = Date,
              scenario_description = Scenario_name %>% str_c(Level, sep = " "),
              Variable = Measure %>%
                recode(Number_at = "infectious",
                       Number_starting = "infected_daily") %>%
                str_c("number", ., Status, sep = "_") %>%
                recode(number_infected_daily_deaths = "number_daily_deaths",
                       number_infected_daily_symptomatic = "number_infected_symptomatic_daily_home",
                       number_infectious_home = "number_infectious"),
              `Number of cases`) %>%
    filter(Variable != "number_infected_deaths") %>%
    spread(Variable, `Number of cases`) %>%
    mutate(health_board = "SCOTLAND")
}

prep_Chris_Robertson_forecast = function(estimates, base_date = today() %>% floor_date("weeks")) { #in prep_tables.R
  estimates %>%
    filter(Status %in% c("Asymptomatic"),
           Date >= base_date - 10, Date <= base_date + 55) %>%
    gather(ValueType, Number, Number_at, Number_starting) %>%
    group_by(Date, Scenario_name, Level, Age_group, ValueType) %>%
    summarise(Infections = sum(Number)) %>%
    ungroup %>%
    filter(ValueType %in% c("Number_starting")) %>%
    select(Date, Scenario_name, Level, Age_group, Infections)
}

#creates a table of new infections
prep_infections_data = function(estimates, 
                                start_date = (today() %>% floor_date("weeks", week_start = 1)) - 56,
                                end_date = start_date + 140,
                                s_gene_status = "all") { #in prep_tables.R
  actuals = get_historic_infections(s_gene_status = s_gene_status) %>% 
    transmute(Date,
              ValueType = "Incidence",
              Actual = Cases)
  
  c(Cases = "cases") %>% 
    extract_long_table(estimates) %>% 
    filter(Date >= start_date, 
           Date <= end_date) %>% 
    transmute(Date, 
              Level, 
              Scenario_name,
              ValueType = Measure %>% 
                recode(Number_at = "Prevalence",
                       Number_starting = "Incidence"),
              Number = `Number of cases`) %>% 
    spread(Level, Number) %>% 
    left_join(actuals)
}

#creates a table of new confirmed cases (symptomatic cases lagged)
prep_confirmed_cases_data = function(estimates, 
                                     start_date = (today() %>% floor_date("weeks", week_start = 1)) - 56,
                                     end_date = start_date + 140) { #in prep_tables.R
  actuals = get_LA_data("Positive") %>% 
    filter(Local_authority == "Grand Total") %>% 
    transmute(Date,
              ValueType = "Incidence",
              Actual = Tests)
  
  c(Cases = "cases_with_symptoms") %>% 
    extract_long_table(estimates) %>% 
    mutate(Date = Date + 3) %>% 
    filter(Date >= start_date, 
           Date <= end_date) %>% 
    transmute(Date, 
              Level, 
              Scenario_name,
              ValueType = Measure %>% 
                recode(Number_at = "Prevalence",
                       Number_starting = "Incidence"),
              Number = `Number of cases`) %>% 
    spread(Level, Number) %>% 
    left_join(actuals)
}

#creates a table of hospitalisation and ICU
prep_hospitalisation_data = function(estimates, 
                                     start_date = (today() %>% floor_date("weeks", week_start = 1)) - 56,
                                     end_date = start_date + 140, 
                                     include_recovering = FALSE,
                                     ICU_source = "published") { #in prep_tables.R
  
  actuals = list(admissions = read_actual_admissions("Daily", ICU_source = ICU_source),
                 beds = read_actual_concurrent(ICU_source = ICU_source)) %>% 
    bind_rows(.id = "Type") %>% 
    transmute(Date, 
              ValueType = Location %>% str_c(Type, sep = " "), 
              Actual = Number)
  
  if(include_recovering) {
    settings = c(Hospital = "hospitalisations", ICU = "icu", Recovering = "recovering")
  }
  else 
    settings = c(Hospital = "hospitalisations", ICU = "icu")
  
  settings %>% 
    extract_long_table(estimates) %>% 
    filter(Date >= start_date, 
           Date <= end_date,
           Status != "Recovering" | Measure == "Number_at") %>% 
    mutate(Status = Status %>% recode(Recovering = "Hospital")) %>% 
    group_by(Date, Level, Scenario_name, Measure, Status) %>% 
    summarise(`Number of cases` = sum(`Number of cases`)) %>% 
    ungroup %>% 
    transmute(Date, 
              Level, 
              Scenario_name,
              ValueType = Measure %>% 
                recode(Number_at = " beds",
                       Number_starting = " admissions") %>% 
                str_c(Status, .),
              Number = `Number of cases`) %>% 
    spread(Level, Number) %>% 
    left_join(actuals)
}

#creates a table of deaths
prep_deaths_data = function(estimates,
                            start_date = (today() %>% floor_date("weeks", week_start = 1)) - 56,
                            end_date = start_date + 140,
                            actual_measure = "Daily deaths") { #in prep_tables.R
  actuals = read_actual_deaths() %>%
    spread(Measure, Number) %>%
    mutate(`Deaths (7 day average)` = `Daily deaths` %>% ma(7) %>% c) %>%
    select("Date", "Cumulative deaths", actual_measure) %>%
    gather(Measure, Number, -Date) %>%
    transmute(Date,
              ValueType = Measure,
              Actual = Number)
  
  c(Cases = "deaths") %>%
    extract_long_table(estimates) %>%
    filter(Date >= start_date,
           Date <= end_date) %>%
    transmute(Date,
              Level,
              Scenario_name,
              ValueType = Measure %>%
                recode(Number_at = "Cumulative deaths",
                       Number_starting = actual_measure),
              Number = `Number of cases`) %>%
    spread(Level, Number) %>%
    left_join(actuals)
}

#creates a table of all the variables for the main pivoted output
prep_main_data = function(estimates, 
                          start_date = (today() %>% floor_date("weeks", week_start = 1)) - 56,
                          end_date = start_date + 140) { #in prep_tables.R
  x = c(Infections = "cases", Symptomatic = "cases_with_symptoms", Hospital = "hospitalisations", ICU = "icu", Mortality = "deaths") %>% 
    extract_long_table(estimates) %>% 
    filter(Date >= start_date, 
           Date <= end_date,
           (Status != "Dead" | Measure == "Number_starting")) %>% 
    group_by(Status, ID, Scenario_name, Level, Date, Measure) %>% 
    summarise(`Number of cases` = sum(`Number of cases`)) %>% 
    ungroup %>% 
    transmute(Date, 
              Level, 
              Scenario_name,
              Status,
              Measure = Measure %>% 
                recode(Number_at = "Prevalence",
                       Number_starting = "Incidence"),
              Number = `Number of cases`) %>% 
    mutate(Number = Level %>% 
             recode(`5th percentile` = Number %>% adjust_lower_bound,
                    `25th percentile` = Number %>% adjust_lower_bound,
                    `75th percentile` = Number %>% adjust_upper_bound,
                    `95th percentile` = Number %>% adjust_upper_bound,
                    .default = Number)) %>% 
    spread(Level, Number)
  
  x %>% 
    filter(Status == "Hospital", Measure == "Prevalence") %>%
    adjust_occupancy %>%
    bind_rows(x %>%
                filter(Status != "Hospital" | Measure != "Prevalence")) %>%
    gather(Level, Number, -Date, -Scenario_name, -Status, -Measure) %>%
    select(Date, Level, Scenario_name, Status, Measure, Number)
}

extract_legend = function(my_ggp) { #in prep_tables.R
  step1 = my_ggp %>% 
    ggplot_build %>% 
    ggplot_gtable
  
  step2 = which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  
  step1$grobs[[step2]]
}

edit_status = function(df, status, number_starting_factor = 1, number_at_factor = 1) { #in prep_tables.R
  df %>% 
    mutate(Number_starting = Status %>% 
             recode(Dead = Number_starting*number_starting_factor,
                    .default = Number_starting),
           Number_at = Status %>% 
             recode(Dead = Number_at*number_at_factor,
                    .default = Number_at))
}

prep_peaks_table = function(estimates,
                            start_date = (today() %>% floor_date("weeks", week_start = 1)),
                            end_date = start_date + 140,
                            measures = c("Infections", "Hospital beds", "Hospital admissions", 
                                         "ICU beds", "ICU admissions", "Deaths"),
                            measures_to_smooth = c(),
                            smoothed_quantiles = c()) { #in prep_tables.R
  if(length(unique(estimates$Scenario_name)) > 1) warning("Multiple scenarios detected. This may lead to unexpected results.")
  
  min = 5
  phi = 10
  
  df = c(Infections = "cases",
         Hospital = "hospitalisations",
         ICU = "icu",
         Deaths = "deaths") %>% 
    extract_long_table(estimates) %>% 
    mutate(Measure = Status %>% 
             str_c(Measure %>% recode(Number_at = " prevalence", Number_starting = "")) %>% 
             recode(`Hospital prevalence` = "Hospital beds",
                    `ICU prevalence` = "ICU beds",
                    Hospital = "Hospital admissions",
                    ICU = "ICU admissions")) %>% 
    filter(Date >= start_date, 
           Date <= end_date,
           Level %in% c("5th percentile", "95th percentile", "Median"),
           Measure %in% measures) %>% 
    mutate(Measure = Measure %>% factor(levels = measures)) %>% 
    select(-ID) %>%
    pivot_wider(names_from = "Level", values_from = "Number of cases") %>% 
    mutate(`5th percentile` = `5th percentile` %>% adjust_lower_bound(min, phi),
           `95th percentile` = `95th percentile` %>% adjust_upper_bound(min, phi))
  
  if(length(measures_to_smooth) > 0) {
    df = df %>% 
      filter(Measure %in% measures_to_smooth) %>%
      group_by(Scenario_name, Measure) %>% 
      mutate_at(smoothed_quantiles,
                smooth_quantile) %>%
      ungroup %>% 
      bind_rows(df %>% filter(! Measure %in% measures_to_smooth))
  }
  
  df %>% 
    pivot_longer(c("5th percentile", "95th percentile", "Median"), 
                 names_to = "Level", 
                 values_to = "Number of cases") %>% 
    group_by(Measure, Level) %>% 
    summarise(Number = max(`Number of cases`)) %>% 
    ungroup %>% 
    spread(Level, Number)%>% 
    mutate(`5th percentile` = `5th percentile` %>% round_better(2, fun = "signif_floor") %>% floor,
           `95th percentile` = `95th percentile` %>% round_better(2, fun = "signif_ceiling") %>% ceiling,
           Median = Median %>% round_better(2) %>% round) %>% 
    arrange(Measure)
}
