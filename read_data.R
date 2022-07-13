read_sicsag_ICU = function(...) { # in read_data.R
  # Code to read in the data in "\\s0177a\datashare\CoMix\Public\SCIRe\Data\Sicsag".
  # ... should be set to Age_group to get output by age group or left blank to get output aggregated over all ages.
  # Read in all the Excel files in the folder.
  # Arguably we should only read in those with "Covid+ by age" in the name in case other data gets put in the folder.
  # On the other hand, reading in all the files means the code will still work if the file naming convention changes.
  all = list.files(path = "./Data/Sicsag", pattern = "*.xlsx", full.names = TRUE)
  
  
  # Separate the files with "backdated" in their name as these have a different format to the rest.
  # Prevalence and incidence are in different sheets ("Midnight" and "New Daily" respectively) so need to read them in separately.
  # Change the format to match that used in read_actual_admissions and read_actual_concurrent.
  backdated_prevalence = all[grep("backdated", all)] %>% 
    lapply(read_excel, sheet = "Midnight") %>% 
    bind_rows %>% 
    transmute(Date = ymd(Report_Date), Location = "ICU", Age_group = Age_Group, ValueType = "Prevalence", Number = `Midnight covid +ve`)
  
  backdated_incidence = all[grep("backdated", all)] %>% 
    lapply(read_excel, sheet = "New Daily") %>% 
    bind_rows %>% 
    transmute(Date = ymd(Report_Date), Location = "ICU", Age_group = Age_Group, ValueType = "Incidence", Number = `New covid +ve admissions`)
  
  backdated = bind_rows(backdated_prevalence, backdated_incidence) 
  
  # Format the non-backdated files using the split_prevalence_and_incidence function we defined earlier.
  not_backdated = all[-grep("backdated", all)] %>% 
    lapply(read_excel) %>% 
    lapply(split_prevalence_and_incidence) %>% 
    bind_rows %>% 
    transmute(Date = Report_Date, Location = "ICU", Age_group = Age_Group, ValueType, Number = `Midnight covid +ve`) %>% 
    filter(!(is.na(Date) | Date == "Report_Date") ) %>% 
    mutate(Date = from_excel_date(Date), Number = as.numeric(Number) )
  
  
  # Combine backdated and non-backdated data.
  # Change the age groups to match our age groups.  Assume 2/3 of the 0-14 age group are 0-9 and 1/3 are 10-19.
  # Summarise over whatever variables we specified in ... when we called the function.
  bind_rows(backdated, not_backdated) %>% 
    # later data has duplicated days. This removes older versions
    mutate(row_number = 1:NROW(.)) %>% 
    arrange(desc(row_number)) %>% 
    distinct(Date, ValueType, Age_group, .keep_all = TRUE) %>% 
    arrange(row_number) %>% 
    select(-row_number) %>% 
    mutate(Age_group = ifelse(Age_group == "0-14", "00-09", Age_group),
           Number = ifelse(Age_group == "0-14", Number * 2/3, Number)
    ) %>% 
    bind_rows(bind_rows(backdated, not_backdated) %>% 
                filter(Age_group %in% c("0-14", "15-19") ) %>% 
                mutate(Number = ifelse(Age_group == "0-14", Number * 1/3, Number) ) %>% 
                group_by(Date, Location, ValueType) %>% 
                summarise(Number = sum(Number) ) %>% 
                ungroup %>% 
                mutate(Age_group = "10-19")
    ) %>% 
    filter(!(Age_group %in% c("15-19", "Unknown") ) ) %>% 
    mutate(Age_group = as.numeric(substr(Age_group, 1, 2) ) ) %>% 
    arrange(Date, ValueType, Age_group) %>% 
    group_by(Date, ValueType, ...) %>% 
    summarise(Number = sum(Number)) %>% 
    ungroup
}

update_ICU_time_series = function() { # in read_data.R
  read_sicsag_ICU() %>% 
    bind_rows(readRDS("Data/ICU_history.rds")) %>% 
    distinct(Date, ValueType, .keep_all = TRUE) %>% 
    arrange(Date, ValueType) %>% 
    saveRDS("Data/ICU_history.rds")
}

read_actual_admissions = function(frequency = "Daily", level="national", ICU_source = "published") { # in read_data.R
  
  hospital = "system_watch_hospital_admissions.csv" %>%
    get_filepath("Data") %>%
    read_csv %>%
    rename(HBName = X1) %>%
    mutate(HBName = HBName %>%
             str_replace("NHS ", "") %>%
             str_to_title %>%
             str_replace("&","and")) %>%
    gather(Date, Number, -HBName) %>%
    mutate(Date = as.Date(Date, format="%a %d %B %Y"),
           Number = Number %>% replace_na(0),
           Location = "Hospital")
  
  if(ICU_source == "published") {
    icu = "trend_hb.csv" %>% 
      get_filepath("Data") %>% 
      read_csv %>% 
      mutate(Date = Date %>% as.character %>% ymd) %>% 
      filter(HBName == "Scotland") %>% 
      select(Date, HospitalAdmissions, ICUAdmissions) %>%
      gather(Location, Number, -Date) %>% 
      mutate(Location = Location %>% str_remove("Admissions")) %>%
      filter(Location=="ICU")
  }
  else if(ICU_source == "sicsag") {
    icu = readRDS("Data/ICU_history.rds") %>% 
      filter(ValueType == "Incidence") %>% 
      select(-ValueType) %>% 
      mutate(Location = "ICU")
  }
  else stop("ICU_source must be 'published' or 'sicsag'")
  
  if(level == "national"){
    data = hospital %>%
      filter(HBName == "Scotland") %>%
      select(-HBName) %>% 
      bind_rows(icu)
    
  }else if(level=="Health Board"){
    data = hospital %>%
      filter(HBName != "Scotland")
    
  }
  data
}#

read_inpatients_data = function(sheet) { # in read_data.R
  
  #there are columns after the last date in the series. Some of these columns also have dates in the name
  #in order to identify the last column of the series, we extract the date from the creation date/time
  date = "C19-Inpatients and ICU Inpatients - New Definitions.xlsx" %>% 
    get_filepath("Data") %>% 
    read_excel(sheet = "C19 Inpatients in hosp confirm", range = "A22") %>% 
    names() %>% 
    str_extract("\\d+/\\d+/\\d+") %>% 
    dmy() %>% 
    to_excel_date() %>% 
    as.character()
  
  #read in the specified sheet
  df = "C19-Inpatients and ICU Inpatients - New Definitions.xlsx" %>% 
    get_filepath("Data") %>% 
    read_excel(sheet = sheet, skip = 4, n_max = 16) %>% 
    rename(Health_board = ...1)
  
  #find all the columns containing the creation date
  date_cols = df %>% names() %>% str_subset(date)
  
  df %>% 
    select("Health_board":date_cols[1]) %>% #select all columns up to the first containing the creation date
    pivot_longer(-Health_board, names_to = "Date", values_to = "Number") %>% 
    mutate(Date = Date %>% 
             str_remove("\\.\\.\\.\\d*") %>% 
             from_excel_date()) %>% 
    group_by(Health_board, Date) %>% 
    summarise(Number = mean(Number)) %>% 
    ungroup()
}

read_actual_concurrent = function(ICU_source = "published") { # in read_data.R
  inpatients = bind_rows(read_inpatients_data("C19 Inpatients in hosp confirm") %>% mutate(Location = "Hospital"),
                         read_inpatients_data("C19 Inpatients in ICU by board") %>% mutate(Location = "ICU"),
                         read_inpatients_data("C19 Inpatients in LongStay ICU ") %>% mutate(Location = "ICU")) %>% 
    filter(!Health_board %>% str_detect("Scotland")) %>% 
    group_by(Date, Location) %>% 
    summarise(Number = sum(Number)) %>% 
    ungroup()
  
  if(ICU_source == "published") {
    inpatients
  }
  else if(ICU_source == "sicsag") {
    inpatients %>% 
      filter(Location == "Hospital") %>% 
      bind_rows(readRDS("Data/ICU_history.rds") %>% 
                  filter(ValueType == "Prevalence") %>% 
                  select(-ValueType) %>% 
                  mutate(Location = "ICU"))
  }
  else stop("ICU_source must be 'published' or 'sicsag'")
}

split_prevalence_and_incidence = function(input_tibble) {
  # Format the non-backdated sicsag files to match the format used in read_actual_admissions and read_actual_concurrent.
  # Prevalence and incidence are in the same sheet, separated by blank lines.
  # Create a variable that's equal to 0 before the blank lines and >0 after, and use this to split the prevalence and incidence.
  input_tibble %>% 
    mutate(na_flag = ifelse(is.na(`Midnight covid +ve`), 1, 0),
           split_var = cumsum(na_flag),
           ValueType = ifelse(split_var == 0, "Prevalence", "Incidence")
    ) %>% 
    split(.$ValueType) %>% 
    bind_rows
}

read_actual_deaths = function(..., drop_recent = 4) { #in read_data.R
  age_group_dict = tibble(Age_group = c(0, 10, 10, 20, 20, 30,
                                        40, 40, 50, 60, 60,
                                        70, 70, 80, 80),
                          AgeGroup = c("0 to 14", "0 to 14", "15 to 19", "20 to 24", "25 to 44", "25 to 44", 
                                       "25 to 44", "45 to 64", "45 to 64", "45 to 64", "65 to 74",
                                       "65 to 74", "75 to 84",  "75 to 84", "85plus"),
                          Contribution = c(2/3, 1/3, 1, 1, 1/4, 1/2,
                                           1/4, 1/4, 1/2, 1/4, 1/2,
                                           1/2, 1/2, 1/2, 1))
  
  "trend_agesex.csv" %>% #from https://www.opendata.nhs.scot/dataset/covid-19-in-scotland
    get_filepath("Data") %>% 
    read_csv() %>% 
    filter(Sex == "Total", AgeGroup != "Total") %>% 
    left_join(age_group_dict, by = "AgeGroup") %>% 
    transmute(Date = Date %>% ymd,
              Age_group,
              `Cumulative deaths` = CumulativeDeaths * Contribution,
              `Daily deaths` = DailyDeaths * Contribution) %>%
    gather(Measure, Number, -Date, -Age_group) %>% 
    group_by(Date, Measure, ...) %>% 
    summarise(Number = sum(Number, na.rm = TRUE)) %>% 
    ungroup %>% 
    filter(Date <= max(Date) - drop_recent)
}
