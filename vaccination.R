get_vaccine_history = function() { #in vaccination.R
  "x" %>%
    list.files() %>%
    str_subset("JCVI Administered") %>%
    str_remove("JCVI Administered ") %>%
    str_remove(".xlsx") %>%
    dmy %>%
    sort %>%
    tail(1) %>%
    format("%d%m%y") %>%
    str_c("x ", ., ".xlsx") %>%
    read_excel(sheet = "DHSC Administered") %>%
    transmute(Date = dose_date %>% ymd,
              Age_group = (dhscagegroup %>% str_extract("\\d\\d") %>% as.integer) %/% 10 * 10,
              Dose = dose %>% str_remove("dose_") %>% as.integer(),
              Product = vacc_product_name %>% str_extract("(Pfizer|AstraZeneca|Moderna)"),
              Local_authority = local_authority,
              Vaccinated = administered) %>% 
    filter(! Date %>% is.na)
}

get_vaccine_roll_out = function(start_date) { #in vaccination.R
  "x" %>%
    list.files() %>%
    str_subset("SGORR vaccine output") %>%
    str_remove("SGORR vaccine output_") %>%
    str_remove(".xlsx") %>%
    ymd %>%
    sort %>%
    tail(1) %>%
    format("%y%m%d") %>%
    str_c("x", ., ".xlsx") %>%
    read_excel(skip = 2, col_types = "text") %>%
    rename(Dose = 1, Vaccine = 2) %>%
    filter(! Vaccine %>% is.na, ! Vaccine %in% c("Total", "Cumulative")) %>%
    fill(Dose) %>%
    gather(Date, Number, -Dose, -Vaccine) %>%
    transmute(Date = Date %>% from_excel_date %>% ymd,
              Dose = Dose %>% str_remove("Dose ") %>% as.numeric,
              Product = Vaccine,
              Vaccinated = Number %>% as.numeric) %>%
    filter(Date >= start_date)
}

add_vaccine = function(phases) { #in vaccination.R
  dates_dict = phases %>% 
    transmute(start_date = start_date %>% ymd,
              end_date = end_date %>% ymd,
              Date = map2(start_date, end_date, seq, by = "days")) %>% 
    unnest
  
  efficacy_assumptions = "Assumptions - Vaccine effectiveness.csv" %>%
    get_filepath("Data") %>%
    read_csv
  
  past_vaccine_data = get_vaccine_history()
  
  future_vaccine_data = get_vaccine_roll_out(max(past_vaccine_data$Date) + 1)
  
  all_vaccine_data = past_vaccine_data %>%
    bind_rows(future_vaccine_data)
  
  vaccine_phases = all_vaccine_data %>% 
    # In the period we're modelling the immune-compromised have all already been vaccinated, so use the non-immune-compromised efficacy assumptions.
    right_join(efficacy_assumptions %>% filter(Immune_compromised == "No") ) %>% 
    mutate(Date = Date + Lag) %>% 
    left_join(dates_dict) %>% 
    group_by(start_date) %>%
    summarise(vaccinations = sum(Vaccinated),
              sterilisation_efficacy = sum(Vaccinated*Sterilisation)/sum(Vaccinated))
  
  phases %>%
    mutate(start_date = start_date %>% ymd, end_date = end_date %>% ymd) %>%
    left_join(vaccine_phases) %>% 
    mutate(vaccinations = vaccinations %>% replace_na(0) %>% as.integer,
           sterilisation_efficacy = (sterilisation_efficacy*efficacy_adjustment) %>% replace_na(0))
}

#does not yet account for waning immunity, and boosters
get_next_dose_1 = function(df, product, new_vaccinations, takeup, population) { #in vaccination.R
  df %>% 
    group_by(Age_group) %>% 
    summarise(Vaccinated = sum(Vaccinated)) %>% 
    ungroup %>% 
    left_join(population) %>% 
    left_join(takeup) %>% 
    transmute(Age_group, Outstanding = (Population*Takeup - Vaccinated) %>% map_dbl(max, 0)) %>% 
    crossing(new_vaccinations %>% 
               filter(Dose == 1,
                      Product == product)) %>% 
    rename(Product = Product) %>% 
    arrange(desc(Age_group), Product) %>% 
    group_by(Product) %>% 
    mutate(Used = cumsum(Outstanding) - Outstanding, 
           Delivered = map2_dbl(Vaccinated-Used, Outstanding, min)) %>%
    ungroup %>% 
    filter(Delivered >= 0) %>% 
    arrange(Product, Age_group) %>% 
    select(Date, Dose, Product, Age_group, Vaccinated = Delivered) %>% 
    bind_rows(df,.)
}

#does not yet account for waning immunity, and boosters
get_next_vaccine = function(vaccination_history, new_vaccinations, takeup, population = get_population(Age_group)) { #in vaccination.R
  new_dose_2 = vaccination_history %>% 
    group_by(Dose, Product, Age_group) %>% 
    summarise(Vaccinated = sum(Vaccinated)) %>% 
    spread(Dose, Vaccinated) %>% 
    ungroup %>% 
    transmute(Product, Age_group, Outstanding = `1` -`2`) %>% 
    left_join(new_vaccinations %>% 
                filter(Dose == 2), 
              by = c("Product" = "Product")) %>% 
    arrange(Product, desc(Age_group)) %>% 
    group_by(Product) %>% 
    mutate(Used = cumsum(Outstanding) - Outstanding, 
           Delivered = map2_dbl(Vaccinated-Used, Outstanding, min)) %>%
    ungroup %>% 
    filter(Delivered > 0) %>% 
    arrange(Product, Age_group) %>% 
    select(Date, Dose, Product, Age_group, Vaccinated = Delivered)
  
  dose_1_history = c("Pfizer", "AstraZeneca", "Moderna") %>% 
    reduce(get_next_dose_1, 
           new_vaccinations,
           takeup,
           population,
           .init = vaccination_history %>% filter(Dose == 1))
  
  dose_1_history %>% 
    bind_rows(vaccination_history %>% filter(Dose == 2),
              new_dose_2)
}

estimate_roll_out_by_age = function(past_vaccine_data, future_vaccine_data) { #in vaccination.R
  takeup = "Assumptions - Vaccine takeup.csv" %>%
    get_filepath("Data") %>% 
    read_csv() %>% 
    transmute(Age_group = `Lower end age`, Takeup = Value)
  
  population = get_population(Age_group)
  
  future_vaccine_data %>% 
    mutate(date = Date) %>% 
    nest(-date) %>% 
    pull(data) %>%  
    reduce(get_next_vaccine,
           takeup,
           population,
           .init = past_vaccine_data %>% 
             filter(! Age_group %>% is.na) %>% 
             group_by(Date, Dose, Product, Age_group) %>% 
             summarise(Vaccinated = sum(Vaccinated)) %>% 
             ungroup)
}

get_immunity_data = function(all_vaccine_data,
                             efficacy_adjustments = tibble(Date = ymd("2000-01-01"), 
                                                           efficacy_adjustment = as.double(NA))) { #in vaccination.R
  vaccine_assumptions = "Assumptions - Vaccine effectiveness.csv" %>%
    get_filepath("Data") %>%
    read_csv
  
  vac_immunity_time = get_immunity_time("vaccine")
  
  dose1_assumptions = vaccine_assumptions %>% filter(Dose != 2) %>% select(-Dose)
  
  dose2_assumptions = vaccine_assumptions %>%
    group_by(Product, Immune_compromised) %>% 
    summarise_at(c("Sterilisation", "Protection"),
                 sum) %>%
    ungroup %>% 
    left_join(vaccine_assumptions %>% filter(Dose == 2) %>% select(Product, Lag, Immune_compromised))
  
  dose1_immunity = all_vaccine_data %>%
    filter(Dose == 1) %>% 
    left_join(dose1_assumptions %>% 
                filter(Immune_compromised == "No")
    ) %>%
    bind_rows(all_vaccine_data %>%
                filter(Dose == 2) %>% 
                left_join(dose1_assumptions %>%
                            filter(Immune_compromised == "No") %>% 
                            mutate_at(c("Sterilisation", "Protection"), `*`, -1)
                )
    ) %>% 
    mutate(Date = Date + Lag) %>%
    arrange(Date) %>%
    group_by(Date, Age_group) %>%
    summarise(Sterilised = sum(Vaccinated * Sterilisation), 
              Protected = sum(Vaccinated * Protection),
              Vaccinated = sum(Vaccinated)) %>%
    ungroup %>%
    complete(Date = c(Date, max(Date, na.rm = TRUE) + 7*(1:13)), Age_group, fill = list(Sterilised = 0, Protected = 0, Vaccinated = 0)) %>%
    group_by(Age_group) %>%
    mutate(Immune = Sterilised %>% cumsum,
           Safe = Protected %>% cumsum) %>% 
    ungroup %>% 
    left_join(efficacy_adjustments) %>% 
    group_by(Age_group) %>% 
    fill(efficacy_adjustment) %>% 
    ungroup %>% 
    replace_na(list(efficacy_adjustment = 1)) %>% 
    mutate(Immune = Immune * efficacy_adjustment,
           Safe = Safe * efficacy_adjustment,
           Dose = 1)
  
  # Calculate vaccine effectiveness for immune-compromised people.
  # In the absence of data, assume that the proportion receiving each type of vaccine is the same as for the
  # total population in March 2021, as all the immune-compromised will have received their first dose by then.
  vaccine_proportions = all_vaccine_data %>% 
    filter(Date <= "2021-03-31" & Dose == 1 & !is.na(Product) ) %>% 
    group_by(Product) %>% 
    summarise(Vaccinated = sum(Vaccinated) ) %>% 
    ungroup %>% 
    mutate(Total = sum(Vaccinated),
           Proportion = Vaccinated / Total
    ) %>% 
    select(Product, Proportion)
  
  immune_compromised = "Assumptions - Vaccine effectiveness.csv" %>%
    get_filepath("Data") %>%
    read_csv %>% 
    group_by(Product, Immune_compromised) %>% 
    summarise(Sterilisation = sum(Sterilisation),
              Protection = sum(Protection)
    ) %>% 
    ungroup %>% 
    left_join(vaccine_proportions) %>% 
    replace_na(list(Proportion = 0) ) %>% 
    group_by(Immune_compromised) %>% 
    summarise(Sterilisation = sum(Sterilisation * Proportion),
              Protection = sum(Protection * Proportion)
    ) %>% 
    ungroup %>% 
    # We need to take out the immune-compromised from the current Immune and Safe numbers.
    # To do this we need to calculate how much they're contributing at the moment, using the non-immune-compromised effectiveness.
    crossing("Assumptions - Immune-compromised - Sources.xlsx" %>%
               get_filepath("Data") %>%
               read_excel(sheet = "Immune-Compromised Population", range = "A22:B31") %>%
               filter(Age_group != 0)
    ) %>% 
    transmute(Immune_compromised,
              Age_group,
              Immune = Immune_compromised_population * Sterilisation,
              Safe = Immune_compromised_population * Protection
    ) %>% 
    # Rearrange so immune-compromised and non-immune-compromised are on the same line.
    gather("key", "value", Immune, Safe) %>% 
    mutate(key = str_c(Immune_compromised, key, sep = "_") ) %>% 
    select(-Immune_compromised) %>% 
    spread(key, value)
  
  dose2_immunity_temp = all_vaccine_data %>%
    filter(Dose == 2) %>% 
    left_join(dose2_assumptions %>% 
                filter(Immune_compromised == "No")
    ) %>%
    mutate(Date = Date + Lag) %>%
    arrange(Date) %>%
    group_by(Date, Age_group) %>%
    summarise(Sterilised = sum(Vaccinated * Sterilisation), 
              Protected = sum(Vaccinated * Protection),
              Vaccinated = sum(Vaccinated)) %>%
    ungroup %>%
    complete(Date = c(Date, max(Date, na.rm = TRUE) + 7*(1:13)), Age_group, fill = list(Sterilised = 0, Protected = 0, Vaccinated = 0)) %>%
    group_by(Age_group) %>%
    mutate(Immune = Sterilised %>% rollsum(vac_immunity_time),
           Safe = Protected %>% rollsum(vac_immunity_time)) %>% 
    ungroup %>% 
    # Separate the Immune and Safe populations by whether they're immune-compromised or not, assuming all immune-compromised people are double vaccinated by the end of June 2021.
    left_join(immune_compromised) %>% 
    mutate(Immune_compromised = "No",
           Immune = ifelse(Date >= "2021-07-01", Immune - No_Immune, Immune),
           Safe = ifelse(Date >= "2021-07-01", Safe - No_Safe, Safe)
    )
  
  dose2_immunity = dose2_immunity_temp %>% 
    bind_rows(dose2_immunity_temp %>% 
                filter(Date >= "2021-07-01") %>% 
                mutate(Immune_compromised = "Yes",
                       Immune = Yes_Immune,
                       Safe = Yes_Safe
                )
    ) %>% 
    select(-No_Immune, -No_Safe, -Yes_Immune, -Yes_Safe) %>% 
    # End of immune-compromised section.
    left_join(efficacy_adjustments) %>% 
    group_by(Age_group) %>% 
    fill(efficacy_adjustment) %>% 
    ungroup %>% 
    replace_na(list(efficacy_adjustment = 1)) %>% 
    mutate(Immune = Immune * efficacy_adjustment,
           Safe = Safe * efficacy_adjustment,
           Dose = 2)
  
  dose1_immunity %>% 
    mutate(Immune_compromised = "No") %>% 
    bind_rows(dose2_immunity) %>% 
    group_by(Date, Age_group, Immune_compromised) %>% 
    summarise_at(c("Sterilised", "Protected", "Vaccinated", "Immune", "Safe"), sum) %>% 
    ungroup
}

prep_vaccine_chart = function(vaccine_data, ...) { #in vaccination.R
  vaccine_data %>% 
    ggplot(aes(Date, 
               ..., 
               fill = Age_group %>% 
                 str_c(Age_group+9, sep="-") %>% 
                 recode(`80-89` = "80+"))) + 
    geom_bar(stat="identity", position = position_stack(), width = 7) + 
    theme_light() + 
    labs(fill = "Age") +
    scale_fill_viridis_d(option = "A") +
    scale_y_continuous(labels = comma) + 
    geom_hline(yintercept = 5463300, linetype = "dotted", size = 0.9)
}
