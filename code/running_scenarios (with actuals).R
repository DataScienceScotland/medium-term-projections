filepath_priority = "development"
source("x.R")
import_priority = "local"



# AS: Uncomment this if you want to do a run for S gene positive only.
# get_historic_infections = function(method = "cases",
#          lag = 1,
#          detection_rate = 0.5,
#          baseline = 20000,
#          s_gene_status = "positive") { # in setup.R
#   if(method == "positivity") {
#     infections = get_LA_data("Negative") %>%
#       bind_rows(get_LA_data("Positive")) %>%
#       group_by(Date, Result) %>%
#       summarise(Tests = sum(Tests)) %>%
#       ungroup %>%
#       spread(Result, Tests) %>%
#       mutate(Prop_pos = Positive/(Positive+Negative)) %>%
#       filter(! Positive %>% is.na) %>%
#       select(-Negative) %>%
#       transmute(Date = Date - lag,
#                 Level = "Actual",
#                 Cases = Prop_pos*baseline)
#   }
#   else if(method == "cases") {
#     infections = get_LA_data("Positive") %>%
#       group_by(Date) %>%
#       summarise(Cases = sum(Tests)) %>%
#       filter(Date >= ymd("2020-08-01")) %>%
#       mutate(Date = Date - lag,
#              Level = "Actual",
#              Cases = Cases %>%
#                adjust_for_weekends %>%
#                `/`(detection_rate))
#   }
#   else stop("method must be either 'cases' or 'positivity'")
# 
#   if(s_gene_status == "all") infections
#   else if(s_gene_status %in% c("target failure", "positive")) {
#     get_s_gene_proportion(s_gene_status) %>%
#       mutate(Date = Date - lag) %>%
#       right_join(infections) %>%
#       mutate(Proportion = Proportion %>% replace_na(0),
#              Cases = Cases*Proportion) %>%
#       select(-Proportion) %>%
#       filter(Date >= "2020-11-15")
#   }
#   else stop("s_gene_status must be either 'all', 'target failure' or 'positive'")
# }



local_path = "x" %>% 
  str_c("x")



#Below code will run the logistics model from the start_date_of_forecast, 
#and then run the actuals through the logistics model and attach these two 
# parts together



#Start date of forecast should be set to the day after the latest day
#of infections actuals
start_date_of_forecast = get_historic_infections() %>%
  pull(Date) %>%
  max() + 1

# AS: Start date of hospitalisations should be set to the day after the latest day of hospitalisation actuals.
# We then cut off two days because the most recent days of hospitalisation actuals are unreliable.
start_date_of_hospitalisations = read_actual_admissions("Daily", ICU_source = "sicsag") %>%
  filter(Location == "Hospital") %>%
  pull(Date) %>%
  max() + 1 - 2

# AS: Uncomment this if you want to do a run for S gene positive only.
# start_date_of_hospitalisations = ymd("20211220")

# AS: Start date of ICU should be set to the day after the latest day of ICU actuals.
# IMPORTANT: Remember to copy across the SICSAG data from \\s0177a\datashare\CoMix\Public\SCIRe\Data\Sicsag 
# before running this on your C drive.
update_ICU_time_series()

start_date_of_ICU = read_actual_admissions("Daily", ICU_source = "sicsag") %>%
  filter(Location == "ICU") %>%
  pull(Date) %>%
  max() +1

# AS: Uncomment this if you want to do a run for S gene positive only.
# start_date_of_ICU = ymd("20211220")



#attach actuals
totals_from_actuals = include_totals_from_actuals(start_date_of_forecast, start_date_of_hospitalisations, start_date_of_ICU)
gc()


#Save the actuals with a sensible name
totals_from_actuals %>% 
  saveRDS("Outputs/raw/x.rds")



scenarios = c("Better", "Central", "Worse")
levels = c("5th percentile", "25th percentile", "Median", "75th percentile", "95th percentile")



#we don't directly use the output of this, but it saves the outputs for each scenario, which is then later used
# AS: We need to give this start_date_of_forecast as an argument so that it cuts off estimates before then.
scenarios %>% 
  map(simulate_epi_curve, start_date_of_forecast)

# So get the data for dates before this from the history.rds file.
map(scenarios,
    ~ readRDS(str_c("SCHEMa outputs/", .x, " sim_output.rds") ) %>% 
      bind_rows("history.rds" %>% 
                  get_filepath %>% 
                  readRDS %>% 
                  filter(Date < "2020-08-02")
               ) %>% 
      saveRDS(str_c("SCHEMa outputs/", .x, " sim_output.rds") )
   )


gc()
apply_vaccine = FALSE

run_weekly_update(scenarios, levels, local_path)

asymp = readRDS(local_path %>% str_c("scotland_estimates.rds")) %>% 
  filter(Status == "Asymptomatic")
# asymp_prop = readRDS(local_path %>% str_c("proportions.rds")) %>% 
#   filter(Status == "Asymptomatic")

asymp %>% 
  saveRDS("Outputs/raw/asymp.rds")
#rm(asymp)

apply_vaccine = TRUE

gc()
run_weekly_update(scenarios, levels, local_path)

other = readRDS(local_path %>% str_c("scotland_estimates.rds")) %>% 
  filter(Status != "Asymptomatic")
# other_prop = readRDS(local_path %>% str_c("proportions.rds")) %>% 
#   filter(Status != "Asymptomatic")

#asymp <- readRDS("Outputs/raw/asymp.rds")

scotland_estimates = asymp %>% 
  bind_rows(other) %>% 
  filter(!is.na(Scenario_name) )

#Save the forecasts with a sensible name
scotland_estimates %>% 
  saveRDS("Outputs/raw/x.rds")
gc()

totals_from_actuals <- readRDS("Outputs/raw/x.rds")

#Join the projections from actuals to the projections from projected cases
# AS: Create copies of totals_from_actuals for all scenarios and levels. The rownames are added 
# so that duplicate rows from infections and hospitalisations aren't dropped in the copies.
message("Joining forecasts to actuals")
scotland_estimates_with_actuals = scotland_estimates %>% 
  # AS: Copy the 5th and 95th percentiles for all scenarios.
  bind_rows(crossing(scotland_estimates %>% 
                       select(ID, Scenario_name, Level) %>% 
                       unique
                    ) %>% 
            right_join(totals_from_actuals %>% 
                       filter(Level %in% c("5th percentile", "95th percentile") ) %>% 
                       add_rownames %>% 
                       select(-ID, -Scenario_name)
                      ) %>% 
            select(-rowname)
           ) %>%
  # AS: Copy the Median for all scenarios and levels other than the 5th and 95th percentiles.
  # We need to add a dummy variable to join on (for some reason crossing doesn't work).
  bind_rows(crossing(scotland_estimates %>% 
                       select(ID, Scenario_name, Level) %>% 
                       unique %>% 
                       filter(!Level %in% c("5th percentile", "95th percentile") ) %>% 
                       mutate(dummy = 1)
                    ) %>% 
            full_join(totals_from_actuals %>% 
                      filter(Level == "Median") %>% 
                      add_rownames %>% 
                      select(-ID, -Scenario_name, -Level) %>% 
                      mutate(dummy = 1)
                     ) %>% 
            select(-rowname, -dummy)
           ) %>% 
  arrange(ID, Scenario_name, Level, Date) %>% 
  # Uncomment this if you need to see where a row in scotland_estimates_with_actuals comes from.
  mutate(source = ifelse(is.na(source), "forecast", source) )



#Save the combined actuals and forecasts with a sensible name
scotland_estimates_with_actuals %>% 
  #edit_status("Dead", 1.4, 1.4) %>% 
  saveRDS("Outputs/raw/x.rds")

gc()





# Create the output file that goes to SPI-M.
export_SPI_forecasts = function(model_version,
                                base_date = today() %>% floor_date("weeks"),
                                estimates = NULL) { #in create_products.R
  
  if(estimates %>% is_null) {
    estimates = readRDS("Outputs/raw/scotland_estimates.rds")
  }
  
  forecasts = estimates %>% 
    prep_SPI_forecast(base_date, base_date+69, model_version = model_version) %>%
    mutate(`Quantile 0.05` = ValueType %>%
             recode(infections_inc = `Quantile 0.05` %>% adjust_lower_bound(5, 10),
                    hospital_prev = `Quantile 0.05` %>% adjust_lower_bound(5, 10),
                    hospital_inc = `Quantile 0.05` %>% adjust_lower_bound(5, 10),
                    icu_prev = `Quantile 0.05` %>% adjust_lower_bound(5, 10),
                    icu_inc = `Quantile 0.05` %>% adjust_lower_bound(5, 10),
                    type28_death_inc_line = `Quantile 0.05` %>% adjust_lower_bound(5, 10)),
           `Quantile 0.25` = ValueType %>%
             recode(infections_inc = `Quantile 0.25` %>% adjust_lower_bound(5, 10),
                    hospital_prev = `Quantile 0.25` %>% adjust_lower_bound(5, 10),
                    hospital_inc = `Quantile 0.25` %>% adjust_lower_bound(5, 10),
                    icu_prev = `Quantile 0.25` %>% adjust_lower_bound(5, 10),
                    icu_inc = `Quantile 0.25` %>% adjust_lower_bound(5, 10),
                    type28_death_inc_line = `Quantile 0.25` %>% adjust_lower_bound(5, 10)),
           `Quantile 0.75` = ValueType %>%
             recode(infections_inc = `Quantile 0.75` %>% adjust_upper_bound(5, 10),
                    hospital_prev = `Quantile 0.75` %>% adjust_upper_bound(5, 10),
                    hospital_inc = `Quantile 0.75` %>% adjust_upper_bound(5, 10),
                    icu_prev = `Quantile 0.75` %>% adjust_upper_bound(5, 10),
                    icu_inc = `Quantile 0.75` %>% adjust_upper_bound(5, 10),
                    type28_death_inc_line = `Quantile 0.75` %>% adjust_upper_bound(5, 10)),
           `Quantile 0.95` = ValueType %>%
             recode(infections_inc = `Quantile 0.95` %>% adjust_upper_bound(5, 10),
                    hospital_prev = `Quantile 0.95` %>% adjust_upper_bound(5, 10),
                    hospital_inc = `Quantile 0.95` %>% adjust_upper_bound(5, 10),
                    icu_prev = `Quantile 0.95` %>% adjust_upper_bound(5, 10),
                    icu_inc = `Quantile 0.95` %>% adjust_upper_bound(5, 10),
                    type28_death_inc_line = `Quantile 0.95` %>% adjust_upper_bound(5, 10)))
  
  export_SPI_forecast(table = forecasts)
}

export_SPI_forecasts(model_version=2.11,estimates=scotland_estimates_with_actuals %>%
                       filter(Scenario_name %in% c("Central"), Status != "Recovering"))
