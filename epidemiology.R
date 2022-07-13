#calculates the number of new cases for the next day, including imported cases and new infections
get_next = function(new_cases, # integer vector - history of daily new cases
                    forecast_day, # integer - not used, just a bin for the increment counter
                    population, # integer - total population
                    r0, # double - average R0 for day in question
                    infectious_time_mean, # integer - number of days a person remains infectious
                    natural_immunity_time = 10000, # lnumber of days that immunity lasts
                    vaccine_immunity_time = 10000,
                    var = 0, # double - variance of the natural log of the distribution of beta
                    general_in = 0, #integer - number of general travellers arriving on day
                    prop_general_infected = 0, #double - proportion of general travellers infected
                    special_in = 0, #integer - number of special travellers arriving on day
                    prop_special_infected = 0, #double - proportion of special travellers infected
                    vaccinations = 0, #integer - number of people vaccinated so far
                    efficacy = 1,
                    efficacy_adjustment = 1,
                    natural_efficacy_adjustment = 1,
                    phase_length = 7)  {
  if(natural_immunity_time < infectious_time_mean)
    stop("You've set infectious time to be longer than length of immunity, which is silly")
  if(general_in %% 1 > 0) {
    general_in = general_in %>% round
  }
  if(special_in %% 1 > 0) {
    special_in = special_in %>% round
  }
  if(! "Nat_immunities" %in% names(new_cases)) {
    new_cases = new_cases %>% mutate(Nat_immunities = Cases, Vac_immunities = 0)
  }
  
  unsusceptible_through_infection = natural_efficacy_adjustment*(new_cases$Nat_immunities %>% tail(natural_immunity_time) %>% sum)
  
  new_vac_immunities = (rbinom(1, vaccinations, efficacy)/phase_length) %>% as.integer
  
  unsusceptible_through_vaccination = efficacy_adjustment*(new_cases$Vac_immunities %>% tail(vaccine_immunity_time) %>% sum)
  
  susceptible = population-unsusceptible_through_infection - unsusceptible_through_vaccination
  
  currently_infectious = new_cases$Cases %>% tail(infectious_time_mean) %>% sum
  #estimates a variance for R0. The fewer people are infected the higher the variance
  r0_var = get_r0_var(r0, var, currently_infectious)
  #gets a random R0, assuming a normal distribution (but with a minimum of 0)
  r0_today = rnorm(1, r0, sqrt(r0_var)) %>% max(0)
  beta = r0_today/infectious_time_mean
  infected = currently_infectious * beta * susceptible/population
  imported = rbinom(1, general_in, prop_general_infected) + rbinom(1, special_in, prop_special_infected)
  new_cases %>%
    mutate(Nat_immunities = Nat_immunities * (1 - new_vac_immunities / (susceptible + unsusceptible_through_infection))) %>% 
    bind_rows(tibble(Cases = infected + imported,
                     R = r0_today * susceptible/population,
                     Nat_immunities = Cases,
                     Vac_immunities = new_vac_immunities,
                     Nat_immune = unsusceptible_through_infection,
                     Vac_immune = unsusceptible_through_vaccination))
}

#given an initial vector of daily new cases, complete it for the given forecast horizon
extend_forecast = function(new_cases,
                           r0, # double - average R0 for day in question
                           horizon, # integer - number of days to forecast
                           population, # integer - total population
                           infectious_time_mean = 12, # integer - number of days a person remains infectious
                           natural_immunity_time = 10000,
                           vaccine_immunity_time = 10000, # lnumber of days that immunity lasts
                           var = 0, # double - variance of the natural log of the distribution of beta
                           general_in = 0, #integer - number of general travellers arriving on day
                           prop_general_infected = 0, #double - proportion of general travellers infected
                           special_in = 0, #integer - number of special travellers arriving on day
                           prop_special_infected = 0, #double - proportion of special travellers infected
                           vaccinations = 0, #integer - number of people vaccinated so far
                           efficacy = 1,
                           efficacy_adjustment = 1,
                           natural_efficacy_adjustment = 1) { #double - proportion of vaccines that are effective
  #new_cases = new_cases %>% filter(! Cases %>% is.na)
  c(1:horizon) %>%
    reduce(get_next,
           population = population,
           r0 = r0,
           infectious_time_mean = infectious_time_mean,
           natural_immunity_time = natural_immunity_time,
           vaccine_immunity_time = vaccine_immunity_time,
           var = var,
           general_in = general_in,
           prop_general_infected = prop_general_infected,
           special_in = special_in,
           prop_special_infected = prop_special_infected,
           vaccinations = vaccinations,
           efficacy = efficacy,
           efficacy_adjustment = efficacy_adjustment,
           natural_efficacy_adjustment = natural_efficacy_adjustment,
           phase_length = horizon,
           .init = new_cases)
}

#compare this forecasting model to the forecasts from the full model 
forecast_new_cases = function(estimates, horizon, testing=TRUE) {
  test_forecast_start = estimates %>%
    rename(Scotland = 3) %>%
    filter(Scotland %>% is.na) %>%
    slice(1) %>%
    .$X1
  
  forecast_start = (estimates$X1 %>% max)+1
  
  combined_estimates = estimates %>%
    extend_scenario(test_forecast_start, forecast_start - test_forecast_start) %>% 
    gather(var, estimate, -X1) %>%
    separate(var,c("Type","Level"),sep="_")
  
  if(testing) {
    xx<<-combined_estimates
    yy <<- estimates %>%
      extend_scenario(min(forecast_start-1, 103), max(1, forecast_start - 103)) %>%
      gather(var, estimate, -X1) %>%
      separate(var,c("Type","Level"),sep="_")
  }
  
  #filter the overlap in estimates
  combined_estimates_overlap = combined_estimates %>%
    spread(Type, estimate) %>%
    filter(X1 >= test_forecast_start)
  
  #rmse
  message("Root mean square error between IC model output and new forecasts is:")
  combined_estimates_overlap %>%
    mutate(diff_squared = (predicted - approx)^2) %>%
    group_by(Level) %>% 
    summarise(rmse = (mean(diff_squared))^(1/2)) %>%
    print
  
  #t-test. (If p value < 0.05 then null hypothesis is unlikely)
  message("t test for cases")
  t.test((combined_estimates_overlap %>% filter(Level=="cases"))$predicted,
         (combined_estimates_overlap %>% filter(Level=="cases"))$approx) %>%
    print
  
  message("t test for max")
  t.test((combined_estimates_overlap %>% filter(Level=="max"))$predicted,
         (combined_estimates_overlap %>% filter(Level=="max"))$approx) %>%
    print
  
  message("t test for min")
  t.test((combined_estimates_overlap %>% filter(Level=="min"))$predicted,
         (combined_estimates_overlap %>% filter(Level=="min"))$approx) %>%
    print
  
  population = get_population() %>% 
    unlist %>% 
    unname
  
  date_day_1 = estimates %>% 
    filter(X1==1) %>% 
    .$time %>% 
    parse_date_time(c("ymd", "dmy")) %>% 
    ymd
  
  max_rt = estimates %>% 
    slice(forecast_start:NROW(.)) %>% 
    .$rt_max %>% 
    max
  
  if(max_rt > 1 & testing) {
    immune_at_max = estimates %>% 
      slice(forecast_start-1) %>% 
      .$predicted_max_cum
    
    horizon = round(96 * log(population/immune_at_max*(1-1/max_rt) + 1/max_rt))
    print(horizon)
  }
  
  curve_estimates = estimates %>% 
    slice(1:(forecast_start-1)) %>%
    bind_rows(tibble(X1 = seq.int(forecast_start, length.out = horizon))) %>%
    extend_scenario(forecast_start, horizon)
  
  if(testing) {
    zz<<-curve_estimates%>%
      gather(var, estimate, -X1) %>%
      separate(var,c("Type","Level"),sep="_")
  }
  curve_estimates %>% 
    transmute(Day = X1,
              Date = date_day_1 + Day - 1,
              predicted_max = approx_max,
              predicted_cases = approx_cases,
              predicted_min = approx_min)
}

extend_scenario = function(epi_model_output, forecast_start, horizon) {
  population = get_population() %>% unlist %>% unname
  
  state = epi_model_output %>% 
    slice(1:(forecast_start-1)) %>% 
    summarise(rt = last(rt),
              rt_min = last(rt_min),
              rt_max = last(rt_max),
              predicted_cases = sum(predicted_cases),
              predicted_max = sum(predicted_max),
              predicted_min = sum(predicted_min))
  
  r0=state$rt * population / (population-state$predicted_cases)
  r0_max=state$rt_max * population / (population-state$predicted_max)
  r0_min=state$rt_min * population / (population-state$predicted_min)
  
  combined_estimates = epi_model_output %>%
    transmute(approx_cases = ifelse(X1<forecast_start, predicted_cases, NA) %>% 
                extend_forecast(r0, horizon, population),
              approx_max = ifelse(X1<forecast_start, predicted_max, NA) %>%
                extend_forecast(r0_max, horizon, population),
              approx_min = ifelse(X1<forecast_start, predicted_min, NA) %>%
                extend_forecast(r0_min, horizon, population),
              X1, predicted_cases, predicted_min, predicted_max)
}

#run forecast_phase for each row in phases
run_sim = function(ID,
                   past_range,
                   phases,
                   var=0,
                   population = get_population() %>% pull(Population),
                   natural_immunity_time = 10000,
                   vaccine_immunity_time = 10000) { #in epidemiology.R
  set.seed(423756 + 843*ID)
  initial_state = past_range %>%
    filter(sim_id == ID)
  phases %>%
    nest(-name) %>%
    pull(data) %>%
    reduce(forecast_phase, 
           var=var, 
           population=population, 
           natural_immunity_time = natural_immunity_time,
           vaccine_immunity_time = vaccine_immunity_time, 
           .init = initial_state) %>%
    mutate(sim_id = ID)
}

get_initial_state = function(past_range) {
  past_range %>% 
    group_by(Date) %>% 
    summarise(Cases = sample(Cases, 1))
}

#for the parameters in a given row in the phases table (i.e. a given phase), run forecast_sim
forecast_phase = function(df, 
                          parameters, 
                          var=0, 
                          population=get_population() %>% pull(Population),
                          natural_immunity_time = 10000,
                          vaccine_immunity_time = 10000) { #in epidemiology.R
  parameters %>% 
    pmap_dfr(forecast_sim, 
             df=df, 
             var=var, 
             population=population, 
             natural_immunity_time = natural_immunity_time,
             vaccine_immunity_time = vaccine_immunity_time)
}

#given a dataframe of historic cases, complete it up to the given end date
forecast_sim = function(df,
                        low_r0,
                        mid_r0,
                        high_r0,
                        end_date,
                        start_date,
                        infectious_time_mean = 12, # integer - number of days a person remains infectious
                        natural_immunity_time = 10000,
                        vaccine_immunity_time = 10000, # lnumber of days that immunity lasts
                        var = 0, # double - variance of the natural log of the distribution of beta
                        general_in = 0, #integer - number of general travellers arriving on day
                        prop_general_infected = 0, #double - proportion of general travellers infected
                        special_in = 0, #integer - number of special travellers arriving on day
                        prop_special_infected = 0, #double - proportion of special travellers infected
                        vaccinations = 0, #integer - number of people vaccinated so far
                        # vaccinated_oldest = 0,
                        # vaccinated_older = 0,
                        sterilisation_efficacy = 1, #double - proportion of vaccines that are effective
                        efficacy_adjustment=1,
                        natural_efficacy_adjustment = 1,
                        population = get_population() %>% pull(Population)) { #in epidemiology.R
  r0 = rdist(low_r0,mid_r0,high_r0)
  horizon = (end_date-start_date+1) %>% as.integer
  df %>% select(-Date) %>%
    extend_forecast(population = population,
                    r0 = r0,
                    horizon = horizon,
                    infectious_time_mean = infectious_time_mean,
                    natural_immunity_time = natural_immunity_time,
                    vaccine_immunity_time = vaccine_immunity_time,
                    var = var,
                    general_in = general_in,
                    prop_general_infected = prop_general_infected,
                    special_in = special_in,
                    prop_special_infected = prop_special_infected,
                    vaccinations = vaccinations,
                    efficacy = sterilisation_efficacy,
                    efficacy_adjustment = efficacy_adjustment,
                    natural_efficacy_adjustment = natural_efficacy_adjustment) %>%
    mutate(Date = df$Date %>% c(max(df$Date) + 1:horizon))
}

# this is what I've plumped for as means to generate sensible randomness in R0
# it assumes individual infected people's beta (and therefore R0) follows a log-normal distribution
# "var" in this code refers to the variance of the natural log of the distribution of beta

# transformed_var estimates the variance of the normal distribution generated for R0...
# ...when n_infected people's R0 values are averaged
get_r0_var = function(r0, var, n_infected, ...) { #in epidemiology.R
  sqrt(3) * r0^2 * var^(3/2) / n_infected
}

rtri = function(min=0, mid=0.5, max=1) { #in epidemiology.R
  F = (mid-min)/(max-min)
  U = runif(1, 0, 1)
  if(min == max) min
  else if(U<F) min + sqrt(U*(max-min)*(mid-min))
  else max - sqrt((1-U)*(max-min)*(max-mid))
}

rdist = function(min=0, mid=0.5, max=1) { #in epidemiology.R
  runif(1, min, max)
}

extend_forecast_multi = function(new_cases,
                                 r0, # double - average R0 for day in question
                                 horizon, # integer - number of days to forecast
                                 population, # integer - total population
                                 infectious_time_mean = 12, # integer - number of days a person remains infectious
                                 immunity_time = 10000, # lnumber of days that immunity lasts
                                 var = 0, # double - variance of the natural log of the distribution of beta
                                 general_in = 0, #integer - number of general travellers arriving on day
                                 prop_general_infected = 0, #double - proportion of general travellers infected
                                 special_in = 0, #integer - number of special travellers arriving on day
                                 prop_special_infected = 0, #double - proportion of special travellers infected
                                 vaccinations = 0, #integer - number of people vaccinated so far
                                 efficacy = 1) { #double - proportion of vaccines that are effective
  
  #new_cases = new_cases %>% filter(! Cases %>% is.na) 
  
  1:horizon %>% 
    reduce(get_next_multi,
           population = population,
           r0 = r0,
           infectious_time_mean = infectious_time_mean,
           immunity_time = immunity_time,
           var = var,
           general_in = general_in,
           prop_general_infected = prop_general_infected,
           special_in = special_in,
           prop_special_infected = prop_special_infected,
           vaccinations = vaccinations,
           efficacy = efficacy,
           .init = new_cases)
}

get_next_multi = function(new_cases, # integer vector - history of daily new cases
                          forecast_day, # integer - not used, just a bin for the increment counter
                          population, # integer - total population
                          r0, # double - average R0 for day in question
                          infectious_time_mean, # integer - number of days a person remains infectious
                          immunity_time = 10000, # lnumber of days that immunity lasts
                          var = 0, # double - variance of the natural log of the distribution of beta
                          general_in = 0, #integer - number of general travellers arriving on day
                          prop_general_infected = 0, #double - proportion of general travellers infected
                          special_in = 0, #integer - number of special travellers arriving on day
                          prop_special_infected = 0, #double - proportion of special travellers infected
                          vaccinations = 0, #integer - number of people vaccinated so far
                          efficacy = 1) { #double - proportion of vaccines that are effective
  if(immunity_time < infectious_time_mean)
    stop("You've set infectious time to be longer than length of immunity, which is silly")
  
  if(general_in %% 1 > 0) {
    general_in = general_in %>% round
  }
  if(special_in %% 1 > 0) {
    special_in = special_in %>% round
  }
  
  Date = new_cases$Date %>% max %>% `+`(1)
  sim_id = new_cases$sim_id[1]
  
  unsusceptible_through_infection = new_cases %>% 
    group_by(Age_group) %>% 
    slice((n()-immunity_time+1):n()) %>% 
    summarise(Unsusceptible_through_infection = sum(Cases))
  unsusceptible_through_vaccination = tibble(Age_group = c("0-17", "18-64", "65-100"),
                                             Unsusceptible_through_vaccination = 0)
  
  susceptible = unsusceptible_through_infection %>% 
    left_join(unsusceptible_through_vaccination, by = "Age_group") %>% 
    left_join(population, by = "Age_group") %>% 
    mutate(Susceptible = (1-Unsusceptible_through_infection/Population) * (1-Unsusceptible_through_vaccination/Population))
  
  currently_infectious = new_cases %>% 
    group_by(Age_group) %>% 
    slice((n()-infectious_time_mean+1):n()) %>% 
    summarise(Currently_infectious = sum(Cases))
  
  #estimates a variance for R0. The fewer people are infected the higher the variance
  r0_today = r0 %>% 
    left_join(currently_infectious, by = "Age_group") %>% 
    mutate(R0_var = map2_dbl(R0, Currently_infectious, ~get_r0_var(.x, var, .y)),
           #gets a random R0, assuming a normal distribution (but with a minimum of 0)
           R0_today = map2_dbl(R0, R0_var, ~rnorm(1, .x, sqrt(.y)) %>% max(0)),
           Beta = R0_today/infectious_time_mean)
  
  infected = r0_today %>% 
    left_join(susceptible %>% rename(Contact_age_group = Age_group), 
              by = "Contact_age_group") %>% 
    mutate(Infected = Currently_infectious * Beta * Susceptible)
  imported = tibble(Contact_age_group = c("0-17", "18-64", "65-100"),
                    Imported = 0)
  
  newest_cases = infected %>% 
    left_join(imported, 
              by = "Contact_age_group") %>% 
    group_by(Contact_age_group) %>% 
    summarise(Cases = sum(Infected+Imported)) %>% 
    transmute(Age_group = Contact_age_group,
              Cases,
              Date = Date,
              sim_id = sim_id)
  
  new_cases %>% 
    bind_rows(newest_cases)
}

forecast_sim_multi = function(df,
                              low_r0,
                              mid_r0,
                              high_r0,
                              end_date,
                              start_date,
                              infectious_time_mean = 12, # integer - number of days a person remains infectious
                              immunity_time = 180, # lnumber of days that immunity lasts
                              var = 20, # double - variance of the natural log of the distribution of beta
                              general_in = 0, #integer - number of general travellers arriving on day
                              prop_general_infected = 0, #double - proportion of general travellers infected
                              special_in = 0, #integer - number of special travellers arriving on day
                              prop_special_infected = 0, #double - proportion of special travellers infected
                              vaccinations = 0, #integer - number of people vaccinated so far
                              efficacy = 1, #double - proportion of vaccines that are effective
                              population = tibble(Age_group = c("0-17", "18-64", "65-100"), 
                                                  Population = get_population() %>% pull(Population) %>% `/`(3))) { #in epidemiology.R
  r0 = rdist(low_r0,mid_r0,high_r0) %>% 
    get_r0_age_group(start_date)
  horizon = (end_date-start_date+1) %>% as.integer
  
  df %>% 
    select(-Date) %>% 
    extend_forecast_multi(population = population,
                          r0 = r0,
                          horizon = horizon,
                          infectious_time_mean = infectious_time_mean,
                          immunity_time = immunity_time,
                          var = var,
                          general_in = general_in,
                          prop_general_infected = prop_general_infected,
                          special_in = special_in,
                          prop_special_infected = prop_special_infected,
                          vaccinations = vaccinations,
                          efficacy = efficacy) %>% 
    mutate(Date = df$Date %>% c(rep(max(df$Date) + 1:horizon, each = 3)))
}

forecast_phase_multi = function(df, 
                                parameters, 
                                var=0, 
                                population=get_population() %>% pull(Population),
                                immunity_time=10000) { #in epidemiology.R
  parameters %>% pmap_dfr(forecast_sim_multi, df=df, var=var, population=population, immunity_time=immunity_time)
}

run_sim_multi = function(ID,
                         initial_state,
                         phases, 
                         var=0, 
                         population = tibble(Age_group = c("0-17", "18-64", "65-100"), 
                                             Population = get_population() %>% pull(Population) %>% `/`(3)),
                         immunity_time=180) { #in epidemiology.R
  set.seed(87345 + 356*ID)
  
  phases %>% 
    nest(-name) %>% 
    pull(data) %>% 
    reduce(forecast_phase_multi, var=var, population=population, immunity_time=immunity_time, .init = initial_state) %>% 
    mutate(sim_id = ID)
}

run_sim_custom = function(ID,
                          initial_state,
                          phases,
                          var=0,
                          population = get_population() %>% pull(Population),
                          immunity_time=180) { #in epidemiology.R
  set.seed(423756 + 843*ID)
  
  phases %>%
    nest(-name) %>%
    pull(data) %>%
    reduce(forecast_phase, var=var, population=population, immunity_time=immunity_time, .init = initial_state) %>%
    mutate(sim_id = ID)
}