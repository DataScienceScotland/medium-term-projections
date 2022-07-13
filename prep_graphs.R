# The Prep_Graph script contains multiple functions which are used to prepare data to be converted into graphs
#
# The Prep_Graph script contains the following functions:
# 1. bed_demand_curve                          2. compare_curves
# 3. vulnerable_recovered_and_resilient_curves 4. deaths_curves
# 5. compare_ranges                            6. compare_curves_TP
# 7. get_tracing_capacity                      8. short_term_forecasts
# 9. prep_graph                                10. prep_graph_grid
# 11. prep_R_reduction_graph                   12. prep_R_graph
# 13. prep_R_reduction_contributions_graph     14. prep_admissions_grid_graph
#
# The Prep_Graph functions are called at multiple stages when running the model 

# Function is called by output_charts in Output.R, this function which creates bar charts of estimated (total)
# hospital bed and icu bed demand, for the slide pack. The function outputs a ggplot.

library(scales)

bed_demand_curve = function(demand, type, name, beds_data, include_lines=TRUE, current_date=today()) { #in prep_graphs.R
  fill = switch(type,
                Hospital = "blue",
                ICU = "red")
  
  title = switch(type,
                 Hospital = "Number of people requiring a hospital bed\n(including intensive care)" %>% 
                   str_c(" -\n", name) %>% 
                   str_replace(" percent", "%"),
                 ICU = "Number of people requiring intensive care" %>% 
                   str_c(" -\n", name) %>% 
                   str_replace(" percent", "%"))
  
  # demand = demand %>% 
  #   mutate(Week = Day/7) 
  # 
  df = demand %>% 
    mutate(frame="demand") %>% 
    bind_rows(beds_data %>% 
                mutate(frame="beds") %>% 
                rename(Number_at = Capacity))
  
  df %>% 
    ggplot(aes(Date, Number_at)) +
    geom_bar(data = df %>% filter(frame == "demand"),
             aes(Date,
                 Number_at, 
                 fill=Age_group %>% 
                   recode(`0`="0 to 9",
                          `10`="10 to 19",
                          `20`="20 to 29",
                          `30`="30 to 39",
                          `40`="40 to 49",
                          `50`="50 to 59",
                          `60`="60 to 69",
                          `70`="70 to 79",
                          `80`="80 and over") %>% 
                   as_factor), 
             stat = "identity", 
             colour = NA) +
      theme_light() +
      ggtitle(title) +
      theme(axis.title.y = element_blank()) +
      labs(fill = "Age group", colour = "") +
      scale_colour_brewer(palette="Set2") +
      scale_fill_brewer(palette="Paired") +
      geom_vline(xintercept = current_date, linetype = "dotted", size = 1.1) + 
      geom_line(data = df %>% filter(frame == "beds"),
                aes(colour=Equipment),
                size=1.1)
}

# Function is called by create_demand_comparison, it creates a ggplots for ICU and hospitals comparing 
# curves

compare_curves = function(Scenario_name, df, type, start_date, end_date, concurrent_or_admissions) { #in prep_graphs.R
  axis.title.y = switch(type,
                        Hospital = "Number of people requiring a hospital\nbed (including intensive care)",
                        ICU = "Number of people requiring intensive\ncare")
  df = switch(type,
              Hospital = df %>% filter(Status %in% c("Oxygen", "Non-invasive ventilation", "Invasive ventilation")),
              ICU = df %>% filter(Status %in% c("Invasive ventilation"))) %>% 
    filter(Date >= start_date, Date <= end_date) %>% 
    group_by(Date, Level) %>% 
    summarise(Number = sum(switch(concurrent_or_admissions,
                                  concurrent = Number_at,
                                  admissions = Number_starting))) %>% 
    ungroup
  
  real_levels = switch(concurrent_or_admissions,
                       concurrent = "Confirmed cases",
                       admissions = "Confirmed cases -\n7 day moving average")
  
  # equipment = get_equipment_data(switch(type, Hospital = "Beds", ICU = "Ventilators"),
  #                                week_max) %>%
  #   filter(Week>=week_min, Week<=week_max) %>%
  #   select(-Week) %>%
  #   mutate(Type = "Real", Scenario_name = type %>% str_c(" capacity")) %>%
  #   rename(Number = Capacity)
  
  #get the actuals for the specified type
  actuals = get_actuals(concurrent_or_admissions) %>% 
    filter(Location %>% str_detect(type), 
           Date %in% df$Date,
           ! Location %>% str_detect("suspected")) %>% 
    #add columns to indicate the type of data for use in the chart, and keep Date and Number_at
    transmute(Level = switch(concurrent_or_admissions,
                             concurrent = "Confirmed cases",
                             admissions = "Confirmed cases -\n7 day moving average"),
              Type = "Real",
              Date,
              Number)
  
  df = df %>% 
    #add column to indicate type of data
    mutate(Type = "Modelled") %>% 
    #combine with the actuals
    bind_rows(actuals) %>% 
    mutate(Type = Type %>% as_factor,
           Level = Level %>% factor(levels = c("Worse scenario", "Most likely scenario", "Better scenario", real_levels)))
  
  type.method.interaction <- interaction(df$Level, df$Type)
  
  # Compute the number of types and methods
  nb.types <- nlevels(df$Type)
  nb.methods <- nlevels(df$Level)
  
  legend.title = "My title"
  
  df %>% 
    ggplot(aes(Date, Number, colour = type.method.interaction, group = type.method.interaction, linetype = type.method.interaction)) +
    geom_line(size=1.1) +
    theme_light() +
    theme(legend.title = element_blank(),
          axis.title.x = element_blank()) +
    scale_colour_manual(legend.title, 
                        values=c(brewer.pal(9,"YlOrRd")[c(6,8,9)],brewer.pal(7,"Blues")[c(5,7)]),
                        labels = levels(df$Level)) +
    scale_linetype_manual(legend.title,
                          values = c(1,1,1,1,3,3),
                          labels = levels(df$Level)) +
    scale_y_continuous(name=axis.title.y)
  
  ggsave("Outputs/curves/" %>% str_c(type, "_", Scenario_name, ".png"),
         height = 10.97,
         width = 14.65,
         units = "cm")
}


# Function is called by create_vunerable_and_resilant function in create_products.R script, it outputs a 
# vunerability recovered and reslilient curve and saves it in the ooutputs folder.

vulnerable_recovered_and_resilient_curves = function(df, Scenario_name, ID, end_date) { #in prep_graphs.R
  df %>% 
    group_by(Status, Date) %>% 
    summarise(Number_at = sum(Number_at, na.rm=0)) %>% 
    filter(Date < end_date) %>% 
    mutate(Number_at = Number_at/1000000) %>% 
    ggplot(aes(Date, Number_at, colour = Status, group = Status)) +
      geom_line(size=1.1) +
      labs(y="Number of people (millions)") +
      theme_light() +
      ggtitle("No interventions") +
      scale_color_brewer(palette = "Dark2")
  
  ggsave("Outputs/curves/vulnerable_resilient" %>% str_c(ID, " ", Scenario_name, ".png"), 
         height = 6.97, 
         width = 14.65, 
         units = "cm")
}

# Function is called by the create_death_curves function in the create_products.R. The function outputs a death
# curve ggplot.

deaths_curves = function(df, current_date=today()) { #in deaths_curves.R
  df %>% 
    filter(Status=="Dead") %>% 
    mutate(Week = Date %>% floor_date("weeks")) %>% 
    group_by(Scenario_name, Week) %>% 
    summarise(Number_starting=sum(Number_starting, na.rm = TRUE)) %>% 
    ggplot(aes(Week,Number_starting,colour=Scenario_name,group=Scenario_name)) +
      geom_line(size=1.1) +
      theme_light() +
      ggtitle("Weekly deaths") +
      scale_color_brewer(palette = "Dark2") +
      theme(legend.title = element_blank(),
            axis.title.y = element_blank()) +
      geom_vline(xintercept = current_date, linetype="dotted", size=1.1)
}

# Function is called by create_demand_comparison in Create_products.R which takes estimates the data frame.
# The function outputs a boxplot time series grouping different scenario groups to compare in the form of a
# ggplot.

compare_ranges = function(estimates, type, start_date, end_date) { #in prep_graphs.R
  title = switch(type,
                 Hospital = "Range of number of people requiring\na hospital bed (including intensive care)",
                 ICU = "Range of number of people requiring\nintensive care")
  estimates = switch(type,
                     Hospital = estimates %>% filter(Status %in% c("Oxygen", "Non-invasive ventilation", "Invasive ventilation")),
                     ICU = estimates %>% filter(Status %in% c("Invasive ventilation"))) %>% 
    filter(Date >= start_date, Date <= end_date)
  
  equipment = get_equipment_data(switch(type, Hospital = "Beds", ICU = "Ventilators"),
                                 1000) %>% 
    filter(Date >= start_date, Date <= end_date)
  
  estimates %>% 
    filter(ID > 0) %>% 
    mutate(ID_group = ((ID-1) %/% 3) %>% recode(`0` = "Lockdown", `1` = "Schools", `2` = "Construction", `3` = "Small outlets"),
           ID = ((ID-1) %% 3) %>% recode(`0` = "upper", `1` = "middle", `2` = "lower"),
           Week = Date %>% floor_date("weeks", week_start = getOption("lubridate.week.start", 1)),
           Week_id = Week %>% as.character %>% as.factor %>% as.integer,
           Fortnight_id = Week_id %/% 2) %>% 
    group_by(ID, ID_group, Date, Week_id) %>% 
    summarise(Number_at = sum(Number_at)) %>% 
    group_by(ID, ID_group, Week_id) %>% 
    summarise(Date = first(Date), Number_at = max(Number_at)) %>% 
    spread(ID, Number_at) %>% 
    ggplot(aes(Date)) +
    geom_boxplot(aes(lower = middle*0.99,
                     middle = middle,
                     upper = middle*1.01,
                     ymin=lower,
                     ymax=upper,
                     colour = ID_group),
                 stat="identity",
                 position = position_dodge(6),
                 width=6,
                 lwd = 0.5) +
    scale_colour_brewer(palette = "Paired") +
    theme_light() +
    theme(legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    ggtitle(title) +
    geom_line(data = equipment, aes(y=Capacity, linetype = Equipment), size=1) +
    scale_linetype_manual(values = c("Beds" = "dashed", "Ventilators" = "dashed"))
}

compare_curves_TP = function(df, 
                             type,
                             start_date,
                             end_date,
                             concurrent_or_admissions="concurrent"){ #in prep_graphs.R
  
  title = switch(type,
                 Hospital = "Number of people requiring a hospital\nbed (including intensive care)",
                 ICU = "Number of people requiring intensive\ncare",
                 Contacts = "Number of contacts needing to be traced",
                 Tracer = "Number of tracers required",
                 Tests = "Number of tests required")
  df = switch(type,
              Hospital = df %>% filter(Status %in% c("Oxygen", "Non-invasive ventilation", "Invasive ventilation")),
              ICU = df %>% filter(Status %in% c("Invasive ventilation")),
              Contacts = df %>% filter(Status %in% c("Contacts")),
              Tracer = df %>% filter(Status %in% c("Tracer")),
              Tests = df %>% filter(Status %in% c("Test"))) %>% 
    filter(Date >= start_date, Date <= end_date) %>% 
    group_by(Date, Scenario_name) %>% 
    summarise(Number_at = sum(Number_at)) %>% 
    ungroup
  
  real_levels = switch(type,
                       Hospital = c("Hospital capacity", "Confirmed cases"),
                       ICU = c("ICU capacity", "Confirmed and suspected cases"),
                       Contacts = c("Initial contact tracing capacity"),
                       Tracer = c("Initial tracer capacity"),
                       Tests = c("Tests capacity"))
  
  capacity_name = switch(type,
                         Hospital = "Hospital capacity",
                         ICU = "ICU capacity",
                         Contacts = "Initial contact tracing capacity",
                         Tracer = "Initial tracer capacity",
                         Tests = "Testing capacity")
  
  equipment = get_equipment_data(switch(type, Hospital = "Beds", ICU = "Ventilators"),
                                 max_week=1000) %>% 
    filter(Date>=start_date, Date<=end_date) %>% 
    select(-Week) %>% 
    mutate(Type = "Real", Scenario_name = type %>% str_c(" capacity")) %>% 
    rename(Number_at = Capacity)
  
  capacity = get_tracing_capacity() %>%
    filter(Date >= start_date, Date <= end_date, Type == type) %>% 
    select(-Day, -Efficiency_s, -Effective_Tracer_Time, -Effective_Tracers) %>%
    mutate(Type = "Real", Scenario_name = capacity_name) 
  
  #get the actuals for the specified type
  actuals = get_actuals(concurrent_or_admissions) %>% 
    filter(Location %>% str_detect(type), 
           Date %in% df$Date,
           ! Location %>% str_detect("suspected")) %>% 
    #add columns to indicate the type of data for use in the chart, and keep Date and Number_at
    transmute(Scenario_name = "Confirmed cases",
              Type = "Real",
              Date,
              Number)
  
  df = df %>% 
    #add column to indicate type of data
    mutate(Type = "Modelled") %>% 
    #combine with the actuals
    bind_rows(actuals, switch(type, 
                              Hospital = equipment,
                              ICU = equipment,
                              Contacts = capacity,
                              Tracer = capacity)) %>% 
    mutate(Type = Type %>% as_factor,
           Scenario_name = Scenario_name %>% factor(levels = c("Worse scenario", "Most likely scenario", "Better scenario", real_levels)))
  
  
  type.method.interaction <- interaction(df$Scenario_name, df$Type)
  
  # Compute the number of types and methods
  nb.types <- nlevels(df$Type)
  nb.methods <- nlevels(df$Scenario_name)
  
  legend.title = "My title"
  
  df %>% 
    ggplot(aes(Date, Number_at, colour = type.method.interaction, group = type.method.interaction, linetype = type.method.interaction)) +
    geom_line(size=1.1) +
    theme_light() +
    ggtitle(title) +
    theme(axis.title.y = element_blank(),
          legend.title = element_blank(),
          axis.title.x = element_blank()) +
    scale_colour_manual(legend.title, 
                        values=c(brewer.pal(9,"YlOrRd")[c(6,8,9)],brewer.pal(7,"Blues")[c(5,7)]),
                        labels = levels(df$Scenario_name)) +
    scale_linetype_manual(legend.title,
                          values = c(1,1,1,3,3,3),
                          labels = levels(df$Scenario_name)) +
    scale_y_continuous(name="Fluorescent intensity/arbitrary units", labels = scales::comma)
}


get_tracing_capacity = function(tracer_expansion=1.25, #A factor to multiply the required tracers by to avoid queues forming
                                number_of_contacts = 10, #Estimated number of contacts a person with Covid symptoms would have
                                efficiency_saving = tibble(x=c(0,15,46), 
                                                           y=c(0,0.25,0.5)), #Efficiency saving in time tracers take to trace
                                initial_tracers = 2002, #Number of tracers available initially
                                tracer_time_per_case = 2, #Hours a tracer spends on a case on top of time spent tracing each contact
                                tracer_time_per_contact = 1, #Hours a tracer spends tracing each contact
                                active_tracer_time = 6) #Hours a tracer spends actively tracing per shift
{#in prep_graphs.R
  tibble(Day=seq(131,322,1), 
         Date = seq(as.Date("2020-05-31"),
                    as.Date("2020-12-08"),1),
         Tracer=initial_tracers,
         Efficiency_s = predict(efficiency_saving %>% 
                                  nls(y ~ 1 - 1/(x/A + B),
                                      data = .,
                                      start = list(A = 1, B=1)),
                                list(x=Day-131)),
         Effective_Tracers =initial_tracers/tracer_expansion) %>% 
    mutate(Efficiency_s=ifelse(Efficiency_s<0,0,ifelse(Efficiency_s>0.5,0.5,Efficiency_s)),
           Effective_Tracer_Time = active_tracer_time /(1-Efficiency_s),
           Contacts=Effective_Tracers * Effective_Tracer_Time *(1/(tracer_time_per_case/number_of_contacts + tracer_time_per_contact))) %>%
    gather(Type, Number_at,Tracer, Contacts) 
}

# Function is called by create_demand_comparison in create_short_term_forecasts_chart.R
# The function outputs a time series of past data for hospitalisations, along with a central and upper forecast

short_term_forecasts = function(filename, audience) {#in prep_graphs.R
  real_data = filename %>%
    get_filepath("Data") %>%
    read_excel(sheet="Real Data") %>%
    filter(geography=="Scotland", value_desc=="Total beds occupied") %>%
    mutate(Type="Real data") %>%
    select(value_date, Type, value)
  
  forecast_data = filename %>%
    get_filepath("Data") %>%
    read_excel(sheet="Forecast Data") %>%
    filter(geography=="Scotland", value_desc=="Total beds occupied") %>%
    rename("Median forecast" = "median", "95th percentile forecast" = "percentile_0.95") %>%
    select(value_date, `Median forecast`, `95th percentile forecast`) %>%
    gather(Type, value,`Median forecast`, `95th percentile forecast`)
  
  data = bind_rows(real_data, forecast_data)
  
  legend.title = "My title"
  
  type.method.interaction <- interaction(data$Type)
  
  
  plot = data %>%
    ggplot(aes(value_date, value, colour = Type, linetype = Type)) +
    geom_line(size=1.1) +
    theme_light() +
    scale_fill_brewer() +
    theme(legend.title = element_blank(),
          axis.title.x = element_blank()) +
    scale_colour_manual(values=c(brewer.pal(9,"YlOrRd")[c(6,9)],brewer.pal(7,"Blues")[c(5)])) +
    scale_linetype_manual(values = c(1,1,3)) +
    scale_y_continuous("Number of beds", labels = scales::comma, expand = c(0, 0), limits = c(0, 1500)) +
    scale_x_datetime(breaks = date_breaks("1 week"),
                     minor_breaks = NULL)
  
  if(audience == "internal")
    plot +
    geom_text(data=. %>%
                filter(Type %in% c("Median forecast", "95th percentile forecast")) %>%
                group_by(Type) %>%
                filter(row_number()==1 | row_number()==n()),
              aes(label=value),
              color="black",
              size=3,
              hjust=0.000001, vjust=0.000001, show.legend=FALSE)
  else
    plot + theme(legend.position = c(0.15,0.15))
}

prep_graph = function(data, 
                      variable, 
                      y_label = variable, 
                      alpha = 0.4, 
                      data2 = NULL,
                      name1 = "1",
                      name2 = "2",
                      include_medians = TRUE,
                      smoothed_quantiles = c(),
                      actuals_style = "ribbon") { #in prep_graphs
  
  min = 5
  phi = 10
  
  if(! data2 %>% is_null) {
    data = data %>% mutate(Scenario_name = Scenario_name %>% str_c(name1, sep = " - ")) %>% 
      bind_rows(data2 %>% mutate(Scenario_name = Scenario_name %>% str_c(name2, sep = " - ")))
  }
  
  scenarios = data$Scenario_name %>% unique
  colours = scenarios %>% 
    recode(Better = "#1B9E77", Central = "#D95F02", Worse = "#7570B3",
           `Vaccine world` = "#4472c4",`Infected world` = "#C55A11",
           `Immune world` = "#4472c4",`Waning world` = "#C55A11",
           `Low compliance world` = "#70AD47",`Variant world` = "#FFC000",
           `Polarised world` = "#70AD47",
           .default = brewer.pal(8, "Dark2")[length(scenarios):1])
  linetypes = scenarios %>% recode(Better = "dotted", Worse = "dashed", .default = "solid")
  
  plotting_data = data %>% 
    mutate(`5th percentile` = `5th percentile` %>% adjust_lower_bound(min, phi),
           `95th percentile` = `95th percentile` %>% adjust_upper_bound(min, phi)) %>% 
    filter(ValueType == variable) %>%
    group_by(Scenario_name) %>% 
    mutate_at(smoothed_quantiles,
              smooth_quantile)
  
  actuals_data = plotting_data %>% 
    filter(! Actual %>% is.na())
  
  if(actuals_style %in% c("whisker", "point")) {
    plotting_data = plotting_data %>% 
      filter(Actual %>% is.na())
  }
  else if(actuals_style != "ribbon") stop("'actuals_style must be 'ribbon', 'whisker' or 'point")
  
  plotting_data %>% 
    ggplot(aes(Date,
               ymin = `5th percentile`,
               ymax = `95th percentile`,
               fill = Scenario_name,
               colour = Scenario_name, 
               linetype = Scenario_name)) +
    geom_ribbon(alpha=alpha, colour=NA) +
    list(if(include_medians) geom_line(size = 0.9, aes(y = Median))) +
    # AS: Added some if statements to prevent the code throwing an error if you try to plot a period with no actuals.
    list(if(actuals_style == "whisker" & nrow(actuals_data) > 0) 
      geom_errorbar(size = 0.5, colour = "darkgrey", aes(ymin = Actual/2, ymax = Actual*2, shape = "Actual"), data = actuals_data)) +
    list(if(nrow(actuals_data) > 0) 
      geom_point(size = 0.9, colour = "black", aes(y = Actual, shape = "Actual"), data = actuals_data)) +
    scale_shape_manual(values = c(Actual = 16)) +
    scale_linetype_manual(values = linetypes,
                          breaks = scenarios) +
    scale_colour_manual(values = c(colours),
                        breaks = scenarios,
                        guide = guide_legend(override.aes = list(shape = rep(NA, length(scenarios)),
                                                                 linetype = linetypes,
                                                                 fill = colours)),
                        aesthetics = c("fill", "colour")) +
    theme_light() +
    theme(legend.title = element_blank()) +
    ylab(y_label) +
    scale_y_continuous(labels = comma) +
    guides(linetype = "none")
}

prep_graph_grid = function(df, 
                           start_date = (today() %>% floor_date("weeks", week_start = 1)) - 56,
                           end_date = start_date + 140,
                           graphs = c("Infections", "Hospital beds", "ICU beds"),
                           df2=NULL, 
                           name1 = 1,
                           name2 = 2,
                           include_medians = TRUE,
                           ICU_source = "published", #set to "sicsag" for SICSAG data (not to be shared)
                           ncol = length(graphs), #number of columns to display the graphs in
                           include_legend = TRUE,
                           s_gene_status = "all") { #in prep_graphs.R
  
  if(! df2 %>% is_null) {
    df = df %>% mutate(Scenario_name = Scenario_name %>% str_c(name1, sep = " - ")) %>% 
      bind_rows(df2 %>% mutate(Scenario_name = Scenario_name %>% str_c(name2, sep = " - ")))
  }
  
  graph_list = list()
  
  if("Infections" %in% graphs) {
    graph_list = list(df %>% 
                        prep_infections_data(start_date, end_date, s_gene_status) %>%
                        prep_graph("Incidence", "Daily infections", data2 = NULL,
                                   include_medians = include_medians,
                                   smoothed_quantiles = c("5th percentile", "Median", "95th percentile"),
                                   actuals_style = "whisker") + 
                        geom_hline(linetype = "dotted", size = 0.9)) %>% 
      c(graph_list, .)
  }
  
  hospitalisation_data = df %>% 
    prep_hospitalisation_data(start_date, end_date+14, include_recovering = FALSE, ICU_source = ICU_source) %>% 
    adjust_occupancy 
  
  if("Hospital beds" %in% graphs) {
    graph_list = list(hospitalisation_data %>% 
                        prep_graph("Hospital beds", 
                                   data2 = NULL, 
                                   include_medians = include_medians,
                                   actuals_style = "ribbon") + 
                        geom_hline(linetype = "dotted", size = 0.9))  %>% 
      c(graph_list, .)
  }
  
  if("Hospital admissions" %in% graphs) {
    graph_list = list(hospitalisation_data %>% 
                        prep_graph("Hospital admissions",
                                   data2 = NULL,
                                   include_medians = include_medians,
                                   actuals_style = "point"))  %>% 
      c(graph_list, .)
  }
  
  if("ICU beds" %in% graphs) {
    graph_list = list(hospitalisation_data %>% 
                        prep_graph("ICU beds", 
                                   data2 = NULL, 
                                   include_medians = include_medians,
                                   actuals_style = "ribbon") + 
                        geom_hline(linetype = "dotted", size = 0.9)) %>% 
      c(graph_list, .)
  }
  
  if("ICU admissions" %in% graphs) {
    graph_list = list(hospitalisation_data %>% 
                        prep_graph("ICU admissions",
                                   data2 = NULL, 
                                   include_medians = include_medians,
                                   actuals_style = "point"))  %>% 
      c(graph_list, .)
  }
  
  if("Daily deaths" %in% graphs) {
    graph_list = list(df %>% 
                        prep_deaths_data(start_date, end_date) %>% 
                        prep_graph("Daily deaths", data2 = NULL, 
                                   include_medians = include_medians,
                                   smoothed_quantiles = c("5th percentile", "Median", "95th percentile"),
                                   actuals_style = "ribbon"))  %>% 
      c(graph_list, .)
  }
  
  if("Deaths (7 day average)" %in% graphs) {
    graph_list = list(df %>% 
                        prep_deaths_data(start_date, end_date, actual_measure = "Deaths (7 day average)") %>% 
                        prep_graph("Deaths (7 day average)", data2 = NULL, 
                                   include_medians = include_medians,
                                   smoothed_quantiles = c("5th percentile", "Median", "95th percentile"),
                                   actuals_style = "ribbon"))  %>% 
      c(graph_list, .)
  }
  
  shared_legend = extract_legend(graph_list[[1]] + theme(legend.position = "bottom", legend.direction = "horizontal"))
  
  graph_list = graph_list %>% 
    map(~ . + 
          theme(legend.position = "none") +
          labs(x=""))
  
  if(include_legend) {
    grid.arrange(arrangeGrob(grobs = graph_list, ncol = ncol),
                 shared_legend, 
                 nrow = 2,
                 heights = c(10,1))
  }
  else {
    grid.arrange(arrangeGrob(grobs = graph_list, ncol = ncol), 
                 nrow = 1,
                 heights = c(11))
  }
}

prep_R_reduction_graph = function(scenario, start_date, end_date, worldname_base) { # in prep_graphs.R
  
  variants_dates = "variants_for_charts.csv" %>%
    get_filepath("Data") %>% 
    read_csv %>% 
    mutate(Date = dmy(Date)) %>% 
    spread(Point, Date) %>% 
    mutate(Date = map2(Start, End, seq.Date, by = 1)) %>% 
    unnest %>% 
    select(Date, World, R0) %>% 
    complete(Date = seq.Date(min(.$Date), max(.$Date), by = 1),
             World = .$World %>% unique) %>% 
    arrange(World, Date) %>% 
    group_by(World) %>% 
    mutate(R0 = R0 %>% na.approx) %>% 
    ungroup %>% 
    filter(World == worldname_base %>% str_remove(" world")) %>% 
    select(Date, R0_no_npi = R0)
  
  scenario %>% 
    str_c("SCHEMa outputs/", ., " sim_output.rds") %>%
    readRDS %>%
    mutate(Date = Date) %>%
    group_by(Date) %>%
    summarise(R = mean(R),
              Nat_immune = mean(Nat_immune),
              Vac_immune = mean(Vac_immune)) %>%
    ungroup %>%
    left_join("Forecast scenarios.xlsx" %>%
                get_filepath("Data") %>%
                read_excel(sheet = scenario) %>%
                transmute(Date = start_date %>% ymd, R0 = mid_r0)) %>%
    fill(R0) %>%
    left_join(variants_dates) %>% 
    transmute(Date,
              R0_no_npi,
              `Vaccine immunity` = (R0 - R) * Vac_immune / (Nat_immune+Vac_immune),
              `Natural immunity` = (R0 - R) * Nat_immune / (Nat_immune+Vac_immune),
              NPI = R0_no_npi-R0) %>%
    filter(Date >= ymd("2021-01-01")) %>%
    gather(`Cause of reduction`, `R reduction`, `Vaccine immunity`, `Natural immunity`, NPI) %>%
    mutate(`Cause of reduction` = `Cause of reduction` %>% factor(levels = c("NPI", "Natural immunity", "Vaccine immunity"))) %>%
    ggplot(aes(Date)) +
    geom_bar(aes(y = `R reduction`, fill = `Cause of reduction`), stat = "identity", position = position_stack(),
             width = 7) +
    geom_line(aes(y = R0_no_npi), colour = "black", size = 0.9, linetype = "solid") +
    geom_line(aes(y = R0_no_npi-1), colour = "black", size = 1.1, linetype = "dotted") +
    ylab("R reduction") +
    xlim(c(start_date,end_date)) +
    theme_light() +
    scale_fill_viridis_d(direction = -1) +
    labs(fill = "Cause of reduction")
}

prep_R_graph = function(scenario, start_date, end_date,
                        colourval="#1B9E77", label=scenario) { # in prep_graphs.R
  scenario %>% 
    str_c("SCHEMa outputs/", ., " sim_output.rds") %>% 
    readRDS %>% 
    filter(Date >= ymd("2020-07-01")) %>% 
    group_by(Date) %>% 
    summarise(mid = median(R),
              low = R %>% quantile(0.05),
              high = R %>% quantile(0.95)) %>% 
    ungroup %>% 
    mutate(Scenario = label) %>% 
    filter(Date >= start_date, Date <= end_date) %>% 
    ggplot(aes(Date, y=mid, ymin=low, ymax=high, colour = Scenario, fill = Scenario)) + 
    geom_ribbon(alpha = 0.12, colour = NA) + 
    geom_line(size = 0.9) + 
    scale_colour_manual(values = c(colourval),
                        guide = guide_legend(override.aes = list(shape = rep(NA, 1),
                                                                 linetype = rep("solid",1),
                                                                 fill = c(colourval))),
                        aesthetics = c("fill", "colour")) +
    ylab("Rt") + 
    theme_light() + 
    ylim(c(0, NA))
}

prep_R_reduction_contributions_graph = function(scenario, start_date, end_date, worldname_base, 
                                                filename = "Forecast scenarios.xlsx" %>%
                                                  get_filepath("Data"),
                                                sheetname = scenario) { # in prep_graphs.R
  lines = c("NPIs/Behaviour change", "Natural immunity", "Vaccine immunity", "Total", "Rt = 1")
  colours = viridis(3, begin = 0.2, end = 0.8, option = "C") %>% c("black", "black")
  linetypes = c("dashed", "dashed", "dashed", "solid", "dotted")
  
  variants_dates = "variants_for_charts.csv" %>%
    get_filepath("Data") %>% 
    read_csv %>% 
    mutate(Date = dmy(Date)) %>% 
    spread(Point, Date) %>% 
    mutate(Date = map2(Start, End, seq.Date, by = 1)) %>% 
    unnest %>% 
    select(Date, World, R0) %>% 
    complete(Date = seq.Date(min(.$Date), max(.$Date), by = 1),
             World = .$World %>% unique) %>% 
    arrange(World, Date) %>% 
    group_by(World) %>% 
    mutate(R0 = R0 %>% na.approx) %>% 
    ungroup %>% 
    filter(World == worldname_base %>% str_remove(" world")) %>% 
    select(Date, R0_no_npi = R0)
  
  scenario %>% 
    str_c("SCHEMa outputs/", ., " sim_output.rds") %>%
    readRDS %>%
    mutate(Date = Date) %>%
    group_by(Date) %>%
    summarise(R = mean(R),
              Nat_immune = mean(Nat_immune),
              Vac_immune = mean(Vac_immune)) %>%
    ungroup %>%
    left_join(filename %>%
                read_excel(sheet = sheetname) %>%
                transmute(Date = start_date %>% ymd, R0 = mid_r0)) %>%
    fill(R0) %>%
    left_join(variants_dates) %>% 
    transmute(Date,
              `Rt = 1` = 1-1/R0_no_npi,
              `Vaccine immunity` = Vac_immune / 5463300,
              `Natural immunity` = Nat_immune / 5463300,
              `NPIs/Behaviour change` = 1-R0/R0_no_npi, 
              Total = 1-R/R0_no_npi) %>%
    filter(Date >= ymd("2021-01-01")) %>%
    gather(`Cause of reduction`, `R reduction`, `Vaccine immunity`, `Natural immunity`, `NPIs/Behaviour change`, `Rt = 1`, Total) %>%
    mutate(`Cause of reduction` = `Cause of reduction` %>% factor(levels = c("NPIs/Behaviour change", "Natural immunity", "Vaccine immunity", "Rt = 1", "Total"))) %>%
    ggplot(aes(Date, `R reduction`, colour = `Cause of reduction`, linetype = `Cause of reduction`)) +
    geom_line(size = 2) + 
    ylab("R reduction") +
    xlim(c(start_date,end_date)) +
    theme_light() +
    scale_colour_manual(breaks = lines,
                        values = colours) +
    scale_linetype_manual(breaks = lines,
                          values = linetypes) +
    labs(colour = "Cause of reduction") + 
    scale_y_continuous(labels = scales::percent, limits = c(0,1))
}

prep_admissions_grid_graph = function() { # in prep_graphs.R
  
  # "system_watch_emergency_admissions.csv" %>% 
  #   get_filepath("Data") %>% 
  #   read_csv() %>% 
  #   transmute(Date = `Week Ending` %>% 
  #               str_remove("Sun ") %>% 
  #               dmy,
  #             Data_type = `Measure Names` %>% 
  #               recode(`Covid Admissions` = "C-19 Admissions"),
  #             age_group = `Age Group` %>% 
  #               recode(`18 - 49` = "18 to 49",
  #                      `Under 18` = "< 18"),
  #             Value = `Measure Values`) %>% 
  
  "COVID19AdmissionsbyAgeGroup.csv" %>% 
    get_filepath("Data") %>% 
    read_csv() %>% 
    filter(A01Scotland == "Scotland") %>% 
    select(-A01Scotland, -A1Date) %>% 
    gather(`Measure Names`, `Measure Values`, -`Age Group`, -`Week Ending`) %>% 
    transmute(Date = `Week Ending` %>%
                str_remove("Sun ") %>%
                dmy,
              Data_type = `Measure Names` %>%
                recode(`Covid` = "C-19 Admissions",
                       `Admissions (Emergency)` = "All Admissions"),
              age_group = `Age Group` %>%
                recode(`18 - 49` = "18 to 49",
                       `Under 18` = "< 18"),
              Value = `Measure Values` %>% str_remove("%") %>% as.double) %>% 
    
    ##choose from above depending on available data.
    filter(Data_type %in% c("C-19 Admissions", "All Admissions"),
           Date > today() - 35) %>% 
    bind_rows((.) %>% 
                group_by(Date, Data_type) %>% 
                summarise(Value = sum(Value)) %>% 
                mutate(age_group = "Total")) %>% 
    filter(age_group != "Unknown") %>% 
    spread(Data_type, Value) %>%
    mutate(`% C-19 admissions` = 100 * `C-19 Admissions` / `All Admissions`) %>% 
    gather(Data_type, Value, -Date, -age_group) %>%
    ggplot(aes(x=Date, y=Value, colour=Data_type)) +
    geom_line(size=1.5)+
    facet_grid(cols = vars(age_group), rows =vars(Data_type), scales="free") +
    labs(y = "", x = "Date")+
    theme_light() +
    theme(legend.title = element_blank(),
          legend.position = "none")+
    theme(panel.spacing = unit(1, "lines")) +
    theme(text = element_text(size=18),
          axis.text.x = element_text(angle=40, hjust=1)) +
    theme(
      strip.text.x = element_text(
        size = 12, color = "black", face = "bold.italic"
      ),
      strip.text.y = element_text(
        size = 12, color = "black", face = "bold.italic"
      )
    ) +
    scale_color_brewer(palette="Dark2")
}