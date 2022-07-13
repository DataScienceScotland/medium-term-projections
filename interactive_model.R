library(shiny)
library(rhandsontable)

# Define UI for application that draws a histogram
ui = fluidPage(
  # Application title
  titlePanel("Covid simulation"),
  
  tabsetPanel(
    tabPanel("Create scenario",
  
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
        sidebarPanel(
          numericInput("n_sims",
                       "Number of simulations to run (effects run time):",
                       value = 10,
                       min = 10,
                       max = 1000),
          sliderInput("var",
                      "Transmission randomness factor:",
                      min = 0,
                      max = 40,
                      value = 20,
                      step = 5),
          dateInput("date_min",
                    "Set graph start date:",
                    min = "2020-02-01",
                    max = "2022-12-01",
                    value = (today() %>% floor_date("weeks", week_start = 1)) - 28,
                    format = "dd-mm-yyyy"),
          dateInput("date_max",
                    "Set graph end date:",
                    min = "2020-03-01",
                    max = "2022-12-01",
                    value = (today() %>% floor_date("weeks", week_start = 1)) + 6,
                    format = "dd-mm-yyyy"),
          numericInput("detection_rate",
                       "Proportion of cases detected:",
                       value = 0.5,
                       min = 0.1,
                       max = 10),
          numericInput("adjustment",
                       "Factor to adjust initial outbreak cases:",
                       value = 1,
                       min = 0.1,
                       max = 2)
        ), #!sidebarPanel
        
        # Show a plot of the generated distribution
        mainPanel(
          textInput("save_name",
                    NULL,
                    placeholder = "Scenario name"),
          rHandsontableOutput("phase_table"),
          actionButton("save_button",
                       "Save scenario"),
          actionButton("sim_button",
                       "Run simulation"),
          plotOutput("cases_plot")
        ) #!mainPanel
      ) #!sideBarLayout
    ), #!tabPanel
    
    tabPanel(
      "Compare scenarios",
      sidebarLayout(
        sidebarPanel(
          uiOutput("comparison_ui"),
          dateInput("comparison_date_min",
                    "Set graph start date:",
                    min = "2020-02-01",
                    max = "2020-08-01",
                    value = "2020-02-01"),
          dateInput("comparison_date_max",
                    "Set graph end date:",
                    min = "2020-09-01",
                    max = "2021-04-01",
                    value = "2020-11-16")
        ), #!sidebarPanel
        
        mainPanel(
          plotOutput("comparison_plot")
        ) #!mainPanel
      ) #!sidebarLayout
    ) #!tabPanel
  ) #!tabsetPanel
) #!fluidPage

# Define server logic required to draw a histogram
server = function(input, output, session) { # in interactive_model.R
  
  oplan <- plan()
  on.exit(plan(oplan), add = TRUE)
  plan(multisession)
  
  positive_cases = get_SCHEMa_options("positive_cases")
  default_phases = get_SCHEMa_options("default_phases")
  population = get_SCHEMa_options("population")
  
  natural_immunity_time = get_immunity_time("natural")
  vaccine_immunity_time = get_immunity_time("vaccine")
  
  #estimates from the Imperial model for the earlier part of the epidemic
  df = "history.rds" %>% 
    get_filepath("Data") %>% 
    readRDS()
  
  scenarios = reactive({
    #invalidateLater(10000, session)
    
    list.files("Outputs") %>% 
      str_subset("sim_output") %>% 
      str_remove(" sim_output.rds")
  })
  
  #ui element to choose which scenarios to compare
  output$comparison_ui = renderUI({
    
    selectInput("comparison_select",
                "Select scenarios for comparison:",
                scenarios(),
                multiple = TRUE)
  })
  
  #multiplies the cases in df by the given adjustment factor
  #(df is the estimates from the IC model)
  adjusted_df = reactive({
    df %>% 
      mutate(Cases = Cases * input$adjustment) %>% 
      filter(Date < min(phases()$start_date))
  })
  
  #converts positive tests to actuals using assumption of detection rate
  adjusted_actuals = reactive({
    positive_cases %>% 
      transmute(Date,
                ValueType = "Incidence",
                Actual = Cases * 0.5 / input$detection_rate)
  })
  
  #runs the simulation
  sim_output = eventReactive(
    input$sim_button, {
      run_simulation_from_scenario(phases(), 
                                   input$n_sims, 
                                   adjusted_df(), 
                                   var = input$var,
                                   natural_immunity_time = natural_immunity_time,
                                   vaccine_immunity_time = vaccine_immunity_time)
    })
  
  #saves the phases table and the simulation outputs
  observeEvent(input$save_button, {
    if(input$save_name == "") {
      showModal( modalDialog(
        title = "Error",
        "Set a scenario name",
        footer = NULL,
        easyClose = TRUE
      ))
    }
    
    else {
      saveRDS(isolate(phases()),
              file = str_c("Outputs/", input$save_name, " phases.rds"))
      saveRDS(isolate(sim_output()),
              file = str_c("Outputs/", input$save_name, " sim_output.rds"))
      showModal( modalDialog(
        title = "Success",
        "Scenario saved",
        footer = NULL,
        easyClose = TRUE
      ))
    }
  })
  
  #plots the lower bound, upper bound and median, along with the estimate of actuals
  output$cases_plot = renderPlot({
    sim_output() %>% 
      group_by(Date) %>% 
      summarise(`5th percentile` = min(Cases),
                `95th percentile` = max(Cases),
                Median = median(Cases)) %>% 
      mutate(ValueType = "Incidence", Scenario_name = "Estimate") %>% 
      #mutate(Level = "Estimate") %>% 
      left_join(adjusted_actuals()) %>% 
      filter(Date >= input$date_min, Date <= input$date_max) %>% 
      prep_graph("Incidence", "Daily infections")
  })
  
  #links to the interactive phases table
  phases = reactive({
    temp = input$phase_table
    if(length(temp) > 0) temp %>% hot_to_r
    else default_phases
  })
  
  #creates the interactive phases table
  output$phase_table = renderRHandsontable({
    phases() %>% 
      #mutate(start_date = start_date %>% format("%d-%m-%y")) %>% 
      rhandsontable %>%
      hot_col("low_r0", format = "0.00") %>%
      hot_col("mid_r0", format = "0.00") %>%
      hot_col("high_r0", format = "0.00") %>%
      #hot_col("start_date", dateFormat = "DD-MM-YY", type = "date") %>%
      #hot_col("end_date", dateFormat = "dd/mm/yyyy", type = "date") %>%
      hot_col("general_in", format = "0") %>%
      hot_col("prop_general_infected", format = "0.0000") %>%
      hot_col("special_in", format = "0") %>%
      hot_col("prop_special_infected", format = "0.0000")
  })
  
  #creates a plot comparing different scenarios
  output$comparison_plot = renderPlot({
    validate(
      need(input$comparison_select != "",
           "Select at least one saved scenario")
    )
    
    scenarios = input$comparison_select
    
    actuals = adjusted_actuals() %>% 
      crossing(Scenario = scenarios)
    
    scenarios %>% 
      map(~str_c("Outputs/", ., " sim_output.rds") %>% readRDS) %>% 
      set_names(scenarios) %>% 
      bind_rows(.id = "Scenario") %>% 
      group_by(Date, Scenario) %>% 
      summarise(Min = quantile(Cases, 0.05),
                Max = quantile(Cases, 0.95),
                Cases = median(Cases)) %>% 
      mutate(Level = "Estimate") %>% 
      bind_rows(actuals) %>% 
      filter(Date >= input$comparison_date_min,
             Date <= input$comparison_date_max) %>% 
      ggplot(aes(Date, y = Cases, ymin = Min, ymax = Max, colour = Level, fill = Level)) +
      geom_ribbon(alpha = 0.5) +
      geom_line(size = 0.9) + 
      scale_colour_brewer(palette = "Dark2") +
      scale_fill_brewer(palette = "Dark2") +
      theme_light() +
      facet_wrap(vars(Scenario)) +
      theme(strip.background =element_rect(fill="grey40"),
            strip.text = element_text(size=11),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.title = element_blank())
  })
}

run_interactive_model = function(default_phases = NULL,
                                 positive_cases = NULL,
                                 population = NULL) { # in interactive_model.R
  
  
  if(positive_cases %>% is_null) {
    #positive test numbers by local authority
    positive_cases = get_historic_infections()
  }
  
  if(default_phases %>% is_null) {
    #the default phase table to use for the scenario when first opening the app
    default_phases = "Forecast scenarios.xlsx" %>% 
      get_filepath("Data") %>% 
      read_excel %>% 
      mutate_at(c("start_date", "end_date"),
                ymd)
  }
  
  if(population %>% is_null()) 
    population = get_population() %>% pull(Population)
  
  with_SCHEMa_options(
    app = shinyApp(ui = ui, server = server), 
    SCHEMa_opts = list(default_phases = default_phases,
                       positive_cases = positive_cases,
                       population = population)
  )
}

get_SCHEMa_options <- function(which = NULL){
  if (is.null(which)){
    getShinyOption("SCHEMa_options")
  } else {
    getShinyOption("SCHEMa_options")[[which]]
  }
}

with_SCHEMa_options = function(app, SCHEMa_opts){
  app$appOptions$SCHEMa_options = SCHEMa_opts
  app
}