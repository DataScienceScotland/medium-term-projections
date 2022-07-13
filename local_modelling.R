library(shiny)
library(rhandsontable)
library(tidyverse)
library(openxlsx)
library(readxl)
library(lubridate)

get_beds_actuals = function(current_beds_date=NULL){
  
  if(is.null(current_beds_date)){
    current_beds_date = today()
  }else{
    current_beds_date = ymd(current_beds_date)
  }
  #in local_modelling.R
  #Input: C19-Inpatients and ICU Inpatients - New Definitions spreadsheet (contains actuals for hosp/ICU beds)
  #Output: Table containing current number of occupied beds and each HB's proportion 
  
  #HB current proportion of total bed occupancy
  ICU_current_beds = str_c("C19-Inpatients and ICU Inpatients - New Definitions.xlsx") %>%
    get_filepath() %>%
    read_excel(sheet = 2,
               skip=4,
               n_max = 14,
               range = anchored("A5", dim=c(15,(as.integer(current_beds_date-ymd("20200911"))+2))),
               trim_ws = TRUE) %>%
    rename("HBName" = 1) %>%
    mutate(HBName = str_replace(str_replace(HBName,"&", "and"),"NHS ","")) %>%
    gather(Date, Number, -HBName) %>%
    mutate(Date = Date %>%
             as.numeric() %>%
             as.Date(origin = "1899-12-30"),
           Status = "ICU") %>%
    filter(Date == current_beds_date)
  
  ICU_current_beds_long_stay = str_c("C19-Inpatients and ICU Inpatients - New Definitions.xlsx") %>%
    get_filepath() %>%
    read_excel(sheet = 3,
               skip=4,
               n_max = 14,
               range = anchored("A5", dim=c(15,(as.integer(current_beds_date-ymd("20210120"))+3))),
               trim_ws = TRUE
    ) %>%
    rename("HBName" = 1) %>%
    mutate(HBName = str_replace(str_replace(HBName,"&", "and"),"NHS ","")) %>%
    gather(Date, long_stay, -HBName) %>%
    mutate(Date = Date %>%
             as.numeric() %>%
             as.Date(origin = "1899-12-30"),
           Status = "ICU") %>%
    filter(Date == current_beds_date)
  
  ICU_current_beds = ICU_current_beds %>%
    left_join(ICU_current_beds_long_stay) %>%
    mutate(Number = Number + long_stay) %>%
    select(-long_stay)
  
  Hospital_current_beds = str_c("C19-Inpatients and ICU Inpatients - New Definitions.xlsx") %>%
    get_filepath() %>%
    read_excel(sheet = 1,
               skip=4,
               n_max = 14,
               range = anchored("A5", dim=c(15,(as.integer(current_beds_date-ymd("20200911"))+2))),
               trim_ws = TRUE) %>%
    rename("HBName" = 1) %>%
    mutate(HBName = str_replace(str_replace(HBName,"&", "and"),"NHS ","")) %>%
    gather(Date, Number, -HBName) %>%
    mutate(Date = Date %>%
             as.numeric() %>%
             as.Date(origin = "1899-12-30"),
           Status = "Hospital") %>%
    filter(Date == current_beds_date)
  
  bind_rows(ICU_current_beds,Hospital_current_beds) %>%
    group_by(Status) %>%
    transmute(HBName,
              Date,
              Number,
              HB_prop_current_beds = Number/sum(Number)) %>%
    ungroup
}


#Save output to the local modelling spreadsheet
save_to_spreadsheet = function (hosp_bed_demand_by_area, icu_bed_demand_by_area, current_beds_date, source){
  #Save output to the local modelling spreadsheet
  #in local_modelling.R
  #Input: Forecast bed demands
  #Output:Modelling - Local forecasts - Data.xlsx
  
  #Load template
  template = "Modelling - Local forecasts - Template.xlsx" %>%
    get_filepath() %>%
    loadWorkbook() 
  
  #Write the date the data was updated
  # AS: Changed this to the Monday of the current week as this is
  # when the data goes up to, rather than when the model is run.
  # get_local_projections("Combined", "date_updated") %>% 
  as.Date(cut(ymd(current_beds_date), "week") ) %>% 
    format.Date("%d/%m/%Y") %>% 
    str_c("Data updated: ", .) %>% 
    writeData(template, sheet = "Cases for NHS slidepack", ., startCol = 1, startRow = 1, colNames = FALSE)
  
  #Write the date of the forecasts
  as.Date(as.Date(cut(ymd(current_beds_date),"week",start.on.monday=FALSE))+14) %>% 
    format.Date("%d/%m/%Y") %>% 
    str_c("Forecast of cases, and probability of exceeding 20, 50, 100, 150, 300, 500, 750, 1000, 1500 or 3000 cases per 100,000 in week commencing ", .) %>% 
    writeData(template, sheet = "Cases for NHS slidepack", ., startCol = 1, startRow = 4, colNames = FALSE)
  
  #Write table into template
  make_sheet_1(current_beds_date, source) %>%
    writeData(template, sheet = "Cases for NHS slidepack", ., startCol = 1, startRow = 5, colNames = TRUE,
              headerStyle = createStyle(textDecoration = "bold"))
  
  make_nhs_table(hosp_bed_demand_by_area, "Hospital", current_beds_date=current_beds_date) %>%
    writeData(template, sheet = "Hosp table for NHS slidepack", ., startCol = 2, startRow = 3, colNames = FALSE)
  
  make_nhs_table(icu_bed_demand_by_area, "ICU", current_beds_date=current_beds_date) %>%
    writeData(template, sheet = "ICU table for NHS slidepack", ., startCol = 2, startRow = 3, colNames = FALSE)
  
  saveWorkbook(template, str_c("Outputs/Modelling - Local forecasts - Data.xlsx"), overwrite = TRUE)
}

get_capacity = function() {
  #in local_modelling.R
  #Input:Health board capacity.csv
  #Output:Hospital/ICU bed capacity by health board
  
  "Health board capacity.csv" %>% 
    get_filepath("Data") %>% 
    read_csv %>% 
    gather(Status, Capacity, Hospital, ICU) %>% 
    rename(HBName = `Health board`)
}

output_csv = function(bed_demand_by_area, Type){
  #in local_modelling.R
  #Input: a table containign bed demand forecast for either Hospital or ICU
  #Output: a csv containing the table of bed demand forecasts
  
  bed_demand_by_area %>%
    filter(Status == Type) %>%
    #distinct %>%
    spread(Date, area_bed_demand) %>%
    arrange(bound,HBName,Area) %>%
    mutate(Area = if_else(Area == 1, HBName, str_c("   ",Area))) %>%
    select(-HBName)%>%
    write_csv(str_c("./Outputs/",Type,".csv"))
}

make_sheet_1 = function(current_beds_date, source){
  #in local_modelling.R
  #Input: local projections
  #Output: a table containing the case and exceedance projections reformatted 
  #for the NHS spreadsheet
  
  cases = get_local_projections(source, "cases")
  
  target_date = (ymd(current_beds_date) %>% floor_date("weeks")) + 14
  
  if(! target_date %in% cases$period_start) {
    warning(target_date %>% str_c(" not in data. It may be out of date. Using ", max(cases$period_start), "."))
    
    target_date = max(cases$period_start)
  }
  
  cases = cases %>%
    filter(period_start == target_date) %>%
    left_join(get_population(Local_authority),
              by = c("Area"="Local_authority")) %>%
    transmute(Area,
              `Estimated cases per 100,000 - low` = round(CIlow,0),
              `Estimated cases per 100,000 - high` = round(CIup,0),
              Population,
              `Estimated cases - low` = round(CIlow*(Population/100000),0),
              `Estimated cases - high` = round(CIup*(Population/100000),0))
  
  exceedance = get_local_projections(source, "exceedance") %>%
    filter(period_start == target_date) %>%
    select(-`P(Cases>600)`,
           # -`P(Cases>1500)`,
           -`P(Cases>2000)`,
           -`P(Cases>2500)`,
           -period_start) %>%
    gather(hotspot, Number, -Area) %>%
    # AS: Changed right to FALSE so the intervals are closed on the left and open on the right,
    # i.e. 15% rounds to 15-25% not 5-15%.  This matches what's in the research findings.
    mutate(Number = cut(Number, 
                        breaks = c(-0.01, 0.0005, 0.0015, 0.0025, 0.005, 0.0075, 0.02),
                        labels = c("0-5%", "5-15%", "15-25%", "25-50%", "50-75%", "75-100%"),
                        right = FALSE
                       )
          ) %>%
    spread(hotspot, Number) %>%
    select("Area", "P(Cases>20)", "P(Cases>50)", "P(Cases>100)", "P(Cases>150)", "P(Cases>300)", "P(Cases>500)", "P(Cases>750)", "P(Cases>1000)", "P(Cases>1500)", "P(Cases>3000)")
  
  table = full_join(cases,exceedance) %>%
    arrange(Area)
}

make_bed_demand_sheet = function(bed_demand_by_area, Type){
  #in local_modelling.R
  #Input: bed demand projections for either Hospital or ICU
  #Output: a table with the bed demand projections reformatted for the NHS spreadsheet
  
  
  
  bed_demand_by_area %>%
    mutate(area_bed_demand = round(area_bed_demand,0)) %>%
    filter(Status == Type) %>%
    arrange(bound, HBName, Area, Date) %>%
    mutate(Area = if_else(Area == 1, str_c("1", HBName), str_c(Area))) %>%
    mutate(area_bed_demand = if_else(area_bed_demand < 5,
                                     if_else(bound=="down",
                                             "0",
                                             if_else(bound=="middle",
                                                     "*",
                                                     if_else(bound=="up",
                                                             "4",
                                                             "unknown"))),
                                     as.character(area_bed_demand))) %>%
    spread(bound, area_bed_demand) %>%
    mutate(output=str_c(middle," (",down,"-",up,")")) %>%
    select(-down, -middle, -up, -planning) %>%
    spread(Date, output) %>%
    arrange(HBName,Area) %>%
    mutate(Area = if_else(str_detect(Area,"1"),str_replace(Area,"1",""), str_c("   ",Area))) %>%
    select(-HBName, -Status)
}



# Code to get hospital and ICU occupancy from the national projections.
# I've pulled this out of get_outputs to avoid duplication as I now use it in the Shiny app as well.
get_total_bed_demand = function(n6w = n6w){
  #in local_modelling.R
  #Input: national level bed demand projections, n6w (the percentiles to use for each week)
  #Output: national level bed demand projections in the required format for get_outputs and the Shiny app.
  "Modelling - Data - 12 week scenarios.xlsx" %>%
    get_filepath() %>%
    read_excel(sheet = "Data") %>%
    mutate(Date = as.Date(Date)) %>%
    rename(total_bed_demand=Number) %>%
    spread(Level, total_bed_demand) %>%
    mutate(`5th_25th` = (`5th percentile` + `25th percentile`)/2,
           `25th_med` = (`25th percentile` + `Median`)/2,
           `med_75th` = (`Median` + `75th percentile`)/2,
           `75th_95th` = (`75th percentile` + `95th percentile`)/2) %>%
    gather(Level, total_bed_demand, -Date, -`Scenario_name`, -Status, -Measure) %>%
    unite(scenario_level, Scenario_name, Level, sep="SEPARATE") %>%
    spread(scenario_level, total_bed_demand) %>%
    {if("Central" %in% n6w$Scenario_name)
      mutate(., `Better_CentralSEPARATEMedian` = (`BetterSEPARATEMedian` + `CentralSEPARATEMedian`)/2)
     else .
    } %>%
    {if("Worse" %in% n6w$Scenario_name)
      mutate(., `Better_WorseSEPARATEMedian` = (`BetterSEPARATEMedian` + `WorseSEPARATEMedian`)/2)
     else .
    } %>%
    gather(scenario_level, total_bed_demand, -Date, -Status, -Measure) %>%
    separate(scenario_level, into=c("Scenario_name", "Level"), sep="SEPARATE") %>%
    right_join(n6w) %>%
    select(Date,Status, bound, total_bed_demand)
}



get_outputs = function(n6w, current_beds_date, source){
  #in local_modelling.R
  #Input:national level bed demand projections, Imperial cases projections, n6w (the percentiles to use for each week)
  #Output: bed demand projections broken down by area (LA and HB)
  
  local_authorities = "LA_to_HB_lookup.xlsx" %>% 
    get_filepath("Data") %>% 
    read_excel %>% 
    select(LA_Name) %>%
    pull
  
  total_bed_demand = get_total_bed_demand(n6w = n6w)
  
  #####
  
  #latest week forecast inlcuding cases and probabilites of outbreaks
   data = get_local_projections(source, "cases")
  
  dates = data %>%
    select(period_start) %>%
    unique %>%
    pull

  data = data %>%
    complete(Area = local_authorities,
             period_start = dates)

  proportion_cases_la = data %>%
    full_join(tibble(Date = seq(from=as.Date(cut(ymd(current_beds_date),"week",start.on.monday=TRUE)),
                                by=7,
                                length.out=7),
                     period_start = c(
                       (ymd(current_beds_date) %>% floor_date("weeks")) + 7,
                       (ymd(current_beds_date) %>% floor_date("weeks")) + 7,
                       (ymd(current_beds_date) %>% floor_date("weeks")) + 7,
                       (ymd(current_beds_date) %>% floor_date("weeks")) + 7,
                       (ymd(current_beds_date) %>% floor_date("weeks")) + 14,
                       (ymd(current_beds_date) %>% floor_date("weeks")) + 14,
                       (ymd(current_beds_date) %>% floor_date("weeks")) + 14)),
              by=c("period_start")) %>%
    filter(!is.na(Date)) %>%
    select(Area,
           value,
           Date) %>%
    #Get Health board names
    left_join("LA_to_HB_lookup.xlsx" %>%
                 get_filepath("Data") %>%
                 read_excel %>%
                 select(LA_Name, HBName),
               by=c("Area"="LA_Name")) %>%
    #Get population
    left_join(get_population(Local_authority),
              by=c("Area"="Local_authority")) %>%
    group_by(Date) %>%
    mutate(prop_total_cases = if_else(value == 0, 0, (value*(Population/100000))/sum(value*(Population/100000),na.rm =TRUE))) %>%
    group_by(HBName, Date) %>%
    mutate(prop_HB_cases = if_else(value == 0, 0, (value*(Population/100000))/sum(value*(Population/100000),na.rm =TRUE))) %>%
    ungroup %>%
    select(-value, -Population)
  
  proportion_cases_hb = proportion_cases_la %>%
    group_by(HBName, Date) %>%
    summarise(prop_total_cases = sum(prop_total_cases),
              prop_HB_cases = sum(prop_HB_cases)) %>%
    ungroup %>%
    mutate(Area = "1")
  
  proportions_forecast = bind_rows(proportion_cases_la,
                          proportion_cases_hb) %>%
    full_join(get_beds_actuals(current_beds_date) %>%
                #repeat hospital values for deaths
                select(-Number,
                       -Date) %>%
                mutate(Status = ifelse(Status=="Hospital",
                                       list(c("Hospital","Mortality")),
                                       "ICU")) %>%
                unnest,
              by=c("HBName")) %>%
    mutate(prop_current_beds = if_else(HBName %in% c("Shetland", 
                                                     "Western Isles"),
                                       HB_prop_current_beds,
                                       prop_HB_cases * HB_prop_current_beds)) %>%
    select(-prop_HB_cases, -HB_prop_current_beds) %>%
    rowwise() %>%
    mutate(down = min(prop_current_beds, prop_total_cases, na.rm = TRUE)# %>% adjust_lower_bound(5, 10)
           ,
           up = max(prop_current_beds, prop_total_cases, na.rm = TRUE)# %>% adjust_lower_bound(5, 10)
           ,
           #Choose best estimate proportion
           middle = sum(prop_current_beds, prop_total_cases, na.rm = TRUE)/2 #%>% adjust_lower_bound(5, 10)
           ,
           #middle = pmin(prop_current_beds,prop_total_cases, na.rm = TRUE) %>% adjust_lower_bound(5, 10),
           planning = max(prop_current_beds, prop_total_cases, na.rm = TRUE)
           #planning = sum(prop_current_beds, prop_total_cases, na.rm = TRUE)/2 #%>% adjust_lower_bound(5, 10)
    ) %>%
    ungroup() %>%
    select(-prop_current_beds,-prop_total_cases) %>%
    gather(bound, prop_bed_demand, -Area, -HBName, -Status, -Date)
  
  # Do it all again but using current cases instead of forecast cases, and average the proportions from each.
  
  #############################################################################
  #If using current cases instead of forecast cases
  # AS: We're no longer getting LocalAuth Time Series so I've changed this to read in trend_la from:
  # https://www.opendata.nhs.scot/dataset/covid-19-in-scotland
  proportion_cases_la = "trend_la.csv" %>%
    get_filepath("Data") %>% 
    read_csv() %>% 
    transmute(Date = as.Date(Date), Local_authority = CAName, Tests = DailyPositive) %>% 
    filter(Date==max(Date),
           Local_authority!="Unknown") %>%
    rename(value=Tests,
           Area=Local_authority) %>%
    left_join("LA_to_HB_lookup.xlsx" %>%
                get_filepath("Data") %>%
                read_excel %>%
                select(LA_Name, HBName),
              by=c("Area"="LA_Name")) %>%
    mutate(prop_total_cases = if_else(value == 0, 0, (value/sum(value,na.rm =TRUE)))) %>%
    group_by(HBName) %>%
    mutate(prop_HB_cases = if_else(value == 0, 0, (value/sum(value,na.rm =TRUE)))) %>%
    ungroup %>%
    select(-value, -Date) %>%
    crossing(Date = seq(from=as.Date(cut(ymd(current_beds_date),"week",start.on.monday=TRUE)),
                        by=7,
                        length.out=7))
  ################################################################################
  
  dates = data %>%
    select(period_start) %>%
    unique %>%
    pull
  
  data = data %>%
    complete(Area = local_authorities,
             period_start = dates)
  
  proportion_cases_hb = proportion_cases_la %>%
    group_by(HBName, Date) %>%
    summarise(prop_total_cases = sum(prop_total_cases),
              prop_HB_cases = sum(prop_HB_cases)) %>%
    ungroup %>%
    mutate(Area = "1")
  
  proportions_current = bind_rows(proportion_cases_la,
                          proportion_cases_hb) %>%
    full_join(get_beds_actuals(current_beds_date) %>%
                #repeat hospital values for deaths
                select(-Number,
                       -Date) %>%
                mutate(Status = ifelse(Status=="Hospital",
                                       list(c("Hospital","Mortality")),
                                       "ICU")) %>%
                unnest,
              by=c("HBName")) %>%
    mutate(prop_current_beds = if_else(HBName %in% c("Shetland", 
                                                     "Western Isles"),
                                       HB_prop_current_beds,
                                       prop_HB_cases * HB_prop_current_beds)) %>%
    select(-prop_HB_cases, -HB_prop_current_beds) %>%
    rowwise() %>%
    mutate(down = min(prop_current_beds, prop_total_cases, na.rm = TRUE)# %>% adjust_lower_bound(5, 10)
           ,
           up = max(prop_current_beds, prop_total_cases, na.rm = TRUE)# %>% adjust_lower_bound(5, 10)
           ,
           #Choose best estimate proportion
           middle = sum(prop_current_beds, prop_total_cases, na.rm = TRUE)/2 #%>% adjust_lower_bound(5, 10)
           ,
           #middle = pmin(prop_current_beds,prop_total_cases, na.rm = TRUE) %>% adjust_lower_bound(5, 10),
           planning = max(prop_current_beds, prop_total_cases, na.rm = TRUE)
           #planning = sum(prop_current_beds, prop_total_cases, na.rm = TRUE)/2 #%>% adjust_lower_bound(5, 10)
    ) %>%
    ungroup() %>%
    select(-prop_current_beds,-prop_total_cases) %>%
    gather(bound, prop_bed_demand, -Area, -HBName, -Status, -Date)
  
  proportions = proportions_forecast %>% 
    full_join(proportions_current, by = c("Area", "HBName", "Status", "Date", "bound") ) %>% 
    rowwise() %>% 
    mutate(prop_bed_demand = ifelse(bound == "down",
                                    min(prop_bed_demand.x, prop_bed_demand.y),
                                    ifelse(bound == "middle",
                                           mean(c(prop_bed_demand.x, prop_bed_demand.y) ),
                                           ifelse(bound == "planning",
                                                  max(prop_bed_demand.x, prop_bed_demand.y),
                                                  ifelse(bound == "up",
                                                         max(prop_bed_demand.x, prop_bed_demand.y),
                                                         stop("Problem with averaging.  Check get_outputs.")
                                                        )
                                                 )
                                          )
                                   )
          ) %>% 
    ungroup() %>% 
    select(-prop_bed_demand.x, -prop_bed_demand.y)
  
  # End of averaging code.
  
  proportions %>%
    right_join(total_bed_demand, by=c("Status","bound","Date")) %>%
    filter(! Area %in% c("Scottish Borders",
                         "Fife",
                         "Orkney Islands",
                         "Shetland Islands",
                         "Dumfries and Galloway",
                         "Na h-Eileanan Siar")) %>%
    transmute(Area,
              HBName,
              Date,
              Status,
              bound,
              area_bed_demand = (prop_bed_demand*total_bed_demand))
}

run_local_modelling_app = function(current_beds_date, source){
  #in local_modelling.R
  #Input: date of the bed occupancy actuals data spreadsheet
  #Output: an app to help choose which percentiles to use, and to produce output based on these chosen percentiles
  # Run the application 
  with_SCHEMa_options(
    app = shinyApp(ui = indicators_ui, server = indicators_server), 
    SCHEMa_opts = list(current_beds_date = current_beds_date,
                       # AS: This allows us to specify the data source (Combined or Imperial) in the
                       # call to run_local_modelling_app rather than having to change the code.
                       source = source
                      )
  )
}

# Define UI for application that draws a histogram
indicators_ui = fluidPage(
  tabsetPanel(
    tabPanel("Hospital",
             h5("Use the table below to choose which scenario and percentiles to use for each week of the projections."),
             rHandsontableOutput("hosp_levels_table"),
             h5("Use the plot button to plot the projections. The summed projections over all local authorities will be shown, along with the current bed occupancy. Adjust the percentiles in the table above as necessary, and re-plot."),
             actionButton("apply_hosp_changes",
                          "Plot"),
             plotOutput("hosp_plot"),
             plotOutput("hosp_plot_national")
    ),
    tabPanel("ICU",
             h5("Use the table below to choose which scenario and percentiles to use for each week of the projections."),
             rHandsontableOutput("icu_levels_table"),
             h5("Use the plot button to plot the projections. The summed projections over all local authorities will be shown, along with the current bed occupancy. Adjust the percentiles in the table above as necessary, and re-plot."),
             actionButton("apply_icu_changes",
                          "Plot"),
             plotOutput("icu_plot"),
             plotOutput("icu_plot_national")
    ),
    tabPanel("Deaths",
             h5("Use the table below to choose which scenario and percentiles to use for each week of the projections."),
             rHandsontableOutput("deaths_levels_table"),
             h5("Use the plot button to plot the projections. The sum over all local authorities will be shown."),
             actionButton("apply_deaths_changes",
                          "Plot"),
             plotOutput("deaths_plot")
    ),
    tabPanel("Produce output",
             actionButton("output_button",
                          "Produce output"),
             actionButton("output_deaths_button",
                          "Produce deaths output")
    )
  )
)

# Define server logic required to draw a histogram
indicators_server <- function(input, output, session) {
  #source("Inputs.R")
  #source("functions.R")
  
  current_beds_date = get_SCHEMa_options("current_beds_date")
  
  # AS: This allows us to specify the data source (Combined or Imperial) in the
  # call to run_local_modelling_app rather than having to change the code.
  source = get_SCHEMa_options("source")
  
  #Table to filter the bed forecasts by to get the projections required
  # AS: Now we're using the actuals, last week and the next two weeks will be the same for all scenarios and percentiles 
  # except the 5th and 95th percentiles.  To ensure we have a range for the Week 1 projections, set Down to the 5th_25th percentile, 
  # Up to the 75th_95th percentile, and Middle and Planning to the Median.  The scenario doesn't matter so set it to Central.
  hosp_levels_default = expand_grid(Date = cut(c(ymd(current_beds_date) - 7, ymd(current_beds_date), ymd(current_beds_date) + 7), "week") %>% as.Date,
                                    Status = "Hospital",
                                    bound = c("down", "middle", "up", "planning"),
                                    Scenario_name = "Central"
                                   ) %>% 
    left_join(tibble(bound = c("down", "middle", "up", "planning"),
                     Level = c("5th_25th", "Median", "75th_95th", "Median"),
                     Measure = "Prevalence"
                    )
             ) %>% 
    bind_rows("hospital_levels_used.csv" %>%
                get_filepath() %>%
                read_csv() %>%
                mutate(Date = as.Date(Date, format="%d/%m/%Y") ) %>% 
                filter(Date >= min(Date) + 28)
             ) %>% 
    bind_rows("hospital_levels_used.csv" %>%
                get_filepath() %>%
                read_csv() %>% 
                filter(Date == max(Date) ) %>% 
                mutate(Date = as.Date(Date, format="%d/%m/%Y") + 7)
             )
  
  icu_levels_default = expand_grid(Date = cut(c(ymd(current_beds_date) - 7, ymd(current_beds_date), ymd(current_beds_date) + 7), "week") %>% as.Date,
                                   Status = "ICU",
                                   bound = c("down", "middle", "up", "planning"),
                                   Scenario_name = "Central"
                                  ) %>% 
    left_join(tibble(bound = c("down", "middle", "up", "planning"),
                     Level = c("5th_25th", "Median", "75th_95th", "Median"),
                     Measure = "Prevalence"
                    )
             ) %>% 
    bind_rows("icu_levels_used.csv" %>%
                get_filepath() %>%
                read_csv() %>%
                mutate(Date = as.Date(Date, format="%d/%m/%Y") ) %>% 
                filter(Date >= min(Date) + 28)
             ) %>% 
    bind_rows("icu_levels_used.csv" %>%
                get_filepath() %>%
                read_csv() %>% 
                filter(Date == max(Date) ) %>% 
                mutate(Date = as.Date(Date, format="%d/%m/%Y") + 7)
             )
  
  deaths_levels_default = "deaths_levels_used.csv" %>%
    get_filepath() %>%
    read_csv() %>%
    mutate(Date = as.Date(Date, format="%d/%m/%Y") ) %>% 
    filter(Date != min(Date) ) %>% 
    bind_rows("deaths_levels_used.csv" %>%
                get_filepath() %>%
                read_csv() %>% 
                filter(Date == max(Date) ) %>% 
                mutate(Date = as.Date(Date, format="%d/%m/%Y") + 7)
             )
  
  #links to the interactive table
  hosp_levels = reactive({
    temp = input$hosp_levels_table
    if(length(temp) > 0) temp %>% hot_to_r
    else hosp_levels_default
    
  })
  
  #links to the interactive table
  icu_levels = reactive({
    temp = input$icu_levels_table
    if(length(temp) > 0) temp %>% hot_to_r
    else icu_levels_default
    
  })
  
  #links to the interactive table
  deaths_levels = reactive({
    temp = input$deaths_levels_table
    if(length(temp) > 0) temp %>% hot_to_r
    else deaths_levels_default
    
  })
  
  #creates the interactive table
  output$hosp_levels_table = renderRHandsontable({
    hosp_levels() %>%
      #mutate(start_date = start_date %>% format("%d-%m-%y")) %>%
      rhandsontable %>%
      hot_col("Date", dateFormat = "DD-MM-YY", type = "date") %>%
      hot_col("Status") %>%
      hot_col("bound") %>%
      hot_col("Scenario_name") %>%
      hot_col("Level", type = "dropdown", source = c("5th percentile",
                                                     "25th percentile",
                                                     "Median",
                                                     "75th percentile",
                                                     "95th percentile")) %>%
      hot_col("Measure", type = "dropdown", source = c("Prevalence"))
  })
  
  #creates the interactive table
  output$icu_levels_table = renderRHandsontable({
    icu_levels() %>%
      #mutate(start_date = start_date %>% format("%d-%m-%y")) %>%
      rhandsontable %>%
      hot_col("Date", dateFormat = "DD-MM-YY", type = "date") %>%
      hot_col("Status") %>%
      hot_col("bound") %>%
      hot_col("Scenario_name") %>%
      hot_col("Level", type = "dropdown", source = c("5th percentile",
                                                     "25th percentile",
                                                     "Median",
                                                     "75th percentile",
                                                     "95th percentile")) %>%
      hot_col("Measure", type = "dropdown", source = c("Prevalence"))
  })
  
  #creates the interactive table
  output$deaths_levels_table = renderRHandsontable({
    deaths_levels() %>%
      #mutate(start_date = start_date %>% format("%d-%m-%y")) %>%
      rhandsontable %>%
      hot_col("Date", dateFormat = "DD-MM-YY", type = "date") %>%
      hot_col("Status") %>%
      hot_col("bound") %>%
      hot_col("Scenario_name") %>%
      hot_col("Level", type = "dropdown", source = c("5th percentile",
                                                     "25th percentile",
                                                     "Median",
                                                     "75th percentile",
                                                     "95th percentile")) %>%
      hot_col("Measure", type = "dropdown", source = c("Prevalence"))
  })
  
  
  hosp_bed_demand_by_area = eventReactive(
    input$apply_hosp_changes, {
      get_outputs(n6w=hosp_levels(),current_beds_date, source) %>%
        spread(bound, area_bed_demand) %>%
        mutate(down = down %>% adjust_lower_bound(5, 10),
               up = up %>% adjust_upper_bound(5,10)) %>%
        gather(bound, area_bed_demand, -Area, -HBName, -Status, -Date)
    })
  
  # Make a plot using the national hospitalisation projections rather than summing the local projections.
  hosp_bed_demand_national = eventReactive(
    input$apply_hosp_changes, {
      get_total_bed_demand(n6w = hosp_levels() ) %>% 
        filter(Date >= current_beds_date %>% ymd %>% floor_date(unit = "weeks", week_start = 1) )
    })
  
  icu_bed_demand_by_area = eventReactive(
    input$apply_icu_changes, {
      get_outputs(n6w=icu_levels(), current_beds_date, source) %>%
        spread(bound, area_bed_demand) %>%
        mutate(down = down %>% adjust_lower_bound(5, 10),
               up = up %>% adjust_upper_bound(5,10)) %>%
        gather(bound, area_bed_demand, -Area, -HBName, -Status, -Date)
    })
  
  # Make a plot using the national ICU projections rather than summing the local projections.
  icu_bed_demand_national = eventReactive(
    input$apply_icu_changes, {
      get_total_bed_demand(n6w = icu_levels() ) %>% 
        filter(Date >= current_beds_date %>% ymd %>% floor_date(unit = "weeks", week_start = 1) )
    })
  
  deaths_bed_demand_by_area = eventReactive(
    input$apply_deaths_changes, {
      get_outputs(n6w=deaths_levels(), current_beds_date, source)
    })
  
  output$hosp_plot = renderPlot({
    hosp_bed_demand_by_area() %>%
      filter(Area == 1) %>%
      group_by(Date, Status, bound) %>%
      summarize(area_bed_demand = sum(area_bed_demand)) %>%
      spread(bound, area_bed_demand) %>%
      ggplot(aes(x=Date,
                 #y=area_bed_demand,
                 group=Status,
                 colour=Status)) +
      geom_ribbon(aes(ymin = down,
                      ymax = up),
                  fill = "grey80"
      )+
      geom_line(aes(y=middle)) +
      geom_line(aes(y=planning)) +
      geom_point(data=get_beds_actuals(current_beds_date) %>%
                   filter(Status=="Hospital") %>%
                   group_by(Date, Status) %>%
                   summarize(Number = sum(Number)) %>%
                   ungroup(),
                 aes(x=Date,y=Number)) +
      theme_light() +
      scale_x_date(date_breaks = "weeks", date_labels = "%b-%d") +
      ggtitle("Hospital bed demand") +
      ylab("Bed demand")
  })
  
  # Output a plot using the national hospital occupancy projections rather than summing the local projections.
  output$hosp_plot_national = renderPlot({
    hosp_bed_demand_national() %>%
      spread(bound, total_bed_demand) %>%
      ggplot(aes(x=Date,
                 group=Status,
                 colour=Status)) +
      geom_ribbon(aes(ymin = down,
                      ymax = up),
                  fill = "grey80"
      )+
      geom_line(aes(y=middle)) +
      geom_line(aes(y=planning)) +
      geom_point(data=get_beds_actuals(current_beds_date) %>%
                   filter(Status=="Hospital") %>%
                   group_by(Date, Status) %>%
                   summarize(Number = sum(Number)) %>%
                   ungroup(),
                 aes(x=Date,y=Number)) +
      theme_light() +
      scale_x_date(date_breaks = "weeks", date_labels = "%b-%d") +
      ggtitle("National hospital bed demand") +
      ylab("Bed demand")
  })
  
  output$icu_plot = renderPlot({
    icu_bed_demand_by_area() %>%
      filter(Area == 1) %>%
      group_by(Date, Status, bound) %>%
      summarize(area_bed_demand = sum(area_bed_demand)) %>%
      spread(bound, area_bed_demand) %>%
      ggplot(aes(x=Date,
                 #y=area_bed_demand,
                 group=Status,
                 colour=Status)) +
      geom_ribbon(aes(ymin = down,
                      ymax = up),
                  fill = "grey80"
      )+
      geom_line(aes(y=middle)) +
      geom_line(aes(y=planning)) +
      # AS: Changed to use SICSAG data as that's what the projections are based on.
      geom_point(data = "ICU_history.rds" %>% 
                   get_filepath() %>% 
                   readRDS() %>% 
                   filter(Date == ymd(current_beds_date), ValueType == "Prevalence") %>% 
                   transmute(Date, Status = "ICU", Number),
                 # get_beds_actuals(current_beds_date) %>%
                 # filter(Status=="ICU") %>%
                 # group_by(Date, Status) %>%
                 # summarize(Number = sum(Number)) %>%
                 # ungroup(),
                 aes(x=Date,y=Number)) +
      theme_light() +
      scale_x_date(date_breaks = "weeks", date_labels = "%b-%d") +
      ggtitle("ICU bed demand") +
      ylab("Bed demand")
  })
  
  # Output a plot using the national ICU occupancy projections rather than summing the local projections.
  output$icu_plot_national = renderPlot({
    icu_bed_demand_national() %>%
      spread(bound, total_bed_demand) %>%
      ggplot(aes(x=Date,
                 group=Status,
                 colour=Status)) +
      geom_ribbon(aes(ymin = down,
                      ymax = up),
                  fill = "grey80"
      )+
      geom_line(aes(y=middle)) +
      geom_line(aes(y=planning)) +
      # AS: Changed to use SICSAG data as that's what the projections are based on.
      geom_point(data = "ICU_history.rds" %>% 
                   get_filepath() %>% 
                   readRDS() %>% 
                   filter(Date == ymd(current_beds_date), ValueType == "Prevalence") %>% 
                   transmute(Date, Status = "ICU", Number),
                   # get_beds_actuals(current_beds_date) %>%
                   # filter(Status=="ICU") %>%
                   # group_by(Date, Status) %>%
                   # summarize(Number = sum(Number)) %>%
                   # ungroup(),
                 aes(x=Date,y=Number)) +
      theme_light() +
      scale_x_date(date_breaks = "weeks", date_labels = "%b-%d") +
      ggtitle("National ICU bed demand") +
      ylab("Bed demand")
  })
  
  output$deaths_plot = renderPlot({
    deaths_bed_demand_by_area() %>%
      filter(Area == 1) %>%
      group_by(Date, Status, bound) %>%
      summarize(area_bed_demand = sum(area_bed_demand)) %>%
      spread(bound, area_bed_demand) %>%
      ggplot(aes(x=Date,
                 #y=area_bed_demand,
                 group=Status,
                 colour=Status)) +
      geom_ribbon(aes(ymin = down,
                      ymax = up),
                  fill = "grey80"
      )+
      geom_line(aes(y=middle)) +
      geom_line(aes(y=planning)) +
      theme_light() +
      scale_x_date(date_breaks = "weeks", date_labels = "%b-%d") +
      ggtitle("Mortality") +
      ylab("Mortality")
  })
  
  observeEvent(input$output_button, {
    message("Writing the percentiles used for Hospitals to csv")
    
    write_csv(isolate(hosp_levels()),
              "Outputs/hospital_levels_used.csv")
    
    message("Writing the percentiles used for ICU to csv")
    write_csv(isolate(icu_levels()),
              "Outputs/icu_levels_used.csv")
    
    message(current_beds_date)
    
    message("Writing projections to NHS excel spreadsheet")
    save_to_spreadsheet(hosp_bed_demand_by_area() %>%
                          filter(Date != min(Date)),
                        icu_bed_demand_by_area() %>%
                          filter(Date != min(Date)),
                        current_beds_date,
                        source)
    # message("14")
    # output_csv(hosp_bed_demand_by_area() %>%
    #              filter(Date != min(Date)), "Hospital")
    # message("15")
    # output_csv(icu_bed_demand_by_area() %>%
    #              filter(Date != min(Date)), "ICU")
    message("Saving hospital projections")
    hosp_bed_demand_by_area = "hosp_bed_demand_by_area.rds" %>%
      get_filepath() %>%
      readRDS() %>%
      #mutate(bound = if_else(bound=="middle","planning",bound)) %>%
      bind_rows(hosp_bed_demand_by_area() %>%
                  mutate(Creation_date = ymd(current_beds_date)))
    
    saveRDS(hosp_bed_demand_by_area,
            "Outputs/hosp_bed_demand_by_area.rds")
    
    message("Saving ICU projections")
    icu_bed_demand_by_area = "icu_bed_demand_by_area.rds" %>%
      get_filepath() %>%
      readRDS() %>%
      #mutate(bound = if_else(bound=="middle","planning",bound)) %>%
      bind_rows(icu_bed_demand_by_area() %>%
                  mutate(Creation_date = ymd(current_beds_date)))
    
    saveRDS(icu_bed_demand_by_area,
            "Outputs/icu_bed_demand_by_area.rds")
    
    # output_levels_table(hosp_bed_demand_by_area,
    #                     "Hospital",
    #                     current_beds_date)
    # 
    # output_levels_table(icu_bed_demand_by_area,
    #                     "ICU",
    #                     current_beds_date)
    
    showModal( modalDialog(
      title = "Success",
      "Output saved",
      footer = NULL,
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$output_deaths_button, {
    message("Writing deaths projections to csv")
    output_csv(deaths_bed_demand_by_area() %>%
                 filter(Date != min(Date)), "Mortality")
    
    output_mortality(deaths_bed_demand_by_area() %>%
                       filter(Date != min(Date)))
    
    
    
    
    
    message("Writing the percentiles used for deaths to csv")
    write_csv(isolate(deaths_levels()),
              "Outputs/deaths_levels_used.csv")
    
    showModal( modalDialog(
      title = "Success",
      "Output saved",
      footer = NULL,
      easyClose = TRUE
    ))
  })
  
}


make_nhs_table=function(df, type, current_beds_date){
  
  occupancy = get_beds_actuals(current_beds_date) %>%
    select(-HB_prop_current_beds, -Date) %>%
    rename(Occupancy = Number) %>%
    mutate(Creation_date = ymd(current_beds_date)) %>%
    filter(Status==type) %>%
    select(HBName, Occupancy) %>%
    mutate(Occupancy = if_else(Occupancy<5,"*",as.character(Occupancy)))
  
  df %>%
    mutate(Date = str_c((Date - as.Date(cut(ymd(current_beds_date),"week",start.on.monday=TRUE)))/7,"_week"),
           bound = bound %>% recode(up = "up",
                                    down = "down",
                                    middle = "Best projection",
                                    planning = "Planning projection"),
           area_bed_demand = round(area_bed_demand,0),
           # AS: Suppress area_bed_demand as well as Occupancy if it's below 5.
           area_bed_demand = if_else(area_bed_demand < 5, "<5", as.character(area_bed_demand) ) ) %>%
    filter(Area == 1,
           Date %in% c("1_week", "2_week", "3_week", "4_week", "5_week", "6_week")) %>%
    select(-Area, -Status) %>%
    unite(bound_date, bound, Date) %>%
    spread(bound_date, area_bed_demand) %>%
    unite(`Projection full range_1_week`,`down_1_week`,`up_1_week`, sep="-") %>%
    unite(`Projection full range_2_week`,`down_2_week`,`up_2_week`,sep="-") %>%
    unite(`Projection full range_3_week`,`down_3_week`,`up_3_week`,sep="-") %>%
    unite(`Projection full range_4_week`,`down_4_week`,`up_4_week`,sep="-") %>%
    # unite(`Projection full range_5_week`,`down_5_week`,`up_5_week`,sep="-") %>%
    # unite(`Projection full range_6_week`,`down_6_week`,`up_6_week`,sep="-") %>%
    left_join(occupancy) %>%
    select(Occupancy,`Best projection_1_week`, `Planning projection_1_week`,`Projection full range_1_week`,
           `Best projection_2_week`, `Planning projection_2_week`,`Projection full range_2_week`,
           `Best projection_3_week`, `Planning projection_3_week`,`Projection full range_3_week`,
           `Best projection_4_week`, `Planning projection_4_week`,`Projection full range_4_week`) # ,
           # `Best projection_5_week`, `Planning projection_5_week`,`Projection full range_5_week`) # ,
           # `Best projection_6_week`, `Planning projection_6_week`,`Projection full range_6_week`)
}

get_local_projections = function(source, type){
  #Input: Source of the local projections
  #Output: Either cases estimates, probabilties of exceeding certain values, or date the data was updated
  
  # AS: The format of the data is now the same whether it's the combined data or Imperial only,
  # so I've removed the outermost if statement and now use source in the filters.
  
    if(type == "cases"){
      data = "combined_all_data_weekly_cases_per_100K.xlsx" %>%
        get_filepath() %>%
        read_excel()
      
      data %>% 
        filter(Group == source, ValueType == "weekly_cases_per_100k") %>%
        mutate(period_start = str_c(`Day of Value`, "-", `Month of Value`, "-", `Year of Value`) %>%
                 dmy()) %>%
        select(Area = Geography,
               value = `Quantile 0.5`,
               CIlow = `Quantile 0.25`,
               CIup = `Quantile 0.75`,
               period_start) %>%
        mutate(value = as.numeric(value),
               CIlow = as.numeric(CIlow),
               CIup = as.numeric(CIup))
      
    }
    else if(type == "exceedance"){
      
      sheets = "combined_exceedance_estimates.xlsx" %>%
        get_filepath() %>%
        excel_sheets()
      
      path = "combined_exceedance_estimates.xlsx" %>%
        get_filepath()
      
      sheets %>%
        map_dfr(function(sheet_name, path){
          read_excel(path=path,
                     sheet=sheet_name) %>%
            mutate(period_start = sheet_name)}, 
          path=path) %>%
        rename(Area="Geography") %>%
        filter(Area != "Scotland") %>% 
        arrange(period_start, Area) %>%
        select_all(~gsub("\\s+", "", .)) %>%
        mutate(`P(Cases>20)` = ((`P(Cases>20)` %>% str_replace("%","") %>% as.numeric)/100),
               `P(Cases>50)` = ((`P(Cases>50)` %>% str_replace("%","") %>% as.numeric)/100),
               `P(Cases>100)` = ((`P(Cases>100)` %>% str_replace("%","") %>% as.numeric)/100),
               `P(Cases>150)` = ((`P(Cases>150)` %>% str_replace("%","") %>% as.numeric)/100),
               `P(Cases>300)` = ((`P(Cases>300)` %>% str_replace("%","") %>% as.numeric)/100),
               `P(Cases>500)` = ((`P(Cases>500)` %>% str_replace("%","") %>% as.numeric)/100),
               `P(Cases>600)` = ((`P(Cases>600)` %>% str_replace("%","") %>% as.numeric)/100),
               `P(Cases>750)` = ((`P(Cases>750)` %>% str_replace("%","") %>% as.numeric)/100),
               `P(Cases>1000)` = ((`P(Cases>1000)` %>% str_replace("%","") %>% as.numeric)/100),
               `P(Cases>1500)` = ((`P(Cases>1500)` %>% str_replace("%","") %>% as.numeric)/100),
               `P(Cases>2000)` = ((`P(Cases>2000)` %>% str_replace("%","") %>% as.numeric)/100),
               `P(Cases>2500)` = ((`P(Cases>2500)` %>% str_replace("%","") %>% as.numeric)/100),
               `P(Cases>3000)` = ((`P(Cases>3000)` %>% str_replace("%","") %>% as.numeric)/100))
      
    }
    else if(type == "date_updated"){
      data = "combined_all_data_weekly_cases_per_100K.xlsx" %>%
        get_filepath() %>%
        read_excel()
      
      data %>% 
        filter(Group == source) %>%
        mutate(data_updated = str_c(`Creation Day`, "-", `Creation Month`, "-", `Creation Year`) %>%
                 dmy()) %>%
        select(data_updated) %>%
        slice(1) %>%
        pull
      
    }
    else{
      message("Enter either cases, probabilities, or date_updated in get_local_projections.")
    }
  
}


output_mortality = function(deaths){
  
  deaths = deaths %>% 
    filter(Area!="1" |
             Area=="1" & HBName %in% c("Dumfries and Galloway",
                                       "Fife",
                                       "Western Isles",
                                       "Orkney",
                                       "Borders",
                                       "Shetland")) %>%
    mutate(Area = if_else(Area=="1",
                          if_else(HBName=="Borders",
                                  "Scottish Border",
                                  if_else(HBName =="Western Isles",
                                          "Na h-Eileanan Siar",
                                          HBName)),
                          Area))
  
  
  middle = deaths %>%
    filter(bound %in% c("middle")) %>%
    select(Area, Date, area_bed_demand) %>%
    mutate(area_bed_demand = (7*area_bed_demand)) %>%
    spread(Date,area_bed_demand)
  
  up = deaths %>%
    filter(bound %in% c("up")) %>%
    select(Area, Date, area_bed_demand) %>%
    mutate(area_bed_demand = (7*area_bed_demand) %>% adjust_upper_bound(5,10)) %>%
    spread(Date,area_bed_demand) 
  
  template = "Modelling -  Local proxy data for deaths - Template.xlsx" %>%
    get_filepath("Output templates") %>%
    loadWorkbook() 
  
  middle %>% 
    writeData(template, sheet = "Model estimates weekly deaths", ., startCol = 1, startRow = 3, colNames = TRUE)
  
  up %>% 
    writeData(template, sheet = "Model estimates weekly deaths", ., startCol = 9, startRow = 3, colNames = TRUE)
  
  saveWorkbook(template, str_c("Outputs/Modelling -  Local proxy data for deaths.xlsx"), overwrite = TRUE)
  
}



get_local_cases = function(area = "HB", source) { #in local_modelling.R
  
  # AS: Calculate the forecast cases in each local authority or health board based on the cases per 100K file.
  
  cases_per_100k = "combined_all_data_weekly_cases_per_100K.xlsx" %>%
    get_filepath() %>%
    read_excel() %>% 
    filter(Group == source, ValueType == "weekly_cases_per_100k") %>%
    mutate(Date = str_c(`Day of Value`, "-", `Month of Value`, "-", `Year of Value`) %>% dmy) %>%
    select(Date,
           Local_authority = Geography,
           `Quantile 0.05`,
           `Quantile 0.25`,
           `Quantile 0.5`,
           `Quantile 0.75`,
           `Quantile 0.95`
          )
  
  population = get_population(Local_authority)
  
  cases_by_la = cases_per_100k %>% 
    left_join(population) %>% 
    mutate_at(c("Quantile 0.05", "Quantile 0.25", "Quantile 0.5", "Quantile 0.75", "Quantile 0.95"),
              ~ . / 100000 * Population
             ) %>% 
    select(-Population) %>% 
    arrange(Date)
  
  la_to_hb_dict = "LA_to_HB_lookup.xlsx" %>% 
    get_filepath("Data") %>% 
    read_excel %>% 
    select(Local_authority = LA_Name, Health_board = HBName)
  
  cases_by_hb = cases_by_la %>% 
    left_join(la_to_hb_dict) %>% 
    group_by(Date, Health_board) %>% 
    summarise_at(c("Quantile 0.05", "Quantile 0.25", "Quantile 0.5", "Quantile 0.75", "Quantile 0.95"), sum) %>% 
    ungroup
  
  if (area == "LA") cases_by_la
  
  if (area == "HB") cases_by_hb
  
}


