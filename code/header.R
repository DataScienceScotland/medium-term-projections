library(tidyverse)
library(lubridate)
library(readxl)
library(openxlsx)
library(viridis)
library(RColorBrewer)
library(RSQLite)

if(! "filepath_priority" %>% exists) filepath_priority = "production"
{ 
  if(filepath_priority == 'local') {
    source("Code/functions/helper_functions.R")
    source("Code/functions/setup.R")
    source("Code/functions/model_functions.R")
    source("Code/functions/population.R")
    source("Code/functions/output.R")
    source("Code/functions/prep_tables.R")
    source("Code/functions/prep_graphs.R")
    source("Code/functions/create_products.R")
    source("Code/functions/daily_update.R")
    source("Code/functions/epidemiology.R")
    source("Code/functions/interactive_model.R")
    source("Code/functions/local_modelling.R")
    source("Code/functions/vaccination.R")
    source("Code/functions/read_data.R")
  } 
  else if(filepath_priority == 'development') {
    source("x.R")
    source("x.R")
    source("x.R")
    source("x.R")
    source("x.R")
    source("x.R")
    source("x.R")
    source("x.R")
    source("x.R")
    source("x.R")
    source("x.R")
    source("x.R")
    source("x.R")
    source("x.R")
  } 
  else if(filepath_priority == 'production') {
    source("x.R")
    source("x.R")
    source("x.R")
    source("x.R")
    source("x.R")
    source("x.R")
    source("x.R")
    source("x.R")
    source("x.R")
    source("x.R")
    source("x.R")
    source("x.R")
  }
  else
    stop("Set filepath_priority to 'local', 'development' or 'production'")
}
