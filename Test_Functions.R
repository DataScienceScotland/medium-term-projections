#Test functions

#All functions used within model can be tested with the following test
#functions to ensure they are working properly

#Run header.R to get all functions required
source("x.R")

#Read in the following data for use in the below test function

timeline=readRDS("x")
model=readRDS("x")
population=readRDS("x")
proportions=readRDS("x")

#Read in initalisated data as below
length_of_infection=8
date_of_admission_from_infection_start=4
hospital_recovery_factor=1
cutoff=0

#After header.R has been sourced and the data items have been read in the test functions can be ran below.
#There is code at the bottom after these test functions which calls each of them.

#The test functions detail if the function is working correctly and if not details where the issues are. 
#If the functions have been recently updated or ames have been changed this will be flagged up as wrong.

#------------------------------------------------------------------
#Function name: get_pop_data()
#Description: Gets the data on population into a suitable format to
#be stored in pop_data

TF_get_pop_data=function() {
  TF_functionnotworking=0
  #Extract pop_data from the function
  TF_pop_data=get_pop_data()
  #Check that pop_data has the correct number of columns
  if (dim(TF_pop_data)[2]!=4) {
    TF_functionnotworking=1 
  }
  #Check that all the column names are correct
  if (!all( names(TF_pop_data)==c("Area code", "Area1", "Age" ,"Persons"))) {
    TF_functionnotworking=1
  }
  #Check that the entries are the right classes
  if (!all(lapply(TF_pop_data,class)==c("character", "character", "integer", "integer"))){
    TF_functionnotworking=1
  }
  #Check that the ages are in a suitable range
  if (!all(TF_pop_data$Age>=0 & TF_pop_data$Age<150)){
    TF_functionnotworking=1
  }
  #Check the Persons are in a suitable range
  if (!all(TF_pop_data$Persons>=0 & TF_pop_data$Persons<8000000000)){
    TF_functionnotworking=1
  }
  
  #Print message of if function is working correctly
  if (TF_functionnotworking==1){
    print("Function get_pop_data not working correctly")
  }
  else {
    print("Function get_pop_data working correctly")
  }
}
#------------------------------------------------------------------
#Function name: get_population()
#Description: Gets the total population for a given age range




#------------------------------------------------------------------

#Function name: run_timeline()
#Description: 

TF_run_timeline=function(length_of_infection,date_of_admission_from_infection_start,
                         hospital_recovery_factor, cutoff) {
  #8 4,1 and 0 are currently default input arguements for this function
    TF_timeline=run_timeline(length_of_infection,date_of_admission_from_infection_start, 
                 hospital_recovery_factor, cutoff)
  
    
    #Vector containing the issue with the function
    diagnosis_sol=c("Incorrect number of columns", "Incorrect column names",
                    "Incorrect class", "Incorrect values for Age_Group",
                    "Incorrect values for Day", "Incorrect values for ID",
                    "Incorrect values for Proportion_starting",
                    "Incorrect values for Proportion_at",
                    "Incorrect entry for severity",
                    "Incorrect entry for Status")
    #Empty vector to fill with if the named problem in sol vector is a problem
    diagnosis_prob=NULL
    
    #Check that timeline has the correct number of columns
    diagnosis_prob=c(diagnosis_prob,dim(TF_timeline)[2]!=7)
    
    #Check that all the column names are correct
    diagnosis_prob=c(diagnosis_prob,!all( names(TF_timeline)==c("ID",
                                    "Day",
                                    "Age_group",
                                    "Proportion_starting",
                                    "severity",
                                    "Status",
                                    "Proportion_at"
    )))
  
    #Check that the entries are the right classes
    diagnosis_prob=c(diagnosis_prob,!all(lapply(TF_timeline,class)==c("numeric", "numeric","numeric",
                                                                      "numeric","character", "character", "numeric")))
    
    #Check that each of the numeric entries as within a suitable range (first line for each column
    #checks that the range is suitable and the second checks that they are integers Or of the correct
    #whole number format if this is required)
    diagnosis_prob=c(diagnosis_prob,(!all(TF_timeline$Age_group>=0 & TF_timeline$Age_group<=90) ||
        !all(TF_timeline$Age_group/10==round(TF_timeline$Age_group/10))),
        (!all(TF_timeline$Day>=0 & TF_timeline$Day<=1000) ||
        !all(TF_timeline$Day==round(TF_timeline$Day))),
        (!all(TF_timeline$ID>=0 & TF_timeline$ID<=4) ||
        !all(TF_timeline$ID==round(TF_timeline$ID))),
        !all(TF_timeline$Proportion_starting>=0 & TF_timeline$Proportion_starting<=1, na.rm=TRUE),
        !all(TF_timeline$Proportion_at>=0 & TF_timeline$Proportion_at<=3, na.rm=TRUE))

    
    #Check that the character entries are of from the correct options
    diagnosis_prob=c(diagnosis_prob,!all(TF_timeline$severity %in% c( "Mild", "Moderate", "Serious", "Severe", "Critical")),
        !all(TF_timeline$Status %in% c( "Infected",
                                        "Oxygen",
                                        "Non-invasive ventilation",
                                        "Invasive ventilation",
                                        "Recovering",
                                        "Recovered",
                                        "Dead" )))
    
    
    if (!all(diagnosis_prob==FALSE)){
      print("Function run_timeline not working correctly because of the following issues:")
      index=which(diagnosis_prob==TRUE)
      print(diagnosis_sol[index])
    }
    else {
      print("Function run_timeline working correctly")
    }
}

#------------------------------------------------------------------
#Function name: run_model()
#Description: 


TF_run_model=function(Age_group, SIMD, Vulnerable, In_care) {
  TF_model=run_model(Age_group, SIMD, Vulnerable, In_care)
  
  #Vector containing the issue with the function
  diagnosis_sol=c("Incorrect number of columns", "Incorrect column names",
                  "Incorrect class", "Incorrect values for ID",
                  "Incorrect values for Age_group", "Incorrect values for Infection_rate",
                  "Incorrect values for mortality_rate",
                  "Incorrect entry for Scenario_name",
                  "Incorrect entry for SIMD",
                  "Incorrect entry for Vulnerable",
                  "Incorrect entry for In_care",
                  "Incorrect entry for Severity")
  #Empty vector to fill with if the named problem in sol vector is a problem
  diagnosis_prob=NULL
  
  
  #Check that timeline has the correct number of columns
  diagnosis_prob=c(diagnosis_prob,(dim(TF_model)[2]!=9))
  
  
  #Check that all the column names are correct
  diagnosis_prob=c(diagnosis_prob,!all( names(TF_model)==c("ID",
                                                           "Scenario_name",
                                                           "Age_group",
                                                           "SIMD",
                                                           "Vulnerable",
                                                           "In_care",
                                                           "severity",
                                                           "infection_rate",
                                                          "mortality_rate"
  )))
  
  
  #Check that the entries are the right classes
  diagnosis_prob=c(diagnosis_prob,(!all(lapply(TF_model,class)==c("numeric", "character","numeric","character",
                                     "character", "character", "character","numeric","numeric"))))
  
  #Check that each of the numeric have suitable values
  diagnosis_prob=c(diagnosis_prob,(!all(TF_model$ID>=0 & TF_model$ID<=4) ||
      !all(TF_model$ID==round(TF_model$ID))),
      (!all(TF_model$Age_group>=0 & TF_model$Age_group<=90) ||
      !all(TF_model$Age_group/10==round(TF_model$Age_group/10))),
      !all(TF_model$infection_rate>=0 & TF_model$infection_rate<=1),
      !all(TF_model$mortality_rate>=0 & TF_model$mortality_rate<=1))
  
  #Check that the character entries are of from the correct options
  diagnosis_prob=c(diagnosis_prob,!all(TF_model$Scenario_name %in% c("No intervention",
                                           "All interventions base",
                                           "All interventions (40% compliance)",
                                           "All interventions (60% compliance)",
                                           "All interventions (75% compliance)")),
      !all(TF_model$'SIMD' %in% c("L", "U")),
      !all(TF_model$'Vulnerable' %in% c("No",  "Yes")),
      !all(TF_model$'In_care' %in% c("No",  "Yes")),
      !all(TF_model$'severity' %in% c( "Mild", "Moderate", "Serious", "Severe", "Critical")))
  
  
  #Print message of if function is working correctly
  if (!all(diagnosis_prob==FALSE)){
    print("Function run_model not working correctly because of the following issues:")
    index=which(diagnosis_prob==TRUE)
    print(diagnosis_sol[index])
  }
  else {
    print("Function run_model working correctly")
  }
}


#------------------------------------------------------------------
#Function name: get_proportions()
#Description: 


TF_get_proportions=function(timeline, model) {
  TF_proportions=timeline %>% get_proportions(model)
  
  #Vector containing the issue with the function
  diagnosis_sol=c("Incorrect number of columns", "Incorrect column names",
                  "Incorrect class", "Incorrect values for Age_group",
                  "Incorrect values for Day","Incorrect values for ID", 
                  "Incorrect values for Proportions_starting",
                   "Incorrect values for Proportions_at",
                  "Incorrect entry for Scenario_name",
                  "Incorrect entry for SIMD",
                  "Incorrect entry for Vulnerable",
                  "Incorrect entry for In_care",
                  "Incorrect entry for Severity",
                  "Mild or Moderate severities are being hospitalised")
  #Empty vector to fill with if the named problem in sol vector is a problem
  diagnosis_prob=NULL
  
  
  #Check that timeline has the correct number of columns
  diagnosis_prob=c(diagnosis_prob,(dim(TF_proportions)[2]!=11))
  
  
  #Check that all the column names are correct
  diagnosis_prob=c(diagnosis_prob,!all( names(TF_proportions)==c("ID",
                                                           "Day",
                                                           "Age_group",
                                                           "Proportion_starting",
                                                           "severity",
                                                           "Status",
                                                           "Proportion_at",
                                                           "Scenario_name",
                                                           "SIMD",
                                                           "Vulnerable",
                                                           "In_care"  
  )))
  
  
  #Check that the entries are the right classes
  diagnosis_prob=c(diagnosis_prob,(!all(lapply(TF_proportions,class)==c("numeric","numeric", 
                                      "numeric","numeric", "character","character","numeric",
                                     "character", "character", "character","character"))))
 
  #Check the numeric values
  diagnosis_prob=c(diagnosis_prob,(!all(TF_proportions$Age_group>=0 & TF_proportions$Age_group<=90) ||
      !all(TF_proportions$Age_group/10==round(TF_proportions$Age_group/10))),
      (!all(TF_proportions$Day>=0 & TF_proportions$Day<=1000) ||
      !all(TF_proportions$Day==round(TF_proportions$Day))),
      (!all(TF_proportions$ID>=0 & TF_proportions$ID<=4) ||
      !all(TF_proportions$ID==round(TF_proportions$ID))),
  !all(TF_proportions$Proportion_starting>=0 & TF_proportions$Proportion_starting<=1),
      !all(TF_proportions$Proportion_at>=0 & TF_proportions$Proportion_at<=1)
)

  
  
  #Check that the character entries are of from the correct options
  diagnosis_prob=c(diagnosis_prob,(!all(TF_proportions$Scenario_name %in% c("No intervention",
                                           "All interventions base",
                                           "All interventions (40% compliance)",
                                           "All interventions (60% compliance)",
                                           "All interventions (75% compliance)"))),
      !all(TF_proportions$'SIMD' %in% c("L", "U")),
      !all(TF_proportions$'Vulnerable' %in% c("No",  "Yes")),
      !all(TF_proportions$'In_care' %in% c("No",  "Yes")),
      !all(TF_proportions$'severity' %in% c( "Mild", "Moderate", "Serious", "Severe", "Critical")))
  
  #Check that mild and moderate serverity does not lead to hospitalisation
  t2<-TF_proportions %>% filter(severity %in% c( "Mild", "Moderate"), Proportion_at>0)
  diagnosis_prob=c(diagnosis_prob,( !all(t2$Status %in% c( "Infected",
                              "Recovering",
                              "Recovered"))))

  
  #Print message of if function is working correctly
  if (!all(diagnosis_prob==FALSE)) {
    print("Function get_proportions not working correctly because of the following issues:")
    index=which(diagnosis_prob==TRUE)
    print(diagnosis_sol[index])
  }
  if (all(diagnosis_prob==FALSE))  {
    print("Function get_proportions working correctly")
  }
  
}
  
  
  

#------------------------------------------------------------------

#Function name: get_totals()
#Description: 


TF_get_totals=function(proportions, population) {
  TF_functionnotworking=0
  TF_scotland_estimates = proportions %>% 
    get_totals(population)
  
  
  #Vector containing the issue with the function
  diagnosis_sol=c("Incorrect number of columns", "Incorrect column names",
                  "Incorrect class", "Incorrect values for Age_group",
                  "Incorrect values for Day","Incorrect values for ID", 
                  "Incorrect values for Population",
                  "Incorrect values for Number_starting",
                  "Incorrect values for Number_at",
                  "Incorrect entry for Scenario_name",
                  "Incorrect entry for SIMD",
                  "Incorrect entry for Vulnerable",
                  "Incorrect entry for In_care",
                  "Incorrect entry for Severity",
                  "Incorrect entry for Status",
                  "Mild or Moderate severities are being hospitalised")
  #Empty vector to fill with if the named problem in sol vector is a problem
  diagnosis_prob=NULL
  
  
  #Check that timeline has the correct number of columns
  diagnosis_prob=c(diagnosis_prob,(dim(TF_scotland_estimates)[2]!=12))
  
  #Check that all the column names are correct
  diagnosis_prob=c(diagnosis_prob,!all( names(TF_scotland_estimates)==c("ID",
                                                                 "Day",
                                                                 "Age_group",
                                                                 "severity",
                                                                 "Status",
                                                                 "Scenario_name",
                                                                 "SIMD",
                                                                 "Vulnerable",     
                                                                 "In_care",
                                                                 "Population",
                                                                 "Number_starting",
                                                                 "Number_at"  
  )))
  
  #Check that the entries are the right classes
  diagnosis_prob=c(diagnosis_prob,(!all(lapply(TF_scotland_estimates,class)==c("numeric","numeric", "numeric","character", 
                                                  "character","character","character", "character",
                                                  "character","numeric", "numeric", "numeric"))))
  
  #Check the numeric values
  diagnosis_prob=c(diagnosis_prob,(!all(TF_scotland_estimates$Age_group>=0 & TF_scotland_estimates$Age_group<=90) ||
      !all(TF_scotland_estimates$Age_group/10==round(TF_scotland_estimates$Age_group/10))),
      (!all(TF_scotland_estimates$Day>=0 & TF_scotland_estimates$Day<=1000) ||
      !all(TF_scotland_estimates$Day==round(TF_scotland_estimates$Day))),
      (!all(TF_scotland_estimates$ID>=0 & TF_scotland_estimates$ID<=4) ||
      !all(TF_scotland_estimates$ID==round(TF_scotland_estimates$ID))),
      !all(TF_scotland_estimates$Population>=0 & TF_scotland_estimates$Population<=1000000),
      !all(TF_scotland_estimates$Number_starting>=0 & TF_scotland_estimates$Number_starting<=1000000),
      !all(TF_scotland_estimates$Number_at>=0 & TF_scotland_estimates$Number_at<=1000000)
  )

  
  #Check that the character entries are of from the correct options
  diagnosis_prob=c(diagnosis_prob,(!all(TF_scotland_estimates$Scenario_name %in% c("No intervention",
                                                 "All interventions base",
                                                 "All interventions (40% compliance)",
                                                 "All interventions (60% compliance)",
                                                 "All interventions (75% compliance)"))),
      !all(TF_scotland_estimates$'SIMD' %in% c("L", "U")),
      !all(TF_scotland_estimates$'Vulnerable' %in% c("No",  "Yes")),
      !all(TF_scotland_estimates$'In_care' %in% c("No",  "Yes")),
      !all(TF_scotland_estimates$'severity' %in% c( "Mild", "Moderate", "Serious", "Severe", "Critical")),
      !all(TF_scotland_estimates$'Status' %in% c("Infected", "Oxygen", "Non-invasive ventilation", "Invasive ventilation",
                                                 "Recovering", "Recovered", "Dead" )))


  #Check that mild and moderate serverity does not lead to hospitalisation
  t2<-TF_scotland_estimates %>% filter(severity %in% c( "Mild", "Moderate"), Number_at>0)
  diagnosis_prob=c(diagnosis_prob,( !all(t2$Status %in% c( "Infected",
                              "Recovering",
                              "Recovered"))))
  

  
  #Print message of if function is working correctly
  if (!all(diagnosis_prob==FALSE)) {
    print("Function get_proportions not working correctly because of the following issues:")
    index=which(diagnosis_prob==TRUE)
    print(diagnosis_sol[index])
  }
  if (all(diagnosis_prob==FALSE))  {
    print("Function get_proportions working correctly")
  }
  
  
  
  
}


#------------------------------------------------------------------

#Run all test functions

TF_get_pop_data()
TF_run_timeline(length_of_infection, date_of_admission_from_infection_start,
                hospital_recovery_factor, cutoff)
TF_run_model(Age_group, SIMD, Vulnerable, In_care)
TF_get_proportions(timeline, model)
TF_get_totals(proportions, population)




