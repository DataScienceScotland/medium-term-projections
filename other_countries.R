get_pop_data_england=function(area_type="Local_authority") { #in other_countries.R
  
  selected_rows = switch(area_type,
                         Local_authority = 3:34,
                         Health_board = 37:50)
  "x.csv" %>% 
     read_csv(skip = 5) %>%
     select(X1,X2,X3,X8) %>% 
    rename("Area code"=X1, "Area1"=X2, "Age"=X3,"Persons"=X8) %>%
  filter(Area1!="x", Area1!="AREA",Age!="All ages") %>% 
    mutate(Persons=Persons*1000) %>%
    mutate(Age=Age %>% str_extract("\\d*") %>% as.integer)
}

