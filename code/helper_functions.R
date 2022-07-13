#draws random numbers from a triangular distribution
rtri = function(n, min=0, mode=0.5, max=1) { #in helper_functions.R
  
  if(min==mode) min = mode-0.0000001
  if(max==mode) max = mode+0.0000001
  
  F = (mode-min) / (max-min)
  runif(n) %>%
    map_if(.<F, ~min - 1000 + sqrt(.*(max-min)*(mode-min))) %>%
    map_if(.>=F, ~max - 1000 - sqrt((1-.)*(max-min)*(max-mode))) %>%
    unlist %>%
    `+`(1000)
}

#adds fans to a ggplot
fans = function(obj, levels, index = 0, reversed = FALSE) { #in helper_functions.R
  
  index = index + 1
  y_quo = rlang::sym("x" %>% str_c(levels[index]))
  df = obj$data
  
  if(reversed) {
    if(levels[index] == 0.5) obj
    else {
      perc = levels[length(levels)+1-index] %>% as.character
      (obj +
          geom_ribbon(aes(ymin=x0.5,ymax=!!y_quo,fill=perc))) %>% 
        fans(levels, index, TRUE)
    }
  }
  else {
    if(levels[index] == 0.5) {
      obj %>% fans(rev(levels), 0, TRUE)
    }
    else {
      perc = levels[index] %>% as.character
      (obj +
          geom_ribbon(aes(ymin=!!y_quo,ymax=x0.5,fill=perc))) %>% 
        fans(levels, index)
    }
  }
}

#set import_priority = "local" to look in local drive first
get_filepath = function(filename, type="Data") { #in helper_functions.R
  if(! "import_priority" %>% exists) import_priority = "shared"
  
  if(import_priority == "shared") {
    if(file.exists("Z:" %>% file.path(filename))) "Z:" %>% file.path(filename)
    else if(file.exists(type %>% file.path(filename))) type %>% file.path(filename)
    else stop("File does not exist")
  }
  else if(import_priority == "local") {
    if(file.exists(type %>% file.path(filename))) type %>% file.path(filename)
    else if(file.exists("Z:" %>% file.path(filename))) "Z:" %>% file.path(filename)
    else stop("File does not exist")
  }
  else if(import_priority == "development") {
    if(file.exists("Y:" %>% file.path(filename))) "Y:" %>% file.path(filename)
    else stop("File does not exist")
  }
  else stop("Invalid import_priority")
}

as_excel_date = function(date) { #in helper_functions.R
  #(date -ymd("1899-12-30")) %>% as.integer
  date %>% format.Date("%d-%b") %>% as_factor
}

#converts an excel date to an R date
from_excel_date = function(number) { #in helper_functions.R
  ymd("1899-12-30") + as.integer(number)
}

to_excel_date = function(date) { #in helper_functions.R
  (date -ymd("1899-12-30")) %>% as.integer
}

#returns TRUE if the expression is a function and FALSE if it isn't
is_function = function (expr) { #in helper_functions.R
  if (! is_assign(expr))
    return(FALSE)
  value = expr[[3]]
  is.call(value) && as.character(value[[1]]) == 'function'
}

#returns the function name
function_name = function (function_expr) { #in helper_functions.R
  as.character(function_expr[[2]])
}

#returns TRUE if the expression is an assignation and FALSE if it isn't
is_assign = function (expr) { #in helper_functions.R
  is.call(expr) && as.character(expr[[1]]) %in% c('=', '<-', 'assign')
}

#returns all of the function names from the given script, either in production or development site
get_function_names = function(filename, site = "production") { #in helper_functions.R
  switch(site,
         production = "x",
         development = "x") %>% 
    str_c(filename) %>% 
    parse %>% 
    Filter(is_function, .) %>% 
    map_chr(function_name)
}

rollsum = function(x, k) { #in helper_functions.R
  y = x %>% cumsum
  
  c(y %>% head(k), y %>% diff(k))
}

#the higher phi is, the bigger the adjustment for values above min
#setting min<0 and phi=0 returns unchanged values for x>=0
adjust_lower_bound = function(x, min=5, phi=10) { #in helper_functions.R
  pmax(x - x*exp((min-x)/phi), 0)
}

#upper bound values will be at least min as long as phi > min
#the higher phi is, the bigger the adjustment
adjust_upper_bound = function(x, min=5, phi=10) { #in helper_functions.R
  x + min*exp(-5*x/(min*phi))
}

adjust_occupancy = function(data, adjust = FALSE) { #in helper_functions.R
  if(adjust) {
    percentile_cols = data %>% names %>% str_subset("percentile")
    
    data %>% 
      gather(Percentile, Value, percentile_cols) %>% 
      mutate(Value = Percentile %>% 
               recode(`5th percentile` = Value * 0.6,
                      `25th percentile` = Value * 0.8,
                      `75th percentile` = Value * 1.2,
                      `95th percentile` = Value * 1.4,
                      .default = Value)) %>% 
      spread(Percentile, Value)
  }
  else data
}

smooth_quantile = function(y, span = 14/(length(y)+1)) { #in helper_functions.R
  # applies loess smoothing to a vector, y
  x = 1:length(y)
  
  loess(y~x, span = span) %>% predict(x)
}

signif_floor = function(x, n){ #in helper_functions.R
  power = (x %>% abs %>% log(10) %>% floor) + 1 - n
  y = floor(x / 10 ^ power) * 10^power
  # handle the x = 0 case
  y[x==0] = 0
  y
}

signif_ceiling = function(x, n){ #in helper_functions.R
  power = (x %>% abs %>% log(10) %>% floor) + 1 - n
  y = ceiling(x / 10 ^ power) * 10^power
  # handle the x = 0 case
  y[x==0] = 0
  y
}

round_better=function(x, n=1, fun = "signif") { #in helper_functions.R
  if(length(x) > 1) x %>% map_dbl(round_better, n, fun)
  
  else {
    power = x %>% abs %>% log(10) %>% floor
    if(x > 3 * 10^power) do.call(fun, list(x, n))
    else do.call(fun, list(x, n+1))
  }
}
