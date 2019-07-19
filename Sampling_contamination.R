## Some helper functions for creating unique identifiers for each observation.
naming_spread = function(df, spot, spread){
  a = rep(x = 1:spot, each = spread) %>% as.character()
  b = rep(x = 1:spread, times = spot) %>% as.character()
  c = paste(a, "-", b, sep = "")
  cbind(df, c, stringsAsFactors = FALSE)
}

naming_spot = function(df, spot){
  a = 1:spot %>% as.character()
  b = rep(0, times = spot) %>% as.character()
  c = paste(a, "-", b, sep = "")
  cbind(df, c, stringsAsFactors = FALSE)
}

## Create a function to generate contamination levels. Remember that param contains log10(mean) and log10(sd). And log() in R means log(..., base = exp(1)).
f_cont_level = function(n, param){
  rlnorm(n = n, meanlog = log(10^param[1]), sdlog = log(10^param[2]))
}

## Define a function that calculates contamination contribution when a point is within spread_radius and reduces to almost 0 when beyond spread_radius
f_decay = function(x, fun, spread_radius, LOC){
  if(x <= spread_radius){
    fun(x = x, spread_radius = spread_radius, LOC = LOC)
  } else {
    0
  }
}

## Calculate the contamination contribution using an exponential function.
f_exp = function(x, spread_radius, LOC){
  theta = - spread_radius/log(LOC)
  exp(-x/theta)
}

## Calculate the contamination contribution using a normal distribution-like function
f_norm = function(x, spread_radius, LOC){
  sigma = sqrt(-spread_radius^2/log(LOC))
  exp(-x^2/sigma^2)
} 

## Calculate the contamination contribution using a constant 
f_unif = function(x, spread_radius, LOC){
  1
}

## Calculate the contamination contribution on a 2D plane
f_density = function(method, x, y, x0, y0, spread_radius, LOC, cont_level){
  
  stopifnot(method %in% c("exp", "norm", "unif"))
  
  d = sqrt((x-x0)^2 + (y-y0)^2)
  
  f_chosen = switch(EXPR = method,
                    "exp" = f_exp,
                    "norm" = f_norm,
                    "unif" = f_unif)
  
  map_dbl(.x = d, .f = f_decay, fun = f_chosen, spread_radius = spread_radius, LOC = LOC)
  
}