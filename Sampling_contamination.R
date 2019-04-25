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


## (OLD) The simulation function for contamination spot and its spread
sim_contam = function(n_contam, xlim, ylim, covariance, n_affected, radius, cont_level){
  
  if(n_contam == 0){
    stop("n_contam = 0. Please select another n_contam.")
  }
  
  ## Generate a matrix that contains contamination coordinates
  x = runif(n = n_contam, min = xlim[1], max = xlim[2])
  y = runif(n = n_contam, min = ylim[1], max = ylim[2])
  
  spot_coord = matrix(data = c(x, y), ncol = 2) %>% 
    as.data.frame()
  
  # Create a factor column.
  label = c(rep("spot", times = n_contam), rep("spread", times = n_contam * n_affected))
  
  # Create a spread radius column
  r = radius
  
  if(n_affected != 0){
    # Each column of spread_coord contains 2*n_affected elements, the first n_affected elements are random numbers generated from mat_1[1,1] and the second n_affected elements are random numbers generated from mat_1[1,2]
    spread_coord = apply(X = spot_coord, MARGIN = 1, FUN = mvrnorm, n = n_affected, Sigma = covar_mat)
    
    # Split the spread_coord by columns, rearrange the vector of each list into an nx2 matrix, then combine these matrices by row.
    spread_coord_2 = spread_coord %>%
      split(x = ., f = col(.)) %>%
      map(.x = ., .f = function(x) {matrix(x, ncol = 2)}) %>%
      do.call(what = rbind, args = .) %>%
      as.data.frame()
    
    # Give a unique identifier to each observation.
    spread_coord_3 = naming_spread(spread_coord_2, n_contam, n_affected)
    spot_coord_2 = naming_spot(spot_coord, n_contam)
    
    # Combine the contamination spot coordinates and the spread coordinates, and then combine the factor column and the spread radius column.
    df_1 = rbind(spot_coord_2, spread_coord_3)
    colnames(df_1) = c("X", "Y", "ID")
    df_2 = cbind(df_1, label, r)
    df_2[["cont_level"]] = f_cont_level(n = nrow(df_2), param = cont_level)
    
  } else {
    
    # Give a unique identifier to each observation.  
    spot_coord_2 = naming_spot(spot_coord, n_contam)
    
    # Give the spot_coord_2 labels and radius column
    df_1 = spot_coord_2
    colnames(df_1) = c("X", "Y", "ID")
    df_2 = cbind(df_1, label, r)
    df_2[["cont_level"]] = f_cont_level(n = nrow(df_2), param = cont_level)
    
  }
  
  # Remove points that are outside the perimeter.
  out = df_2$X < xlim[1] |
    df_2$X > xlim[2] |
    df_2$Y < ylim[1] |
    df_2$Y > ylim[2]
  
  df_3 = df_2[!out, ]
  
  # Reset the row names
  rownames(df_3) = NULL
  
  # Add a column for contamination level in discrete spread mode. For spots and spreads, the two types of level are identical.
  df_3[["dis_level"]] = df_3[["cont_level"]]
  
  df_3$ID = as.character(df_3$ID)
  
  return(df_3)
}

