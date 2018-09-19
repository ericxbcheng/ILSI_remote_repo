
## Some helper functions for creating unique identifiers for each observation.
naming_spread = function(df, spot, spread){
  a = rep(x = 1:spot, each = spread) %>% as.character()
  b = rep(x = 1:spread, times = spot) %>% as.character()
  c = paste(a, "-", b, sep = "")
  cbind(df, c)
}

naming_spot = function(df, spot){
  a = 1:spot %>% as.character()
  b = rep(0, times = spot) %>% as.character()
  c = paste(a, "-", b, sep = "")
  cbind(df, c)
}

## The simulation function for contamination spot and its spread
sim_contam = function(n_contam, xlim, ylim, covariance, n_affected, radius){
  
  ## Generate a matrix that contains contamination coordinates
  x = runif(n = n_contam, min = xlim[1], max = xlim[2])
  y = runif(n = n_contam, min = ylim[1], max = ylim[2])
  
  spot_coord = matrix(data = c(x, y), ncol = 2) %>% as.data.frame()
  
  # Each column of spread_coord contains 2*n_affected elements, the first n_affected elements are random numbers generated from mat_1[1,1] and the second n_affected elements are random numbers generated from mat_1[1,2]
  spread_coord = apply(X = spot_coord, MARGIN = 1, FUN = mvrnorm, n = n_affected, Sigma = covar_mat)
  
  # Split the spread_coord by columns, rearrange the vector of each list into an nx2 matrix, then combine these matrices by row.
  spread_coord_2 = split(x = spread_coord, f = col(spread_coord))
  spread_coord_3 = lapply(X = spread_coord_2, FUN = function(x){matrix(x, ncol = 2)})
  spread_coord_4 = do.call(what = rbind, args = spread_coord_3) %>% as.data.frame()
  
  # Give a unique identifier to each observation.
  spread_coord_5 = naming_spread(spread_coord_4, n_contam, n_affected)
  spot_coord_2 = naming_spot(spot_coord, n_contam)
  
  # Create a factor column.
  label = c(rep("spot", times = n_contam), rep("spread", times = n_contam * n_affected))
  
  # Create a spread radius column
  r = spread_radius
  
  # Combine the contamination spot coordinates and the spread coordinates, and then combine the factor column and the spread radius column.
  df_1 = rbind(spot_coord_2, spread_coord_5)
  colnames(df_1) = c("X", "Y", "ID")
  df_2 = cbind(df_1, label, r)
  
  # Remove points that are outside the perimeter.
  out = which(df_2[ ,1] < xlim[1] | df_2[ , 1] > xlim[2] | df_2[,2] < ylim[1] | df_2[,2] > ylim[2], arr.ind = TRUE)[1]
  if(is.na(out) == FALSE){
    df_3 = df_2[-out, ]
  } else {
    df_3 = df_2
  }
  
  # Reset the row names
  rownames(df_3) = NULL
  
  return(df_3)
}



