# Create a function to idenfity points that falls within a certain distance from another point
## Determine which sample points fall within the spread_radius in the continuous spread case.
calc_cover_cont = function(df_dist, r){
  a = subset(x = df_dist, subset = label == "spot", drop = FALSE)
  b = subset(x = a, subset = Distance <= r)
  return(b)
}

## Determine which spot and spread points fall within the sp_radius in the discrete spread case.
calc_cover_dis = function(df_dist, r){
  a = subset(x = df_dist, subset = Distance <= r)
  return(a)
}

## Wrap up function
calc_cover = function(df_dist, spread_radius, sp_radius, spread, geom, df_contam_sp){
  
  if(spread == "discrete"){
    calc_cover_dis(df_dist = df_dist, r = sp_radius)
  } else if (spread == "continuous"){
    
    # Area-based: spread_radius = diagonal length of the field
    if(geom == "area"){
      spread_radius = subset(x = df_contam_sp, subset = label == "spot", select = r, drop = TRUE)
    }
    
    calc_cover_cont(df_dist = df_dist, r = spread_radius)
    
  } else {
    stop("Unknown spread type.")
  }
}