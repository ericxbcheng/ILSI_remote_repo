sim_iterate = function(n_sim, n_contam, xlim, ylim, n_affected, covar_mat, spread_radius, method, n_sp, sp_radius, spread, n_strata, by, cont_level, LOC, fun){
  
  # Initialize a numeric vector to store RODs
  ROD = vector(mode = "numeric", length = n_sim)
  
  # Iterate the simulation without parallelization
  for(i in 1:n_sim){
    ROD[i] = sim_outcome(
      n_contam = n_contam,
      xlim = x_lim,
      ylim = y_lim,
      n_affected = n_affected,
      covar_mat = covar_mat,
      spread_radius = spread_radius,
      method = method,
      n_sp = n_sp,
      sp_radius = sp_radius,
      spread = spread,
      n_strata = n_strata,
      by = by,
      cont_level = cont_level,
      LOC = LOC,
      fun = fun
    )
  }
  
  return(ROD)
}
