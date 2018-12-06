# Iterate the simulation on different values of a single parameter
tune_param = function(val, n_iter, ...){
  
  # For each value of the parameter we want to tune, generate an f_outcome() loaded with arguments
  a = map(.x = val, .f = gen_sim_outcome, ...)
  
  # Iterate each f_outcome() n_iter times and produce a series of lists containing I_det, ROD, decision
  b = map(.x = a, .f = sim_iterate, n_iter = n_iter)
  
  # Organize the outcomes into one single list with "I_det", "ROD", "Decision"
  c = clean(b)
  
  # Add a vector of parameter values to the list
  c$param = rep(x = val, each = n_iter) %>% as.factor()
  
  return(c)
}
