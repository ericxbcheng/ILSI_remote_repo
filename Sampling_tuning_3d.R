# Iterate the simulation on different values of a single parameter
tune_param_new = function(val, n_iter, ...){
  
  # Capture the arguments as expressions
  Args_exprs = enexprs(...)
  
  # For each value of the parameter we want to tune, generate an f_outcome() loaded with arguments
  a = map(.x = val, .f = gen_sim_outcome_new, ...)
  
  # Iterate each f_outcome() n_iter times and produce a series of lists containing either I_det, decision or c_true, decision, mean_raw, mean_work, mean_test
  b = map(.x = a, .f = sim_iterate, n_iter = n_iter)
  
  # Organize the outcomes into one single list
  c = clean_new(data = b, spread = eval_tidy(Args_exprs[["spread"]]), diag = eval_tidy(Args_exprs[["diag"]]))
  
  # Add a vector of parameter values to the list
  c$param = rep(x = val, each = n_iter) 
  
  return(c)
}

## Create a function that removes one of the arguments. 
one_arg_less_new = function(name, Args_default){
  
  # Check point: Is the parame_name included in the list below?
  stopifnot(any(name %in% c("n_contam", "lims", "spread", "spread_radius", 
                            "method_sp", "n_sp", "n_strata","by", "LOC", "fun", 
                            "case", "m", "M", "method_det", "covar_mat", 
                            "n_affected", "dis_level","cont_level","sp_radius", 
                            "container", "L", "rho", "m_kbar", "conc_neg", 
                            "tox", "Mc", "diag", "bg_level")))
  
  a = Args_default
  
  # Set the chosen parameter to NULL
  a[[name]] = NULL
  return(a)
}

## Create a function that can tune a single parameter given the parameter name and values
tune_param2_new = function(val, name, Args_default, n_iter, ...){
  
  # Produce a list of arguments with the tuning parameter removed
  a = one_arg_less_new(name = name, Args_default = Args_default)
  
  # Tune the chosen parameter for n_iter times. !!!a means splicing the list "a" and releasing all the remaining arguments.
  b = exec(f = tune_param_new, val = val, n_iter = n_iter, !!!a)
  
  return(b)
}

# Create a function that summarize the sens, spec and mean c_true
summary_calc_metrics = function(df, Mc){
  a = mean(df$c_true)
  b = calc_metrics(c_true = df$c_true, decision = df$decision, Mc = Mc)
  return(c(mean_c_true = a,b))
}