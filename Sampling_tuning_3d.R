# This function makes a list of arguments from the default arguments
make_tune_args = function(Args, param, val){
  
  # Checkpoint
  stopifnot(param %in% names(Args))
  
  Args[[param]] = val
  return(Args)
}

# Create a function that passes tuning parameters to sim_iterate2()
tune_param = function(Args, n_seed, n_iter, param, val, ...){
  
  # Get tuning parameters
  a = make_tune_args(Args = Args, param = param, val = val)
  
  # 2 layers of iteration
  b = sim_iterate2(n_seed = n_seed, n_iter = n_iter, Args = a)

  # Add a vector of tuning parameter value
  b$param = rep.int(x = val, times = n_seed * n_iter)
  
  return(b)
}

# Create a function that summarize the sens, spec and mean c_true
summary_calc_metrics = function(df, Mc){
  a = mean(df$c_true)
  b = calc_metrics(c_true = df$c_true, decision = df$decision, Mc = Mc)
  return(c(mean_c_true = a,b))
}