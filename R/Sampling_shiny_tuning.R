# Parse the string "a,b,c" from val_prim into a numeric vector c(a,b,c)
parse_num_vec = function(string){
  return(str_split(string = string, pattern = ",") %>%
           unlist() %>%
           as.numeric())
}

# Parse the string "a,b,c" from val_sec into a character vector c("a","b","c")
parse_char_vec = function(string){
  
  # Split by the comma
  a = str_split(string = string, pattern = ",") %>%
    unlist() 
  
  # Remove extra whitespace
  return(str_trim(a))
}

# Iterate the model without any tuning parameters
iterate_tune0 = function(input, Args){
  
  return(sim_iterate2(n_seed = input$n_seed, n_iter = input$n_iter, Args = Args))
  
}

# Iterate the model with one tuning parameter
iterate_tune1 = function(input, Args){
  
  vals = parse_num_vec(string = input$val_prim)
  
  return(map(.x = vals, .f = tune_param, Args = Args, n_seed = input$n_seed, n_iter = input$n_iter, param = input$var_prim))
  
}

# Secondary tuning parameter iterations
iterate_tune2 = function(input, Args){
  
  # Parse tuning values for primary and secondary parameters
  vals_prim = parse_num_vec(string = input$val_prim)
  vals_sec = parse_char_vec(string = input$val_sec)
  
  # Create a list of argument lists, each argument list corresponding to one secondary tuning value
  Args_sec = map(.x = vals_sec, .f = update_arg, arg_list = Args, name = input$var_sec)
  
  # For each argument list in Args_sec, do iterate_tune1()
  sim_data = list()
  for(i in 1:length(vals_sec)){
    
    sim_data[[i]] = map(.x = vals_prim, .f = tune_param, 
                        Args = Args_sec[[i]], n_seed = input$n_seed, n_iter = input$n_iter,param = input$var_prim)
    
  }
  return(list(sim_data = sim_data, vals_prim = vals_prim, vals_sec = vals_sec))
}

# Function to update arguments
update_arg = function(arg_list, name, value){
  
  a = arg_list
  a[[name]] = value
  return(a)
}

# A summary function for 0 tuning parameter
metrics_cont_0 = function(data){
  
  # Calculate the acceptance prob
  a = calc_Prej_one(data) %>%
    tibble(P_rej = ., seed = as.numeric(names(.)), Paccept = 1 - .)
  return(a)
}

# Data cleaning function for secondary tuning scenarios
metrics_cont_sec = function(data, input, vals_prim, vals_sec){
  
  # Data should contain 2 layers. 
  # The outer layer contains data for different secondary tuning. 
  # The inner layer contains data for different primary tuning under each value of the secondary tuning.
  a = map(.x = data, .f = metrics_cont_n) %>%
    bind_rows()
  
  # Add a column to indicate secondary tuning values
  b = rep(x = vals_sec, each = input$n_seed * length(vals_prim))
  
  # Combine by columns
  c = cbind.data.frame(a, b, stringsAsFactors = FALSE)
  colnames(c)[colnames(c) == "b"] = "param2"
  
  return(c)
}