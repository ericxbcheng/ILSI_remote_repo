# Determine if we accept or reject a lot
do_rej = function(decision){
  # Check point
  stopifnot(decision %in% 1:7)
  
  # Based on the lookup table, numbers 2,3,6 mean rejection while the others mean acceptance. 
  # Convert the decision vector into a logical vector. 1 = rejection, 0 = acceptance.
  a = {decision %in% c(2, 3, 6)}
  
  return(a)
}

# Calculate the probability of rejection
calc_Prej = function(decision){
  
  # Determine whether to reject or accept a lot
  a = do_rej(decision = decision)
  
  # Calculate the probability of rejection
  b = mean(a)
  
  return(b)
}

# Calculate the probability of detection (whether any of the contamination is detected)
calc_Pdet = function(I_det){
  
  # Check point
  stopifnot(I_det %in% c(0,1))
  
  # Calculate the probability of detection
  mean(I_det)
  
}

# Calculate detection probability for simulations for each seed
calc_Pdet_one = function(data){
  split(x = data$I_det, f = data$seed) %>%
    map(.x = ., .f = calc_Pdet) %>%
    unlist()
}

# Calculate probability of rejection for simulations for each seed
calc_Prej_one = function(data){
  split(x = data$decision, f = data$seed) %>%
    map(.x = ., .f = calc_Prej) %>%
    unlist()
}

# Clean up the probability of detection. "data" takes a list of n lists.
calc_Pdet_n = function(data){
  
  # Checkpoint
  if(!is.null(names(data))){
    stop("Data should be a list of lists, not a single list.")
  }
  
  map(.x = data, .f = calc_Pdet_one) %>%
    unlist() %>%
    tibble(seed = as.integer(names(.)), P_det = .)
}

# Clean up the probability of acceptance. "data" takes a list of n lists.
calc_Paccept_n = function(data){
  
  # Checkpoint
  if(!is.null(names(data))){
    stop("Data should be a list of lists, not a single list.")
  }
  
  map(.x = data, .f = calc_Prej_one) %>%
    unlist() %>%
    tibble(seed = as.integer(names(.)), P_rej = .) %>%
    mutate(Paccept = 1 - P_rej)
}

# Calculate P(detection) and P(acceptance) in the continuous case for a list of n lists
metrics_cont_n = function(data){

  # Checkpoint
  if(!is.null(names(data))){
    stop("Data should be a list of lists, not a single list.")
  }
  
  # Get Pdet
  a = calc_Pdet_n(data)
  
  # Get Paccept
  b = calc_Paccept_n(data)
  
  # Get parameter values
  c = map_dbl(.x = data, .f = function(x) x$param[[1]])
  
  # Form output
  d = cbind(a, Paccept = b$Paccept, param = rep(x = c, each = nrow(a) / length(data)))
  
  return(d)
}

# Calculate sensitivity and specificity
calc_metrics = function(c_true, decision, Mc){
  
  # Determine whether a lot is truly contaminated
  true = {c_true >= Mc} %>% 
    as.integer() %>%
    factor(x = ., levels = c(1, 0))
  
  # Determine whether a lot is rejected
  pred = do_rej(decision = decision) %>%
    as.integer() %>%
    factor(x = ., levels = c(1, 0))
  
  # calculate sensitivity and specificity
  conf_mat = table(true, pred)
  sens = conf_mat[1,1] / sum(conf_mat[1, ])
  spec = conf_mat[2,2] / sum(conf_mat[2, ])
  
  return(c(sens, spec))
}

# Calculate metrics for iterations under each seed
calc_metrics_one = function(data, Mc){
  a = split(x = data$c_true, f = data$seed)
  b = split(x = data$decision, f = data$seed)
  
  map2(.x = a, .y = b, .f = calc_metrics, Mc = Mc) %>%
    unlist()
}

# Calculate metrics for multiple lists
calc_metrics_n = function(data, Mc){
  
  # Checkpoint
  if(!is.null(names(data))){
    stop("Data should be a list of lists, not a single list.")
  }
  
  a = map(.x = data, .f = calc_metrics_one, Mc = Mc) %>%
        unlist()
  
  # Extract sensitivity and specificity and seed
  ind = rep(x = c(1,2), length.out = length(a))
  sens = a[ind == 1]
  spec = a[ind == 2]
  
  b = tibble(sens = sens, 
             spec = spec)
  
  return(b)
}

metrics_dis_n = function(data, Mc){
  
  # Checkpoint
  if(!is.null(names(data))){
    stop("Data should be a list of lists, not a single list.")
  }
  
  # Get Paccept
  a = calc_Paccept_n(data = data)
  
  # Get metrics
  b = calc_metrics_n(data = data, Mc = Mc)
  
  # Get parameter values
  c = map_dbl(.x = data, .f = function(x) x$param[[1]])
  
  # Form output
  d = cbind(a, b, param = rep(x = c, each = nrow(a) / length(data)))
  
  return(d)
}
