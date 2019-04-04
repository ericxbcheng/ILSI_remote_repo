# Organize the results
clean_new = function(spread, data, diag){
  
  if(spread == "discrete"){
    clean_dis(data = data, diag = diag)
    
  } else {
    clean_cont(data = data)
    
  }
}

# Organize the results for discrete case
clean_dis = function(data, diag){
  
  # Convert the list into a vector
  a = unlist(data)
  
  if(diag == FALSE){
    
    # Produce a sequence of c(1,2) for element extraction
    ind = rep(x = c(1,2), times = length(data))
    
    c_true = a[ind == 1]
    decision = a[ind == 2]
    
    # Output
    return(list("c_true" = c_true, "decision" = decision))
    
  } else {
    
    # Produce a sequence of c(1,2,3,4,5) for element extraction
    ind = rep(x = c(1,2,3,4,5), times = length(data))
    
    c_true = a[ind == 1]
    decision = a[ind == 2]
    mean_raw = a[ind == 3]
    mean_work = a[ind == 4]
    mean_test = a[ind == 5]
    
    # Output
    return(list("c_true" = c_true, "decision" = decision, 
                "mean_raw" = mean_raw, "mean_work" = mean_work, "mean_test" = mean_test))
  }
}

# Organize the results for continuous case
clean_cont = function(data){
  # Convert the data into a vector
  a = unlist(data)
  
  #Produce a sequence of indices for element extraction
  ind = rep(x = c(1,2), times = length(data))
  
  # Extract elements respectively
  I_det = a[ind == 1]
  decision = a[ind == 2]
  
  # Output
  b = list("I_det" = I_det, "decision" = decision)
  
  return(b)
}

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
  
  return(list(sens = sens, spec = spec))
}