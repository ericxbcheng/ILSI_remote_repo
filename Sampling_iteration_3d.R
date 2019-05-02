# Organize the results
clean_new = function(spread, data, diag){
  
  stopifnot(spread %in% c("continuous", "discrete"))
  
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
  
  return(c(sens = sens, spec = spec))
}

# A function factory
gen_sim_outcome_new = function(n_contam, lims, spread, spread_radius, method_sp, n_sp, n_strata, 
                               by, LOC, fun, case, m, M, m_sp, method_det, covar_mat, n_affected, dis_level, cont_level, 
                               sp_radius, container, L, rho, m_kbar, conc_neg, tox, Mc, diag, bg_level, geom){
  function(...){
    sim_outcome_new(n_contam = n_contam, lims = lims, spread = spread, covar_mat = covar_mat,
                    n_affected = n_affected, dis_level = dis_level, method_sp = method_sp, sp_radius = sp_radius,
                    container = container, L = L, rho = rho, m_kbar = m_kbar, conc_neg = conc_neg, 
                    tox = tox, Mc = Mc, m_sp, method_det = method_det, diag = diag, spread_radius = spread_radius, 
                    n_sp = n_sp, n_strata = n_strata, by = by, LOC = LOC, fun = fun, case = case, m = m, 
                    M = M, cont_level = cont_level, bg_level = bg_level, geom = geom)
  }
}
