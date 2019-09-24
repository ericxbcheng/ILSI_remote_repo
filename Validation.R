
# A converter from pound to kg
lb2kg = function(x){
  return(0.453592 * x)
}

get_cube_side = function(pound, rho){
  
  # V = m_total / rho (rho = g/cm^3)
  # Assume V = side ^ 3 ==> side = V ^ 1/3 (cm)
  # Convert cm to m
  return((lb2kg(x = pound) * 1000 / rho)^(1/3) * 0.01)
}

gen_sim_data_val = function(df_contam, rho, m_kbar, lims, conc_neg){
  
  # Extract contaminated kernels
  c_pos = df_contam$dis_level
  
  # Calculate the number of healthy kernels and assign concentrations
  n_total = calc_k_num(rho = rho, m_kbar = m_kbar, sampler = FALSE, lims = lims)
  n_neg = n_total - length(c_pos)
  c_neg = gen_conc_neg(n = n_neg, conc_neg = conc_neg)
  
  raw = c(c_pos, c_neg)
  c_true = mean(raw)
  
  return(list(combined = df_contam, raw = raw, c_true = c_true))
}

split_grains = function(data, n){
  
  # Checkpoint
  stopifnot(length(data) >= n)
  
  # Randomize the grains
  a = sample(x = data, size = length(data), replace = FALSE)
    
  # Split the grains into n parts
  # cut() performs in a way that sort the x and assign intervals. It won't produce equal interval cuts when x is skewed
  b = split(x = a, f = cut(x = 1:length(a), breaks = n, labels = FALSE))
  
  return(b)
}

get_subsample = function(data, m_sp, m_kbar, n_sub){
  
  # data = a ground test sample
  # m_sp = mass of each subsample (50g)
  # n_sub = the number of subsamples from one test sample
  
  # Checkpoint 1
  stopifnot(n_sub >= 1)
  
  # Calculate the number of grains to form one subsample
  n_k = round(m_sp / m_kbar)
  
  # Checkpoint 2: there should be enough ground test sample for subsampling
  stopifnot(n_sub * n_k <= length(data))

  if(n_sub == 1){
    a = sample(x = data, size = n_k, replace = FALSE)
    return(mean(a))   

  } else {
    a = sample(x = data, size = n_sub * n_k, replace = FALSE)
    b = split(x = a, f = cut(x = 1:length(a), breaks = n_sub, labels = FALSE))
    c = map(.x = b, .f = mean)
    return(unlist(c))
  }
}

get_sample_dis_val = function(data, n, homogeneity, m_sp, m_kbar, unbalanced, n_sub){
 
  # Generate n test samples
  a = split_grains(data = data, n = n)
  
  # Grind each test sample
  b = map(.x = a, .f = grinder, homogeneity = homogeneity)
  
  if(unbalanced == TRUE){
    # For half of the test samples, get 2 subsamples
    # For the other half, get 1 subsample
    half = 1:round(n/2)
    
    c = map(.x = b[half], .f = get_subsample, m_sp = m_sp, m_kbar = m_kbar, n_sub = 2)
    d = map(.x = b[-half], .f = get_subsample, m_sp = m_sp, m_kbar = m_kbar, n_sub = 1)
    
    return(list(c, d))
  } else {
    # Get 2 subsamples for each test sample
    c = map(.x = b, .f = get_subsample, m_sp = m_sp, m_kbar = m_kbar, n_sub = n_sub)
    return(c)
  }
}

clean_val = function(data){
  
  a = unlist(data)
  
  # Split the names of a by the decimal point.
  # decimal point is an extended regular expression, so we need to do: "[.]"
  # when there is only 1 subsample, sub_sp is assigned 1.
  name_temp = names(a)
  b = str_split(string = name_temp, pattern = "[.]", simplify = TRUE) %>%
    data.frame(stringsAsFactors = TRUE) %>%
    rename(.data = ., test_sp = X1, sub_sp = X2) %>%
    mutate(value = a)
  
  bool = b$sub_sp == ""
  b$sub_sp[bool] = "1"
  
  return(b)
}

sim_intmed_dis_val = function(geom, n_contam, c_hat, rho, m_kbar, conc_neg, lims, spread, 
                              covar_mat, n_affected, spread_radius, cont_level, dis_level, seed,
                              n, n_sub, m_sp, homogeneity, unbalanced){
  
  contam_xy = sim_contam_new(geom = geom, n_contam = n_contam, c_hat = c_hat, 
                             rho = rho, m_kbar = m_kbar, conc_neg = conc_neg, 
                             lims = lims, spread = spread, covar = covar_mat, n_affected = n_affected,
                             spread_radius = spread_radius, cont_level = cont_level, 
                             dis_level = dis_level, seed = seed)
  
  contam_sp_xy = gen_sim_data_val(df_contam = contam_xy, rho = rho, m_kbar = m_kbar, lims = lims, conc_neg = conc_neg)
  
  sample_dis = get_sample_dis_val(data = contam_sp_xy$raw, n = n, homogeneity = homogeneity, m_sp = m_sp, m_kbar = m_kbar, unbalanced = unbalanced, n_sub = n_sub)
  
  return(list(c_true = contam_sp_xy$c_true, df = clean_val(sample_dis)))
}

calc_var_comp = function(df){
  
  # Run a random effect ANOVA with REML
  mod = lmer(formula = value ~ 1 + (1|test_sp), data = df, REML = TRUE)
  
  # Get the mean concentration from the test
  c_test = mod@beta
  
  # Calculate variance components
  var_comp = formatVC(varcor = VarCorr(x = mod), comp = "Var")[,3] %>%
    as.numeric()
  var_test = var_comp[1]
  var_sub = var_comp[2]
  var_total = var_test + var_sub
  
  return(list(c_test = c_test, var_test = var_test, var_sub = var_sub, var_total = var_total))
}

sim_outcome_val = function(geom, n_contam, c_hat, rho, m_kbar, conc_neg, lims, spread, 
                           covar_mat, n_affected, spread_radius, cont_level, dis_level, seed,
                           n, n_sub, m_sp, homogeneity, unbalanced){
  
  a = sim_intmed_dis_val(geom = geom, n_contam = n_contam, c_hat = c_hat, rho = rho, m_kbar = m_kbar, conc_neg = conc_neg, lims = lims, spread = spread, 
                         covar = covar, n_affected = n_affected, spread_radius = spread_radius, cont_level = cont_level, dis_level = dis_level, seed = seed,
                         n = n, n_sub = n_sub, m_sp = m_sp, homogeneity = homogeneity, unbalanced = unbalanced)
  
  b = calc_var_comp(df = a$df)
  
  return(c(c_true = a$c_true, b))
}
