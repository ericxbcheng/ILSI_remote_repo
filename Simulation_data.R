# A function that produces argument lists for combinations of n_sp and sampling strategies
Set_ArgList = function(n_sp_new, case_new, Args_default){
  
  # Change the n_sp and case accordingly
  Args_default$n_sp = n_sp_new
  Args_default$case = case_new
  
  ArgList_srs = ArgList_strs = ArgList_ss = Args_default
  
  # SRS
  ArgList_srs$method_sp = "srs"
  
  # STRS
  ArgList_strs$method_sp = "strs"
  
  # SS
  ArgList_ss$method_sp = "ss"
  
  return(list("srs" = ArgList_srs, "strs" = ArgList_strs, "ss" = ArgList_ss))
}

# A function that names RDS outputs: contam_sp_straty_case.rds
naming_rds = function(contam, sp, strategy, case){
  paste0(contam, "_", sp, "_", strategy, "_", case, ".rds")
}

### ATTENTION: this function only works when n_contam = 1:6
output_rds = function(seed, n_iter, val, name, contam = "1to6", Args_default, sp, case){
  # SRS
  set.seed(seed)
  result_srs = map(.x = 1:n_iter, .f = tune_param2, val = val, name = name, Args_default = Args_default$srs, n_iter = n_iter)
  saveRDS(object = result_srs, file = naming_rds(contam = contam, sp = sp, strategy = "srs", case = case))
  
  # STRS
  set.seed(seed)
  result_strs = map(.x = 1:n_iter, .f = tune_param2, val = val, name = name, Args_default = Args_default$strs, n_iter = n_iter)
  saveRDS(object = result_strs, file = naming_rds(contam = contam, sp = sp, strategy = "strs", case = case))
  
  # SS
  set.seed(seed)
  result_ss = map(.x = 1:n_iter, .f = tune_param2, val = val, name = name, Args_default = Args_default$ss, n_iter = n_iter)
  saveRDS(object = result_ss, file = naming_rds(contam = contam, sp = sp, strategy = "ss", case = case))
}

# Check to see if each list in the secondary branch has the correct length. If TRUE is returned, it means at least one list in the branch has different lengths.
f_sec = function(data, length){
  any(summary(data)[,1] != as.character(length))
}

# Check each list in the primary branch
f_pri = function(data, length){
  map_lgl(.x = data, .f = f_sec, length = length)
}

# Check each list in the root level
f_root = function(data, length){
  a = map(.x = data, .f = f_pri, length = length) 
  b = map_lgl(.x = a, .f = any)
  
  if(any(b) == FALSE){
    cat(paste0("All the leaf nodes contain ", length, " elements."))
  } else {
    warning("Inconsistent number of elements detected! \n")
    which(b)
  }
}

# Capture the rds name and extract n_sp, method_sp, case
arg_extract = function(name){
  
  # Split the name by "_"
  a = str_split(string = name, pattern = "_") %>%
    unlist()
  
  # Extract n_sp, method_sp, case
  n_sp = as.integer(a[2])
  method_sp = a[3]
  case = str_remove(string = a[4], pattern = ".rds") %>%
    as.integer()
  
  return(list(n_sp = n_sp, method_sp = method_sp, case = case))
}

# A function that cleans each rds file by putting P_det, Paccept, n_contam, n_sp, case, method_sp together in a data frame
clean_rds = function(data, name){
  
  temp = arg_extract(name = name)
  
  a = clean_Pdet(data = data, method_sp = temp$method_sp)
  b = clean_Paccept(data = data, method_sp = temp$method_sp)
  c = bind_cols(a, b["Paccept"]) %>%
    mutate(n_sp = temp$n_sp, case = temp$case)
  
  return(c)
}