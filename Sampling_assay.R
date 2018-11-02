# Create a function that returns the proper attribute plan based on the case
get_attr_plan = function(case, m, M){
  stopifnot(is.double(case), is.double(m), is.double(M))
  stopifnot(m >=0, M >= 0)
  
  ## Select the case
  a = switch(EXPR = case, 
             `1` = list(n = 5, c = 3),
             `2` = list(n = 5, c = 2),
             `3` = list(n = 5, c = 1),
             `4` = list(n = 5, c = 3),
             `5` = list(n = 5, c = 2),
             `6` = list(n = 5, c = 1),
             `7` = list(n = 5, c = 2),
             `8` = list(n = 5, c = 1),
             `9` = list(n = 10, c = 1),
             `10` = list(n = 5, c = 0),
             `11` = list(n = 10, c = 0),
             `12` = list(n = 20, c = 0),
             `13` = list(n = 15, c = 0),
             `14` = list(n = 30, c = 0),
             `15` = list(n = 60, c = 0),
             stop("Unknown case", call. = FALSE)
  )
  
  ## Make sure 2-class cases have m = M = 0
  if(case %in% 10:15){
    if(M != 0 | m != 0){
      warning(paste0("Case ", case, ": 2-class plan. m and M are coerced to 0." ))
      m = M = 0
    }
  }
  
  ## Append m and M to the list of n_asp and c
  b = append(x = a, values = list(m = m, M = M))
  
  return(b)
}

# Create a function that provides a LOD value corresponding to the detection method. 
get_LOD = function(method_det){
  if(method_det == "plating"){
    2500
  } else if (method_det == "enrichment"){
    1
  } else {
    stop("Unknown detection method")
  }
}

# Create a function that decides whether to accept or reject the lot
lot_decision = function(data, case, m, M, method_det){
  
  ## Get the appropriate attribute plan
  micro_ct = get_attr_plan(case = case, m = m, M = M)
  
  ## Get LOD for the chosen method of detection
  LOD = get_LOD(method_det = method_det)
  
  ## Subset out the contamination levels at the sample points
  a = subset(x = data, subset = label == "sample point", select = cont_level, drop = FALSE)
  
  ## Check if n_sp == n in the microbiological criteria
  if(nrow(a) != micro_ct[["n"]]){
    warning("n_sp does not equal to n.")
  }
  
  ## Find out the sample points with contamination level >= LOD
  b = a >= LOD
  
  ## Decision: reject lot if any sample has contamination level >= M or if the number of samples with contamination level >= m is above c
  if(any(a[b, ] >= micro_ct[["M"]])){
    cat("Reject lot.")
  } else {
    if(sum(a[b, ] >= micro_ct[["m"]]) > micro_ct[["c"]]){
      cat("Reject lot.")
    } else {
      cat("Accept lot.")
    }
  }
}