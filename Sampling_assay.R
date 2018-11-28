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
  switch(EXPR = method_det,
         "plating" = 2500,
         "enrichment" = 1,
         "ELISA aflatoxin" = 1,
         stop("Unknown detection method", call. = FALSE))
}

# Create a function that makes decisions for continuous spread scenarios
decision_cont = function(df, LOD, case, m, M){
  
  ## Get the appropriate attribute plan
  micro_ct = get_attr_plan(case = case, m = m, M = M)
  
  ## Check if n_sp == n in the microbiological criteria
  if(nrow(df) != micro_ct[["n"]]){
    warning("n_sp does not equal to n.")
  }
  
  ## Find out the sample points with contamination level >= LOD
  conc = df[["cont_level"]]
  detected = conc >= LOD
  
  ## Decision: reject lot if any sample has contamination level >= M or if the number of samples with contamination level >= m is above c
  if(!any(detected)){
    return(1)
  } else {
    
    if(any(conc[detected] >= micro_ct[["M"]])){
      return(2)
    } else if (sum(conc[detected] >= micro_ct[["m"]]) > micro_ct[["c"]]){
      return(3)
    } else {
      return(4)
    }
  } 
}

# Create a function that makes decision for discrete spread scenarios
decision_dis = function(df, LOD, Mc){
  
  ## Calculate the mean concentration of all samples
  c_bar = mean(df[["dis_level"]])
  
  ## Determine whether c_bar is above LOD. If it is >= LOD, then determine if it exceeds Mc.
  if(c_bar < LOD){
    return(5)
  } else {
    if(c_bar >= Mc){
      return(6)
    } else {
      return(7)
    }
  }
}

# Create a function that decides whether to accept or reject the lot
lot_decision = function(data, case, m, M, Mc, spread, method_det){
  
  ## Get LOD for the chosen method of detection
  LOD = get_LOD(method_det = method_det)
  
  ## Subset out the contamination levels at the sample points
  a = subset(x = data, subset = label == "sample point", select = c(cont_level, dis_level), drop = FALSE)
  
  ## Make decision based on the spread type
  if(spread == "continuous"){
    decision_cont(df = a, LOD = LOD, case = case, M = M, m = m)
  } else if (spread == "discrete"){
    decision_dis(df = a, LOD = LOD, Mc = Mc)
  } else {
    stop("Unknown type of spread. Choose either 'continuous' or 'discrete'.")
  }
  
}

# Create texts for lot decisions.
words = function(x){
  switch(EXPR = x,
         `1` = cat("Accept lot. Microbial load < LOD."),
         `2` = cat("Reject lot. At least 1 sample has contamination level >= M."),
         `3` = cat("Reject lot. The number of positive samples is > c."),
         `4` = cat("Accept lot."),
         `5` = cat("Accept lot. Mean sample concentration < LOD."),
         `6` = cat("Reject lot. Mean sample concentration >= Mc"),
         `7` = cat("Accept lot."),
         warning("Unknown lot decision.", call. = FALSE))
}