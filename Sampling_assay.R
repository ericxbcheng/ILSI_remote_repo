# Create a function that returns the proper attribute plan based on the case
get_attr_plan = function(case, m, M = 0){
  stopifnot(is.numeric(case), is.numeric(m), is.numeric(M))
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
    if(M != 0){
      warning(paste0("Case ", case, ": 2-class plan. M is coerced to 0." ))
      M = 0
    }
  }
  
  ## Append m and M to the list of n_asp and c
  b = append(x = a, values = list(m = m, M = M))
  
  return(b)
}

# Create a function that provides a LOD value corresponding to the detection method. 
# V = sample volume. Enrichment can theoretically detect 1 CFU as in 1 cell
get_LOD = function(method_det){
  switch(EXPR = method_det,
         "plating" = 2500,
         "enrichment" = 1,
         "ELISA aflatoxin" = 1,
         stop("Unknown detection method", call. = FALSE))
}

# Create a function that converts concentration values to binary results
conc2bin = function(method_det, LOD, conc, m_sp){
  
  # Check point
  stopifnot(method_det %in% c("plating", "enrichment"))
  
  if(method_det == "plating"){
    a = {conc >= LOD}
    return(a)
    
  } else {
    
    # Find the samples that has < 1 CFU
    CFU = conc * m_sp
    a = {CFU < LOD}
    
    ## For samples whose CFU < 1, we assume the enrichment result follows a Bernoulli distribution with p = CFU
    ## For samples whose CFU >=1, its enrichment result would be positive (denoted as 1)
    b = vector(mode = "numeric", length = length(conc))
    b[a] = rbinom(n = sum(a), size = 1, prob = CFU[a])
    b[!a] = 1
    
    return(as.logical(b))
  }
}

# Convert binary detection result to lot decision (plating)
bin2deci_plating = function(micro_ct, conc, detected){
  
  ## Decision: reject lot if any sample has contamination level >= M OR 
  ## if the number of samples with contamination level >= m is above c
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

# Convert binary detection result to lot decision (enrichment)
bin2deci_enrichment = function(micro_ct, detected){
  
  # Checkpoint: c should be 0 when using enrichment
  if(micro_ct[["c"]] != 0){
    warning("For enrichment please choose a 2-class plan (case 10 ~ 15).")
  }
  
  # A lot would be accepted only when all samples are negative
  if(!any(detected)){
    return(1)
  } else {
    return(3) 
  }
}

# Create a function that makes a lot decision based on the attribute sampling plan
bin2deci = function(micro_ct, detected, method_det, conc){
  
  # Check point
  stopifnot(method_det %in% c("plating", "enrichment"))
  
  if(method_det == "plating"){
    bin2deci_plating(micro_ct = micro_ct, conc = conc, detected = detected)
    
  } else {
    bin2deci_enrichment(micro_ct = micro_ct, detected = detected)
    
  }
}

decision_cont_new = function(conc, LOD, case, m, M, m_sp, method_det){
  
  # Get the attribute sampling plan parameters
  a = get_attr_plan(case = case, m = m, M = M)
  
  # Checkpoint: See if n in attribute plan matches n_sp:
  if(a$n != length(conc)){
    warning("n_sp does not match n from attribute plan!")
  }
  
  ## Convert concentration to binary result
  # For plating, it means whether a sample can be detected or not
  # For enrichment, it means whether a sample is positive or not
  b = conc2bin(method_det = method_det, LOD = LOD, conc = conc, m_sp = m_sp)
  
  # Return lot decision
  c = bin2deci(micro_ct = a, detected = b, method_det = method_det, conc = conc)
  
  return(c)
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