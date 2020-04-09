# A function for loading the input parameters into a list
load_once = function(input, output){
  
  if(input$sidebarMenu == "2D"){
    # Manual mode: 2D
    chosen_mode = "2D"
    spread = "continuous"
    
    if(input$by == "2d"){
      n_strata = c(input$n_strata_row, input$n_strata_col)
    } else {
      n_strata = input$n_strata
    }
    
    ArgList_default = list(n_contam = input$n_contam, lims = list(xlim = c(0, input$x_lim), ylim = c(0, input$y_lim)),
                           spread = spread, spread_radius = input$spread_radius,
                           cont_level = c(input$cont_level_mu, input$cont_level_sd), method_sp = input$method_sp,
                           n_sp = input$n_sp, n_strata = n_strata, by = input$by, LOC = input$LOC,
                           fun = input$fun, case = input$case, m = input$m, M = input$M, m_sp = input$m_sp,
                           method_det = input$method_det, bg_level = input$bg_level, geom = input$geom)
    
  } else if(input$sidebarMenu == "3D"){
    # Manual mode: 3D
    chosen_mode = "3D"
    spread = "discrete"
    
  } else if (input$sidebarMenu == "v_smart"){
    
    # Smart mode
    chosen_mode = "v_smart"
    
    if(input$spread_vs == "continuous"){
      
      # STRS or k-step SS
      if(input$method_sp_vs != "srs"){
        if(input$by_vs == "2d"){
          n_strata = c(input$n_strata_row_vs, input$n_strata_col_vs)
        } else {
          n_strata = input$n_strata_vs
        }
      } else {
        n_strata = NA
      }
      
      ArgList_default = list(n_contam = input$n_contam_vs, lims = list(xlim = c(0, input$x_lim_vs), ylim = c(0, input$y_lim_vs)),
                             spread = input$spread_vs, spread_radius = input$spread_radius_vs,
                             cont_level = c(input$cont_level_mu_vs, input$cont_level_sd_vs), method_sp = input$method_sp_vs,
                             n_sp = input$n_sp_vs, n_strata = n_strata, by = input$by_vs, LOC = input$LOC_vs,
                             fun = input$fun_vs, case = input$case_vs, m = input$m_vs, M = input$M_vs, m_sp = input$m_sp_vs,
                             method_det = input$method_det_vs, bg_level = input$bg_level_vs, geom = input$geom_vs)
      
    } else if (input$spread_vs == "discrete"){
      ph
    } else {
      stop("Unknown spread type")
    }
    
  } else {
    stop("Unknown manual mode")
  }
  return(list(ArgList_default = ArgList_default, chosen_mode = chosen_mode))
}

# A variable-interpretation look-up table. The argument 'var' takes a variable name and returns its interpretation
explain_var = function(var){
  switch(EXPR = var, 
         "n_contam" = "Number of contamination points",
         "lims.xlim2" = "Length (m)",
         "lims.ylim2" = "Width (m)",
         "lims.zlim2" = "Height (m)",
         "spread" = "Geometry of product",
         "spread_radius" = "Radius of a contaminated zone (m)",
         "cont_level1" = "Mean contamination level (log CFU/g)",
         "cont_level2" = "Standard deviation of contamination level (log CFU/g)",
         "method_sp" = "Sampling strategy",
         "n_sp" = "Number of sample points",
         "n_strata" = "Number of strata",
         "n_strata1" = "Number of column strata",
         "n_strata2" = "Number of row strata",
         "by" = "Stratification direction",
         "LOC" = "Limit of contamination contribution",
         "fun" = "Decay function",
         "case" = "Attribute sampling plan case",
         "m" = "m",
         "M" = "M",
         "m_sp" = "Individual sample mass (g)",
         "bg_level" = "Background contamination level (CFU/g)",
         "geom" = "Geometry of hazard",
         "method_det" = "Detection method",
         "n_seed" = "Number of contamination patterns",
         "n_iter" = "Number of sampling patterns per contamination pattern",
         "n_iter_total_vs" = "Total number of iterations",
         "var_prim" = "Primary tuning parameter",
         "var_sec" = "Secondary tuning parameter",
         stop("Unknown variable name"))
}

get_tuning_info = function(n_vars, var_prim, var_sec, val_prim, val_sec){
  if(n_vars == 0){
    return(NULL)
    
  } else if (n_vars == 1){
    return(c("var_prim" = paste(var_prim, val_prim, sep = ":")))
    
  } else if (n_vars == 2){
    return(c("var_prim" = paste(var_prim, val_prim, sep = ": "), 
             "var_sec" = paste(var_sec, val_sec, sep = ": ")))
    
  } else {
    stop("Unknown number of tuning variables")
  }
}

# A function that presents the list of arguments in a nice table
make_var_table = function(Args, input, chosen_mode){
  
  # Remove unnecessary arguments
  a = unlist(Args)
  bool = names(a) %in% c("lims.xlim1", "lims.ylim1", "lims.zlim1")
  b = a[!bool]
  
  # Get the iteration information
  if(chosen_mode != "v_smart"){
    temp = c(b, "n_seed" = input$n_seed, "n_iter" = input$n_iter)
    d = get_tuning_info(n_vars = input$n_vars, var_prim = input$var_prim, 
                        var_sec = input$var_sec, val_prim = input$val_prim, val_sec = input$val_sec)
    } else if (chosen_mode == "v_smart"){
    temp = c(b, "n_iter_total_vs" = input$n_iter_total_vs)
    d = get_tuning_info(n_vars = input$n_vars_vs, var_prim = input$var_prim_vs, 
                        var_sec = input$var_sec_vs, val_prim = input$val_prim_vs, val_sec = input$val_sec_vs)
    } else {
    stop("Unknown number of tuning variables")
  }
  
  e = c(temp, d)
  
  # Interpret the variables
  var_meanings = map_chr(.x = names(e), .f = explain_var)
  
  # Make a tibble
  f = tibble(Variable = var_meanings, Value = e)
  
  return(f)
}

# Place holder
ph = p("Under development")