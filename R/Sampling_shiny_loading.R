#Helper: create the dis_level list in the 3D mode
make_dis_level_gui = function(input, chosen_mode){

  # manual or smart?
  if(chosen_mode == "3D"){
    type = input$dis_level_type

    # dis_level: constant VS Gamma
    if(type == "constant"){
      dis_level = make_dis_level(type = type, args = input$dis_level_const_arg)

    } else if (type == "Gamma"){
      dis_level = make_dis_level(type = type, args = c(input$dis_level_gm_mode, input$dis_level_gm_lb))

    } else {
      stop("Unknown discrete level type. Choose 'constant' or 'Gamma'.")
    }
  } else if (chosen_mode == "v_smart"){
    type = input$dis_level_type_vs

    # dis_level: constant VS Gamma
    if(type == "constant"){
      dis_level = make_dis_level(type = type, args = input$dis_level_const_arg_vs)

    } else if (type == "Gamma"){
      dis_level = make_dis_level(type = type, args = c(input$dis_level_gm_mode_vs, input$dis_level_gm_lb_vs))

    } else {
      stop("Unknown discrete level type. Choose 'constant' or 'Gamma'.")
    }
  } else {
    stop("Wrong chosen mode.")
  }
  return(dis_level)
}

# Make n_strata based on 'method_sp' and 'by' for smart versions (2D + 3D)
make_n_strata = function(input){

  if(input$spread_vs == "continuous"){

    # SRS: n_strata = NA
    # STRS: n_strata = row/col/c(row, col)
    # SS: n_strata = row/col

    if(input$method_sp_vs != "srs"){
      if(input$by_vs == "2d"){
        n_strata = c(input$n_strata_col_vs, input$n_strata_row_vs)
      } else {
        n_strata = input$n_strata_vs
      }
    } else {
      n_strata = NA
    }

  } else if (input$spread_vs == "discrete"){

    # SRS: n_strata = NA
    # STRS: n_strata = row/col/c(row, col)
    # SS: n_strata = NA

    if(input$method_sp_3d_vs == "strs"){
      if(input$by_3d_vs == "2d"){
        n_strata = c(input$n_strata_col_3d_vs, input$n_strata_row_3d_vs)
      } else {
        n_strata = input$n_strata_3d_vs
      }
    } else {
      n_strata = NA
    }

  } else {
    stop("Unknown spread type")
  }
  return(n_strata)
}

# A function for loading the input parameters into a list
load_once = function(input, output, conc_neg){

  if(input$sidebarMenu == "2D"){

    chosen_mode = "2D"
    ArgList_default = load_once_manual_2D(input = input)

  } else if(input$sidebarMenu == "3D"){

    chosen_mode = "3D"
    ArgList_default = load_once_manual_3D(input = input, conc_neg = conc_neg)

  } else if (input$sidebarMenu == "v_smart"){

    chosen_mode = "v_smart"
    if(input$spread_vs == "continuous"){

      ArgList_default = load_once_smart_2D(input = input)

    } else if (input$spread_vs == "discrete"){

      ArgList_default = load_once_smart_3D(input = input, conc_neg = conc_neg)

    } else {
      stop("Unknown spread type")
    }
  } else {
    stop("Unknown mode. Choose 2D, 3D, or smart version.")
  }
  return(list(ArgList_default = ArgList_default, chosen_mode = chosen_mode))
}

# A variable-interpretation look-up table. The argument 'var' takes a variable name and returns its interpretation
explain_var = function(var){
  switch(EXPR = var,
         "n_contam" = "Number of contamination points",
         "c_hat" = "Overall mycotoxin level (ppb)",
         "lims.xlim2" = "Length (m)",
         "lims.ylim2" = "Width (m)",
         "lims.zlim2" = "Height (m)",
         "spread" = "Geometry of product",
         "spread_radius" = "Radius of a contaminated zone (m)",
         "n_affected" = "Number of grains in a cluster",
         "cont_level1" = "Mean contamination level (log CFU/g)",
         "cont_level2" = "Standard deviation of contamination level (log CFU/g)",
         "dis_level.type" = "Mycotoxin distribution in contaminated grains",
         "dis_level.args" = "Mycotoxin level in contaminated grains(ppb)",
         "dis_level.args.mode" = "Mode (Most frequent level)(ppb)",
         "dis_level.args.lb" = "Lower bound (ppb)",
         "sp_radius" = "Probe radius (m)",
         "L" = "Probe length (m)",
         "rho" = "Density (g/cm^3)",
         "m_kbar" = "Single kernel mass (g)",
         "covar_mat" = "Cluster covariance matrix",
         "tox" = "Mycotoxin",
         "Mc" = "Mc (ppb)",
         "homogeneity" = "% Grinding",
         "container" = "Grain container",
         "type" = "Hopper car type",
         "compartment" = "Number of compartments",
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

  ### Remove unnecessary arguments
  # Remove conc_neg
  if(!is.null(Args$conc_neg)){
    Args$conc_neg = NULL
  }

  # Unlist covar_mat
  if(!is.null(Args$covar_mat)){
    Args$covar_mat = paste0(unlist(Args$covar_mat), collapse = ",")
  }

  a = unlist(Args)
  bool = names(a) %in% c("lims.xlim1", "lims.ylim1", "lims.zlim1", "verbose")
  b = a[!bool]

  # Get the iteration information
  if(chosen_mode == "2D"){
      temp = c(b, "n_seed" = input$n_seed, "n_iter" = input$n_iter)
      d = get_tuning_info(n_vars = input$n_vars, var_prim = input$var_prim,
                          var_sec = input$var_sec, val_prim = input$val_prim, val_sec = input$val_sec)
    } else if (chosen_mode == "1D"){
      temp = c(b, "n_seed" = input$n_seed_1D, "n_iter" = input$n_iter_1D)
      d = get_tuning_info(n_vars = input$n_vars_1D, var_prim = input$var_prim_1D,
                          var_sec = input$var_sec_1D, val_prim = input$val_prim_1D, val_sec = input$val_sec_1D)


    } else if (chosen_mode == "3D"){
      temp = c(b, "n_seed" = input$n_seed_3d, "n_iter" = input$n_iter_3d)
      d = get_tuning_info(n_vars = input$n_vars_3d, var_prim = input$var_prim_3d,
                          var_sec = input$var_sec_3d, val_prim = input$val_prim_3d, val_sec = input$val_sec_3d)

    } else if (chosen_mode == "v_smart"){

      if(input$spread_vs == "continuous"){
        temp = c(b, "n_iter_total_vs" = input$n_iter_total_vs)
        d = get_tuning_info(n_vars = input$n_vars_vs, var_prim = input$var_prim_vs,
                            var_sec = input$var_sec_vs, val_prim = input$val_prim_vs, val_sec = input$val_sec_vs)

      } else if (input$spread_vs == "discrete"){
        temp = c(b, "n_iter_total_vs" = input$n_iter_total_3d_vs)
        d = get_tuning_info(n_vars = input$n_vars_3d_vs, var_prim = input$var_prim_3d_vs,
                            var_sec = input$var_sec_3d_vs, val_prim = input$val_prim_3d_vs,
                            val_sec = input$val_sec_3d_vs)

      } else {
        stop("Unknown spread type.")
      }
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

####################### 2D ###############################

# Load parameters once for 2D manual version
load_once_manual_2D = function(input){

  if(input$by == "2d"){
    n_strata = c(input$n_strata_col, input$n_strata_row)
  } else {
    n_strata = input$n_strata
  }

  ArgList_default = list(n_contam = input$n_contam, lims = list(xlim = c(0, input$x_lim), ylim = c(0, input$y_lim)),
                         spread = "continuous", spread_radius = input$spread_radius,
                         cont_level = c(input$cont_level_mu, input$cont_level_sd), method_sp = input$method_sp,
                         n_sp = input$n_sp, n_strata = n_strata, by = input$by, LOC = input$LOC,
                         fun = input$fun, case = input$case, m = input$m, M = input$M, m_sp = input$m_sp,
                         method_det = input$method_det, bg_level = input$bg_level, geom = input$geom)
  return(ArgList_default)
}

# Load parameters once for 2D smart mode
load_once_smart_2D = function(input){

  # 2 tuning variables: override 'by' and 'n_strata'
  if(input$n_vars_vs != 2){
    by = input$by_vs
    n_strata = make_n_strata(input = input)

  } else if(input$n_vars_vs == 2){
    by = input$by_vs_tune
    n_strata = input$n_strata_vs_tune

  } else {
    stop("Unknown number of tuning variables")
  }

  ArgList_default = list(n_contam = input$n_contam_vs, lims = list(xlim = c(0, input$x_lim_vs), ylim = c(0, input$y_lim_vs)),
                         spread = input$spread_vs, spread_radius = input$spread_radius_vs,
                         cont_level = c(input$cont_level_mu_vs, input$cont_level_sd_vs), method_sp = input$method_sp_vs,
                         n_sp = input$n_sp_vs, n_strata = n_strata, by = by, LOC = input$LOC_vs,
                         fun = input$fun_vs, case = input$case_vs, m = input$m_vs, M = input$M_vs, m_sp = input$m_sp_vs,
                         method_det = input$method_det_vs, bg_level = input$bg_level_vs, geom = input$geom_vs)
  return(ArgList_default)
}

####################### 3D ##############################

# Load parameters once for 3D manual version
load_once_manual_3D = function(input, conc_neg){

  # Create the discrete contamination level
  dis_level = make_dis_level_gui(input = input, chosen_mode = "3D")

  # n_affected: = 0 VS > 0
  if(input$n_affected > 0){
    covar_mat = make_covar_mat(spread = "discrete", varx = input$vcov_11, vary = input$vcov_22, varz = input$vcov_33,
                               covxy = input$vcov_12, covxz = input$vcov_13, covyz = input$vcov_23)
  } else {
    covar_mat = NULL
  }

  # by = "2d" or "row/column"
  if(input$by_3d == "2d"){
    n_strata = c(input$n_strata_col_3d, input$n_strata_row_3d)
  } else {
    n_strata = input$n_strata_3d
  }

  ArgList_default = list(c_hat = input$c_hat, lims = list(xlim = c(0, input$x_lim_3d),
                                                          ylim = c(0, input$y_lim_3d),
                                                          zlim = c(0, input$z_lim_3d)),
                         spread = "discrete", covar_mat = covar_mat, n_affected = input$n_affected,
                         dis_level = dis_level, method_sp = input$method_sp_3d,
                         sp_radius = input$d/2, n_sp = input$n_sp_3d, n_strata = n_strata,
                         by = input$by_3d, L = input$z_lim_3d, rho = input$rho,
                         m_kbar = input$m_kbar, conc_neg = conc_neg, tox = input$tox,
                         Mc = input$Mc, method_det = input$method_det_3d, verbose = FALSE,
                         homogeneity = input$homogeneity, container = input$container,
                         compartment = input$compartment, type = input$type)

  return(ArgList_default)
}

# Load parameters once for 3D smart version
load_once_smart_3D = function(input, conc_neg){

  # Create the discrete contamination level
  dis_level = make_dis_level_gui(input = input, chosen_mode = "v_smart")

  # n_affected: = 0 VS > 0
  if(input$n_affected_vs > 0){
    covar_mat = make_covar_mat(spread = "discrete", varx = input$vcov_11_vs, vary = input$vcov_22_vs,
                               varz = input$vcov_33_vs, covxy = input$vcov_12_vs,
                               covxz = input$vcov_13_vs, covyz = input$vcov_23_vs)
  } else {
    covar_mat = NULL
  }

  #### Override: n_sp, by, n_strata, container

  # SRS: n_sp = ..., by = NA, container = compartment = type = NA
  # STRS: n_sp = ..., by = ..., n_strata = ..., container = compartment = type = NA
  # SS: n_sp = by = n_strata = NA, container = ..., compartment = ..., type = ...
  # When n_vars_3d_vs == 2, we assume the default method_sp == "SRS"

  if(input$n_vars_3d_vs %in% c(0, 1)){
    by = input$by_3d_vs
    n_strata = make_n_strata(input = input)
    container = input$container_vs
    compartment = input$compartment_vs
    type = input$type_vs

  } else if (input$n_vars_3d_vs == 2){
    by = input$by_3d_vs_tune
    n_strata = input$n_strata_3d_vs_tune
    container = input$container_vs_tune
    compartment = input$compartment_vs_tune
    type = input$type_vs_tune

  } else {
    stop("Unknown number of tuning variables")
  }

  ArgList_default = list(c_hat = input$c_hat_vs, lims = list(xlim = c(0, input$x_lim_3d_vs),
                                                          ylim = c(0, input$y_lim_3d_vs),
                                                          zlim = c(0, input$z_lim_3d_vs)),
                         spread = "discrete", covar_mat = covar_mat, n_affected = input$n_affected_vs,
                         dis_level = dis_level, method_sp = input$method_sp_3d_vs,
                         sp_radius = input$d_vs/2, n_sp = input$n_sp_3d_vs, n_strata = n_strata,
                         by = by, L = input$z_lim_3d_vs, rho = input$rho_vs,
                         m_kbar = input$m_kbar_vs, conc_neg = conc_neg, tox = input$tox_vs,
                         Mc = input$Mc_vs, method_det = input$method_det_3d_vs, verbose = FALSE,
                         homogeneity = input$homogeneity_vs, container = container,
                         compartment = compartment, type = type)

  return(ArgList_default)
}
