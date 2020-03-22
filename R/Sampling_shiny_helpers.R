# A function for visualizing one iteration
vis_once = function(input, output, spread, ArgList){
  
  if(spread == "continuous"){
    
    # Remove unnecessary arguments
    ArgList_vis = ArgList
    ArgList_vis[c("case", "M", "m_sp", "method_det")] = NULL
    ArgList_vis$seed = NaN
    
    # Produce intermediate outputs
    one_iteration = do.call(what = sim_intmed, args = ArgList_vis)
    output$overlay_draw = renderPlot(expr = {overlay_draw(method_sp = ArgList_vis$method_sp, data = one_iteration[["contam_sp_xy"]] , 
                                                          spread = ArgList_vis$spread, xlim = ArgList_vis$lims$xlim, ylim = ArgList_vis$lims$ylim, 
                                                          n_strata = ArgList_vis$n_strata, by = ArgList_vis$by)})
    output$contam_level_draw = renderPlot(expr = {contam_level_draw(dimension = "3d", method = ArgList_vis$fun, 
                                                                    spread_radius = ArgList_vis$spread_radius, LOC = ArgList_vis$LOC, 
                                                                    df_contam = one_iteration[["contam_sp_xy"]] , xlim = ArgList_vis$lims$xlim, 
                                                                    ylim = ArgList_vis$lims$ylim, bg_level = ArgList_vis$bg_level, 
                                                                    geom = ArgList_vis$geom)})
    
  } else {
    message("Under construction.") 
  }
}

# A function for loading the input parameters into a list
load_once = function(input, output){
  
  if(input$sidebarMenu == "2D"){
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
    spread = "discrete"
  } else {
    stop("Unknown manual mode")
  }
  return(list(spread = spread, ArgList_default = ArgList_default))
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
         "var_prim" = "Primary tuning parameter",
         "var_sec" = "Secondary tuning parameter",
         stop("Unknown variable name"))
}


# A function that presents the list of arguments in a nice table
make_var_table = function(Args, input){
  
  # Remove unnecessary arguments
  a = unlist(Args)
  bool = names(a) %in% c("lims.xlim1", "lims.ylim1", "lims.zlim1")
  b = a[!bool]
  
  # Get the iteration information
  c = c(b, "n_seed" = input$n_seed, "n_iter" = input$n_iter)
  
  if(input$n_vars == 0){
    
    d = c
    
  } else if (input$n_vars == 1){
    
    d = c(c, "var_prim" = input$val_prim)
    
  } else if (input$n_vars == 2) {
    
    d = c(c, "var_prim" = input$val_prim, "var_sec" = input$val_sec)
    
  } else {
    stop("Unknown number of tuning variables")
  }
  
  # Interpret the variables
  var_meanings = map_chr(.x = names(d), .f = explain_var)
  
  # Make a tibble
  e = tibble(Variable = var_meanings, Value = d)
  
  return(e)
}


# Iterate the model without any tuning parameters
iterate_tune0 = function(input, Args){
  
  return(sim_iterate2(n_seed = input$n_seed, n_iter = input$n_iter, Args = Args))
  
}

# Parse the string "a,b,c" from val_prim into a numeric vector c(a,b,c)
parse_num_vec = function(string){
  return(str_split(string = string, pattern = ",") %>%
           unlist() %>%
           as.numeric())
}

# Parse the string "a,b,c" from val_sec into a character vector c("a","b","c")
parse_char_vec = function(string){
  
  # Split by the comma
  a = str_split(string = string, pattern = ",") %>%
    unlist() 
  
  # Remove extra whitespace
  return(str_trim(a))
}

# Iterate the model with one tuning parameter
iterate_tune1 = function(input, Args){
  
  vals = parse_num_vec(string = input$val_prim)
  
  return(map(.x = vals, .f = tune_param, Args = Args, n_seed = input$n_seed, n_iter = input$n_iter, param = input$var_prim))
  
}

# Function to update arguments
update_arg = function(arg_list, name, value){
  
  a = arg_list
  a[[name]] = value
  return(a)
}

# Secondary tuning parameter iterations
iterate_tune2 = function(input, Args){
  
  # Parse tuning values for primary and secondary parameters
  vals_prim = parse_num_vec(string = input$val_prim)
  vals_sec = parse_char_vec(string = input$val_sec)
  
  # Create a list of argument lists, each argument list corresponding to one secondary tuning value
  Args_sec = map(.x = vals_sec, .f = update_arg, arg_list = Args, name = input$var_sec)
  
  # For each argument list in Args_sec, do iterate_tune1()
  sim_data = list()
  for(i in 1:length(vals_sec)){
    
    sim_data[[i]] = map(.x = vals_prim, .f = tune_param, 
                        Args = Args_sec[[i]], n_seed = input$n_seed, n_iter = input$n_iter,param = input$var_prim)
    
  }
  return(list(sim_data = sim_data, vals_prim = vals_prim, vals_sec = vals_sec))
}

# A summary function for 0 tuning parameter
metrics_cont_0 = function(data){
  
  # Calculate the acceptance prob
  a = calc_Prej_one(data) %>%
    tibble(P_rej = ., seed = as.numeric(names(.)), Paccept = 1 - .)
  return(a)
}

# Data cleaning function for secondary tuning scenarios
metrics_cont_sec = function(data, input, vals_prim, vals_sec){
  
  # Data should contain 2 layers. 
  # The outer layer contains data for different secondary tuning. 
  # The inner layer contains data for different primary tuning under each value of the secondary tuning.
  a = map(.x = data, .f = metrics_cont_n) %>%
    bind_rows()
  
  # Add a column to indicate secondary tuning values
  b = rep(x = vals_sec, each = input$n_seed * length(vals_prim))
  
  # Combine by columns
  c = cbind.data.frame(a, b, stringsAsFactors = FALSE)
  colnames(c)[colnames(c) == "b"] = "param2"
  
  return(c)
}

# Plot when there is no tuning parameter
plot_tune0 = function(data){
  
  ggplot(data = data) +
    geom_boxplot(aes(x = "NA", y = Paccept)) +
    geom_point(aes(x = "NA",  y = mean(data$Paccept)), color = "red", pch = 4, size = 5) +
    labs(x = NULL, y = "Probability of acceptance") +
    scale_y_continuous(breaks = seq(0,1,0.1)) +
    coord_cartesian(ylim = c(0,1)) +
    theme_bw()
}

# Generate labels according to variables
gen_label = function(var){
  
  a = switch(EXPR = var,
             "n_contam" = "Number of contamination points",
             "n_sp" = "NUmber of sample points",
             "m_sp" = "Individual sample mass (g)",
             "method_sp" = "Sample strategy",
             stop("Unknown variable"))
  
  return(a)
}

# Plot when there is one tuning parameter
plot_tune1 = function(data, input){
  
  xlab = gen_label(var = input$var_prim)
  
  # Summarise the data
  a = data %>%
    gather(data = ., key = "metric", value = "value", -c(seed, param)) %>%
    group_by(param, metric) %>%
    summarise(lb = quantile(x = value, probs = 0.025), 
              med = median(x = value),
              ub = quantile(x = value, probs = 0.975)) %>%
    dplyr::filter(metric == "Paccept")
  
  # Visualize
  b = ggplot(data = a) +
    geom_ribbon(aes_string(x = "param", ymin = "lb", ymax = "ub"), alpha = 0.3, color = "lightgrey") +
    geom_line(aes_string(x = "param", y = "med")) +
    geom_point(aes_string(x = "param", y = "med")) +
    scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.1)) +
    coord_cartesian(ylim = c(0,1)) +
    labs(x = xlab, y = "Probability of acceptance (2.5th - 97.5th percentile)") +
    theme_bw() 
  return(b)
}

# Plot when there is one tuning parameter
plot_tune2_ribbon = function(data, input){
  
  # Make the x axis and legend labels
  xlab = gen_label(var = input$var_prim)
  legend_lab = gen_label(var = input$var_sec)
  
  # Summarise the data
  a = data %>%
    gather(data = ., key = "metric", value = "value", -c(seed, param, param2)) %>%
    group_by(param2, param, metric) %>%
    summarise(lb = quantile(x = value, probs = 0.025), 
              med = median(x = value),
              ub = quantile(x = value, probs = 0.975)) %>%
    dplyr::filter(metric == "Paccept")
  
  # Visualize
  b = ggplot(data = a) +
    geom_ribbon(aes_string(x = "param", ymin = "lb", ymax = "ub", group = "param2", fill = "param2"), alpha = 0.3) +
    geom_line(aes_string(x = "param", y = "med", color = "param2")) +
    geom_point(aes_string(x = "param", y = "med", color = "param2")) +
    scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.1)) +
    scale_fill_discrete(name = legend_lab) +
    scale_color_discrete(name = legend_lab) +
    coord_cartesian(ylim = c(0,1)) +
    labs(x = xlab, y = "Probability of acceptance (2.5th - 97.5th percentile)") +
    theme_bw() +
    theme(legend.position = "top")
  
  
  return(b)
}

# Visualize with boxplots
plot_tune2_boxplot = function(data, input, yvar){
  
  # Make the x axis and legend labels
  xlab = gen_label(var = input$var_prim)
  ylab = switch(EXPR = yvar, 
                "P_det" = "Detection Probability", 
                "Paccept" = "Probability of acceptance")
  legend_lab = gen_label(var = input$var_sec)
  
  # Summarise the data
  
  a = ggplot(data = data, aes_string(y = yvar)) +
    geom_boxplot(aes(x = as.factor(param), group = interaction(param, param2), fill = param2)) +
    scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.1)) +
    coord_cartesian(ylim = c(0,1)) +
    scale_fill_discrete(name = legend_lab) +
    labs(x = xlab, y = ylab) +
    theme_bw()+
    theme(legend.position = "top")
  return(a)
}
