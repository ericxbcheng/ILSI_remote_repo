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

# Iterate the model without any tuning parameters
iterate_tune0 = function(input, Args, chosen_mode){

  if(chosen_mode == "2D"){
    n_seed = input$n_seed
    n_iter = input$n_iter

  } else if (chosen_mode == "1D"){
    n_seed = input$n_seed_1D
    n_iter = input$n_iter_1D

  } else if (chosen_mode == "3D"){
    n_seed = input$n_seed_3d
    n_iter = input$n_iter_3d

  } else if (chosen_mode == "v_smart"){

    if(input$spread_vs == "continuous"){
      n_seed = n_iter = round(sqrt(input$n_iter_total_vs))

    } else if (input$spread_vs == "discrete"){
      n_seed = n_iter = round(sqrt(input$n_iter_total_3d_vs))

    }
  } else {
    stop("Unknown chosen mode")
  }

  return(sim_iterate2(n_seed = n_seed, n_iter = n_iter, Args = Args))
}

# Iterate the model with one tuning parameter
iterate_tune1_gui = function(input, Args, chosen_mode){

  if(chosen_mode == "2D"){
    n_seed = input$n_seed
    n_iter = input$n_iter
    var_prim = input$var_prim
    vals = parse_num_vec(string = input$val_prim)

  } else if (chosen_mode == "3D"){
    n_seed = input$n_seed_3d
    n_iter = input$n_iter_3d
    var_prim = input$var_prim_3d
    vals = parse_num_vec(string = input$val_prim_3d)

  } else if (chosen_mode == "v_smart"){

    if(input$spread_vs == "continuous"){
      n_seed = n_iter = round(sqrt(input$n_iter_total_vs))
      var_prim = input$var_prim_vs
      vals = parse_num_vec(string = input$val_prim_vs)

    } else if (input$spread_vs == "discrete"){
      n_seed = n_iter = round(sqrt(input$n_iter_total_3d_vs))
      var_prim = input$var_prim_3d_vs
      vals = parse_num_vec(string = input$val_prim_3d_vs)

    } else {
      stop("Unknown spread type.")
    }
  }

  return(tune_param_n(vals = vals, Args = Args, n_seed = n_seed, n_iter = n_iter, var_prim = var_prim))
}

# Secondary tuning parameter iterations
iterate_tune2_gui = function(input, Args, chosen_mode){

  # Parse tuning values for primary and secondary parameters
  if(chosen_mode == "2D"){
    n_seed = input$n_seed
    n_iter = input$n_iter
    var_prim = input$var_prim
    var_sec = input$var_sec
    vals_prim = parse_num_vec(string = input$val_prim)
    vals_sec = parse_char_vec(string = input$val_sec)

  } else if (chosen_mode == "3D"){
    n_seed = input$n_seed_3d
    n_iter = input$n_iter_3d
    var_prim = input$var_prim_3d
    var_sec = input$var_sec_3d
    vals_prim = parse_num_vec(string = input$val_prim_3d)
    vals_sec = parse_char_vec(string = input$val_sec_3d)

  } else if (chosen_mode == "v_smart"){
    if(input$spread_vs == "continuous"){
      n_seed = n_iter = round(sqrt(input$n_iter_total_vs))
      var_prim = input$var_prim_vs
      var_sec = input$var_sec_vs
      vals_prim = parse_num_vec(string = input$val_prim_vs)
      vals_sec = parse_char_vec(string = input$val_sec_vs)

    } else if (input$spread_vs == "discrete"){
      n_seed = n_iter = round(sqrt(input$n_iter_total_3d_vs))
      var_prim = input$var_prim_3d_vs
      var_sec = input$var_sec_3d_vs
      vals_prim = parse_num_vec(string = input$val_prim_3d_vs)
      vals_sec = parse_char_vec(string = input$val_sec_3d_vs)

    } else {
      stop("Unknown spread type")
    }
  }

  # Conduct the 2-parameter tuning
  sim_data = tune_param_sec(Args = Args, var_prim = var_prim, vals_prim = vals_prim,
                            var_sec = var_sec, vals_sec = vals_sec, n_seed = n_seed, n_iter = n_iter)

  return(list(sim_data = sim_data, vals_prim = vals_prim, vals_sec = vals_sec))
}

# Iterate and tuning for both 2D and 3D
f_iterate_tune = function(input, output, Args, chosen_mode){

  # Find the correct n_vars
  if(chosen_mode == "2D"){
    # 2D manual
    n_vars = input$n_vars
    return(f_iterate_tune_2d(input = input, output = output, Args = Args,
                             n_vars = n_vars, chosen_mode = chosen_mode))

  } else if (chosen_mode == "1D"){

    n_vars = input$n_vars_1D
    return(f_iterate_tune_2d(input = input, output = output, Args = Args,
                             n_vars = n_vars, chosen_mode = chosen_mode))

  } else if (chosen_mode == "3D"){
    # 3D manual
    n_vars = input$n_vars_3d
    return(f_iterate_tune_3d(input = input, output = output, Args = Args,
                             n_vars = n_vars, chosen_mode = chosen_mode))

  } else if (chosen_mode == "v_smart"){

    if(input$spread_vs == "continuous"){
      # 2D smart
      n_vars = input$n_vars_vs
      return(f_iterate_tune_2d(input = input, output = output, Args = Args,
                               n_vars = n_vars, chosen_mode = chosen_mode))

    } else if (input$spread_vs == "discrete"){
      # 3D smart
      n_vars = input$n_vars_3d_vs
      return(f_iterate_tune_3d(input = input, output = output, Args = Args,
                               n_vars = n_vars, chosen_mode = chosen_mode))

    } else {
      stop("Unknown spread type.")
    }
  } else {
    stop("Unknown chosen mode")
  }
}

# UI for tuning (smart mode)
f_ui_tuning_vs = function(input, ...){
  if(input$spread_vs == "continuous"){
    f_ui_tuning_2d_vs(input = input)

  } else if(input$spread_vs == "discrete"){
    f_ui_tuning_3d_vs(input = input)

  } else {
    stop("Unknown spread type")
  }
}


#################################### 2D #############################################

# The P_det/Paccept visualization switch (only for 2 tuning parameters)
f_yvar = function(input, chosen_mode){

  # Select the right n_vars input parameter
  if(chosen_mode != "v_smart"){
    n_vars = input$n_vars

  } else if(chosen_mode == "v_smart"){
    n_vars = input$n_vars_vs

  } else {
    stop("Unknown chosen mode")
  }

  # Create a radio button for either plotting P_det or Paccept when n_vars == 2
  if(n_vars == 2){
    radioButtons(inputId = "yvar",
                 label = NULL,
                 choices = list("Detection probability" = "P_det",
                                "Acceptance probability" = "Paccept"),
                 inline = TRUE)
  } else {
    NULL
  }
}

# # Data cleaning function for secondary tuning scenarios
metrics_cont_sec_gui = function(data, input, vals_prim, vals_sec, chosen_mode){

  # Determine n_seed
  if(chosen_mode != "v_smart"){
    n_seed = input$n_seed
  } else if(chosen_mode == "v_smart"){
    n_seed = round(sqrt(input$n_iter_total_vs))
  } else {
    stop("Unknown chosen mode")
  }

  # Summarize the metrics
  return(metrics_cont_sec(data = data, vals_prim = vals_prim, vals_sec = vals_sec, n_seed = n_seed))
}

# Reactively present choices (2D)
f_var_tuning_2d = function(geom){

  choices = list("Number of contamination points" = "n_contam",
                 "Number of sample points" = "n_sp",
                 "Individual sample mass (g)" = "m_sp")

  if(geom == "point"){
    return(choices)
  } else if (geom == "area"){
    return(choices[-1])
  } else {
    stop("Unknown geometry. Choose 'point' or 'area'.")
  }
}

# UI for tuning 2D smart mode
f_ui_tuning_2d_vs = function(input, ...){
  if(input$n_vars_vs == 0){
    NULL

  } else if(input$n_vars_vs == 1){
    verticalLayout(
      p("Q15A. Which parameter do you want to tune?"),
      selectInput(inputId = "var_prim_vs",
                  label = NULL,
                  choices = f_var_tuning_2d(geom = input$geom_vs)),
      p("Q15B. What values do you want to tune over? (separated by a comma)"),
      textInput(inputId = "val_prim_vs", label = NULL, value = "1,2,3")
    )

  } else if(input$n_vars_vs == 2) {
    verticalLayout(
      p("Q15A. Which primary parameter do you want to tune?"),
      selectInput(inputId = "var_prim_vs",
                  label = NULL,
                  choices = f_var_tuning_2d(geom = input$geom_vs)),
      p("Q15B. What values do you want to tune the primary parameter over? (separated by a comma)"),
      textInput(inputId = "val_prim_vs", label = NULL, value = "1,2,3"),
      p("Q15C. Which secondary parameter do you want to tune?"),
      selectInput(inputId = "var_sec_vs",
                  label = NULL,
                  choices = list("Sampling strategy" = "method_sp")),
      wellPanel(
        p("Q15C-1. Override: stratification direction"),
        selectInput(inputId = "by_vs_tune",
                    label = NULL,
                    choices = list("Vertical" = "row", "Horizontal" = "column"),
                    multiple = FALSE),
        p("Q15C-2. Override: number of strata (must be a factor of number of samples)"),
        numericInput(inputId = "n_strata_vs_tune", label = NULL, value = NULL, min = 1)
      ),
      p("Q15D. What values do you want to tune the secondary parameter over? (separated by a comma)"),
      textInput(inputId = "val_sec_vs", label = NULL, value = "srs, strs, ss")
    )
  } else {
    stop("Unknown number of tuning paramters")
  }
}

# Iterate and tuning for 2D mode (smart + manual)
f_iterate_tune_2d = function(input, output, Args, n_vars, chosen_mode){

  if(n_vars == 0){

    # When there is no tuning parameter
    data_raw = iterate_tune0(input = input, Args = Args, chosen_mode = chosen_mode)
    data_cleaned = metrics_cont_0(data = data_raw)
    return(list(data_cleaned = data_cleaned, n_vars = n_vars))

  } else if (n_vars == 1){

    # When there is 1 tuning parameter
    data_raw = iterate_tune1_gui(input = input, Args = Args, chosen_mode = chosen_mode)
    data_cleaned = metrics_cont_n(data = data_raw)
    return(list(data_cleaned = data_cleaned, n_vars = n_vars))

  } else if (n_vars == 2) {

    data_raw = iterate_tune2_gui(input = input, Args = Args, chosen_mode = chosen_mode)
    data_cleaned = metrics_cont_sec_gui(data = data_raw[["sim_data"]],
                                    input = input,
                                    vals_prim = data_raw[["vals_prim"]],
                                    vals_sec = data_raw[["vals_sec"]], chosen_mode = chosen_mode)
    return(list(data_cleaned = data_cleaned, n_vars = n_vars))

  } else {
    message("Unknown number of tuning variables")
  }
}

############################################## 3D ##########################################

# Reactively present choices (3D)
f_var_tuning_3d = function(method_sp, n_vars){

  choices = list("Overall mycotoxin level (ppb)" = "c_hat",
                 "Number of probes" = "n_sp",
                 "Number of grains in a cluster" = "n_affected",
                 "Sampling strategy" = "method_sp")

  if(n_vars == 1){

    if(method_sp == "ss"){
      return(choices[c(1,3)])

    } else if (method_sp %in% c("srs", "strs")){
      return(choices[1:3])

    } else {
      stop("Unknown sampling strategy. Choose 'srs', 'strs', or 'ss'.")
    }

  } else if (n_vars == 2){

    if(method_sp == "ss"){
      return(choices[c(3,4)])

    } else if (method_sp %in% c("srs", "strs")){
      return(choices[2:4])

    } else {
      stop("Unknown sampling strategy. Choose 'srs', 'strs', or 'ss'.")
    }

  } else {
    stop("Undefined number of tuning variables. Choose 1 or 2.")
  }
}

# Tuning 3D smart mode
f_ui_tuning_3d_vs = function(input, ...){
  if(input$n_vars_3d_vs == 0){
    NULL

  } else if(input$n_vars_3d_vs == 1){
    verticalLayout(
      p("Q15A. Which parameter do you want to tune?"),
      selectInput(inputId = "var_prim_3d_vs",
                  label = NULL,
                  choices = f_var_tuning_3d(method_sp = input$method_sp_3d_vs, n_vars = 1)),
      p("Q15B. What values do you want to tune over? (separated by a comma)"),
      textInput(inputId = "val_prim_3d_vs", label = NULL, value = "5,10,20")
    )

  } else if(input$n_vars_3d_vs == 2) {
    verticalLayout(
      p("Q15A. Which primary parameter do you want to tune?"),
      selectInput(inputId = "var_prim_3d_vs",
                  label = NULL,
                  choices = list("Overall mycotoxin level (ppb)" = "c_hat")),
      p("Q15B. What values do you want to tune the primary parameter over? (separated by a comma)"),
      textInput(inputId = "val_prim_3d_vs", label = NULL, value = "5,10,20"),
      p("Q15C. Which secondary parameter do you want to tune?"),
      selectInput(inputId = "var_sec_3d_vs",
                  label = NULL,
                  choices = f_var_tuning_3d(method_sp = input$method_sp_3d_vs, n_vars = 2)),
      conditionalPanel(condition = "input.var_sec_3d_vs == 'method_sp'",
                       wellPanel(
                         p("Q15C-1. Override: stratification direction"),
                         selectInput(inputId = "by_3d_vs_tune",
                                     label = NULL,
                                     choices = list("Vertical" = "row", "Horizontal" = "column"),
                                     multiple = FALSE),
                         p("Q15C-2. Override: number of strata (must be a factor of number of samples)"),
                         numericInput(inputId = "n_strata_3d_vs_tune", label = NULL, value = NULL, min = 1),
                         p("Q15C-3. Override: container"),
                         selectInput(inputId = "container_vs_tune", label = NULL,
                                     choices = list("Truck" = "truck", "Barge" = "barge", "Hopper car" = "hopper")),
                         conditionalPanel(condition = "input.container_vs_tune == 'hopper'",
                                          splitLayout(
                                            selectInput(inputId = "compartment_vs_tune", label = "Number of compartments", choices = list(2, 3)),
                                            selectInput(inputId = "type_vs_tune", label = "Hopper car type",
                                                        choices = list("Open-top" = "open_top", "Trough" = "trough"))
                                          ))
                       )
      ),
      p("Q15D. What values do you want to tune the secondary parameter over? (separated by a comma)"),
      textInput(inputId = "val_sec_3d_vs", label = NULL, value = "5, 10, 100")
    )
  } else {
    stop("Unknown number of tuning paramters")
  }
}

# # Data cleaning function for secondary tuning scenarios
metrics_dis_sec_gui = function(data, input, vals_prim, vals_sec, chosen_mode){

  # Determine n_seed
  if(chosen_mode == "3D"){
    n_seed = input$n_seed
  } else if(chosen_mode == "v_smart"){
    n_seed = round(sqrt(input$n_iter_total_3d_vs))
  } else {
    stop("Unknown or wrong chosen mode. Choose 3D manual or smart mode.")
  }

  # Summarize the metrics
  return(metrics_dis_sec(data = data, vals_prim = vals_prim, vals_sec = vals_sec, n_seed = n_seed))
}

# Iterate and tuning for 3D mode (smart + manual)
f_iterate_tune_3d = function(input, output, Args, n_vars, chosen_mode){

  if(n_vars == 0){

    # When there is no tuning parameter
    data_raw = iterate_tune0(input = input, Args = Args, chosen_mode = chosen_mode)
    data_cleaned = metrics_dis_0(data = data_raw)
    return(list(data_cleaned = data_cleaned, n_vars = n_vars))

  } else if (n_vars == 1){

    # When there is 1 tuning parameter
    data_raw = iterate_tune1_gui(input = input, Args = Args, chosen_mode = chosen_mode)
    data_cleaned = metrics_dis_n(data = data_raw, Mc = input$Mc, metrics = FALSE)
    return(list(data_cleaned = data_cleaned, n_vars = n_vars))

  } else if (n_vars == 2) {

    data_raw = iterate_tune2_gui(input = input, Args = Args, chosen_mode = chosen_mode)
    data_cleaned = metrics_dis_sec_gui(data = data_raw[["sim_data"]],
                                   input = input, vals_prim = data_raw[["vals_prim"]],
                                   vals_sec = data_raw[["vals_sec"]],
                                   chosen_mode = chosen_mode)
    return(list(data_cleaned = data_cleaned, n_vars = n_vars))

  } else {
    message("Unknown number of tuning variables")
  }
}
