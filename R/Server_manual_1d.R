# Tuning for the manual mode
output$ui_tuning_1D = renderUI(expr = {
  if(input$n_vars_1D == 0){
    NULL
    
  } else if(input$n_vars_1D == 1){
    verticalLayout(
      selectInput(inputId = "var_prim_1D",
                  label = "Primary tuning parameter",
                  choices = list("Number of contamination points" = "n_contam_1D",
                                 "Number of sample points" = "n_sp_1D",
                                 "Individual sample mass (g)" = "m_sp_1D")),
      textInput(inputId = "val_prim_1D", label = "Tuning value(s) (separated by a comma)", value = "1,2,3")
    )
    
  } else if(input$n_vars_1D == 2) {
    verticalLayout(
      selectInput(inputId = "var_prim_1D",
                  label = "Primary tuning parameter",
                  choices = list("Number of contamination points" = "n_contam_1D",
                                 "Number of sample points" = "n_sp_1D",
                                 "Individual sample mass (g)" = "m_sp_1D")),
      textInput(inputId = "val_prim_1D", label = "Tuning value(s)", value = "1,2,3"),
      selectInput(inputId = "var_sec_1D",
                  label = "Secondary tuning parameter",
                  choices = list("Sampling strategy" = "method_sp_1D")),
      textInput(inputId = "val_sec_1D", label = "Tuning value(s) (separated by a comma)", value = "srs, strs, ss")
    )
  } else {
    stop("Unknown number of tuning paramters")
  }
})

# Load the parameter once (manual version)
list_load = list()
observeEvent(eventExpr = {input$load_1D}, handlerExpr = {
  
  # Load the parameters once  
  list_load <<- load_once_1D(input = input, output = output)
  
  # The P_det/Paccept visualization switch (only for 2 tuning parameters)
  output$yvar = renderUI(expr = {f_yvar(input = input, chosen_mode = list_load$chosen_mode)})
  
  # The chosen parameters table
  output$print_param = renderTable(expr = make_var_table(Args = list_load$ArgList_default, 
                                                         input = input, 
                                                         chosen_mode = list_load$chosen_mode))
}, ignoreInit = TRUE, ignoreNULL = TRUE)

# Visualize for one iteration (manual mode)
observeEvent(eventExpr = {input$vis_1D}, handlerExpr = {
  
  vis_once(input = input, output = output, 
           ArgList = list_load$ArgList_default, chosen_mode = list_load$chosen_mode)
  
}, ignoreInit = TRUE, ignoreNULL = TRUE)

# Multiple iterations (manual mode)
observeEvent(eventExpr = {input$iterate_1D}, handlerExpr = {
  
  # Create a progress message
  showModal(ui = modalDialog("Iteration in progress", size = "s"))
  
  result_iter <<- f_iterate_tune(input = input, output = output, 
                                 Args = list_load$ArgList_default, 
                                 chosen_mode = list_load$chosen_mode)
  
  vis_n(data = result_iter, input = input, output = output, chosen_mode = list_load$chosen_mode)
  
  # Close the message
  removeModal()
  
}, ignoreInit = TRUE, ignoreNULL = TRUE)


load_once_1D <- function(input, output, conc_neg){
  
  chosen_mode = "1D"
  ArgList_default = load_once_manual_1D(input = input)
  
  return(list(ArgList_default = ArgList_default, chosen_mode = chosen_mode))
}

load_once_manual_1D <- function(input){
  
  if(input$by == "1d"){
    n_strata = c(input$n_strata_row_1D, input$n_strata_col_1D)
  } else {
    n_strata = input$n_strata_1D
  }
  
  ArgList_default = list(n_contam = input$n_contam_1D, lims = list(xlim = c(0, input$x_lim_1D), ylim = c(0, input$y_lim_1D)),
                         spread = "continuous", spread_radius = input$spread_radius_1D,
                         cont_level = c(input$cont_level_mu_1D, input$cont_level_sd_1D), method_sp = input$method_sp_1D,
                         n_sp = input$n_sp_1D, n_strata = n_strata, by = input$by_1D, LOC = input$LOC_1D,
                         fun = input$fun_1D, case = input$case_1D, m = input$m_1D, M = input$M_1D, m_sp = input$m_sp_1D,
                         method_det = input$method_det_1D, bg_level = input$bg_level_1D, geom = input$geom_1D)
  return(ArgList_default)
}

