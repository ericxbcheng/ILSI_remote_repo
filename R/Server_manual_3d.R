output$ui_tuning_3d = renderUI(expr = {
  if(input$n_vars_3d == 0){
    NULL
    
  } else if(input$n_vars_3d == 1){
    verticalLayout(
      selectInput(inputId = "var_prim_3d",
                  label = "Primary tuning parameter",
                  choices = list("Overall mycotoxin level (ppb)" = "c_hat",
                                 "Number of probes" = "n_sp",
                                 "Number of grains in a cluster" = "n_affected")),
      textInput(inputId = "val_prim_3d", label = "Tuning value(s) (separated by a comma)", value = "10,20,30")
    )
    
  } else if(input$n_vars_3d == 2) {
    verticalLayout(
      selectInput(inputId = "var_prim_3d",
                  label = "Primary tuning parameter",
                  choices = list("Overall mycotoxin level (ppb)" = "c_hat",
                                 "Number of probes" = "n_sp",
                                 "Number of grains in a cluster" = "n_affected")),
      textInput(inputId = "val_prim_3d", label = "Tuning value(s)", value = "10,20,30"),
      selectInput(inputId = "var_sec_3d",
                  label = "Secondary tuning parameter",
                  choices = list("Sampling strategy" = "method_sp")),
      textInput(inputId = "val_sec_3d", label = "Tuning value(s) (separated by a comma)", value = "srs, strs, ss")
    )
  } else {
    stop("Unknown number of tuning paramters")
  }
})

# Generate the conc_neg vector, which represents concentrations of healthy kernels.
observeEvent(eventExpr = {input$Mc}, handlerExpr = {
  conc_neg <<- rpert(n = 10^6, min = 0, mode = 0.7, max = input$Mc - 0.01, shape = 80)
})

# Load the parameter once (manual version 3D)
list_load = list()
observeEvent(eventExpr = {input$load_3d}, handlerExpr = {
  
  # Load the parameters once  
  list_load <<- load_once(input = input, output = output, conc_neg = conc_neg)
  
  # # The P_det/Paccept visualization switch (only for 2 tuning parameters)
  # output$yvar = renderUI(expr = {f_yvar(input = input, chosen_mode = list_load$chosen_mode)})

  # The chosen parameters table
  output$print_param = renderTable(expr = make_var_table(Args = list_load$ArgList_default,
                                                         input = input,
                                                         chosen_mode = list_load$chosen_mode))
}, ignoreInit = TRUE, ignoreNULL = TRUE)

# Visualize for one iteration (manual mode)
observeEvent(eventExpr = {input$vis_3d}, handlerExpr = {
  
  vis_once(input = input, output = output,
           ArgList = list_load$ArgList_default, chosen_mode = list_load$chosen_mode)
  
}, ignoreInit = TRUE, ignoreNULL = TRUE)

# Multiple iterations (manual mode)
observeEvent(eventExpr = {input$iterate_3d}, handlerExpr = {
  
  # result_iter = f_iterate_tune_2d(input = input, output = output, 
  #                                 Args = list_load$ArgList_default, 
  #                                 chosen_mode = list_load$chosen_mode)
  # 
  # vis_n(data = result_iter, input = input, output = output, chosen_mode = list_load$chosen_mode)
  
}, ignoreInit = TRUE, ignoreNULL = TRUE)