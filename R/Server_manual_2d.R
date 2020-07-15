# Tuning for the manual mode
output$ui_tuning = renderUI(expr = {
  if(input$n_vars == 0){
    NULL
    
  } else if(input$n_vars == 1){
    verticalLayout(
      selectInput(inputId = "var_prim",
                  label = "Primary tuning parameter",
                  choices = f_var_tuning_2d(geom = input$geom)),
      textInput(inputId = "val_prim", label = "Tuning value(s) (separated by a comma)", value = "1,2,3")
    )
    
  } else if(input$n_vars == 2) {
    verticalLayout(
      selectInput(inputId = "var_prim",
                  label = "Primary tuning parameter",
                  choices = f_var_tuning_2d(geom = input$geom)),
      textInput(inputId = "val_prim", label = "Tuning value(s)", value = "1,2,3"),
      selectInput(inputId = "var_sec",
                  label = "Secondary tuning parameter",
                  choices = list("Sampling strategy" = "method_sp")),
      textInput(inputId = "val_sec", label = "Tuning value(s) (separated by a comma)", value = "srs, strs, ss")
    )
  } else {
    stop("Unknown number of tuning paramters")
  }
})

# Load the parameter once (manual version)
list_load = list()
observeEvent(eventExpr = {input$load}, handlerExpr = {
  
  # Load the parameters once  
  list_load <<- load_once(input = input, output = output)
  
  # The P_det/Paccept visualization switch (only for 2 tuning parameters)
  output$yvar = renderUI(expr = {f_yvar(input = input, chosen_mode = list_load$chosen_mode)})
  
  # The chosen parameters table
  output$print_param = renderTable(expr = make_var_table(Args = list_load$ArgList_default, 
                                                         input = input, 
                                                         chosen_mode = list_load$chosen_mode))
}, ignoreInit = TRUE, ignoreNULL = TRUE)

# Visualize for one iteration (manual mode)
observeEvent(eventExpr = {input$vis}, handlerExpr = {
  
  vis_once(input = input, output = output, 
           ArgList = list_load$ArgList_default, chosen_mode = list_load$chosen_mode)
  
}, ignoreInit = TRUE, ignoreNULL = TRUE)

# Multiple iterations (manual mode)
observeEvent(eventExpr = {input$iterate}, handlerExpr = {
  
  # Create a progress message
  showModal(ui = modalDialog("Iteration in progress", size = "s"))
  
  result_iter <<- f_iterate_tune(input = input, output = output, 
                                  Args = list_load$ArgList_default, 
                                  chosen_mode = list_load$chosen_mode)
  
  vis_n(data = result_iter, input = input, output = output, chosen_mode = list_load$chosen_mode)
  
  # Close the message
  removeModal()
  
}, ignoreInit = TRUE, ignoreNULL = TRUE)

# Warning: n_sp and n_strata
observeEvent(eventExpr = {input$method_sp}, handlerExpr = {
  if(input$method_sp == "strs"){
    observeEvent(eventExpr = {c(input$n_strata, input$by, input$n_strata_row, input$n_strata_col)}, handlerExpr = {
      
      if(input$by == "2d"){
        req(input$n_strata_row, input$n_strata_col)
      } else {
        req(input$n_strata)
      }
      
      warning_n_sp_n_strata(spread = "continuous", v_smart = FALSE, input = input)
      
    })
  }
})

# Associate case with n_sp
observeEvent(eventExpr = {input$case}, handlerExpr = {
  
  # Match the n_sp with case
  n_sp_update = case_sp_lookup(case = input$case)
  
  # Update n_sp_vs
  updateNumericInput(session = session, inputId = "n_sp", value = n_sp_update)
})

# Give a warning when n_sp is not consistent with case
observeEvent(eventExpr = {input$n_sp}, handlerExpr = {
  make_modal_n_sp_case(n_sp = input$n_sp, case = input$case)
})
