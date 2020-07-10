output$ui_tuning_3d = renderUI(expr = {
  if(input$n_vars_3d == 0){
    NULL
    
  } else if(input$n_vars_3d == 1){
    verticalLayout(
      selectInput(inputId = "var_prim_3d",
                  label = "Primary tuning parameter",
                  choices = f_var_tuning_3d(method_sp = input$method_sp_3d, n_vars = 1)),
      textInput(inputId = "val_prim_3d", label = "Tuning value(s) (separated by a comma)", value = "5,10,20")
    )
    
  } else if(input$n_vars_3d == 2) {
    verticalLayout(
      selectInput(inputId = "var_prim_3d",
                  label = "Primary tuning parameter",
                  choices = list("Overall mycotoxin level (ppb)" = "c_hat")),
      textInput(inputId = "val_prim_3d", label = "Tuning value(s)", value = "5,10,20"),
      selectInput(inputId = "var_sec_3d",
                  label = "Secondary tuning parameter",
                  choices = f_var_tuning_3d(method_sp = input$method_sp_3d, n_vars = 2)),
      textInput(inputId = "val_sec_3d", label = "Tuning value(s) (separated by a comma)", value = "5, 10, 100")
    )
  } else {
    stop("Unknown number of tuning paramters")
  }
})

# Generate the conc_neg vector, which represents concentrations of healthy kernels.
observeEvent(eventExpr = {input$Mc}, handlerExpr = {
  conc_neg <<- rpert(n = 10^6, min = 0, mode = 0.7, max = input$Mc - 0.01, shape = 80)
})

# Update the maximum boundary of n_affected
observeEvent(eventExpr = {input$c_hat}, handlerExpr = {
  
  lims = list(xlim = c(0, input$x_lim_3d), ylim = c(0, input$y_lim_3d), zlim = c(0, input$z_lim_3d))
  dis_level = make_dis_level_gui(input = input, chosen_mode = "3D")
  
  # n_contam_total = n_spot + n_spot * n_affected
  n_contam_total = calc_n_contam(c_hat = input$c_hat, lims = lims, rho = input$rho, m_kbar = input$m_kbar, dis_level = dis_level, conc_neg = conc_neg)
  n_affected_max = max(0, n_contam_total - 1)
  
  updateNumericInput(session = session, inputId = "n_affected", max = n_affected_max)
})

# Load the parameter once (manual version 3D)
list_load = list()
observeEvent(eventExpr = {input$load_3d}, handlerExpr = {
  
  # Load the parameters once  
  list_load <<- load_once(input = input, output = output, conc_neg = conc_neg)

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
  
  # Create a progress message
  showModal(ui = modalDialog("Iteration in progress", size = "s"))
  
  # Tune the model
  result_iter <<- f_iterate_tune(input = input, output = output,
                                  Args = list_load$ArgList_default,
                                  chosen_mode = list_load$chosen_mode)
  
  # Visualize the tuning results
  vis_n(data = result_iter, input = input, output = output, chosen_mode = list_load$chosen_mode)
  
  # Close the message
  removeModal()
  
}, ignoreInit = TRUE, ignoreNULL = TRUE)

# Show a message when tuning over n_affected
observeEvent(eventExpr = {input$n_vars_3d}, handlerExpr = {
  
  if(input$n_vars_3d == 1){
    
    observeEvent(eventExpr = {input$var_prim_3d}, handlerExpr = {
      if(input$var_prim_3d == "n_affected"){
        if(input$n_affected == 0){
          showModal(ui = modalDialog("Please go back and set 'number of grains in a cluster' to > 0. For example, set it to 1."))
        }
      }
    })
    
  } else if (input$n_vars_3d == 2){
    
    observeEvent(eventExpr = {input$var_sec_3d}, handlerExpr = {
      if(input$var_sec_3d == "n_affected"){
        if(input$n_affected == 0){
          showModal(ui = modalDialog("Please go back and set 'number of grains in a cluster' to > 0. For example, set it to 1."))
        }
      }
    })
    
  } else {
    NULL
  }
}, ignoreNULL = TRUE, ignoreInit = TRUE)

# Warning: n_sp and n_strata
observeEvent(eventExpr = {input$method_sp_3d}, handlerExpr = {
  if(input$method_sp_3d == "strs"){
    observeEvent(eventExpr = {c(input$n_strata_3d, input$by_3d, input$n_strata_row_3d, input$n_strata_col_3d)}, handlerExpr = {
      
      if(input$by_3d == "2d"){
        req(input$n_strata_row_3d, input$n_strata_col_3d)
      } else {
        req(input$n_strata_3d)
      }
      
      warning_n_sp_n_strata(spread = "discrete", v_smart = FALSE, input = input)
      
    })
  }
})
