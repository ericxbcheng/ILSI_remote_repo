# Dimensions
observeEvent(eventExpr = {input$spread_vs}, handlerExpr = {
  if(input$spread_vs == "continuous"){
    output$ui_dims = renderUI(expr = {f_ui_dims(input = input)})
  } else {
    NULL
  }
})

# Geom + contamination + sampling
observeEvent(eventExpr = {input$geom_vs}, handlerExpr = {
  
  # Geom
  output$ui_geom = renderUI(expr = {f_ui_geom(input = input)})
  
  # Contamination
  output$ui_contam = renderUI(expr = {f_ui_contam(input = input)})
  
  # Sampling
  output$ui_sp = renderUI(expr = {f_ui_sp(input = input)})
  
})

# Associate case with n_sp
observeEvent(eventExpr = {input$case_vs}, handlerExpr = {
  
  # Match the n_sp with case
  n_sp_update = case_sp_lookup(case = input$case_vs)
  
  # Update n_sp_vs
  updateNumericInput(session = session, inputId = "n_sp_vs", value = n_sp_update)
})

# Give a warning when n_sp is not consistent with case
observeEvent(eventExpr = {input$n_sp_vs}, handlerExpr = {
  make_modal_n_sp_case(n_sp = input$n_sp_vs, case = input$case_vs)
})


# Sampling strategy
observeEvent(eventExpr = {input$method_sp_vs}, handlerExpr = {
  
  # Sampling strategy
  output$ui_method_sp = renderUI(expr = {f_ui_method_sp(input = input)})
  
  # Assay
  output$ui_assay = renderUI(expr = {f_ui_assay(input = input)})
})

# Iteration
observeEvent(eventExpr = {input$method_det_vs}, handlerExpr = {
  output$ui_iter = renderUI(expr = {f_ui_iter(input = input)})
})

# tuning and loading
observeEvent(eventExpr = {input$n_vars_vs}, handlerExpr = {
  
  # Tuning and buttons for the smart mode
  output$ui_tuning_vs = renderUI(expr = {f_ui_tuning_vs(input = input)})
  
  # UI for loading (smart version)
  output$ui_load = renderUI(expr = {
    verticalLayout(
      actionButton(inputId = "load_vs", label = "Load parameters"),
      h2()
    )
  })
}, ignoreInit = TRUE, ignoreNULL = TRUE)

# UI for visualization and iteration (smart version)
observeEvent(eventExpr = {input$load_vs}, handlerExpr = {
  output$ui_vis_iter = renderUI(expr = {
    verticalLayout(
      splitLayout(
        actionButton(inputId = "vis_vs", label = "Visualize"),
        actionButton(inputId = "iterate_vs", label = "Iterate")
      )
    )
  })
}, ignoreInit = TRUE, ignoreNULL = TRUE)

# Load the parameter once (smart version)
observeEvent(eventExpr = {input$load_vs}, handlerExpr = {
  
  # Load the parameters once
  list_load <<- load_once(input = input, output = output)
  
  # The P_det/Paccept visualization switch (only for 2 tuning parameters)
  output$yvar = renderUI(expr = {f_yvar(input = input, chosen_mode = list_load$chosen_mode)})
  
  # The chosen parameters table
  output$print_param = renderTable(expr = make_var_table(Args = list_load$ArgList_default,
                                                         input = input,
                                                         chosen_mode = list_load$chosen_mode))
}, ignoreInit = TRUE, ignoreNULL = TRUE)

# Visualize for one iteration (smart mode)
observeEvent(eventExpr = {input$vis_vs}, handlerExpr = {
  
  # Create plots for visualization for one iteration
  vis_once(input = input, output = output, 
           ArgList = list_load$ArgList_default, chosen_mode = list_load$chosen_mode)
  
  # Project these visualizations to ui_vis_once
  output$ui_vis_once = renderUI(expr = {
    verticalLayout(
      plotOutput(outputId = "overlay_draw_vs"),
      plotOutput(outputId = "contam_level_draw_vs")
    )
  })
  
}, ignoreInit = TRUE, ignoreNULL = TRUE)

# Multiple iterations (smart mode)
observeEvent(eventExpr = {input$iterate_vs}, handlerExpr = {
  
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
observeEvent(eventExpr = {input$method_sp_vs}, handlerExpr = {
  if(input$method_sp_vs == "strs"){
    observeEvent(eventExpr = {c(input$n_strata_vs, input$by_vs, input$n_strata_row_vs, input$n_strata_col_vs)}, handlerExpr = {
      
      if(input$by_vs == "2d"){
        req(input$n_strata_row_vs, input$n_strata_col_vs)
      } else {
        req(input$n_strata_vs)
      }
      
      warning_n_sp_n_strata(spread = "continuous", v_smart = TRUE, input = input)
      
    })
  }
})

source(file="R/Documentation_Smart_2D.R", local=TRUE)