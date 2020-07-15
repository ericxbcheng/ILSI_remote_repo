
observeEvent(eventExpr = {input$spread_vs}, handlerExpr = {
  if(input$spread_vs == "discrete"){
    
    # Dimensions
    output$ui_dims_3d = renderUI(expr = {f_ui_dims(input = input)})
    
    # Grains
    output$ui_grain_3d = renderUI(expr = {f_ui_grain(input = input)})
    
    # Contamination
    output$ui_contam_3d = renderUI(expr = {f_ui_contam(input = input)})
    
    # Generate the conc_neg vector, which represents concentrations of healthy kernels.
    observeEvent(eventExpr = {input$Mc_vs}, handlerExpr = {
      conc_neg_vs <<- rpert(n = 10^6, min = 0, mode = 0.7, max = input$Mc_vs - 0.01, shape = 80)
    })
    
  } else {
    NULL
  }
}, ignoreNULL = TRUE, ignoreInit = TRUE)

# n_affected
observeEvent(eventExpr = {input$dis_level_type_vs}, handlerExpr = {
  
  # Create the n_affected section
  output$ui_n_affected_3d = renderUI(expr = {f_ui_n_affected(input = input)})
  
  # Update the maximum boundary of n_affected
  observeEvent(eventExpr = {input$c_hat_vs}, handlerExpr = {
    
    if(!anyNA(c(input$c_hat_vs, input$x_lim_3d_vs, input$y_lim_3d_vs,
                input$z_lim_3d_vs, input$rho_vs, input$m_kbar_vs))){
      
      lims = list(xlim = c(0, input$x_lim_3d_vs), ylim = c(0, input$y_lim_3d_vs), zlim = c(0, input$z_lim_3d_vs))
      dis_level = make_dis_level_gui(input = input, chosen_mode = "v_smart")
      
      # n_contam_total = n_spot + n_spot * n_affected
      n_contam_total = calc_n_contam(c_hat = input$c_hat_vs, lims = lims,
                                     rho = input$rho_vs, m_kbar = input$m_kbar_vs,
                                     dis_level = dis_level, conc_neg = conc_neg_vs)
      n_affected_max = max(0, n_contam_total - 1)
      
      # Update the n_affected_vs according to the new c_hat_vs
      updateNumericInput(session = session, inputId = "n_affected_vs", max = n_affected_max)
      
    } else {
      NULL
    }
  })
}, ignoreNULL = TRUE, ignoreInit = TRUE)

observeEvent(eventExpr = {input$n_affected_vs}, handlerExpr = {
  
  if(!is.na(input$n_affected_vs)){
    # Probe diameter
    output$ui_sp_3d = renderUI(expr = {f_ui_sp(input = input)})
    
  } else {
    NULL
  }
})

observeEvent(eventExpr = {input$method_sp_3d_vs}, handlerExpr = {
  
  # Sampling strategy
  output$ui_method_sp_3d = renderUI(expr = {f_ui_method_sp(input = input)})
  
  # Grinding and assay
  output$ui_assay_3d = renderUI(expr = {f_ui_assay(input = input)})
  
})

observeEvent(eventExpr = {input$method_det_3d_vs}, handlerExpr = {
  
  # Iteration
  output$ui_iter_3d = renderUI(expr = {f_ui_iter(input = input)})

})

observeEvent(eventExpr = {input$n_vars_3d_vs}, handlerExpr = {
  
  # Tuning UI
  output$ui_tuning_3d_vs = renderUI(expr = {f_ui_tuning_vs(input = input)})
  
  # UI for loading (smart version)
  output$ui_load_3d = renderUI(expr = {
    verticalLayout(
      actionButton(inputId = "load_3d_vs", label = "Load parameters"),
      h2()
    )
  })
}, ignoreNULL = TRUE, ignoreInit = TRUE)

# UI for visualization and iteration (smart version)
observeEvent(eventExpr = {input$load_3d_vs}, handlerExpr = {
  output$ui_vis_iter_3d = renderUI(expr = {
    verticalLayout(
      splitLayout(
        actionButton(inputId = "vis_3d_vs", label = "Visualize"),
        actionButton(inputId = "iterate_3d_vs", label = "Iterate")
      )
    )
  })
}, ignoreInit = TRUE, ignoreNULL = TRUE)

# Load the parameter once (smart version 3D)
list_load = list()
observeEvent(eventExpr = {input$load_3d_vs}, handlerExpr = {
  
  # Load the parameters once  
  list_load <<- load_once(input = input, output = output, conc_neg = conc_neg_vs)
  
  # The chosen parameters table
  output$print_param = renderTable(expr = make_var_table(Args = list_load$ArgList_default,
                                                         input = input,
                                                         chosen_mode = list_load$chosen_mode))
}, ignoreInit = TRUE, ignoreNULL = TRUE)

# Visualize for one iteration (smart mode)
observeEvent(eventExpr = {input$vis_3d_vs}, handlerExpr = {
  
  # Create visualization for one iteration
  vis_once(input = input, output = output,
           ArgList = list_load$ArgList_default, chosen_mode = list_load$chosen_mode)
  
  # Project these visualizations to ui_vis_once
  output$ui_vis_once = renderUI(expr = {
    verticalLayout(
      plotOutput(outputId = "overlay_top_vs"),
      plotOutput(outputId = "overlay_side_vs")
    )
  })
  
}, ignoreInit = TRUE, ignoreNULL = TRUE)

# Multiple iterations (smart mode)
observeEvent(eventExpr = {input$iterate_3d_vs}, handlerExpr = {
  
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

# Show a message when tuning over n_affected or method_sp
observeEvent(eventExpr = {input$n_vars_3d_vs}, handlerExpr = {
  
  if(input$n_vars_3d_vs == 1){
    
    observeEvent(eventExpr = {input$var_prim_3d_vs}, handlerExpr = {
      if(input$var_prim_3d_vs == "n_affected"){
        if(input$n_affected_vs == 0){
          showModal(ui = modalDialog("Please go back to Q9 and set it to > 0. For example, set it to 1."))
        }
      }
    })
    
  } else if (input$n_vars_3d_vs == 2){
    
    observeEvent(eventExpr = {input$var_sec_3d_vs}, handlerExpr = {
      if(input$var_sec_3d_vs == "n_affected"){
        if(input$n_affected_vs == 0){
          showModal(ui = modalDialog("Please go back to Q9 and set it to > 0. For example, set it to 1."))
        }
        
      } else if (input$var_sec_3d_vs == "method_sp"){
        if(input$method_sp_3d_vs == "ss"){
          showModal(ui = modalDialog("Please go back to Q11 and set it to 'simple random sampling' or 'stratified random sampling' so that you can set the number of samples for tuning purposes."))
        }
        
      } else {
        NULL
      }
    })
    
  } else {
    NULL
  }
}, ignoreNULL = TRUE, ignoreInit = TRUE)

# Warning: n_sp and n_strata
observeEvent(eventExpr = {input$method_sp_3d_vs}, handlerExpr = {
  if(input$method_sp_3d_vs == "strs"){
    observeEvent(eventExpr = {c(input$n_strata_3d_vs, input$by_3d_vs, input$n_strata_row_3d_vs, input$n_strata_col_3d_vs)}, handlerExpr = {
      
      if(input$by_3d_vs == "2d"){
        req(input$n_strata_row_3d_vs, input$n_strata_col_3d_vs)
      } else {
        req(input$n_strata_3d_vs)
      }
      
      warning_n_sp_n_strata(spread = "discrete", v_smart = TRUE, input = input)
      
    })
  }
})

source(file="R/Documentation_Smart_3D.R", local=TRUE)