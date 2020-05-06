
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
      dis_level = make_dis_level(input = input, chosen_mode = "v_smart")
      
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
  
})
