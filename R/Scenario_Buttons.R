#3D Scenario
#Size of the bin
  observeEvent(input$ThreeD_Scenario, {
    #3D bin size
    updateNumericInput(session,"x_lim_3d",value = 1)
    updateNumericInput(session,"y_lim_3d",value = 1)
    updateNumericInput(session,"z_lim_3d",value = 1)
    #mycotoxin type
    updateSelectInput(session,"tox",selected ="AF")
    #overall mycotoxin level
    updateNumericInput(session,"c_hat",value=1)
    #mycotoxin distribution
    updateSelectInput(session,"dis_level_type",selected = "Gamma")
    #Mode and mycotoxin levels
    updateNumericInput(session,"dis_level_const_arg", value = 40000)
    updateNumericInput(session,"dis_level_gm_mode", value = 40000)
    updateNumericInput(session,"dis_level_gm_lb", value = 20)
    #number of grains in cluster
    updateNumericInput(session,"n_affected", value = 1)
    #Conditional if more than 1 cluster
    updateNumericInput(session,"vcov_11", value = 0.0004)
    updateNumericInput(session,"vcov_12", value = 0)
    updateNumericInput(session,"vcov_13", value = 0)
    updateNumericInput(session,"vcov_22", value = 0.0004)
    updateNumericInput(session,"vcov_23", value = 0)
    updateNumericInput(session,"vcov_33", value = 0.0004)
    #Sampling Strategy
    updateSelectInput(session,"method_sp_3d",selected = "srs")
    #well panel
    updateNumericInput(session,"n_sp_3d",value= 5 )
    updateSelectInput(session,"by_3d", selected = "row" )
    updateNumericInput(session,"n_strata_row_3d",value= 0 )
    updateNumericInput(session,"n_strata_col_3d",value= 0 )
    updateSelectInput(session,"container",selected = "hopper")
    updateSelectInput(session,"compartment",selected = 2)
    updateSelectInput(session,"type",selected = "open_top")
    #probe diameter
    updateNumericInput(session,"d", value= 0.04)
    #mass kernell
    updateNumericInput(session,"m_kbar", value= 0.3)
    updateNumericInput(session,"rho", value= 1.28)
    #homogeneity
    updateSliderInput(session,"homogeneity", value = 0.5)
    #assay
    updateSelectInput(session, "method_det_3d", selected = "ELISA aflatoxin")
    updateNumericInput(session,"Mc",value = 20 )
    #iteration section
    updateNumericInput(session,"n_seed_3d", value= 1 )
    updateNumericInput(session,"n_iter_3d", value= 1 )
    #tuning parameters
    updateSelectInput(session, "n_vars_3d", selected = 2)
    #selecting Input, missing the tunning options, depends on the scenario
    updateSelectInput(session, "var_prim_3d", selected = "c_hat")
    updateTextInput(session, "val_prim_3d", value = "5,10,20")
    #Second tunning parameter
    updateSelectInput(session,"var_sec_3d", selected = "n_affected")
    updateTextInput(session, "val_sec_3d", value = "5,10,20")
    
  })
  
  #Modal Shows updated
  observeEvent(input$ThreeD_Scenario, {
    showModal(modalDialog(
      title = "Your Scenario has been updated",
      easyClose = TRUE
      ,size = "s"
    ))
  })
  
#2D Scenario

  observeEvent(input$TwoD_Scenario, { 
    #Field Size
    updateNumericInput(session,"x_lim",value = 100)
    updateNumericInput(session,"y_lim",value = 23.3)
    #geometry 
    updateSelectInput(session,"geom", selected = "point"  )
    #updating point
    updateNumericInput(session, "n_contam", value = 1)
    updateNumericInput(session, "spread_radius", value = 1)
    #contamination level
    updateNumericInput(session,"cont_level_mu", value =2 )
    updateNumericInput(session,"cont_level_sd", value =.5 )
    #background level
    updateNumericInput(session,"bg_level", value =.00001 )
    #decay function
    updateSelectInput(session,"fun",selected= "unif" )
    #conditionl decay
    updateNumericInput(session, "LOC", value = .001)
    #number of sampling points
    updateNumericInput(session, "n_sp", value = 5 )
    #sampling strategy
    updateSelectInput(session, "method_sp", selected = "srs")
    #well pannel
    updateSelectInput(session, "by", selected = "row")
    updateNumericInput(session,"n_strata", value =5 )
    updateNumericInput(session,"n_strata_row", value =2 )
    updateNumericInput(session,"n_strata_col", value =1 )
    #slider case
    updateSliderInput(session, "case", value = 9)
    #individual sample mass
    updateNumericInput(session,"m_sp", value = 25 )
    updateSelectInput(session, "method_det", selected = "enrichment")
    #iteration section
    updateNumericInput(session, "n_seed", value =1)
    updateNumericInput(session, "n_iter", value =1)
    #missing tunning
    updateSelectInput(session, "n_vars", selected = 2)
    updateSelectInput(session,"var_prim", selected = "n_contam")
    updateTextInput(session,"val_prim", value = "1,2,3")
    updateSelectInput(session,"var_sec", selected = "method_sp" ) 
    updateTextInput(session, "val_sec", value = "srs, strs, ss")
    
  })
  
  #Modal Shows updated
  observeEvent(input$TwoD_Scenario, {
    showModal(modalDialog(
      title = "Your Scenario has been updated",
      easyClose = TRUE
      ,size = "s"
    ))
  })
  