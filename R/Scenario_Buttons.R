#3D Scenario
#Size of the bin
  observeEvent(input$ThreeD_Scenario, {
    #3D bin size
    updateNumericInput(session,"x_lim_3d",value = 3)
    updateNumericInput(session,"y_lim_3d",value = 2)
    updateNumericInput(session,"z_lim_3d",value = 2)
    #mycotoxin type
    updateSelectInput(session,"tox",selected ="AF")
    #overall mycotoxin level
    updateNumericInput(session,"c_hat",value=200)
    #mycotoxin distribution
    updateSelectInput(session,"dis_level_type",selected = "Gamma")
    #Mode and mycotoxin levels
    updateNumericInput(session,"dis_level_const_arg", value = 4500)
    updateNumericInput(session,"dis_level_gm_mode", value = 45000)
    updateNumericInput(session,"dis_level_gm_lb", value = 25)
    #number of grains in cluster
    updateNumericInput(session,"n_affected", value = 1)
    #Conditional if more than 1 cluster
    updateNumericInput(session,"vcov_11", value = .0005)
    updateNumericInput(session,"vcov_12", value = 0.1)
    updateNumericInput(session,"vcov_13", value = 0.1)
    updateNumericInput(session,"vcov_22", value = 0.0005)
    updateNumericInput(session,"vcov_23", value = 0)
    updateNumericInput(session,"vcov_33", value = 0.0005)
    #Sampling Strategy
    updateSelectInput(session,"method_sp_3d",selected = "srs")
    #well panel
    updateNumericInput(session,"n_sp_3d",value= 6 )
    updateSelectInput(session,"by_3d", selected = "row" )
    updateNumericInput(session,"n_strata_row_3d",value= 0 )
    updateNumericInput(session,"n_strata_col_3d",value= 0 )
    updateSelectInput(session,"container",selected = "hopper")
    updateSelectInput(session,"compartment",selected = 2)
    updateSelectInput(session,"type",selected = "open_top")
    #probe diameter
    updateNumericInput(session,"d", value= 0.05)
    #mass kernell
    updateNumericInput(session,"m_kbar", value= 0.2)
    updateNumericInput(session,"rho", value= 1.28)
    #homogeneity
    updateSliderInput(session,"homogeneity", value = 0.5)
    #assay
    updateSelectInput(session, "method_det_3d", selected = "ELISA aflatoxin")
    updateNumericInput(session,"Mc",value = 22 )
    #iteration section
    updateNumericInput(session,"n_seed_3d", value= 1 )
    updateNumericInput(session,"n_iter_3d", value= 2 )
    #tuning parameters
    updateSelectInput(session, "n_vars_3d", selected = 2)
    
  })
  
