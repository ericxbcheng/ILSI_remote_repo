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