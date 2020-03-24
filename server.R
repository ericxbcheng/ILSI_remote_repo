#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinydashboard)

source(file = "Sampling_libraries.R")
source(file = "Sampling_contamination.R")
source(file = "Sampling_contamination_3d.R")
source(file = "Sampling_visualization.R")
source(file = "Sampling_assay_prep.R")
source(file = "Sampling_plan.R")
source(file = "Sampling_plan_3d.R")
source(file = "Sampling_assay_3d.R")
source(file = "Sampling_assay.R")
source(file = "Sampling_outcome_3d.R")
source(file = "Sampling_outcome.R")
source(file = "Sampling_iteration.R")
source(file = "Sampling_tuning_3d.R")
source(file = "Sampling_analysis.R")
source(file = "R/Sampling_shiny_helpers.R")

shinyServer(function(input, output, session) {
  
  # Tuning conditional panels
  output$ui_tuning = renderUI(expr = {
    if(input$n_vars == 0){
      NULL
      
    } else if(input$n_vars == 1){
      verticalLayout(
        selectInput(inputId = "var_prim",
                    label = "Primary tuning parameter",
                    choices = list("Number of contamination points" = "n_contam",
                                   "Number of sample points" = "n_sp",
                                   "Individual sample mass (g)" = "m_sp")),
        textInput(inputId = "val_prim", label = "Tuning value(s) (separated by a comma)", value = "1,2,3")
      )
      
    } else if(input$n_vars == 2) {
      verticalLayout(
        selectInput(inputId = "var_prim",
                    label = "Primary tuning parameter",
                    choices = list("Number of contamination points" = "n_contam",
                                   "Number of sample points" = "n_sp",
                                   "Individual sample mass (g)" = "m_sp")),
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
  
  # The P_det/Paccept visualization switch (only for 2 tuning parameters)
  output$yvar = renderUI(expr = {
    
    if(input$n_vars == 2){
      radioButtons(inputId = "yvar", 
                   label = NULL, 
                   choices = list("Detection probability" = "P_det", 
                                  "Acceptance probability" = "Paccept"),
                   inline = TRUE)
    }
  })

  # Smart version

  
  
  
  # Load the parameter once
  list_load = list()
  observeEvent(eventExpr = {input$load}, handlerExpr = {
    
    list_load <<- load_once(input = input, output = output)
    output$print_param = renderTable(expr = make_var_table(Args = list_load$ArgList_default, input = input))

  })
  
  # Visualize for one iteration
  observeEvent(eventExpr = {input$vis}, handlerExpr = {
    
    vis_once(input = input, output = output, spread = list_load$spread, ArgList = list_load$ArgList_default)
    
  })
  
  # Multiple iterations
  observeEvent(eventExpr = {input$iteration}, handlerExpr = {
    
    if(input$n_vars == 0){
      
      # When there is no tuning parameter
      data_raw = iterate_tune0(input = input, Args = list_load$ArgList_default)
      data_cleaned <<- metrics_cont_0(data = data_raw)
      output$plot_iterate = renderPlot(expr = {plot_tune0(data = data_cleaned)})
      
    } else if (input$n_vars == 1){
      
      # When there is 1 tuning parameter
      data_raw = iterate_tune1(input = input, Args = list_load$ArgList_default)
      data_cleaned <<- metrics_cont_n(data = data_raw)
      output$plot_iterate = renderPlot(expr = {plot_tune1(data = data_cleaned, input = input)})
      
    } else if (input$n_vars == 2) {
      
      data_raw = iterate_tune2(input = input, Args = list_load$ArgList_default)
      data_cleaned <<- metrics_cont_sec(data = data_raw[["sim_data"]], 
                                        input = input, 
                                        vals_prim = data_raw[["vals_prim"]], 
                                        vals_sec = data_raw[["vals_sec"]])
      
      observeEvent(eventExpr = {input$yvar}, handlerExpr = {
        output$plot_iterate = renderPlot(expr = {plot_tune2_boxplot(data = data_cleaned, input = input, yvar = input$yvar)})
      })
      
    } else {
      message("Under construction")
    }
  })
  
  # Download
  output$downloadData = downloadHandler(
    filename = "simulation.csv",
    content = function(file){
      write.csv(x = data_cleaned, file)
    },
    contentType = "text/csv"
  )
})