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


shinyServer(function(input, output) {
  
  list_load = list()
  
  # Smart version
  observeEvent(eventExpr = {input$spread}, handlerExpr = {
    output$questionnaire = renderUI(expr = f_questionnaire(input = input))
  })
  
  # Load the parameter once
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
      output$plot_iterate = renderPlot(expr = {plot_tune2(data = data_cleaned, input = input)})
      
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