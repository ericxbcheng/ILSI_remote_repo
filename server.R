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
library(shinyjs)

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
source(file = "R/Sampling_shiny_loading.R")
source(file = "R/Sampling_shiny_tuning.R")
source(file = "R/Sampling_shiny_visualization.R")
source(file = "R/Sampling_shiny_contam.R")
source(file = "R/Sampling_shiny_plan.R")
source(file = "R/Sampling_shiny_assay.R")
source(file = "R/Sampling_shiny_iteration.R")

shinyServer(function(input, output, session) {
  
  # Tuning for the manual mode
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

  ##### Smart version
  # Dimensions
  observeEvent(eventExpr = {input$spread_vs}, handlerExpr = {
    output$ui_dims = renderUI(expr = {f_ui_dims(input = input)})
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
    n_sp_update = case_sp_lookup(input = input)
    
    # Update n_sp_vs
    updateNumericInput(session = session, inputId = "n_sp_vs", value = n_sp_update)
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
  
  # Load the parameter once (manual version)
  list_load = list()
  observeEvent(eventExpr = {input$load}, handlerExpr = {

      list_load <<- load_once(input = input, output = output)
      output$print_param = renderTable(expr = make_var_table(Args = list_load$ArgList_default, 
                                                             input = input, 
                                                             chosen_mode = list_load$chosen_mode))
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Load the parameter once (smart version)
  observeEvent(eventExpr = {input$load_vs}, handlerExpr = {

      list_load <<- load_once(input = input, output = output)
      output$print_param = renderTable(expr = make_var_table(Args = list_load$ArgList_default,
                                                             input = input,
                                                             chosen_mode = list_load$chosen_mode))
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Visualize for one iteration (manual mode)
  observeEvent(eventExpr = {input$vis}, handlerExpr = {

      vis_once(input = input, output = output, 
               ArgList = list_load$ArgList_default, chosen_mode = list_load$chosen_mode)

  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Visualize for one iteration (smart mode)
  observeEvent(eventExpr = {input$vis_vs}, handlerExpr = {
    
    vis_once(input = input, output = output, 
             ArgList = list_load$ArgList_default, chosen_mode = list_load$chosen_mode)

  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Multiple iterations (manual mode)
  observeEvent(eventExpr = {input$iterate}, handlerExpr = {
    
    f_event_iteration_2d(input = input, output = output, 
                         Args = list_load$ArgList_default, chosen_mode = list_load$chosen_mode)
    
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Multiple iterations (smart mode)
  observeEvent(eventExpr = {input$iterate_vs}, handlerExpr = {
    
    f_event_iteration_2d(input = input, output = output, 
                         Args = list_load$ArgList_default, chosen_mode = list_load$chosen_mode)
    
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Download
  output$downloadData = downloadHandler(
    filename = "simulation.csv",
    content = function(file){
      write.csv(x = data_cleaned, file)
    },
    contentType = "text/csv"
  )
})