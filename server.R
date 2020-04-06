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
source(file = "R/Sampling_shiny_helpers.R")

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
    output$ui_dims = renderUI(expr = {
      
      if(input$spread_vs == "continuous"){
        verticalLayout(
          p("Q2. What are the dimensions of the field?"),
          splitLayout(
            numericInput(inputId = "x_lim_vs", label = "Length (m)", value = NULL, min = 1),
            numericInput(inputId = "y_lim_vs", label = "Width (m)", value = NULL, min = 1)
          ),
          p("Q3. How would you describe the geometry of the hazards?"),
          radioButtons(inputId = "geom_vs", 
                      label = NULL, 
                      choices = list("Point-source" = "point", "Area-based" = "area"),
                      selected = character(0), 
                      inline = TRUE)
          )
      } else {
        ph
      }
    })
  })
  
  # Geom
  observeEvent(eventExpr = {input$geom_vs}, handlerExpr = {
    
    if(input$geom_vs == "point"){
      output$ui_geom = renderUI(expr = {
        verticalLayout(
          p("Q3A. Number of contamination points?"),
          numericInput(inputId = "n_contam_vs", label = NULL, value = NULL, min = 1, step = 1),
          p("Q3B. Radius of contamination area(m)?"),
          numericInput(inputId = "spread_radius_vs", label = NULL, value = NULL, min = 0)
        )
      })
    } else {
      output$ui_geom = NULL
    }
  })
  
  # Contamination
  observeEvent(eventExpr = {input$geom_vs}, handlerExpr = {
    
    if(input$geom_vs %in% c("point", "area")){
      output$ui_contam = renderUI(expr = {
        verticalLayout(
          p("Q4. Mean contamination level (log CFU/g)"),
          numericInput(inputId = "cont_level_mu_vs", label = NULL, value = NULL),
          p("Q5. Standard deviation of contamination level (log CFU/g)"),
          numericInput(inputId = "cont_level_sd_vs", label = NULL, value = NULL),
          p("Q6. Background level (CFU/g)"),
          numericInput(inputId = "bg_level_vs", label = NULL, value = 0.00001, min = 0),
          p("Q7. Decay function"),
          radioButtons(inputId = "fun_vs",
                      label = NULL,
                      choices = list("Exponential" = "exp", "Gaussian" = "norm", "Uniform" = "unif"), 
                      selected = character(0), 
                      inline = TRUE),
          conditionalPanel(
            condition = "input.fun_vs == 'exp' | input.fun_vs == 'norm'",
            p("Q7A. How fast do you want the contamination to decay? (left: fast; right: slow)"),
            sliderInput(inputId = "LOC_vs", label = NULL, value = 0.001, min = 0.0001, max = 0.01, ticks = TRUE)
          )
        )
      })
      
    } else {
      output$ui_contam = NULL
    }
  })
  
  # Sampling
  observeEvent(eventExpr = {input$geom_vs}, handlerExpr = {
    
    if(input$geom_vs %in% c("point", "area")){
      output$ui_sp = renderUI(expr = {
        verticalLayout(
          p("Q8. Which sampling attribute case?"),
          sliderInput(inputId = "case_vs", label = NULL, min = 1, value = 10, max = 15, step = 1, round = TRUE),
          disabled(
            p("Q9. How many samples do you want?"),
            numericInput(inputId = "n_sp_vs", label = NULL, value = 5, min = 1, step = 1)
          ),
          p("Q10. What are the microbiological criteria?"),
          splitLayout(
            numericInput(inputId = "m", label = "m", value = 0, min = 0),
            numericInput(inputId = "M", label = "M", value = 0, min = 0)
          ),
          p("Q11. Which sampling strategy would you use?"),
          radioButtons(inputId = "method_sp_vs", label = NULL, choices = list("SRS" = "srs", "STRS" = "strs", "k-step SS" = "ss"), selected = character(0), inline = TRUE)
        )
      })
    } else {
      output$ui_sp = NULL
    }
  })
  
  # Associate case with n_sp
  observeEvent(eventExpr = {input$case_vs}, handlerExpr = {
    
    n_sp_update = switch(EXPR = input$case_vs, 
                         `1` = 5,
                         `2` = 5,
                         `3` = 5,
                         `4` = 5,
                         `5` = 5,
                         `6` = 5,
                         `7` = 5,
                         `8` = 5,
                         `9` = 10,
                         `10` = 5,
                         `11` = 10,
                         `12` = 20,
                         `13` = 15,
                         `14` = 30,
                         `15` = 60,
                         stop("Unknown case", call. = FALSE)
    )
    
    updateNumericInput(session = session, inputId = "n_sp_vs", value = n_sp_update)
  })
  
  # Sampling strategy
  observeEvent(eventExpr = {input$method_sp_vs}, handlerExpr = {
    
    if(input$method_sp_vs == "srs"){
      output$ui_method_sp = NULL
    } else if (input$method_sp_vs == "strs"){
      
      output$ui_method_sp = renderUI(expr = {
        verticalLayout(
          p("Q11A. Along which direction do you want to stratify the field?"),
          selectInput(inputId = "by_vs",
                      label = NULL,
                      choices = list("Row" = "row", "Column" = "column", "2D" = "2d"),
                      multiple = FALSE),
          p("Q11B. How many strata do you want along this direction?"),
          conditionalPanel(condition = "input.by_vs != '2d'", 
                           numericInput(inputId = "n_strata_vs", label = NULL, value = NULL, min = 1)
                           ),
          conditionalPanel(condition = "input.by_vs == '2d'",
                           splitLayout(
                             numericInput(inputId = "n_strata_row_vs", label = "Row strata", value = NULL, min = 1),
                             numericInput(inputId = "n_strata_col_vs", label = "Column strata", value = NULL, min = 1)
                             )
                           )
        )
      })
    } else if (input$method_sp_vs == "ss"){
      
      output$ui_method_sp = renderUI(expr = {
        verticalLayout(
          p("Q11A. Along which direction do you want to stratify the field?"),
          selectInput(inputId = "by_vs",
                      label = NULL,
                      choices = list("Row" = "row", "Column" = "column"),
                      multiple = FALSE),
          p("Q11B. How many strata do you want along this direction?"),
          numericInput(inputId = "n_strata_vs", label = NULL, value = NULL, min = 1)
        )
      })
    } else {
      stop("Unknown sampling method")
    }
  })
  
  # Assay
  observeEvent(eventExpr = {input$method_sp_vs}, handlerExpr = {
    output$ui_assay = renderUI(expr = {
      verticalLayout(
        p("Q12. What's the individual sample mass (g)?"),
        numericInput(inputId = "m_sp_vs", label = NULL, value = 25, min = 0),
        p("Q13. Which detection method would you use?"),
        radioButtons(inputId = "method_det_vs",
                    label = NULL,
                    choices = list("Plating" = "plating", "Enrichment" = "enrichment"),
                    selected = character(0), 
                    inline = TRUE)
      )
    })
  })
  
  # Iteration
  observeEvent(eventExpr = {input$method_det_vs}, handlerExpr = {
    output$ui_iter = renderUI(expr = {
      verticalLayout(
        h3(),
        h2("Iteration section"),
        p("Q14. How many times do you want to iterate the model?"),
        numericInput(inputId = "n_iter_total_vs", label = NULL, value = 25, min = 1, step = 1),
        p("Q15. How many parameters do you want to tune over?"),
        radioButtons(inputId = "n_vars_vs", label = NULL, choices = list(0,1,2), selected = character(0), inline = TRUE)
      )
    })
  })
  
  # Tuning and buttons for the smart mode
  observeEvent(eventExpr = {input$n_vars_vs}, handlerExpr = {
    output$ui_tuning_vs = renderUI(expr = {
      if(input$n_vars_vs == 0){
        NULL
        
      } else if(input$n_vars_vs == 1){
        verticalLayout(
          p("Q15A. Which parameter do you want to tune?"),
          selectInput(inputId = "var_prim_vs",
                      label = NULL,
                      choices = list("Number of contamination points" = "n_contam_vs",
                                     "Number of sample points" = "n_sp_vs",
                                     "Individual sample mass (g)" = "m_sp_vs")),
          p("Q15B. What values do you want to tune over? (separated by a comma)"),
          textInput(inputId = "val_prim_vs", label = NULL, value = "1,2,3")
        )
        
      } else if(input$n_vars_vs == 2) {
        verticalLayout(
          p("Q15A. Which primary parameter do you want to tune?"),
          selectInput(inputId = "var_prim_vs",
                      label = NULL,
                      choices = list("Number of contamination points" = "n_contam_vs",
                                     "Number of sample points" = "n_sp_vs",
                                     "Individual sample mass (g)" = "m_sp_vs")),
          p("Q15B. What values do you want to tune the primary parameter over? (separated by a comma)"),
          textInput(inputId = "val_prim_vs", label = NULL, value = "1,2,3"),
          p("Q15C. Which secondary parameter do you want to tune?"),
          selectInput(inputId = "var_sec_vs",
                      label = NULL,
                      choices = list("Sampling strategy" = "method_sp_vs")),
          p("Q15D. What values do you want to tune the secondary parameter over? (separated by a comma)"),
          textInput(inputId = "val_sec_vs", label = NULL, value = "srs, strs, ss")
        )
      } else {
        stop("Unknown number of tuning paramters")
      }
    })
  })
  
  # UI for loading (smart version)
  observeEvent(eventExpr = {input$n_vars_vs}, handlerExpr = {
    output$ui_load = renderUI(expr = {
      verticalLayout(
        actionButton(inputId = "load_vs", label = "Load parameters"),
        h2()
      )
    })
  })
  
  # UI for visualization and iteration (smart version)
  observeEvent(eventExpr = {input$load_vs}, handlerExpr = {
    output$ui_vis_iter = renderUI(expr = {
      verticalLayout(
        splitLayout(
          actionButton(inputId = "vis_vs", label = "Visualize"),
          actionButton(inputId = "iteration_vs", label = "Iterate")
        )
      )
    })
  })
  
  # Load the parameter once (for both smart and manual version)
  list_load = list()
  observeEvent(eventExpr = {c(input$load, input$load_vs)}, handlerExpr = {

    list_load <<- load_once(input = input, output = output)
    output$print_param = renderTable(expr = make_var_table(Args = list_load$ArgList_default, input = input))

  }, ignoreInit = TRUE)
  
  # Visualize for one iteration
  observeEvent(eventExpr = {input$vis}, handlerExpr = {
    
    vis_once(input = input, output = output, ArgList = list_load$ArgList_default)
    
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