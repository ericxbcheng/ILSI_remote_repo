f_ui_sp = function(input, ...){
  if(input$spread_vs == "continuous"){
    if(input$geom_vs %in% c("point", "area")){
      verticalLayout(
        p("Q8. Which sampling attribute case?"),
        sliderInput(inputId = "case_vs", label = NULL, min = 1, value = 10, max = 15, step = 1, round = TRUE),
        disabled(
          p("Q9. How many samples do you want?"),
          fluidRow(
            column(width = 10, 
                   numericInput(inputId = "n_sp_vs", label = NULL, value = 5, min = 1, step = 1), style = "padding-right: 0px;"),
            column(width = 2, 
                   actionButton("Doc_Samplepoints_Sma", "",style = "padding: 5px; margin: 0px 0px 0px 0px;",icon = icon("question-circle") )
            )) 
        ),
        p("Q10. What are the microbiological criteria?"),
        splitLayout(
          numericInput(inputId = "m_vs", label = "m", value = 0, min = 0),
          numericInput(inputId = "M_vs", label = "M", value = 0, min = 0)
        ),
        p("Q11. Which sampling strategy would you use?"),
        fluidRow(
          column(width = 10, 
                 radioButtons(inputId = "method_sp_vs", label = NULL, choices = list("SRS" = "srs", "STRS" = "strs", "k-step SS" = "ss"), selected = character(0), inline = TRUE), style = "padding-right: 0px;"),
          column(width = 2, 
                 actionButton("Doc_SamplingStrat_Sma", "",style = "padding: 5px; margin: 0px 0px 0px 0px;",icon = icon("question-circle") )
          )) 
      )} else {
        NULL
      }
    } else if (input$spread_vs == "discrete"){
      verticalLayout(
        p("Q10. What's the diameter of the probe (m)?"),
        fluidRow(
          column(width = 10, 
                 numericInput(inputId = "d_vs", label = NULL, value = 0.04, min = 0.01, max = 1, step = 0.01), style = "padding-right: 0px;"),
          column(width = 2, 
                 actionButton("Doc_Probe", "",style = "padding: 5px; margin: 0px 0px 0px 0px;",icon = icon("question-circle") )
          )) ,
        p("Q11. Which sampling strategy would you use?"),
        fluidRow(
          column(width = 10, 
                 radioButtons(inputId = "method_sp_3d_vs", label = NULL, choices = list("SRS" = "srs", "STRS" = "strs", "GIPSA SS" = "ss"), selected = character(0), inline = TRUE), style = "padding-right: 0px;"),
          column(width = 2, 
                 actionButton("Doc_Sampling_s", "",style = "padding: 5px; margin: 0px 0px 0px 0px;",icon = icon("question-circle") )
          ))
      )
  } else {
    NULL
  }
}

case_sp_lookup = function(case){
  switch(EXPR = case, 
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
}

f_ui_method_sp = function(input,...){
  if(input$spread_vs == "continuous"){
    f_ui_method_sp_2d(input = input)
    
  } else if(input$spread_vs == "discrete"){
    f_ui_method_sp_3d(input = input)
    
  } else {
    stop("Unknown spread type")
  }
}

f_ui_method_sp_2d = function(input,...){
  if(input$method_sp_vs == "srs"){
    NULL
  } else if (input$method_sp_vs == "strs"){
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
  } else if (input$method_sp_vs == "ss"){
    verticalLayout(
      p("Q11A. Along which direction do you want to take a sample every k steps?"),
      selectInput(inputId = "by_vs",
                  label = NULL,
                  choices = list("Row" = "row", "Column" = "column"),
                  multiple = FALSE)
    )
  } else {
    stop("Unknown sampling method")
  }
}

f_ui_method_sp_3d = function(input, ...){
  if(input$method_sp_3d_vs == "srs"){
    verticalLayout(
      p("Q11A. How many probes of sample do you want to take?"),
      numericInput(inputId = "n_sp_3d_vs", label = NULL, value = NULL, min = 1)
    )
  } else if (input$method_sp_3d_vs == "strs") {
    verticalLayout(
      p("Q11A. How many probes of sample do you want to take?"),
      numericInput(inputId = "n_sp_3d_vs", label = NULL, value = NULL, min = 1),
      p("Q11B. Along which direction do you want to stratify the bin?"),
      selectInput(inputId = "by_3d_vs",
                  label = NULL,
                  choices = list("Row" = "row", "Column" = "column", "2D" = "2d"),
                  selected = NA,
                  multiple = FALSE),
      p("Q11C. How many strata do you want along this direction?"),
      conditionalPanel(condition = "input.by_3d_vs != '2d'", 
                       numericInput(inputId = "n_strata_3d_vs", label = NULL, value = NULL, min = 1)
      ),
      conditionalPanel(condition = "input.by_3d_vs == '2d'",
                       splitLayout(
                         numericInput(inputId = "n_strata_row_3d_vs", label = "Row strata", value = NULL, min = 1),
                         numericInput(inputId = "n_strata_col_3d_vs", label = "Column strata", value = NULL, min = 1)
                       )
      )
    )
  } else if (input$method_sp_3d_vs == "ss"){
    verticalLayout(
      p("Q11A. Which grain container do you have?"),
      selectInput(inputId = "container_vs", label = NULL, 
                  choices = list("Truck" = "truck", "Barge" = "barge", "Hopper car" = "hopper")),
      conditionalPanel(condition = "input.container_vs == 'hopper'",
                       p("Q11B. Define hopper car's features"),
                       splitLayout(
                         selectInput(inputId = "compartment_vs", label = "Number of compartments", choices = list(2, 3)),
                         selectInput(inputId = "type_vs", label = "Hopper car type", 
                                     choices = list("Open-top" = "open_top", "Trough" = "trough"))
                       )
                       
      )
    )
  } else {
    stop("Unknown sampling strategy.")
  }
}

# A warning message when n_sp is not consistent with n_strata
warning_n_sp_n_strata = function(spread, v_smart, input){
  
  # Extract n_sp, n_strata, by
  if(spread == "continuous"){
    if(v_smart == FALSE){
      n_sp = input$n_sp
      by = input$by
      if(by != "2d"){
        n_strata = input$n_strata
        
      } else {
        n_strata = c(input$n_strata_row, input$n_strata_col)
      }
      
    } else {
      n_sp = input$n_sp_vs
      by = input$by_vs
      n_strata = make_n_strata(input = input)
      
    }
  } else if (spread == "discrete"){
    if(v_smart == FALSE){
      n_sp = input$n_sp_3d
      by = input$by_3d
      if(by != "2d"){
        n_strata = input$n_strata_3d
      } else {
        n_strata = c(input$n_strata_row_3d, input$n_strata_col_3d)
      }
      
    } else {
      n_sp = input$n_sp_3d_vs
      by = input$by_3d_vs
      n_strata = make_n_strata(input = input)
      
    }
  } else {
    stop("Undefined spread type. Select 'continuous' or 'discrete'.")
  }
  
  # Make the warning
  make_modal_n_sp_n_strata(n_sp = n_sp, n_strata = n_strata, by = by)
}

# Make a warning message for when n_sp is not consistent with n_strata
make_modal_n_sp_n_strata = function(n_sp, n_strata, by){
  
  if(by != "2d"){
    if(n_sp %% n_strata != 0){
      showModal(ui = modalDialog("Please ensure the number of samples is a multiple of the number of strata."))
    }
  } else {
    if(n_sp %% (n_strata[1]*n_strata[2]) != 0){
      showModal(ui = modalDialog("Please ensure the number of samples is a multiple of row strata times column strata."))
    }
  }
}

# Make a warning message for when n_sp is not consistent with case
make_modal_n_sp_case = function(n_sp, case){
  
  # Correct n_sp
  n_sp_correct = case_sp_lookup(case = case)
  
  if(n_sp != n_sp_correct){
    showModal(ui = modalDialog("Warning! The number of samples doesn't match the attribute case. The assayed samples will be evaluated by the chosen attribute case."))
  }
}
