f_ui_sp = function(input, ...){
  if(input$spread_vs == "continuous"){
    if(input$geom_vs %in% c("point", "area")){
      verticalLayout(
        p("Q8. Which sampling attribute case?"),
        sliderInput(inputId = "case_vs", label = NULL, min = 1, value = 10, max = 15, step = 1, round = TRUE),
        disabled(
          actionButton("Doc_Samplepoints_Sma", "",width = "40px",icon = icon("question-circle") ),
          p("Q9. How many samples do you want?"),
          numericInput(inputId = "n_sp_vs", label = NULL, value = 5, min = 1, step = 1)
        ),
        p("Q10. What are the microbiological criteria?"),
        splitLayout(
          numericInput(inputId = "m_vs", label = "m", value = 0, min = 0),
          numericInput(inputId = "M_vs", label = "M", value = 0, min = 0)
        ),
        actionButton("Doc_SamplingStrat_Sma", "",width = "40px",icon = icon("question-circle") ),
        p("Q11. Which sampling strategy would you use?"),
        radioButtons(inputId = "method_sp_vs", label = NULL, choices = list("SRS" = "srs", "STRS" = "strs", "k-step SS" = "ss"), selected = character(0), inline = TRUE)
      )} else {
        NULL
      }
    } else if (input$spread_vs == "discrete"){
      verticalLayout(
        actionButton("Doc_Probe", "",width = "40px",icon = icon("question-circle") ),
        p("Q10. What's the diameter of the probe (m)?"),
        numericInput(inputId = "d_vs", label = NULL, value = 0.04, min = 0.01, max = 1, step = 0.01),
        actionButton("Doc_Sampling_s", "",width = "40px",icon = icon("question-circle") ),
        p("Q11. Which sampling strategy would you use?"),
        radioButtons(inputId = "method_sp_3d_vs", label = NULL, choices = list("SRS" = "srs", "STRS" = "strs", "GIPSA SS" = "ss"), selected = character(0), inline = TRUE)
      )
  } else {
    NULL
  }
}

case_sp_lookup = function(input){
  switch(EXPR = input$case_vs, 
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
