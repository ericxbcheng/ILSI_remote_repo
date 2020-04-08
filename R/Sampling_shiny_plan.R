f_ui_sp = function(input, ...){
  if(input$geom_vs %in% c("point", "area")){
      verticalLayout(
        p("Q8. Which sampling attribute case?"),
        sliderInput(inputId = "case_vs", label = NULL, min = 1, value = 10, max = 15, step = 1, round = TRUE),
        disabled(
          p("Q9. How many samples do you want?"),
          numericInput(inputId = "n_sp_vs", label = NULL, value = 5, min = 1, step = 1)
        ),
        p("Q10. What are the microbiological criteria?"),
        splitLayout(
          numericInput(inputId = "m_vs", label = "m", value = 0, min = 0),
          numericInput(inputId = "M_vs", label = "M", value = 0, min = 0)
        ),
        p("Q11. Which sampling strategy would you use?"),
        radioButtons(inputId = "method_sp_vs", label = NULL, choices = list("SRS" = "srs", "STRS" = "strs", "k-step SS" = "ss"), selected = character(0), inline = TRUE)
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
      p("Q11A. Along which direction do you want to stratify the field?"),
      selectInput(inputId = "by_vs",
                  label = NULL,
                  choices = list("Row" = "row", "Column" = "column"),
                  multiple = FALSE),
      p("Q11B. How many strata do you want along this direction?"),
      numericInput(inputId = "n_strata_vs", label = NULL, value = NULL, min = 1)
    )
  } else {
    stop("Unknown sampling method")
  }
}