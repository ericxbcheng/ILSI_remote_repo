f_ui_assay = function(input){
  if(input$spread_vs == "continuous"){
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
  } else if(input$spread_vs == "discrete"){
    verticalLayout(
      p("Q12. How homogeneous should the samples be after grinding?"),
      fluidRow(
        column(width = 10, 
               sliderInput(inputId = "homogeneity_vs", label = NULL, value = 0.6, min = 0, max = 1, step = 0.1), style = "padding-right: 0px;"),
        column(width = 2, 
               actionButton("Doc_Homogeneity_s", "",style = "padding: 5px; margin: 0px 0px 0px 0px;",icon = icon("question-circle") )
        )) ,
      p("Q13. What's the detection method?"),
      radioButtons(inputId = "method_det_3d_vs", label = NULL, choices = list("ELISA" = "ELISA aflatoxin"), 
                   selected = character(0), inline = TRUE)
    )
  } else {
    stop("Unknown spread type")
  }
}