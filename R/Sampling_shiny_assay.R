f_ui_assay = function(input){
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
}