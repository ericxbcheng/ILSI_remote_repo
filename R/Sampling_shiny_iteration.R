f_ui_iter = function(input){
  verticalLayout(
    h3(),
    h2("Iteration section"),
    p("Q14. How many times do you want to iterate the model?"),
    numericInput(inputId = "n_iter_total_vs", label = NULL, value = 25, min = 1, step = 1),
    p("Q15. How many parameters do you want to tune over?"),
    radioButtons(inputId = "n_vars_vs", label = NULL, choices = list(0,1,2), selected = character(0), inline = TRUE)
  )
}