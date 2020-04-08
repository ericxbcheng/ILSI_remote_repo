f_ui_dims = function(input, ...){
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
}

f_ui_geom = function(input, ...) {
  if (input$geom_vs == "point") {
    verticalLayout(
      p("Q3A. Number of contamination points?"),
      numericInput(inputId = "n_contam_vs", label = NULL, value = NULL, min = 1, step = 1),
      p("Q3B. Radius of contamination area(m)?"),
      numericInput(inputId = "spread_radius_vs", label = NULL, value = NULL, min = 0)
    )
  } else {
    NULL
  }
}

f_ui_contam = function(input, ...){
  if(input$geom_vs %in% c("point", "area")){
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
  } else {
    NULL
  }
}