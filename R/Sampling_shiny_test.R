# Wizard interface
# Switch tab in the wizard interface
switch_tab = function(page, session){
  updateTabsetPanel(session = session, inputId = "wizard", selected = page)
}

output$questionnaire = renderUI(expr = {
  fluidPage(
    tags$style("#wizard { display:none; }"),
    tabsetPanel(
      id = "wizard",
      tabPanel(title = "Q1",  
               p("Q1: Which type of product do you want to simulate?"),
               radioButtons(inputId = "spread",
                            label = NULL,
                            selected = character(0),
                            inline = TRUE,
                            choiceNames = list("Produce in a field", "Grains in a bin"),
                            choiceValues = list("continuous", "discrete")),
               actionButton(inputId = "b12", label = "Next")),
      tabPanel(title = "Q2", p("Q2"),
               actionButton(inputId = "b21", label = "Previous"), 
               actionButton(inputId = "b23", label = "Next")),
      tabPanel(title = "Q3", p("Q3"),
               actionButton(inputId = "b32", label = "Previous"))
    )
  )
})

observeEvent(eventExpr = input$b12, handlerExpr = switch_tab(session = session, page = "Q2"))
observeEvent(eventExpr = input$b23, handlerExpr = switch_tab(session = session, page = "Q3"))
observeEvent(eventExpr = input$b21, handlerExpr = switch_tab(session = session, page = "Q1"))
observeEvent(eventExpr = input$b32, handlerExpr = switch_tab(session = session, page = "Q2"))

# renderUI() with highly-nested IF-ELSE
observeEvent(eventExpr = {input$spread}, handlerExpr = {
  output$questionnaire = renderUI(expr = f_questionnaire(input = input))
})

# Questionnaire
f_questionnaire = function(input){
  
  if(input$spread == "continuous"){
    
    verticalLayout(
      p("2. What are the dimensions of the field?"),
      splitLayout(
        numericInput(inputId = "x_lim", label = "Length (m)", value = NULL, min = 1),
        numericInput(inputId = "y_lim", label = "Width (m)", value = NULL, min = 1)
      ),
      
      p("3. How would you describe the geometry of the hazards?"),
      radioButtons(inputId = "geom",
                   label = "Geometry",
                   choices = list("Point-source" = "point", "Area-based" = "area"),
                   selected = character(0),
                   inline = TRUE),
      
      if(input$geom == "point"){
        p("point")
      } else if (input$geom == "area"){
        p("area")
      } else {
        NULL
      }
      
    )
    
  } else if (input$spread == "discrete"){
    message("Under construction")
  } else {
    stop("Unknown product type")
  }
}

# Conditional panels
v_smart = fluidPage(
  fluidRow(
    box(title = "Questionnaire", 
        p("Q1: Which type of product do you want to simulate?"),
        radioButtons(inputId = "spread",
                     label = NULL,
                     selected = character(0),
                     inline = TRUE,
                     choiceNames = list("Produce in a field", "Grains in a bin"),
                     choiceValues = list("continuous", "discrete")),
        
        # Smart version 2D
        conditionalPanel(condition = "input.spread == 'continuous'",
                         p("Q2. What are the dimensions of the field?"),
                         splitLayout(
                           numericInput(inputId = "x_lim", label = "Length (m)", value = NULL, min = 1),
                           numericInput(inputId = "y_lim", label = "Width (m)", value = NULL, min = 1)
                         ),
                         p("Q3. How would you describe the geometry of the hazards?"),
                         radioButtons(inputId = "geom",
                                      label = NULL,
                                      choices = list("Point-source" = "point", "Area-based" = "area"),
                                      selected = character(0),
                                      inline = TRUE)),
        conditionalPanel(condition = "input.geom == 'point'",
                         p("Q4. How many contamination zones do you want to simulate?"),
                         numericInput(inputId = "n_contam", label = NULL, value = 1, min = 1, step = 1),
                         p("Q5. What's the radius of each contamination area (m)?"),
                         numericInput(inputId = "spread_radius", label = NULL, value = 1, min = 0)),
        conditionalPanel(condition = "input.geom == 'area'", ph),
        
        
        # Smart version 3D
        conditionalPanel(condition = "input.spread == 'discrete'", ph)
    ),
    
    box(title = "Visualization for one iteration")
  )
)
