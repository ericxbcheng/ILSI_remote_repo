#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

sidebar = dashboardSidebar(
  
  sidebarMenu(
    id = "sidebarMenu",
    menuItem(text = "Introduction", 
             tabName = "intro"),
    menuItem(text = "Inputs", 
             menuItem(text = "Smart Version", 
                      tabName = "v_smart"), 
             menuItem(text = "Manual Version", 
                      tabName = "v_manual", 
                      menuSubItem(text = "2D", tabName = "2D"), 
                      menuSubItem(text = "3D", tabName = "3D")),
             tabName = "inputs"),
    menuItem(text = "Outputs", 
             menuSubItem(text = "Visualization", tabName = "vis"),
             menuSubItem(text = "Data Export", tabName = "export"),
             tabName = "outputs")
  )
)

v_manual_2D = fluidRow(
  
  box(title = "2D Input Parameters",
      splitLayout(
        numericInput(inputId = "x_lim", label = "Length (m)", value = 10, min = 1),
        numericInput(inputId = "y_lim", label = "Width (m)", value = 10, min = 1)
      ),
      selectInput(inputId = "geom", label = "Geometry", choices = list("Point-source" = "point", "Area-based" = "area"), multiple = FALSE),
      conditionalPanel(
        condition = "input.geom == 'point'",
        numericInput(inputId = "n_contam", label = "Number of contamination points", value = 1, min = 1, step = 1),
        numericInput(inputId = "spread_radius", label = "Radius of contamination area (m)", value = 1, min = 0)
      ),
      splitLayout(
        numericInput(inputId = "cont_level_mu", label = "Mean contamination level (log CFU/g)", value = 3),
        numericInput(inputId = "cont_level_sd", label = "Standard deviation of contamination level (log CFU/g)", value = 1)
      ),
      numericInput(inputId = "bg_level", label = "Background level (CFU/g)", value = 0.00001, min = 0),
      selectInput(inputId = "fun", label = "Decay function", choices = list("Exponential" = "exp", "Gaussian" = "norm", "Uniform" = "unif")),
      conditionalPanel(
        condition = "input.fun != 'unif'",
        numericInput(inputId = "LOC", label = "Limit of contamination contribution (0 - 1)", value = 0.001, min = 0, max = 1)
      ),
      numericInput(inputId = "n_sp", label = "Number of sample points", value = 5, min = 1, step = 1),
      selectInput(inputId = "method_sp", label = "Sampling strategy", choices = list("SRS" = "srs", "STRS" = "strs", "k-step SS" = "ss")),
      conditionalPanel(
        condition = "input.method_sp == 'strs'",
        selectInput(inputId = "by", label = "Stratify by", choices = list("Row" = "row", "Column" = "column", "2D" = "2d")),
        conditionalPanel(
          condition = "input.by != '2d'",
          numericInput(inputId = "n_strata", label = "Number of strata", value = NULL, min = 1)
        ),
        conditionalPanel(
          condition = "input.by == '2d'",
          numericInput(inputId = "n_strata_row", label = "Number of strata (row)", value = NULL, min = 1),
          numericInput(inputId = "n_strata_col", label = "Number of strata (column)", value = NULL, min = 1)
        )
      ),
      conditionalPanel(
        condition = "input.method_sp == 'ss'",
        selectInput(inputId = "by", label = "By", choices = list("Row" = "row", "Column" = "column"))
      ),
      numericInput(inputId = "m_sp", label = "Individual sample mass (g)", value = 25, min = 0),
      selectInput(inputId = "method_det", label = "Detection method", choices = list("Plating" = "plating", "Enrichment" = "enrichment")),
      sliderInput(inputId = "case", label = "Case", min = 1, value = 10, max = 15, step = 1, round = TRUE),
      splitLayout(
        numericInput(inputId = "m", label = "m", value = 0, min = 0),
        numericInput(inputId = "M", label = "M", value = 0, min = 0)
      ),
      h3(),
      h2("Iteration section"),
      splitLayout(
        numericInput(inputId = "n_seed", label = "Number of contamination patterns", value = 1, min = 1, step = 1),
        numericInput(inputId = "n_iter", label = "Number of sampling patterns per contamination pattern", value = 1, min = 1, step = 1)
      ),
      selectInput(inputId = "n_vars", label = "Number of tuning parameters", choices = list(0,1,2), multiple = FALSE),
      conditionalPanel(
        condition = "input.n_vars == 1",
        selectInput(inputId = "var_prim", 
                    label = "Primary tuning parameter", 
                    choices = list("Number of contamination points" = "n_contam",
                                   "Number of sample points" = "n_sp",
                                   "Individual sample mass (g)" = "m_sp")),
        textInput(inputId = "val_prim", label = "Tuning value(s)", value = NULL)
      ),
      conditionalPanel(
        condition = "input.n_vars == 2",
        selectInput(inputId = "var_prim", 
                    label = "Primary tuning parameter", 
                    choices = list("Number of contamination points" = "n_contam",
                                   "Number of sample points" = "n_sp",
                                   "Individual sample mass (g)" = "m_sp")),
        textInput(inputId = "val_prim", label = "Tuning value(s)", value = NULL),
        selectInput(inputId = "var_sec", 
                    label = "Secondary tuning parameter", 
                    choices = list("Sampling strategy" = "method_sp")),
        textInput(inputId = "val_sec", label = "Tuning value(s)", value = NULL)
      ),
      actionButton(inputId = "load", label = "Load parameters"),
      h2(),
      conditionalPanel(
        condition = "input.load > 0",
        splitLayout(
          actionButton(inputId = "vis", label = "Visualize"),
          actionButton(inputId = "iteration", label = "Iterate")
        )
      )
  ),
  
  box(title = "Visualization for one iteration",
      plotOutput(outputId = "overlay_draw"),
      plotOutput(outputId = "contam_level_draw")
  )
)


# Visualization page
page_vis = fluidRow(

  box(title = "Selected parameters",
      verbatimTextOutput(outputId = "print_param")),
  box(title = "Visualization for multiple iterations",
      plotOutput(outputId = "plot_iterate"))
)


body = dashboardBody(
  
  tabItems(
    tabItem(tabName = "intro", 
            h2("This is the introduction page.")),
    tabItem(tabName = "v_smart", 
            h2("This is the smart version page.")),
    tabItem(tabName = "2D", v_manual_2D),
    tabItem(tabName = "3D", h2("3D inputs.")),
    tabItem(tabName = "vis", page_vis),
    tabItem(tabName = "export",
            h2("This is the data export page."))
  )
)    

shinyUI(dashboardPage(
  
  dashboardHeader(title = "2D and 3D Sampling Simulation"),
  sidebar,
  body
))