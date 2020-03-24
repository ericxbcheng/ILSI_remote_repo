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

# Manual version for 2D
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
      wellPanel(
        selectInput(inputId = "by", 
                    label = "Stratify by", 
                    choices = list("Row" = "row", "Column" = "column", "2D" = "2d"), 
                    selected = NA,
                    multiple = FALSE),
        numericInput(inputId = "n_strata", label = "Number of strata", value = 5, min = 1),
        numericInput(inputId = "n_strata_row", label = "Number of row strata (2D only)", value = NULL, min = 1),
        numericInput(inputId = "n_strata_col", label = "Number of column strata (2D only)", value = NULL, min = 1)
      ),
      numericInput(inputId = "m_sp", label = "Individual sample mass (g)", value = 25, min = 0),
      selectInput(inputId = "method_det", 
                  label = "Detection method", 
                  choices = list("Plating" = "plating", "Enrichment" = "enrichment"), 
                  selected = "enrichment", 
                  multiple = FALSE),
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
      uiOutput(outputId = "ui_tuning"),
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

# Manual version for 3D
v_manual_3D = fluidRow(
  
  box(
    title = "3D Input Parameters"
  ),
  box(
    title = "Visualization for one iteration"
  )
)

# Place holder
ph = p("Under development")

# Smart version 
v_smart = fluidPage(
  fluidRow(
    box(title = "Questionnaire", ph),
    
    box(title = "Visualization for one iteration", ph)
    )
)

# Visualization page
page_vis = fluidRow(

  box(title = "Selected parameters",
      tableOutput(outputId = "print_param")),
  box(title = "Visualization for multiple iterations",
      uiOutput(outputId = "yvar"),
      plotOutput(outputId = "plot_iterate"))
)

page_export = fluidRow(
  
  box(title = "Download the simulation data",
      p("Click the following button to download the csv file that contains the simulation data."),
      downloadButton(outputId = "downloadData", label = "Download")
      ),
  
  box(
    title = "Variable interpretation",
    p("The csv file contains a header with multiple variables. The interpretation is as follows.")
  )
  
)


body = dashboardBody(
  
  tabItems(
    tabItem(tabName = "intro", 
            h2("This is the introduction page.")),
    tabItem(tabName = "v_smart", v_smart),
    tabItem(tabName = "2D", v_manual_2D),
    tabItem(tabName = "3D", v_manual_3D),
    tabItem(tabName = "vis", page_vis),
    tabItem(tabName = "export", page_export)
  )
)    

shinyUI(dashboardPage(
  
  dashboardHeader(title = "2D and 3D Sampling Simulation"),
  sidebar,
  body
))