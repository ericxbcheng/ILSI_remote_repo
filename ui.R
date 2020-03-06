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

# v_manual_3d = sidebarLayout(
#     sidebarPanel(
#         numericInput(inputId = "c_hat", label = "Estimated bulk contamination level (ppb)", value = 1, min = 1, step = 1),
#         numericInput(inputId = "x_lim", label = "Length (m)", value = 10, min = 1),
#         numericInput(inputId = "y_lim", label = "Width (m)", value = 10, min = 1),
#         numericInput(inputId = "z_lim", label = "Height (m)", value = 10, min = 1)
#         
#     ),
#     mainPanel()
# )

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
        numericInput(inputId = "n_contam", label = "Number of contamination points", value = 1, min = 1, step = 1),
        numericInput(inputId = "x_lim", label = "Length (m)", value = 10, min = 1),
        numericInput(inputId = "y_lim", label = "Width (m)", value = 10, min = 1),
        selectInput(inputId = "geom", label = "Geometry", choices = list("point", "area"), multiple = FALSE),
        numericInput(inputId = "cont_level_mu", label = "Mean contamination level (log CFU/g)", value = 3),
        numericInput(inputId = "cont_level_sd", label = "Standard deviation of contamination level (log CFU/g)", value = 1),
        numericInput(inputId = "bg_level", label = "Background level (CFU/g)", value = 0.00001, min = 0),
        numericInput(inputId = "spread_radius", label = "Radius of contamination area (m)", value = 1, min = 0),
        numericInput(inputId = "LOC", label = "LOC", value = 0.001),
        selectInput(inputId = "fun", label = "Decay function", choices = list("Exponential" = "exp", "Gaussian" = "norm", "Uniform" = "unif")),
        selectInput(inputId = "method_sp", label = "Sampling strategy", choices = list("SRS" = "srs", "STRS" = "strs", "k-step SS" = "ss")),
        numericInput(inputId = "n_sp", label = "Number of sample points", value = 1, min = 1),
        selectInput(inputId = "by", label = "By", choices = list("Row" = "row", "Column" = "column", "2D" = "2d")),
        numericInput(inputId = "n_strata", label = "Number of strata", value = NULL, min = 1),
        numericInput(inputId = "m_sp", label = "Individual sample mass (g)", value = 25, min = 0),
        selectInput(inputId = "method_det", label = "Detection method", choices = list("Plating", "Enrichment")),
        sliderInput(inputId = "case", label = "Case", min = 1, value = 10, max = 15, step = 1, round = TRUE),
        numericInput(inputId = "m", label = "m", value = 0, min = 0),
        numericInput(inputId = "M", label = "M", value = 0, min = 0),
        numericInput(inputId = "n_iter", label = "Number of iteration", value = 10, min = 1, step = 1),
        splitLayout(actionButton(inputId = "vis", label = "Visualize"),
                    actionButton(inputId = "iteration", label = "Iterate"))),
    
    box(title = "Visualization for one iteration",
        plotOutput(outputId = "overlay_draw"),
        plotOutput(outputId = "contam_level_draw")
        
        )
    
    
)

body = dashboardBody(
    
    tabItems(
        tabItem(tabName = "intro", 
                h2("This is the introduction page.")),
        tabItem(tabName = "v_smart", 
                h2("This is the smart version page.")),
        tabItem(tabName = "2D", v_manual_2D),
        tabItem(tabName = "3D", h2("3D inputs.")),
        tabItem(tabName = "vis", 
                h2("This is the visualization page.")),
        tabItem(tabName = "export",
                h2("This is the data export page."))
    )
)    
    
shinyUI(dashboardPage(
    
    dashboardHeader(title = "2D and 3D Sampling Simulation"),
    sidebar,
    body
))
