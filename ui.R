#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

page_intro = mainPanel(
    br(),
    p("This is the introduction page.")
)

v_smart = sidebarLayout(
    sidebarPanel(),
    mainPanel()
    
)

v_manual_2d = sidebarLayout(
    sidebarPanel(
        numericInput(inputId = "n_contam", label = "Number of contamination points", value = 1, min = 1, step = 1),
        numericInput(inputId = "x_lim", label = "Length (m)", value = 10, min = 1),
        numericInput(inputId = "y_lim", label = "Width (m)", value = 10, min = 1),
        selectInput(inputId = "geom", label = "Geometry", choices = list("point", "area"), multiple = FALSE),
        numericInput(inputId = "cont_level_mu", label = "Mean contamination level (log CFU/g)", value = 3),
        numericInput(inputId = "cont_level_sd", label = "Standard deviation of contamination level (log CFU/g)", value = 1),
        numericInput(inputId = "bg_level", label = "Background level (CFU/g)", value = 0.00001, min = 0),
        numericInput(inputId = "spread_radius", label = "Radius of contamination area (m)", value = 1, min = 0),
        numericInput(inputId = "LOC", label = "LOC", value = 0.001),
        selectInput(inputId = "fun", label = "Decay function", choices = list("Exponential", "Gaussian", "Uniform")),
        selectInput(inputId = "method_sp", label = "Sampling strategy", choices = list("SRS", "STRS", "k-step SS")),
        numericInput(inputId = "n_sp", label = "Number of sample points", value = 1, min = 1),
        numericInput(inputId = "n_strata", label = "Number of strata", value = NULL, min = 1),
        selectInput(inputId = "by", label = "by", choices = list("row", "column", "2d")),
        numericInput(inputId = "m_sp", label = "Individual sample mass (g)", value = 25, min = 0),
        selectInput(inputId = "method_det", label = "Detection method", choices = list("Plating", "Enrichment")),
        sliderInput(inputId = "case", label = "Case", min = 1, value = 10, max = 15, step = 1, round = TRUE),
        numericInput(inputId = "m", label = "m", value = 0, min = 0),
        numericInput(inputId = "M", label = "M", value = 0, min = 0),
        numericInput(inputId = "n_iter", label = "Number of iteration", value = 10, min = 1, step = 1),
        splitLayout(actionButton(inputId = "vis", label = "Visualize"),
                    submitButton(text = "Run"))
        
    ),
    mainPanel(
        # plotOutput(outputId = "contam_sp_xy", hover = "plot_hover"),
        # verbatimTextOutput(outputId = "debug"),
        verbatimTextOutput(outputId = "debug2")
    )
)

v_manual_3d = sidebarLayout(
    sidebarPanel(
        numericInput(inputId = "c_hat", label = "Estimated bulk contamination level (ppb)", value = 1, min = 1, step = 1),
        numericInput(inputId = "x_lim", label = "Length (m)", value = 10, min = 1),
        numericInput(inputId = "y_lim", label = "Width (m)", value = 10, min = 1),
        numericInput(inputId = "z_lim", label = "Height (m)", value = 10, min = 1)
        
    ),
    mainPanel()
)

v_manual = tabsetPanel(
    id = "v_manual",
    tabPanel(title = "2D", v_manual_2d),
    tabPanel(title = "3D", v_manual_3d)
)

page_inputs = tabsetPanel(
    tabPanel(title = "Smart Version", v_smart),
    tabPanel(title = "Manual Version", v_manual)
)


page_outputs = sidebarLayout(
    sidebarPanel(),
    mainPanel()
)

shinyUI(fluidPage(
    
    headerPanel(p("2D and 3D Sampling Simulation")),
    
    tabsetPanel(
        tabPanel(title = "Introduction", page_intro),
        
        tabPanel(title = "Inputs", page_inputs),
        
        tabPanel(title = "Outputs", page_outputs)
    )
))
