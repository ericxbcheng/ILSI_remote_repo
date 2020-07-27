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

# Manual version for 3D
v_manual_3D = fluidRow(

  box(title = "3D Input Parameters", 
      
      fluidRow(
        column(width = 10, 
               actionButton( "ThreeD_Scenario", "Click for Automatic Scenario"), style = "padding-right: 0px;"),
        column(width = 2, 
               actionButton("Doc_Scenario", "",icon = icon("question-circle"), 
                            style = "padding: 5px; margin: 0px 0px 0px 0px;" )
        )),
      
      fluidRow(
        column(width = 10, 
               splitLayout(
                 numericInput(inputId = "x_lim_3d", label = "Length (m)", value = 1, min = 1),
                 numericInput(inputId = "y_lim_3d", label = "Width (m)", value = 1, min = 1),
                 numericInput(inputId = "z_lim_3d", label = "Height (m)", value = 1, min = 1)
               ), style = "padding-right: 0px;"),
        column(width = 2, 
               actionButton("Doc_Bins", "",icon = icon("question-circle"), 
                            style = "padding: 5px; margin: 26px 0px 0px 0px;" )
        )),
      fluidRow(
        column(width = 10, 
               splitLayout(
                 selectInput(inputId = "tox", label = "Mycotoxin", choices = list("Aflatoxin" = "AF")),
                 numericInput(inputId = "c_hat", label = "Overall mycotoxin level (ppb)", value = 1, min = 0.001)
               ), style = "padding-right: 0px;"),
        column(width = 2, 
               actionButton("Doc_Levels", "",icon = icon("question-circle"), 
                            style = "padding: 5px; margin: 26px 0px 0px 0px;" )
        )), 
    
      fluidRow(
        column(width = 10, 
               selectInput(inputId = "dis_level_type", 
                           label = "Mycotoxin distribution in contaminated grains", 
                           choices = list("Uniform" = "constant", "Gamma" = "Gamma"), 
                           multiple = FALSE),style = "padding-right: 0px;"),
        column(width = 2, 
               actionButton("Doc_Distributions", "",style = "padding: 5px; margin: 26px 0px 0px 0px;",icon = icon("question-circle") )
        )), 
    
    conditionalPanel(condition = "input.dis_level_type == 'constant'",
                     numericInput(inputId = "dis_level_const_arg", 
                                  label = "Mycotoxin level in contaminated grains(ppb)", 
                                  value = 40000, min = 0.001)),
    conditionalPanel(condition = "input.dis_level_type == 'Gamma'",
                     splitLayout(
                       numericInput(inputId = "dis_level_gm_mode", 
                                    label = "Mode (Most frequent level)(ppb)", value = 40000),
                       numericInput(inputId = "dis_level_gm_lb", 
                                    label = "Lower bound (ppb)", 
                                    value = 20, 
                                    min = 0.001)
                       )
                     ),
    fluidRow(
      column(width = 10, 
             numericInput(inputId = "n_affected", label = "Number of grains in a cluster", value = 0, min = 0, step = 1),style = "padding-right: 0px;"),
      column(width = 2, 
             actionButton("Doc_Clusters", "",style = "padding: 5px; margin: 26px 0px 0px 0px;",icon = icon("question-circle") )
      )), 
    conditionalPanel(condition = "input.n_affected > 0",
                     wellPanel(
                       p(strong("Cluster covariance matrix")),
                       splitLayout(
                         numericInput(inputId = "vcov_11", label = "Length", value = 0.0004, min = 0),
                         numericInput(inputId = "vcov_12", label = "Width", value = 0, min = 0),
                         numericInput(inputId = "vcov_13", label = "Height", value = 0, min = 0)
                       ),
                       splitLayout(
                         p(""),
                         numericInput(inputId = "vcov_22", label = NULL, value = 0.0004, min = 0),
                         numericInput(inputId = "vcov_23", label = NULL, value = 0, min = 0)
                       ),
                       splitLayout(
                         p(""),
                         p(""),
                         numericInput(inputId = "vcov_33", label = NULL, value = 0.0004, min = 0)
                       )
                     )),
    fluidRow(
      column(width = 10, 
             selectInput(inputId = "method_sp_3d", label = "Sampling strategy", choices = list("SRS" = "srs", "STRS" = "strs", "SS" = "ss")),style = "padding-right: 0px;"),
      column(width = 2, 
             actionButton("Doc_Sampling", "",style = "padding: 5px; margin: 26px 0px 0px 0px;",icon = icon("question-circle") )
      )), 
    wellPanel(
      numericInput(inputId = "n_sp_3d", label = "Number of probes", value = 5, min = 1),
      selectInput(inputId = "by_3d",
                  label = "Stratify by",
                  choices = list("Row" = "row", "Column" = "column", "2D" = "2d"),
                  selected = NA,
                  multiple = FALSE),
      numericInput(inputId = "n_strata_3d", label = "Number of strata", value = 5, min = 1),
      numericInput(inputId = "n_strata_row_3d", label = "Number of row strata (2D only)", value = NULL, min = 1),
      numericInput(inputId = "n_strata_col_3d", label = "Number of column strata (2D only)", value = NULL, min = 1),
      selectInput(inputId = "container", label = "Grain container (SS Only)", 
                  choices = list("Truck" = "truck", "Barge" = "barge", "Hopper car" = "hopper")),
      conditionalPanel(condition = "input.container == 'hopper'",
                       selectInput(inputId = "compartment", label = "Number of compartments", choices = list(2, 3)),
                       selectInput(inputId = "type", label = "Hopper car type", 
                                   choices = list("Open-top" = "open_top", "Trough" = "trough")))
      
    ),
    fluidRow(
      column(width = 10, 
             numericInput(inputId = "d", label = "Probe diameter (m)", value = 0.04, min = 0.01, max = 1, step = 0.01),style = "padding-right: 0px;"),
      column(width = 2, 
             actionButton("Doc_Probes", "",style = "padding: 5px; margin: 26px 0px 0px 0px;",icon = icon("question-circle") )
      )), 
    
    splitLayout(
      numericInput(inputId = "m_kbar", label = "Single kernel mass (g)", value = 0.3, min = 0.001),
      numericInput(inputId = "rho", label = "Density (g/cm^3)", value = 1.28, min = 0.001)
    ),
    fluidRow(
      column(width = 10, 
             sliderInput(inputId = "homogeneity", label = "How homogeneous should the samples be after grinding?", value = 0.6, min = 0, max = 1, step = 0.1),style = "padding-right: 0px;"),
      column(width = 2, 
             actionButton("Doc_Homogeneity", "",style = "padding: 5px; margin: 26px 0px 0px 0px;",icon = icon("question-circle") )
      )), 
    splitLayout(
      selectInput(inputId = "method_det_3d", label = "Detection method", choices = list("ELISA" = "ELISA aflatoxin")),
      numericInput(inputId = "Mc", label = "Mc (ppb)", value = 20, min = 0.001)
    ),
    h3(),
    h2("Iteration section"),
    splitLayout(
      numericInput(inputId = "n_seed_3d", label = "Number of contamination patterns", value = 1, min = 1, step = 1),
      numericInput(inputId = "n_iter_3d", label = "Number of sampling patterns per contamination pattern", value = 1, min = 1, step = 1)
    ),
    fluidRow(
      column(width = 10, 
             selectInput(inputId = "n_vars_3d", label = "Number of tuning parameters", choices = list(0,1,2), multiple = FALSE),style = "padding-right: 0px;"),
      column(width = 2, 
             actionButton("Doc_Tuning", "",style = "padding: 5px; margin: 26px 0px 0px 0px;",icon = icon("question-circle") )
      )), 
    
    uiOutput(outputId = "ui_tuning_3d"),
    actionButton(inputId = "load_3d", label = "Load parameters"),
    h2(),
    conditionalPanel(
      condition = "input.load_3d > 0",
      splitLayout(
        actionButton(inputId = "vis_3d", label = "Visualize"),
        actionButton(inputId = "iterate_3d", label = "Iterate")
      )
    )
  ),
  box(
    title = "Visualization for one iteration",
    plotOutput(outputId = "overlay_top"),
    plotOutput(outputId = "overlay_side")
  )
)

# Place holder
ph = p("Under development")

# Manual version for 2D
v_manual_2D = fluidRow(
  
  box(title = "2D Input Parameters", 
      
      fluidRow(
        column(width = 10, 
               actionButton("TwoD_Scenario", "Click for Automatic Scenario"), style = "padding-right: 5px;"),
        column(width = 2, 
               actionButton("Doc_Scenario", "",icon = icon("question-circle"), 
                            style = "padding: 5px; margin: 0px 0px 0px 0px;" )
        )),
      
      fluidRow(
        column(width = 10, 
               splitLayout(
                 numericInput(inputId = "x_lim", label = "Length (m)", value = 10, min = 1),
                 numericInput(inputId = "y_lim", label = "Width (m)", value = 10, min = 1)
                 ), style = "padding-right: 0px;"),
      column(width = 2, 
             actionButton("Doc_Field_Man", "",icon = icon("question-circle"), 
                          style = "padding: 5px; margin: 26px 0px 0px 0px;" )
             )),
      
      fluidRow(
        column(width = 10, 
               selectInput(inputId = "geom", label = "Geometry", choices = list("Point-source" = "point", "Area-based" = "area"), multiple = FALSE), style = "padding-right: 0px;"),
        column(width = 2, 
               actionButton("Doc_Geometry_Man", "",style = "padding: 5px; margin: 26px 0px 0px 0px;",icon = icon("question-circle") ),
        )),
      fluidRow(
        column(width = 10, 
               conditionalPanel(
                 condition = "input.geom == 'point'",
                 numericInput(inputId = "n_contam", label = "Number of contamination points", value = 1, min = 1, step = 1),
                 numericInput(inputId = "spread_radius", label = "Radius of contamination area (m)", value = 1, min = 0)
               ),style = "padding-right: 0px;"),
        column(width = 2, 
               actionButton("Doc_Radius_Man", "",style = "padding: 5px; margin: 26px 0px 0px 0px;",icon = icon("question-circle") )
        )), 
      fluidRow(
        column(width = 10, 
               splitLayout(
                 numericInput(inputId = "cont_level_mu", label = "Mean contamination level (log CFU/g)", value = 3),
                 numericInput(inputId = "cont_level_sd", label = "Standard deviation of contamination level (log CFU/g)", value = 1)
               ),style = "padding-right: 0px;"),
        column(width = 2, 
               actionButton("Doc_Contlevels_Man", "",style = "padding: 5px; margin: 26px 0px 0px 0px;",icon = icon("question-circle") )
        )), 
      fluidRow(
        column(width = 10, 
               numericInput(inputId = "bg_level", label = "Background level (CFU/g)", value = 0.00001, min = 0),style = "padding-right: 0px;"),
        column(width = 2, 
               actionButton("Doc_Backlevels_Man", "",style = "padding: 5px; margin: 26px 0px 0px 0px;",icon = icon("question-circle") )
        )), 
      fluidRow(
        column(width = 10, 
               selectInput(inputId = "fun", label = "Decay function", choices = list("Exponential" = "exp", "Gaussian" = "norm", "Uniform" = "unif")),style = "padding-right: 0px;"),
        column(width = 2, 
               actionButton("Doc_Decay_Man", "",style = "padding: 5px; margin: 26px 0px 0px 0px;",icon = icon("question-circle") )
        )), 
      fluidRow(
        column(width = 10, 
               conditionalPanel(
                 condition = "input.fun != 'unif'",
                 numericInput(inputId = "LOC", label = "Limit of contamination contribution (0 - 1)", value = 0.001, min = 0, max = 1)
               ),style = "padding-right: 0px;"),
        column(width = 2, 
               actionButton("Doc_LimitCont_Man", "",style = "padding: 5px; margin: 26px 0px 0px 0px;",icon = icon("question-circle") )
        )), 
      fluidRow(
        column(width = 10, 
               numericInput(inputId = "n_sp", label = "Number of sample points", value = 5, min = 1, step = 1),style = "padding-right: 0px;"),
        column(width = 2, 
               actionButton("Doc_Samplepoints_Man", "",style = "padding: 5px; margin: 26px 0px 0px 0px;",icon = icon("question-circle") )
        )), 
      
      fluidRow(
        column(width = 10, 
               selectInput(inputId = "method_sp", label = "Sampling strategy", choices = list("SRS" = "srs", "STRS" = "strs", "k-step SS" = "ss")),style = "padding-right: 0px;"),
        column(width = 2, 
               actionButton("Doc_SamplingStrat_Man", "",style = "padding: 5px; margin: 26px 0px 0px 0px;",icon = icon("question-circle") )
        )), 
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
      sliderInput(inputId = "case", label = "Case", min = 1, value = 10, max = 15, step = 1, round = TRUE),
      splitLayout(
        numericInput(inputId = "m", label = "m", value = 0, min = 0),
        numericInput(inputId = "M", label = "M", value = 0, min = 0)
      ),
      numericInput(inputId = "m_sp", label = "Individual sample mass (g)", value = 25, min = 0),
      selectInput(inputId = "method_det",
                  label = "Detection method",
                  choices = list("Plating" = "plating", "Enrichment" = "enrichment"),
                  selected = "enrichment",
                  multiple = FALSE),
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
          actionButton(inputId = "iterate", label = "Iterate")
        )
      )
  ),
  
  box(title = "Visualization for one iteration",
      plotOutput(outputId = "overlay_draw"),
      plotOutput(outputId = "contam_level_draw")
  )
)

# Smart version 
v_smart = fluidPage(
  fluidRow(
    box(title = "Questionnaire", 
        p("Q1: Which type of product do you want to simulate?"),
        radioButtons(inputId = "spread_vs",
                     label = NULL,
                     selected = character(0),
                     inline = TRUE,
                     choiceNames = list("Produce in a field", "Grains in a bin"),
                     choiceValues = list("continuous", "discrete")),
        conditionalPanel(condition = "input.spread_vs == 'continuous'", 
                         uiOutput(outputId = "ui_dims"),
                         uiOutput(outputId = "ui_geom"),
                         uiOutput(outputId = "ui_contam"),
                         uiOutput(outputId = "ui_sp"),
                         uiOutput(outputId = "ui_method_sp"),
                         uiOutput(outputId = "ui_assay"),
                         uiOutput(outputId = "ui_iter"),
                         uiOutput(outputId = "ui_tuning_vs"),
                         uiOutput(outputId = "ui_load"),
                         uiOutput(outputId = "ui_vis_iter")),
        conditionalPanel(condition = "input.spread_vs == 'discrete'",
                         uiOutput(outputId = "ui_dims_3d"),
                         uiOutput(outputId = "ui_grain_3d"),
                         uiOutput(outputId = "ui_contam_3d"),
                         uiOutput(outputId = "ui_n_affected_3d"),
                         uiOutput(outputId = "ui_sp_3d"),
                         uiOutput(outputId = "ui_method_sp_3d"),
                         uiOutput(outputId = "ui_assay_3d"),
                         uiOutput(outputId = "ui_iter_3d"),
                         uiOutput(outputId = "ui_tuning_3d_vs"),
                         uiOutput(outputId = "ui_load_3d"),
                         uiOutput(outputId = "ui_vis_iter_3d"))
        ),
    
    box(title = "Visualization for one iteration", 
        uiOutput(outputId = "ui_vis_once")
        )
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
