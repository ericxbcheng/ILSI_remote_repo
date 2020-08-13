#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinydashboard)
library(shinyjs)

source(file = "Sampling_libraries.R")
source(file = "Sampling_contamination.R")
source(file = "Sampling_contamination_3d.R")
source(file = "Sampling_visualization.R")
source(file = "Sampling_assay_prep.R")
source(file = "Sampling_plan.R")
source(file = "Sampling_plan_3d.R")
source(file = "Sampling_assay_3d.R")
source(file = "Sampling_assay.R")
source(file = "Sampling_outcome_3d.R")
source(file = "Sampling_outcome.R")
source(file = "Sampling_iteration.R")
source(file = "Sampling_tuning_3d.R")
source(file = "Sampling_analysis.R")
source(file = "R/Sampling_shiny_loading.R")
source(file = "R/Sampling_shiny_tuning.R")
source(file = "R/Sampling_shiny_visualization.R")
source(file = "R/Sampling_shiny_contam.R")
source(file = "R/Sampling_shiny_plan.R")
source(file = "R/Sampling_shiny_assay.R")
source(file = "R/Sampling_shiny_iteration.R")

shinyServer(function(input, output, session) {

  # Manual version 1D
  source(file = "R/Server_manual_1d.R", local = TRUE)


  # Manual version 2D
  source(file = "R/Server_manual_2d.R", local = TRUE)

  # Manual version 3D
  source(file = "R/Server_manual_3d.R", local = TRUE)

  # Smart version 2D
  source(file = "R/Server_smart_2d.R", local = TRUE)

  # Smart version 3D
  source(file = "R/Server_smart_3d.R", local = TRUE)

  # Modal windows documentation
  source(file = "R/Documentation_Pop_ups.R", local = TRUE)

  # Modal windows documentation
  source(file = "R/Scenario_Buttons.R", local = TRUE)

  # Download
  output$downloadData = downloadHandler(
    filename = "simulation.csv",
    content = function(file){
      write.csv(x = result_iter$data_cleaned, file)
    },
    contentType = "text/csv"
  )
})
