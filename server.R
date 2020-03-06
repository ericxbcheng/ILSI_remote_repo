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

shinyServer(function(input, output) {
  
  observe(x = {print(input$sidebarMenu)})
  observe(x = {print(input$n_contam)})
  
  # observeEvent(eventExpr = {input$v_manual == "2D"}, handlerExpr = {
  #     
  #     # output$debug = renderPrint(expr = input$v_manual)
  #     
  #     ArgList_default = reactive(x = {list(n_contam = input$n_contam, lims = list(x_lim = input$x_lim, y_lim = input$y_lim), 
  #                                          spread = "continuous", spread_radius = input$spread_radius, 
  #                                          cont_level = c(input$cont_level_mu, input$cont_level_sd), method_sp = input$method_sp, 
  #                                          n_sp = input$n_sp, n_strata = input$n_strata, by = input$by, LOC = input$LOC, 
  #                                          fun = input$fun, case = input$case, m = input$m, M = input$M, m_sp = input$m_sp, 
  #                                          method_det = input$method_det, bg_level = input$bg_level, geom = input$geom)})
  #     
  #     # Remove unnecessary arguments
  #     ArgList_vis = reactive(x = ArgList_default()) 
  #     reactive(x = {ArgList_vis()[c("case", "M", "m_sp", "method_det")] = NULL})
  #     reactive(x = {ArgList_vis()$seed = NaN})
  #     
  #     # Produce intermediate outputs
  #     one_iteration = reactive(x = {do.call(what = sim_intmed, args = ArgList_vis())})
  #   })
  
  # observeEvent(eventExpr = input$vis, handlerExpr = {
  #   output$debug2 = renderPrint(expr = "Visualization")
  #   # output$contam_sp_xy = renderPlot(expr = {overlay_draw(method_sp = input$method_sp, data = one_iteration()[["contam_sp_xy"]] ,
  #   #                                                                    spread = "continuous", xlim = input$x_lim, ylim = input$y_lim, 
  #   #                                                                    n_strata = input$n_strata, by = input$by)})
  # })
  
})
