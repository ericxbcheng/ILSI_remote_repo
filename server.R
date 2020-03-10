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

vis_once = function(input, output, spread, ArgList){
  
  if(spread == "continuous"){
    
    # Remove unnecessary arguments
    ArgList_vis = ArgList
    ArgList_vis[c("case", "M", "m_sp", "method_det")] = NULL
    ArgList_vis$seed = NaN
    print(ArgList_vis)
    # Produce intermediate outputs
    one_iteration = do.call(what = sim_intmed, args = ArgList_vis)
    output$overlay_draw = renderPlot(expr = {overlay_draw(method_sp = ArgList_vis$method_sp, data = one_iteration[["contam_sp_xy"]] , 
                                                          spread = ArgList_vis$spread, xlim = ArgList_vis$lims$xlim, ylim = ArgList_vis$lims$ylim, 
                                                          n_strata = ArgList_vis$n_strata, by = ArgList_vis$by)})
    output$contam_level_draw = renderPlot(expr = {contam_level_draw(dimension = "3d", method = ArgList_vis$fun, 
                                                                    spread_radius = ArgList_vis$spread_radius, LOC = ArgList_vis$LOC, 
                                                                    df_contam = one_iteration[["contam_sp_xy"]] , xlim = ArgList_vis$lims$xlim, 
                                                                    ylim = ArgList_vis$lims$ylim, bg_level = ArgList_vis$bg_level, 
                                                                    geom = ArgList_vis$geom)})
    
  } else {
    message("Under construction.") 
  }
}

load_once = function(input, output){
  
  if(input$sidebarMenu == "2D"){
    spread = "continuous"
    
    ArgList_default = list(n_contam = input$n_contam, lims = list(xlim = c(0, input$x_lim), ylim = c(0, input$y_lim)),
                           spread = spread, spread_radius = input$spread_radius,
                           cont_level = c(input$cont_level_mu, input$cont_level_sd), method_sp = input$method_sp,
                           n_sp = input$n_sp, n_strata = input$n_strata, by = input$by, LOC = input$LOC,
                           fun = input$fun, case = input$case, m = input$m, M = input$M, m_sp = input$m_sp,
                           method_det = input$method_det, bg_level = input$bg_level, geom = input$geom)
    
  } else if(input$sidebarMenu == "3D"){
    spread = "discrete"
  } else {
    stop("Unknown manual mode")
  }
  return(list(spread = spread, ArgList_default = ArgList_default))
}

# Iterate the model without any tuning parameters
iterate_tune0 = function(input, output, Args){
  
  output$data_raw = sim_iterate2(n_seed = input$n_seed, n_iter = input$n_iter, Args = Args)
  
}

# Parse the string "a,b,c" from val_prim into a numeric vector c(a,b,c)
parse_num_vec = function(string){
  return(str_split(string = string, pattern = ",") %>%
           unlist() %>%
           as.numeric())
}

# Iterate the model with one tuning parameter
iterate_tune1 = function(input, output, Args){
  
  vals = parse_num_vec(string = input$val_prim)
  
  output$data_raw = map(.x = vals, .f = tune_param, Args = ArgList_default, n_seed = input$n_seed, n_iter = input$n_iter, param = input$var_prim)
  
}



shinyServer(function(input, output) {
  
  a = list()
  
  # Load the parameter once
  observeEvent(eventExpr = {input$load}, handlerExpr = {
    a <<- load_once(input = input, output = output)
    output$print_param = renderPrint(expr = a$ArgList_default)
  })
  
  # Visualize for one iteration
  observeEvent(eventExpr = {input$vis}, handlerExpr = {
    vis_once(input = input, output = output, spread = a$spread, ArgList = a$ArgList_default)
  })
  
  observeEvent(eventExpr = {input$val_prim},handlerExpr = {
    b = parse_num_vec(string = input$val_prim)
    print(b)
  })
})