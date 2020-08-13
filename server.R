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
<<<<<<< Updated upstream

vis_once = function(input, output, spread, ArgList){
  
  if(spread == "continuous"){
    
    # Remove unnecessary arguments
    ArgList_vis = ArgList
    ArgList_vis[c("case", "M", "m_sp", "method_det")] = NULL
    ArgList_vis$seed = 123
    
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

#VISUALIZE FUNCTION IN 1D
vis_once_1D = function(input, output, spread, ArgList){
  
  if(spread == "continuous"){
    
    # Remove unnecessary arguments
    ArgList_vis = ArgList
    ArgList_vis[c("case", "M", "m_sp", "method_det")] = NULL
    ArgList_vis$seed = 123
    
    # Produce intermediate outputs
    one_iteration = do.call(what = sim_intmed, args = ArgList_vis)
    output$overlay_draw_1D = renderPlot(expr = {overlay_draw(method_sp = ArgList_vis$method_sp, data = one_iteration[["contam_sp_xy"]] , 
                                                          spread = ArgList_vis$spread, xlim = ArgList_vis$lims$xlim, ylim = ArgList_vis$lims$ylim, 
                                                          n_strata = ArgList_vis$n_strata, by = ArgList_vis$by)})
    output$contam_level_draw_1D = renderPlot(expr = {contam_level_draw(dimension = "3d", method = ArgList_vis$fun, 
                                                                    spread_radius = ArgList_vis$spread_radius, LOC = ArgList_vis$LOC, 
                                                                    df_contam = one_iteration[["contam_sp_xy"]] , xlim = ArgList_vis$lims$xlim, 
                                                                    ylim = ArgList_vis$lims$ylim, bg_level = ArgList_vis$bg_level, 
                                                                    geom = ArgList_vis$geom)})
    
  } else {
    message("Under construction.") 
  }
}



#TESTING
load_once_1D = function(input, output){
  
  if(input$sidebarMenu == "1D"){
    spread = "continuous"
    
    if(input$by_1D == "1d"){
      n_strata = c(input$n_strata_row_1D, input$n_strata_col_1D)
    } else {
      n_strata = input$n_strata_1D
    }
    
    ArgList_default = list(n_contam = input$n_contam_1D, lims = list(xlim = c(0, input$x_lim_1D), ylim = c(0, input$y_lim_1D)),
                           spread = spread, spread_radius = input$spread_radius_1D,
                           cont_level = c(input$cont_level_mu_1D, input$cont_level_sd_1D), method_sp = input$method_sp_1D,
                           n_sp = input$n_sp_1D, n_strata = n_strata, by = input$by_1D, LOC = input$LOC_1D,
                           fun = input$fun_1D, case = input$case_1D, m = input$m_1D, M = input$M_1D, m_sp = input$m_sp_1D,
                           method_det = input$method_det_1D, bg_level = input$bg_level_1D, geom = input$geom_1D)
    
  } else if(input$sidebarMenu == "3D"){
    spread = "discrete"
  } else {
    stop("Unknown manual mode")
  }
  return(list(spread = spread, ArgList_default = ArgList_default))
}

load_once = function(input, output){
  
  if(input$sidebarMenu == "2D"){
    spread = "continuous"
    
    if(input$by == "2d"){
      n_strata = c(input$n_strata_row, input$n_strata_col)
    } else {
      n_strata = input$n_strata
    }
    
    ArgList_default = list(n_contam = input$n_contam, lims = list(xlim = c(0, input$x_lim), ylim = c(0, input$y_lim)),
                           spread = spread, spread_radius = input$spread_radius,
                           cont_level = c(input$cont_level_mu, input$cont_level_sd), method_sp = input$method_sp,
                           n_sp = input$n_sp, n_strata = n_strata, by = input$by, LOC = input$LOC,
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
iterate_tune0 = function(input, Args){
  
  return(sim_iterate2(n_seed = input$n_seed, n_iter = input$n_iter, Args = Args))
  
}

# Parse the string "a,b,c" from val_prim into a numeric vector c(a,b,c)
parse_num_vec = function(string){
  return(str_split(string = string, pattern = ",") %>%
           unlist() %>%
           as.numeric())
}

# Iterate the model with one tuning parameter
iterate_tune1 = function(input, Args){
  
  vals = parse_num_vec(string = input$val_prim)
  
  return(map(.x = vals, .f = tune_param, Args = Args, n_seed = input$n_seed, n_iter = input$n_iter, param = input$var_prim))
  
}

plot_tune0 = function(data){
  
  # Calculate the acceptance prob
  a = calc_Prej_one(data) %>%
    tibble(seed = as.numeric(names(.)), Paccept = 1 - .)
  
  ggplot(data = a) +
    geom_boxplot(aes(x = "NA", y = Paccept)) +
    geom_point(aes(x = "NA",  y = mean(a$Paccept)), color = "red", pch = 4, size = 5) +
    labs(x = NULL, y = "Probability of acceptance") +
    scale_y_continuous(breaks = seq(0,1,0.1)) +
    coord_cartesian(ylim = c(0,1)) +
    theme_bw()
}

plot_tune1 = function(data, input){
  
  if(input$var_prim == "n_contam"){
    xlab = "Number of contamination points"
    
  } else if(input$var_prim == "n_sp"){
    xlab = "Number of sample points"
    
  } else if(input$var_prim == "m_sp"){
    xlab = "Individual sample mass (g)"
    
  } else {
    stop("Unknown primary tuning parameter")
  }
  
  # Summarise the data
  a = data %>%
    gather(data = ., key = "metric", value = "value", -c(seed, param)) %>%
    group_by(param, metric) %>%
    summarise(lb = quantile(x = value, probs = 0.025), 
              med = median(x = value),
              ub = quantile(x = value, probs = 0.975)) %>%
    dplyr::filter(metric == "Paccept")
  
  # Visualize
  b = ggplot(data = a) +
    geom_ribbon(aes_string(x = "param", ymin = "lb", ymax = "ub"), alpha = 0.3, color = "lightgrey") +
    geom_line(aes_string(x = "param", y = "med")) +
    geom_point(aes_string(x = "param", y = "med")) +
    scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.1)) +
    coord_cartesian(ylim = c(0,1)) +
    labs(x = xlab, y = "Probability of acceptance (2.5th - 97.5th percentile)") +
    theme_bw() 
  return(b)
}

shinyServer(function(input, output) {
  
  list_load = list()
  
  # Load the parameter once
  observeEvent(eventExpr = {input$load}, handlerExpr = {
    list_load <<- load_once(input = input, output = output)
    output$print_param = renderPrint(expr = list_load$ArgList_default)
  })
  
  #LOAD 1D
  observeEvent(eventExpr = {input$load_1D}, handlerExpr = {
    list_load <<- load_once_1D(input = input, output = output)
    output$print_param = renderPrint(expr = list_load$ArgList_default)
  })
  
  # Visualize for one iteration
  observeEvent(eventExpr = {input$vis}, handlerExpr = {
    vis_once(input = input, output = output, spread = list_load$spread, ArgList = list_load$ArgList_default)
  })
  
  #VISUALIZE 1D
  observeEvent(eventExpr = {input$vis_1D}, handlerExpr = {
    vis_once_1D(input = input, output = output, spread = list_load$spread, ArgList = list_load$ArgList_default)
  })
  
  observeEvent(eventExpr = {input$iteration}, handlerExpr = {
    
    if(input$n_vars == 0){
      
      # When there is no tuning parameter
      data_raw = iterate_tune0(input = input, Args = list_load$ArgList_default)
      output$plot_iterate = renderPlot(expr = {plot_tune0(data = data_raw)})
      
    } else if (input$n_vars == 1){
      
      # When there is 1 tuning parameter
      data_raw = iterate_tune1(input = input, Args = list_load$ArgList_default)
      data_cleaned = metrics_cont_n(data = data_raw)
      output$plot_iterate = renderPlot(expr = {plot_tune1(data = data_cleaned, input = input)})
      
    } else {
      
      message("Under construction")
      
    }
  })
=======
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
  
  # Download
  output$downloadData = downloadHandler(
    filename = "simulation.csv",
    content = function(file){
      write.csv(x = result_iter$data_cleaned, file)
    },
    contentType = "text/csv"
  )
>>>>>>> Stashed changes
})