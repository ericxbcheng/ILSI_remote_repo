# A function for visualizing one iteration
vis_once = function(input, output, ArgList, chosen_mode){

  if(ArgList$spread == "continuous"){

    if (chosen_mode == "1D") {
      vis_once_1d(output = output, ArgList = ArgList, chosen_mode = chosen_mode)
    } else {
      vis_once_2d(output = output, ArgList = ArgList, chosen_mode = chosen_mode)
    }

    # vis_once_2d(output = output, ArgList = ArgList, chosen_mode = chosen_mode)
    #vis_once_2d(output = output, ArgList = ArgList, chosen_mode = chosen_mode)

  } else if (ArgList$spread == "discrete"){
    vis_once_3d(output = output, ArgList = ArgList, chosen_mode = chosen_mode)

  } else {
    stop("Unknown spread type")
  }
}


#RUBEN ADDED
# Visualizing once for 1D mode
vis_once_1d = function(output, ArgList, chosen_mode){

  # Remove unnecessary arguments
  ArgList_vis = ArgList
  ArgList_vis[c("case", "M", "m_sp", "method_det")] = NULL
  ArgList_vis$seed = NaN

  # Produce intermediate outputs
  one_iteration = do.call(what = sim_intmed, args = ArgList_vis)

  # Plot the overlay figure
  fig_overlay_1D = overlay_draw(method_sp = ArgList_vis$method_sp, data = one_iteration[["contam_sp_xy"]],
                                spread = ArgList_vis$spread, xlim = ArgList_vis$lims$xlim,
                                ylim = ArgList_vis$lims$ylim, n_strata = ArgList_vis$n_strata, by = ArgList_vis$by)

  # Plot the 3D contamination level figure
  ### Must use expr() to wrap contam_level_draw() because the graph is not an object but a side-effect
  fig_contam_level_1D = expr(contam_level_draw(dimension = "3d", method = ArgList_vis$fun,
                                               spread_radius = ArgList_vis$spread_radius, LOC = ArgList_vis$LOC,
                                               df_contam = one_iteration[["contam_sp_xy"]], xlim = ArgList_vis$lims$xlim,
                                               ylim = ArgList_vis$lims$ylim, bg_level = ArgList_vis$bg_level,
                                               geom = ArgList_vis$geom))

  if(chosen_mode != "v_smart"){
    output$overlay_draw_1D = renderPlot(expr = {fig_overlay_1D})
    output$contam_level_draw_1D = renderPlot(expr = {eval(fig_contam_level_1D)})

  } else {
    output$overlay_draw_vs = renderPlot(expr = {fig_overlay})
    output$contam_level_draw_vs = renderPlot(expr = {eval(fig_contam_level_1D)})
  }
}

# Visualizing once for 2D mode
vis_once_2d = function(output, ArgList, chosen_mode){

  # Remove unnecessary arguments
  ArgList_vis = ArgList
  ArgList_vis[c("case", "M", "m_sp", "method_det")] = NULL
  ArgList_vis$seed = NaN

  # Produce intermediate outputs
  one_iteration = do.call(what = sim_intmed, args = ArgList_vis)

  # Plot the overlay figure
  fig_overlay = overlay_draw(method_sp = ArgList_vis$method_sp, data = one_iteration[["contam_sp_xy"]],
                       spread = ArgList_vis$spread, xlim = ArgList_vis$lims$xlim,
                       ylim = ArgList_vis$lims$ylim, n_strata = ArgList_vis$n_strata, by = ArgList_vis$by)

  # Plot the 3D contamination level figure
  ### Must use expr() to wrap contam_level_draw() because the graph is not an object but a side-effect
  fig_contam_level = expr(contam_level_draw(dimension = "3d", method = ArgList_vis$fun,
                                       spread_radius = ArgList_vis$spread_radius, LOC = ArgList_vis$LOC,
                                       df_contam = one_iteration[["contam_sp_xy"]], xlim = ArgList_vis$lims$xlim,
                                       ylim = ArgList_vis$lims$ylim, bg_level = ArgList_vis$bg_level,
                                       geom = ArgList_vis$geom))

  if(chosen_mode != "v_smart"){
    output$overlay_draw = renderPlot(expr = {fig_overlay})
    output$contam_level_draw = renderPlot(expr = {eval(fig_contam_level)})

  } else {
    output$overlay_draw_vs = renderPlot(expr = {fig_overlay})
    output$contam_level_draw_vs = renderPlot(expr = {eval(fig_contam_level)})
  }
}

# Visualizing once for 3D model
vis_once_3d = function(output, ArgList, chosen_mode){

  # Remove unnecessary arguments
  ArgList_vis = ArgList
  ArgList_vis[c("Mc", "method_det", "verbose")] = NULL
  ArgList_vis$seed = NaN

  # Produce intermediate outputs
  one_iteration = do.call(what = sim_intmed, args = ArgList_vis)

  # Plot the overview
  fig_overlay_top = overlay_draw(method_sp = ArgList_vis$method_sp,
                                 data = one_iteration[["contam_sp_xy"]]$combined ,
                                 spread = ArgList_vis$spread, xlim = ArgList_vis$lims$xlim,
                                 ylim = ArgList_vis$lims$ylim, n_strata = ArgList_vis$n_strata,
                                 by = ArgList_vis$by)

  # Plot the sideview
  fig_overlay_side = overlay_draw_probe(data = one_iteration[["contam_sp_xy"]]$combined,
                                        lims = ArgList_vis$lims, L = ArgList_vis$L)

  # Choose between manual and smart mode
  if(chosen_mode != "v_smart"){
    output$overlay_top = renderPlot(expr = {fig_overlay_top})
    output$overlay_side = renderPlot(expr = {fig_overlay_side})

  } else {
    output$overlay_top_vs = renderPlot(expr = {fig_overlay_top})
    output$overlay_side_vs = renderPlot(expr = {fig_overlay_side})
  }

}

# Visualize multiple iterations for 2D mode (smart + manual)
vis_n_tune2_2d = function(input, output, data, chosen_mode){
  observeEvent(eventExpr = {input$yvar}, handlerExpr = {
    output$plot_iterate = renderPlot(expr = {
      plot_tune2_boxplot_gui(data = data, input = input,
                             yvar = input$yvar, chosen_mode = chosen_mode)

    })
  })
}

# Visualize multiple iterations for 3D mode (smart + manual)
vis_n_tune2_3d = function(input, output, data, chosen_mode){
  output$plot_iterate = renderPlot(expr = {
    plot_tune2_ribbon_gui(data = data, input = input,
                          chosen_mode = chosen_mode)

  })
}

# Visualization for multiple iterations and tuning
vis_n = function(data, input, output, chosen_mode){
  if(data$n_vars == 0){
    output$plot_iterate = renderPlot(expr = {
      plot_tune0(data = data$data_cleaned)
    })

  } else if (data$n_vars == 1){
    output$plot_iterate = renderPlot(expr = {
      plot_tune1_gui(data = data$data_cleaned, input = input, chosen_mode = chosen_mode)
    })

  } else if (data$n_vars == 2){

    if(chosen_mode == "2D"){
      vis_n_tune2_2d(input = input, output = output, data = data$data_cleaned, chosen_mode = chosen_mode)

    } else if (chosen_mode == "3D"){
      vis_n_tune2_3d(input = input, output = output, data = data$data_cleaned, chosen_mode = chosen_mode)

    } else if (chosen_mode == "v_smart"){

      if(input$spread_vs == "continuous"){
        vis_n_tune2_2d(input = input, output = output, data = data$data_cleaned, chosen_mode = chosen_mode)

      } else if (input$spread_vs == "discrete"){
        vis_n_tune2_3d(input = input, output = output, data = data$data_cleaned, chosen_mode = chosen_mode)

      } else {
        stop("Unknown spread type.")
      }
    } else {
      stop("Unknown chosen mode. Please select 2D, 3D, more smart mode.")
    }
  } else {
    stop("Unknown number of tuning vars")
  }
}

# # Plot when there is one tuning parameter
plot_tune1_gui = function(data, input, chosen_mode){

  if(chosen_mode == "2D"){
    xlab = explain_var(var = input$var_prim)
  } else if (chosen_mode == "3D"){
    xlab = explain_var(var = input$var_prim_3d)
  } else if(chosen_mode == "v_smart"){

    if(input$spread_vs == "continuous"){
      xlab = explain_var(var = input$var_prim_vs)
    } else if (input$spread_vs == "discrete"){
      xlab = explain_var(var = input$var_prim_3d_vs)
    } else {
      stop("Unknown spread type")
    }
  } else {
    stop("Unknown chosen mode")
  }

  # Create a plot
  return(plot_tune1(data = data, xlab = xlab))
}

# Plot when there is one tuning parameter
plot_tune2_ribbon_gui = function(data, input, chosen_mode){

  # Make the x axis and legend labels
  if(chosen_mode == "2D"){
    xlab = explain_var(var = input$var_prim)
    legend_lab = explain_var(var = input$var_sec)

  } else if (chosen_mode == "3D"){
    xlab = explain_var(var = input$var_prim_3d)
    legend_lab = explain_var(var = input$var_sec_3d)

  } else if(chosen_mode == "v_smart"){

    if(input$spread_vs == "continuous"){
      xlab = explain_var(var = input$var_prim_vs)
      legend_lab = explain_var(var = input$var_sec_vs)

    } else if (input$spread_vs == "discrete"){
      xlab = explain_var(var = input$var_prim_3d_vs)
      legend_lab = explain_var(var = input$var_sec_3d_vs)

    } else {
      stop("Unknown spread type")
    }
  } else {
    stop("Unknown chosen mode")
  }

  # Create a ribbon plot
  return(plot_tune2_ribbon(data = data, xlab = xlab, legend_lab = legend_lab))
}

# Visualize with boxplots
plot_tune2_boxplot_gui = function(data, input, yvar, chosen_mode){

  # Make the x axis and legend labels
  if(chosen_mode == "2D"){
    xlab = explain_var(var = input$var_prim)
    legend_lab = explain_var(var = input$var_sec)

  } else if (chosen_mode == "3D"){
    xlab = explain_var(var = input$var_prim_3d)
    legend_lab = explain_var(var = input$var_sec_3d)

  } else if(chosen_mode == "v_smart"){

    if(input$spread_vs == "continuous"){
      xlab = explain_var(var = input$var_prim_vs)
      legend_lab = explain_var(var = input$var_sec_vs)

    } else if (input$spread_vs == "discrete"){
      xlab = explain_var(var = input$var_prim_3d_vs)
      legend_lab = explain_var(var = input$var_sec_3d_vs)

    } else {
      stop("Unknown spread type")
    }
  } else {
    stop("Unknown chosen mode")
  }

  # Create boxplots
  return(plot_tune2_boxplot(data = data, xlab = xlab, legend_lab = legend_lab, yvar = yvar))
}
