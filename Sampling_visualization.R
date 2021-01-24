######################## Contamination #########################################

# Contamination plots
contam_draw = function(data, spread, xlim, ylim){
  if (spread == "discrete") {
    ggplot() +
      geom_point(data = data, aes(x = X, y = Y, shape = label, color = dis_level)) +
      scale_color_gradient(name = "Contamination Level", low = "coral", high = "red4") +
      coord_fixed(ratio = 1, xlim = xlim, ylim = ylim) +
      theme_bw()
  } else if (spread == "continuous") {
    ggplot() +
      geom_point(data = subset(data, subset = data$label == "spot"),
                 aes(x = X, y = Y, shape = label, color = cont_level)) +
      scale_color_gradient(name = "Contamination Level", low = "coral", high = "red4") +
      geom_circle(
        data = subset(data, subset = data$label == "spot"),
        aes(x0 = X, y0 = Y, r = r), fill = "coral", alpha = 0.1) +
      coord_fixed(ratio = 1, xlim = xlim, ylim = ylim) +
      theme_bw() 
  }
}

## Wrap-up funciton for plotting contamination level
contam_level_draw = function(dimension, method, spread_radius, LOC, df_contam, xlim, ylim, geom,bg_level,  ...){
  if(dimension == "2d"){
    contam_level_draw_2d(method = method, spread_radius = spread_radius, LOC = LOC)
    
  } else if (dimension == "3d"){
    contam_level_draw_3d(method = method, df_contam = df_contam, spread_radius = spread_radius, 
                         LOC = LOC, xlim = xlim, ylim = ylim, geom = geom, bg_level, ...)
    
  } else {
    stop("we do not support this dimension. Please choose '2d' or '3d'.")
  }
}

## Draw the contamination level plot for continuous spread in a 2D plane.
contam_level_draw_2d = function(method, spread_radius, LOC){
  
  stopifnot(fun %in% c("exp", "norm", "unif"))
  
  f_chosen = switch(EXPR = method,
                    "exp" = f_exp,
                    "norm" = f_norm,
                    "unif" = f_unif)
  
  ggplot(data = data.frame(x = c(0, spread_radius)), aes(x = x))+
    geom_line(stat = "function", fun = f_chosen, args = list(spread_radius = spread_radius, LOC = LOC)) +
    coord_cartesian(ylim = c(0,1)) +
    labs(x = "Distance from a contamination source", y = "Contamination contribution") +
    theme_bw()
}

## Draw thw contamination level plot for continuous spread in a 3D space
contam_level_draw_3d = function(method, df_contam, xlim, ylim, spread_radius, LOC, bg_level, geom, interactive = FALSE){
  
  ### Extract the coordinates of contamination spots
  spot_coord = df_contam %>%
    dplyr::filter(label == "spot") %>%
    dplyr::select(c(X, Y, cont_level))
  
  if(geom == "area"){
    # -0.1 is for dealing with the bug in scatter3D
    spread_radius = subset(x = df_contam, subset = label == "spot", select = r, drop = TRUE) - 0.1
  }
  
  ### Create a grid that has the dimensions of the field
  a = expand.grid(X = seq(xlim[1],xlim[2],0.1), Y = seq(ylim[1], ylim[2],0.1))
  
  ### Calculate the contamination contribution from each contamination spot on each point in the field and put them in a matrix
  ## Each column is the contribution of one contamination spot to all the locations in the field
  b = mapply(FUN = f_density, spot_coord$X, spot_coord$Y, MoreArgs = list(method = method, x = a$X, y = a$Y, spread_radius = spread_radius, LOC = LOC))
  
  ### Calculate the contamination level distribution from each contamination spot
  I = matrix(data = 1, nrow = nrow(b))
  scalars = I %*% spot_coord$cont_level
  c = scalars * b
  
  ### Calculate the contamination level at each point in the field by summing up the contamination contributed by all sources
  d = rowSums(c)
  a$Z = log10(d + bg_level)
  
  ### Make the 3D plot
  if(interactive == FALSE){
    scatter3D(x = a$X, y = a$Y, z = a$Z, colvar = a$Z, zlab = "log CFU/mL", pch = 16)
  } else {
    plot_ly(x = a$X, y = a$Y, z = a$Z, color = a$Z,type = "scatter3d") 
  }
}

############################### Sampling #################################################

## Simple random sampling
sp_draw_srs = function(data, spread, xlim, ylim, sp_radius){
  if(spread == "discrete"){
    ggplot() +
      geom_point(data = data, aes(x = X, y = Y, color = label, shape = label)) +
      scale_color_manual(values = c("darkgreen")) +
      scale_shape_manual(values = 15) +
      geom_circle(data = data, aes(x0 = X, y0 = Y, r = sp_radius), fill = "darkgreen", alpha = 0.1) +
      coord_fixed(ratio = 1, xlim = xlim, ylim = ylim) +
      theme_bw()
  } else if (spread == "continuous"){
    ggplot() +
      geom_point(data = data, aes(x = X, y = Y, color = label, shape = label)) +
      scale_color_manual(values = c("darkgreen")) +
      scale_shape_manual(values = 15) +
      coord_fixed(ratio = 1, xlim = xlim, ylim = ylim) +
      theme_bw()
  }
}

## Stratified random sampling
sp_draw_strs = function(data, spread, xlim, ylim, n_strata, by, ...){
  base = sp_draw_srs(data = data, spread = spread, xlim = xlim, ylim = ylim, ...)
  
  if(by == "row"){
    bounds = calc_bounds_2d(xlim = xlim, ylim = ylim, n_strata = n_strata, by = by)
    
    base + 
      geom_hline(yintercept = bounds, color = "darkgrey")
  } else if (by == "column"){
    bounds = calc_bounds_2d(xlim = xlim, ylim = ylim, n_strata = n_strata, by = by)
    
    base +
      geom_vline(xintercept = bounds, color = "darkgrey")
  } else if (by == "2d"){
    n_x = n_strata[1]
    n_y = n_strata[2]
    xbounds = calc_bounds_2d(xlim = xlim, ylim = ylim, n_strata = n_x, by = "column")
    ybounds = calc_bounds_2d(xlim = xlim, ylim = ylim, n_strata = n_y, by = "row")
    
    base +
      geom_hline(yintercept = ybounds, color = "darkgrey") +
      geom_vline(xintercept = xbounds, color = "darkgrey")
  } else {
    stop("Unknown sampling strategy.")
  }
}

### A wrapper function
sp_draw = function(method_sp, data, spread, xlim, ylim, n_strata, by, ...){
  if(method_sp %in% c("srs", "ss")){
    sp_draw_srs(data = data, spread = spread, xlim = xlim, ylim = ylim, ...)
  } else if (method_sp == "strs"){
    sp_draw_strs(data = data, spread = spread, xlim = xlim, ylim = ylim, n_strata = n_strata, by = by, ...)
  }
}

##################################### Overlay plots ##################################
## Simple random sampling
overlay_draw_srs = function(data, spread, xlim, ylim){
  if(spread == "discrete"){
    ggplot() +
      geom_point(data = subset(data, subset = label == "sample point"), aes(x = X, y = Y, shape = label), color = "darkgreen") +
      geom_circle(data = subset(x = data, subset = label == "sample point"),
                  aes(x0 = X, y0 = Y, r = r),
                  fill = "darkgreen",
                  alpha = 0.1) +
      geom_point(data = subset(data, subset = label != "sample point"), aes(x = X, y = Y, color = dis_level, shape = label)) +
      scale_color_gradient(name = "Contamination Level (ng/g)", low = "orange", high = "red2") +
      scale_shape_manual(values = c(15, 16, 17)) +
      coord_fixed(ratio = 1, xlim = xlim, ylim = ylim) +
      theme_bw()
  } else if (spread == "continuous"){
    ggplot() +
      geom_point(data = subset(x = data, subset = label == "sample point"),
                 aes(x = X, y = Y,
                     shape = label),
                 color = "darkgreen") +
      geom_point(data = subset(x = data, subset = label == "spot"), 
                 aes(x = X, y = Y, 
                     color = cont_level, 
                     shape = label)) +
      scale_color_gradient(name = "Contamination Level (CFU/g)", low = "orange", high = "red2") +
      scale_shape_manual(values = c(15, 16)) +
      geom_circle(data = subset(x = data, subset = label == "spot"),
                  aes(x0 = X, y0 = Y, r = r), 
                  fill = "coral", 
                  alpha = 0.1) +
      coord_fixed(ratio = 1, xlim = xlim, ylim = ylim) +
      theme_bw()
  }
}

## Stratified random sampling
overlay_draw_strs = function(data, spread, xlim, ylim, n_strata, by){
  base = overlay_draw_srs(data = data, spread = spread, xlim = xlim, ylim = ylim)
  
  if(by == "2d"){
    n_x = n_strata[1]
    n_y = n_strata[2]
    xbounds = calc_bounds_2d(xlim = xlim, ylim = ylim, n_strata = n_x, by = "column")
    ybounds = calc_bounds_2d(xlim = xlim, ylim = ylim, n_strata = n_y, by = "row")
    
    base +
      geom_hline(yintercept = ybounds, color = "darkgrey") +
      geom_vline(xintercept = xbounds, color = "darkgrey")
    
  } else {
    
    bounds = calc_bounds_2d(xlim = xlim, ylim = ylim, n_strata = n_strata, by = by)
    if(by == "row"){
      base + 
        geom_hline(yintercept = bounds, color = "darkgrey")
    } else if (by == "column"){
      base +
        geom_vline(xintercept = bounds, color = "darkgrey")
    }
  }
}

### A wrapper function
overlay_draw = function(method_sp, data, spread, xlim, ylim, n_strata, by){
  if(method_sp %in% c("srs", "ss")){
    overlay_draw_srs(data = data, spread = spread, xlim = xlim, ylim = ylim)
  } else if (method_sp == "strs"){
    overlay_draw_strs(data = data, spread = spread, xlim = xlim, ylim = ylim, n_strata = n_strata, by = by)
  }
}

# A function for drawing X-Z with probes in them
overlay_draw_probe = function(data, lims, L){
  ggplot() +
    geom_rect(data = subset(data, subset = label == "sample point"), 
              aes(xmin = X - r, xmax = X + r, ymin = lims$zlim[2] - L, ymax = lims$zlim[2]), fill = "darkgreen") +
    geom_point(data = subset(data, subset = label != "sample point"), aes(x = X, y = Z, color = dis_level, shape = label)) +
    scale_color_gradient(name = "Contamination Level", low = "orange", high = "red2") +
    scale_shape_manual(values = c(15, 16, 17)) +
    coord_fixed(ratio = 1, xlim = lims$xlim, ylim = lims$zlim) +
    theme_bw()
}

######################## Assay ########################################

# Create a function that draws the contamination level of samples and shows the microbiological criteria in the continuous spread scenario
assay_draw_cont_plating = function(df, M, m, method_det, case){
  
  a = get_attr_plan(case = case, m = m, M = M)
  M1 = a$M
  m1 = a$m
  
  temp1 = subset(x = df, subset = label == "sample point")
  temp2 = data.frame(Thresholds = c("M", "m", "LOD"), 
                    val = c(M1, m1, get_LOD(method_det = method_det)))
  
  ggplot() +
    geom_col(data = temp1, aes(x = ID, y = cont_level), fill = "darkgrey") +
    geom_hline(data = temp2, aes(yintercept = val, color = Thresholds), size = 1.5) +
    geom_text(data = temp1, 
              aes(x = ID, y = cont_level, 
                  label = scientific(cont_level, digits = 2)), 
              position = position_nudge(y = 0.3)) +
    scale_y_log10(label = scientific_format()) +
    scale_color_manual(values = c("#00BA38", "#D89000", "#F8766D" )) +
    labs(x = "Sample point", y = "Contamination level (CFU/g)") +
    theme_bw() 
}

# Draw CFU for each sample and binary results for enrichment
assay_draw_cont_enrichment = function(data, m_sp, m, M, case, method_det){
  
  temp0 = data %>%
    dplyr::filter(label == "sample point") %>%
    dplyr::select(ID, cont_level)
  
  # Run intermediate functions in lot_decision_new()
  attr_plan = get_attr_plan(case = case, m = m, M = M)
  LOD = get_LOD(method_det = method_det)
  bin_conc = conc2bin(method_det = method_det, LOD = LOD, conc = temp0$cont_level, m_sp = m_sp) %>%
    as.numeric()
  
  # Subsetting
  temp1 = temp0 %>%
    mutate(CFU = cont_level * m_sp, bin_conc = bin_conc)
  
  temp2 = data.frame(Thresholds = "LOD", 
                     val = LOD) 
  
  a = ggplot() +
    geom_col(data = temp1, aes(x = ID, y = CFU), fill = "darkgrey") +
    geom_hline(data = temp2, aes(yintercept = val, color = Thresholds), size = 1.5) +
    geom_text(data = temp1, 
              aes(x = ID, y = CFU, 
                  label = scientific(CFU, digits = 2)), 
              position = position_nudge(y = 0.3)) +
    scale_y_log10(label = scientific_format()) +
    scale_color_manual(values = c("#00BA38")) +
    labs(x = "Sample point", y = "CFU") +
    theme_bw() 
  
  b = ggplot(data = temp1) +
    geom_col(aes(x = ID, y = bin_conc)) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = "Sample point", y = "Positive after enrichment?") +
    theme_bw()
  
  return(list(a,b))
}

assay_draw_cont = function(df, M, m, case, m_sp, method_det){
  if(method_det == "plating"){
    assay_draw_cont_plating(df = df, M = M, m = m, method_det = method_det, case = case)
    
  } else if(method_det == "enrichment"){
    assay_draw_cont_enrichment(data = df, m_sp = m_sp, m = m, M = M, case = case, method_det = method_det)
    
  } else {
    stop("Unknown detection method.")
  }
}

## A new function for discrete case
assay_draw_dis_new = function(data, Mc, method_det){
  
  temp1 = temp = tibble(Conc = data) %>%
    mutate(class = ifelse(test = Conc < Mc, yes = "L", no = "H"))
  
  temp2 = data.frame(Thresholds = c("Mc", "LOD"), 
                     val = c(Mc, get_LOD(method_det = method_det)))
  
  ggplot()+
    geom_col(aes(x = "Mean sample concentration", y = mean(temp1$Conc)), width = 0.5, fill = "darkgrey") +
    geom_hline(data = temp2, aes(yintercept = val, color = Thresholds), size = 1.5) +
    geom_text(data = temp1, aes(x = "Mean sample concentration", 
                                y = mean(temp1$Conc), 
                                label = scientific(mean(temp1$Conc), digits = 2)), 
              nudge_y = 0.3) +
    scale_color_manual(values = c("#00BA38", "#F8766D")) +
    labs(x = NULL, y = "Contamination level (ng/g)") +
    theme_bw()
}

# A wrapper function that includes assay_draw_cont() and assay_draw_dis()
assay_draw = function(data, M, m, m_sp, Mc, method_det, spread, case){
  if(spread == "discrete"){
    assay_draw_dis_new(data = data, Mc = Mc, method_det = method_det)
  } else if (spread == "continuous"){
    assay_draw_cont(df = data, M = M, m = m, method_det = method_det, case = case, m_sp = m_sp)
  } else {
    warning("Unknown type of spread.")
  }
}

# Draw the distribution of mycotoxin in the discrete case
sample_dist = function(raw, work, test, Mc){
  
  temp = tibble(Conc = c(raw, work, test),
                Type = c(
                  rep("raw", times = length(raw)),
                  rep("work", times = length(work)),
                  rep("test", times = length(test))
                )) 
  
  temp$Type = factor(x = temp$Type, levels = c("raw", "work", "test"))
  
  ggplot(data = temp) +
    geom_boxplot(aes(x = Type, y = Conc, group = Type)) +
    geom_hline(yintercept = Mc, color = "red") +
    scale_y_log10() +
    labs(x = "Sample type", y = "Concentration (ppb)") +
    theme_bw()
}

###################################### Tuning #########################################

# Plot P(detection) and P(acceptance) for continuous case
plot_metrics_cont = function(df, xlab){
  
  temp = df %>%
    group_by(param) %>%
    gather(data = ., key = "Metric", value = "Value", -c(param, seed)) %>%
    group_by(param, Metric) %>%
    summarise(q2.5 = stats::quantile(x = Value, probs = 0.025),
              med = median(Value), 
              q97.5 = stats::quantile(x = Value, probs = 0.975))
  
  ggplot(data = temp) +
    geom_ribbon(aes(x = param, ymin = q2.5, ymax = q97.5), alpha = 0.3) +
    geom_line(aes(x = param, y = med)) +
    geom_point(aes(x = param, y = med)) +
    coord_cartesian(ylim = c(0,1)) +
    facet_grid(Metric ~ ., labeller = labeller(Metric = c(P_det = "Probability of detection",
                                                          Paccept = "Probability of acceptance"))) +
    labs(x = xlab, y = "Median (2.5th - 97.5th percentile)") +
    theme_bw() +
    theme(legend.position = "top")
}

# Plot P(acceptance) for discrete case
plot_metrics_dis = function(df, xlab, verbose = FALSE){
  
  temp = df %>%
    gather(data = ., key = "Metric", value = "Value", -c(seed, P_rej, sens, spec, param, c_true)) %>%
    group_by(param, Metric) %>%
    summarise(q2.5 = stats::quantile(x = Value, probs = 0.025),
              med = median(Value), 
              q97.5 = stats::quantile(x = Value, probs = 0.975))
  
  a = ggplot(data = temp) +
    geom_ribbon(aes(x = param, ymin = q2.5, ymax = q97.5), alpha = 0.3) +
    geom_line(aes(x = param, y = med)) +
    geom_point(aes(x = param, y = med)) +
    scale_x_log10(breaks = temp$param) +
    coord_cartesian(ylim = c(0,1)) +
    labs(x = xlab, y = "Probability of acceptance (2.5th - 97.5th percentile)") +
    theme_bw() +
    theme(legend.position = "top")
  
  if(verbose == FALSE){
    return(a)
  } else {
    b = ggplot(data = df) +
      stat_summary(aes(x = param, y = c_true), geom = "pointrange") +
      geom_abline(slope = 1, intercept = 0, color = "red") +
      labs(x = "Input concentration (ppb)", y = "True concentration (ppb)") +
      theme_bw()
    
    return(list(a,b))
  }
}

##################### Functions adapted from GUI #############################

# Plot when there is no tuning parameter
plot_tune0 = function(data){
  
  ggplot(data = data) +
    geom_boxplot(aes(x = "NA", y = Paccept)) +
    geom_point(aes(x = "NA",  y = mean(Paccept)), color = "red", pch = 4, size = 5) +
    labs(x = NULL, y = "Probability of acceptance", subtitle = paste0("Median: ", median(data$Paccept), " Mean: ", mean(data$Paccept))) +
    scale_y_continuous(breaks = seq(0,1,0.1)) +
    coord_cartesian(ylim = c(0,1)) +
    theme_bw()
}

# Plot when there is one tuning parameter
plot_tune1 = function(data, xlab){
  
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

# Plot when there is one tuning parameter
plot_tune2_ribbon = function(data, xlab, legend_lab){
  
  # Summarise the data
  a = data %>%
    gather(data = ., key = "metric", value = "value", -c(seed, param, param2)) %>%
    group_by(param2, param, metric) %>%
    summarise(lb = quantile(x = value, probs = 0.025), 
              med = median(x = value),
              ub = quantile(x = value, probs = 0.975)) %>%
    dplyr::filter(metric == "Paccept")
  
  # Visualize
  b = ggplot(data = a) +
    geom_ribbon(aes(x = param, ymin = lb, ymax = ub, group = as.factor(param2), fill = as.factor(param2)), alpha = 0.3) +
    geom_line(aes(x = param, y = med, color = as.factor(param2))) +
    geom_point(aes(x = param, y = med, color = as.factor(param2))) +
    scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.1)) +
    scale_fill_discrete(name = legend_lab) +
    scale_color_discrete(name = legend_lab) +
    coord_cartesian(ylim = c(0,1)) +
    labs(x = xlab, y = "Probability of acceptance (2.5th - 97.5th percentile)") +
    theme_bw() +
    theme(legend.position = "top")
  
  return(b)
}

# Visualize with boxplots
plot_tune2_boxplot = function(data, xlab, legend_lab, yvar){
  
  # Create a y-axis label
  ylab = switch(EXPR = yvar, 
                "P_det" = "Probability of detection", 
                "Paccept" = "Probability of acceptance")
  
  # Summarise the data
  a = ggplot(data = data, aes_string(y = yvar)) +
    geom_boxplot(aes(x = as.factor(param), group = interaction(param, param2), fill = as.factor(param2))) +
    scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.1)) +
    coord_cartesian(ylim = c(0,1)) +
    scale_fill_discrete(name = legend_lab) +
    labs(x = xlab, y = ylab) +
    theme_bw()+
    theme(legend.position = "top")
  return(a)
}
