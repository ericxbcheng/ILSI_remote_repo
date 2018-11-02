# Contamination plots
contam_draw = function(data, spread, xlim, ylim){
  if (spread == "discrete") {
    ggplot() +
      geom_point(data = data, aes(x = X, y = Y, shape = label, color = cont_level)) +
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

## Draw the contamination level plot for continuous spread in a 2D plane.
contam_level_draw_2d = function(method, spread_radius, LOC){
  
  if(method == "exp"){
    f_chosen = f_exp
  } else if (method == "norm"){
    f_chosen = f_norm
  } else {
    stop("Method is undefined. Choose 'exp' or 'norm'.")
  }
  
  ggplot(data = data.frame(x = c(0, spread_radius)), aes(x = x))+
    geom_line(stat = "function", fun = f_chosen, args = list(spread_radius = spread_radius, LOC = LOC)) +
    labs(x = "Distance from a contamination source", y = "Contamination contribution") +
    theme_bw()
}

## Draw thw contamination level plot for continuous spread in a 3D space
contam_level_draw_3d = function(method, df_contam, xlim, ylim, spread_radius, LOC, interactive = FALSE){
  
  ### Extract the coordinates of contamination spots
  spot_coord = df_contam %>%
    dplyr::filter(label == "spot") %>%
    dplyr::select(c(X, Y, cont_level))
  
  ### Create a grid that has the dimensions of the field
  a = expand.grid(X = seq(xlim[1],xlim[2],0.1), Y = seq(ylim[1], ylim[2],0.1))
  
  ### Calculate the contamination contribution from each contamination spot on each point in the field and put them in a matrix
  b = mapply(FUN = f_density, spot_coord$X, spot_coord$Y, MoreArgs = list(method = method, x = a$X, y = a$Y, spread_radius = spread_radius, LOC = LOC))
  
  ### Calculate the contamination level distribution from each contamination spot
  I = matrix(data = 1, nrow = nrow(b))
  scalars = I %*% spot_coord$cont_level
  c = scalars * b
  
  ### Calculate the contamination level at each point in the field by summing up the contamination contributed by all sources
  d = rowSums(c)
  a$Z = log10(d)
  
  ### Make the 3D plot
  if(interactive == FALSE){
    scatter3D(x = a$X, y = a$Y, z = a$Z, colvar = a$Z, zlab = "log CFU/mL")
  } else {
    plot_ly(x = a$X, y = a$Y, z = a$Z, color = a$Z,type = "scatter3d") 
  }
}

## Wrap-up funciton
contam_level_draw = function(dimension, method, spread_radius, LOC, df_contam, xlim, ylim, ...){
  if(dimension == "2d"){
    contam_level_draw_2d(method = method, spread_radius = spread_radius, LOC = LOC)
    
  } else if (dimension == "3d"){
    contam_level_draw_3d(method = method, df_contam = df_contam, spread_radius = spread_radius, LOC = LOC, xlim = xlim, ylim = ylim, ...)
    
  } else {
    stop("we do not support this dimension. Please choose '2d' or '3d'.")
  }
}



# Sampling plan plots

## Simple random sampling
sp_draw_srs = function(data, spread, xlim, ylim){
  if(spread == "discrete"){
    ggplot() +
      geom_point(data = data, aes(x = X, y = Y, color = label, shape = label)) +
      scale_color_manual(values = c("darkgreen")) +
      scale_shape_manual(values = 15) +
      geom_circle(data = data, aes(x0 = X, y0 = Y, r = r), fill = "darkgreen", alpha = 0.1) +
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
sp_draw_strs = function(data, spread, xlim, ylim, n_strata, by){
  base = sp_draw_srs(data = data, spread = spread, xlim = xlim, ylim = ylim)
  bounds = calc_bounds(xlim = xlim, ylim = ylim, n_strata = n_strata, by = by)
  if(by == "row"){
    base + 
      geom_hline(yintercept = bounds, color = "darkgrey")
  } else if (by == "column"){
    base +
      geom_vline(xintercept = bounds, color = "darkgrey")
  }
}

### A wrapper function
sp_draw = function(method_sp, data, spread, xlim, ylim, n_strata, by){
  if(method_sp %in% c("srs", "ss")){
    sp_draw_srs(data = data, spread = spread, xlim = xlim, ylim = ylim)
  } else if (method_sp == "strs"){
    sp_draw_strs(data = data, spread = spread, xlim = xlim, ylim = ylim, n_strata = n_strata, by = by)
  }
}

# Overlay plots

## Simple random sampling
overlay_draw_srs = function(data, spread, xlim, ylim){
  if(spread == "discrete"){
    ggplot() +
      geom_point(data = subset(data, subset = label == "sample point"), aes(x = X, y = Y, shape = label), color = "darkgreen") +
      geom_circle(data = subset(x = data, subset = label == "sample point"),
                  aes(x0 = X, y0 = Y, r = r),
                  fill = "darkgreen",
                  alpha = 0.1) +
      geom_point(data = subset(data, subset = label != "sample point"), aes(x = X, y = Y, color = cont_level, shape = label)) +
      scale_color_gradient(name = "Contamination Level", low = "orange", high = "red2") +
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
      scale_color_gradient(name = "Contamination Level", low = "orange", high = "red2") +
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
  bounds = calc_bounds(xlim = xlim, ylim = ylim, n_strata = n_strata, by = by)
  if(by == "row"){
    base + 
      geom_hline(yintercept = bounds, color = "darkgrey")
  } else if (by == "column"){
    base +
      geom_vline(xintercept = bounds, color = "darkgrey")
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

# Create a function that draws the contamination level of samples and shows the microbiological criteria
assay_draw = function(data, M, m, method_det){
  temp = data.frame(Thresholds = c("M", "m", "LOD"), 
                    val = c(M, m, get_LOD(method_det = method_det)))
  
  ggplot() +
    geom_col(data = subset(x = data, subset = label == "sample point"), aes(x = ID, y = cont_level), fill = "darkgrey") +
    geom_text(data = subset(x = data, subset = label == "sample point"), 
              aes(x = ID, y = cont_level, 
                  label = round(cont_level, digits = 2)), 
              position = position_nudge(y = 0.2)) +
    geom_hline(data = temp, aes(yintercept = val, color = Thresholds), size = 1.5) +
    scale_y_log10(breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000), label = scientific_format()) +
    scale_color_manual(values = c("#00BA38", "#D89000", "#F8766D" )) +
    labs(x = "Sample point", y = "Contamination level (CFU/g)") +
    theme_bw() 
}