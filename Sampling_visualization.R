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

## Draw the contamination level plot.
contam_level_draw = function(method, spread_radius, LOC){
  
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
sp_draw = function(method, data, spread, xlim, ylim, n_strata, by){
  if(method %in% c("srs", "ss")){
    sp_draw_srs(data = data, spread = spread, xlim = xlim, ylim = ylim)
  } else if (method == "strs"){
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
overlay_draw = function(method, data, spread, xlim, ylim, n_strata, by){
  if(method %in% c("srs", "ss")){
    overlay_draw_srs(data = data, spread = spread, xlim = xlim, ylim = ylim)
  } else if (method == "strs"){
    overlay_draw_strs(data = data, spread = spread, xlim = xlim, ylim = ylim, n_strata = n_strata, by = by)
  }
}

