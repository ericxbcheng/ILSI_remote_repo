# Contamination plots
contam_draw = function(data, spread, xlim, ylim){
  if (spread == "discrete") {
    ggplot() +
      geom_point(data = data, aes(x = X, y = Y, color = label)) +
      coord_fixed(ratio = 1, xlim = xlim, ylim = ylim) +
      theme_bw()
  } else if (spread == "continuous") {
    ggplot() +
      geom_point(data = subset(data, subset = data$label == "spot"),
                 aes(x = X, y = Y, color = label)) +
      geom_circle(
        data = subset(data, subset = data$label == "spot"),
        aes(x0 = X, y0 = Y, r = r), fill = "coral", alpha = 0.1) +
      coord_fixed(ratio = 1, xlim = xlim, ylim = ylim) +
      theme_bw()
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
sp_draw = function(method, data, spread, xlim, ylim, n_strata, by = "row"){
  if(method == "srs"){
    sp_draw_srs(data, spread, xlim, ylim)
  } else if (method == "strs"){
    sp_draw_strs(data, spread, xlim, ylim, n_strata, by)
  }
}

# Overlay plots

## Simple random sampling
overlay_draw_srs = function(data, spread, xlim, ylim){
  if(spread == "discrete"){
    ggplot() +
      geom_point(data = data, aes(x = X, y = Y, color = label, shape = label)) +
      scale_color_manual(values = c("coral", "cornflowerblue", "darkgreen")) +
      scale_shape_manual(values = c(16, 16, 15)) +
      geom_circle(data = subset(x = data, subset = label == "sample point"),
                  aes(x0 = X, y0 = Y, r = r),
                  fill = "darkgreen",
                  alpha = 0.1) +
      coord_fixed(ratio = 1, xlim = xlim, ylim = ylim) +
      theme_bw()
  } else if (spread == "continuous"){
    ggplot() +
      geom_point(data = subset(x = data, subset = label != "spread"), 
                 aes(x = X, y = Y, 
                     color = label, 
                     shape = label)) +
      scale_color_manual(values = c("coral", "darkgreen")) +
      scale_shape_manual(values = c(16, 15)) +
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
overlay_draw = function(method, data, spread, xlim, ylim, n_strata, by = "row"){
  if(method == "srs"){
    overlay_draw_srs(data, spread, xlim, ylim)
  } else if (method == "strs"){
    overlay_draw_strs(data, spread, xlim, ylim, n_strata, by)
  }
}

