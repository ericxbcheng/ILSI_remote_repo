library(reshape2)
library(kableExtra)

source(file = "Sampling_contamination.R")

# The input parameters
n_sp = 10
sp_radius = 1

# Run this if we want reproducibility
#set.seed(123)

# Create a function that generates a simple random sampling plan
sim_plan = function(n_sp, x_lim, y_lim, radius){
  
  ## Generate a data frame that contains the coordinates of the sampling points
  x_sp = runif(n = n_sp, min = x_lim[1], max = x_lim[2])
  y_sp = runif(n = n_sp, min = y_lim[1], max = y_lim[2])
  
  ## Generate a unique identifier for each sample point
  a = rep("sp", times = n_sp)
  b = 1:n_sp %>% as.character()
  ID = paste(a, "-", b, sep = "")
  
  ## Generate a column for sample radius
  r = rep(radius, times = n_sp)
  
  data.frame(X = x_sp,
             Y = y_sp,
             ID = ID,
             label = "sample point",
             r = r)
}

sp_xy = sim_plan(n_sp = n_sp, x_lim = x_lim, y_lim = y_lim, r = sp_radius)

contam_sp_xy = rbind(contam_xy, sp_xy)
rownames(contam_sp_xy) = NULL

# Visualization

## Sampling plan for continuous spread
plot_sp_dis = ggplot() +
  geom_point(data = sp_xy, aes(x = X, y = Y, color = label, shape = label)) +
  scale_color_manual(values = c("darkgreen")) +
  scale_shape_manual(values = 15) +
  geom_circle(data = sp_xy, aes(x0 = X, y0 = Y, r = r), fill = "darkgreen", alpha = 0.1) +
  coord_fixed(ratio = 1, xlim = x_lim, ylim = y_lim) +
  theme_bw()

## Sampling plan for discrete spread
plot_sp_cont = ggplot() +
  geom_point(data = sp_xy, aes(x = X, y = Y, color = label, shape = label)) +
  scale_color_manual(values = c("darkgreen")) +
  scale_shape_manual(values = 15) +
  coord_fixed(ratio = 1, xlim = x_lim, ylim = y_lim) +
  theme_bw()

## Sampling plan for continuous spread
plot_overlay_cont = ggplot() +
  geom_point(data = subset(x = contam_sp_xy, subset = label != "spread"), 
             aes(x = X, y = Y, 
                 color = label, 
                 shape = label)) +
  scale_color_manual(values = c("coral", "darkgreen")) +
  scale_shape_manual(values = c(16, 15)) +
  geom_circle(data = subset(x = contam_sp_xy, subset = label == "spot"),
              aes(x0 = X, y0 = Y, r = r), 
              fill = "coral", 
              alpha = 0.1) +
  coord_fixed(ratio = 1, xlim = x_lim, ylim = y_lim) +
  theme_bw()

## Sampling plan for discrete spread
plot_overlay_dis = ggplot() +
  geom_point(data = contam_sp_xy, aes(x = X, y = Y, color = label, shape = label)) +
  scale_color_manual(values = c("coral", "cornflowerblue", "darkgreen")) +
  scale_shape_manual(values = c(16, 16, 15)) +
  geom_circle(data = subset(x = contam_sp_xy, subset = label == "sample point"),
              aes(x0 = X, y0 = Y, r = r),
              fill = "darkgreen",
              alpha = 0.1) +
  coord_fixed(ratio = 1, xlim = x_lim, ylim = y_lim) +
  theme_bw()