
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
