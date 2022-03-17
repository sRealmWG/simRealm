# Plotting the distance kernels of the different selected movement sd parameters
source("./analysis/parameters/community_v2.r")

# Plotting function ----
plotting_kernels <- function(param_i, xlim = NULL) {
   title_string <- paste0("movement_sd = ", params[param_i])

   x <- rnorm(10^7, sd = params[param_i])
   y <- rnorm(10^7, sd = params[param_i])

   density_data <- density(sqrt(x^2 + y^2))

   if (is.null(xlim)) xlim <- range(density_data$x)

   plot(density_data, las = 1, main = title_string, xlim = xlim)

   return(xlim)
}

svg(filename = "./figures/dispersal_kernels/dispersal_kernels_v1.svg", width = 12L, height = 6L)
params <- sort(unique(parameter_table$MOVEMENT_SD), decreasing = TRUE)
par(mfcol = n2mfrow(length(params)))

# First plot ----
## to keep the boundaries of the largest range
xlim1 <- plotting_kernels(param_i = 1L)

# Next plots ----
for (param_i in 2:length(params)) {
   plotting_kernels(param_i = param_i, xlim = xlim1)
}

dev.off()

