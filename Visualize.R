library(tourr)
library(gpairs)

# 2D visualization
plot(data_set)
gpairs(data_set, outer.rot = c(90,0))  # some types of plots won't display

# 3D visualization
animate(data_set, fps=15)
