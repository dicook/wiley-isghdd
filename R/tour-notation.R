# Figure 2 diagram illustrating a projection: Huber plot of 2D data with 1D projection
library(ggplot2)
p <- PPtreeViz::Huberplot(iris[,1:2],iris[,5],PPmethod="LDA") 

ggsave(p, filename = "notation-huber.pdf", 
       path = here::here("figures"),
       width = 12, height = 5, units = "in")
