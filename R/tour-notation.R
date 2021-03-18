# Figure 2 diagram illustrating a projection: Huber plot of 2D data with 1D projection
library(ggplot2)
p <- PPtreeViz::Huberplot(iris[,1:2],iris[,5],PPmethod="LDA") 

ggsave(p, filename = "notation-huber.pdf", 
       path = here::here("figures"),
       width = 12, height = 5, units = "in")

# figure 3 diagram illustrating different displays for different d
library(tourr)
render(flea[,1:6], 
       guided_tour(holes(), d= 1),
       display_dist(),
       "pdf", 
       frames = 1,
       file.path(here::here("figures", "notation-display-1d.pdf")))

render(flea[,1:6], 
       guided_tour(holes(), d= 2),
       display_xy(),
       "pdf", 
       frames = 1,
       file.path(here::here("figures", "notation-display-2d.pdf")))

render(flea[,1:6], 
       guided_tour(holes(), d= 5),
       display_pcp(),
       "pdf", 
       frames = 1,
       file.path(here::here("figures", "notation-display-5d.pdf")))
