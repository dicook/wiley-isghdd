# wire frame cubes
library(tidyverse)
library(ggthemes)
library(geozoo)
library(tourr)
library(patchwork)
library(ggrepel)

set.seed(5)
c1 <- cube.iterate(p = 1)
c1$points <- as_tibble(c1$points)
c1$edges <- as_tibble(c1$edges)
c2 <- cube.iterate(p = 2)
c2$points <- as_tibble(c2$points)
c2$edges <- as_tibble(c2$edges)
c3 <- cube.iterate(p = 3)
proj <- basis_random(3,2) 
c3$points <- c3$points %*% proj
colnames(c3$points) <- c("Var1", "Var2")
c3$points <- as_tibble(c3$points)
c3$edges <- as_tibble(c3$edges)
c4 <- cube.iterate(p = 4)
proj <- basis_random(4,2) 
c4$points <- c4$points %*% proj
colnames(c4$points) <- c("Var1", "Var2")
c4$points <- as_tibble(c4$points)
c4$edges <- as_tibble(c4$edges) 
c4$edges.sub <- tibble(from = c(1,1,1,2,2,3,3,4,5,5,6,7), 
                       to = c(2,3,5,4,6,4,7,8,6,7,8,8))
c5 <- cube.iterate(p = 5)
proj <- basis_random(5, 2) 
c5$points <- c5$points %*% proj
colnames(c5$points) <- c("Var1", "Var2")
c5$points <- as_tibble(c5$points)
c5$edges <- as_tibble(c5$edges) 
c5$edges.sub <- tibble(from = c(1,1,1,1,2,2,2,3,3,3,4,4,5,5,5,6,6,7,7,8,9,9,9,10,10,11,11,12,13,13,14,15), 
                       to = c(2,3,5,9,4,6,10,4,7,11,8,12,6,7,13,8,14,8,15,16,10,11,13,12,14,12,15,16,14,15,16,16))

# plot
# 1D
p1 <- ggplot() +
  geom_point(data=c1$points, aes(x=Var1, y=1)) +
  geom_segment(data=c1$edges, 
               aes(x=c1$points$Var1[c1$edges$from], 
                   xend=c1$points$Var1[c1$edges$to],
                   y=1, yend=1), 
               linetype=3, colour = "#607848") + 
  geom_point(data=c1$points[1,], aes(x=Var1, y=1), colour = "#E7298A") +
  ggtitle("1D") +
  theme_void() +
  theme(title = element_text(colour = "black", size = 24),
        aspect.ratio = 1) +
  xlim(c(-0.2, 1.2))

# 2D
p2 <- ggplot() +
  geom_point(data=c2$points, aes(x=Var1, y=Var2)) +
  geom_segment(data=c2$edges[c(1,4),], 
               aes(x=c2$points$Var1[from], 
                   xend=c2$points$Var1[to],
                   y=c2$points$Var2[from], 
                   yend=c2$points$Var2[to])) + 
  geom_segment(data=c2$edges[c(2,3),], 
               aes(x=c2$points$Var1[from], 
                   xend=c2$points$Var1[to],
                   y=c2$points$Var2[from], 
                   yend=c2$points$Var2[to]), 
               linetype = 3, colour = "#607848") + # dashed connectors
  geom_point(data=c2$points[1:2,], aes(x=Var1, y=Var2), 
             colour = "#E7298A") +
  geom_segment(data=c2$edges[1,], 
               aes(x=c2$points$Var1[from], 
                   xend=c2$points$Var1[to],
                   y=c2$points$Var2[from], 
                   yend=c2$points$Var2[to]), 
               colour = "#E7298A") + 
  ggtitle("2D") +
  theme_void() +
  theme(title = element_text(colour = "black", size = 24),
        aspect.ratio = 1) +
  xlim(c(-0.15, 1.15)) + ylim(c(-0.15, 1.15))

# 3D
c_in <- c(1,2,4,6,9,10,11,12)
c_out <- c(3,5,7,8)
p3 <- ggplot() +
  geom_point(data=c3$points, aes(x=Var1, y=Var2)) +
  geom_segment(data=c3$edges[c_in,], 
               aes(x=c3$points$Var1[from], 
                   xend=c3$points$Var1[to],
                   y=c3$points$Var2[from], 
                   yend=c3$points$Var2[to])) + 
  geom_segment(data=c3$edges[c_out,], 
               aes(x=c3$points$Var1[from], 
                   xend=c3$points$Var1[to],
                   y=c3$points$Var2[from], 
                   yend=c3$points$Var2[to]), 
               linetype = 3, colour = "#607848") + 
  geom_point(data=c3$points[1:4,], aes(x=Var1, y=Var2), colour = "#E7298A") +
  geom_segment(data=c3$edges[c(1,2,4,6),], 
               aes(x=c3$points$Var1[from], 
                   xend=c3$points$Var1[to],
                   y=c3$points$Var2[from], 
                   yend=c3$points$Var2[to]), colour = "#E7298A") + 
  ggtitle("3D") +
  theme_void() +
  theme(title = element_text(colour = "black", size = 24),
        aspect.ratio = 1)
# p3 + geom_text_repel(data=c3$points, aes(x=Var1, y=Var2, label = 1:nrow(c3$points)), size=5) 
  
# 4D
c_out <- c(4, 7, 10, 12, 15, 17, 19, 20)
c_in <- c(1:nrow(c4$edges))[-c_out]
p4 <- ggplot() +
  geom_point(data=c4$points, aes(x=Var1, y=Var2)) +
  geom_segment(data=c4$edges[c_in,], 
               aes(x=c4$points$Var1[from], 
                   xend=c4$points$Var1[to],
                   y=c4$points$Var2[from], 
                   yend=c4$points$Var2[to])) + 
  geom_segment(data=c4$edges[c_out,], 
               aes(x=c4$points$Var1[from], 
                   xend=c4$points$Var1[to],
                   y=c4$points$Var2[from], 
                   yend=c4$points$Var2[to]),
               linetype = 3, colour = "#607848") + 
  geom_point(data=c4$points[1:8,], aes(x=Var1, y=Var2), colour = "#E7298A") +
  geom_segment(data=c4$edges.sub, 
               aes(x=c4$points$Var1[from], 
                   xend=c4$points$Var1[to],
                   y=c4$points$Var2[from], 
                   yend=c4$points$Var2[to]), colour = "#E7298A") + 
  ggtitle("4D") +
  theme_void() +
  theme(title = element_text(colour = "black", size = 24),
        aspect.ratio = 1)
# p4 + geom_text_repel(data=c4$points, aes(x=Var1, y=Var2, label = 1:nrow(c4$points)), size=5) 

# 5D
c_out <- c(5,9,13,16,20,23,26,28,32,35,38,
           40,43,45,47,48)
c_in <- c(1:nrow(c5$edges))[-c_out]
p5 <- ggplot() +
  geom_point(data=c5$points, aes(x=Var1, y=Var2)) +
  geom_segment(data=c5$edges[c_in,], 
               aes(x=c5$points$Var1[from], 
                   xend=c5$points$Var1[to],
                   y=c5$points$Var2[from], 
                   yend=c5$points$Var2[to])) + 
  geom_segment(data=c5$edges[c_out,], 
               aes(x=c5$points$Var1[from], 
                   xend=c5$points$Var1[to],
                   y=c5$points$Var2[from], 
                   yend=c5$points$Var2[to]),
               linetype = 3, colour = "#607848") + 
  geom_point(data=c5$points[1:16,], aes(x=Var1, y=Var2), colour = "#E7298A") +
  geom_segment(data=c5$edges.sub, 
               aes(x=c5$points$Var1[from], 
                   xend=c5$points$Var1[to],
                   y=c5$points$Var2[from], 
                   yend=c5$points$Var2[to]), colour = "#E7298A") + 
  ggtitle("5D") +
  theme_void() +
  theme(title = element_text(colour = "black", size = 24),
        aspect.ratio = 1)
# p5 + geom_text_repel(data=c5$points, aes(x=Var1, y=Var2, label = 1:nrow(c5$points)), size=5) 

p1 + p2 + p3 + p4 + p5 + 
  plot_layout(ncol = 5)
