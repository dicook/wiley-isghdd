### Nicholas Spyrison 08 March 2021
### Recreate figure 3 from spinifex paper. Silo'd, with minimal dependencies.

require("spinifex")
require("tourr")
require("ggplot2")
require("magrittr")
my_theme <- list(scale_color_brewer(palette = "Dark2"),
                 theme_void(), 
                 theme(legend.position ="none"))

### Create the 
make_radial_tour_fig <- function(){
  dat  <- scale_sd(flea[, 1:6])
  clas <- factor(flea$species)
  bas  <- MASS::lda(dat, grouping = clas)$scaling %>% tourr::orthonormalise()
  mvar <- 4 ## aede1, primary disting of 1 cluster.
  
  if(F) ## Manual check the starting LDA basis
    view_frame(bas, dat, mvar,
               aes_args = list(color = clas, shape = clas),
               ggproto = my_theme)
  
  ang <- .29
  mtour <- manual_tour(bas, manip_var = mvar, angle = ang)
  if(F){ ## Manually check for frames to use
    play_manual_tour(bas, dat, mvar, angle = ang)
    for(i in 1:dim(mtour)[3]){
      mvr <- mtour[mvar,, i]
      msg <- paste0("i=",i,". norm(mv)=", sqrt(mvr[1]^2 + mvr[2]^2))
      print(msg)
    }
  }
  ## Show frames: 1, 4, 10, 15
  
  p1 <- view_frame(mtour[,, 1], dat, mvar,
                   aes_args = list(color = clas, shape = clas)) + my_theme
  p2 <- view_frame(mtour[,, 4], dat, mvar,
                   aes_args = list(color = clas, shape = clas)) + my_theme
  p3 <- view_frame(mtour[,, 10], dat, mvar,
                   aes_args = list(color = clas, shape = clas)) + my_theme
  p4 <- view_frame(mtour[,, 15], dat, mvar,
                   aes_args = list(color = clas, shape = clas)) + my_theme
  
  ## Return ggplot figure.
  # gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 4)
  cowplot::plot_grid(p1, p2, p3, p4, ncol = 4,
                     scale = c(1, 1, .75, 1), hjust = -.1,
                     label_fontface = "plain",
                     labels = c("1) norm = 0.86", "2) norm = 1", "3) norm = 0", "4) norm = 0.86")
  )
}

fig <- make_radial_tour_fig()
ggsave(filename = "fig_radial_manual_tour.png",
       plot = fig,
       device = "png", path = "./figures", 
       dpi = 300,
       width = 6, height = 1.5, units = "in"
)
