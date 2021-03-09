### Nicholas Spyrison 08 March 2021
### Recreate figure 3 from spinifex paper. Silo'd, with minimal dependencies.

#```{r step3, echo=F, warning=F, out.width='100%', fig.width=8, fig.height=2, fig.cap="Snapshots of a radial manual tour manipulating aede2: (1) original projection, (2) full contribution, (3) zero contribution, (4) back to original. "}
# install.packages("spinifex") ## Made with Cran spinifex_0.2.7
require("spinifex")
require("tourr")
require("ggplot2")
require("magrittr")
my_theme <- list(scale_color_brewer(palette = "Dark2"),
                 theme_void(), 
                 theme(legend.position ="none"))
# ## Testing a slight change to scale to see if that works better as framse change
# scale_axes <-
#   function (x, position = "center", to = data.frame(x = c(-1L, 1L), y = c(-1L, 1L))){
#     if(is.null(to)) to <- data.frame(x = c(-1L, 1L), y = c(-1L, 1L))
#     x_to <- c(min(to[, 1L]), max(to[, 1L]))
#     y_to <- c(min(to[, 2L]), max(to[, 2L]))
#     xdiff <- diff(x_to)
#     ydiff <- diff(y_to)
#     xcenter <- mean(x_to)
#     ycenter <- mean(y_to)
#     if(position == "center") {
#       scale <- 0.3 * max(ydiff, xdiff)
#       xoff <- xcenter
#       yoff <- ycenter
#     }
#     x[, 1L] <- scale * x[, 1L] + xoff
#     x[, 2L] <- scale * x[, 2L] + yoff
#     return(x)
#   }
## Doesn't seem to make the change I expected. may want tyo test in dev version.

make_radial_tour_fig <- function(){
  dat  <- scale_sd(flea[, 1:6])
  clas <- factor(flea$species)
  bas  <- MASS::lda(dat, grouping = clas)$scaling %>% tourr::orthonormalise()
  mvar <- which(abs(bas[, 1]) == max(abs(bas[, 1])))
  
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
  ## Show frames: 1, 4, 10, 16
  
  cn_label <- colnames(dat)
  p1 <- view_frame(mtour[,, 1], dat, mvar,
                   aes_args = list(color = clas, shape = clas)) +
    ggtitle("1) norm = 0.91") + my_theme
  p2 <- view_frame(mtour[,, 4], dat, mvar,
                   aes_args = list(color = clas, shape = clas)) +
    ggtitle("2) norm = 1") + my_theme
  p3 <- view_frame(mtour[,, 10], dat, mvar,
                   aes_args = list(color = clas, shape = clas)) +
    ggtitle("3) norm = 0") + my_theme
  p4 <- view_frame(mtour[,, 16], dat, mvar,
                   aes_args = list(color = clas, shape = clas)) +
    ggtitle("4) norm = 0.91") + my_theme
  
  ## Return ggplot figure.
  gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 4)
}
