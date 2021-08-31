# Figure 2 diagram illustrating a projection: Huber plot of 2D data with 1D projection
library(ggplot2)
library(palmerpenguins)
library(dplyr)
library(PPtreeViz)
penguins_small <- penguins %>% filter(!is.na(bill_length_mm))

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

# figure 4 diagram illustrating different types of target generation
library(tourr)
library(ferrn)
library(dplyr)
n_basis <- 20
start_basis <- holes_1d_better %>% filter(id == 1) %>% pull(basis) %>% .[[1]]
set.seed(1234)
grand <- save_history(
  boa5, 
  tour_path = grand_tour(d = 1),
  max_bases = n_basis
)

interp <- interpolate(grand)

attr(interp, "class") <- NULL
attr(interp, "data") <- NULL
grand_list <- list()
for (i in 1: dim(interp)[3]) grand_list[[i]] <- interp[,,i]
index <- holes()
grand_tibble <- purrr::map_dfr(grand_list, 
                               ~tibble::tibble(basis = list(matrix(.x, ncol = 1)))) %>% 
  mutate(method = "grand_tour", 
         id = row_number() + 1,
         info = "interpolation",
         index_val = vapply(basis, function(x) index(x), double(1))) %>% 
  add_row(basis = list(start_basis), method = "grand_tour", id = 1, 
          info = "interpolation", index_val = index(start_basis)) %>% 
  arrange(id)

dt <- bind_rows(grand_tibble, holes_1d_better %>% mutate(method = "guided_tour")) %>% 
  compute_pca(group = method, flip = FALSE) %>% purrr::pluck("aug")

p <- ggplot() + 
  add_space(get_space_param(dt)) + 
  add_start(get_start(dt), start_color = method, start_alpha = 0.8) + 
  add_end(get_best(dt, group = method), end_color = method, end_size = 9) + 
  add_interp(get_interp(dt), interp_color = method) + 
  ggplot2::theme_void() +
  ggplot2::theme(aspect.ratio = 1, legend.position = "bottom", legend.title = ggplot2::element_blank()) + 
  scale_color_discrete_botanical() + 
  facet_wrap(vars(method)) + 
  theme(strip.text.x = element_blank(),
        legend.text = element_text(size = 20))

ggsave(p, filename = "notation-target-gen.pdf", 
       path = here::here("figures"),
       width = 10, height = 5, units = "in")

# This is a hack to get minimum index value in the Huber plot, too
Huberplot2 <- function (origdata2D, origclass, PPmethod = "LDA", weight = TRUE, 
          r = 1, lambda = 0.5, opt.proj = TRUE, UserDefFtn = NULL, 
          min = FALSE, scale_data = 1,
          ...) 
{
  index <- NULL
  best.proj <- NULL
  best.index <- 0
  origdata2D <- as.matrix(origdata2D)
  for (i in 0:360) {
    theta <- pi/180 * i
    proj.data <- matrix(cos(theta) * origdata2D[, 1] + sin(theta) * 
                          origdata2D[, 2])
    proj <- matrix(c(cos(theta), sin(theta)), ncol = 1)
    if (PPmethod == "LDA") {
      newindex <- LDAindex(origclass, origdata2D, proj = proj, 
                           weight = weight)
    }
    else if (PPmethod == "PDA") {
      newindex <- PDAindex(origclass, origdata2D, proj, 
                           weight = weight, lambda = lambda)
    }
    else if (PPmethod == "Lr") {
      newindex <- Lrindex(origclass, origdata2D, proj, 
                          weight = weight, r = r)
    }
    else if (PPmethod == "GINI") {
      newindex <- GINIindex1D(origclass, origdata2D, proj)
    }
    else if (PPmethod == "ENTROPY") {
      newindex <- ENTROPYindex1D(origclass, origdata2D, 
                                 proj)
    }
    else if (PPmethod == "UserDef") {
      newindex <- UserDefFtn(proj.data, ...)
    }
    index <- c(index, newindex)
  }
  if (!min) {
    sel.index <- which(index[1:360] > signif(max(index), 6) - 
                       1e-06)
  } else
    sel.index <- which(index[1:360] < signif(min(index), 6) + 
                         1e-06)
  theta.best.all <- pi/180 * (sel.index - 1)
  theta.best <- theta.best.all[1]
  proj.data.best <- matrix(cos(theta.best) * origdata2D[, 1] + 
                             sin(theta.best) * origdata2D[, 2])
  index.best <- max(index)
  range <- round(max(index) - min(index), 5)
  if (range == 0) {
    PPindex <- rep(4, length(index))
  }
  else {
    PPindex <- (index - min(index))/range * 2 + 3
  }
  data.circle <- NULL
  data.index <- NULL
  for (i in 1:361) {
    theta <- pi/180 * (i - 1)
    data.index <- rbind(data.index, c(PPindex[i] * cos(theta), 
                                      PPindex[i] * sin(theta)))
    data.circle <- rbind(data.circle, c(4 * cos(theta), 4 * 
                                          sin(theta)))
  }
  maxdiff <- max(c(diff(range(origdata2D[, 1])), diff(range(origdata2D[, 
                                                                       2]))))
  orig.scaled <- apply(origdata2D, 2, function(x) (x - mean(x))/maxdiff * 
                         3.5)
  data.cX <- data.circle[, 1]
  data.cY <- data.circle[, 2]
  data.X <- data.index[, 1]
  data.Y <- data.index[, 2]
  plot.data <- data.frame(data.cX, data.cY, data.X, data.Y)
  x <- orig.scaled[, 1]
  y <- orig.scaled[, 2]
  group <- origclass
  point.data <- data.frame(x, y, group)
  min.X <- min(unlist(plot.data))
  max.X <- max(unlist(plot.data))
  P1 <- ggplot(data = plot.data, aes(x = data.X, y = data.Y)) + 
    geom_path() + geom_path(aes(x = data.cX, y = data.cY), 
                            linetype = "dashed") + 
    geom_point(data = point.data, aes(x = x*scale_data, y = y*scale_data, color = group, shape = group)) + scale_x_continuous(breaks = NULL) + 
    scale_y_continuous(breaks = NULL) + xlab("") + ylab("") + 
    coord_fixed() + theme_bw() + theme(panel.border = element_blank())
  if (opt.proj) {
    P1 <- P1 + geom_abline(intercept = 0, slope = sin(theta.best)/cos(theta.best))
    if (length(theta.best.all) > 1) 
      for (i in 2:length(theta.best.all)) P1 <- P1 + geom_abline(intercept = 0, 
                                                                 slope = sin(theta.best.all[i])/cos(theta.best.all[i]), 
                                                                 linetype = "dashed")
  }
  best.proj.data <- proj.data.best
  group <- origclass
  hist.data <- data.frame(best.proj.data, group)
  P2 <- ggplot(data = hist.data, aes(x = best.proj.data, group = group)) + 
    #geom_histogram(aes(fill = group), position = "stack") +
    geom_density(aes(fill = group, colour = group), alpha=0.7) +
    xlab("Projection") +
    theme(axis.text.x = element_blank())
  gridExtra::grid.arrange(P1, P2, nrow = 1)
}

p <- Huberplot2(penguins_small[,3:4],
                           penguins_small$species,
                           PPmethod="LDA", scale=1.8) 

ggsave(p, filename = "notation-huber1.pdf", 
       path = here::here("figures"),
       width = 12, height = 5, units = "in")

p <- Huberplot2(penguins_small[,3:4],
                penguins_small$species,
                PPmethod="LDA", min=TRUE, scale=1.8) 

ggsave(p, filename = "notation-huber2.pdf", 
       path = here::here("figures"),
       width = 12, height = 5, units = "in")

