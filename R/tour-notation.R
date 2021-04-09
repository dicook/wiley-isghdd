# Figure 2 diagram illustrating a projection: Huber plot of 2D data with 1D projection
library(ggplot2)
library(palmerpenguins)
library(dplyr)
penguins_small <- penguins %>% filter(!is.na(bill_length_mm))
p <- PPtreeViz::Huberplot(penguins_small[,3:4],penguins_small$species,PPmethod="LDA") 

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

# figure 4 diagram illustrating different types of target generation
library(tourr)
library(ferrn)
library(dplyr)
n_basis <- 20
set.seed(1234)
grand <- save_history(
  boa5, 
  grand_tour(d = 1),
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
         id = row_number(),
         info = "interpolation",
         index_val = vapply(basis, function(x) index(x), double(1)))

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
