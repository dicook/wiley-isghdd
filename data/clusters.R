# adapted from Holmes and Ngyuen, 2019
# Simple Gaussian 10-d
library(ggplot2)
library(tourr)
library(Rtsne)
library(dplyr)
library(readr)

tidy_tsne <- function(model, data) {
  enframe <- as.data.frame(model[["Y"]])
  colnames(enframe) <- c("tsneX", "tsneY", "tsneZ")[seq_len(ncol(enframe))]
  dplyr::bind_cols(enframe, data)
}


# 5 gaussian clusters with equal covariance
set.seed(63968)
m <- 500
d <- 5
p <- 10
k <- 5
sigma <- 1
muMax <- 25
mu <- matrix(runif(k*d, max = muMax), ncol = d)
X <- lapply(seq_len(k), function(i) {
  ix <- rep(1, m) %*% t(mu[i, ]) + matrix(sigma*rnorm(m*d), ncol = d)
  as.data.frame(ix)
})
X_tbl <- dplyr::bind_rows(X, .id = "cluster")
X <- as.matrix(X_tbl[,-1])
Y <- X  %*% matrix(rnorm(d*p), ncol = p)
colnames(Y) <- sprintf("dim%02d", 1:p)
tour_tbl <- dplyr::bind_cols(as.data.frame(Y), 
                             cluster = factor(X_tbl[["cluster"]]))


tsne_y <- Rtsne::Rtsne(Y)
#embed_tbl <- tidy_tsne(tsne_y, 
#                       list(cluster = factor(X_tbl[["cluster"]])))
#colnames(embed_tbl) <- c("tsneX", "tsneY", "cluster")
tsne_df <- tibble(tsneX = tsne_y$Y[,1], 
                    tsneY = tsne_y$Y[,2], 
                    cluster = X_tbl$cluster)
ggplot(tsne_df, aes(x=tsneX, y=tsneY, colour=cluster)) +
  geom_point() + coord_equal()

# multichallenge data set
url <- "http://ifs.tuwien.ac.at/dm/download/multiChallenge-matrix.txt"
multi <- read_tsv(url, col_names = FALSE) %>% 
  mutate(
    index = rep(seq_len(200), 5),
    key = rep(1:5, each = 200L)
  ) %>%
  filter(key %in% c(1, 4)) %>% 
  mutate(group = rep(c("hierarchical", "flat"), each = 200L)) %>% 
  select(group, X1:X10)


set.seed(143280)
tsne_multi <- Rtsne::Rtsne(multi[, -1])
#embed_multi <- tidy_tsne(tsne_multi, list(group = multi[["group"]]))
#colnames(embed_multi) <- c("tsneX", "tsneY", "group")

tsne_multi_df <- tibble(tsneX = tsne_multi$Y[,1], 
                  tsneY = tsne_multi$Y[,2], 
                  cluster = multi$group)
ggplot(tsne_multi_df, aes(x=tsneX, y=tsneY, colour=cluster)) +
  geom_point() + coord_equal() +
  scale_colour_brewer("", palette="Dark2")

# tourr
library(tourr)
library(RColorBrewer)
animate_xy(multi[,2:11], col=multi$group)
clrs = brewer.pal(3, "Dark2")[as.numeric(factor(multi$group))]
render_gif(multi[,2:11], 
           grand_tour(),
           display_xy(col=clrs, axes="bottomleft"), 
           frames = 200,
           "figures/cluster_example.gif")

render_gif(multi[,2:11], 
           grand_tour(),
           display_xy(col=clrs, axes="bottomleft", half_range = 0.5, center = TRUE), 
           frames = 200,
           "figures/cluster_example_zoom.gif")



