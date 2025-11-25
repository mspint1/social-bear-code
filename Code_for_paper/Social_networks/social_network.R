
######################## TOTAL NETWORK #############################

# Load required packages
library(readxl)
library(dplyr)
library(igraph)
library(ggraph)
library(ggforce)
library(ggnewscale)
library(ggrepel)

# Load data
df <- read_excel("Social_network_Interactions.xlsx")

# Build weighted, undirected graph (weight = count of ≤100 m rows)
edges <- df %>%
  filter(!is.na(Distance_m), Distance_m <= 100) %>%
  mutate(a = pmin(`Bear1 Name`, `Bear2 Name`),
         b = pmax(`Bear1 Name`, `Bear2 Name`)) %>%
  count(a, b, name = "weight") %>%
  rename(from = a, to = b)

g <- graph_from_data_frame(edges, directed = FALSE)
E(g)$weight   <- edges$weight
V(g)$strength <- strength(g, weights = E(g)$weight)

# Louvain packs
set.seed(30)
lv <- cluster_louvain(g, weights = E(g)$weight)
sizes <- sizes(lv); pack_id <- membership(lv)
ord <- order(sizes, decreasing = TRUE)
relabel <- setNames(seq_along(ord), ord)
V(g)$pack <- factor(relabel[as.character(pack_id)])

# Pack layout
set.seed(30)
comm <- as.integer(V(g)$pack)
packs_unique <- sort(unique(comm))
P <- length(packs_unique)

# pack layout with radius
sub_lays  <- vector("list", P)
sub_radii <- numeric(P)
for (i in seq_along(packs_unique)) {
  k   <- packs_unique[i]
  idx <- which(comm == k)
  subg <- induced_subgraph(g, idx)
  
  if (vcount(subg) > 1 && ecount(subg) > 0) {
    L <- layout_with_fr(subg, weights = E(subg)$weight, niter = 1500)
    L <- scale(L, center = TRUE, scale = FALSE)
  } else {
    L <- matrix(0, nrow = length(idx), ncol = 2)
  }
  sub_lays[[i]]  <- L
  sub_radii[i]   <- if (nrow(L) > 1) max(sqrt(rowSums(L^2))) else 0.2
}

# Place pack centroids on a circle 
max_r <- max(sub_radii)
R     <- 1.6 * max_r                     
pack_sizes <- tabulate(comm)
ord_by_size <- order(pack_sizes, decreasing = TRUE)

theta <- seq(0, 2*pi, length.out = P + 1)[-(P + 1)]
theta <- theta[ord_by_size]
centroids <- cbind(R * cos(theta), R * sin(theta))
centroids_back <- matrix(NA_real_, nrow = P, ncol = 2)
centroids_back[ord_by_size, ] <- centroids

# Assemble global layout
intra_scale_base <- 1.7               
lay <- matrix(NA_real_, nrow = vcount(g), ncol = 2)
for (i in seq_along(packs_unique)) {
  k   <- packs_unique[i]
  idx <- which(comm == k)
  L   <- sub_lays[[i]]
  s   <- intra_scale_base * (ifelse(sub_radii[i] > 0, 1 / sub_radii[i], 1))
  Ls  <- s * L
  lay[idx, ] <- sweep(Ls, 2, centroids_back[k, ], FUN = "+")
}

# tiny jitter of the nodes
lay <- lay + matrix(rnorm(length(lay), sd = 0.005), ncol = 2)
# --------------------------------------------------------

# Keep the name of the bear
labs_keep <- V(g)$name

# Manual list of packs
packs <- list(
  "Pack 1" = c("MPG-51-F", "MPG-58-F", "MPG-39-M"),
  "Pack 2" = c("MPG-40-M", "MPG-20-F", "MPG-32-F", "MPG-49-F"),
  "Pack 3" = c("MPG-64-F", "MPG-55-F", "MPG-41-M", "MPG-42-M"),
  "Pack 4" = c("MPG-10-M", "MPG-44-F")
)
pack_df <- data.frame(
  name = unlist(packs),
  pack_border = rep(names(packs), lengths(packs)),
  row.names = NULL
)
node_tbl2 <- data.frame(name = V(g)$name) %>% left_join(pack_df, by = "name")
V(g)$pack_border <- node_tbl2$pack_border

pack_border_cols <- c("Pack 1"="#008080","Pack 2"="#228B22","Pack 3"="#722F37","Pack 4"="#000000")

# setting up specific pack nodes
coords <- as.data.frame(lay); names(coords) <- c("x","y")
coords$name        <- V(g)$name
coords$pack        <- V(g)$pack
coords$pack_border <- V(g)$pack_border
cx <- mean(coords$x); cy <- mean(coords$y)
dx <- coords$x - cx;   dy <- coords$y - cy
r  <- sqrt(dx^2 + dy^2) + 1e-9
k  <- 0.15                          
coords$nx <- k * dx / r
coords$ny <- k * dy / r

# get density of edges
density_val <- edge_density(g, loops = FALSE)



# Plot the network
ggraph(g, layout = lay) +
  geom_edge_link(aes(width = weight, edge_alpha = weight),
                 colour = "grey40", show.legend = TRUE) +
  scale_edge_width(range = c(0.2, 1.3)) +
  scale_edge_alpha(range = c(0.08, 0.6), guide = "none") +
  
  geom_mark_hull(
    aes(x, y, group = pack, fill = pack, color = pack),
    concavity = 5, expand = unit(1.2, "mm"),  
    alpha = 0.10, size = 0.7, show.legend = FALSE
  ) +
  
  geom_node_point(aes(size = strength, color = pack), alpha = 0.95) +
  
  ggrepel::geom_text_repel(
    data = coords[coords$name %in% labs_keep, ],
    inherit.aes = FALSE,
    aes(x = x, y = y, label = name, color = pack),
    size = 3.2, show.legend = FALSE, max.overlaps = Inf,
    nudge_x = coords$nx[coords$name %in% labs_keep],
    nudge_y = coords$ny[coords$name %in% labs_keep],
    point.padding = grid::unit(3, "mm"),
    box.padding   = grid::unit(1.6, "mm"),
    force = 6, force_pull = 0.02,
    min.segment.length = 0, segment.size = 0.25
  ) +
  
  ggnewscale::new_scale_color() +
  geom_node_point(
    data = function(d) d[!is.na(d$pack_border), ],
    aes(x = x, y = y, color = pack_border),
    shape = 21, fill = NA, stroke = 1.8, size = 7, show.legend = FALSE
  ) +
  scale_color_manual(values = pack_border_cols, drop = FALSE) +
  
  labs(color = "Pack", size = "Node strength") +
  theme_void() +
  theme(
    panel.border = ggplot2::element_rect(color = "black", fill = NA),
    plot.margin = margin(30, 40, 30, 40)
  ) +
  coord_cartesian(clip = "off")

###################################### BY YEAR ##############################################

library(readxl)
library(dplyr)
library(igraph)
library(ggraph)
library(ggforce)
library(ggnewscale)
library(ggrepel)

# Set year
YEAR <- 2020
df <- read_excel("Social_network_Interactions.xlsx")
df_y <- df %>% dplyr::filter(Year == YEAR)
bear_to_drop <- ""        

df_y <- df_y %>%
  dplyr::filter(!(`Bear1 Name` == bear_to_drop | `Bear2 Name` == bear_to_drop))


# build an age structure table
age_tbl <- df_y %>%
  dplyr::select(`Bear1 Name`, `Bear1 Age`, `Bear2 Name`, `Bear2 Age`) %>%
  dplyr::rename(
    name1 = `Bear1 Name`, age1 = `Bear1 Age`,
    name2 = `Bear2 Name`, age2 = `Bear2 Age`
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = c(".value","which"),
    names_pattern = "(name|age)(1|2)"
  ) %>%
  dplyr::group_by(name) %>%
  dplyr::summarise(
    age = suppressWarnings(as.numeric(dplyr::first(stats::na.omit(age)))),
    .groups = "drop"
  )

# Build weighted, undirected graph (weight = count of ≤100 m rows)
edges <- df_y %>%
  dplyr::filter(!is.na(Distance_m), Distance_m <= 100) %>%
  dplyr::mutate(
    a = pmin(`Bear1 Name`, `Bear2 Name`),
    b = pmax(`Bear1 Name`, `Bear2 Name`)
  ) %>%
  dplyr::count(a, b, name = "weight") %>%
  dplyr::rename(from = a, to = b)

stopifnot(nrow(edges) > 0)

g <- igraph::graph_from_data_frame(edges, directed = FALSE)
E(g)$weight   <- edges$weight
V(g)$strength <- igraph::strength(g, weights = E(g)$weight)

# attach age to nodes
V(g)$age <- age_tbl$age[match(V(g)$name, age_tbl$name)]

age_to_class <- function(a) {
  dplyr::case_when(
    is.na(a)            ~ NA_character_,
    a < 1               ~ "Cub (<1)",
    a == 1              ~ "Yearling (1)",
    a >= 2  & a <= 3    ~ "Sub-adult (2–3)",
    a >= 4  & a <= 7    ~ "New adult (4–7)",
    a >= 8  & a <= 15   ~ "Middle-aged (8–15)",
    a >= 16             ~ "Old adult (16+)"
  )
}

V(g)$age_class <- factor(
  age_to_class(V(g)$age),
  levels = c("Cub (<1)", "Yearling (1)", "Sub-adult (2–3)",
             "New adult (4–7)", "Middle-aged (8–15)", "Old adult (16+)")
)

# set louvain clusters
set.seed(30)
lv <- igraph::cluster_louvain(g, weights = E(g)$weight)
V(g)$pack <- as.factor(membership(lv))

# set pack id's
set.seed(30)
comm <- as.integer(V(g)$pack)
packs_unique <- sort(unique(comm))
P <- length(packs_unique)

sub_lays  <- vector("list", P)
sub_radii <- numeric(P)
for (i in seq_along(packs_unique)) {
  k   <- packs_unique[i]
  idx <- which(comm == k)
  subg <- igraph::induced_subgraph(g, idx)
  if (igraph::vcount(subg) > 1 && igraph::ecount(subg) > 0) {
    L <- igraph::layout_with_fr(subg, weights = E(subg)$weight, niter = 1500)
    L <- scale(L, center = TRUE, scale = FALSE)
  } else {
    L <- matrix(0, nrow = length(idx), ncol = 2)
  }
  sub_lays[[i]]  <- L
  sub_radii[i]   <- if (nrow(L) > 1) max(sqrt(rowSums(L^2))) else 0.2
}

max_r <- max(sub_radii)
R     <- 1.6 * max_r
theta <- seq(0, 2*pi, length.out = P + 1)[-(P + 1)] 
centroids <- cbind(R * cos(theta), R * sin(theta))
centroids_back <- matrix(NA_real_, nrow = P, ncol = 2)
centroids_back[packs_unique, ] <- centroids

intra_scale_base <- 1.7
lay <- matrix(NA_real_, nrow = igraph::vcount(g), ncol = 2)
for (i in seq_along(packs_unique)) {
  k   <- packs_unique[i]
  idx <- which(comm == k)
  L   <- sub_lays[[i]]
  s   <- intra_scale_base * (ifelse(sub_radii[i] > 0, 1 / sub_radii[i], 1))
  Ls  <- s * L
  lay[idx, ] <- sweep(Ls, 2, centroids_back[k, ], FUN = "+")
}
lay <- lay + matrix(rnorm(length(lay), sd = 0.005), ncol = 2)

# Set repel and coords
coords <- as.data.frame(lay)
names(coords) <- c("x","y")
coords$name <- V(g)$name
coords$pack <- V(g)$pack
cx <- mean(coords$x); cy <- mean(coords$y)
dx <- coords$x - cx;  dy <- coords$y - cy
r  <- sqrt(dx^2 + dy^2) + 1e-9
k  <- 0.15
coords$nx <- k * dx / r
coords$ny <- k * dy / r

V(g)$x <- lay[,1]
V(g)$y <- lay[,2]
V(g)$label <- V(g)$name

# save age classes and packs
V(g)$age_class <- as.character(V(g)$age_class)
V(g)$pack      <- as.character(V(g)$pack)

# Save to a graphml for work in CYTOSCAPE
# igraph::write_graph(g, file = "bears_2023.graphml", format = "graphml")

# Plot network
ggraph(g, layout = lay) +
  geom_edge_link(aes(width = weight, edge_alpha = weight),
                 colour = "grey40", show.legend = TRUE) +
  scale_edge_width(range = c(0.6, 1.2)) +
  scale_edge_alpha(range = c(0.30, 0.80), guide = "none") +
  geom_mark_hull(
    aes(x, y, group = pack, fill = pack, color = pack),
    concavity = 5, expand = unit(1.2, "mm"),
    alpha = 0.10, size = 0.7, show.legend = FALSE
  ) +
  ggnewscale::new_scale_fill() +
  geom_node_point(aes(size = strength, color = pack, fill = age_class),
                  alpha = 0.95, shape = 21, stroke = 0.6) +
  scale_fill_viridis_d(name = "Age class", option = "C", drop = TRUE) +
  ggrepel::geom_text_repel(
    data = coords[coords$name %in% V(g)$name, ],
    inherit.aes = FALSE,
    aes(x = x, y = y, label = name, color = pack),
    size = 3.2, fontface = "bold", show.legend = FALSE, max.overlaps = Inf,
    nudge_x = coords$nx[coords$name %in% V(g)$name],
    nudge_y = coords$ny[coords$name %in% V(g)$name],
    point.padding = grid::unit(3, "mm"),
    box.padding   = grid::unit(1.6, "mm"),
    force = 6, force_pull = 0.02,
    min.segment.length = 0, segment.size = 0.25
  ) +
  labs(
    title = YEAR,
    color = "Pack",
    size = "Node strength",
    fill  = "Age class"
  ) +
  theme_void() +
  theme(
    text = element_text(family = "ArialMT"),
    panel.border = element_rect(color = "black", fill = NA),
    plot.margin = margin(30, 40, 30, 40),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  ) +
  coord_cartesian(clip = "off")

# ---- Optional summary for tracking ----
pack_summary <- data.frame(
  Year = YEAR,
  Bear = V(g)$name,
  Pack = V(g)$pack,
  Strength = V(g)$strength,
  Age = V(g)$age,
  Age_class = V(g)$age_class
)
print(pack_summary)



#############################TOTAL NETWORK CYTOSCAPE #######################

# read data
df <- read_excel("Social_network_Interactions.xlsx")

edges <- df %>%
  filter(!is.na(Distance_m), Distance_m <= 100) %>%
  mutate(
    a = pmin(`Bear1 Name`, `Bear2 Name`),
    b = pmax(`Bear1 Name`, `Bear2 Name`)
  ) %>%
  count(a, b, name = "weight") %>%
  rename(from = a, to = b)

# Create graph
g <- graph_from_data_frame(edges, directed = FALSE)

# Add attributes
E(g)$weight   <- edges$weight
V(g)$strength <- strength(g, weights = E(g)$weight)

# Louviain community
set.seed(30)
lv <- cluster_louvain(g, weights = E(g)$weight)
sizes <- sizes(lv)
pack_id <- membership(lv)

ord <- order(sizes, decreasing = TRUE)
relabel <- setNames(seq_along(ord), ord)
V(g)$pack <- factor(relabel[as.character(pack_id)])

# Pack layout
set.seed(30)
comm <- as.integer(V(g)$pack)
packs_unique <- sort(unique(comm))
P <- length(packs_unique)

sub_lays  <- vector("list", P)
sub_radii <- numeric(P)

for (i in seq_along(packs_unique)) {
  k   <- packs_unique[i]
  idx <- which(comm == k)
  subg <- induced_subgraph(g, idx)
  
  if (vcount(subg) > 1 && ecount(subg) > 0) {
    L <- layout_with_fr(subg, weights = E(subg)$weight, niter = 1500)
    L <- scale(L, center = TRUE, scale = FALSE)
  } else {
    L <- matrix(0, nrow = length(idx), ncol = 2)
  }
  
  sub_lays[[i]]  <- L
  sub_radii[i]   <- if (nrow(L) > 1) max(sqrt(rowSums(L^2))) else 0.2
}

max_r <- max(sub_radii)
R     <- 1.6 * max_r
pack_sizes <- tabulate(comm)
ord_by_size <- order(pack_sizes, decreasing = TRUE)

theta <- seq(0, 2*pi, length.out = P + 1)[-(P + 1)]
theta <- theta[ord_by_size]
centroids <- cbind(R * cos(theta), R * sin(theta))
centroids_back <- matrix(NA_real_, nrow = P, ncol = 2)
centroids_back[ord_by_size, ] <- centroids

intra_scale_base <- 1.7
lay <- matrix(NA_real_, nrow = vcount(g), ncol = 2)

for (i in seq_along(packs_unique)) {
  k   <- packs_unique[i]
  idx <- which(comm == k)
  L   <- sub_lays[[i]]
  s   <- intra_scale_base * (ifelse(sub_radii[i] > 0, 1 / sub_radii[i], 1))
  Ls  <- s * L
  lay[idx, ] <- sweep(Ls, 2, centroids_back[k, ], FUN = "+")
}

# Small jitter for visualization
lay <- lay + matrix(rnorm(length(lay), sd = 0.005), ncol = 2)

# Save attributes for export
V(g)$x <- lay[, 1]
V(g)$y <- lay[, 2]
V(g)$label <- V(g)$name
V(g)$pack <- as.character(V(g)$pack)
V(g)$strength <- as.numeric(V(g)$strength)
E(g)$weight <- as.numeric(E(g)$weight)

# Replace NA attributes with blanks
for (attr in c("pack", "label")) {
  vals <- get.vertex.attribute(g, attr)
  vals[is.na(vals)] <- ""
  g <- set_vertex_attr(g, attr, value = vals)
}

# Export graph for CYTOSCAPE
write_graph(g, "Total_Social_Network.graphml", format = "graphml")
