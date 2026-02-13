# Load required libraries
library(readr)
library(tidyr)
library(dplyr)
library(igraph)
library(ggraph)
library(tidygraph)
library(tibble)
library(pheatmap)

# Load data
bear_patch_data <- read_csv("Network_analysis_patches/Hours_visited_1week_list_042825.csv")

# Pivot to wide format: BearID × Locations
bear_patch_data_wide <- bear_patch_data %>%
  pivot_wider(
    names_from = Location,
    values_from = Duration_hr,
    values_fill = list(Duration_hr = 0)
  )


# Calculate correlation
Duration_hr_matrix <- bear_patch_data_wide %>% select(-BearID)
cor_matrix <- cor(Duration_hr_matrix)

# Turn into a long table
cor_df <- as.data.frame(as.table(cor_matrix))
names(cor_df) <- c("Bear1", "Bear2", "Correlation")

# Filter for significant correlations (and remove self-loops)
cor_df_filtered <- cor_df %>%
  filter(Bear1 != Bear2) %>%
  filter(Correlation > 0.5)

# Map numeric row numbers back to BearIDs
# Because Duration_hr_matrix loses BearID names when you select(-BearID), 
# we manually map it back using rownames

# Assign BearID as row names before correlation
row.names(Duration_hr_matrix) <- bear_patch_data_wide$BearID

# redo correlation after assigning rownames:
cor_matrix <- cor(t(Duration_hr_matrix))

# Convert again to long format
cor_df <- as.data.frame(as.table(cor_matrix))
names(cor_df) <- c("Bear1", "Bear2", "Correlation")

# Filter
cor_df_filtered <- cor_df %>%
  filter(Bear1 != Bear2) %>%
  filter(Correlation > 0.5)

# Create vertices using BearIDs
vertices <- data.frame(name = unique(c(cor_df_filtered$Bear1, cor_df_filtered$Bear2)))

# Create graph with BearIDs
g <- graph_from_data_frame(d = cor_df_filtered, vertices = vertices, directed = FALSE)

# plot with ggraph
g_tbl <- as_tbl_graph(g)

ggraph(g_tbl, layout = 'fr') +
  geom_edge_link(aes(width = Correlation), alpha = 0.8) +
  geom_node_point(size = 5, color = "skyblue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  ggtitle("Correlation Network of BearIDs")



# Calculate total duration per BearID
bear_totals <- bear_patch_data_wide %>%
  mutate(Total_Hours = rowSums(select(., -BearID))) %>%
  select(BearID, Total_Hours)

# Add it to the graph
g_tbl <- g_tbl %>%
  left_join(bear_totals, by = c("name" = "BearID"))

# Plot with node size scaled by total hours
ggraph(g_tbl, layout = 'fr') +
  geom_edge_link(aes(width = Correlation), alpha = 0.8) +
  geom_node_point(aes(size = Total_Hours), color = "skyblue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  ggtitle("Correlation Network of BearIDs (Node Size = Total Hours)")

set.seed(5)
ggraph(g_tbl, layout = 'fr') +
  geom_edge_link(aes(width = Correlation), alpha = 0.8) +
  geom_node_point(aes(size = Total_Hours), color = "skyblue") +
  geom_node_text(aes(label = name),
                 repel = TRUE, 
                 position = position_jitter(width = 0.2, height = 0.3),# <- Repel = TRUE makes labels repel from each other (automatically jitters them)
                 size = 3,
                 force = 2,        # <- You can adjust "force" if you want stronger repelling
                 max.overlaps = 100) +  # <- Allow lots of overlaps to be resolved
  theme_void() +
  scale_edge_width(range = c(0.1, 1))+
  ggtitle("Correlation Network based on hours at patches")

# Degree centrality
deg_centrality <- degree(g, mode = "all")

# Betweenness centrality
bet_centrality <- betweenness(g, normalized = TRUE)

# Closeness centrality
clo_centrality <- closeness(g, normalized = TRUE)

# Eigenvector centrality
eig_centrality <- eigen_centrality(g)$vector

#make table
centrality_table <- data.frame(
  BearID = names(deg_centrality),
  Degree = deg_centrality,
  Betweenness = bet_centrality,
  Closeness = clo_centrality,
  Eigenvector = eig_centrality
)

# Add centrality to graph object
V(g)$degree <- deg_centrality

# Redo plotting
g_tbl <- as_tbl_graph(g)
# Bring in Total_Hours again
g_tbl <- g_tbl %>%
  left_join(bear_totals, by = c("name" = "BearID"))

set.seed(10)

ggraph(g_tbl, layout = 'fr') + #layouts = 'kk'  "graphopt" 'fr'
  geom_edge_link(aes(width = Correlation), alpha = 0.8) +
  geom_node_point(aes(size = Total_Hours, color = degree)) +  # Size and color by degree centrality
  scale_color_viridis_c() +
  theme_void() +
  scale_edge_width(range = c(0.1, 1))


# Calculate the density of the graph
network_density <- graph.density(g)

#community density
# Perform community detection using the Louvain algorithm
community_louvain <- cluster_louvain(g)

# Print the community assignments (which bear belongs to which community)
print(community_louvain)

# Extract the modularity score
modularity_score <- modularity(community_louvain)
cat("Modularity: ", modularity_score, "\n")

# Add the community membership to the graph vertices
V(g)$community <- community_louvain$membership

# Convert the graph to a tidygraph object (g_tbl) again
g_tbl <- as_tbl_graph(g)

# Add the community membership to the g_tbl
g_tbl <- g_tbl %>%
  left_join(data.frame(name = V(g)$name, community = V(g)$community), by = "name")

# Now plot with communities
Louvain <- ggraph(g_tbl, layout = 'fr') +
  geom_edge_link(aes(width = Correlation), alpha = 0.8) +
  geom_node_point(aes(color = factor(community.x)), size = 5) + # Color nodes by community
  geom_node_text(aes(label = NA), repel = TRUE, position = position_jitter(width = 1, height = 1)) +
  theme_void()+
  scale_edge_width(range = c(0.1, 1))+
  ggtitle("Correlation Network with Louvain Communities"
          
)

# Pairwise overlaps
# Load data
bear_patch_overlap_data <- read_csv("Pairwise_overlaps_1week_list_042825.csv")

# aggregate overlaps
aggregated_data <- bear_patch_overlap_data %>%
  group_by(Bear_1, Bear_2) %>%
  summarize(Hours_Overlap = sum(Hours_Overlap), .groups = 'drop')

# make the data symmetric
swapped <- aggregated_data %>%
  rename(Bear_1_tmp = Bear_1, Bear_2_tmp = Bear_2) %>%
  transmute(Bear_1 = Bear_2_tmp, Bear_2 = Bear_1_tmp, Hours_Overlap = Hours_Overlap)

full_data <- bind_rows(aggregated_data, swapped)

# set up bear pairs
all_bears <- unique(c(full_data$Bear_1, full_data$Bear_2))

full_pairs <- expand.grid(Bear_1 = all_bears, Bear_2 = all_bears)

# join data
full_joined <- left_join(full_pairs, full_data, by = c("Bear_1", "Bear_2"))

# Replace NA with 0 (no overlap recorded between those bears)
full_joined$Hours_Overlap[is.na(full_joined$Hours_Overlap)] <- 0

# turn into matrix
heatmap_matrix <- full_joined %>%
  pivot_wider(names_from = Bear_2, values_from = Hours_Overlap) %>%
  column_to_rownames("Bear_1") %>%
  as.matrix()

# remove self comparisons
diag(heatmap_matrix) <- NA

# values for scaling
max_overlap <- max(heatmap_matrix, na.rm = TRUE)

# plot the heatmap
heatmap <- pheatmap(heatmap_matrix,
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         display_numbers = TRUE,
         na_col = "white",
         color = colorRampPalette(c("white", "red"))(50),
         breaks = seq(0, max_overlap, length.out = 51),
         main = "Bear Hours Overlap Heatmap")

## Save Script
ggsave(
  filename = "Louvain_Communities_180mm.pdf",
  plot = Louvain,
  device = cairo_pdf(),
  width = 3.35,          # in inches (85 mm or 3.35)
  height = 4,            
  units = "in",
  dpi = 300
)
