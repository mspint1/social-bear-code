# Load required libraries 
library(readxl)     
library(ggplot2)    
library(dplyr)      
library(rstatix)    
library(ggpubr)     

# Load data
df <- read_excel("Docs_needed_for_code/GD_onlystudybears.xlsx")  

# Creating packs based on Network Analysis
pack1 <- c("MPGRanch44-F","MPGRanch10-M") #Rikki and Back Float Bear
pack2 <- c("MPGRanch64-F", "MPGRanch55-F", "MPGRanch42-M", "MPGRanch41-M") #Squirt, Dahlia, Ham, Green Eggs
pack3 <- c("MPGRanch51-F", "MPGRanch58-F", "MPGRanch39-M") #Splitear, Athena, ZZ
pack4 <- c("MPGRanch40-M", "MPGRanch20-F", "MPGRanch32-F", "MPGRanch49-F")# Troublemaker, Coca Cola, Fanta, Bombshell

network_bears <- c(pack1, pack2, pack3, pack4)

# Extract labels
row_labels <- df[[ncol(df)]]       
data_matrix <- df[, -ncol(df)]     
col_labels <- colnames(data_matrix) 

# Create an empty dataframe
results <- data.frame(
  Type = character(),   
  Value = numeric(),   
  Bear1 = character(),  
  Bear2 = character()   
)

# Loop through and extract values
for (i in seq_along(row_labels)) {
  for (j in seq_along(col_labels)) {
    x <- row_labels[i]                 
    y <- col_labels[j]                 
    value <- data_matrix[i, j][[1]]   
    
    if (is.na(x) || is.na(y)) next
    if (x == "" || y == "") next
    if (x == y) next
    if (is.na(value)) next
    
    # Classify and store
    if (grepl("-M$", x) && grepl("-M$", y)) {
      results <- rbind(results, data.frame(Type = "MM", Value = value, Bear1 = x, Bear2 = y))
    } else if (grepl("-F$", x) && grepl("-F$", y)) {
      results <- rbind(results, data.frame(Type = "FF", Value = value, Bear1 = x, Bear2 = y))
    } else if ((grepl("-M$", x) && grepl("-F$", y)) || (grepl("-F$", x) && grepl("-M$", y))) {
      results <- rbind(results, data.frame(Type = "MF", Value = value, Bear1 = x, Bear2 = y))
    }
  }
}


# Statistics comparison
sex_stat_comparisons <- list(
  c("MM", "FF"),
  c("MM", "MF"),
  c("FF", "MF")
)

# Change type to factor
results$Type <- factor(results$Type, levels = c("FF", "MF", "MM"))


# Define theme
My_Theme = theme_bw() +
  theme(axis.title.x = element_text(size=12, family = "Arial"),
        axis.text.x = element_text(angle=45, colour="black", 
                                   vjust=1, hjust=1, size=12, family = "Arial"),
        axis.text.y = element_text(colour = "black", size=12, family = "Arial"),
        axis.title.y = element_text(size=12, family = "Arial"),
        plot.title = element_text(size=12, family = "Arial"),
        legend.title = element_text(face = "bold", size=12, family = "Arial"),
        legend.text = element_text(size = 12, family = "Arial"),
        legend.position = "none",  
        legend.key.size = unit(1, "cm"),
        strip.text.x = element_text(size=12, family = "Arial"),
        strip.text.y = element_text(size=12, family = "Arial"),
        panel.border = element_rect(fill = NA, colour = "black"),
        strip.background = element_rect(colour="black"))

# Plot
boxplot <- ggplot(results, aes(x = Type, y = Value, fill = Type)) +
  geom_boxplot() +              
  scale_y_continuous(
    breaks = seq(0, 30, 5),     
    limits = c(0, 30)           
  ) +
  labs(
    title = NULL,
    x = NULL,
    y = "Genetic distance"
  ) +
  My_Theme +
  scale_fill_manual(values = c("FF" = "#CC6677", "MF" = "#332288", "MM" = "#DDCC77")) +
  stat_compare_means(
    comparisons = sex_stat_comparisons,
    method = "wilcox.test",
    exact = TRUE,
    label = "p.signif"
  )

# Run statistics test
test <- wilcox.test(Value ~ Type, data = results, exact = FALSE)

#Subset data for each sex type
mm <- subset(results, Type == "MM")$Value
ff <- subset(results, Type == "FF")$Value
mf <- subset(results, Type == "MF")$Value

# Get p.values for each sex type
test_mm_ff <- wilcox.test(mm, ff, exact = FALSE)
test_mm_mf <- wilcox.test(mm, mf, exact = FALSE)
test_ff_mf <- wilcox.test(ff, mf, exact = FALSE)

# Get effect size for MM/FF types.
results %>%
  filter(Type %in% c("MM", "FF")) %>%
  wilcox_effsize(Value ~ Type)

# Get effect size for FF/MF types.
results %>%
  filter(Type %in% c("FF", "MF")) %>%
  droplevels() %>%
  mutate(Type = as.character(Type)) %>%  
  wilcox_effsize(Value ~ Type)

# Get effect size for MM/MF types.
results %>%
  filter(Type %in% c("MM", "MF")) %>%
  droplevels() %>%
  mutate(Type = as.character(Type)) %>% 
  wilcox_effsize(Value ~ Type)


# Plotting GD for all bears

# Seperate the Data
results$Group <- case_when(
  results$Bear1 %in% pack1 & results$Bear2 %in% pack1 ~ "Pack1",
  results$Bear1 %in% pack2 & results$Bear2 %in% pack2 ~ "Pack2",
  results$Bear1 %in% pack3 & results$Bear2 %in% pack3 ~ "Pack3",
  results$Bear1 %in% pack4 & results$Bear2 %in% pack4 ~ "Pack4",
  results$Bear1 %in% network_bears & results$Bear2 %in% network_bears ~ "Network bears",
  TRUE ~ "All bears"
)

# Set factor level order for plotting
group_levels <- c("All bears", "Network bears", "Pack1", "Pack2", "Pack3", "Pack4")
results$Group <- factor(results$Group, levels = group_levels)

# Get a comparison list
sig_comparisons <- list(
  c("All bears", "Network bears"),
  c("All bears", "Pack2"),
  c("Network bears", "Pack2")
)

# Get the results of the wilcox test
pairwise.wilcox.test(results$Value, results$Group, p.adjust.method = "BH")

# Set up data for dual y-axis
group_sizes <- data.frame(
  Group = c("All bears", "Network bears", "Pack1", "Pack2", "Pack3", "Pack4"),
  Num_Bears = c(
    length(unique(c(results$Bear1[results$Group == "All bears"], results$Bear2[results$Group == "All_Bears"]))),
    length(unique(c(results$Bear1[results$Group == "Network bears"], results$Bear2[results$Group == "Network_Bears"]))),
    length(unique(c(results$Bear1[results$Group == "Pack1"], results$Bear2[results$Group == "Pack1"]))),
    length(unique(c(results$Bear1[results$Group == "Pack2"], results$Bear2[results$Group == "Pack2"]))),
    length(unique(c(results$Bear1[results$Group == "Pack3"], results$Bear2[results$Group == "Pack3"]))),
    length(unique(c(results$Bear1[results$Group == "Pack4"], results$Bear2[results$Group == "Pack4"])))
  )
)

# Scale to GD 
max_dist <- max(results$Value, na.rm = TRUE)
group_sizes <- group_sizes %>%
  mutate(Scaled_Count = Num_Bears / max(Num_Bears) * max_dist,
         Group = factor(Group, levels = group_levels))
# Plot
network_plot <- ggplot(results, aes(x = Group, y = Value, fill = Group)) +
  geom_col(data = group_sizes,
           aes(x = Group, y = Scaled_Count, fill = Group),
           width = 0.5,
           alpha = 0.2,
           inherit.aes = FALSE) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  stat_compare_means(
    comparisons = sig_comparisons,
    method = "wilcox.test",
    exact = TRUE,
    label = "p.signif",
    step.increase = 0.1
  ) +
  labs(x = NULL, y = "Genetic Distance") +
  My_Theme +
  scale_fill_manual(values = c(
    "All bears" = "#999999",
    "Network bears" = "#E69F00",
    "Pack1" = "black",
    "Pack2" = "red",
    "Pack3" = "#0072B2",
    "Pack4" = "forestgreen"
  )) +

  
  # Dual y-axis (Num Bears)
  scale_y_continuous(
    name = "Genetic distance",
    sec.axis = sec_axis(
      trans = ~ . * max(group_sizes$Num_Bears) / max_dist,
      name = "Number of bears"
    )
  )


## Save Script
ggsave(
  filename = "Figs_created/Fig4B.pdf",
  plot = last_plot(),
  device = cairo_pdf,
  width = 6.8,          # 6.8 matches other figures in the composition
  height = 5,            # 5 matches other figures in the composition
  units = "in",
  dpi = 300,
)

