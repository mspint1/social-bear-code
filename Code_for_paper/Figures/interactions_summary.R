# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Define theme for plots
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

# Load data
df <- read_csv("Interactions_summary.csv")   

# Mutate data for easier plotting
data_long <- df |>
  pivot_longer(
    cols = everything(),
    names_to = "Interaction",
    values_to = "Events")
data_long <- data_long %>%
  mutate(Interaction = reorder(Interaction, Events, .fun = median, na.rm = TRUE))

# Define comparisons for statistics
comparisons <- list(
  c("Avoidance", "Non-tracking"),
  c("Avoidance", "Synchronous"),
  c("Avoidance", "Tracking"),
  c("Non-tracking", "Synchronous"),
  c("Non-tracking", "Tracking"),
  c("Synchronous", "Tracking")
)

# Save the statistics
test <- wilcox.test(Interaction ~ Type, data = df, exact = FALSE)

# Plot
ggplot(data_long, aes(x = Interaction, y = Events)) +
  geom_boxplot(width = 0.6) + 
  scale_y_log10() +
  labs(x = NULL, y = "Number of events") +
  My_Theme +
  stat_compare_means(
    method = "wilcox.test",
    label = "p.signif",
    comparisons = comparisons,
    tip.length = 0.01,
    hide.ns = TRUE,
    vjust = 0.55
  )

ggsave(
  filename = "Bear_Codes/Figures/Fig3B.pdf",   
  plot = last_plot(),                  
  device = cairo_pdf,                     
  width = 3.35,                        # 85 mm
  height = 4,                          
  units = "in",                        
  dpi = 300,                           
)


# Begin getting percent of interactions
totals <- df |>
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))

long_df <- totals %>%
  pivot_longer(cols = everything(),
               names_to = "Behavior",
               values_to = "Total") %>%
  mutate(Percent = Total / sum(Total) * 100)

long_df <- long_df %>%
  mutate(Behavior = reorder(Behavior, Percent, .desc = FALSE))

# Plot percentage of interaction
ggplot(long_df, aes(x = Behavior, y = Percent)) +
  geom_col(width = 0.4) +
  geom_text(aes(label = paste0(round(Percent, 1), "%")),
            vjust = -0.5, size = 3) +
  labs(
       x = NULL, y = "Percentage of interaction (%)") +
  My_Theme
