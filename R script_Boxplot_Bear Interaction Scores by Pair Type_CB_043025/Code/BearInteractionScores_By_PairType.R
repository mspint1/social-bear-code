# Load required libraries
library(tidyverse)   
library(ggplot2)     
library(dplyr)       
library(tidyr)       
library(forcats)     
library(readxl)      
library(writexl)     
library(ggpubr)      
library(rstatix)     
library(ggrepel)    

# Load the Excel file with interaction strengths by pair type
# Expected columns: M.M, M.F, F.F (values multiplied by 100 for visibility)
boxplot <- read_excel("Docs_needed_for_code/2020_2024_Boxplot_BySex.xlsx")

# Convert the data to long format with 'Group' (pair type) and 'Value' columns
data_long <- pivot_longer(boxplot, cols = everything(), names_to = "Group", values_to = "Value")

# Filter out rows with missing or negative values (invalid data)
data_long <- data_long %>%
  filter(!is.na(Value) & Value >= 0)

# Clean up group labels for clarity in plots
data_long$Group <- recode(data_long$Group,
                          "Female-Female" = "FF",
                          "Male-Female" = "MF",
                          "Male-Male" = "MM")


# Add sample size labels to the main data frame
data_long <- left_join(data_long, group_counts, by = "Group")

# Reorder factor levels (GroupLabel) by mean interaction value for better plot ordering
data_long <- data_long %>%
  group_by(GroupLabel) %>%
  mutate(mean_value = mean(Value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(GroupLabel = fct_reorder(GroupLabel, mean_value))

# Define a theme
My_Theme = theme_bw() +
  theme(axis.title.x = element_text(size=12, family = "Arial"),
        axis.text.x = element_text(angle=45, colour="black", 
                                   vjust=1, hjust=1, size=12, family = "Arial"),
        axis.text.y = element_text(colour = "black", size=12, family = "Arial"),
        axis.title.y = element_text(size=12, family = "Arial"),
        plot.title = element_text(size=12, family = "Arial"),
        legend.title = element_text(size=10, family = "Arial"),
        legend.position = "none",
        legend.text = element_text(size = 10, family = "Arial"),
        legend.key.size = unit(1, "cm"),
        strip.text.x = element_text(size=12, family = "Arial"),
        strip.text.y = element_text(size=12, family = "Arial"),
        panel.border = element_rect(fill = NA, colour = "black"),
        strip.background = element_rect(colour="black"))

# Create the boxplot with log10 y-scale and custom colors per group
long_boxplot <- ggplot(data_long, aes(x = Group, y = Value, fill = Group)) +
  geom_boxplot(color = "black", outlier.shape = TRUE)+
  scale_y_log10(limits = c(1, 1000)) +  # Log scale for better visualization of skewed data
  stat_compare_means(
    comparisons = list(c("FF", "MF"), c("FF", "MM"), c("MF", "MM")),
    method = "wilcox.test",
    label = "p.signif",
    hide.ns = FALSE
  ) +
  scale_fill_manual(values = c(
    "FF" = "#CC6677",
    "MF" = "#332288",
    "MM" = "#DDCC77"
  )) +
  My_Theme +
  labs(
    x = NULL,
    y = "Interaction rate (log10)"
  )

# Save the plot
ggsave(
  filename = "Fig_created/FigS7B.pdf",  
  plot = last_plot(),               
  device = cairo_pdf,                     
  width = 3.35,                        # 85 mm
  height = 4,                          
  units = "in",                        
  dpi = 300,                           
)
