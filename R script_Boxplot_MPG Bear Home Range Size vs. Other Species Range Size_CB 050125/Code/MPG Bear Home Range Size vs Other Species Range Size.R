# Load required packages
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


# Code for MPG Bears vs. Other Species Range Size. 
# The Excel data to load in should have columns for MPG Male/Female bear home range sizes (km2) as well as other species columns for their home range sizes.

# Read the data
female_boxplot <- read_excel("Docs_needed_for_code/Female_HRsize.xlsx") 
male_boxplot <- read_excel("Docs_needed_for_code/Male_HRsize.xlsx")
all_sexes_boxplot <- read_excel("Docs_needed_for_code/All_sexes_HRsize.xlsx")
  
# Transform data into long format
data_long <- pivot_longer(all_sexes_boxplot, cols = -Sex, names_to = "Group", values_to = "Value")

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
        strip.text.x = element_text(size=10, family = "Arial"),
        strip.text.y = element_text(size=10, family = "Arial"),
        panel.border = element_rect(fill = NA, colour = "black"),
        strip.background = element_rect(colour="black"))


# Compute mean for each group
group_means <- data_long %>%
  group_by(Group) %>%
  summarise(mean_value = mean(Value, na.rm = TRUE)) %>%
  arrange(mean_value)
data_long$Group

# Order the Group factor based on the computed means
data_long$Group <- factor(data_long$Group, levels = group_means$Group)

# Plot
long_boxplot <- ggplot(data_long, aes(x = Group, y = Value, fill = Group)) +
  geom_boxplot(width = 0.8)+
  theme_minimal() +
  scale_fill_manual(values = c("MPG bears"="bisque", "Mountain lion"="cadetblue", "Brown hyena"="lightpink",
                               "Brown bear"="lightgreen", "Grey fox"="lightcoral", "Moose"="lightgoldenrod",
                               "Skunk"="skyblue", "Rattlesnake"="plum", "Wolverine"="salmon", "Other"="khaki")) +
  My_Theme +
  labs(y = "Homerange size (km²)", x = NULL)

#### Facetted by species/sex
facet_data_long <- all_sexes_boxplot |>
  pivot_longer(cols = -Sex, names_to = "Group", values_to = "Value") |>
  drop_na(Sex, Value, Group)|>
  mutate(Group_Sex = paste(Group, Sex, sep = "_"),
         Group = fct_reorder(Group, Value, .fun = median, .desc = FALSE)  # reorder by median
  )

# Plot
ggplot(facet_data_long, aes(x = Group, y = Value, fill = Sex)) +
  geom_boxplot(width = 0.7, outlier.shape = NA) +
  facet_wrap(~ Sex, scales = "free_x") +
  scale_fill_manual(values = c("M" = "#DDCC77", "F" = "#CC6677")) +
  My_Theme +
  labs(y = "Homerange size (km²)", x = NULL, fill = "Sex") 


### Female only
ggplot(facet_data_long %>% filter(Sex == "F"), 
       aes(x = reorder(Group, Value, FUN = median), y = Value, fill = Sex)) +
  geom_boxplot(width = 0.7) +
  scale_fill_manual(values = c("F" = "#CC6677")) +
  My_Theme +
  labs(
    y = "Homerange size (km²)", 
    x = NULL
  )

### Male only
ggplot(facet_data_long %>% filter(Sex == "M"), 
       aes(x = reorder(Group, Value, FUN = median), y = Value, fill = Sex)) +
  geom_boxplot(width = 0.7) +
  scale_fill_manual(values = c("M" = "#DDCC77")) +
  My_Theme +
  labs(
    y = "Homerange size (km²)", 
    x = NULL
  )

### Triple Plot
mf_data <- facet_data_long
all_data <- facet_data_long |>
  group_by(Group) |>
  mutate(Sex = "All")
triple_data <- bind_rows(mf_data, all_data)

## Non Log
ggplot(triple_data, aes(x = Group, y = Value, fill = Sex)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.6, outlier.shape = NA) +
  scale_fill_manual(values = c("F" = "#CC6677", "M" = "#DDCC77", "All" = "gray80")) +
  My_Theme +
  labs(y = "Range Size (km²)", x = NULL, fill = "Sex") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Log
ggplot(triple_data, aes(x = Group, y = Value, fill = Sex)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.6, outlier.shape = NA) +
  scale_y_log10() +  # ← KEY CHANGE
  scale_fill_manual(values = c("F" = "#CC6677", "M" = "#DDCC77", "All" = "gray80")) +
  My_Theme +
  labs(y = "Range Size (km², log scale)", x = "Species", fill = "Sex") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###
ggsave(
  filename = "Figs_created/FigS6B.pdf",
  plot = last_plot(),                 
  device = cairo_pdf,                     
  width = 5,                        # Matches composition of other figures
  height = 4,                         
  units = "in",                      
  dpi = 300,                         
)

