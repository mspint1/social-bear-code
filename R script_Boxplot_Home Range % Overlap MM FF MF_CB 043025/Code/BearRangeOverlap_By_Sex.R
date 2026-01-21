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


#Code for MPG Bears Range Overlap by Pair Type (MM, MF, FF), 2020-2024. 
#The Excel data to read in should have 3 columns: M/M pair overlaps, M/F pair overlaps, F/F pair overlaps. 
boxplot <- read_excel("Docs_needed_for_code/summary_overlap.xlsx")

# Filter varead_excel()# Filter valid data
overlap_data <- boxplot %>%
  filter(!is.na(Year), !is.na(Type), !is.na(Final_Value))

# Count number of bear pairs per Year-Type group
count_data <- overlap_data %>%
  group_by(Year, Type) %>%
  summarise(N_Pairs = n(), .groups = "drop")

# Create scaled count for dual y-axis bar height
scale_factor <- 1  
count_data <- count_data %>%
  mutate(scaled_N = N_Pairs * scale_factor)


#Add wilcox test num
stat_results <- compare_means(
  Final_Value ~ Type,
  data = overlap_data,
  method = "wilcox.test",
  group.by = "Year",
  comparisons = my_comparisons
)
# Define theme
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
# Plot
geom_boxplot(data = overlap_data,
             aes(x = factor(Year), y = Final_Value, fill = Type),
             position = position_dodge(width = 0.7),
             width = 0.6,
             color = "black")+
  
  
  # Color settings
  scale_fill_manual(values = c("FF" = "#CC6677", "MF" = "#332288", "MM" = "#DDCC77")) +
  
  # Dual Y-axis
  scale_y_continuous(
    name = "Homerange overlap (%)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Number of bear pairs")
  ) +
  My_Theme +
  labs(
    x = NULL
  )

########################## Code REQUIRED to create Fig1D ####################################

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

# 1. Load and clean data
boxplot <- read_excel("Docs_needed_for_code/summary_overlap.xlsx")

overlap_data <- boxplot %>%
  filter(!is.na(Type), !is.na(Final_Value)) %>%
  # This line ensures the boxes appear in the order: FF, MF, MM
  mutate(Type = factor(Type, levels = c("FF", "MF", "MM")))

# 2. Define specific pairs
my_comparisons <- list( c("FF", "MM"), c("MF", "MM"), c("FF", "MF") )

# 3. Define Theme
My_Theme <- theme_bw() +
  theme(
    axis.title.x = element_blank(), 
    axis.text.x = element_text(angle=45, colour="black", vjust=1, hjust=1, size=12, family="Arial"),
    axis.text.y = element_text(colour="black", size=12, family="Arial"),
    axis.title.y = element_text(size=12, family="Arial"),
    legend.position = "none",       
    panel.border = element_rect(fill = NA, colour = "black"),
    strip.background = element_rect(colour="black")
  )

# 4. Plotting
ggplot(overlap_data, aes(x = Type, y = Final_Value, fill = Type)) +
  geom_boxplot(width = 0.5, color = "black", outlier.shape = NA) +
  scale_fill_manual(values = c("FF" = "#CC6677", "MF" = "#332288", "MM" = "#DDCC77")) +
  stat_compare_means(
    comparisons = my_comparisons,
    method = "wilcox.test", 
    label = "p.signif",   
    vjust = 0.5
  ) +
  scale_y_continuous(name = "Homerange overlap (%)", limits = c(0, 140)) + 
  My_Theme
