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

#Code for Male-Female Home range size Box Plot. 
# Excel data to load in should contain 2 columns: Males and Females. The columns should contain individual bear home range sizes (km2) which have been averaged across their annual home range sizes (e.g. Fanta's home range for 2021, 2022, and 2023 have been averaged).

# Load home range size data
# - Should contain two columns: one for Male home ranges and one for Female home ranges
# - Each cell should be an individual's average range across years (e.g., 2021–2023)
   
boxplot <- read_excel("Docs_needed_for_code/MFRange.xlsx")

# Convert data from wide to long format
# - Resulting columns: 'Group' (Males or Females) and 'Value' (home range size)
data_long <- pivot_longer(boxplot, cols = everything(), names_to = "Group", values_to = "Value")
data_long$Group <- recode(data_long$Group,
                          "Males" = "M",
                          "Females" = "F")

# Define custom plotting theme for consistency with other figures
My_Theme = theme_bw() +
  theme(axis.title.x = element_text(size=12, family = "Arial"),
        axis.text.x = element_text(angle=45, colour="black", 
                                   vjust=1, hjust=1, size=12, family = "Arial"),
        axis.text.y = element_text(colour = "black", size=12, family = "Arial"),
        axis.title.y = element_text(size=12, family = "Arial"),
        plot.title = element_text(size=12, family = "Arial"),
        legend.title = element_text(face = "bold", size=12, family = "Arial"),
        legend.text = element_text(size = 12, family = "Arial"),
        legend.position = "none",  # Hide legend since x-axis already identifies groups
        legend.key.size = unit(1, "cm"),
        strip.text.x = element_text(size=12, family = "Arial"),
        strip.text.y = element_text(size=12, family = "Arial"),
        panel.border = element_rect(fill = NA, colour = "black"),
        strip.background = element_rect(colour="black"))


k_val<-kruskal.test(Value ~ Group, data = data_long) # Use this value in plot
data_long$Group <- factor(data_long$Group, levels = c("F", "M"))

data_long <- data_long %>%
  filter(Value <= 350)

# Plot
long_boxplot <- ggplot(data_long, aes(x = Group, y = Value, fill = Group)) +
  geom_boxplot(width = 0.75, color = "black", outliers = FALSE) +
  scale_fill_manual(values = c("M" = "#DDCC77", "F" = "#CC6677")) +
  My_Theme +
  annotate("text", x = 1.2, y = 170, 
             label = "Kruskal–Wallis, p = 0.00833", 
             size = 3, fontface = "italic",
           family = "Arial") +

  labs(
       x = NULL, 
       y = "Homerange size (km²)") 

ggsave(
  filename = "Figs_created/FigS2B.pdf",   
  plot = last_plot(),                  
  device = cairo_pdf,                     
  width = 3.35,                        # 85 mm
  height = 4,                          
  units = "in",                        
  dpi = 300,                          
)

####################### Create summary boxplot for homerange sizes showing number of bears and range size per year. #####################

# Data for second plot
summary_box <- read_excel("Docs_needed_for_code/summary_data.xlsx")
outliers <- read_excel("Docs_needed_for_code/Outlier_ranges.xlsx")

# Turn years into characters
summary_box$Year <- as.character(summary_box$Year)
outliers$Year <- as.character(outliers$Year)

# Filter any data not already filtered
summary_box_filtered <- summary_box %>%
  pivot_longer(cols = c("M", "F"),
               names_to = "Sex",
               values_to = "Range_Size") %>%
  filter(!is.na(Range_Size), !is.na(Year), !is.na(Sex)) %>%
  filter(Range_Size <= 350)

# Keep all outlier data for different plotting
outliers_long <- outliers %>%
  pivot_longer(c("M", "F"), names_to = "Sex", values_to = "Range_Size") %>%
  mutate(
    Range_Size = na_if(Range_Size, 0) 
  ) %>%
  filter(!is.na(Range_Size), !is.na(Year), !is.na(Sex))

# Combine both dataframes
combined_df <- bind_rows(summary_box_filtered, outliers_long)

combined_df <- combined_df %>%
  filter(!(Sex == "F" & str_detect(Year, "-M"))) %>%
  filter(!(Sex == "M" & str_detect(Year, "-F")))



combined_df$Year <- factor(
  combined_df$Year,
  levels = c(
    "2020", "2021", "2022", "2023", "2024",   # years
    "MPG-32-F", "MPG-65-F", "MPG-27-F",       # female outliers, small to large
    "MPG-62-M", "MPG-43-M", "MPG-41-M", "MPG-68-M", "MPG-40-M", "MPG-99-M", "MPG-60-M"  # male outliers, small to large
  )
)

# Dual axis
scale_factor <- 1000

bear_counts <- combined_df %>%
  filter(str_detect(Year, "^\\d{4}$")) %>%
  group_by(Year, Sex) %>%
  summarise(N_Bears = n(), .groups = "drop") %>%
  mutate(scaled_N = N_Bears * scale_factor)

# Plot
summary_boxplot <- ggplot(combined_df, aes(x = factor(Year), y = Range_Size, fill = Sex)) +
  geom_col(
    data = bear_counts,
    aes(x = factor(Year), y = scaled_N, fill = Sex),
    alpha = 0.25,
    width = 0.7,
    position = position_dodge(width = 0.7)
  ) +
  geom_boxplot(width = 0.6, color = "black") +
  scale_fill_manual(values = c("M" = "#DDCC77", "F" = "#CC6677")) +
  facet_wrap(~ Sex, scales = "free_x") +  
  coord_cartesian(ylim = c(0, 25000)) +
  scale_y_continuous(
    name = "Homerange size (km²)",
    sec.axis = sec_axis(~./scale_factor, name = "Number of bears")
  ) +
  labs(x = NULL, y = "Homerange size (km²)") +
  My_Theme

# Save plot
ggsave(
  filename = "Figs_created/FigS2A.pdf", 
  plot = last_plot(),           
  device = cairo_pdf,                   
  width = 6.85,                        # 165 mm
  height = 4,                          
  units = "in",                        
  dpi = 300,                           
)

