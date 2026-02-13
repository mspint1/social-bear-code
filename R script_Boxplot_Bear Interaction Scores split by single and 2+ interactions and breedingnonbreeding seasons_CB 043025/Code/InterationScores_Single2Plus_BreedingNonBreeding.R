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

# Code for 2020-2024 Interaction Strengths Box Plot for bear pairs, split by Single and 2+ Pt interactions and Breeding/Nonbreeding season. The Excel data to read in should contain 12 columns: Male-Male, Male-Female, Female-Female Single and 2+Pt/Breeding-nonbreeding season interaction strength scores that have been enlarged by x100 (e.g. 0.78 x 100 = 78).
# Read the Excel file
boxplot <- read_excel("Docs_needed_for_code/Boxplot_breednonbreed_041725.xlsx")

# Reshape to long format
data_long <- pivot_longer(boxplot, cols = everything(), names_to = "Group", values_to = "Value")

# Extract new metadata: Season, PairType, and Points
data_long <- data_long %>%
  mutate(
    Season = case_when(
      str_detect(Group, "Nonbreeding") ~ "Nonbreeding",
      str_detect(Group, "Breeding") ~ "Breeding"
    ),
    PairType = case_when(
      str_detect(Group, "M/M") ~ "MM", #27 Pairs
      str_detect(Group, "F/F") ~ "FF", #86 Pairs
      str_detect(Group, "M/F") ~ "MF" # 99 Pairs
    ),
    Points = case_when(
      str_detect(Group, "1 Pt") ~ "1 Pt",
      str_detect(Group, "2\\+ Pts") ~ "2+ Pts"
    )
  )

# Set factor levels to control plot order
data_long$PairType <- factor(data_long$PairType, levels = c(
  "FF", "MF", "MM"
))
data_long$Points <- factor(data_long$Points, levels = c("1 Pt", "2+ Pts"))
data_long$Season <- factor(data_long$Season, levels = c("Nonbreeding", "Breeding"))
data_long <- data_long %>% 
  filter(!is.na(Value), Value > 0)


# Define custom theme
My_Theme = theme_bw() +
  theme(axis.title.x = element_text(size=12, family = "Arial"),
        axis.text.x = element_text(angle=45, colour="black", 
                                   vjust=1, hjust=1, size=12, family = "Arial"),
        axis.text.y = element_text(colour = "black", size=12, family = "Arial"),
        axis.title.y = element_text(size=12, family = "Arial"),
        plot.title = element_text(size=12, family = "Arial"),
        legend.title = element_text(size=10, family = "Arial"),
        legend.text = element_text(size = 10, family = "Arial"),
        legend.position = "none",
        legend.key.size = unit(1, "cm"),
        strip.text.x = element_text(size=12, family = "Arial"),
        strip.text.y = element_text(size=12, family = "Arial"),
        panel.border = element_rect(fill = NA, colour = "black"),
        strip.background = element_rect(colour="black"))

# Create a data frame with Wilcoxon test results for each PairType × Points combo
pval_table <- data_long %>%
  group_by(PairType, Points) %>%
  nest() %>%
  mutate(
    test_result = map(data, ~ wilcox.test(Value ~ Season, data = .x, exact = TRUE) %>% tidy())
  ) %>%
  unnest(test_result) %>%
  select(PairType, Points, p.value) %>%
  arrange(p.value) %>%
  mutate(
    Comparison = paste("Breeding vs Nonbreeding -", PairType, "-", Points),
    Significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  ) %>%
  select(Comparison, p.value, Significance)


# Plot
ggplot(data_long, aes(x = Points, y = Value)) +
  geom_boxplot(
    aes(fill = PairType),
    width = 0.6,
    position = position_dodge(width = 0.6),
    outlier = TRUE
  ) +
  facet_grid(Season ~ PairType, scales = "free_x", space = "free_x") +
  scale_y_log10() +
  scale_fill_manual(values = c(
    "MM" = "#DDCC77",
    "FF" = "#CC6677",
    "MF" = "#332288"
  )) +
  My_Theme +
  labs(
    title = NULL,
    x = NULL,
    y = "Interaction strength (log10)",
    fill = "Pair Type"
  )

#Split breeding-nonbreeding
ggplot(data_long, aes(x = PairType, y = Value, fill = PairType)) +
  geom_boxplot(width = 0.6, outlier = TRUE) +
  facet_grid(Season ~ Points, scales = "free_x", space = "free_x") +
  scale_y_log10() +
  scale_fill_manual(values = c(
    "FF" = "#CC6677",
    "MF" = "#332288",
    "MM" = "#DDCC77"
  )) +
  My_Theme +
  labs(
    x = NULL,
    y = "Interaction strength (log10)",
    fill = "Pair Type"
  )

# Create a data frame with Wilcoxon test results for each PairType × Points combo
pval_table <- data_long %>%
  group_by(PairType, Points) %>%
  nest() %>%
  mutate(
    test_result = map(data, ~ wilcox.test(Value ~ Season, data = .x, exact = TRUE) %>% tidy())
  ) %>%
  unnest(test_result) %>%
  select(PairType, Points, p.value) %>%
  arrange(p.value) %>%
  mutate(
    Comparison = paste("Breeding vs Nonbreeding -", PairType, "-", Points),
    Significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  ) %>%
  select(Comparison, p.value, Significance)

# Compute averages and plot
mean_data <- data_long %>%
  group_by(PairType, Season, Points) %>%
  summarise(Mean = mean(Value, na.rm = TRUE), .groups = "drop")


# Plot
ggplot(data_long, aes(x = Points, y = Value)) +
  geom_boxplot(
    aes(fill = PairType),
    width = 0.6,
    position = position_dodge(width = 0.6),
  ) +
  facet_grid(Season ~ PairType, scales = "free_x", space = "free_x") +
  scale_y_log10() +
  scale_fill_manual(values = c(
    "MM" = "#DDCC77",
    "FF" = "#CC6677",
    "MF" = "#332288"
  )) +
  My_Theme +
  labs(
    title = NULL,
    x = NULL,
    y = expression("Interaction strength (log"[10]*")",
                   fill = "Pair Type"
    ) 
  ) +
  stat_compare_means(
    method = "wilcox.test",        # non-parametric comparison
    label = "p.signif",            # display as *, **, etc.
    comparisons = list(c("1 Pt", "2+ Pts")),
    tip.length = 0.01,
    hide.ns = TRUE,
    vjust = 0.55,
  )

kruskal.test(Value ~ interaction(PairType, Season, Points), data = data_long)
data_long %>%
  mutate(Group = interaction(PairType, Season, Points)) %>%
  kruskal_test(Value ~ Group)

# Run Dunn post-hoc test with BH correction
data_long %>%
  mutate(Group = interaction(PairType, Season, Points)) %>%
  dunn_test(Value ~ Group, p.adjust.method = "BH")
data_long %>%
  group_by(PairType, Season) %>%
  kruskal_test(Value ~ Points)

## Saving plot
ggsave(
  filename = "Figs_created/FigsS7A.pdf",
  plot = last_plot(),               
  device = cairo_pdf,                    
  width = 6.85,                        # 165 mm or 6.85 inches
  height = 4,                       
  units = "in",                        
  dpi = 300                           
)





#### averages ####

mean_data <- data_long %>%
  group_by(PairType, Season, Points) %>%
  summarise(Mean = mean(Value, na.rm = TRUE), .groups = "drop")



ggplot(data_long, aes(x = Points, y = Value)) +
  geom_boxplot(
    aes(fill = PairType),
    width = 0.6,
    position = position_dodge(width = 0.6),
  ) +
  facet_grid(Season ~ PairType, scales = "free_x", space = "free_x") +
  scale_y_log10() +
  scale_fill_manual(values = c(
    "MM" = "#DDCC77",
    "FF" = "#CC6677",
    "MF" = "#332288"
  )) +
  My_Theme +
  labs(
    title = NULL,
    x = NULL,
    y = expression("Interaction strength (log"[10]*")",
    fill = "Pair Type"
  ) #+
  #geom_point(
    #data = mean_data,
    #aes(x = Points, y = Mean, group = PairType),
    #shape = 95,           
    #size = 12,
    #color = "black",
    #position = position_dodge(width = 0.6),
    #inherit.aes = FALSE
  ) +
  stat_compare_means(
    method = "wilcox.test",        # non-parametric comparison
    label = "p.signif",            # display as *, **, etc.
    comparisons = list(c("1 Pt", "2+ Pts")),
    tip.length = 0.01,
    hide.ns = TRUE,
    vjust = 0.55,
  )



print(mean_data)
kruskal.test(Value ~ interaction(PairType, Season, Points), data = data_long)
data_long %>%
  mutate(Group = interaction(PairType, Season, Points)) %>%
  kruskal_test(Value ~ Group)

# Run Dunn post-hoc test with BH correction
data_long %>%
  mutate(Group = interaction(PairType, Season, Points)) %>%
  dunn_test(Value ~ Group, p.adjust.method = "BH")
data_long %>%
  group_by(PairType, Season) %>%
  kruskal_test(Value ~ Points)

## Saving plot
ggsave(
  filename = "Figs_created/FigsS7A.pdf",   # or .png / .pdf
  plot = last_plot(),                  # or replace with your ggplot object
  device = cairo_pdf,                     # or "png" / "pdf"
  width = 6.85,                        # 85 mm or 6.85 inches
  height = 4,                          # pick what looks good, in inches
  units = "in",                        # set units to inches
  dpi = 300                           # journal standard
)


