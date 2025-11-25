# Load required packages
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Define a theme used when plotting
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

# Read in the data
df <- read_excel("breed_nobreed.xlsx")

# Reshape the data
df_long <- df %>%
  pivot_longer(
    cols = c(Nonbreeding, Breeding),
    names_to  = "Season",
    values_to = "HR_size"
  ) %>%
  mutate(
    Season = factor(Season, levels = c("Breeding", "Nonbreeding")),
    sex    = factor(Sex, levels = c("Male", "Female"))
  ) |>
  filter(HR_size <= 350)

# 3. Box-and-Whisker plot
ggplot(df_long, aes(x = factor(Year), y = HR_size, fill = Sex)) +   
  geom_boxplot(
    position = position_dodge(width = 0.8),
    width = 0.6,
    outlier.shape = NA
  ) +
  facet_wrap(~Season) +
  scale_fill_manual(values = c(
    "M" = "#DDCC77",   # Male
    "F" = "#CC6677"    # Female
  )) +                  
  labs(
    title = NULL,
    x = NULL,
    y = "Homerange size (km²)"   
  ) +
  My_Theme

# Save the plot as a PDF
ggsave(
  filename = "breed_nobreedsize.pdf",   
  plot = last_plot(),                  
  device = cairo_pdf(),                   
  width = 3.35,                        # 85 mm
  height = 4,                         
)
