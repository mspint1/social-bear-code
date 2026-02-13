# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(ggplot2)
library(ggpubr)
library(purrr)

# Load data
breeding <- read_csv("Breeding_Overlap_table.csv")
nonbreeding <- read_csv("Nonbreeding_Overlap_table.csv")

# function for overlap
pivot_overlap_matrix <- function(df, season_label) {
  df |>
    rename(Bear1 = 1) |>
    pivot_longer(-Bear1, names_to = "Bear2", values_to = "Overlap") |>
    mutate(
      Year1 = str_extract(Bear1, "^\\d{4}") %>% as.integer(),
      Year2 = str_extract(Bear2, "^\\d{4}") %>% as.integer(),
      Season = season_label
    ) |>
    # Keep only pairs from the same year
    filter(Year1 == Year2) %>%
    # Simplify names (remove year & any other extra words)
    mutate(
      Year = Year1,
      Bear1 = str_remove(Bear1, "^\\d{4}_"),
      Bear1 = str_remove(Bear1, "_Positions.*"),
      Bear2 = str_remove(Bear2, "^\\d{4}_"),
      Bear2 = str_remove(Bear2, "_Positions.*")
    ) |>
    select(Year, Season, Bear1, Bear2, Overlap)
}

# apply to both data
breeding_long <- pivot_overlap_matrix(breeding, "Breeding")
nonbreeding_long <- pivot_overlap_matrix(nonbreeding, "Nonbreeding")

# combine and filter
overlap_all <- bind_rows(breeding_long, nonbreeding_long) %>%
  filter(!is.na(Overlap)) %>%       
  filter(Bear1 != Bear2)            

### Filter bears for removal

remove_list <- tibble(
  Bear = c("Fanta", "Green_Eggs", "Mousse", "Mousse", "Ratatouille",
           "Rattlesnake", "She_Tan", "She_Tan", "Oblong", "Tikki", "Troublemaker", "Wanderer", "Wanderer"),
  Year = c(2022, 2021, 2021, 2022, 2024, 2021, 2020, 2021,2022, 2020, 2020, 2021, 2022)
)

# continue filtering
overlap_filtered <- overlap_all %>%
  filter(
    !(Bear1 %in% remove_list$Bear & Year %in% remove_list$Year),
    !(Bear2 %in% remove_list$Bear & Year %in% remove_list$Year)
  )
removed_rows <- anti_join(overlap_all, overlap_filtered)
removed_rows %>%
  count(Year)

# Statistic test needed for plot
overall_test <- wilcox.test(Overlap ~ Season, data = overlap_filtered)
overall_test$p.value
p_val <- overall_test$p.value



##### Plotting ###########

# Defining theme
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

## Assign comparisons for statistics
year_comparisons <- list(
  c("2020", "2021"),
  c("2020", "2022"),
  c("2020", "2023"),
  c("2020", "2024"),
  c("2021", "2022"),
  c("2021", "2023"),
  c("2021", "2024"),
  c("2022", "2023"),
  c("2022", "2024"),
  c("2023", "2024")
)

# Plot 
ggplot(overlap_filtered, aes(x = factor(Year), y = Overlap, fill = factor(Season))) +
  scale_fill_manual(values = c(
    "Breeding" = "brown",
    "Nonbreeding" = "green"
  ))+
  geom_boxplot(width = 0.6) +
  stat_compare_means(
    comparisons = year_comparisons,
    method = "wilcox.test",
    label = "p.signif",
    hide.ns = FALSE
  )+
  facet_wrap(~ Season) +
  labs(
    title = NULL,
    x = NULL,
    y = "Homerange overlap (%)"
  ) +
  My_Theme

# Save the plot as a pdf
ggsave(
  filename = "Figures/FigS12.pdf",   
  plot = last_plot(),                  
  device = cairo_pdf,                    
  width = 3.35,                        # 85 mm
  height = 4,                         
  units = "in",                        
)


