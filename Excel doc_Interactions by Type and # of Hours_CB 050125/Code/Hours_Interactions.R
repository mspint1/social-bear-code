library(ggplot2)
library(dplyr)

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

# loading in data
interaction_data <- data.frame(
  Type = c("MF", "FF", "MM"),
  Single_GPS = c(511, 558, 101),
  two_plus = c(494, 359, 80)
)

hour_data <- data.frame(
  Type = c("MF", "FF", "MM"),
  Single_GPS = c(511, 558, 101),
  two_plus = c(2999, 1554, 262)
)

interaction_long <- pivot_longer(interaction_data, cols = c(Single_GPS, two_plus),
                                 names_to = "PointType", values_to = "Interactions")
hour_long<- pivot_longer(hour_data, cols = c(Single_GPS, two_plus),
                        names_to = "PointType", values_to = "Hours")

# Plotting interactions
interaction_plot <- ggplot(interaction_long, aes(x = PointType, y = Interactions, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = Interactions), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 2.5) +
  scale_fill_manual(values = c("FF" = "#CC6677", "MM" = "#DDCC77", "MF" = "#332288")) +
  scale_x_discrete(labels = c("Single_GPS" = "Single GPS Point",
                              "two_plus" = "2+ GPS Points")) +
  labs(title = NULL,
       x = NULL, y = "Number of interactions", fill = "Interaction Type") +
  My_Theme

# Plotting by hour
hour_plot <- ggplot(hour_long, aes(x = PointType, y = Hours, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = Hours), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 2.5) +
  scale_fill_manual(values = c("FF" = "#CC6677", "MM" = "#DDCC77", "MF" = "#332288")) +
  scale_x_discrete(labels = c("Single_GPS" = "Single GPS Point",
                              "two_plus" = "2+ GPS Points"))+
  labs(title = NULL,
       x = NULL, y = "Hours spent interacting", fill = "Interaction Type") +
  My_Theme



## Save Script
ggsave(
  filename = "Figs_created/hour_interactions85mm.pdf",
  plot = hour_plot,
  device = cairo_pdf(),
  width = 3.35,          # in inches (85 mm or 3.35)
  height = 4,            
  units = "in",
  dpi = 300,
)


