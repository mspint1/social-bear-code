# Load libraries
library(ggplot2)

# DATA WAS CREATED FROM tracking_dropoff code. This is using that data.
delay_data <- data.frame(
  Delay_days = 1:30,
  Num_Delayed_Interactions = c(10, 46, 17, 13, 9, 13, 21, 9, 17, 12,
                               17, 32, 14, 5, 19, 8, 12, 9, 6, 3,
                               19, 14, 4, 2, 6, 4, 5, 16, 1, 3)
)

# Plot
ggplot(delay_data, aes(x = Delay_days, y = Num_Delayed_Interactions)) +
  geom_line(size = 1, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed")+
  labs(
    x = "Delay (days)",
    y = "Number of delayed interactions"
  ) +
  My_Theme

ggsave(
  filename = "Figures/FigS13.pdf",  
  plot = last_plot(),                  
  device = cairo_pdf,                    
  width = 6.85,                        # 165 mm
  height = 4,                          
  units = "in",                        
  dpi = 300,                           
)
