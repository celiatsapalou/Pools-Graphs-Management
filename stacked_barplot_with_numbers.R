library(ggplot2)
library(dplyr)

# Define colors for each superpopulation
superpop_colors <- c("#ffd845", "#710027", "#018ead", "#2356A7", "#c44cfd")
names(superpop_colors) <- c("AFR", "AMR", "EAS", "EUR", "SAS")

# Create the data frame
data_old_pools <- data.frame(
  Pool = rep(c("Pool-1", "Pool-2", "Pool-3"), each = 5),
  Superpopulation = rep(c("AFR", "AMR", "EAS", "EUR", "SAS"), times = 3),
  Unique_Samples = c(8, 6, 5, 5, 4, 9, 4, 6, 5, 5, 8, 4, 3, 3, 6)
)

# Create the stacked bar plot
plot_old_pools <- ggplot(data_old_pools, aes(x = Pool, y = Unique_Samples, fill = Superpopulation)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Unique_Samples), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  scale_fill_manual(values = superpop_colors) +
  labs(title = "New Pools - Sample composition of the first 3 pools after sequencing",
       x = "Pool",
       y = "Unique Samples") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        panel.grid = element_blank())

# Display the plot
print(plot_old_pools)

