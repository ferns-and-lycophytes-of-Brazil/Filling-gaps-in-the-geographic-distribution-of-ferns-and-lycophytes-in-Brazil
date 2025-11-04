library(tidyverse)

# --- read the CSV ---
df <- read.csv("Boxplotdados.csv", stringsAsFactors = FALSE, check.names = FALSE)

# --- data cleaning ---
pct_col <- "Percentage of records in Protected Areas"
status_col <- "conservation status"

df <- df %>%
  mutate(
    Percent = as.numeric(gsub("[^0-9\\.]", "", .data[[pct_col]])),
    Status = str_split_fixed(.data[[status_col]], ",", n = 2)[,1] %>% str_trim()
  )

# define factor with desired order and colors
status_levels <- c("CR", "EN", "VU")
status_colors <- c("CR" = "#E63946", "EN" = "#F4A261", "VU" = "#FFD166")  # softer, visual colors

df$Status <- factor(df$Status, levels = status_levels)

# create ggplot
p <- ggplot(df, aes(x = Status, y = Percent, fill = Status)) +
  geom_jitter(aes(color = Status), width = 0.15, size = 2, alpha = 0.9) +  # points with same color as box
  geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.5, color = "black") +  # boxplot on top
  scale_fill_manual(values = status_colors) +
  scale_color_manual(values = status_colors) +  # points colored same as boxes
  labs(x = "Conservation Status", y = "Percentage of Records in Protected Areas") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

print(p)

# save the plot
ggsave("my_boxplot.png", plot = p, width = 8, height = 6, dpi = 300)
