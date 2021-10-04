#' ---
#' title: "Making Plots With Color Hex Color Codes"
#' subtitle: 'Converts the R G B colour coefficients to the corresponding Color Hex Color Codes'
#' author: "Alex Koiter"
#' date: "Oct 04, 2021"
#' output:
#'   html_document:
#'     theme: cosmo
#'     toc: true
#'---
#' # Load libraries 
#+ message = FALSE
library(tidyverse)
library(readxl)

#' # Load data
results <- read.csv("Results.csv")
ids <- read_excel("ids.xlsx")

#' # Data munging  
data <- ids %>%
  left_join(results) %>%
  filter(!notes %in% c("duplicate")) %>%
  pivot_longer(cols = X:B, names_to = "Colour", values_to = "value") %>%
  mutate(mid_depth = (lower_depth - upper_depth)/2 + upper_depth) %>%
  filter(Colour %in% c("R", "G", "B")) %>%
  mutate(Colour = fct_relevel(Colour, "R", "G", "B")) %>%
  mutate(core = recode(core, "1" = "Core 1", "2" = "Core 2", "soil" = "Soil Profile"))

#' # Plot RGB with depth
p1 <- ggplot(data = data, aes(x = mid_depth, y = value, colour = Colour)) +
  geom_point() +
  geom_line() +
  geom_errorbarh(aes(y = value, xmin = upper_depth, xmax = lower_depth)) +
  theme_bw(base_size = 18) +
  coord_flip() +
  scale_x_reverse(limits = c(60, 0), breaks = seq(0, 70, by = 10), expand = c(0, 0)) +
  labs(x = "Depth (cm)", y = "Value") +
  theme(legend.position = c(0.1, 0.15),
        legend.title = element_blank()) +
  facet_wrap(~core, scales = "free_x")
p1
ggsave(plot = p1, "RGB_colour_fig.png", height = 150, width = 150, units = "mm", dpi = 600)  

#' # Data munging and conversion to hex colours
hex <- ids %>%
  left_join(results) %>%
  filter(!notes %in% c("duplicate")) %>%
  select("id", "core", "upper_depth", "lower_depth", "R", "G", "B") %>%
  mutate(hex = rgb(R, G, B, max = 255)) %>%
  arrange(core, upper_depth) %>%
  mutate(mid_depth = (lower_depth - upper_depth)/2 + upper_depth) %>%
  mutate(core = recode(core, "1" = "Core 1", "2" = "Core 2", "soil" = "Soil Profile"))

#' # Create vector of hex colour codes
soil_colour <- as.vector(hex$hex)

#' # Plot hex colours with depth
p2 <- ggplot(data = hex, aes(x = mid_depth, y = core)) +
  geom_tile(aes(width = lower_depth - upper_depth, fill = id, height = 0.9)) +
  theme_bw(base_size = 18) +
  coord_flip() +
  scale_x_reverse(limits = c(60, 0), breaks = seq(0, 70, by = 10), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0.27, 0)) +
  scale_fill_manual(values = soil_colour) +
  labs(x = "Depth (cm)") +
  theme(legend.position = "none",
        axis.title.x=element_blank()) 
p2

ggsave(plot = p2, "Hex_colour_fig.png", height = 150, width = 150, units = "mm", dpi = 600)  
