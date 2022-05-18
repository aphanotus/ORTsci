# Map of nations and institutions with open sciebnce policies

# Load packages
x <- c("tidyverse","viridis","rnaturalearth","rnaturalearthdata","ggrepel")
invisible(lapply(x, require, character.only = TRUE))

world <- ne_countries(scale = "medium", returnclass = "sf")
world$policy <- FALSE

national.policy <- c("United Kingdom", "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")
institutions <- read.csv("institutions.with.open.sci.policies.csv")

world$policy[which(world$name %in% national.policy)] <- TRUE

map <- ggplot(data = world) +
  theme_minimal() +
  # theme(legend.position="bottom") +
  theme(legend.position="none") +
  geom_sf(aes(fill = policy), color = "gray65") +
  scale_fill_manual(name = "National Open Science Policy?", values = c("gray95","lightgreen")) +
  geom_point(data = institutions,
    aes(x=long, y=lat),
    color = "darkgreen", alpha = 0.65, size = 2 ) +
  geom_text_repel(data = institutions,
    aes(label = short.name, x=long, y=lat),
    color = "darkgreen", alpha = 0.9, size = 2,
    max.overlaps = 20, min.segment.length = 0) +
  scale_color_manual(name = "", values = c("darkblue")) +
  labs(x = NULL, y = NULL,
       title = "Nations and Institutions with Open Science Policies")

map

ggsave("institutions.with.open.sci.policies.pdf", map, units = "in", height = 7, width = 9, scale = 1)
ggsave("institutions.with.open.sci.policies.png", map, units = "in", height = 7, width = 9, scale = 1)
