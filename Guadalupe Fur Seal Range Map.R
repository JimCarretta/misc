# Install and load necessary packages
install.packages(c("geodata", "ggplot2", "elevatr", "rnaturalearth", "rnaturalearthdata", "raster", "sf"))
library(elevatr)
library(geodata)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(sf)
library(raster)

setwd("c:/carretta/temporary")

sighting.data.iNat <- read.csv("Guadalupe_Fur_Seal.csv")

# Load world map data

world <- ne_countries(scale = "large", returnclass = "sf")

target_countries <- c("British Columbia", "Canada", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Mexico", "Nicaragua", "United States of America")

states <- ne_states(returnclass = "sf")

target_states <- c("British Columbia", "California", "Oregon", "Washington")

# Filter data for California and Western Mexico
state_polys <- states[states$name %in% target_states, ]
country_polys <- world[world$name %in% target_countries, ]

# Plot map 
p <- ggplot() +
  geom_sf(data = country_polys, color = "black", fill = "beige", size = 0.1) +
  geom_sf(data = state_polys, color = "black", fill = "beige", size = 0.1) +
  coord_sf(xlim = c(-145, -100), ylim = c(15,50), expand=FALSE) +
  geom_point(data=sighting.data.iNat, aes(x=longitude, y=latitude, color=Source)) +
  theme_classic() +
  theme(panel.background = element_rect(fill="lightblue")) +
  labs(title="Guadalupe Fur Seal Distribution", x="Longitude", y="Latitude") +
  theme(axis.text.x = element_text(angle = 90)) # rotate axis text

p <- p + annotate("pointrange", x=-118.28119, y=29.03, ymin=29.03, ymax=29.03, color="blue", size=1, alpha=0.4)

p

ggsave("Guadalupe_Fur_Seal.tif", width=3200, height=2400, units="px")
