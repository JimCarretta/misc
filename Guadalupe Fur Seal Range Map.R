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

states <- ne_states(returnclass = "sf")

target_polys <- c("Arizona", "Baja California", "Baja California Sur", "British Columbia", "California", "Chihuahua", "Coahuila", "Colorado", "Durango", "Idaho", "Montana", "Nayarit", "Nevada", 
                  "New Mexico", "Oregon", "Sinaloa", "Sonora", "Texas", "Utah", "Washington", "Wyoming")

# Filter data for California and Western Mexico
map_polys <- states[states$name %in% target_polys, ]


# Plot map 
p <- ggplot() +
  geom_sf(data = map_polys, color = "black", fill = "beige", size = 0.1) +
  coord_sf(xlim = c(-145, -85), ylim = c(8,49), expand=FALSE) +
  geom_point(data=sighting.data.iNat, aes(x=longitude, y=latitude)) +
  theme_classic() +
  theme(panel.background = element_rect(fill="lightblue")) +
  labs(title="Guadalupe Fur Seal Distribution", x="Longitude", y="Latitude") +
  theme(axis.text.x = element_text(angle = 90)) # rotate axis text

p <- p + annotate("pointrange", x=-118.28119, y=29.03, ymin=29.03, ymax=29.03, color="blue", size=1, alpha=0.4)

p

ggsave("Guadalupe_Fur_Seal.tif", width=3200, height=2400, units="px")
