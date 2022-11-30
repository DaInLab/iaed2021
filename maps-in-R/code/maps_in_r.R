if(!("remotes") %in% installed.packages()) install.packages("remotes")
if(!("sf") %in% installed.packages()) install.packages("sf")


remotes::install_github("schochastics/roughsf")
install.packages("sf")
install.packages("rnaturalearthdata")
install.packages("udunits2")
install.packages("rnaturalearth")

library(roughsf)
library(rnaturalearth)

library("rnaturalearthdata")

library(sf)

library(udunits2)


ger <- rnaturalearth::ne_countries(scale = "medium", country = "Germany", returnclass = "sf")
ger <- st_cast(ger, "POLYGON")
ger$fill <- "#CD2626"
ger$stroke <- 2
ger$fillweight <- 0.5

# MULTIPOLYGON (and also MULTILINESTRING) are not supported
ger <- st_cast(ger, "POLYGON")

cities <- data.frame(name = c("Berlin", "Munich", "Hamburg", "Cologne"))
cities$geometry <- st_sfc(
  st_point(c(13.4, 52.5200)), st_point(c(11.582, 48.1351)),
  st_point(c(9.9937, 53.5511)), st_point(c(6.9603, 50.9375))
)
cities <- st_sf(cities)
st_crs(cities) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
cities$size <- 15
cities$color <- "#000000"
cities$label <- cities$name
cities$label_pos <- "e"

roughsf::roughsf(list(ger, cities),
                 title = "Sketchy Map of Germany", 
                 caption = "drawn by @schochastics",
                 title_font = "48px Pristina", 
                 font = "30px Pristina", 
                 caption_font = "30px Pristina",
                 roughness = 1, bowing = 1, simplification = 1,
                 width = 800, height = 1000, 
)
