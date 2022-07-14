library("ggplot2")
theme_set(theme_bw())
library("sf")

#library("rnaturalearth")
#library("rnaturalearthdata")



world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)  ### "sf" "data.frame"

ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")


ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(60.50,77.50), ylim = c(23.35, 37.05), expand = FALSE)  ### pakistan


####### this works

punjab.polygon <- st_read("VaccinationStudy/Data/Adminbdy Shapefile/Tehsil_Boundary.shp") %>%
  filter(PROVINCE == "PUNJAB") %>%
  merge(tehsils.map[,c(2,27:31)], by = "TEHSIL", all.x = T)
  
class(punjab.polygon)
plot(punjab.polygon["Shape_Area"])

ggplot(punjab.polygon) + 
  geom_sf(aes(fill=OutreachProportion)) +
  scale_fill_gradient(low="lightgreen", high="darkgreen")
