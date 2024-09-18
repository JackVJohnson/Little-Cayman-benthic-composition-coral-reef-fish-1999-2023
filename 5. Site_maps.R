########################################################################################
############################## Fish-benthos relationship ###############################
########################################################################################

# objective: map of Little within Caribbean  
# Author: Jack Johnson (jackvjohnson@hotmail.com)
# Date created: July 2023
# Last edited: 

# clear workspace
rm(list=ls())

# libraries

library(tidyverse)
library(fishualize)
library(patchwork)
library(sf)
library(ggmap)
library(ggsn)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)

#########################################################################################
################################## working directories ##################################

shapefile_wd <- "C:/Users/jackv/OneDrive - Central Caribbean Marine Institute, Inc/Shapefiles/Cayman Islands"

output_directory <-  "C:/Users/jackv/OneDrive - Central Caribbean Marine Institute, Inc/Projects/AGGRA/Fish benthos relationship/Output_directory"

tidy_wd <- "C:/Users/jackv/OneDrive - Central Caribbean Marine Institute, Inc/Projects/AGGRA/Data_tidy/TIDY_COMPLETE"



my_theme <- theme_classic() +
  theme(axis.title.x = element_text(size = 20, color = "black"), axis.title.y = element_text(size = 20, color = "black"), text=element_text(size=16)) +
  theme(axis.text.y = element_text(size=16, color="black"), axis.text.x = element_text(size=16, color="black", angle = 0, vjust = .0)) +
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 18, face = "bold")) +
  theme(plot.title = element_text(size = 22, face = "bold"))

#########################################################################################
################################## read in coral surveys ################################

coral_cover_df <- read.csv(file=file.path(tidy_wd, "CORALCOVER_TIDY.csv"))
moorings <- read.csv(file=file.path(tidy_wd, "LITTLE_CAYMAN_PUBLIC_MOORINGS_2023.csv"))

site_map_df <- coral_cover_df %>%
  group_by(Site_Name) %>%
  summarise(n=n())

#Icon = 19.697681, -80.061424 

df3 <- site_map_df %>% 
  group_by(Site_Name) %>%
  summarise(n=sum(n))

df4 <- left_join(df3, moorings, by = "Site_Name")

icon <- data.frame(Site_Name = "Icon", n =55, ID = NA, Depth = NA, Pins = NA, Northing = 19.697681, Easting = -80.061424, Island = NA, Aspect = NA, MPA = NA)

df5 <- rbind(df4, icon)
df5 <- df5[-7,]

#########################################################################################
################################## read in shape file  ##################################

setwd(shapefile_wd)
#Little Cayman
#Longitude: -80.133056°, Latitude: 19.730000°
#Longitude: -79.936944°, Latitude: 19.626389°

# site 
#N 19°41'59" = 19.699722° (latitude)
#W 80°03'42" = -80.061667° (longitude)
site_coordinates <- data.frame(Longitude = -80.061667, Latitude = 19.699722)


lc_map <- st_read("cym_admbnda_adm0_2020.shp")
lc_map
lc_map <- st_transform(lc_map, crs = 4326)

p1 <- ggplot() +
  geom_sf(data=lc_map, fill = "white") +
  geom_point(data = df5, aes(x = Easting, y = Northing, color = n), size = 3) +
  scale_color_viridis_c(option = "A", direction = -1) +
  coord_sf(xlim=c(-80.21056,-79.936944), ylim=c(19.62638, 19.790000), expand = T) +
  #geom_point(data = site_coordinates, aes(x = Longitude, y = Latitude), shape = 16, size = 5, color = "black") +
  annotation_scale(location = "tr",
                   pad_x = unit(0.7, "cm"), pad_y = unit(11, "cm")) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(1.5, "cm"), pad_y = unit(9, "cm") ,
                         height = unit(2, "cm"), width = unit(2, "cm") ,
                         style = north_arrow_fancy_orienteering) +
  theme_classic() +
  labs(x="", y="", color="Number of 
surveys") +
  theme(axis.title.x = element_text(size = 22, color = "black"), axis.title.y = element_text(size = 22, color = "black"), text=element_text(size=16)) +
  theme(axis.text.y = element_text(size=16, color="black"), axis.text.x = element_text(size=16, color="black", angle = 0, vjust = .0)) +
  #theme(legend.position = "none")  +
  theme(panel.background = element_rect(fill = "#5ECFFA")) +
  theme(panel.border= element_rect(colour = "black", linewidth =1, fill =NA))

p1  

# trim a world map to show the wider Caribbean region 

ocean <- ne_download(type = "ocean", category = "physical", scale = "medium")
ocean <- st_as_sf(ocean)

p2 <- ggplot() + 
  geom_sf(data = ocean, fill = "#5ECFFA") +
  coord_sf(xlim=c(-92.478113,-62.031176), ylim=c(8.8,27), expand = T) +
  annotation_scale() +
  #annotation_north_arrow(location = "tr", which_north = "true", 
  #                       height = unit(1, "cm"), width = unit(1, "cm") ,
  #                       style = north_arrow_fancy_orienteering) +
  theme_classic() +
  xlab("") + ylab("") +
  #theme(panel.background = element_rect(fill = "white")) +
  geom_rect(aes(xmin = -80.5, xmax = -79.5, ymin = 19, ymax = 20), color = "black", fill = NA, linewidth = 1) +
  annotate("text", x=-78, y=15, label = "Caribbean 
Sea", size = 8) +
  theme(axis.title.x = element_text(size = 22, color = "black"), axis.title.y = element_text(size = 22, color = "black"), text=element_text(size=16)) +
  theme(axis.text.y = element_text(size=16, color="black"), axis.text.x = element_text(size=16, color="black", angle = 0, vjust = .0)) +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", linewidth =1, fill =NA),
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank()
  )
p2


tiff(file=file.path(output_directory, "sites_map_FIGURE.tif"), height = 4000, width = 4000, res = 400)
p1 + inset_element(p2, left = -.035, bottom = 0.5, right = 0.6, top = 1, align_to = "full") 
dev.off()

png(file=file.path(output_directory, "sites_map_FIGURE.png"), height = 4000, width = 4000, res = 400)
p1 + inset_element(p2, left = -.035, bottom = 0.5, right = 0.6, top = 1, align_to = "full") 
dev.off()



length(df3$Site_Name)
# 23 sites
sum(df3$n)
# 1518 transects
summary(coral_cover_df$Depth)
# for depth, some idiots have put it in ft when it's all meant to be m. Because of this we won't be able to include depth as a random effect, but a min of 2 and a max of 54, makes sense to assume lowest is meters and highest is depth. All surveys took place between 2-18m 


