#####################
#ggrepel example code
#####################

# Clean environment
rm(list = ls())

# Setup
###########################

# Preparing packages
if (!require("pacman")) install.packages("pacman")

# Load packages
pacman::p_load(dplyr,fasterize,ggplot2,ggrepel,ggsn,ggspatial,metR,raster,RColorBrewer,rgdal,rgeos,scales,sf,sp,tidyr)

# get the most updated version of ggrepel
devtools::install_github("slowkow/ggrepel")
library(ggrepel)

# Setting data directory
gis_dir <- setwd("pathname")

# where to save provinces
provinces_map_dir <-  "C:\\Users\\free\\Dropbox (MPAMystery)\\RICOTTA_GIS\\oceans_program\\dom\\fiji_report\\maps\\province"


# Read data
###########################
# Loading the required data
# Administrative boundary data
fiji <- st_read(dsn = gis_dir, layer = "fiji") # Fiji land administrative boundary
qoliqoli <- st_read(dsn = gis_dir, layer = "qoliqoli") # Qoliqoli (district) boundaries where survey sites occurred
provinces <- st_read(dsn = gis_dir, layer = "province") # Fiji provinces where survey sites occurred
gsr <- st_read(dsn = gis_dir, layer = "gsr") # Great Sea Reef boundary

# Ecological data
fji_coral <- st_read(dsn = gis_dir, layer = "fiji_coral") # Coral data extent in Great Sea Reef

# Rasterize ecological data
# Coral data
coral_temp <- raster(extent(fji_coral),res = 25, crs = fji_coral) # create a template raster with the coral reef extent
coral_rast <- fasterize(fji_coral,coral_temp) # rasterizing the coral data
coral_map <- raster::as.data.frame(coral_rast, xy=T) %>% # Convert to dataframe to have mapped later
  dplyr::filter(!is.na(layer)) %>%
  setNames(c("longitude", "latitude", "coral")) %>%
  mutate(coral = "Coral")

# Historic survey site data
surv_site <- st_read(dsn = gis_dir, layer = "gsr_survey_sites") %>% # all survey sites
  # Rearrange columns
  dplyr::select(Site,District,Historic_f,Qoliqoli_I,Place,Villages,
                Province,Latitude,Longitude,
                Province_1,Sub_group,Historic_b,
                Past_data_,geometry) %>%
  # Remove place, villages, province, sub-group, historic_b, past data
  dplyr::select(-Place,
                -Villages,
                -Qoliqoli_I,
                -Province,
                -Sub_group,
                -Historic_b,
                -Past_data_) %>%
  # Rename columns
  rename(site=Site,
         district=District,
         surveyor=Historic_f,
         latitude=Latitude,
         longitude=Longitude,
         province=Province_1)


levels(surv_site$surveyor) # get order of the surveyors
levels(surv_site$province) # survey sites were not conducted in Ra Province

# Add Ra province to the provinces field to assist with the map for loops
levels(surv_site$province) <- c(levels(surv_site$province), "Ra")
levels(surv_site$province) # see that Ra now appears for the field

# Quick map of the area
map1 <- ggplot() +
  geom_sf(data = fji_eez, fill = NA, size = 0.05) +
  geom_sf(data = fiji, fill = "red") +
  geom_sf(data = gsr) +
  geom_sf(data = qoliqoli) +
  geom_sf(data = provinces, fill = "green") +
  geom_sf(data = surv_site) +
  scale_x_longitude(breaks = seq(-178,180,2)) +
  xlab("Longitude") + 
  ylab("Latitude") +
  ggtitle("Quick map") +
  theme_bw()
map1

# Map setup
###########################
## Defining map elements
# Colors
# administrative
land_col <- "#878787"
water_col <- "#a6cee3"

# unique site colors
eia_col <- "#ffffb3" # light yellow
new_col <- "#fb8072" # light pink
rfc_col <- "#bebada" # light purple
wwf_col <- "#8dd3c7" # light green

# survey site shapes
eia_shape <- 21 # circle
new_shape <- 22 # square
rfc_shape <- 23 # diamond
wwf_shape <- 24 # triangle up

# scale bar
scalebar_ba <- annotation_scale(width_hint = 0.2, # percent of the plot occupied (20%)
                                pad_x = unit(1.55, "in"), # how much padded from the x=0 position
                                pad_y = unit(0.05, "in")) # how much padded from the y=0 position

# north arrow
narrow_ba <- annotation_north_arrow(height = unit(0.25, "in"), 
                                    width = unit(0.20, "in"),
                                    pad_x = unit(2.4, "in"),
                                    pad_y = unit(0.05, "in"),
                                    style = north_arrow_orienteering(
                                      line_width = 1,
                                      line_col = "black",
                                      fill = c("white", "black"),
                                      text_col = "black",
                                      text_family = "",
                                      text_face = NULL,
                                      text_size = 5,
                                      text_angle = 0))

# map themes
map_theme_ba <- theme(axis.text=element_text(size=8),
                      axis.title=element_text(size=10),
                      plot.title=element_text(size=12),
                      panel.grid.major = element_line(color = "transparent"), 
                      panel.grid.minor = element_line(color = "transparent"),
                      panel.background = element_rect(fill = water_col),
                      axis.text.y = element_text(angle = 90, hjust = 0.5),
                      legend.position =  c(0.15,0.9), # alternative = bottom
                      legend.title = element_blank(), # remove the legend title
                      legend.text = element_text(size=6), # text size of the descriptor
                      legend.background = element_rect(fill = "transparent"), # make the box transparent --> "transparent"
                      legend.box.background = element_rect(fill = "white", color = "#4C84A2"), # white background with blue border
                      legend.box.margin = margin(1,1,1,1), # add some space between the box and the text
                      legend.spacing.y = unit(0.025, "in"),
                      legend.key.size = unit(0.1, "in"), # size of the color box
                      legend.key = element_rect(fill = "transparent"), # make the background of the key clear
                      legend.margin = margin(0, 0.0, 0, 0, "in"), # reduce spacing between legend elements
                      axis.line = element_line(colour = "black"))


# Figure generation
###########################

# get the Ba province
i <- 1

for (i in 1){
  # get province
  province_do <- provinces[i,]
  # get the limits
  xlim_prov <- c(xmin = st_bbox(province_do)$xmin, xmax = st_bbox(province_do)$xmax)
  ylim_prov <- c(xmin = st_bbox(province_do)$ymin, xmax = st_bbox(province_do)$ymax)
  
  # extract province name
  prov_name <- province_do$Name
  
  # x-axis limits
  if(i==1){xbreaks <- seq(177,178,0.25)}
  
  # Create the loop for the provincial survery site maps
  coral_map_sample <- sample_frac(coral_map,0.01) # display only 1% of coral data to speed up map process
  
  province_survey <- ggplot() + 
    # load Fiji land
    geom_sf(data = fiji, fill = land_col, color = NA) +
    # load Great Sea Reef
    geom_sf(data = gsr, fill = NA, aes(linetype = "Great Sea Reef"), size = 0.5) +
    # load coral data
    geom_tile(data = coral_map_sample, aes(x=longitude,y=latitude, color="Coral")) +
    # load province
    geom_sf(data = province_do, fill = NA, aes(linetype = Name), size = 0.5) +
    # load suvery site data
    geom_sf(data = surv_site, aes(fill=surveyor, shape=surveyor),show.legend = "point") +
    # focus on the area of interest
    coord_sf(xlim = xlim_prov, 
             ylim = ylim_prov) + 
    # x-axis breaks
    scale_x_longitude(breaks = xbreaks) +
    # survey shape
    scale_shape_manual(name = "Survey Site",
                       labels = c("Ba EIA",
                                  "New Site",
                                  "Reef Check",
                                  "WWF"),
                       values = c(eia_shape,
                                  new_shape,
                                  rfc_shape,
                                  wwf_shape),
                       guide = guide_legend(override.aes = list(fill = c(eia_col,
                                                                         new_col,
                                                                         rfc_col,
                                                                         wwf_col)))) + 
    # surveyor fill
    scale_fill_manual(labels = c("Ba EIA",
                                 "New Site",
                                 "Reef Check",
                                 "WWF"),
                      values = c(eia_col,
                                 new_col,
                                 rfc_col,
                                 wwf_col)) +
    # Great Sea Reef legend
    scale_linetype_manual(name = "Borders",
                          values = c("solid", "3313"),
                          guide = guide_legend(override.aes = list(color = c("grey30","grey50"),
                                                                   shape = c(NA,NA)))) + 
    # coral legend
    scale_color_manual(name = "Benthic habitat",
                       values = coral_col,
                       label = "Coral reefs",
                       guide = guide_legend(override.aes = list(fill = coral_col,
                                                                shape = NA))) + 
    # remove fill symbology
    guides(fill = FALSE) + 
    # repel text of sites in area of interest
    # geom_sf_text(data = filter(surv_site, province == prov_name), aes(x=longitude, y = latitude, label = site),
    #                         size = 2, col = "black", fontface = "bold",
    #                         nudge_x = 2500, nudge_y = 2500,
    #                         check_overlap = FALSE) +
    ggrepel::geom_text_repel(data = filter(surv_site, province == prov_name),
                             mapping = aes(x = longitude,
                                           y = latitude,
                                           label = site),
                             size = 2,
                             point.padding = NA) +
    # labels + title
    labs(x="",y="", title="") + 
    # map elements
    scalebar_ba +
    narrow_ba +
    # theme
    theme_bw() + 
    map_theme_ba_survey

  # Export plots
  out_survey <- paste0(prov_name,"_survey.tiff")
  ggsave(province_survey, filename=file.path(province_map_dir, out_survey), width=6.5,
         height=4.5, units="in", dpi=600, compression = "lzw")
}