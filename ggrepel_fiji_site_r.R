
provinces <- st_read(dsn = gis_dir, layer = "province") # Fiji provinces where survey sites occurred

# Ecological data
fji_coral <- st_read(dsn = gis_dir, layer = "fiji_coral") # Coral data extent in Great Sea Reef

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
  
  province_survey <- ggplot() + 
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
    # theme
    theme_bw()

  # Export plots
  out_survey <- paste0(prov_name,"_survey.tiff")
  ggsave(province_survey, filename=file.path(province_map_dir, out_survey), width=6.5,
         height=4.5, units="in", dpi=600, compression = "lzw")
}
