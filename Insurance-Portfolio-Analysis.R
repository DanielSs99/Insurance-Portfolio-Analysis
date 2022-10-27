# library ---------------------------------------------------------------------
pacman::p_load(tidyverse, readxl, ggmap)
# Data ------------------------------------------------------------------------
Dt <- read.csv('fl_insurance_portfolio_data.csv')
# Info of Dataset -------------------------------------------------------------
class(Dt); names(Dt); str(Dt); head(Dt, 10)
dim(Dt); sum(is.na(Dt))
# Analysis --------------------------------------------------------------------
Dt20 <- subset(Dt, select = c('TIV.2020'))
Dt21 <- subset(Dt, select = c('TIV.2021'))
summary(Dt20)
summary(Dt21)

p1 <- ggplot(Dt) + aes(x = log(TIV.2020), y =..density..) + geom_histogram() +
  geom_density()

p2 <- ggplot(Dt) + aes(x = log(TIV.2021), y =..density..) + geom_histogram() +
  geom_density()

gridExtra::grid.arrange(p1,p2, ncol = 2)



library(echarts4r) # charts
library(tidyverse) # general use
library(lubridate) # dates and times
library(prophet) # forecasting

e_common(font_family = "helvetica", theme = "westeros")

top_destinations <- Dt %>% 
  count(County) %>% 
  top_n(15, n) %>% 
  arrange(n)


top_destinations %>%
  e_charts(x = County) %>%
  e_bar(n, legend = FALSE, name = "Flights") %>% 
  e_labels(position = "right") %>% 
  e_tooltip() %>% 
  e_title("Flights by destination", "Top 15 destinations") %>% 
  e_flip_coords() %>% 
  e_y_axis(splitLine = list(show = FALSE)) %>% 
  e_x_axis(show = FALSE) %>% 
  e_toolbox_feature(
    feature = "saveAsImage",
    title = "Save as image"
  )

# Maps ------------------------------------------------------------------------
bc_box <- make_bbox(Longitude, Latitude, Dt)
bc_box
bc_big <- get_map(location = bc_box, source = "google", maptype = "terrain")
ggmap(bc_big) +
  geom_point(data = Dt, mapping = aes(x = Longitude, y = Latitude, color = Line))

states <- map_data("state")
west_coast <- subset(states, region %in% c("florida"))
ggplot(data = west_coast) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") + 
  coord_fixed(1.3)

counties <- map_data("county")
ca_county <- subset(counties, region == "florida")

ca_base <- ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")
ca_base + theme_nothing()

ca_base + theme_nothing() + 
  geom_polygon(data = ca_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)  # get the state border back on top

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

elbow_room1 <- ca_base + 
  geom_polygon(data = Dt, aes(fill = County), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes

elbow_room1 


library(mapdeck)
set_token(Sys.getenv("MAPBOX"))
crash_data = read.csv("https://git.io/geocompr-mapdeck")
crash_data = na.omit(crash_data)
ms = mapdeck_style("dark")
mapdeck(style = ms, pitch = 45, location = c(0, 52), zoom = 4) |>
  add_grid(data = crash_data, lat = "lat", lon = "lng", cell_size = 1000,
           elevation_scale = 50, layer_id = "grid_layer",
           colour_range = viridisLite::plasma(6))


key <- 'abc' 

df <- read.csv(paste0(
  'https://raw.githubusercontent.com/uber-common/deck.gl-data/master/',
  'examples/3d-heatmap/heatmap-data.csv'
))

df <- df[!is.na(df$lng), ]


mapdeck( style = mapdeck_style('dark'), pitch = 45 ) %>%
  add_heatmap(
    data = Dt
    , lat = "Latitude"
    , lon = "Longitude"
    , weight = "weight",
    , colour_range = colourvalues::colour_values(1:6, palette = "inferno")
  )

