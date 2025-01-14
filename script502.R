library(tidyverse)

library("RColorBrewer")

#load the data
data <-  read_csv("C:/Users/USER/Desktop/HDS/data visualisation/data/dft-road-casualty-statistics-collision-2022.csv") 
  

data1 <- data %>% 
    mutate( pedestrian_crossing_human_control = ifelse(pedestrian_crossing_human_control == 0, 0, 1), 
            pedestrian_crossing_physical_facilities = ifelse(pedestrian_crossing_physical_facilities == 0, 0, 1))
#load data key
datakey <- read_csv("C:/Users/USER/Desktop/HDS/data visualisation/data/roadsafetykey.csv") %>%
  filter(Dataset == "accidents") %>%
  select(-Id, -Dataset)


# isolate the key for police force
datapoliceforce <- datakey %>%
  filter(Attribute == "Police_Force" )
  
# find the count of accidents for each police force  
policeforcecount <-  inner_join(data, datapoliceforce, by = c( "police_force" = "Code")) %>%
  select(Label) %>%
  group_by(Label) %>%
  summarise(count = n())

#load data key for districts
datadistrict <- read_csv("C:/Users/USER/Desktop/HDS/data visualisation/data/LAD_DEC_2021_UK_NC.csv") %>%
  select(-LAD21NMW)

#find the count of accidents per distric
district <- inner_join(data, datadistrict, by = c( "local_authority_ons_district" = "district_code")) %>%
  select(local_authority_ons_district, district_name) %>%
  group_by(local_authority_ons_district, district_name) %>%
  summarise(count = n())


#isolate data for accident severity
dataseverity <- datakey %>%
  filter(Attribute == "Accident_Severity" )


#isolate data for day of week 
dataday <- datakey %>%
  filter(Attribute == "Day_of_Week" )

#isolate data for road type
dataroad <- datakey %>%
  filter(Attribute == "Road_Type" )

#isolate data for junction detail and control
datajunctiondetail <- datakey %>%
  filter(Attribute == "Junction_Detail" )

datajunctioncontrol <- datakey %>%
  filter(Attribute == "Junction_Control" )

#isolate data for light
datalight <- datakey %>%
  filter(Attribute == "Light_Conditions" )

#data weather 

dataweather <- datakey %>%
  filter(Attribute == "Weather_Conditions" )
#data Road_Surface_Conditions
dataroadcondition <- datakey %>%
  filter(Attribute == "Road_Surface_Conditions" )

# data Special_Conditions_at_Site
dataspeccondition <- datakey %>%
  filter(Attribute == "Special_Conditions_at_Site" )


# data Carriageway_Hazards

datahazard <- datakey %>%
  filter(Attribute == "Carriageway_Hazards" )

# data Urban_or_Rural_Area

dataareatype <- datakey %>%
  filter(Attribute == "Urban_or_Rural_Area" )


#lablelling rows
datarowlabels <- data1 %>% 
  inner_join(datapoliceforce, by = c( "police_force" = "Code")) %>%
  inner_join(datadistrict, by = c( "local_authority_ons_district" = "district_code")) %>%
  inner_join(dataseverity, by = c( "accident_severity" = "Code")) %>%
  inner_join(dataday, by = c( "day_of_week" = "Code")) %>%
  inner_join(dataroad, by = c( "road_type" = "Code")) %>%
  inner_join(datajunctiondetail, by = c( "junction_detail" = "Code")) %>%
  inner_join(datajunctioncontrol, by = c( "junction_control" = "Code")) %>%
  inner_join(datalight, by = c( "light_conditions" = "Code")) %>%
  inner_join(dataweather, by = c( "weather_conditions" = "Code")) %>%
  inner_join(dataroadcondition, by = c( "road_surface_conditions" = "Code")) %>%
  inner_join(dataspeccondition, by = c( "special_conditions_at_site" = "Code")) %>%
  inner_join(datahazard, by = c( "carriageway_hazards" = "Code")) %>%
  inner_join(dataareatype, by = c( "urban_or_rural_area" = "Code")) 

datarowlabels <- datarowlabels %>%
  mutate(police_force = Label.x, accident_severity = Label.y, day_of_week = Label.x.x,
         road_type = Label.y.y, junction_detail = Label.x.x.x, junction_control = Label.y.y.y,
         light_conditions = Label.x.x.x.x, weather_conditions = Label.y.y.y.y,
         road_surface_conditions = Label.x.x.x.x.x, special_conditions_at_site = Label.y.y.y.y.y,
         carriageway_hazards = Label.x.x.x.x.x.x, urban_or_rural_area = Label.y.y.y.y.y.y,
         local_authority_ons_district = district_name)

datarowlabels <- datarowlabels %>%
  select(date, day_of_week, police_force, accident_severity, number_of_vehicles, number_of_casualties,
         time, local_authority_ons_district, road_type, speed_limit, junction_detail, junction_control, pedestrian_crossing_human_control,
         pedestrian_crossing_physical_facilities, light_conditions, weather_conditions, road_surface_conditions,
         special_conditions_at_site, carriageway_hazards, urban_or_rural_area) %>%
  filter(urban_or_rural_area == 'Urban' | urban_or_rural_area == 'Rural') 

datarowlabels <- na.omit(datarowlabels)


datarowlabels$day_of_week <- factor(datarowlabels$day_of_week,
                                    levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                                    ordered = TRUE)


num_colors <- 6
color_palette <- brewer.pal(num_colors, "YlOrRd")

ggplot(datarowlabels, aes(x = day_of_week, y = number_of_casualties, fill = accident_severity)) +
  geom_col() +  
  facet_grid(~urban_or_rural_area) +
  scale_fill_manual(values = rev(color_palette)) +  
  labs(title = "Number of casualties by day of the week and accident severity",
       x = "Day of the week",
       y = "Number of casualties",
       fill = "Accident severity") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 16)
        ) 

filtered_data <- subset(datarowlabels, road_type != "Unknown")

ggplot(filtered_data, aes(x = road_type, fill = as.factor(speed_limit))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = color_palette) + 
  labs(title = "Number of accidents by road type and speed limit",
       x = "Road type",
       y = "Number of collisions",  
       fill = "Speed limit (mph)") +
  theme_minimal()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16)
  )


datarowlabels$hour <- as.numeric(format(as.POSIXct(datarowlabels$time, format = "%H:%M:%S"), "%H"))

# Group the data by hour and calculate the count of collisions
hourly_counts <- aggregate(rep(1, nrow(datarowlabels)), by = list(hour = datarowlabels$hour), sum)


ggplot(hourly_counts, aes(x = hour, y = x)) +
  geom_line(color = brewer.pal(9, "YlOrRd")[5]) +
  geom_ribbon(aes(ymin = 0, ymax = x), fill = brewer.pal(9, "YlOrRd")[5], alpha = 0.3) +
  labs(
    x = "Hour of day",
    y = "Number of collisions",
    title = "Distribution of collisions by hour"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16)
  )


