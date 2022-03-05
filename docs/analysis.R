library(gt)
library(tidyverse)
library(dplyr)
library(zoo)
library(gtsummary)
library(leaflet)
library(ggplot2)
library(plotly)
library(maps)


inc_data <- read.csv(file = "incarceration_trends.csv")
View(inc_data)
inc_data <- data.frame(inc_data)
inc_data <- arrange(inc_data, year)
inc_data$total_pop <- as.numeric(sub(",", "", unlist(inc_data$total_pop, use.names=FALSE)))




urban_pop_kc <- inc_data %>%
  filter(county_name == 'King County')


chart1 <- ggplot(urban_pop_kc, aes(x = year, y = total_pop)) + geom_point() + ylab("Total Incarcerated Population in King County, WA vs TX") +ggtitle("Total Incarcerated Population by Year in King County, WA vs TX", subtitle = waiver())
chart1


df_for_chart2 <- inc_data %>%
  filter(year == '2010')

chart2 <- plot_ly(df_for_chart2, x = ~state, y = ~total_pop, type = 'bar', name = 'Total Incarcerated Population (2010)')  %>% layout(title = 'Total Incarcerated Population by State in 2010', xaxis = list(title = 'State'), yaxis = list(title = "Total Incarcerated Population (2010)"))
chart2


map_county_fips <- readRDS("county_map_fips.rds")
head(map_county_fips)
ggplot() +
  geom_polygon(data = map_county_fips, 
               mapping = aes(x = long, y = lat, group = group)) + 
  coord_quickmap()

map_county_per_diff <- map_county_fips %>%
  left_join(inc_data, by = "fips")

map_state <- map_data("state")
mapchart1 <- ggplot(data = map_county_per_diff, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = total_pop)) + 
  geom_polygon(data = map_state, fill = NA, color = "black") +
  scale_fill_gradient2(low = "white", high = "red") +
  coord_quickmap() + 
  labs(title = "Total Incarcerated Populations since 1970")


