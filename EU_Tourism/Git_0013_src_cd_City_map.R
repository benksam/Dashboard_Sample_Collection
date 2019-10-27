

### FULL city data with TOP Selected Cities
city_top_data = read_csv("tourism/city_top_data.csv")

top10_cities_geo = read_csv("tourism/city_top10_geo.csv")

city_top_gtour = city_top_data %>% 
  left_join(top10_cities_geo, by = c("Place" = "City")) %>% mutate(Place = as.factor(Place),
                                                                   Variable = as.factor(Variable))
