
city_top_gtour = read_csv("tourism/city_top_tour_coord.csv")

city_top_gtour = city_top_gtour %>% mutate(Place = as.factor(Place),
                                           Variable = as.factor(Variable),
                                           Indicator = as.factor(Indicator),
                                           Country = as.factor(Country)) 