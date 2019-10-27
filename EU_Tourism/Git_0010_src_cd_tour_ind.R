
cntr_tour = read_csv("tourism/Country_Tourism.csv")
city_tour = read_csv("tourism/City_Tourism.csv")


### Checking Country data plots and summary stats!
cntr_tour = cntr_tour %>% 
  mutate(Country = factor(Country),
         Variable = factor(Variable))

city_tour = city_tour %>% 
  mutate(Country = factor(Country),
         Variable = factor(Variable))

ind_country_sel = list("Number of available beds per 1000 residents",
                       "Total nights spent in tourist accommodation establishments per resident population",
                       "Total nights spent in tourist accommodation establishments",
                       "Number of bed-places in tourist accommodation establishments")

ind_city_sel = list("Number of available beds per 1000 residents",
                    "Total nights spent in tourist accommodation establishments per resident population",
                    "Total nights spent in tourist accommodation establishments",
                    "Number of bed-places in tourist accommodation establishments",
                    "Number of cinema seats (total capacity)",
                    "Number of theatres",
                    "Number of public libraries (all distribution points)",
                    "Number of public swimming pools (indoor and outdoor, excluding beaches)")
