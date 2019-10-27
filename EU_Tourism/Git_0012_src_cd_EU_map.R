

nuts0g = st_read("Eurostat/EU_Nuts0.shp")

cntr_gtour = nuts0g %>% select(id) %>% 
  left_join(cntr_tour , by = c("id" = "Acronym")) 

cntr_gtour = cntr_gtour %>% filter(!(is.na(Value)))

cntr_tour = cntr_tour %>% mutate_if(is.character, as.factor)


cntr_tour_BedPerRes = cntr_tour %>% filter(Variable == "Number of available beds per 1000 residents")
cntr_tour_BedPlaces = cntr_tour %>% filter(Variable == "Number of bed-places in tourist accommodation establishments")
cntr_tour_Nights = cntr_tour %>% filter(Variable == "Total nights spent in tourist accommodation establishments")
cntr_tour_NightsPerRes = cntr_tour %>% filter(Variable == "Total nights spent in tourist accommodation establishments per resident population")
