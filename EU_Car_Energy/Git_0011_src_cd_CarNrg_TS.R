

carnrg <- read_csv("1_RawData/carnrg.csv")


# Sorted by carburant for LONG data SHAPING!!!
carb_TS_alph <- function(year) {
  carnrg %>% 
    filter(time == year) %>% 
    count(mot_nrg, wt = values) %>% 
    arrange(mot_nrg)
}

Year = as.list(2013:2017)


# Sorted Alphabetically by Carburant
list_carb_year = lapply(Year, carb_TS_alph) 
list_carb_year_cols = lapply(Year, carb_TS_alph) %>% bind_cols()


### Augment the Year list with duplicates preparing for LONG merging
Year_long = modify(Year, ~rep(.x, 17)) %>% enframe %>% unnest %>% select(-1)

list_carb_year_long = list_carb_year %>% bind_rows()

list_carb_year_long = cbind(Year_long, list_carb_year_long)
carb_year_long = list_carb_year_long %>% rename(year = value)




##### --- TS relevant plots -------  

carb_year_long = carb_year_long %>% rename(Year = year,
                                           Energy = mot_nrg,
                                           Registrations = n) %>% 
  mutate_if(is.character, as.factor) %>% 
  filter(Energy %in% c("DIE",
                       "PET",
                       "PET_X_HYB",
                       "DIE_X_HYB",
                       "ELC",
                       "ELC_PET_HYB",
                       "ELC_PET_PI",
                       "ELC_DIE_HYB",
                       "ELC_DIE_PI",
                       "ALT",
                       "GAS",
                       "LPG",
                       "OTH")) %>% 
  mutate(Energy = fct_recode(Energy,
                             "Diesel" = "DIE",
                             "Petrolium" = "PET",
                             "Petrol excl. hybrids" = "PET_X_HYB",
                             "Diesel excl. hybrids" = "DIE_X_HYB",
                             "Electrical" = "ELC",
                             "Hybrid electric-petrol" = "ELC_PET_HYB",
                             "Plug-in hybrid petrol-electric" = "ELC_PET_PI",
                             "Hybrid diesel-electric" = "ELC_DIE_HYB",
                             "Plug-in hybrid diesel-electric" = "ELC_DIE_PI",
                             "Alternative" = "ALT",
                             "Gas" = "GAS",
                             "Liquefied petroleum gases" = "LPG",
                             "Other" = "OTH"))



