

##### --- 1. PCA wide-data 2013-2017 - Preparation into Long for Maps ------


# CarNRG data from PCA
PCA_CAR_NRG_imp = read.csv("1_RawData/PCA_CAR_NRG_imp.csv")
PCA_CAR_NRG_imp = as_tibble(PCA_CAR_NRG_imp)


# Appending CLuster IDs
ClusterID = append(res17.hcpc$data.clust$clust, res13.hcpc$data.clust$clust)

PCA_CAR_NRG_imp = PCA_CAR_NRG_imp %>% mutate(ClusterID = as.factor(ClusterID))

PCA_CAR_NRG_imp = PCA_CAR_NRG_imp %>% 
  mutate(country = str_sub(country, 1,2))


# Euro-GIS NUTS0 data
library(sf)
nuts0g = st_read("1_RawData/EU_Nuts0.shp")


### GIS data
PCA_CAR_NRG_gimp =  nuts0g %>% select(id, NUTS_NAME) %>% 
  left_join(PCA_CAR_NRG_imp, by = c("id" = "country")) %>% 
  filter(!(is.na(year)))


PCA_CAR_NRG_imp_glong = PCA_CAR_NRG_gimp %>% 
  arrange(id, year) %>% 
  gather(Energy, Promilles, 
         -id,
         -NUTS_NAME,
         -year,
         -pop,
         -gdp_ind,
         -gdp_cap,
         -geometry,
         -ClusterID)


PCA_CAR_NRG_imp_glong = PCA_CAR_NRG_imp_glong %>% 
  rename(Country = id,
         Country_Name = NUTS_NAME,
         Year = year,
         Population = pop,
         GDP_Index = gdp_ind,
         GDP_cap = gdp_cap,
         Registration = Promilles)



# PCA_CAR_NRG_imp_glong %>% pull(Energy) %>% unique
PCA_CAR_NRG_imp_glong = PCA_CAR_NRG_imp_glong %>%
  mutate(Energy = fct_recode(Energy,
                             "Diesel" = "DIE",
                             "Petrolium" = "PET",
                             "Petrol excl. hybrids" = "PET_X_HYB",
                             "Diesel excl. hybrids" = "DIE_X_HYB",
                             "Electrical" = "ELC",
                             # "Hybrid electric-petrol" = "ELC_PET_HYB",
                             # "Plug-in hybrid petrol-electric" = "ELC_PET_PI",
                             # "Hybrid diesel-electric" = "ELC_DIE_HYB",
                             # "Plug-in hybrid diesel-electric" = "ELC_DIE_PI",
                             "Alternative" = "ALT",
                             "Gas" = "GAS",
                             "Liquefied petroleum gases" = "LPG",
                             "Other" = "OTH"))



##### --- 2. PCA wide-data GROWTH RATES 2015-2017 - Preparation into Long for Maps ------

# Rownames Back to column!!!
PCA_carnrg_1517_gr_imp  = PCA_carnrg_1517_gr_imp  %>%
  rownames_to_column(var = "Country")

### Getting Cluster IDs for maps
PCA_carnrg_1517_gr_clust = res.hcpc$call$X %>% 
  rownames_to_column(var = "Country") %>% 
  select(Country, ClusterID = clust) %>% 
  arrange(Country)


### Joining CLuster IDs
PCA_carnrg_1517_gr_imp = PCA_carnrg_1517_gr_imp %>% 
  left_join(PCA_carnrg_1517_gr_clust, by = "Country")


# Euro-GIS NUTS0 data
# nuts0g = st_read("1_RawData/EU_Nuts0.shp")


### GIS data
PCA_carnrg_1517_gr_gimp =  nuts0g %>% select(id, NUTS_NAME) %>% 
  left_join(PCA_carnrg_1517_gr_imp, by = c("id" = "Country")) %>% 
  filter(!(is.na(ClusterID))) %>% 
  rename(Country = id,
         Country_Name = NUTS_NAME)


PCA_carnrg_1517_gr_imp_glong = PCA_carnrg_1517_gr_gimp %>% 
  # st_drop_geometry() %>% 
  arrange(Country) %>% 
  select(1:10, 13, 23) %>% 
  gather(Energy, GrowthRates, 
         - Country,
         - Country_Name,
         - gdp_cap,
         - ClusterID,
         - geometry) %>% 
  mutate(Energy = as.factor(Energy))


PCA_carnrg_1517_gr_imp_glong = PCA_carnrg_1517_gr_imp_glong %>%
  rename(GDP_cap = gdp_cap) %>% 
  mutate(Energy = fct_recode(Energy,
                             "Diesel" = "DIE",
                             "Petrolium" = "PET",
                             "Petrol excl. hybrids" = "PET_X_HYB",
                             "Diesel excl. hybrids" = "DIE_X_HYB",
                             "Electrical" = "ELC",
                             # "Hybrid electric-petrol" = "ELC_PET_HYB",
                             # "Plug-in hybrid petrol-electric" = "ELC_PET_PI",
                             # "Hybrid diesel-electric" = "ELC_DIE_HYB",
                             # "Plug-in hybrid diesel-electric" = "ELC_DIE_PI",
                             "Alternative" = "ALT",
                             "Gas" = "GAS",
                             # "Liquefied petroleum gases" = "LPG",
                             "Other" = "OTH"))

