
demo_r_d3dens = read.csv("1_RawData/demo_r_d3dens.csv")


demo_r_d3dens_EU17 = demo_r_d3dens %>% filter(time == 2017) %>% 
  filter(geo %in% EU_all) %>% select(-time, unit)
demo_r_d3dens_EU17r =  demo_r_d3dens_EU17 %>% filter(geo %in% EU_id)
rdensity_EU = nuts03g %>% right_join(demo_r_d3dens_EU17r, by = c("id" = "geo")) %>% select(id, NUTS_NAME,values)


# Extra data wrangling for COUNTRY CENTROIDS joined to Regional Density data
demo_r_d3dens_EU17c =  demo_r_d3dens_EU17 %>% filter(geo %in% countries)
rdensity_EUc = nuts03g %>% right_join(demo_r_d3dens_EU17c, by = c("id" = "geo")) %>% select(id, NUTS_NAME, values)


# Country CENTROIDS
ccoord = rdensity_EUc %>% 
  st_centroid() %>% 
  select(geometry) %>% 
  as_data_frame() %>% 
  separate(geometry, into = c("clng","clat", "void"), sep = " ") %>% 
  select(1,2) %>% 
  mutate(clng = parse_number(clng),
         clat = parse_number(clat))


cmergid = bind_cols(as_tibble(rdensity_EUc$id), ccoord) %>% 
  rename(id = value)

rdensity_EUc = rdensity_EUc %>% left_join(cmergid, by = "id")


# Merging centroid clng/clat of Country-NUTS0 with NUTS3 data
nuts3gEU = nuts3gEU %>% left_join(rdensity_EUc %>%
                                    st_drop_geometry() %>%
                                    select(id, clng, clat), by = c("CNTR_CODE" = "id"))

dfnuts3gEU = nuts3gEU %>% st_drop_geometry() %>% select(id, CNTR_CODE, clng, clat)

rdensity_EUaug = rdensity_EU %>% 
  left_join(dfnuts3gEU, by= "id") %>% 
  mutate(CNTR_CODE = as_factor(CNTR_CODE))

# glimpse(rdensity_EUaug)


### Summary density data
Europe_sum = rdensity_EUaug %>% st_drop_geometry() %>%
  select(ID = id, Region = NUTS_NAME, Density = values) %>% 
  summarise(Average = mean(Density),
            Median = median(Density),
            Minimum = min(Density),
            Maximum = max(Density),
            Range = diff(range(Density)),
            Standard_Deviation = sd(Density))

row.names(Europe_sum) = "Europe"

Scandinavia_sum = rdensity_EUaug %>% st_drop_geometry() %>% 
  select(ID = id, Region = NUTS_NAME, Density = values, Country = CNTR_CODE) %>% 
  filter(Country %in% sk) %>% 
  summarise(Average = mean(Density, na.rm = TRUE),
            Median = median(Density, na.rm = TRUE),
            Minimum = min(Density, na.rm = TRUE),
            Maximum = max(Density, na.rm = TRUE),
            Range = diff(range(Density, na.rm = TRUE)),
            Standard_Deviation = sd(Density, na.rm = TRUE))

row.names(Scandinavia_sum) = "Scandinavia"

Sweden_sum = rdensity_EUaug %>% st_drop_geometry() %>% 
  select(ID = id, Region = NUTS_NAME, Density = values, Country = CNTR_CODE) %>% 
  filter(Country %in% "SE") %>% 
  summarise(Average = mean(Density, na.rm = TRUE),
            Median = median(Density, na.rm = TRUE),
            Minimum = min(Density, na.rm = TRUE),
            Maximum = max(Density, na.rm = TRUE),
            Range = diff(range(Density, na.rm = TRUE)),
            Standard_Deviation = sd(Density, na.rm = TRUE))

row.names(Sweden_sum) = "Sweden"


Norway_sum = rdensity_EUaug %>% st_drop_geometry() %>% 
  select(ID = id, Region = NUTS_NAME, Density = values, Country = CNTR_CODE) %>% 
  filter(Country %in% "NO") %>% 
  summarise(Average = mean(Density, na.rm = TRUE),
            Median = median(Density, na.rm = TRUE),
            Minimum = min(Density, na.rm = TRUE),
            Maximum = max(Density, na.rm = TRUE),
            Range = diff(range(Density, na.rm = TRUE)),
            Standard_Deviation = sd(Density, na.rm = TRUE))

row.names(Norway_sum) = "Norway"

Denmark_sum = rdensity_EUaug %>% st_drop_geometry() %>% 
  select(ID = id, Region = NUTS_NAME, Density = values, Country = CNTR_CODE) %>% 
  filter(Country %in% "DK") %>% 
  summarise(Average = mean(Density, na.rm = TRUE),
            Median = median(Density, na.rm = TRUE),
            Minimum = min(Density, na.rm = TRUE),
            Maximum = max(Density, na.rm = TRUE),
            Range = diff(range(Density, na.rm = TRUE)),
            Standard_Deviation = sd(Density, na.rm = TRUE))

row.names(Denmark_sum) = "Denmark"

Finland_sum = rdensity_EUaug %>% st_drop_geometry() %>% 
  select(ID = id, Region = NUTS_NAME, Density = values, Country = CNTR_CODE) %>% 
  filter(Country %in% "FI") %>% 
  summarise(Average = mean(Density, na.rm = TRUE),
            Median = median(Density, na.rm = TRUE),
            Minimum = min(Density, na.rm = TRUE),
            Maximum = max(Density, na.rm = TRUE),
            Range = diff(range(Density, na.rm = TRUE)),
            Standard_Deviation = sd(Density, na.rm = TRUE))

row.names(Finland_sum) = "Finland"


Iceland_sum = rdensity_EUaug %>% st_drop_geometry() %>% 
  select(ID = id, Region = NUTS_NAME, Density = values, Country = CNTR_CODE) %>% 
  filter(Country %in% "IS") %>% 
  summarise(Average = mean(Density, na.rm = TRUE),
            Median = median(Density, na.rm = TRUE),
            Minimum = min(Density, na.rm = TRUE),
            Maximum = max(Density, na.rm = TRUE),
            Range = diff(range(Density, na.rm = TRUE)),
            Standard_Deviation = sd(Density, na.rm = TRUE))

row.names(Iceland_sum) = "Iceland"

EUDensity_Sum = rbind(Europe_sum, Scandinavia_sum, Sweden_sum, Norway_sum, Denmark_sum,
                      Finland_sum, Iceland_sum)

EUDensity_Sum = EUDensity_Sum %>% select(1:4,6,5) %>% rownames_to_column(var = "Country")

