
cens_11fs_r3 = read.csv("1_RawData/cens_11fs_r3.csv")

cens_11fs_r3 = cens_11fs_r3 %>% mutate_if(is.character, as.factor) 
cens_11fs_r3_poptot = cens_11fs_r3 %>% 
  filter(age == "TOTAL") %>% 
  filter(sex == "T") %>% 
  filter(hhstatus == "TOTAL") %>%
  count(geo, wt = values)

nuts0g = st_read("Eurostat/EU_Nuts0.shp")
nuts3g = st_read("Eurostat/EU_Nuts3.shp")

countries = nuts3g %>% pull(CNTR_CODE) %>% unique()
nuts3gEU = nuts3g %>% filter(CNTR_CODE %in% countries)
EU_id = nuts3gEU %>% pull(id) %>% unique() %>% droplevels()
EU_all = append(as.character(countries),as.character(EU_id))
sk = c("DK", "SE", "FI", "NO", "IS")
nuts3gs = nuts3g %>% filter(CNTR_CODE %in% sk)
sk_id = nuts3gs %>% pull(id) %>% unique() %>% droplevels()
sk_all = append(sk, as.character(sk_id))

nuts03g = st_read("Eurostat/EU_Nuts03.shp")

cens_11fs_r3_gpoptot = cens_11fs_r3_poptot %>% left_join(nuts03g, by = c("geo"))
cens_11fs_r3_poptot_sel = cens_11fs_r3_gpoptot %>%
  select(NUTS_NAME, geo, n) %>%
  rename(Country = NUTS_NAME,
         ID = geo,
         'Population (2011)' = n)
cens_11fs_r3_poptot_selEUc = cens_11fs_r3_poptot_sel %>% filter(ID %in% countries)
