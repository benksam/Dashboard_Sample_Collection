

cens_11fs_r3_btw2065_gpairs = st_read("Eurostat/ActivePopData_gpairs.shp")

sk = c("DK", "SE", "FI", "NO", "IS")
nuts3gSKr = nuts3g %>% filter(CNTR_CODE %in% sk)

sk_id = nuts3gSKr %>% pull(id) %>% unique() %>% droplevels()
sk_all = append(sk, as.character(sk_id))

dfSKr = st_drop_geometry(nuts3gSKr) %>% select(id, NUTS_NAME)

cens_11fs_r3_btw2065_pairsSKc = cens_11fs_r3_btw2065_gpairs %>% filter(geo %in% sk)
cens_11fs_r3_btw2065_pairsSKr = cens_11fs_r3_btw2065_gpairs %>% filter(geo %in% sk_id)


# Country CENTROIDS
skcoord = cens_11fs_r3_btw2065_pairsSKc  %>% 
  st_centroid() %>% 
  select(geometry) %>% 
  as_data_frame() %>% 
  separate(geometry, into = c("clng","clat", "void"), sep = " ") %>% 
  select(1,2) %>% 
  mutate(clng = parse_number(clng),
         clat = parse_number(clat))

# Preparing to merge coordinates
skmergid = bind_cols(as_tibble(cens_11fs_r3_btw2065_pairsSKc$geo), skcoord) %>% 
  rename(id = value)

# need to merge country first
prep_merge_skc = cens_11fs_r3_btw2065_pairsSKr %>% 
  mutate(reg = geo) %>% 
  separate(geo, into = c("id", "num"), 2) %>% st_drop_geometry() %>% 
  select(id, reg)


cens_11fs_r3_btw2065_pairsSKr = cens_11fs_r3_btw2065_pairsSKr %>% 
  left_join(prep_merge_skc, by = c("geo" = "reg"))


cens_11fs_r3_btw2065_pairsSKr = cens_11fs_r3_btw2065_pairsSKr %>% 
  left_join(skmergid, by = "id") %>% mutate_if(is.character, as.factor) %>% 
  filter(id != "FI")

Diff_cens_11fs_r3_btw2065_pairsSKr = cens_11fs_r3_btw2065_pairsSKr %>% 
  mutate(Diff = CplPop - MarPop)


### ---- Average Active Population for Scandinavia  ----
# Average Active Population


land = levels(cens_11fs_r3_btw2065_pairsSKr$id)

Av_AP_sk =  cens_11fs_r3_btw2065_pairsSKr %>% 
  st_drop_geometry() %>% 
  select(ID = id, Active_Population = ActivePop) %>% 
  filter(ID != "FI") %>% 
  summarise(Average = mean(Active_Population, na.rm = TRUE))


# Median Active Population

Med_AP_sk =   cens_11fs_r3_btw2065_pairsSKr %>% 
  st_drop_geometry() %>% 
  select(ID = id, Active_Population = ActivePop) %>% 
  filter(ID != "FI") %>% 
  summarise(Median = median(Active_Population, na.rm = TRUE))


# Minimum Active Population


Min_AP_sk =   cens_11fs_r3_btw2065_pairsSKr %>% 
  st_drop_geometry() %>% 
  select(ID = id, Active_Population = ActivePop) %>% 
  filter(ID != "FI") %>% 
  summarise(Minimum= min(Active_Population, na.rm = TRUE))


# Maximum Active Population


Max_AP_sk =   cens_11fs_r3_btw2065_pairsSKr %>% 
  st_drop_geometry() %>% 
  select(ID = id, Active_Population = ActivePop) %>% 
  filter(ID != "FI") %>% 
  summarise(Maximum= max(Active_Population, na.rm = TRUE))

# Sd Active Population


SD_AP_sk =   cens_11fs_r3_btw2065_pairsSKr %>% 
  st_drop_geometry() %>% 
  select(ID = id, Active_Population = ActivePop) %>% 
  filter(ID != "FI") %>% 
  summarise(Standard_Deviation= sd(Active_Population, na.rm = TRUE))

# Range Active Population

Range_AP_sk =  cens_11fs_r3_btw2065_pairsSKr %>% 
  st_drop_geometry() %>% 
  select(ID = id, Active_Population = ActivePop) %>% 
  filter(ID != "FI") %>% 
  summarise(Range = diff(range(Active_Population, na.rm = TRUE)))

Skand_AP = c("Scandinavia", 
             Av_AP_sk,
             Med_AP_sk,
             Min_AP_sk,
             Max_AP_sk,
             SD_AP_sk,
             Range_AP_sk)

### ---- Average Active Population for Scandinavian Countries ----

# Average Active Population
Av_AP = function(country){
  
  cens_11fs_r3_btw2065_pairsSKr %>% 
    st_drop_geometry() %>% 
    select(ID = id, Active_Population = ActivePop) %>% 
    filter(ID == country) %>% 
    summarise(Average = mean(Active_Population, na.rm = TRUE))
  
}


# Median Active Population
Med_AP = function(country){
  
  cens_11fs_r3_btw2065_pairsSKr %>% 
    st_drop_geometry() %>% 
    select(ID = id, Active_Population = ActivePop) %>% 
    filter(ID == country) %>% 
    summarise(Median = median(Active_Population, na.rm = TRUE))
  
}


# Minimum Active Population
Min_AP = function(country){
  
  cens_11fs_r3_btw2065_pairsSKr %>% 
    st_drop_geometry() %>% 
    select(ID = id, Active_Population = ActivePop) %>% 
    filter(ID == country) %>% 
    summarise(Minimum= min(Active_Population, na.rm = TRUE))
  
}


# Maximum Active Population
Max_AP = function(country){
  
  cens_11fs_r3_btw2065_pairsSKr %>% 
    st_drop_geometry() %>% 
    select(ID = id, Active_Population = ActivePop) %>% 
    filter(ID == country) %>% 
    summarise(Maximum= max(Active_Population, na.rm = TRUE))
  
}

# Sd Active Population
SD_AP = function(country){
  
  cens_11fs_r3_btw2065_pairsSKr %>% 
    st_drop_geometry() %>% 
    select(ID = id, Active_Population = ActivePop) %>% 
    filter(ID == country) %>% 
    summarise(Standard_Deviation= sd(Active_Population, na.rm = TRUE))
  
}


# Range Active Population
range_AP = function(country){
  
  cens_11fs_r3_btw2065_pairsSKr %>% 
    st_drop_geometry() %>% 
    select(ID = id, Active_Population = ActivePop) %>% 
    filter(ID == country) %>% 
    summarise(Range = diff(range(Active_Population, na.rm = TRUE)))
  
}


df_ActPop = cbind(Country = land,
                  map_df(land, Av_AP), 
                  map_df(land, Med_AP),
                  map_df(land, Min_AP),
                  map_df(land, Max_AP),
                  map_df(land, SD_AP),
                  map_df(land, range_AP)) %>% 
  filter(Country != "FI")



df_Skand_AP = as.data.frame(set_names(Skand_AP, names(df_ActPop)))

df_ActPop_tot = rbind(df_Skand_AP, df_ActPop)


### --- Couple Proportion ----


# Average Couple Proportion
Av_CP = function(country){
  
  cens_11fs_r3_btw2065_pairsSKr %>% 
    st_drop_geometry() %>% 
    select(ID = id, Couple_Proportion = CplPop) %>% 
    filter(ID == country) %>% 
    summarise(Average = mean(Couple_Proportion, na.rm = TRUE))
  
}


# Median Couple Proportion
Med_CP = function(country){
  
  cens_11fs_r3_btw2065_pairsSKr %>% 
    st_drop_geometry() %>% 
    select(ID = id, Couple_Proportion = CplPop) %>% 
    filter(ID == country) %>% 
    summarise(Median = median(Couple_Proportion, na.rm = TRUE))
  
}


# Minimum Couple Proportion
Min_CP = function(country){
  
  cens_11fs_r3_btw2065_pairsSKr %>% 
    st_drop_geometry() %>% 
    select(ID = id, Couple_Proportion = CplPop) %>% 
    filter(ID == country) %>% 
    summarise(Minimum= min(Couple_Proportion, na.rm = TRUE))
  
}


# Maximum Couple Proportion
Max_CP = function(country){
  
  cens_11fs_r3_btw2065_pairsSKr %>% 
    st_drop_geometry() %>% 
    select(ID = id, Couple_Proportion = CplPop) %>% 
    filter(ID == country) %>% 
    summarise(Maximum= max(Couple_Proportion, na.rm = TRUE))
  
}


# Sd Couple Proportion
SD_CP = function(country){
  
  cens_11fs_r3_btw2065_pairsSKr %>% 
    st_drop_geometry() %>% 
    select(ID = id, Couple_Proportion = CplPop) %>% 
    filter(ID == country) %>% 
    summarise(Standard_Deviation= sd(Couple_Proportion, na.rm = TRUE))
  
}

# Range Couple Proportion
range_CP = function(country){
  
  cens_11fs_r3_btw2065_pairsSKr %>% 
    st_drop_geometry() %>% 
    select(ID = id, Couple_Proportion = CplPop) %>% 
    filter(ID == country) %>% 
    summarise(Range = diff(range(Couple_Proportion, na.rm = TRUE)))
  
}


df_CP = cbind(Country = land,
              map_df(land, Av_CP), 
              map_df(land, Med_CP),
              map_df(land, Min_CP),
              map_df(land, Max_CP),
              map_df(land, SD_CP),
              map_df(land, range_CP)) %>% 
  filter(Country != "FI")



### --- Married Proportion ----


# Average Couple Proportion
Av_MP = function(country){
  
  cens_11fs_r3_btw2065_pairsSKr %>% 
    st_drop_geometry() %>% 
    select(ID = id, Married_Proportion = MarPop) %>% 
    filter(ID == country) %>% 
    summarise(Average = mean(Married_Proportion, na.rm = TRUE))
  
}


# Median Married Proportion
Med_MP = function(country){
  
  cens_11fs_r3_btw2065_pairsSKr %>% 
    st_drop_geometry() %>% 
    select(ID = id, Married_Proportion = MarPop) %>% 
    filter(ID == country) %>% 
    summarise(Median = median(Married_Proportion, na.rm = TRUE))
  
}


# Minimum Married Proportion
Min_MP = function(country){
  
  cens_11fs_r3_btw2065_pairsSKr %>% 
    st_drop_geometry() %>% 
    select(ID = id, Married_Proportion = MarPop) %>% 
    filter(ID == country) %>% 
    summarise(Minimum= min(Married_Proportion, na.rm = TRUE))
  
}

# Maximum Married Proportion
Max_MP = function(country){
  
  cens_11fs_r3_btw2065_pairsSKr %>% 
    st_drop_geometry() %>% 
    select(ID = id, Married_Proportion = MarPop) %>% 
    filter(ID == country) %>% 
    summarise(Maximum= max(Married_Proportion, na.rm = TRUE))
  
}

# Sd Married Proportion
SD_MP = function(country){
  
  cens_11fs_r3_btw2065_pairsSKr %>% 
    st_drop_geometry() %>% 
    select(ID = id, Married_Proportion = MarPop) %>% 
    filter(ID == country) %>% 
    summarise(Standard_Deviation= sd(Married_Proportion, na.rm = TRUE))
  
}

# Range Married Proportion
range_MP = function(country){
  
  cens_11fs_r3_btw2065_pairsSKr %>% 
    st_drop_geometry() %>% 
    select(ID = id, Married_Proportion = MarPop) %>% 
    filter(ID == country) %>% 
    summarise(Range = diff(range(Married_Proportion, na.rm = TRUE)))
  
}

df_MP = cbind(Country = land,
              map_df(land, Av_MP), 
              map_df(land, Med_MP),
              map_df(land, Min_MP),
              map_df(land, Max_MP),
              map_df(land, SD_MP),
              map_df(land, range_MP)) %>% 
  filter(Country != "FI")



### --- DIFFERENCES Couple - Married ----

# Average Couple Proportion
Av_Diff = function(country){
  
  Diff_cens_11fs_r3_btw2065_pairsSKr %>% 
    st_drop_geometry() %>% 
    select(ID = id, Differential = Diff) %>% 
    filter(ID == country) %>% 
    summarise(Average = mean(Differential, na.rm = TRUE))
  
}

# Median Married Proportion
Med_Diff = function(country){
  
  Diff_cens_11fs_r3_btw2065_pairsSKr %>% 
    st_drop_geometry() %>% 
    select(ID = id, Differential = Diff) %>% 
    filter(ID == country) %>% 
    summarise(Median = median(Differential, na.rm = TRUE))
  
}

# Minimum Married Proportion
Min_Diff = function(country){
  
  Diff_cens_11fs_r3_btw2065_pairsSKr %>% 
    st_drop_geometry() %>% 
    select(ID = id, Differential = Diff) %>% 
    filter(ID == country) %>% 
    summarise(Minimum= min(Differential, na.rm = TRUE))
  
}


# Maximum Married Proportion
Max_Diff = function(country){
  
  Diff_cens_11fs_r3_btw2065_pairsSKr %>% 
    st_drop_geometry() %>% 
    select(ID = id, Differential = Diff) %>% 
    filter(ID == country) %>% 
    summarise(Maximum= max(Differential, na.rm = TRUE))
  
}


# Sd Married Proportion
SD_Diff = function(country){
  
  Diff_cens_11fs_r3_btw2065_pairsSKr %>% 
    st_drop_geometry() %>% 
    select(ID = id, Differential = Diff) %>% 
    filter(ID == country) %>% 
    summarise(Standard_Deviation= sd(Differential, na.rm = TRUE))
  
}


# Range Married Proportion
range_Diff = function(country){
  
  Diff_cens_11fs_r3_btw2065_pairsSKr %>% 
    st_drop_geometry() %>% 
    select(ID = id, Differential = Diff) %>% 
    filter(ID == country) %>% 
    summarise(Range = diff(range(Differential, na.rm = TRUE)))
  
}


df_Diff = cbind(Country = land,
                map_df(land, Av_Diff), 
                map_df(land, Med_Diff),
                map_df(land, Min_Diff),
                map_df(land, Max_Diff),
                map_df(land, SD_Diff),
                map_df(land, range_Diff)) %>% 
  filter(Country != "FI")

