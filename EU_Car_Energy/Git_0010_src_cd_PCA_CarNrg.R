


PCA_CAR_NRG_imp = read.csv("1_RawData/PCA_CAR_NRG_imp.csv")

# Countries as rownames for PCA
rownames(PCA_CAR_NRG_imp) <- PCA_CAR_NRG_imp[,1]
PCA_CAR_NRG_imp[,1] <- NULL


######## ----- PCA: base sur 2017 (2013 comme individus supplementaires) -----


res17.pca <- PCA(graph = FALSE, 
                 PCA_CAR_NRG_imp[,-1],
                 ind.sup = 27:50,
                 quanti.sup = c(1:3)) ## Effectue l'ACP



### For Plotting Top Rankings - Wide
data_res17.pca = res17.pca$call$X %>% 
  rownames_to_column(var = "Country") %>% 
  mutate(Country = str_sub(Country, 1,2)) %>% 
  slice(1:26) 



# For Plotting Cluster Plots
res17.hcpc <- HCPC(res17.pca, 
                   nb.clust=5, 
                   kk = Inf, 
                   graph=F, 
                   consol=F)


######## ----- PCA: based on 2017 (2013 as individus supplementaires) -----

# Not Year Column
res13.pca <- PCA(graph = FALSE, 
                 PCA_CAR_NRG_imp[,-1],
                 ind.sup = 1:26,
                 quanti.sup = c(1:3)) ## Effectue l'ACP


### For Plotting Top Rankings - Wide
data_res13.pca = res13.pca$call$X %>% 
  rownames_to_column(var = "Country") %>% 
  mutate(Country = str_sub(Country, 1,2)) %>%
  slice(27:50) 




res13.hcpc <- HCPC(res13.pca, 
                   nb.clust=5, 
                   kk = Inf, 
                   graph=F, 
                   consol=F)



######## ----- PCA: GROWTH RATES 2015-2017 (Imputations by variables in Levels) -----



PCA_carnrg_1517_gr_imp = read.csv("1_RawData/PCA_carnrg_1517_gr_imp.csv")

# Countries as rownames for PCA
rownames(PCA_carnrg_1517_gr_imp) <- PCA_carnrg_1517_gr_imp[,1]
PCA_carnrg_1517_gr_imp[,1] <- NULL


# PCA thanks to imputed growth_rates 
res.pca <- PCA(PCA_carnrg_1517_gr_imp,
               quanti.sup = c(9:20)) 


res.hcpc <- HCPC(res.pca, 
                   nb.clust=4, 
                   kk = Inf, 
                   graph=F, 
                   consol=F)


## Long data for GROWTH RATE 
carnrg1517_gr_long = read_csv("1_RawData/carnrg1517_gr_long.csv")






