#-------------------------------------
# ERMN Forest Data Summaries: Regeneration
#-------------------------------------

#---- Functions ----
# Write dataframe to shapefile using common settings
write_to_shp <- function(data, x = "X", y = "Y", shp_name){
  st_write(st_as_sf(data, coords = c(x, y), crs = park_crs),
           shp_name, delete_layer = FALSE)#FALSE)
}


#---- Eventually delete this whole section once source_script_ERMN is correct ----
setwd('C:/ERMN Veg/Veg_Data_2022/ERMNforest_summaries')
tablepath = 'C:/ERMN Veg/Veg_Data_2022/ERMNforest_summaries/draft_tables/FONE/'
shppath = 'C:/ERMN Veg/Veg_Data_2022/ERMNforest_summaries/shapefiles/FONE/'

importData(type='DSN', odbc="ERMNVeg20241211")


# Set parameters
park = 'FRHI'
years = c(2007:2024)
from = 2007
from_4yr = 2019 # earliest year in the latest four year cycle (to - 4). If to = 2023, then from_4yr = 2018
to = 2024
QAQC = FALSE
retired = TRUE
anrevisit = FALSE
cycle_latest = 4
park_crs = ifelse(park %in% c("DEWA"), 26918, 26917)
num_plots = case_when(park == "ALPO" ~ 23,
                      park == "BLUE" ~ 40,
                      park == "DEWA" ~ 102,
                      park == "FONE" ~ 20,
                      park == "FRHI" ~ 20,
                      park == "FLNI" ~ 12,
                      park == "GARI" ~ 43,
                      park == "JOFL" ~ 12,
                      park == "NERI" ~ 102)
plot_size = 707
# from_prev = 2016
# to_prev = 2019

# args_all = list(park = park, years = years, QAQC = QAQC, retired = retired, anrevisit = anrevisit)
# args_4yr = list(park = park, from = from_4yr, to = to, QAQC = QAQC, retired = retired, anrevisit = anrevisit)


#---- Plot event lists ----
plotevs <- joinLocEvent(park = park, years = years, QAQC = QAQC, retired = retired, anrevisit = anrevisit)
plotevs_4yr <- plotevs %>% filter(between(Year, from_4yr, to))


#---- Table 1. Regen densities of native canopy species by plot and year ----
reg <- joinRegenData(speciesType = 'native', canopyForm = 'canopy', units = 'ha',
                     park=park, years=c(from:to), QAQC = QAQC, retired = retired, anrevisit = anrevisit)

reg_cycle_table <- reg %>% group_by(Plot_Name, Cycle) %>% 
                           summarize(seed_den = round(sum(tot.seed.dens, na.rm = TRUE), 2),
                                     sap_den = round(sum(sap.dens, na.rm = TRUE), 2),
                                     stock = round(sum(ave.stock5u, na.rm = TRUE), 2), 
                                     .groups = 'drop') %>% 
                           left_join(plotevs %>% 
                                       select(Plot_Name, Cycle),
                                     ., by = c("Plot_Name", "Cycle"))

reg_cols <- c("seed_den", "sap_den", "stock")
reg_cycle_table[, reg_cols][is.na(reg_cycle_table[, reg_cols])] <- 0 

reg_cycle_wide <- reg_cycle_table %>% 
  pivot_wider(names_from = Cycle, values_from = c(seed_den, sap_den, stock)) %>% arrange(Plot_Name)


write.csv(reg_cycle_wide, 
          paste0(tablepath, "Table_1_", park, "_regen_by_cycle.csv"), row.names = FALSE)


#---- Map 1 Regen of native canopy species by cycle ----
reg_cycle <- reg %>% group_by(Plot_Name, Cycle) %>% 
                     summarize(regen.dens = sum(regen.dens, na.rm = TRUE), .groups = 'drop') %>% 
                     left_join(plotevs %>% select(Plot_Name, Plot_Number, X_Coord, Y_Coord, Cycle), ., 
                               by = c("Plot_Name", "Cycle")) %>% 
                     arrange(Plot_Name, Cycle) %>% 
                     pivot_wider(names_from = Cycle, values_from = regen.dens, 
                                 names_prefix = "Cycle_", values_fill = 0) %>% 
                     rename(X = X_Coord, Y = Y_Coord) #abbr for shapefile

write_to_shp(reg_cycle, 
             shp_name = paste0(shppath, "Map_1_", park, "_regen_by_cycle_", to, ".shp" ))



#---- Map 2 Regen of native canopy species by size class over the past four years ----
reg_sz_cols <- c("seedht0.dens", "seedht1.dens", "seedht2.dens", "seedht3.dens", "seedht4.dens", "sap.dens", "regen.dens") 

reg_size <- reg %>% select(Plot_Name, Year, all_of(reg_sz_cols))

reg_size_4yr <- reg_size %>% filter(between(Year, from_4yr, to)) %>% 
                             left_join(plotevs_4yr %>% select(Plot_Name, Plot_Number, X_Coord, Y_Coord), ., by = 'Plot_Name') %>%
                             rename(X = X_Coord, Y = Y_Coord) %>% 
                             arrange (Plot_Name, Year)

reg_size_4yr[, reg_sz_cols][reg_size_4yr[is.na(reg_size_4yr[,reg_sz_cols])]] <- 0

write_to_shp(reg_size_4yr, 
             shp_name = paste0(shppath, "Map_2_", park, 
                               "_regen_by_size_class_cycle_", cycle_latest, ".shp"))

#####---- Figure 1A Regen trends by size class ----  NOT YET NEEDS TO BE UPDATED FOR ERMN #####
# Note that I'm combining 5-6 years into cycle 4; need to add note to figure caption
reg_vs <- do.call(joinRegenData, 
                  args = c(args_vs, speciesType = 'native', 
                           canopyForm = 'canopy', units = 'sq.m'))

reg_size_cy <- reg_vs %>% group_by(Plot_Name, cycle) %>% 
                          summarize_at(vars(all_of(reg_sz_cols)), sum, na.rm = TRUE) %>% 
                          left_join(plotevs_vs %>% select(Plot_Name, cycle),
                                    ., by = c("Plot_Name", "cycle")) 

reg_size_cy[reg_sz_cols][reg_size_cy[is.na(reg_sz_cols)]] <- 0

# Use forestTrends to generate loess-smoothed CIs
reg_smooth <- map_dfr(reg_sz_cols, 
                      ~case_boot_loess(reg_size_cy, x = 'cycle', y = .x, ID = 'Plot_Name', 
                                     span = 8/4, num_reps = 250, chatty = TRUE) %>% 
                      mutate(size_class = .x)
)

# For plot labels
reg_colors <- c("seed_15_30cm" = "#D6D6FF", 
                "seed_30_100cm" = "#8F97E3", 
                "seed_100_150cm" = "#556CC9", 
                "seed_p150cm" = "#244EAD", 
                "sap_den" = "#05e646")

reg_smooth$size_class <- factor(reg_smooth$size_class, 
                                levels = c("seed_15_30cm", "seed_30_100cm", 
                                           "seed_100_150cm", "seed_p150cm", 
                                           "sap_den"))

cycle_labs = c("1" = "Cycle 1: 2006 \u2013 2009",
               "2" = "Cycle 2: 2010 \u2013 2013", 
               "3" = "Cycle 3: 2014 \u2013 2017", 
               "4" = "Cycle 4: 2018 \u2013 2022",
               "5" = "Cycle 5: 2023")

reg_labels <- c("15 \u2013 30 cm", "30 \u2013 100 cm", "100 \u2013 150 cm",
                ">150 cm & < 1 cm DBH", "Saplings: 1 \u2013 9.9 cm DBH")

reg_trend_plot <- 
  ggplot(reg_smooth, aes(x = size_class, y = estimate, color = size_class,#linetype = sign, 
                         group = size_class))+ theme_FHM()+
  geom_bar(stat = 'identity', aes(fill = size_class, color = 'DimGrey'))+
  geom_errorbar(aes(ymin = lower95, ymax = upper95), width = 0.2, linewidth = 0.5, 
                color = 'DimGrey', alpha = 0.8)+
  labs(x  = "Cycle", y = bquote(Stems/m^2))+
  facet_wrap(~cycle, labeller = as_labeller(cycle_labs), ncol = 5)+
  scale_color_manual(values = reg_colors, name = "Size Class",
                     labels = reg_labels)+
  scale_fill_manual(values = reg_colors, name = "Size Class",
                    labels = reg_labels)+
  scale_x_discrete(name = "Size Class",
                   labels = reg_labels)+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
        legend.position = 'none')

reg_trend_plot
  
ggsave(paste0(new_path, "figures/", "Figure_1A_", park, "_regen_by_size_class_by_cycle.svg"),
       height = 5, width = 7.5, units = 'in')

#####---- Figure 1B Diam. dist. trends by size class ---- NOT YET NEEDS TO BE UPDATED FOR ERMN #####
  # Note that I'm combining 5-6 years into cycle 4; need to add note to figure caption
  # Including all species and canopy forms
tree_dd <- do.call(sumTreeDBHDist, args = c(args_vs, status = 'live'))

dbh_cols <- c('dens_10_19.9', 'dens_20_29.9', 'dens_30_39.9', 'dens_40_49.9', 
              'dens_50_59.9', 'dens_60_69.9', 'dens_70_79.9', 'dens_80_89.9',
              'dens_90_99.9', 'dens_100p')

tree_dbh_sm <- map_dfr(dbh_cols, 
                    ~case_boot_loess(tree_dd, x = 'cycle', y = .x, ID = 'Plot_Name', 
                                     span = 8/4, num_reps = 250, chatty = TRUE) %>% 
                     mutate(dbh_class = .x)
)

tree_dbh_sm$estimate[tree_dbh_sm$estimate < 0] <- 0

tree_dbh_sm$dbh_class <- factor(tree_dbh_sm$dbh_class, 
                                levels = dbh_cols)
levels(tree_dbh_sm$dbh_class)

dbh_labels <- c("10 \u2013 19.9 cm", 
                "20 \u2013 29.9 cm", 
                "30 \u2013 39.9 cm", 
                "40 \u2013 49.9 cm", 
                "50 \u2013 59.9 cm", 
                "60 \u2013 69.9 cm", 
                "70 \u2013 79.9 cm", 
                "80 \u2013 89.9 cm", 
                "90 \u2013 99.9 cm",
                ">= 100 cm")

dbh_labels <- c("10", 
                "20", 
                "30", 
                "40", 
                "50", 
                "60", 
                "70", 
                "80", 
                "90",
                "100+")

# check flat diameter distribution
head(tree_dbh_sm)

e <- min(tree_dbh_sm$estimate[tree_dbh_sm$estimate > 0])

tree_dbh_sm <- suppressWarnings(# warning is for 'c', which we're not using
  tree_dbh_sm %>% 
  mutate(dbh_class2 = gsub("p", "", dbh_class)) %>% 
  separate(dbh_class2, into = c("a", "b", "c"), sep = "_", remove = FALSE) %>% 
  mutate(class = as.numeric(b)) %>% select(-a, -b, -c, -dbh_class2))

AIC_test <- map_dfr(seq_along(cycle_labs), function(x){
  df <- tree_dbh_sm %>% filter(cycle == x)
  lin_mod <- lm(estimate ~ class, data = df)
  exp_mod <- lm(log(estimate + e) ~ class, data = df)
  aic_check <- data.frame(cycle = x,
                          linear = AIC(lin_mod),
                          exp = AIC(exp_mod) + sum(2*log(df$estimate + e)))
})

AIC_test$best_mod <- ifelse(AIC_test$linear < AIC_test$exp + 4, "linear", "log-linear")
# added 4 to exp because linear is the simpler model. The log-linear model needs
# to be < 4 points lower than linear AIC to count.

cycle_labs_tr = c("1" = ifelse(AIC_test$best_mod[AIC_test$cycle == 1] == 'linear',
                               paste0(cycle_labs[1], "*"), cycle_labs[1]),
                  "2" = ifelse(AIC_test$best_mod[AIC_test$cycle == 2] == 'linear',
                               paste0(cycle_labs[2], "*"), cycle_labs[2]),
                  "3" = ifelse(AIC_test$best_mod[AIC_test$cycle == 3] == 'linear',
                               paste0(cycle_labs[3], "*"), cycle_labs[3]),
                  "4" = ifelse(AIC_test$best_mod[AIC_test$cycle == 4] == 'linear',
                               paste0(cycle_labs[4], "*"), cycle_labs[4]),
                  "5" = ifelse(AIC_test$best_mod[AIC_test$cycle == 5] == 'linear',
                               paste0(cycle_labs[5], "*"), cycle_labs[5]))

dbh_trend_plot <- 
  ggplot(tree_dbh_sm, aes(x = dbh_class, y = estimate))+ theme_FHM()+
  geom_bar(stat = 'identity', fill = "#81B082" , color = 'DimGrey')+
  geom_errorbar(aes(ymin = lower95, ymax = upper95), width = 0.2, linewidth = 0.5, 
                color = 'DimGrey', alpha = 0.8)+
  labs(x  = "Cycle", y = "Stems/ha")+
  facet_wrap(~cycle, labeller = as_labeller(cycle_labs_tr), ncol = 5)+
  scale_color_manual(values = reg_colors, name = "DBH Size Class",
                     labels = reg_labels)+
  scale_fill_manual(values = reg_colors, name = "DBH Size Class",
                    labels = reg_labels)+
  scale_x_discrete(name = "DBH Size Class",
                   labels = dbh_labels)+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
        legend.position = 'none')

dbh_trend_plot

ggsave(paste0(new_path, "figures/", "Figure_1B_", park, "_tree_dbh_dist_by_cycle.svg"),
       height = 4.6, width = 7.8, units = 'in')

#---- Map 3 Regen by composition including all species ----
spp_grps <- read.csv("ERMN_tree_species_groups.csv")

reg_spp_all <- joinRegenSpData(speciesType = 'all', canopyForm = 'all',  units = 'ha',
                               park = park, years = c(from_4yr:to), QAQC=QAQC, rejected=rejected, anrevisit=anrevisit)

regen_spp_grps <- merge(reg_spp_all,spp_grps[,c("Latin_name","ERMNGroup")], by="Latin_name", all.x=T)

regen_wide1 <- regen_spp_grps %>% group_by(Plot_Name, ERMNGroup) %>%
  summarize(grp_regen_dens = sum(regen.dens)) %>% unique(., by = "Plot_Name")

regen_wide2 <- merge(regen_wide1,plotevs[,c("Plot_Name", "Plot_Number", "X_Coord","Y_Coord")], by="Plot_Name", all.x=T) %>%
  unique(., by = c("Plot_Name", "ERMNGroup")) %>% arrange(Plot_Name,ERMNGroup) %>% mutate (X = X_Coord, Y = Y_Coord)

regen_wide <- regen_wide2 %>% pivot_wider(names_from = ERMNGroup, values_from = grp_regen_dens, values_fill = 0) %>% 
  arrange(Plot_Name) %>% select(-c("X_Coord", "Y_Coord"))

reg_plot_total <- reg_spp_all %>% group_by(Plot_Name) %>% summarise(regen.plot.total = sum(regen.dens))

regen_sp_final <- merge(regen_wide, reg_plot_total, by = "Plot_Name", all.x=T)

write_to_shp(regen_sp_final, shp_name = paste0(shppath, "Map_3_", park, "_regen_by_spp_cycle", cycle_latest, ".shp"))


#---- Map 4 Tree canopy composition including all species ----
spp_grps <- read.csv("ERMN_tree_species_groups.csv")

trees_4yr <- joinTreeData(status = 'live', speciesType = 'all', canopyPosition = 'all',
                          park=park, years=c(from_4yr:to), QAQC=QAQC, rejected=rejected, anrevisit=anrevisit)

trees_4yr_grps <- merge(trees_4yr,spp_grps[,c("Latin_name","ERMNGroup")], by="Latin_name", all.x=T)

trees_wide1 <- trees_4yr_grps %>% group_by(Plot_Name, Plot_Number, X_Coord, Y_Coord, ERMNGroup) %>%
  summarize(BA_sum = sum(BA_cm2)) %>% mutate (X = X_Coord, Y = Y_Coord) 

trees_wide <- trees_wide1 %>% pivot_wider(names_from = ERMNGroup, values_from = BA_sum, values_fill = 0) %>% 
  arrange(Plot_Name)

ba_plot_total <- trees_4yr_grps %>% group_by(Plot_Name) %>% summarise(plot.ba.total = (sum(BA_cm2)/707))
# This reports basal area in m2/ha. 

trees_final <- merge(trees_wide, ba_plot_total, by = "Plot_Name", all.x=T) %>% select(-c("X_Coord", "Y_Coord"))

write_to_shp(trees_final, shp_name = paste0(shppath, "Map_4_", park, "_treeBA_by_spp_cycle", cycle_latest, ".shp"))
# This reports basal area in m2/ha.

#---- Map 5 Regen stocking index ----
reg_stock_4yr <- reg %>% select(Plot_Name, Year, ave.stock5u) %>% filter(between(Year, from_4yr, to)) %>% 
  left_join(plotevs_4yr %>% select(Plot_Name, Plot_Number, X_Coord, Y_Coord), ., by = 'Plot_Name') %>%
  rename(X = X_Coord, Y = Y_Coord) %>% 
  arrange (Plot_Name, Year)

write_to_shp(reg_stock_4yr, shp_name = 
         paste0(shppath, "Map_5_", park, "_stock_index_cycle_", cycle_latest, ".shp"))

#---- Map 6 Deer Browse Index ----
stand_4yr <- joinStandData(park=park, years=c(from_4yr:to), QAQC=QAQC, rejected=rejected, anrevisit=anrevisit) %>% 
  select(Plot_Name, Year, Browse_Index) 

dbi <- left_join(plotevs_4yr %>% select(Plot_Name, Plot_Number, X = X_Coord, Y = Y_Coord),
                 stand_4yr, by = "Plot_Name") %>% 
       select(Plot_Name, Plot_Number, X, Y, DBI = Browse_Index)

mean(dbi$DBI) #FRHI = 3.45 

write_to_shp(dbi, shp_name = 
         paste0(shppath, "Map_6_", park, "_browse_index_cycle_", cycle_latest, ".shp"))


# ---- Map 7 Canopy Cover Trends --- ####
stand_allyr <- joinStandData(park=park, years=c(from:to), QAQC=QAQC, rejected=rejected, anrevisit=anrevisit) %>% 
  select(Plot_Name, Cycle, Crown_Closure_Class, Crown_Closure_Pct) 

cancov1 <- stand_allyr %>% mutate (CanopyCov = ifelse(is.na(Crown_Closure_Pct), Crown_Closure_Class, Crown_Closure_Pct)) %>%
  select(Plot_Name, Cycle, CanopyCov)

cancov_wide <- cancov1 %>% pivot_wider(names_from = Cycle, names_prefix = "Cycle", values_from = CanopyCov)

cancov <- left_join(plotevs_4yr %>% select(Plot_Name, Plot_Number, X = X_Coord, Y = Y_Coord),
                    cancov_wide, by = "Plot_Name") %>% 
  select(Plot_Name, Plot_Number, X, Y, Cycle1, Cycle2, Cycle3, Cycle4)

write_to_shp(cancov, shp_name = paste0(shppath, "Map_7_", park, "_canopy_cover.shp"))





#### ---- Map 8 Invasive % Cover by Cycle ---- 
invcov <- joinQuadData(speciesType = 'invasive', GrowthForm = 'all', 
                       park = park, years = years, QAQC = QAQC, retired = retired, anrevisit = anrevisit)

invcov_wider <- invcov %>% select(Plot_Name, Plot_Number, Cycle, X=X_Coord, Y=Y_Coord, ave.q.cov) %>% 
  pivot_wider(names_from = Cycle, names_prefix = "Cycle", values_from = ave.q.cov) 

write_to_shp(invcov_wider, shp_name = 
               paste0(shppath, "Map_8_", park, "_invas_cover_cycle_", cycle_latest, ".shp"))

# ---- Map 9 Invasive % Cover by Species ----
# Generate comprehensive species list for all invasive plants observed in ERMN
invspp_allobs <- joinQuadSpData(speciesType = 'invasive', GrowthForm = 'all', 
                             park = "all", years = years, QAQC = QAQC, retired = retired, anrevisit = anrevisit)

invspp_all_unique <- invspp_allobs %>% select(Latin_name) %>% unique() %>% arrange(Latin_name)

# Invasive plant data for PARK
invspp_park <- joinQuadSpData(speciesType = 'invasive', GrowthForm = 'all', 
                                park = park, years = years, QAQC = QAQC, retired = retired, anrevisit = anrevisit)

park_invspp_list <- invspp_park %>%  select(Latin_name) %>% unique() %>% arrange(Latin_name)

######################### THIS IS THE PLACE TO START!! ###################################################################

# Lump some species in the same genus
invspp_4yr <- joinQuadSpecies(from = from_4yr, to = to, speciesType = 'invasive') %>% 
  select(ScientificName) %>% unique() %>% arrange(ScientificName)

table(invspp_4yr$ScientificName)

Ligustrum = c("Ligustrum", "Ligustrum obtusifolium", "Ligustrum vulgare")
Lonicera = c("Lonicera - Exotic", "Lonicera morrowii", "Lonicera X bella", "Lonicera", 
             "Lonicera maackii")
Vincetoxicum = c("Vincetoxicum", "Vincetoxicum hirundinaria", "Vincetoxicum nigrum",
                 "Vincetoxicum rossicum")
Elaeagnus = c("Elaeagnus", "Elaeagnus angustifolia", "Elaeagnus umbellata")
Euonymus = c("Euonymus", "Euonymus alatus", "Euonymus atropurpureus")
Centaurea = c("Centaurea", "Centaurea jacea", "Centaurea stoebe")


# Determine 12 most common invasives in a park by cover
invspp_4yr <- joinQuadSpData(speciesType = 'invasive', GrowthForm = 'all', 
                             park = park, years=c(from_4yr:to), QAQC = QAQC, retired = retired, anrevisit = anrevisit)

invspp_4yr$present <- 1

plotspp_df <- data.frame(expand.grid(unique(invspp_4yr$Plot_Name), 
                                     unique(invspp_4yr$Latin_name), 
                                     stringsAsFactors = FALSE)) %>% 
  rename(Plot_Name = Var1, Latin_name = Var2) %>% 
  filter(!is.na(Latin_name))

topspp <- invspp_4yr %>% left_join(plotspp_df, ., by = c('Plot_Name', 'Latin_name')) %>% 
  arrange(Plot_Name, Latin_name) %>% 
  mutate(ave.q.cov = replace_na(ave.q.cov, 0),
         present = replace_na(present, 0)) %>% 
  group_by(Latin_name) %>% 
  summarize(avg_cov = mean(ave.q.cov, na.rm = T),
            num_plots = sum(present), .groups = 'drop') %>% 
  arrange(desc(avg_cov)) %>% slice(1:12) %>% select(Latin_name)

# Prep for shapefile
invspp <- invspp1 %>% 
  mutate(ScientificName = case_when(
    ScientificName %in% topspp$ScientificName ~ ScientificName, 
    ScientificName == "None present" ~ "None present", 
    TRUE ~ "Other invasive")) %>% 
  group_by(Plot_Name, ScientificName) %>% 
  summarize(quad_cov = sum(quad_avg_cov), .groups = 'drop') %>% 
  left_join(plotevs_4yr %>% select(Plot_Name, X = xCoordinate, Y = yCoordinate),
            ., by = "Plot_Name") %>% 
  #left_join(., prepTaxa() %>% select(ScientificName, CommonName), by = "ScientificName") %>% 
  select(Plot_Name, X, Y, ScientificName, quad_cov) %>% 
  mutate(quad_cov = replace_na(quad_cov, 0),
         sppcode = 
           case_when(ScientificName == "Lonicera - Exotic" ~ "LONEXO",
                     is.na(word(ScientificName, 2)) ~ 
                       toupper(paste0(substr(word(ScientificName, 1), 1, 3), "SPP")),
                     TRUE ~ toupper(paste0(substr(word(ScientificName, 1), 1, 3), 
                       substr(word(ScientificName, 2), 1, 3))))) %>% 
  arrange(sppcode, Plot_Name) %>% 
  select(Plot_Name, X, Y, sppcode, quad_cov) %>% 
  pivot_wider(names_from = sppcode, values_from = quad_cov, values_fill = 0) %>% 
  select(-NONPRE)

invspp$totcov = rowSums(invspp[,4:ncol(invspp)])

write_to_shp(invspp, shp_name = 
               paste0(new_path, "shapefiles/", park, "_inv_cover_by_species.shp"))


allcov <- do.call(joinQuadSpecies, args = c(args_4yr, speciesType = 'all')) %>% 
  select(Plot_Name, cycle, ScientificName, quad_avg_cov, Exotic)

invspp <- prepTaxa() %>% select(ScientificName, InvasiveNETN)

covsum <- left_join(allcov, invspp, by = "ScientificName") %>% 
  filter(!(InvasiveNETN == FALSE & Exotic == TRUE)) %>% # native vs. invasive
  group_by(Plot_Name, InvasiveNETN) %>% 
  summarize(avgcov = sum(quad_avg_cov, na.rm = TRUE), .groups = 'drop') %>% 
  group_by(InvasiveNETN) %>% summarize(avg_cov = sum(avgcov)/29)

#---- Map 9 Tree Pests/Diseases --- NOT YET NEEDS TO BE UPDATED FOR ERMN ----
# First compile plot-level disturbances that may include priority pests/pathogens
disturb <- do.call(joinStandDisturbance, args = args_4yr) %>% 
  filter(DisturbanceLabel != "None") %>% 
  mutate(pest = case_when(grepl("beech leaf disease|BLD|Beech leaf disease", DisturbanceNote) ~ "BLD",
                          grepl("Emerald|emerald|EAB", DisturbanceNote) ~ "EAB",
                          grepl("Red pine scale|RPS|red pine scale", DisturbanceNote) ~ "RPS",
                          grepl("HWA|hemlock woolly adelgid|EHS|Hemlock woolly adelgid|wooly", 
                                DisturbanceNote) ~ "HWA",
                          grepl("BBD|beech bark disease|Beech bark disease", DisturbanceNote) ~ "BBD",
                          grepl("GM|spongy|gypsy", DisturbanceNote) ~ "GM",
                          TRUE ~ NA_character_
  )) %>% filter(!is.na(pest)) %>% 
  select(Plot_Name, pest) %>% unique()
#mutate(detect = 'plot_dist')

# Next compile pest/pathogens from Tree Conditions
treecond_4yr <- do.call(joinTreeConditions, args = c(args_4yr, status = 'live'))

pests <- c("ALB", "BBD", "BLD", "BC", "BWA", "DOG", "EAB", "EHS", "GM", "HWA", "RPS", 
           "SB", "SLF", "SOD", "SPB", "SW")


treepests <- treecond_4yr %>% select(Plot_Name, all_of(pests)) %>% 
  #group_by(Plot_Name) %>% summarize(across(all_of(pests), ~ifelse(sum(.x) > 0, 1, 0))) %>% 
  group_by(Plot_Name) %>% summarize_at(vars(all_of(pests)), ~ifelse(sum(.x) > 0, 1, 0)) %>% 
  pivot_longer(-Plot_Name, names_to = "pest", values_to = 'tree_cond') %>% 
  filter(tree_cond > 0) %>% 
  arrange(Plot_Name) %>% unique() %>% select(Plot_Name, pest)


# Compile notes from visit that could contain mentions of pests

vnotes <- do.call(joinVisitNotes, args = args_4yr) %>% 
  mutate(pest = case_when(grepl("beech leaf disease|BLD|Beech leaf disease", Notes) ~ "BLD",
                          grepl("Emerald|emerald|EAB", Notes) ~ "EAB",
                          grepl("Red pine scale|RPS|red pine scale", Notes) ~ "RPS",
                          grepl("HWA|hemlock woolly adelgid|EHS|Hemlock woolly adelgid|wooly", 
                                Notes) ~ "HWA",
                          grepl("BBD|beech bark disease|Beech bark disease", Notes) ~ "BBD",
                          grepl("GM|spongy|gypsy", Notes) ~ "GM",
                          TRUE ~ NA_character_
  )) %>% filter(!is.na(pest)) %>% 
  select(Plot_Name, pest) %>% unique()

# Combine detections to 1 shapefile
pest_detects <- rbind(treepests, disturb, vnotes) %>% 
  left_join(plotevs_4yr %>% select(Plot_Name, SampleYear, X = xCoordinate, Y = yCoordinate),
            ., by = "Plot_Name") %>% 
  select(Plot_Name, SampleYear, X, Y, everything()) %>% unique() %>%  
  mutate(pest = replace_na(pest, "None"),
         detect = ifelse(pest == "None", 0, 1))

pests_wide <- pest_detects %>% 
  pivot_wider(names_from = pest, values_from = detect, values_fill = 0) %>% 
  select(-None)

if(park == "MABI"){
pests_wide$EAB[pests_wide$Plot_Name == "MABI-013" & pests_wide$SampleYear == 2022] <- 0
# Tree note said "No EAB", but was picked up in query for positive EAB detections
}

if(park == "MABI"){
  pests_wide$EAB[pests_wide$Plot_Name == "MABI-005" & pests_wide$SampleYear == 2023] <- 0
  # Tree note said "No sign of EAB", but was picked up in query for positive EAB detections
}

if(park == "SAGA" & to == 2022){
  pests_wide$EAB[pests_wide$Plot_Name == "SAGA-017" & pests_wide$SampleYear == 2022] <- 0
  # Tree note said "No EAB", but was picked up in query for positive EAB detections
}

pests_wide$none <- rowSums(pests_wide[,5:ncol(pests_wide)])

if(park %in% c("MABI", "SAGA")){
  worms <- do.call(joinStandData, args = args_vs) %>% select(Plot_Name, cycle, Earthworms) %>% 
    mutate(Earthworms = ifelse(Earthworms == 1, 1, 0)) %>% 
    group_by(Plot_Name) %>% 
    summarize(worms = ifelse(sum(Earthworms, na.rm = T) > 0, 1, 0))
  
  pests_wide <- left_join(pests_wide, worms, by = "Plot_Name")
  
  }

write_to_shp(pests_wide, shp_name = 
               paste0(new_path, "shapefiles/", park, "_pest_detections_", cycle_latest, ".shp"))




#---- Table 2 Average Invasive cover by plot and cycle ----
inv_plots <- do.call(sumSpeciesList, args = c(args_all, speciesType = "invasive")) %>% 
  mutate(present = ifelse(ScientificName == "None present", 0, 1)) %>% 
  group_by(Plot_Name, PlotCode, cycle, PanelCode) %>% 
  summarize(inv_cov = sum(quad_avg_cov, na.rm = T),
            numspp = sum(present), .groups = 'drop')

inv_plots_wide <- inv_plots %>% pivot_wider(names_from = cycle, 
                                            values_from = c(inv_cov, numspp)) %>% 
  select(-Plot_Name) %>% mutate(PanelCode = ifelse(PanelCode %in% c(1:2), 1, 2))

write.csv(inv_plots_wide, paste0(new_path, "tables/", "Table_2_", park, 
                                 "_invasives_by_plot_cycle.csv"), row.names = FALSE)

#---- Table 3 Invasive species by number of plots cycle
inv_spp1 <- do.call(sumSpeciesList, args = c(args_all, speciesType = 'exotic')) %>% 
  mutate(present = ifelse(ScientificName == "None present", 0, 1)) %>% arrange(cycle) %>% 
  group_by(ScientificName, cycle) %>% summarize(num_plots = sum(present), .groups = 'drop') %>% 
  pivot_wider(names_from = cycle, values_from = num_plots, values_fill = 0,
              names_prefix = "cycle_")

centaurea <- do.call(sumSpeciesList, args = c(args_all, speciesType = 'exotic')) %>% 
  filter(grepl("Centaurea", ScientificName)) %>% 
  mutate(present = 1) %>% 
  group_by(Plot_Name, cycle) %>% summarize(num_plots = ifelse(sum(present) > 0, 1, 0), .groups = 'drop') %>% 
  pivot_wider(names_from = cycle, values_from = num_plots, values_fill = 0,
              names_prefix = "cycle_") 

colSums(centaurea[,-1])

lonicera <- do.call(sumSpeciesList, args = c(args_all, speciesType = 'exotic')) %>% 
  filter(grepl("Lonicera", ScientificName)) %>% 
  mutate(present = 1) %>% 
  group_by(Plot_Name, cycle) %>% summarize(num_plots = ifelse(sum(present) > 0, 1, 0), .groups = 'drop') %>% 
  pivot_wider(names_from = cycle, values_from = num_plots, values_fill = 0,
              names_prefix = "cycle_") 

colSums(lonicera[,-1])

euonymus <- do.call(sumSpeciesList, args = c(args_all, speciesType = 'exotic')) %>% 
  filter(grepl("Euonymus", ScientificName)) %>% 
  mutate(present = 1) %>% 
  group_by(Plot_Name, cycle) %>% summarize(num_plots = ifelse(sum(present) > 0, 1, 0), .groups = 'drop') %>% 
  pivot_wider(names_from = cycle, values_from = num_plots, values_fill = 0,
              names_prefix = "cycle_") 

colSums(euonymus[,-1])

ligustrum <- do.call(sumSpeciesList, args = c(args_all, speciesType = 'exotic')) %>% 
  filter(grepl("Ligustrum", ScientificName)) %>% 
  mutate(present = 1) %>% 
  group_by(Plot_Name, cycle) %>% summarize(num_plots = ifelse(sum(present) > 0, 1, 0), .groups = 'drop') %>% 
  pivot_wider(names_from = cycle, values_from = num_plots, values_fill = 0,
              names_prefix = "cycle_") 

colSums(ligustrum[,-1])

vincetoxicum <- do.call(sumSpeciesList, args = c(args_all, speciesType = 'exotic')) %>% 
  filter(grepl("Vincetoxicum", ScientificName)) %>% 
  mutate(present = 1) %>% 
  group_by(Plot_Name, cycle) %>% summarize(num_plots = ifelse(sum(present) > 0, 1, 0), .groups = 'drop') %>% 
  pivot_wider(names_from = cycle, values_from = num_plots, values_fill = 0,
              names_prefix = "cycle_") 

colSums(vincetoxicum[,-1])


inv_spp <- left_join(inv_spp1, prepTaxa() %>% select(ScientificName, CommonName),
                     by = "ScientificName") %>% select(ScientificName, CommonName, everything())

write.csv(inv_spp, paste0(new_path, "tables/", "Table_3_", park,
                          "_num_invspp_by_cycle.csv"), row.names = FALSE)

#---- Early Detections -----
taxa <- prepTaxa()
spp_all <- do.call(sumSpeciesList, args = c(args_4yr))

# Need to import ParkTaxonProtectedStatus table from local database until it's added to the taxon view
connect <- "Driver={SQL Server};server=localhost\\SQLEXPRESS;database=NETN_Forest;trusted_connection=TRUE;ReadOnly=True"

con <- RODBC::odbcDriverConnect(connection = connect, readOnlyOptimize = TRUE, rows_at_time = 1)
xref_taxon <- RODBC::sqlQuery(con, paste0("SELECT * FROM [NETN_Forest].[xrefCOMN].[ParkTaxonProtectedStatus]"))
tlu_park <- RODBC::sqlQuery(con, paste0("SELECT * FROM [NETN_Forest].[tluCOMN].[Park]"))
RODBC::odbcClose(con)

tlu_park2 <- tlu_park %>% select(ID, Unit) %>% 
  unique() %>% 
  rename(ParkID = ID) %>% 
  filter(Unit %in% park)

ised_taxon1 <- xref_taxon %>% select(ParkID, TaxonID, IsEarlyDetection) %>% 
  filter(IsEarlyDetection == 1) %>% 
  unique()

if(park == "MIMA"){
ised_taxon1 <- rbind(ised_taxon1, 
                     data.frame(ParkID = c(5, 6), # MIMA 2 units 
                                TaxonID = c(517, 986), # AMPBRE, PHAARU 
                                IsEarlyDetection = c(1, 1)))
}

ised_taxon2 <- inner_join(ised_taxon1, tlu_park2, by = "ParkID") %>% select(-ParkID) %>% unique()

ised_taxon <- left_join(ised_taxon2, taxa %>% select(TaxonID, TSN, ScientificName), 
                        by = "TaxonID")

ised_join <- left_join(spp_all, ised_taxon, by = c("TSN", "ScientificName", "ParkUnit" = "Unit")) %>% 
  filter(IsEarlyDetection == 1) %>% 
  select(-SampleYear, -cycle, -TSN, BA_cm2, -DBH_mean, -stock, -shrub_pct_freq,
         -quad_pct_freq, -IsEarlyDetection) %>% 
  arrange(Plot_Name, ScientificName) %>% 
  left_join(plotevs_4yr %>% select(Plot_Name, X = xCoordinate, Y = yCoordinate),
            ., by = "Plot_Name") %>% filter(!is.na(ScientificName)) %>% 
  select(Plot_Name, X, Y, ScientificName, quad_avg_cov)

write.csv(ised_join, paste0(new_path, "tables/", park, "_early_detection_plant_species.csv"),
          row.names = FALSE)

#---- Invasive Detections for MABI/SAGA/ACAD ---- 
taxa <- prepTaxa()

spp_inv <- do.call(sumSpeciesList, args = c(args_4yr, speciesType = 'invasive')) |> 
  select(Plot_Name, SampleYear, quad_avg_cov, ScientificName) |> 
  filter(!ScientificName %in% "None present")

spp_inv2 <- left_join(spp_inv, plotevs_4yr |> select(Plot_Name, X = xCoordinate, Y = yCoordinate),
                      by = "Plot_Name")

#---- ED Pests ----
priority_pests <- c("ALB", "BLD", "EAB", "EHS", "HWA", "RPS", "SLF", "SOD", "SPB", "SW")

pest_eds <- pests_wide %>% select(Plot_Name, SampleYear, X, Y, any_of(priority_pests)) 

if(nrow(pest_eds) > 0){
pest_eds$num_pres <- rowSums(pest_eds[5:ncol(pest_eds)])
pest_eds <- pest_eds |> filter(num_pres >= 1)
write.csv(pest_eds, paste0(new_path, 'tables/', park, "_pest_detections.csv"), row.names = F)
}

#---- Rubus cover for MABI -----
if(park == "MABI"){
rubus <- do.call(joinQuadSpecies, args = args_vs) %>% filter(grepl("Rubus", ScientificName))

rubus2 <- rubus %>% group_by(Plot_Name, cycle) %>% 
  summarize(avg_cov = sum(quad_avg_cov, na.rm = T), .groups = 'drop') %>% 
  left_join(plotevs %>% select(Plot_Name, cycle, X = xCoordinate, Y = yCoordinate), 
            ., by = c("Plot_Name", "cycle")) 

rubus2$avg_cov[is.na(rubus2$avg_cov)] <- 0

rubus_wide <- rubus2 %>% pivot_wider(names_from = cycle, values_from = avg_cov, values_fill = 0)

write_to_shp(rubus_wide, shp_name = 
               paste0(new_path, "shapefiles/", park, "_Rubus_cover_", ".shp"))

}

#---- CWD by cycle
if(park %in% c("MABI", "SAGA", "MIMA")){
cwd1 <- do.call(joinCWDData, args = args_vs) %>% select(Plot_Name, cycle, CWD_Vol)

cwd <- cwd1 %>% group_by(Plot_Name, cycle) %>% 
  summarize(cwd_vol = sum(CWD_Vol, na.rm = T), .groups = 'drop') %>% 
  left_join(plotevs %>% select(Plot_Name, cycle, X = xCoordinate, Y = yCoordinate), 
            ., by = c("Plot_Name", "cycle"))
  
cwd$cwd_vol[is.na(cwd$cwd_vol)] <- 0

cwd_wide <- cwd %>% pivot_wider(names_from = cycle, 
                                values_from = cwd_vol, 
                                names_prefix = "cycle_",
                                values_fill = 0) 


apply(cwd_wide[,4:ncol(cwd_wide)], 2, mean)

max_cwd <- max(cwd_wide[,c(4:ncol(cwd_wide))])

# Need to fudge b/c splitting across cycles for ArcMap charts to be
# on the same scale
if(park == 'MABI' & to == 2023){
cwd_wide <- 
rbind(cwd_wide,
      data.frame(Plot_Name = "MABI-XXX", X = 699991, Y = 4834089,
                 cycle_1 = max_cwd,  cycle_2 = max_cwd, 
                 cycle_3 = max_cwd,  cycle_4 = max_cwd,
                 cycle_5 = max_cwd))
}

write_to_shp(cwd_wide, shp_name = 
               paste0(new_path, "shapefiles/", park, "_CWD_vol_by_cycle_", ".shp"))

}

#---- MABI Only: plot harvest history -----
if(park == "MABI"){
cut_trees <- do.call(joinTreeData, args_all) |> filter(TreeStatusCode == "DC")
cut_df <- as.data.frame(table(cut_trees$Plot_Name, cut_trees$SampleYear))
cut_wide <- cut_df |> pivot_wider(names_from = Var2, values_from = Freq)
write.csv(cut_wide, paste0(new_path, "tables/", "Table_5_", park, "_harvesting_history.csv"), row.names = F)
}

if(park == "MABI"){
  ferns <- sumSpeciesList(park = "MABI", from = from_4yr, to = to) |> 
    filter(ScientificName %in% c("Dennstaedtia punctilobula", "Thelypteris noveboracensis")) |> 
    select(Plot_Name, ScientificName, quad_avg_cov)

  ferns_sum <- ferns |> group_by(Plot_Name) |> summarize(avg_cov = sum(quad_avg_cov)) |> 
    filter(avg_cov >= 15)
  ferns_sum
  
}
