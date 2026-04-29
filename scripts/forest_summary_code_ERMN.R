#-------------------------------------
# ERMN Forest Data Summaries
#-------------------------------------

#---- Functions ----
# Write dataframe to shapefile using common settings
write_to_shp <- function(data, x = "X", y = "Y", shp_name){
  st_write(st_as_sf(data, coords = c(x, y), crs = park_crs),
           shp_name, delete_layer = FALSE)#FALSE)
}

# Imports/Libraries
devtools::install_github("sjperles/ERMNforest")
library(ERMNforest)
# library(RODBC)
devtools::install_github("KateMMiller/forestTrends")
library(forestTrends)
library(tidyverse)
library(sf)

rm(list = ls())

#---- Eventually delete this whole section once source_script_ERMN is correct ----
setwd('C:/ERMN Veg/Veg_Data_2022/ERMNforest_summaries')
tablepath = 'C:/ERMN Veg/Veg_Data_2022/ERMNforest_summaries/draft_tables/DEWA/'
shppath = 'C:/ERMN Veg/Veg_Data_2022/ERMNforest_summaries/shapefiles/DEWA/'
figpath = 'C:/ERMN Veg/Veg_Data_2022/ERMNforest_summaries/figures/DEWA/'

importData(type='DSN', odbc="ERMNVeg20250430")


# Set parameters
park = 'JOFL'
veg = 'all'
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


#---- Plot event lists ----
plotevs <- joinLocEvent(park = park, veg = veg, years = years, QAQC = QAQC, retired = retired, anrevisit = anrevisit)
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

reg_size_4yr$seedht0.dens[is.na(reg_size_4yr$seedht0.dens)] <- 0
reg_size_4yr$seedht1.dens[is.na(reg_size_4yr$seedht1.dens)] <- 0
reg_size_4yr$seedht2.dens[is.na(reg_size_4yr$seedht2.dens)] <- 0
reg_size_4yr$seedht3.dens[is.na(reg_size_4yr$seedht3.dens)] <- 0
reg_size_4yr$seedht4.dens[is.na(reg_size_4yr$seedht4.dens)] <- 0
reg_size_4yr$sap.dens[is.na(reg_size_4yr$sap.dens)] <- 0
reg_size_4yr$regen.dens[is.na(reg_size_4yr$regen.dens)] <- 0

write_to_shp(reg_size_4yr, 
             shp_name = paste0(shppath, "Map_2_", park, 
                               "_regen_by_size_class_cycle_", cycle_latest, ".shp"))

#####---- Figure 1 Regen trends by size class #####
reg_sz_cols1 <- c("seedht0.dens", "seedht1.dens", "seedht2.dens", "seedht3.dens", "seedht4.dens", "sap.dens")

reg_size_cy <- reg %>% group_by(Plot_Name, Cycle) %>% 
                          summarize_at(vars(all_of(reg_sz_cols1)), sum, na.rm = TRUE) %>% 
                          left_join(plotevs %>% select(Plot_Name, Cycle),
                                    ., by = c("Plot_Name", "Cycle")) %>%
                          mutate (cycle = as.numeric(Cycle)) %>% select (-c(Cycle)) %>%
                          arrange(Plot_Name, cycle)

reg_size_cy[reg_sz_cols1][reg_size_cy[is.na(reg_sz_cols1)]] <- 0


# Use forestTrends to generate loess-smoothed CIs
reg_smooth <- map_dfr(reg_sz_cols1, 
                      ~case_boot_loess(reg_size_cy, x = 'cycle', y = .x, ID = 'Plot_Name', 
                                     span = 8/4, num_reps = 250, chatty = TRUE) %>% 
                      mutate(size_class = .x)
)

# For plot labels
reg_colors <- c("seedht0.dens" = "#c6dbef",
                "seedht1.dens" = "#9ecae1", 
                "seedht2.dens" = "#6baed6", 
                "seedht3.dens" = "#3182bd", 
                "seedht4.dens" = "#08519c",
                "sap.dens" = "#05e646")

reg_smooth$size_class <- factor(reg_smooth$size_class, 
                                levels = c("seedht0.dens","seedht1.dens", "seedht2.dens",
                                           "seedht3.dens", "seedht4.dens", "sap.dens"))

cycle_labs = c("1" = "Cycle 1: 2007 \u2013 2010",
               "2" = "Cycle 2: 2011 \u2013 2014", 
               "3" = "Cycle 3: 2015 \u2013 2018", 
               "4" = "Cycle 4: 2019 \u2013 2024")

reg_labels <- c("5 \u2013 15 cm", "15 \u2013 30 cm", "30 \u2013 100 cm", "100 \u2013 150 cm",
                ">150 cm & < 1 cm DBH", "Saplings: 1 \u2013 9.9 cm DBH")

reg_trend_plot <- 
  ggplot(reg_smooth, aes(x = size_class, y = estimate, color = size_class,#linetype = sign, 
                         group = size_class)) + theme_FHM()+
  geom_bar(stat = 'identity', aes(fill = size_class, color = 'reg_colors'))+
  geom_errorbar(aes(ymin = lower95, ymax = upper95), width = 0.2, linewidth = 0.5, 
                color = 'DimGrey', alpha = 0.8) +
  labs(x  = "Cycle", y = bquote(Stems/ha))+
  facet_wrap(~cycle, labeller = as_labeller(cycle_labs), ncol = 6) +
  scale_color_manual(values = reg_colors, name = "Size Class",
                     labels = reg_labels)+
  scale_fill_manual(values = reg_colors, name = "Size Class",
                    labels = reg_labels)+
  scale_x_discrete(name = "Size Class",
                   labels = reg_labels)+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
        legend.position = 'none')

reg_trend_plot
  
ggsave(paste0(figpath, "Figure_1_", park, "_regen_by_size_class_by_cycle.jpeg"),
       height = 5, width = 7.5, units = 'in')


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
write.csv(invspp_all_unique, "allinvsp.csv", row.names = FALSE)

# Group some species by genus
Reynoutria = c("Reynoutria japonica", "Reynoutria", "Reynoutria sachalinensis")
Persicaria = c("Persicaria longiseta", "Persicaria maculosa", "Persicaria perfoliata")
Ligustrum = c("Ligustrum", "Ligustrum obtusifolium var. obtusifolium", "Ligustrum vulgare", "Ligustrum ovalifolium")
Lonicera = c("Lonicera Exotic", "Lonicera morrowii", "Lonicera", "Lonicera maackii")
Vincetoxicum = c("Vincetoxicum", "Vincetoxicum hirundinaria", "Vincetoxicum nigrum",
                 "Vincetoxicum rossicum")
Elaeagnus = c("Elaeagnus", "Elaeagnus angustifolia", "Elaeagnus umbellata")
Euonymus = c("Euonymus", "Euonymus alatus", "Euonymus atropurpureus")
Centaurea = c("Centaurea", "Centaurea stoebe", "Centaurea stoebe ssp. micranthos")

# Invasive plant data for PARK over last cycle
invspp_park4yr <- joinQuadSpData(speciesType = 'invasive', GrowthForm = 'all', 
                                park = park, years=c(from_4yr:to), QAQC = QAQC, retired = retired, anrevisit = anrevisit)

invspp1 <- invspp_park4yr %>%
  mutate(ave.q.cov = replace_na(ave.q.cov, 0),
         present = ifelse(ave.q.cov > 0, 1, 0),
         Latin_name = replace_na(Latin_name, "None present"),
         Latin_name = case_when(
           Latin_name %in% Reynoutria ~ "Reynoutria",
           Latin_name %in% Persicaria ~ "Persicaria - Exotic",
           Latin_name %in% Ligustrum ~ "Ligustrum",
           Latin_name %in% Lonicera ~ "Lonicera - Exotic",
           Latin_name %in% Vincetoxicum ~ "Vincetoxicum",
           Latin_name %in% Elaeagnus ~ "Elaeagnus",
           Latin_name %in% Euonymus ~"Euonymus",
           Latin_name %in% Centaurea ~"Centaurea",
           TRUE ~ Latin_name)) %>% 
  arrange(Plot_Name, Latin_name)

# Determine 12 most common invasives in a park by cover
plotspp_df <- data.frame(expand.grid(unique(invspp1$Plot_Name), 
                                     unique(invspp1$Latin_name), 
                                     stringsAsFactors = FALSE)) %>% 
  rename(Plot_Name = Var1, Latin_name = Var2) %>% 
  filter(Latin_name != "None present")

topspp <- invspp1 %>% left_join(plotspp_df, invspp1, by = c('Plot_Name', 'Latin_name')) %>% 
  arrange(Plot_Name, Latin_name) %>% 
  mutate(ave.q.cov = replace_na(ave.q.cov, 0),
         present = replace_na(present, 0)) %>% 
  group_by(Latin_name) %>% 
  summarize(sum_cov = sum(ave.q.cov, na.rm = T),
            num_plots = sum(present), 
            avg_cov = sum_cov/num_plots,
            .groups = 'drop') %>% 
  arrange(desc(avg_cov)) %>% slice(1:12) %>% select(Latin_name)

# Prep for shapefile
invspp <- invspp1 %>% 
  mutate(Latin_name = case_when(
    Latin_name == "None present" ~ "None present",
    Latin_name %in% topspp$Latin_name ~ Latin_name,
    TRUE ~ "Other invasive")) %>% 
  group_by(Plot_Name, X=X_Coord, Y=Y_Coord, Latin_name) %>% 
  summarize(quad_cov = sum(ave.q.cov), .groups = 'drop') %>% # lumps the rest of spp covers
  pivot_wider(names_from = Latin_name, values_from = quad_cov, values_fill = 0) %>%
  select(-c("None present"))

invspp$totcov = rowSums(invspp[,4:ncol(invspp)])

write_to_shp(invspp, shp_name = paste0(shppath, "Map_9_", park, "_invspp_cover.shp"))


#####---- Map 10 CWD by cycle ####
cwd_data1 <- joinCWDData(park = park, years = years, QAQC = QAQC, retired = retired,
                         anrevisit = anrevisit, units = 'ha')

cwd2 <- cwd_data1 %>% group_by(Plot_Name, Cycle) %>% 
    summarize(cwd_vol = sum(CWD_Vol, na.rm = T), .groups = 'drop')  %>% 
    left_join(plotevs %>% select(Plot_Name, Cycle, X = X_Coord, Y = Y_Coord), 
              ., by = c("Plot_Name", "Cycle")) %>%
  arrange(Plot_Name, Cycle)
  
  cwd2$cwd_vol[is.na(cwd2$cwd_vol)] <- 0
  
  cwd_wide <- cwd2 %>% pivot_wider(names_from = Cycle, 
                                  values_from = cwd_vol, 
                                  names_prefix = "Cycle_",
                                  values_fill = 0) 
  
  write_to_shp(cwd_wide, shp_name = 
                 paste0(shppath, "Map_10_", park, "_CWD_vol_by_cycle_", ".shp"))
  
  
extraNERI <- anti_join(reg_cycle, reg_size_4yr)


#---- Table 2 Average Invasive cover by plot and cycle, NOT UPDATED FOR ERMN YET ----
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

#---- Table 3 Invasive species by number of plots cycle, NOT UPDATED FOR ERMN YET -----
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

