#-------------------------------------------------------------------
# Smoothed tree, sapling, seedling changes in abundance over time: ERMN
# ++++++++ MUST RUN source_script_ERMN.R FIRST ++++++++
#-------------------------------------------------------------------
library(ERMNforest)
library(forestTrends)
library(tidyverse)
library(sf)
library(ggpubr)
span <- 4/5 #roughly linear between timesteps
#span = 4/5

rm()

#---- Eventually delete this whole section once source_script_ERMN is correct ----
setwd('C:/ERMN Veg/Veg_Data_2022/ERMNforest_summaries')
tablepath = 'C:/ERMN Veg/Veg_Data_2022/ERMNforest_summaries/draft_tables/JOFL/'
shppath = 'C:/ERMN Veg/Veg_Data_2022/ERMNforest_summaries/shapefiles/JOFL/'
figpath = 'C:/ERMN Veg/Veg_Data_2022/ERMNforest_summaries/figures/JOFL/'

importData(type='DSN', odbc="ERMNVeg20250430")


# Set parameters
park = 'JOFL'
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


args_all = list(park = park, from = from, to = to, QAQC = QAQC, retired = retired, anrevisit = TRUE)
args_4yr = list(park = park, from = from_4yr, to = to, QAQC = QAQC, retired = retired, anrevisit = anrevisit)
args_vs = list(park = park, from = from, to = to, QAQC = QAQC, retired = retired, anrevisit = anrevisit)

trspp_grps <- read.csv("ERMN_tree_species_groups.csv")

plotevs <- joinLocEvent(park = park, years = years, QAQC = QAQC, retired = retired, anrevisit = anrevisit)
plotevs_4yr <- plotevs %>% filter(between(Year, from_4yr, to))


# if(!exists('trspp_grps')){stop("Must run source_script_NETN.R before this script will work.")}
# head(trspp_grps)


##### --- COLOR and LINE WEIGHTS BY SPECIES --- #####
# ADJUST LINE WEIGHT FOR EACH SPECIES AND PARK AND METRIC
cols = c(
  "Acer rubrum (red maple)" = "#38A800",
  "Acer negundo (box elder)" = "#cccc00",
  "Acer platanoides (Norway maple)" = "#8b0000",
  "Acer spp. (maple)" = "#00FF00",
  "Acer saccharum (sugar maple)" = "#009999",
  "Acer pensylvanicum (striped mapled)" = "#FFFF00",
  "Amelanchier spp. (serviceberry)" = "#9371B9",
  "Ailanthus altissima (tree-of-heaven)" = "#cd4a8f",
  "Asimina triloba (pawpaw)" = "#FF00C5",
  "Aesculus flava (buckeye)" = "#efdf00",
  "Betula lenta (black birch)" = "#ffd8b1", # darkened color for 2024 NETN figs; does not match maps or MIDN
  "Betula spp. (birch)" = "#ffd8b1", 
  "Carya spp. (hickory)" = "#911eb4",
  "Crataegus spp. (hawthorn)" = "#42d4f4",
  "Fagus grandifolia (beech)" = "#FFAA00",
  "Fraxinus spp. (ash)" = "#A87000",
  "Fraxinus americana (white ash)" = "#A87000",
  "Ilex opaca (American holly)" = "#42d4f4",
  "Juniperus virginiana (eastern redcedar)" = "#9371B9",
  "Liquidambar styraciflua (sweetgum)" = "#FFFF00",
  "Liriodendron tulipifera (tuliptree)" = "#4363d8",
  "Magnolia spp. (magnolia)" = "#620436",
  "Nyssa sylvatica (black gum)" = "#000075",
  "Other exotic spp." = "#ca0020",
  "Other native canopy spp." = "#d9d9d9",
  "Pinus spp. (pine)" = "#5A462B",
  "Pinus strobus (eastern white pine)" = "#5A1111",
  "Pinus virginiana (Virginia pine)" = "#E5740D",
  "Pinus resinosa (red pine)" = "#E5740D",
  "Platanus occidentalis (sycamore)" = "#E5740D",
  "Prunus serotina (black cherry)" ="#00E6A9", 
  "Prunus spp. (cherry)" ="#0d00ba", 
  "Pyrus calleryana (Bradford pear)" = "#cd4a8f",
  "Quercus spp. (oak)" = "#0E5D2C",
  "Quercus alba (white oak)" = "#0E5D2C",
  "Quercus rubra (northern red oak)" = "#0E5D2C",
  "Quercus palustris (pin oak)" = "#072b16",
  "Robinia pseudoacacia (black locust)" = "#cccc00",
  "Other native subcanopy spp." = "#ffa8b4",
  "Tilia americana (basswood)" = "#fb693b",
  "Tsuga canadensis (hemlock)" = "#9bd2ef",
  "Ulmus spp. (elm)" = "#808000", 
  "Unknown spp." = "#CACACA",
  "Diospyros virginiana (persimmon)" = "#006666", #for ASIS only
  "Amelanchier spp. (serviceberry)" = "#ffd8b1", #for ASIS only
  "Sassafras albidum (sassafrass)" = "#59538A", #ASIS only
  "Other conifer" = "#42d4f4",#ACAD only
  "Picea spp. (spruce)" = "#000075",#ACAD only
  "Populus spp. (aspen)" = "#FFFF00")#ACAD only


lines = c(
  "Acer rubrum (red maple)" = "solid",
  "Acer negundo (box elder)" = "solid",
  "Acer platanoides (Norway maple)" = "solid",
  "Acer spp. (maple)" = "solid",
  "Acer saccharum (sugar maple)" = "solid",
  "Acer pensylvanicum (striped mapled)" = "solid",
  "Amelanchier spp. (serviceberry)" = "solid",
  "Ailanthus altissima (tree-of-heaven)" = "solid",
  "Asimina triloba (pawpaw)" = "solid",
  "Aesculus flava (buckeye)" = "solid",
  "Betula lenta (black birch)" = "solid", # darkened color for 2024 NETN figs; does not match maps or MIDN
  "Betula spp. (birch)" = "solid", 
  "Carya spp. (hickory)" = "solid",
  "Crataegus spp. (hawthorn)" = "solid",
  "Fagus grandifolia (beech)" = "solid",
  "Fraxinus spp. (ash)" = "solid",
  "Fraxinus americana (white ash)" = "solid",
  "Ilex opaca (American holly)" = "solid",
  "Juniperus virginiana (eastern redcedar)" = "solid",
  "Liquidambar styraciflua (sweetgum)" = "solid",
  "Liriodendron tulipifera (tuliptree)" = "solid",
  "Magnolia spp. (magnolia)" = "solid",
  "Nyssa sylvatica (black gum)" = "solid",
  "Other exotic spp." = "solid",
  "Other native canopy spp." = "solid",
  "Pinus spp. (pine)" = "solid",
  "Pinus strobus (eastern white pine)" = "solid",
  "Pinus taeda (loblolly pine)" = "solid",
  "Pinus virginiana (Virginia pine)" = "solid",
  "Pinus resinosa (red pine)" = "solid",
  "Platanus occidentalis (sycamore)" = "solid",
  "Prunus spp. (cherry)" ="solid", 
  "Prunus serotina (black cherry)" ="solid", 
  "Pyrus calleryana (Bradford pear)" = "solid",
  "Quercus spp. (oak)" = "solid",
  "Quercus alba (white oak)" = "dashed",
  "Quercus rubra (northern red oak)" = "twodash",
  "Quercus palustris (pin oak)" = "solid",
  "Robinia pseudoacacia (black locust)" = "solid",
  "Other native subcanopy spp." = "solid",
  "Tilia americana (basswood)" = "solid",
  "Tsuga canadensis (hemlock)" = "solid",
  "Ulmus spp. (elm)" = "solid", 
  "Unknown spp." = "solid",
  "Diospyros virginiana (persimmon)" = "solid", #for ASIS only
  "Amelanchier spp. (serviceberry)" = "solid", #for ASIS only
  "Sassafras albidum (sassafrass)" = "solid", #ASIS only
  "Abies balsamea (balsam fir)" = 'solid',#ACAD only
  "Other conifer" = "solid",#ACAD only
  "Picea spp. (spruce)" = "solid",#ACAD only
  "Populus spp. (aspen)" = "solid")#ACAD only



######---- Tree trends by species ---- ######
trees1 <- joinTreeData(status = 'live', speciesType = 'all', canopyForm = 'all',
                     park=park, years=c(from:to), QAQC = QAQC, retired = retired, anrevisit = anrevisit)

trees2 <- trees1 %>% mutate(BA_m2ha = BA_cm2/707,
                           stems = pres) %>%
         select(Plot_Name, Year, Cycle, Latin_name, stems, BA_m2ha) %>%
  arrange(Plot_Name, Year)  
  
tree_grps <- left_join(trees2, trspp_grps |> select(Latin_name, ERMNGroup, LegendName), 
                       by = "Latin_name")
if(nrow(tree_grps[which(is.na(tree_grps$ERMNGroup)),]) > 0){
  warning("There's at least 1 NA in tree_grps$spp_group, meaning at least one species is missing a group.")} #check if any spp. is missing a group

table(tree_grps$ERMNGroup)
table(tree_grps$Latin_name, tree_grps$ERMNGroup)

#### NEEDS UPDATE FOR ERMN Park by Park Park specific changes to tree species groups###
if(park == "FRHI"){
  tree_grps <- tree_grps %>% 
    mutate(ERMNGroup = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum",
                                 Latin_name == "Acer negundo" ~ "Acer negundo",
                                 Latin_name == "Acer saccharum" ~ "Acer saccharum",
                                 Latin_name == "Acer saccharinum" ~ "Other Native",
                                 Latin_name == "Betula lenta" ~ "Other Native",
                                 Latin_name == "Prunus serotina" ~ "Prunus serotina",
                                 Latin_name == "Platanus occidentalis" ~ "Platanus occidentalis",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(LegendName = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum (red maple)",
                                  Latin_name == "Acer negundo" ~ "Acer negundo (box elder)",
                                  Latin_name == "Acer saccharum" ~ "Acer saccharum (sugar maple)",
                                  Latin_name == "Acer saccharinum" ~ "Other native canopy spp.",
                                  Latin_name == "Betula lenta" ~ "Other native canopy spp.",
                                  Latin_name == "Prunus serotina" ~ "Prunus serotina (black cherry)",
                                  Latin_name == "Platanus occidentalis" ~ "Platanus occidentalis (sycamore)",
                                  TRUE ~ LegendName))
}

if(park == "FONE"){
  tree_grps <- tree_grps %>% 
    mutate(ERMNGroup = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum",
                                 Latin_name == "Acer saccharum" ~ "Other Native",
                                 Latin_name == "Betula lenta" ~ "Betula lenta",
                                 Latin_name == "Fraxinus americana" ~ "Fraxinus americana",
                                 Latin_name == "Fagus grandifolia" ~ "Other Native",
                                 Latin_name == "Magnolia acuminata" ~ "Other Native",
                                 Latin_name == "Prunus serotina" ~ "Prunus serotina",
                                 Latin_name == "Quercus alba" ~ "Quercus alba",
                                 Latin_name == "Quercus rubra" ~ "Quercus rubra",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(LegendName = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum (red maple)",
                                  Latin_name == "Acer saccharum" ~ "Other native canopy spp.",
                                  Latin_name == "Betula lenta" ~ "Betula lenta (black birch)",
                                  Latin_name == "Fraxinus americana" ~ "Fraxinus americana (white ash)",
                                  Latin_name == "Fagus grandifolia" ~ "Other native canopy spp.",
                                  Latin_name == "Magnolia acuminata" ~ "Other native canopy spp.",
                                  Latin_name == "Prunus serotina" ~ "Prunus serotina (black cherry)",
                                  Latin_name == "Quercus alba" ~ "Quercus alba (white oak)",
                                  Latin_name == "Quercus rubra" ~ "Quercus rubra (northern red oak)",
                                  TRUE ~ LegendName))
}

if(park == "ALPO"){
  tree_grps <- tree_grps %>% 
    mutate(ERMNGroup = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum",
                                 Latin_name == "Acer saccharum" ~ "Acer saccharum",
                                 Latin_name == "Prunus serotina" ~ "Prunus serotina",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(LegendName = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum (red maple)",
                                  Latin_name == "Acer saccharum" ~ "Acer saccharum (sugar maple)",
                                  Latin_name == "Prunus serotina" ~ "Prunus serotina (black cherry)",
                                  TRUE ~ LegendName))
}

if(park == "JOFL"){
  tree_grps <- tree_grps %>% 
    mutate(ERMNGroup = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum",
                                 Latin_name == "Acer saccharum" ~ "Acer saccharum",
                                 Latin_name == "Betula lenta" ~ "Betula lenta",
                                 Latin_name == "Prunus serotina" ~ "Prunus serotina",
                                 Latin_name == "Robinia pseudoacacia" ~ "Robinia pseudoacacia",
                                 TRUE ~ ERMNGroup)) %>%
    mutate(ERMNGroup = case_when(ERMNGroup == "Carya" ~ "Other Native",
                                 ERMNGroup == "Magnolia" ~ "Other Native",
                                 ERMNGroup == "Prunus" ~ "Other Native",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(LegendName = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum (red maple)",
                                  Latin_name == "Acer saccharum" ~ "Acer saccharum (sugar maple)",
                                  Latin_name == "Betula lenta" ~ "Betula lenta (black birch)",
                                  Latin_name == "Prunus serotina" ~ "Prunus serotina (black cherry)",
                                  Latin_name == "Robinia pseudoacacia" ~ "Robinia pseudoacacia (black locust)",
                                  TRUE ~ LegendName)) %>%
    mutate(LegendName = case_when(ERMNGroup == "Other Native" ~ "Other native canopy spp.",
                                  TRUE ~ LegendName))
}

# This will create all combination of plot, year, spp, but adds years not sampled by plots.
plot_yr <- plotevs |> ungroup() |> select(Plot_Name, Year) |> unique()
length(unique(plot_yr$Plot_Name))

plot_spp_yr1 <- expand.grid(Plot_Name = unique(plot_yr$Plot_Name), 
                            Year = unique(plot_yr$Year),
                            ERMNGroup = unique(tree_grps$ERMNGroup)) |> 
  select(Plot_Name, Year, ERMNGroup)

plot_spp_yr2 <- left_join(plot_spp_yr1, 
                          tree_grps |> select(ERMNGroup) |> unique(), 
                          by = "ERMNGroup", relationship = 'many-to-many')

plot_spp_yr3 <- left_join(plot_yr, plot_spp_yr2, by = c("Plot_Name", "Year"))

dup_spp_check <- as.data.frame(table(plot_spp_yr3$ERMNGroup))

if(length(unique(dup_spp_check$Freq)) > 1)(stop("Not all tree species have the same frequency in expand grid. Check for duplicate species codes."))


# Join tree data to all possible combos of year, plot, species
plot_spp_yr <- left_join(plot_spp_yr3, tree_grps |> select(ERMNGroup, LegendName) |> unique(), 
                         by = "ERMNGroup")

tree_spp_sum1 <- left_join(plot_spp_yr, tree_grps [,c("Plot_Name", "Year", "ERMNGroup", "stems","BA_m2ha")],
                           by = c("Plot_Name", "Year", "ERMNGroup"),
                           relationship = 'many-to-many') 

tree_spp_sum1[,c("stems", "BA_m2ha")][is.na(tree_spp_sum1[,c("stems", "BA_m2ha")])] <- 0


tree_spp_sum <- tree_spp_sum1 |> group_by(Plot_Name, Year, ERMNGroup) |> 
  summarize(stems_ha = (sum(stems)/(707/10000)),
            BA_m2ha = sum(BA_m2ha), .groups = 'drop')

spp_list <- sort(unique(tree_spp_sum$ERMNGroup))
table(tree_spp_sum$ERMNGroup)

#####---- Tree Stems ----####
tree_stem_smooth <- purrr::map_dfr(spp_list, 
                                   function(spp){
                                     df <- tree_spp_sum |> filter(ERMNGroup %in% spp)
                                     case_boot_loess(df, x = "Year", y = "stems_ha", ID = "Plot_Name",
                                                     group = "ERMNGroup", 
                                                     span = span, num_reps = 10) |>
                                       mutate(ERMNGroup = spp)
                                   }
)
# Determine if significant based on whether first and last year CIs overlap
tree_stem_smooth2 <- 
  left_join(tree_stem_smooth, 
            tree_stem_smooth |> arrange(Year) |> group_by(ERMNGroup) |> 
              summarize(up_first = first(upper95), up_last = last(upper95),
                        lo_first = first(lower95), lo_last = last(lower95),
                        sign = case_when(up_first < lo_last ~ "signinc",
                                         lo_first > up_last ~ "signdec",
                                         is.na(up_first) ~ "notmod",
                                         TRUE ~ "nonsign")) |> 
              select(ERMNGroup, sign),
            by = "ERMNGroup")

# Join full group names back into dataframe
tree_stem_smooth3 <- left_join(tree_stem_smooth2,
                               plot_spp_yr |> select(ERMNGroup, LegendName) |> unique(),
                               by = c('ERMNGroup'), 
                               relationship = 'many-to-many') |> 
  mutate(ERMNGroup = as.character(ERMNGroup)) |> 
  arrange(ERMNGroup)


#--- Tree BA
tree_BA_smooth <- purrr::map_dfr(spp_list, 
                                 function(spp){
                                   df <- tree_spp_sum |> filter(ERMNGroup %in% spp)
                                   case_boot_loess(df, x = "Year", y = "BA_m2ha", ID = "Plot_Name",
                                                   group = "ERMNGroup", 
                                                   span = span, num_reps = 20) |>
                                     mutate(ERMNGroup = spp)
                                 }
)


tree_BA_smooth2 <- 
  left_join(tree_BA_smooth, 
            tree_BA_smooth |> arrange(Year) |> group_by(ERMNGroup) |> 
              summarize(up_first = first(upper95), up_last = last(upper95),
                        lo_first = first(lower95), lo_last = last(lower95),
                        sign = case_when(up_first < lo_last ~ "signinc",
                                         lo_first > up_last ~ "signdec",
                                         is.na(up_first) ~ "notmod",
                                         TRUE ~ "nonsign")) |> 
              select(ERMNGroup, sign),
            by = "ERMNGroup")

tree_BA_smooth3 <- left_join(tree_BA_smooth2,
                               plot_spp_yr |> select(ERMNGroup, LegendName) |> unique(),
                               by = c('ERMNGroup'), 
                               relationship = 'many-to-many') |> 
  mutate(ERMNGroup = as.character(ERMNGroup)) |> 
  arrange(ERMNGroup)


net_ba_year <- tree_BA_smooth3 |> group_by(term, Year) |> summarize(net_ba = sum(estimate))
net_ba_year



#---- Net stem/BA plots by species
net_stems <- 
  ggplot(tree_stem_smooth3, aes(x = Year, y = estimate)) +
  geom_line(aes(color = LegendName, linetype = LegendName), linewidth = 1.25) +
  labs(x = NULL, y = "Tree Density (stems/ha)") +
  # geom_line(data = mor_rec_tot %>% filter(unit_type == "stem"),
  #           aes(x = cycle, y = park_total), color = 'black') +
  theme_FHM()+
  # may have to update for different parks
  scale_color_manual(values = cols,  name = NULL) +
  scale_linetype_manual(values = lines,  name = NULL) +
  scale_x_continuous(breaks = c(seq(from, to, by = 2), to), 
                     limits = c(from, to)) +
  scale_y_continuous(limits = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'right', # b/c shares page with 4B 
        legend.key.width = unit(1.5, 'cm'),
        plot.margin = margin(0.1, 0.4, 1.5, 0.4, 'cm')) #+ 
#guides(color = guide_legend(nrow = spp_rows))

net_stems
ggsave(paste0(figpath, "Figure_5_", park, "_smoothed_tree_stems_by_species_cycle.jpg"),
       height = 5, width = 8, units = 'in')


net_ba <- 
  ggplot(tree_BA_smooth3, aes(x = Year, y = estimate)) +
  geom_line(aes(color = LegendName, linetype = LegendName), linewidth = 1.25) +
  labs(x = NULL, y = "Tree Basal Area (sq.m/ha)") +
  # geom_line(data = mor_rec_tot %>% filter(unit_type == "stem"),
  #           aes(x = cycle, y = park_total), color = 'black') +
  theme_FHM()+
  # may have to update for different parks
  scale_color_manual(values = cols,  name = NULL) +
  scale_linetype_manual(values = lines, name = NULL) +
  scale_x_continuous(breaks = c(seq(from, to, by = 2), to), 
                     limits = c(from, to)) +
  scale_y_continuous(limits = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'right', 
        legend.key.width = unit(1.5, 'cm'),
        plot.margin = margin(0.5, 0.4, 0.1, 0.4, 'cm')) #+ 
# guides(color = guide_legend(nrow = spp_rows))

net_ba
ggsave(paste0(figpath, "Figure_4_", park, "_smoothed_BA_by_species_cycle.jpg"),
       height = 5, width = 8, dpi=600)



ggarrange(net_stems, net_ba, common.legend = T, legend = 'right', nrow = 2, labels = c("A.", "B.")) 

ggsave(paste0(new_path, "figures/Figure_5_", park, "_smoothed_tree_dens_BA_by_species_cycle.svg"),
       height = 11, width = 9.5)

ggsave(paste0(new_path, "figures/Figure_5_", park, "_smoothed_tree_dens_BA_by_species_cycle.png"),
       height = 11, width = 9.5, dpi = 600)


###### ----- Regeneration trends for seedlings and saplings ----- ######
reg1 <- joinRegenSpData(speciesType = 'all', canopyForm = 'all', units = 'ha',
                     park=park, years=c(from:to), QAQC = QAQC, retired = retired, anrevisit = anrevisit)

reg_grps <- left_join(reg1, trspp_grps |> select(Latin_name, ERMNGroup, LegendName), 
                      by = c("Latin_name"))

othertest <- subset(reg_grps, ERMNGroup == "Understory")

table(reg_grps$Latin_name, reg_grps$ERMNGroup)
table(reg_grps$ERMNGroup)

#Park specific exceptions to default groupings
if(park == "FRHI"){
  reg_grps <- reg_grps %>% 
    mutate(ERMNGroup = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum",
                                 Latin_name == "Acer negundo" ~ "Acer negundo",
                                 Latin_name == "Acer saccharum" ~ "Acer saccharum",
                                 Latin_name == "Prunus serotina" ~ "Prunus serotina",
                                 Latin_name == "Quercus palustris" ~ "Quercus",
                                 Latin_name == "Asimina triloba" ~ "Asimina triloba",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(ERMNGroup = case_when(ERMNGroup == "Acer" ~ "Other Native",
                                 ERMNGroup == "Betula" ~ "Other Native",
                                 ERMNGroup == "Carya" ~ "Other Native",
                                 ERMNGroup == "Pinus" ~ "Other Native",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(LegendName = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum (red maple)",
                                  Latin_name == "Acer negundo" ~ "Acer negundo (box elder)",
                                  Latin_name == "Acer saccharum" ~ "Acer saccharum (sugar maple)",
                                  Latin_name == "Prunus serotina" ~ "Prunus serotina (black cherry)",
                                  Latin_name == "Quercus palustris" ~ "Quercus spp. (oak)",
                                  Latin_name == "Asimina triloba" ~ "Asimina triloba (pawpaw)",
                                  TRUE ~ LegendName)) %>%
    mutate(LegendName = case_when(ERMNGroup == "Other Native" ~ "Other native canopy spp.",
                                 TRUE ~ LegendName))
} 

if(park == "FONE"){
  reg_grps <- reg_grps %>% 
    mutate(ERMNGroup = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum",
                                 Latin_name == "Amelanchier arborea" ~ "Amelanchier",
                                 Latin_name == "Amelanchier" ~ "Amelanchier",
                                 Latin_name == "Crataegus" ~ "Crataegus",
                                 Latin_name == "Crataegus A" ~ "Crataegus",
                                 Latin_name == "Crataegus B" ~ "Crataegus",
                                 Latin_name == "Prunus serotina" ~ "Prunus serotina",
                                 Latin_name == "Tsuga canadensis" ~ "Other Native",
                                 Latin_name == "Ulmus" ~ "Other Native",
                                 Latin_name == "Ulmus americana" ~ "Other Native",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(ERMNGroup = case_when(ERMNGroup == "Prunus" ~ "Other Native",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(LegendName = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum (red maple)",
                                  Latin_name == "Amelanchier" ~ "Amelanchier spp. (serviceberry)",
                                  Latin_name == "Amelanchier arborea" ~ "Amelanchier spp. (serviceberry)",
                                  Latin_name == "Crataegus" ~ "Crataegus spp. (hawthorn)",
                                  Latin_name == "Crataegus A" ~ "Crataegus spp. (hawthorn)",
                                  Latin_name == "Crataegus B" ~ "Crataegus spp. (hawthorn)",
                                  Latin_name == "Prunus serotina" ~ "Prunus serotina (black cherry)",
                                  Latin_name == "Tsuga canadensis" ~ "Other native canopy spp.",
                                  Latin_name == "Ulmus" ~ "Other native canopy spp.",
                                  Latin_name == "Ulmus americana" ~ "Other native canopy spp.",
                                  TRUE ~ LegendName)) %>%
    mutate(LegendName = case_when(ERMNGroup == "Other Native" ~ "Other native canopy spp.",
                                  TRUE ~ LegendName))
} 


if(park == "ALPO"){
  reg_grps <- reg_grps %>% 
    mutate(ERMNGroup = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum",
                                 Latin_name == "Acer saccharum" ~ "Acer saccharum",
                                 Latin_name == "Acer pensylvanicum" ~ "Acer pensylvanicum",
                                 Latin_name == "Prunus serotina" ~ "Prunus serotina",
                                 Latin_name == "Tsuga canadensis" ~ "Other Native",
                                 Latin_name == "Tilia americana" ~ "Other Native",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(ERMNGroup = case_when(ERMNGroup == "Magnolia" ~ "Other Native",
                                 ERMNGroup == "Prunus" ~ "Other Native",
                                 ERMNGroup == "Ulmus" ~ "Other Native",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(LegendName = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum (red maple)",
                                  Latin_name == "Acer saccharum" ~ "Acer saccharum (sugar maple)",
                                  Latin_name == "Acer pensylvanicum" ~ "Acer pensylvanicum (striped mapled)",
                                  Latin_name == "Prunus serotina" ~ "Prunus serotina (black cherry)",
                                  Latin_name == "Tsuga canadensis" ~ "Other native canopy spp.",
                                  Latin_name == "Tilia americana" ~ "Other native canopy spp.",
                                  TRUE ~ LegendName)) %>%
    mutate(LegendName = case_when(ERMNGroup == "Other Native" ~ "Other native canopy spp.",
                                  TRUE ~ LegendName))
}

if(park == "JOFL"){
  reg_grps <- reg_grps %>% 
    mutate(ERMNGroup = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum",
                                 Latin_name == "Acer saccharum" ~ "Acer saccharum",
                                 Latin_name == "Acer pensylvanicum" ~ "Acer pensylvanicum",
                                 Latin_name == "Amelanchier arborea" ~ "Amelanchier",
                                 Latin_name == "Amelanchier" ~ "Amelanchier",
                                 Latin_name == "Crataegus" ~ "Crataegus",
                                 Latin_name == "Crataegus A" ~ "Crataegus",
                                 Latin_name == "Crataegus B" ~ "Crataegus",
                                 Latin_name == "Liriodendron tulipifera" ~ "Other Native",
                                 Latin_name == "Prunus serotina" ~ "Prunus serotina",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(ERMNGroup = case_when(ERMNGroup == "Magnolia" ~ "Other Native",
                                 ERMNGroup == "Pinus" ~ "Other Native",
                                 ERMNGroup == "Prunus" ~ "Other Native",
                                 ERMNGroup == "Ulmus" ~ "Other Native",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(LegendName = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum (red maple)",
                                  Latin_name == "Acer saccharum" ~ "Acer saccharum (sugar maple)",
                                  Latin_name == "Acer pensylvanicum" ~ "Acer pensylvanicum (striped mapled)",
                                  Latin_name == "Amelanchier" ~ "Amelanchier spp. (serviceberry)",
                                  Latin_name == "Amelanchier arborea" ~ "Amelanchier spp. (serviceberry)",
                                  Latin_name == "Crataegus" ~ "Crataegus spp. (hawthorn)",
                                  Latin_name == "Crataegus A" ~ "Crataegus spp. (hawthorn)",
                                  Latin_name == "Crataegus B" ~ "Crataegus spp. (hawthorn)",
                                  Latin_name == "Liriodendron tulipifera" ~ "Other native canopy spp.",
                                  Latin_name == "Prunus serotina" ~ "Prunus serotina (black cherry)",
                                  TRUE ~ LegendName)) %>%
    mutate(LegendName = case_when(ERMNGroup == "Other Native" ~ "Other native canopy spp.",
                                  TRUE ~ LegendName))
}


table(reg_grps$Latin_name, reg_grps$ERMNGroup)
table(reg_grps$ERMNGroup)

if(nrow(reg_grps[which(is.na(reg_grps$spp_grp)),]) > 0){
  warning("There's at least 1 NA in reg_grps$spp_group, meaning at least one species is missing a group.")} #check if any spp. is missing a group



# This will create all combination of plot, year, spp, but adds years not sampled by plots.
plot_yr <- plotevs |> ungroup() |> select(Plot_Name, Year) |> unique()

length(unique(plot_yr$Plot_Name))

plot_regspp_yr1 <- expand.grid(Plot_Name = unique(reg_grps$Plot_Name), 
                            Year = unique(reg_grps$Year),
                            ERMNGroup = unique(reg_grps$ERMNGroup)) |> 
  select(Plot_Name, Year, ERMNGroup)

length(unique(plot_regspp_yr1$ERMNGroup))

plot_regspp_yr2 <- left_join(plot_yr, plot_regspp_yr1, by = c("Plot_Name", "Year"))

dup_regspp_check <- as.data.frame(table(plot_regspp_yr2$ERMNGroup))

if(length(unique(dup_regspp_check$Freq)) > 1)(stop("Not all tree species have the same frequency in expand grid. Check for duplicate species codes."))


# Join tree data to all possible combos of year, plot, species
plot_regspp_yr <- left_join(plot_regspp_yr2, reg_grps |> select(ERMNGroup, LegendName) |> unique(), 
                            by = "ERMNGroup")

reg_spp_smooth1 <- left_join(plot_regspp_yr, reg_grps [,c("Plot_Name", "Year", "ERMNGroup", "LegendName", "tot.seed.dens","sap.dens")],
                           by = c("Plot_Name", "Year", "ERMNGroup", "LegendName")) 

reg_spp_smooth1[,c("tot.seed.dens", "sap.dens")][is.na(reg_spp_smooth1[,c("tot.seed.dens", "sap.dens")])] <- 0

reg_spp_sum <- reg_spp_smooth1 |> group_by(Plot_Name, Year, ERMNGroup, LegendName) |> 
  summarize(seed_ha = sum(tot.seed.dens),
            sap_ha = sum(sap.dens), .groups = 'drop')

rspp_list <- sort(unique(reg_spp_sum$ERMNGroup))
rspp_list
length(rspp_list)

table(reg_spp_sum$Year, reg_spp_sum$Plot_Name)
length(unique(reg_spp_sum$Plot_Name))


seed_smooth <- purrr::map_dfr(rspp_list, 
                              function(spp){
                              df <- reg_spp_sum |> filter(ERMNGroup %in% spp)
                              case_boot_loess(df, x = "Year", y = "seed_ha", ID = "Plot_Name",
                                              group = "ERMNGroup", 
                                              span = span, num_reps = 20) |>
                               mutate(ERMNGroup = spp)
                              }
                              )

sap_smooth <- purrr::map_dfr(rspp_list, 
                              function(spp){
                                df <- reg_spp_sum |> filter(ERMNGroup %in% spp)
                                case_boot_loess(df, x = "Year", y = "sap_ha", ID = "Plot_Name",
                                                group = "ERMNGroup", 
                                                span = span, num_reps = 20) |>
                                  mutate(ERMNGroup = spp)
                              }
)

# Determine if significant based on whether first and last year CIs overlap
seed_smooth2 <- 
  left_join(seed_smooth, 
            seed_smooth |> arrange(Year) |> group_by(ERMNGroup) |> 
                           summarize(up_first = first(upper95), up_last = last(upper95),
                                     lo_first = first(lower95), lo_last = last(lower95),
                                     sign = case_when(up_first < lo_last ~ "signinc",
                                                      lo_first > up_last ~ "signdec",
                                                      is.na(up_first) ~ "notmod",
                                                      TRUE ~ "nonsign")) |> 
                           select(ERMNGroup, sign),
  by = "ERMNGroup")

# Join full group names back into dataframe
seed_smooth3 <- left_join(seed_smooth2,
                          plot_regspp_yr |> select(ERMNGroup, LegendName) |> unique(),
                          by = c('ERMNGroup'), 
                          relationship = 'many-to-many') |> 
  mutate(ERMNGroup = as.character(ERMNGroup)) |>
  group_by(ERMNGroup) |> mutate(drop = ifelse(sign == "notmod", "drop", "keep")) |> 
  filter(drop == "keep") |> select(-drop) |>  
  group_by(ERMNGroup) |> mutate(drop = ifelse(sum(estimate) == 0, "drop", "keep")) |> 
  filter(drop == "keep") |> select(-drop) |>  
  
  arrange(ERMNGroup)


rsspp_list <- sort(unique(seed_smooth3$ERMNGroup))
rsspp_list
length(rsspp_list)


# Saplings
sap_smooth2 <- 
  left_join(sap_smooth, 
            sap_smooth |> arrange(Year) |> group_by(ERMNGroup) |> 
              summarize(up_first = first(upper95), up_last = last(upper95),
                        lo_first = first(lower95), lo_last = last(lower95),
                        sign = case_when(up_first < lo_last ~ "signinc",
                                         lo_first > up_last ~ "signdec",
                                         is.na(up_first) ~ "notmod",
                                         TRUE ~ "nonsign")) |> 
              select(ERMNGroup, sign),
            by = "ERMNGroup")

sap_smooth3 <- left_join(sap_smooth2,
                         plot_regspp_yr |> select(ERMNGroup, LegendName) |> unique(),
                         by = c('ERMNGroup'), 
                         relationship = 'many-to-many') |> 
  mutate(ERMNGroup = as.character(ERMNGroup)) |>
  group_by(ERMNGroup) |> mutate(drop = ifelse(sign == "notmod", "drop", "keep")) |> 
  filter(drop == "keep") |> select(-drop) |>  
  group_by(ERMNGroup) |> mutate(drop = ifelse(sum(estimate) == 0, "drop", "keep")) |> 
  filter(drop == "keep") |> select(-drop) |>  
  arrange(ERMNGroup)

raspp_list <- sort(unique(sap_smooth3$ERMNGroup))
raspp_list

length(raspp_list)

# Seedling Figure
net_seeds <- 
  ggplot(seed_smooth3, 
         aes(x = Year, y = estimate)) +
  geom_line(aes(color = LegendName, linetype = LegendName), linewidth = 1.5) +
  labs(x = NULL, y = "Seedling Density (stems/ha)") +
  theme_FHM()+
  scale_color_manual(values = cols, name = NULL) +
  scale_linetype_manual(values = lines, name = NULL) +
  scale_x_continuous(breaks = c(seq(from, to, by = 2), to), 
                     limits = c(from, to)) +
  scale_y_continuous(limits = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'right', 
        legend.key.width = unit(1.5, 'cm'),
        plot.margin = margin(0.1, 0.4, 1.5, 0.4, 'cm'))# + 
#guides(color = guide_legend(nrow = 5))

net_seeds

ggsave(paste0(figpath, "Figure_2_", park, "_net_seedlings_by_species_cycle.jpg"),
       height = 5, width = 8, dpi = 600)


# Sapling Figure
net_saps <- 
  ggplot(sap_smooth3, aes(x = Year, y = estimate)) +
  geom_line(aes(color = LegendName, linetype = LegendName), linewidth = 1.5) +
  labs(x = NULL, y = "Sapling Density (stems/ha)") +
  theme_FHM()+
  scale_color_manual(values = cols, name = NULL) +
  scale_linetype_manual(values = lines, name = NULL) +
  scale_x_continuous(breaks = c(seq(from, to, by = 2), to), 
                     limits = c(from, to)) +
  scale_y_continuous(limits = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'right', 
        legend.key.width = unit(1.5, 'cm'),
        plot.margin = margin(0.5, 0.4, 0.1, 0.4, 'cm')) #+
#guides(color = guide_legend(nrow = spp_rows))

net_saps

ggsave(paste0(figpath, "Figure_3_", park, "_net_saplings_by_species_cycle.jpg"),
       height = 5, width = 8, dpi = 600)

ggarrange(net_seeds, net_saps, common.legend = T, legend = 'right', nrow = 2, labels = c("A.", "B."))

ggsave(paste0(new_path, "figures/Figure_4_", park, "_smoothed_regen_by_species_cycle.svg"),
       height = 11, width = 9.5)

ggsave(paste0(new_path, "figures/Figure_4_", park, "_smoothed_regen_by_species_cycle.png"),
       height = 11, width = 9.5, dpi = 600)



#----- Trends in invasive guilds over time -----
#stunted woodlands included
guilds <- do.call(sumQuadGuilds, c(args_vs, speciesType = 'invasive', splitHerb = F))
guild_list <- sort(unique(guilds$Group))

guild_smooth <- purrr::map_dfr(guild_list,
                               function(g){
                                 df <- guilds |> filter(Group %in% g)
                                 case_boot_loess(df, x = "SampleYear", y = "quad_pct_cover", 
                                                 ID = "Plot_Name", span = span,
                                                 group = "Group",
                                                 num_reps = 1) |> 
                                   mutate(guild = g)
                               })

#guild_smooth2 <- 
gcols <- c("Tree" = "#4A68BF",
           "Shrub" = "#CD5C5C",
           "Herbaceous" = "#228b22",
           "Graminoid" = "#ffd700")

guild_plot <- 
  ggplot(guild_smooth, aes(x = SampleYear, y = estimate)) +
  geom_line(aes(color = guild, group = guild), linewidth = 1.5) +
  labs(x = NULL, y = "% Invasive Cover") +
  theme_FHM()+
  scale_color_manual(values = gcols,  name = "Invasive Guild") +
  scale_x_continuous(breaks = c(seq(from, to, by = 2), to), 
                     limits = c(from, to)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'right',  
        legend.key.width = unit(1.5, 'cm'), 
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10), 
        plot.margin = margin(0.4, 0.4, 0.1, 0.4, "cm"))

guild_plot

ggsave(paste0(new_path, "figures/", "Figure_6_", park, "_smoothed_invasive_cover_by_guild_cycle.svg"),
       height = 4.6, width = 8)

ggsave(paste0(new_path, "figures/", "Figure_6_", park, "_smoothed_invasive_cover_by_guild_cycle.png"),
       height = 4.6, width = 8, dpi = 600)


# #---- Shrub cover -----
# # 
# shrubs <- do.call(joinMicroShrubData, args_vs) |> filter(SampleYear >= 2010)
# table(shrubs$ScientificName, shrubs$SampleYear)
# 
# other_native <- c("Amphicarpaea bracteata", "Aronia melanocarpa", "Gaylussacia baccata",
#                   "Hamamelis virginiana", # off for SAGA
#                   "Ilex verticillata", "Lyonia ligustrina", "Rosa",
#                   "Spiraea alba", "Vaccinium angustifolium", "Vaccinium pallidum", 
#                   "Vaccinium stamineum", "Zanthoxylum americanum", 'Rhus typhina', 
#                   'Vaccinium corymbosum') # adjust per park
# corylus = c("Corylus americana", "Corylus cornuta")
# other_exotic <- c("Crataegus", "Viburnum lantana", "Rhodotypos scandens", 
#                   'Viburnum dilatatum', 'Viburnum sieboldii',
#                   'Euonymus alatus')# adjust per park
# cornus <- c("Cornus", "Cornus amomum", "Cornus racemosa")
# rubus <- c("Rubus", "Rubus allegheniensis", "Rubus idaeus", "Rubus occidentalis", "Rubus odoratus")
# viburnum <- c("Viburnum dentatum", "Viburnum lentago", 'Viburnum lantanoides')
# vitis <- c("Vitis", "Vitis aestivalis", "Vitis riparia")
# lonicera <- c("Lonicera", "Lonicera - Exotic", "Lonicera morrowii")
# parth <- c("Parthenocissus", "Parthenocissus quinquefolia")
# natvines <- c(parth, vitis, "Toxicodendron radicans")
# ligustrum <- c("Ligustrum", "Ligustrum vulgare")
# 
# table(shrubs$ScientificName)
# 
# shrubs <- left_join(shrubs, prepTaxa() |> select(ScientificName, CommonName), by = "ScientificName")
# 
# shrubs <- shrubs %>% 
#   mutate(spp_grp = case_when(ScientificName %in% c(other_native, viburnum, parth, natvines) ~ "Other Native spp.",
#                              ScientificName %in% other_exotic ~ "Other Exotic spp.", 
#                              ScientificName %in% cornus ~ "Cornus spp. (dogwood)",
#                              ScientificName %in% corylus ~ "Corylus spp. (hazelnut)",
#                              ScientificName %in% rubus ~ "Rubus spp. (brambles)",
#                              #ScientificName %in% viburnum ~ "Viburnum spp. (arrowwood)", # included in other_native
#                              #ScientificName %in% vitis ~ "Vitis spp. (grape)",#  included in other_native
#                              ScientificName %in% lonicera ~ "Lonicera spp. (exotic honeysuckle)",
#                             # ScientificName %in% natvines ~ "Native vine spp.", # included in other_native
#                              ScientificName %in% ligustrum ~ "Ligustrum spp. (privet)",
#                              TRUE ~ paste0(ScientificName, " (", CommonName, ")")))
# 
# shrub_sum <- shrubs |> group_by(Plot_Name, SampleYear, spp_grp) |> 
#   summarize(avg_cov = sum(shrub_avg_cov, na.rm = T), .groups = 'drop')
# 
# table(shrub_sum$spp_grp)
# head(shrubs)
# 
# # Shifting to loess smoother with case bootstrap. Need a matrix of site x species x year
# plot_yr <- plot_evs |> ungroup() |> select(Plot_Name, SampleYear) |> filter(SampleYear >= 2010) |> unique()
# 
# # This will create all combination of plot, year, spp, but adds years not sampled by plots.
# # Need to then left join to drop unsampled years.
# plot_spp_yr1 <- expand.grid(Plot_Name = unique(plot_yr$Plot_Name), 
#                            SampleYear = unique(plot_yr$SampleYear),
#                            spp_grp = unique(shrub_sum$spp_grp)) |> 
#   filter(spp_grp != "None present (NA)") |> 
#   mutate(species = word(spp_grp, 1),
#          genus = ifelse(is.na(word(spp_grp, 2)), "spp.", word(spp_grp, 2)),
#          sppcode = toupper(paste0(substr(species, 1, 3), substr(genus, 1, 3)))) |> 
#   select(Plot_Name, SampleYear, spp_grp, sppcode)
# 
# plot_spp_yr <- left_join(plot_yr, plot_spp_yr1, by = c("Plot_Name", "SampleYear"))
# 
# dup_spp_check <- as.data.frame(table(plot_spp_yr$sppcode))
# 
# if(length(unique(dup_spp_check$Freq)) > 1)(stop("Not all species have the same frequency in expand grid. Check for duplicate species codes."))
# 
# shrub_smooth <- left_join(plot_spp_yr, shrub_sum |> select(Plot_Name, SampleYear, spp_grp, avg_cov), 
#                             by = c("Plot_Name", "SampleYear", "spp_grp")) #|> 
# 
# shrub_smooth[,c("avg_cov")][is.na(shrub_smooth[,c("avg_cov")])] <- 0
# table(shrub_smooth$sppcode)
# 
# spp_list <- sort(unique(shrub_smooth$sppcode))
# 
# span = 8/length(unique(shrub_smooth$SampleYear))
# 
# head(shrub_smooth)
# shrub_smooth <- purrr::map_dfr(spp_list, 
#                               function(spp){
#                                 df <- shrub_smooth |> filter(sppcode %in% spp)
#                                 case_boot_loess(df, x = "SampleYear", y = "avg_cov", ID = "Plot_Name",
#                                                 group = "sppcode", 
#                                                 span = span, num_reps = 1000) |>
#                                   mutate(sppcode = spp)
#                               }
# )
# 
# # Determine if significant based on whether first and last year CIs overlap
# shrub_smooth2 <- 
#   left_join(shrub_smooth, 
#             shrub_smooth |> arrange(SampleYear) |> group_by(sppcode) |> 
#               summarize(up_first = first(upper95), up_last = last(upper95),
#                         lo_first = first(lower95), lo_last = last(lower95),
#                         sign = case_when(up_first < lo_last ~ "signinc",
#                                          lo_first > up_last ~ "signdec",
#                                          is.na(up_first) ~ "notmod",
#                                          TRUE ~ "nonsign")) |> 
#               select(sppcode, sign),
#             by = "sppcode")
# 
# # Join full group names back into dataframe
# shrub_smooth3 <- left_join(shrub_smooth2,
#                           plot_spp_yr |> select(spp_grp, sppcode) |> unique(),
#                           by = c('sppcode'), 
#                           relationship = 'many-to-many') |> 
#   mutate(spp_grp = as.character(spp_grp)) |> 
#   arrange(spp_grp)
# 
# # Plotting trends by species group facet
# shrub_trends <- 
#   ggplot(shrub_smooth3, aes(x = SampleYear, y = estimate, linetype = sign,
#                            color = sign, fill = sign)) +
#   geom_ribbon(aes(ymin = lower95, ymax = upper95), alpha = 0.2) +
#   geom_line(linewidth = 0.5) +
#   scale_linetype_manual(values = c("notmod" = 'dashed', "nonsign" = 'dashed',
#                                    "signinc" = 'solid', "signdec" = 'solid'), drop = FALSE) +
#   scale_fill_manual(values = c("notmod" = "white", "nonsign" =  "#696969",
#                                "signinc" = "#228822", "signdec" = "#CD5C5C"), drop = FALSE)+
#   scale_color_manual(values = c("notmod" = "#CACACA", "nonsign" = "black",
#                                 "signinc" = "#228822", "signdec" = "#CD5C5C"), drop = FALSE) +
#   facet_wrap(~spp_grp, scales = 'free_y') + 
#   labs(y = "Shrub % Cover", x = "Year") +
#   scale_x_continuous(breaks = c(seq(2010, to, by = 3), to), 
#                      limits = c(2009, to)) +
#   theme_FHM() + 
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5), 
#         legend.position = 'bottom')
# 
# shrub_trends
# 
# svg(paste0(new_path, "figures/", "Figure_XB_", park, "_smoothed_shrub_cover_by_species_cycle.svg"),
#     height = 8, width = 7)
# shrub_trends
# dev.off()
# 
# table(shrub_smooth3$spp_grp)
# 
# net_shrubs <- 
#   ggplot(shrub_smooth3, 
#          aes(x = SampleYear, y = estimate)) +
#   geom_line(aes(color = spp_grp, linetype = spp_grp), linewidth = 1.5) +
#   labs(x = NULL, y = "Shrub % Cover") +
#   theme_FHM()+
#   scale_color_manual(values = c(
#   "Berberis thunbergii (Japanese barberry)" = "#4CE600",
#    "Celastrus orbiculatus (oriental bittersweet)" = "#FFAA00",
#     "Cornus spp. (dogwood)" = "#95DE34",
#     "Cornus alternifolia (alternate-leaf dogwood)" = "#95de34",
#     "Corylus spp. (hazelnut)" = "#57A588",
#     "Hamamelis virginiana (American witchhazel)" = "#FFF000",
#     "Lonicera spp. (exotic honeysuckle)" = "#0070FF",
#     "Native vine spp." = "#308E33",
#     "Other Exotic spp." = "#F9CF36",
#     "Other Native spp." = "#828282",
#     "Rhamnus cathartica (common buckthorn)" = "#FF7854",
#     "Rosa multiflora (multiflora rose)" = "#BB3636",
#     "Rubus spp. (brambles)" = "#DC91F6",
#     "Toxicodendron radicans (eastern poison ivy)" = "#937648",
#     "Viburnum spp. (arrowwood)" = "#57A588",
#     "Vitis spp. (grape)" = "#8C62B4",
#     "Zanthoxylum americanum (Common pricky-ash)" = "#B9C63A",
#     "Viburnum lantanoides (hobblebush)" = "#DC91F6",
#     "Ligustrum spp. (privet)" = "#B9C63A",
#     "Lindera benzoin (northern spicebush)" = "#95de34",
#     "Lonicera japonica (Japanese honeysuckle)" = "#0070FF",
#     "Rubus phoenicolasius (wine raspberry)" = "#937648"
#   ), name = NULL) +
#   scale_linetype_manual(values = c(
#    "Berberis thunbergii (Japanese barberry)" = "solid",
#    "Celastrus orbiculatus (oriental bittersweet)" = "solid",
#     "Cornus spp. (dogwood)" = "dotdash",
#     "Cornus alternifolia (alternate-leaf dogwood)" = "solid",
#     "Corylus spp. (hazelnut)" = "dotdash",
#     "Hamamelis virginiana (American witchhazel)" = 'dotdash',
#     "Lonicera spp. (exotic honeysuckle)" = "solid",
#     "Native vine spp." = "dashed",
#     "Other Exotic spp." = "solid",
#     "Other Native spp." = "dotdash",
#     "Parthenocissus spp. (Virginia creeper)" = "dotted",
#     "Rhamnus cathartica (common buckthorn)" = "solid",
#     "Rosa multiflora (multiflora rose)" = "solid",
#     "Rubus spp. (brambles)" = "dotdash",
#     "Toxicodendron radicans (eastern poison ivy)" = "dotted",
#     "Viburnum spp. (arrowwood)" = "dotdash",
#     "Vitis spp. (grape)" = "dotdash",
#      "Viburnum lantanoides (hobblebush)" = "solid",
#    "Zanthoxylum americanum (Common pricky-ash)" = "dotdash",
#    "Ligustrum spp. (privet)" = "solid",
#    "Lindera benzoin (northern spicebush)" = "dotted",
#    "Lonicera japonica (Japanese honeysuckle)" = "dotted",
#    "Rubus phoenicolasius (wine raspberry)" = "dotdash"
#   ), name = NULL) +
#   scale_x_continuous(breaks = c(seq(2010, to, by = 2), to), 
#                      limits = c(2009.9, to)) +
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
#         legend.position = 'bottom', 
#         legend.key.width = unit(1, 'cm')) + 
#   guides(color = guide_legend(nrow = 3))
# 
# net_shrubs
# 
# svg(paste0(new_path, "figures/", "Figure_5_", park, "_smoothed_shrub_cover_by_species_cycle.svg"),
#     height = 6, width = 8)
# net_shrubs
# dev.off()
