#-------------------------------------------------------------------
# Smoothed tree, sapling, seedling changes in abundance over time: ERMN
# ++++++++ MUST RUN source_script_ERMN.R FIRST ++++++++
#-------------------------------------------------------------------
devtools::install_github("sjperles/ERMNforest")
library(ERMNforest)
library(forestTrends)
library(tidyverse)
library(sf)
library(ggpubr)

rm(list = ls())

span <- 4/5 #roughly linear between timesteps

#---- Eventually delete this whole section once source_script_ERMN is correct ----
setwd('C:/ERMN Veg/Veg_Data_2022/ERMNforest_summaries')
tablepath = 'C:/ERMN Veg/Veg_Data_2022/ERMNforest_summaries/draft_tables/DEWA/'
shppath = 'C:/ERMN Veg/Veg_Data_2022/ERMNforest_summaries/shapefiles/DEWA/'
figpath = 'C:/ERMN Veg/Veg_Data_2022/ERMNforest_summaries/figures/DEWA/'

importData(type='DSN', odbc="ERMNVeg20250430")


# Set parameters
park = 'DEWA'
veg = 'Succe'
years = c(2007:2024)
from = 2007
from_4yr = 2019 # earliest year in the latest four year cycle (to - 4). If to = 2023, then from_4yr = 2018
to = 2024
QAQC = FALSE
retired = TRUE
anrevisit = FALSE
cycle_latest = 4
park_crs = ifelse(park %in% c("DEWA"), 26918, 26917)
plot_size = 707


args_all = list(park = park, veg = veg, from = from, to = to, QAQC = QAQC, retired = retired, anrevisit = TRUE)
args_4yr = list(park = park, veg = veg, from = from_4yr, to = to, QAQC = QAQC, retired = retired, anrevisit = anrevisit)
args_vs = list(park = park, veg = veg, from = from, to = to, QAQC = QAQC, retired = retired, anrevisit = anrevisit)

trspp_grps <- read.csv("ERMN_tree_species_groups.csv")

plotevs <- joinLocEvent(park = park, veg = veg, years = years, QAQC = QAQC, retired = retired, anrevisit = anrevisit)
plotevs_4yr <- plotevs %>% filter(between(Year, from_4yr, to))

table(plotevs$Vegetation_Domain, plotevs$Cycle)

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
  "Acer saccharinum (silver maple)" = "#000075",
  "Acer pensylvanicum (striped mapled)" = "#FFFF00",
  "Amelanchier spp. (serviceberry)" = "#9371B9",
  "Ailanthus altissima (tree-of-heaven)" = "#cd4a8f",
  "Asimina triloba (pawpaw)" = "#FF00C5",
  "Aesculus flava (buckeye)" = "#efdf00",
  "Betula lenta (black birch)" = "#ffd8b1", # darkened color for 2024 NETN figs; does not match maps or MIDN
  "Betula spp. (birch)" = "#ffd8b1", 
  "Benthamidia florida (flowering dogwood)" = "#000075",
  "Carpinus caroliniana (musclewood)" = "#cd4a8f",
  "Carya spp. (hickory)" = "#911eb4",
  "Carya cordiformis (bitternut hickory)" = "#911eb4",
  "Crataegus spp. (hawthorn)" = "#42d4f4",
  "Diospyros virginiana (persimmon)" = "#006666",
  "Fagus grandifolia (beech)" = "#FFAA00",
  "Fraxinus spp. (ash)" = "#A87000",
  "Fraxinus americana (white ash)" = "#A87000",
  "Ilex opaca (American holly)" = "#42d4f4",
  "Juglans nigra (black walnut)" = "#42d4f4",
  "Juniperus virginiana (eastern redcedar)" = "#9371B9",
  "Liquidambar styraciflua (sweetgum)" = "#FFFF00",
  "Liriodendron tulipifera (tuliptree)" = "#4363d8",
  "Magnolia spp. (magnolia)" = "#620436",
  "Nyssa sylvatica (black gum)" = "#000075",
  "Ostrya virginiana (ironwood)" = "#42d4f4",
  "Other exotic spp." = "#ca0020",
  "Other native canopy spp." = "#d9d9d9",
  "Other native subcanopy spp." = "#ffa8b4",
  "Pinus spp. (pine)" = "#5A462B",
  "Pinus strobus (eastern white pine)" = "#5A1111",
  "Pinus virginiana (Virginia pine)" = "#E5740D",
  "Pinus resinosa (red pine)" = "#E5740D",
  "Platanus occidentalis (sycamore)" = "#E5740D",
  "Populus tremuloides (quaking aspen)" = "#efdf00",
  "Prunus serotina (black cherry)" ="#00E6A9", 
  "Prunus spp. (cherry)" ="#0d00ba", 
  "Pyrus calleryana (Bradford pear)" = "#cd4a8f",
  "Quercus spp. (oak)" = "#0E5D2C",
  "Quercus alba (white oak)" = "#0E5D2C",
  "Quercus rubra (northern red oak)" = "#0E5D2C",
  "Quercus coccinea (scarlet oak)" = "#0E5D2C",
  "Quercus velutina (black oak)" = "#0E5D2C",
  "Quercus montana (chestnut oak)" = "#0E5D2C",
  "Quercus palustris (pin oak)" = "#072b16",
  "Robinia pseudoacacia (black locust)" = "#cccc00",
  "Sassafras albidum (sassafrass)" = "#59538A",
  "Tilia americana (basswood)" = "#fb693b",
  "Tsuga canadensis (hemlock)" = "#9bd2ef",
  "Ulmus spp. (elm)" = "#808000", 
  "Unknown spp." = "#CACACA",
  "Picea spp. (spruce)" = "#000075",#ACAD only
  "Populus spp. (aspen)" = "#FFFF00")#ACAD only


lines = c(
  "Acer rubrum (red maple)" = "solid",
  "Acer negundo (box elder)" = "solid",
  "Acer platanoides (Norway maple)" = "solid",
  "Acer spp. (maple)" = "solid",
  "Acer saccharum (sugar maple)" = "solid",
  "Acer saccharinum (silver maple)" = "solid",
  "Acer pensylvanicum (striped mapled)" = "solid",
  "Amelanchier spp. (serviceberry)" = "solid",
  "Ailanthus altissima (tree-of-heaven)" = "solid",
  "Asimina triloba (pawpaw)" = "solid",
  "Aesculus flava (buckeye)" = "solid",
  "Betula lenta (black birch)" = "solid", # darkened color for 2024 NETN figs; does not match maps or MIDN
  "Betula spp. (birch)" = "solid", 
  "Benthamidia florida (flowering dogwood)" = "solid",
  "Carpinus caroliniana (musclewood)" = "solid",
  "Carya spp. (hickory)" = "solid",
  "Carya cordiformis (bitternut hickory)" = "dotdash",
  "Crataegus spp. (hawthorn)" = "solid",
  "Fagus grandifolia (beech)" = "solid",
  "Fraxinus spp. (ash)" = "solid",
  "Fraxinus americana (white ash)" = "solid",
  "Ilex opaca (American holly)" = "solid",
  "Juglans nigra (black walnut)" = "solid",
  "Juniperus virginiana (eastern redcedar)" = "solid",
  "Liquidambar styraciflua (sweetgum)" = "solid",
  "Liriodendron tulipifera (tuliptree)" = "solid",
  "Magnolia spp. (magnolia)" = "solid",
  "Nyssa sylvatica (black gum)" = "solid",
  "Ostrya virginiana (ironwood)" = "solid",
  "Other exotic spp." = "solid",
  "Other native canopy spp." = "solid",
  "Pinus spp. (pine)" = "solid",
  "Pinus strobus (eastern white pine)" = "solid",
  "Pinus taeda (loblolly pine)" = "solid",
  "Pinus virginiana (Virginia pine)" = "solid",
  "Pinus resinosa (red pine)" = "solid",
  "Platanus occidentalis (sycamore)" = "solid",
  "Populus tremuloides (quaking aspen)" = "solid",
  "Prunus spp. (cherry)" ="solid", 
  "Prunus serotina (black cherry)" ="solid", 
  "Pyrus calleryana (Bradford pear)" = "solid",
  "Quercus spp. (oak)" = "solid",
  "Quercus alba (white oak)" = "dashed",
  "Quercus rubra (northern red oak)" = "twodash",
  "Quercus coccinea (scarlet oak)" = "longdash",
  "Quercus velutina (black oak)" = "dotdash",
  "Quercus montana (chestnut oak)" = "dotted",
  "Quercus palustris (pin oak)" = "solid",
  "Robinia pseudoacacia (black locust)" = "solid",
  "Other native subcanopy spp." = "solid",
  "Tilia americana (basswood)" = "solid",
  "Tsuga canadensis (hemlock)" = "solid",
  "Ulmus spp. (elm)" = "solid", 
  "Unknown spp." = "solid",
  "Diospyros virginiana (persimmon)" = "solid", #for ASIS only
  "Sassafras albidum (sassafrass)" = "solid", #ASIS only
  "Abies balsamea (balsam fir)" = 'solid',#ACAD only
  "Other conifer" = "solid",#ACAD only
  "Picea spp. (spruce)" = "solid",#ACAD only
  "Populus spp. (aspen)" = "solid")#ACAD only

#####---- Figure 1 Regen trends by size class #####
reg <- joinRegenData(speciesType = 'native', canopyForm = 'canopy', units = 'ha',
                     park=park, veg=veg, years=c(from:to), QAQC = QAQC, retired = retired, anrevisit = anrevisit)

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
  ggplot(reg_smooth, aes(x = size_class, y = estimate, ymin=0, color = size_class, 
                         group = size_class)) + theme_FHM() +
  geom_bar(stat = 'identity', aes(fill = size_class, color = 'reg_colors'))+
  geom_errorbar(aes(ymin = lower95, ymax = upper95), width = 0.2, linewidth = 0.5, 
                color = 'DimGrey', alpha = 0.8) +
  labs(x  = "Cycle", y = bquote(Stems/ha))+
  facet_wrap(~cycle, labeller = as_labeller(cycle_labs), ncol = 6) +
  scale_color_manual(values = reg_colors, name = "size_class",
                     labels = reg_labels)+
  scale_fill_manual(values = reg_colors, name = "size_class",
                    labels = reg_labels)+
  scale_x_discrete(name = "size_class",
                   labels = reg_labels)+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
        legend.position = 'none')

reg_trend_plot

ggsave(paste0(figpath, "Figure_1_", park, "_regen_by_size_class_", veg, "_by_cycle.jpeg"),
       height = 5, width = 7.5, units = 'in')


######---- Tree trends by species ---- ######
trees1 <- joinTreeData(status = 'live', speciesType = 'all', canopyForm = 'all',
                     park=park, veg=veg, years=c(from:to), QAQC = QAQC, retired = retired, anrevisit = anrevisit)

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


if(park == "DEWA" & veg == "Succe"){
  tree_grps <- tree_grps %>% 
    mutate(ERMNGroup = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum",
                                 Latin_name == "Acer saccharum" ~ "Acer saccharum",
                                 Latin_name == "Acer saccharinum" ~ "Other Native",
                                 Latin_name == "Benthamidia florida" ~ "Benthamidia florida",
                                 Latin_name == "Carpinus caroliniana" ~ "Carpinus caroliniana",
                                 Latin_name == "Juglans nigra" ~ "Juglans nigra",
                                 Latin_name == "Juniperus virginiana" ~ "Juniperus virginiana",
                                 Latin_name == "Prunus serotina" ~ "Prunus serotina",
                                 Latin_name == "Pinus strobus" ~ "Pinus strobus",
                                 Latin_name == "Populus tremuloides" ~ "Populus tremuloides",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(ERMNGroup = case_when(ERMNGroup == "Fagus" ~ "Other Native",
                                 ERMNGroup == "Liriodendron" ~ "Other Native",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(LegendName = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum (red maple)",
                                  Latin_name == "Acer saccharum" ~ "Acer saccharum (sugar maple)",
                                  Latin_name == "Acer saccharinum" ~ "Other native canopy spp.",
                                  Latin_name == "Benthamidia florida" ~ "Benthamidia florida (flowering dogwood)",
                                  Latin_name == "Carpinus caroliniana" ~ "Carpinus caroliniana (musclewood)",
                                  Latin_name == "Juglans nigra" ~ "Juglans nigra (black walnut)",
                                  Latin_name == "Juniperus virginiana" ~ "Juniperus virginiana (eastern redcedar)",
                                  Latin_name == "Prunus serotina" ~ "Prunus serotina (black cherry)",
                                  Latin_name == "Pinus strobus" ~ "Pinus strobus (eastern white pine)",
                                  Latin_name == "Populus tremuloides" ~ "Populus tremuloides (quaking aspen)",
                                  TRUE ~ LegendName)) %>%
    mutate(LegendName = case_when(ERMNGroup == "Other Native" ~ "Other native canopy spp.",
                                  TRUE ~ LegendName))
  
}

if(park == "DEWA" & veg == "EarlySucc"){
  tree_grps <- tree_grps %>% 
    mutate(ERMNGroup = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum",
                                 Latin_name == "Juglans nigra" ~ "Juglans nigra",
                                 Latin_name == "Juniperus virginiana" ~ "Juniperus virginiana",
                                 Latin_name == "Prunus serotina" ~ "Prunus serotina",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(LegendName = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum (red maple)",
                                  Latin_name == "Juglans nigra" ~ "Juglans nigra (black walnut)",
                                  Latin_name == "Juniperus virginiana" ~ "Juniperus virginiana (eastern redcedar)",
                                  Latin_name == "Prunus serotina" ~ "Prunus serotina (black cherry)",
                                  TRUE ~ LegendName))
    
}

if(park == "DEWA" & veg == "RipPalus"){
  tree_grps <- tree_grps %>% 
    mutate(ERMNGroup = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum",
                                 Latin_name == "Acer saccharum" ~ "Acer saccharum",
                                 Latin_name == "Acer saccharinum" ~ "Acer saccharinum",
                                 Latin_name == "Carya cordiformis" ~ "Carya cordiformis",
                                 Latin_name == "Platanus occidentalis" ~ "Platanus occidentalis",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(ERMNGroup = case_when(ERMNGroup == "Fagus" ~ "Other Native",
                                 ERMNGroup == "Fraxinus" ~ "Other Native",
                                 ERMNGroup == "Prunus" ~ "Other Native",
                                 ERMNGroup == "Ulmus" ~ "Other Native",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(LegendName = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum (red maple)",
                                  Latin_name == "Acer saccharum" ~ "Acer saccharum (sugar maple)",
                                  Latin_name == "Acer saccharinum" ~ "Acer saccharinum (silver maple)",
                                  Latin_name == "Carya cordiformis" ~ "Carya cordiformis (bitternut hickory)",
                                  Latin_name == "Platanus occidentalis" ~ "Platanus occidentalis (sycamore)",
                                  TRUE ~ LegendName)) %>%
    mutate(LegendName = case_when(ERMNGroup == "Other Native" ~ "Other native canopy spp.",
                                  TRUE ~ LegendName))
}


if(park == "DEWA" & veg == "Xeric"){
  tree_grps <- tree_grps %>% 
    mutate(ERMNGroup = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum",
                                 Latin_name == "Amelanchier arborea" ~ "Amelanchier spp.",
                                 Latin_name == "Betula lenta" ~ "Betula lenta",
                                 Latin_name == "Nyssa sylvatica" ~ "Nyssa sylvatica",
                                 Latin_name == "Quercus alba" ~ "Quercus alba",
                                 Latin_name == "Quercus rubra" ~ "Quercus rubra",
                                 Latin_name == "Quercus coccinea" ~ "Quercus coccinea",
                                 Latin_name == "Quercus velutina" ~ "Quercus velutina",
                                 Latin_name == "Quercus montana" ~ "Quercus montana",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(ERMNGroup = case_when(ERMNGroup == "Acer" ~ "Other Native",
                                 ERMNGroup == "Betula" ~ "Other Native",
                                 ERMNGroup == "Fagus" ~ "Other Native",
                                 ERMNGroup == "Fraxinus" ~ "Other Native",
                                 ERMNGroup == "Liriodendron" ~ "Other Native",
                                 ERMNGroup == "Prunus" ~ "Other Native",
                                 ERMNGroup == "Quercus" ~ "Other Native",
                                 ERMNGroup == "Tilia" ~ "Other Native",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(LegendName = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum (red maple)",
                                  Latin_name == "Amelanchier arborea" ~ "Amelanchier spp. (serviceberry)",
                                  Latin_name == "Betula lenta" ~ "Betula lenta (black birch)",
                                  Latin_name == "Nyssa sylvatica" ~ "Nyssa sylvatica (black gum)",
                                  Latin_name == "Quercus alba" ~ "Quercus alba (white oak)",
                                  Latin_name == "Quercus rubra" ~ "Quercus rubra (northern red oak)",
                                  Latin_name == "Quercus coccinea" ~ "Quercus coccinea (scarlet oak)",
                                  Latin_name == "Quercus velutina" ~ "Quercus velutina (black oak)",
                                  Latin_name == "Quercus montana" ~ "Quercus montana (chestnut oak)",
                                  TRUE ~ LegendName)) %>%
    mutate(LegendName = case_when(ERMNGroup == "Other Native" ~ "Other native canopy spp.",
                                  TRUE ~ LegendName))
}

if(park == "DEWA" & veg == "Mesic"){
  tree_grps <- tree_grps %>% 
    mutate(ERMNGroup = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum",
                                 Latin_name == "Acer saccharum" ~ "Acer saccharum",
                                 Latin_name == "Juglans nigra" ~ "Juglans nigra",
                                 Latin_name == "Quercus rubra" ~ "Quercus rubra",
                                 Latin_name == "Quercus alba" ~ "Quercus alba",
                                 Latin_name == "Quercus velutina" ~ "Quercus velutina",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(ERMNGroup = case_when(ERMNGroup == "Liriodendron" ~ "Other Native",
                                 ERMNGroup == "Prunus" ~ "Other Native",
                                 ERMNGroup == "Tilia" ~ "Other Native",
                                 ERMNGroup == "Ulmus" ~ "Other Native",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(LegendName = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum (red maple)",
                                  Latin_name == "Acer saccharum" ~ "Acer saccharum (sugar maple)",
                                  Latin_name == "Juglans nigra" ~ "Juglans nigra (black walnut)",
                                  Latin_name == "Quercus rubra" ~ "Quercus rubra (northern red oak)",
                                  Latin_name == "Quercus alba" ~ "Quercus alba (white oak)",
                                  Latin_name == "Quercus velutina" ~ "Quercus velutina (black oak)",
                                  TRUE ~ LegendName)) %>%
    mutate(LegendName = case_when(ERMNGroup == "Other Native" ~ "Other native canopy spp.",
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
                                                     span = span, num_reps = 50) |>
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
                                                   span = span, num_reps = 50) |>
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
  theme_FHM()+
  scale_color_manual(values = cols,  name = NULL) +
  scale_linetype_manual(values = lines,  name = NULL) +
  scale_x_continuous(breaks = c(seq(from, to, by = 2), to), 
                     limits = c(from, to)) +
  scale_y_continuous(limits = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = 'right',
        legend.key.width = unit(1.5, 'cm'),
        plot.margin = margin(0.1, 0.4, 1.5, 0.4, 'cm')) #+ 
#guides(color = guide_legend(nrow = spp_rows))

net_stems

ggsave(paste0(figpath, "Figure_5_", park, "_smoothed_tree_stems_", veg, "_by_species_cycle.jpg"),
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
ggsave(paste0(figpath, "Figure_4_", park, "_smoothed_BA_", veg, "_by_species_cycle.jpg"),
       height = 5, width = 8, dpi=600)



ggarrange(net_stems, net_ba, common.legend = T, legend = 'right', nrow = 2, labels = c("A.", "B.")) 

ggsave(paste0(new_path, "figures/Figure_5_", park, "_smoothed_tree_dens_BA_by_species_cycle.svg"),
       height = 11, width = 9.5)

ggsave(paste0(new_path, "figures/Figure_5_", park, "_smoothed_tree_dens_BA_by_species_cycle.png"),
       height = 11, width = 9.5, dpi = 600)


###### ----- Regeneration trends for seedlings and saplings ----- ######
reg1 <- joinRegenSpData(speciesType = 'all', canopyForm = 'all', units = 'ha',
                     park=park, veg=veg, years=c(from:to), QAQC = QAQC, retired = retired, anrevisit = anrevisit)

reg_grps <- left_join(reg1, trspp_grps |> select(Latin_name, ERMNGroup, LegendName), 
                      by = c("Latin_name"))

table(reg_grps$Latin_name, reg_grps$ERMNGroup)
table(reg_grps$ERMNGroup)

#Park specific exceptions to default groupings
if(park == "DEWA" & veg == "Succe"){
  reg_grps <- reg_grps %>% mutate(ERMNGroup = case_when(ERMNGroup == "Liriodendron" ~ "Other Native",
                                 ERMNGroup == "Tilia" ~ "Other Native",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(LegendName = case_when(ERMNGroup == "Other Native" ~ "Other native canopy spp.",
                                  TRUE ~ LegendName))
}

if(park == "DEWA" & veg == "EarlySucc"){
  reg_grps <- reg_grps %>% mutate(ERMNGroup = case_when(Latin_name == "Juglans nigra" ~ "Juglans nigra",
                                                        Latin_name == "Juniperus virginiana" ~ "Juniperus virginiana",
                                                        TRUE ~ ERMNGroup)) %>% 
    mutate(ERMNGroup = case_when(ERMNGroup == "Liriodendron" ~ "Other Native",
                                 ERMNGroup == "Fagus" ~ "Other Native",
                                 ERMNGroup == "Ulmus" ~ "Other Native",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(LegendName = case_when(Latin_name == "Juglans nigra" ~ "Juglans nigra (black walnut)",
                                  Latin_name == "Juniperus virginiana" ~ "Juniperus virginiana (eastern redcedar)",
                                  TRUE ~ LegendName)) %>% 
    mutate(LegendName = case_when(ERMNGroup == "Other Native" ~ "Other native canopy spp.",
                                  TRUE ~ LegendName))
}

if(park == "DEWA" & veg == "RipPalus"){
  reg_grps <- reg_grps %>% 
    mutate(ERMNGroup = case_when(Latin_name == "Carpinus caroliniana" ~ "Carpinus caroliniana",
                                 Latin_name == "Ostrya virginiana" ~ "Ostrya virginiana",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(ERMNGroup = case_when(ERMNGroup == "Betula" ~ "Other Native",
                                 ERMNGroup == "Pinus" ~ "Other Native",
                                 ERMNGroup == "Ulmus" ~ "Other Native",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(LegendName = case_when(Latin_name == "Carpinus caroliniana" ~ "Carpinus caroliniana (musclewood)",
                                  Latin_name == "Ostrya virginiana" ~ "Ostrya virginiana (ironwood)",
                                  TRUE ~ LegendName)) %>%
    mutate(LegendName = case_when(ERMNGroup == "Other Native" ~ "Other native canopy spp.",
                                  TRUE ~ LegendName))
}

if(park == "DEWA" & veg == "Mesic"){
  reg_grps <- reg_grps %>% 
    mutate(ERMNGroup = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum",
                                 Latin_name == "Acer saccharum" ~ "Acer saccharum",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(ERMNGroup = case_when(ERMNGroup == "Acer" ~ "Other Native",
                                 ERMNGroup == "Liriodendron" ~ "Other Native",
                                 ERMNGroup == "Tilia" ~ "Other Native",
                                 ERMNGroup == "Ulmus" ~ "Other Native",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(LegendName = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum (red maple)",
                                  Latin_name == "Acer saccharum" ~ "Acer saccharum (sugar maple)",
                                  TRUE ~ LegendName)) %>%
    mutate(LegendName = case_when(ERMNGroup == "Other Native" ~ "Other native canopy spp.",
                                  TRUE ~ LegendName))
}


if(park == "DEWA" & veg == "Xeric"){
  reg_grps <- reg_grps %>% 
    mutate(ERMNGroup = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum",
                                 Latin_name == "Amelanchier" ~ "Amelanchier spp.",
                                 Latin_name == "Amelanchier arborea" ~ "Amelanchier spp.",
                                 Latin_name == "Betula lenta" ~ "Betula lenta",
                                 Latin_name == "Nyssa sylvatica" ~ "Nyssa sylvatica",
                                 Latin_name == "Sassafras albidum" ~ "Sassafras albidum",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(ERMNGroup = case_when(ERMNGroup == "Acer" ~ "Other Native",
                                 ERMNGroup == "Betula" ~ "Other Native",
                                 ERMNGroup == "Fagus" ~ "Other Native",
                                 ERMNGroup == "Fraxinus" ~ "Other Native",
                                 ERMNGroup == "Liriodendron" ~ "Other Native",
                                 ERMNGroup == "Magnolia" ~ "Other Native",
                                 ERMNGroup == "Prunus" ~ "Other Native",
                                 ERMNGroup == "Ulmus" ~ "Other Native",
                                 ERMNGroup == "Tilia" ~ "Other Native",
                                 TRUE ~ ERMNGroup)) %>% 
    mutate(LegendName = case_when(Latin_name == "Acer rubrum" ~ "Acer rubrum (red maple)",
                                  Latin_name == "Amelanchier" ~ "Amelanchier spp. (serviceberry)",
                                  Latin_name == "Amelanchier arborea" ~ "Amelanchier spp. (serviceberry)",
                                  Latin_name == "Betula lenta" ~ "Betula lenta (black birch)",
                                  Latin_name == "Nyssa sylvatica" ~ "Nyssa sylvatica (black gum)",
                                  Latin_name == "Sassafras albidum" ~ "Sassafras albidum (sassafrass)",
                                  TRUE ~ LegendName)) %>%
    mutate(LegendName = case_when(ERMNGroup == "Other Native" ~ "Other native canopy spp.",
                                  TRUE ~ LegendName))
}

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


# Join regen data to all possible combos of year, plot, species
plot_regspp_yr <- left_join(plot_regspp_yr2, reg_grps |> select(ERMNGroup, LegendName) |> unique(), 
                            by = "ERMNGroup")

reg_spp_smooth1 <- left_join(plot_regspp_yr, reg_grps [,c("Plot_Name", "Year", "ERMNGroup", "LegendName", "tot.seed.dens","sap.dens","sap.BA.m2")],
                           by = c("Plot_Name", "Year", "ERMNGroup", "LegendName")) 

reg_spp_smooth1[,c("tot.seed.dens", "sap.dens", "sap.BA.m2")][is.na(reg_spp_smooth1[,c("tot.seed.dens", "sap.dens", "sap.BA.m2")])] <- 0

reg_spp_sum <- reg_spp_smooth1 |> group_by(Plot_Name, Year, ERMNGroup, LegendName) |> 
  summarize(seed_ha = sum(tot.seed.dens),
            sap_ha = sum(sap.dens),
            sap_BA = sum(sap.BA.m2),.groups = 'drop')

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
                                              span = span, num_reps = 50) |>
                               mutate(ERMNGroup = spp)
                              }
                              )

sap_smooth <- purrr::map_dfr(rspp_list, 
                              function(spp){
                                df <- reg_spp_sum |> filter(ERMNGroup %in% spp)
                                case_boot_loess(df, x = "Year", y = "sap_ha", ID = "Plot_Name",
                                                group = "ERMNGroup", 
                                                span = span, num_reps = 50) |>
                                  mutate(ERMNGroup = spp)
                              }
)

sapBA_smooth <- purrr::map_dfr(rspp_list, 
                             function(spp){
                               df <- reg_spp_sum |> filter(ERMNGroup %in% spp)
                               case_boot_loess(df, x = "Year", y = "sap_BA", ID = "Plot_Name",
                                               group = "ERMNGroup", 
                                               span = span, num_reps = 50) |>
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


# Sapling Density
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

# Sapling Basal Area
sapBA_smooth2 <- 
  left_join(sapBA_smooth, 
            sapBA_smooth |> arrange(Year) |> group_by(ERMNGroup) |> 
              summarize(up_first = first(upper95), up_last = last(upper95),
                        lo_first = first(lower95), lo_last = last(lower95),
                        sign = case_when(up_first < lo_last ~ "signinc",
                                         lo_first > up_last ~ "signdec",
                                         is.na(up_first) ~ "notmod",
                                         TRUE ~ "nonsign")) |> 
              select(ERMNGroup, sign),
            by = "ERMNGroup")

sapBA_smooth3 <- left_join(sapBA_smooth2,
                         plot_regspp_yr |> select(ERMNGroup, LegendName) |> unique(),
                         by = c('ERMNGroup'), 
                         relationship = 'many-to-many') |> 
  mutate(ERMNGroup = as.character(ERMNGroup)) |>
  group_by(ERMNGroup) |> mutate(drop = ifelse(sign == "notmod", "drop", "keep")) |> 
  filter(drop == "keep") |> select(-drop) |>  
  group_by(ERMNGroup) |> mutate(drop = ifelse(sum(estimate) == 0, "drop", "keep")) |> 
  filter(drop == "keep") |> select(-drop) |>  
  arrange(ERMNGroup)




# Seedling Figure
net_seeds <- 
  ggplot(seed_smooth3, 
         aes(x = Year, y = estimate)) +
  geom_line(aes(color = LegendName, linetype = LegendName), linewidth = 1.25) +
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

ggsave(paste0(figpath, "Figure_2_", park, "_net_seedlings_", veg, "_by_species_cycle.jpg"),
       height = 5, width = 8, dpi = 600)


# Sapling Density Figure
net_saps <- 
  ggplot(sap_smooth3, aes(x = Year, y = estimate)) +
  geom_line(aes(color = LegendName, linetype = LegendName), linewidth = 1.25) +
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

ggsave(paste0(figpath, "Figure_3_", park, "_net_saplings_", veg, "_by_species_cycle.jpg"),
       height = 5, width = 8, dpi = 600)


# Sapling Basal Area Figure
net_sapsBA <- 
  ggplot(sapBA_smooth3, aes(x = Year, y = estimate)) +
  geom_line(aes(color = LegendName, linetype = LegendName), linewidth = 1.25) +
  labs(x = NULL, y = "Sapling Basal Area (m2)") +
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

net_sapsBA

ggsave(paste0(figpath, "Figure_3BA_", park, "_net_saplingsBA_", veg, "_by_species_cycle.jpg"),
       height = 5, width = 8, dpi = 600)






# Pasting figures together
ggarrange(net_seeds, net_saps, common.legend = T, legend = 'right', nrow = 2, labels = c("A.", "B."))

ggsave(paste0(new_path, "figures/Figure_4_", park, "_smoothed_regen_by_species_cycle.svg"),
       height = 11, width = 9.5)

ggsave(paste0(new_path, "figures/Figure_4_", park, "_smoothed_regen_", veg, "by_species_cycle.png"),
       height = 11, width = 9.5, dpi = 600)


