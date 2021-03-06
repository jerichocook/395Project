# Things that need to be loaded from the library
library(plyr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggbiplot)

# Load the Diet Database
diet_database = read.table("aviandietdatabase.txt",header=TRUE, sep = '\t', quote = '\"', fill=T, stringsAsFactors = F)

# Source the Database functions
source('database_summary_functions.R')

# Search Database for diurnal and nocturnal raptors by Family
species = unique(diet_database[, c('Common_Name', 'Family')])
spCountByFamily = data.frame(table(species$Family))

# Diurnal raptors of interest (Family Accipitridae) by Common Name
  # Bald Eagle
  # Black-chested Buzzard-Eagle
  # Broad-winged Hawk
  # Common Black Hawk
  # Cooper's Hawk
  # Ferruginous Hawk
  # Golden Eagle
  # Gray Hawk
  # Harris's Hawk
  # Northern Harrier
  # Mississippi Kite
  # Northern Goshawk
  # Variable Hawk
  # Red-shouldered Hawk
  # Red-tailed Hawk
  # Rough-legged Hawk
  # White-tailed Hawk
  # Sharp-shinned Hawk
  # Short-tailed Hawk
  # Snail Kite
  # Swainson's Hawk
  # Swallow-tailed Kite
  # White-tailed Eagle
  # White-tailed Kite
  # Zone-tailed Hawk
# Total number of species = 25

# Nocturnal raptors of interest (Family Strigidae) by Common Name
  # Great Horned Owl
  # Short-eared Owl
  # Barred Owl
  # Boreal Owl
  # Burrowing Owl
  # Eastern Screech-Owl
  # Elf Owl
  # Ferruginous Pygmy-Owl
  # Flammulated Owl
  # Great Gray Owl
  # Long-eared Owl
  # Northern Hawk Owl
  # Northern Pygmy-Owl
  # Northern Saw-whet Owl
  # Snowy Owl
  # Spotted Owl
  # Whiskered Screech-Owl
  # Western Screech-Owl
# Total number of species = 18

# Subset Database for focal species of Families Accipitridae and Strigidae
focal_spp = diet_database[diet_database$Family == "Accipitridae" | diet_database$Family == "Strigidae",]

# assign diurnal/nocturnal status to species

speciesNames = unique(focal_spp$Common_Name)

owls = speciesNames[grep("Owl", speciesNames)]

speciesTraits = data.frame(species = speciesNames, activity = 'diurnal')
speciesTraits$activity = as.character(speciesTraits$activity)
speciesTraits$activity[speciesTraits$species %in% owls] = 'nocturnal'
speciesTraits$actColor = 'blue'
speciesTraits$actColor[speciesTraits$activity == 'nocturnal'] = 'red'

# For-Loop for i (Items)

final_i = c()

for(sp in speciesNames){
  i = dietSummary(sp, by = 'Order', season = c('spring', 'summer'), dietType = 'Items')
  print(sp)
  if(length(i != 0)){
    sp_vec_i = cbind(sp, i) 
    final_i = rbind(final_i, sp_vec_i) 
  }
}

# For-Loop for w (Wt_or_Vol)

final_w = c()

for(sp in speciesNames){
  w = dietSummary(sp, by = 'Order', season = c('spring', 'summer'), dietType = 'Wt_or_Vol')
  print(sp)
  if(length(w != 0)){
    sp_vec_w = cbind(sp, w) 
    final_w = rbind(final_w, sp_vec_w) 
  }
}

# rename Frac_Diet
final_i$Items = final_i$Frac_Diet
final_i$Frac_Diet = NULL
final_w$Wt_or_Vol = final_w$Frac_Diet
final_w$Frac_Diet = NULL

# pca for final_i w/ ggplot

final_i_matrix = spread(final_i, Taxon, Items)
final_i_matrix[is.na(final_i_matrix)] = 0
final_i_matrix$sp = NULL
pca_i = prcomp(final_i_matrix)
pca_i$rotation
plot(pca_i$x[,1], pca_i$x[,2], pch = 16)
pca_i_df = as.data.frame(pca_i$x)

ggplot(data = pca_i_df, aes(x = PC1, y = PC2)) + 
  xlab("PC1") +
  ylab("PC2") +
  geom_hline(yintercept = 0, colour = "black") + 
  geom_vline(xintercept = 0, colour = "black") + 
  theme_classic() +
  geom_point(color = "tomato")

# pca for final_w w/ ggplot

final_w_matrix = spread(final_w, Taxon, Wt_or_Vol)
final_w_matrix[is.na(final_w_matrix)] = 0
final_w_matrix$sp = NULL
pca_w = prcomp(final_w_matrix)
pca_w$rotation
plot(pca_w$x[,1], pca_w$x[,2], pch = 16)
pca_w_df = as.data.frame(pca_w$x)

ggplot(data = pca_w_df, aes(x = PC1, y = PC2)) + 
  xlab("PC1") +
  ylab("PC2") +
  geom_hline(yintercept = 0, colour = "black") + 
  geom_vline(xintercept = 0, colour = "black") + 
  theme_classic() +
  geom_point(color = "tomato")

# create new speciesTraits dataframe unique to i and w (so that they have the same number of rows)

speciesTraits_i = data.frame(species = c("Bald Eagle", "Broad-winged Hawk", "Common Black Hawk", "Cooper's Hawk", "Golden Eagle", "Great Horned Owl", "Northern Harrier", "Mississippi Kite", "Northern Goshawk", "Red-tailed Hawk", "Rough-legged Hawk", "Sharp-shinned Hawk", "Short-eared Owl", "Swainson's Hawk", "Barred Owl", "Boreal Owl", "Burrowing Owl", "Eastern Screech-Owl", "Great Gray Owl", "Long-eared Owl", "Northern Hawk Owl", "Northern Pygmy-Owl", "Northern Saw-whet Owl", "Spotted Owl", "Western Screech-Owl"), activity = 'diurnal')
speciesTraits_i$activity = as.character(speciesTraits_i$activity)
speciesTraits_i$activity[speciesTraits_i$species %in% owls] = 'nocturnal'
speciesTraits_i$actColor = 'blue'
speciesTraits_i$actColor[speciesTraits_i$activity == 'nocturnal'] = 'red'
speciesTraits_i = speciesTraits_i[-c("mite"), ]

speciesTraits_w = data.frame(species = c("Bald Eagle", "Broad-winged Hawk", "Cooper's Hawk", "Golden Eagle", "Great Horned Owl", "Northern Harrier", "Northern Goshawk", "Red-tailed Hawk", "White-tailed Hawk", "Short-eared Owl", "Swainson's Hawk", "White-tailed Eagle", "Flammulated Owl", "Great Gray Owl", "Long-eared Owl", "Northern Hawk Owl", "Western Screech-Owl"), activity = 'diurnal')
speciesTraits_w$activity = as.character(speciesTraits_w$activity)
speciesTraits_w$activity[speciesTraits_w$species %in% owls] = 'nocturnal'
speciesTraits_w$actColor = 'blue'
speciesTraits_w$actColor[speciesTraits_w$activity == 'nocturnal'] = 'red'

# create speciesSize data frame for i and w

speciesSize_i = data.frame(species = c("Bald Eagle", "Broad-winged Hawk", "Common Black Hawk", "Cooper's Hawk", "Golden Eagle", "Great Horned Owl", "Northern Harrier", "Mississippi Kite", "Northern Goshawk", "Red-tailed Hawk", "Rough-legged Hawk", "Sharp-shinned Hawk", "Short-eared Owl", "Swainson's Hawk", "Barred Owl", "Boreal Owl", "Burrowing Owl", "Eastern Screech-Owl", "Great Gray Owl", "Long-eared Owl", "Northern Hawk Owl", "Northern Pygmy-Owl", "Northern Saw-whet Owl", "Spotted Owl", "Western Screech-Owl"))
speciesSize_i$size = c('>800g', '<800g', '<800g', '<800g', '>800g', '>800g', '<800g', '<800g', '>800g', '>800g', '>800g', '<800g', '<800g', '>800g', '<800g', '<800g', '<800g', '<800g', '>800g', '<800g', '<800g', '<800g', '<800g', '<800g', '<800g')
speciesSize_i$size = as.character(speciesSize_i$size)
speciesSize_i$sizeColor = 'purple'
speciesSize_i$sizeColor[speciesSize_i$size == '<800g'] = 'green'

speciesSize_w = data.frame(species = c("Bald Eagle", "Broad-winged Hawk", "Cooper's Hawk", "Golden Eagle", "Great Horned Owl", "Northern Harrier", "Northern Goshawk", "Red-tailed Hawk", "White-tailed Hawk", "Short-eared Owl", "Swainson's Hawk", "White-tailed Eagle", "Flammulated Owl", "Great Gray Owl", "Long-eared Owl", "Northern Hawk Owl", "Western Screech-Owl"))
speciesSize_w$size = c('>800g', '<800g', '<800g', '>800g', '>800g', '<800g', '>800g', '>800g', '>800g', '<800g', '>800g', '>800g', '<800g', '>800g', '<800g', '<800g', '<800g')
speciesSize_w$size = as.character(speciesSize_w$size)
speciesSize_w$sizeColor = 'purple'
speciesSize_w$sizeColor[speciesSize_w$size == '<800g'] = 'green'

# ggbiplot of pca_i and pca_w colored by activity
  # need to remove columns where the value is "0" in final_i_matrix or ggbiplot will not run
final_i_matrix$`Collembola larva` = NULL
final_i_matrix$`Diptera larva` = NULL
final_i_matrix$`Thysanoptera larva` = NULL
final_i_matrix$`Unid. Arachnida larva` = NULL
final_i_matrix$`Unid. Chilopoda larva` = NULL
final_i_matrix$`Unid. Diplopoda larva` = NULL
  # re-run pca_i after removing columns of value "0"
pca_i = prcomp(final_i_matrix)

# identify which prey species are significant to keep as arrows
sig_sp_i = subset(pca_i$rotation, abs(pca_i$rotation[,1]) >= 0.1 | abs(pca_i$rotation[,2]) >=0.1)
sig_sp_w = subset(pca_w$rotation, abs(pca_w$rotation[,1]) >= 0.1 | abs(pca_w$rotation[,2]) >=0.1)

#ggbiplot for i colored by activity
plot_i_activity = ggbiplot(pca_i, groups = speciesTraits_i$activity, varname.size = 0) +
  theme_classic() + theme(legend.title=element_blank()) + xlim(-3, 3) + ylim(-3, 3)

  #get ggplot2 object
plot_i_activity = ggplot_build(plot_i_activity)

  #remove arrows for species that aren't significant
plot_i_activity$data[[1]] <- plot_i_activity$data[[1]][c(4, 12, 17, 26, 31:32, 40, 54), ]
plot_i_activity$data[[3]] <- plot_i_activity$data[[3]][c(4, 12, 17, 26, 31:32, 40, 54), ]

  #re-plot with significant arrows
plot(ggplot_gtable(plot_i_activity))

#ggbiplot for w colored by activity
plot_w_activity = ggbiplot(pca_w, groups = speciesTraits_w$activity, varname.size = 0) + 
  theme_classic() + theme(legend.title=element_blank()) + xlim(-3, 3) + ylim(-3, 3)

  #get ggplot2 object
plot_w_activity = ggplot_build(plot_w_activity)

  #remove arrows for species that aren't significant
plot_w_activity$data[[1]] <- plot_w_activity$data[[1]][c(1, 3, 16, 19, 25), ]
plot_w_activity$data[[3]] <- plot_w_activity$data[[3]][c(1, 3, 16, 19, 25), ]

  #re-plot with significant arrows
plot(ggplot_gtable(plot_w_activity))




# ggbiplot of pca_i and pca_w colored by size

#ggbiplot for i colored by size
plot_i_size = ggbiplot(pca_i, groups = speciesSize_i$size, varname.size = 0) + 
  scale_color_manual(name="", values = c("purple", "darkgreen")) +
  theme_classic() + theme(legend.title=element_blank()) + xlim(-3, 3) + ylim(-3, 3)

  #get ggplot2 object
plot_i_size = ggplot_build(plot_i_size)

  #remove arrows for species that aren't significant
plot_i_size$data[[1]] <- plot_i_size$data[[1]][c(4, 12, 17, 26, 31:32, 40, 54), ]
plot_i_size$data[[3]] <- plot_i_size$data[[3]][c(4, 12, 17, 26, 31:32, 40, 54), ]

  #re-plot with significant arrows
plot(ggplot_gtable(plot_i_size))

# ggbiplot of pca_w colored by size
plot_w_size = ggbiplot(pca_w, groups = speciesSize_w$size, varname.size = 0) + 
  scale_color_manual(name="", values=c("purple", "darkgreen")) + 
  theme_classic() + theme(legend.title=element_blank()) + xlim(-3, 3) + ylim(-3, 3)

  #get ggplot2 object
plot_w_size = ggplot_build(plot_w_size)

  #remove arrows for species that aren't significant
plot_w_size$data[[1]] <- plot_w_size$data[[1]][c(1, 3, 16, 19, 25), ]
plot_w_size$data[[3]] <- plot_w_size$data[[3]][c(1, 3, 16, 19, 25), ]

  #re-plot with significant arrows
plot(ggplot_gtable(plot_w_size))

# multi-regression model data
math_data_i = read.csv("395Mathematical_Model_Data_i.csv", header = TRUE)
math_data_w = read.csv("395Mathematical_Model_Data_w.csv", header = TRUE)

# models for i
lm_pc1_i <- lm(pc1 ~ f_mass + f_wing + f_tarsus + wing_tarsus, data = math_data_i)
summary(lm_pc1_i)
lm_pc2_i <- lm(pc2 ~ f_mass + f_wing + f_tarsus + wing_tarsus, data = math_data_i)
summary(lm_pc2_i)

# models for w
lm_pc1_w <- lm(pc1 ~ f_mass + f_wing + f_tarsus + wing_tarsus, data = math_data_w)
summary(lm_pc1_w)
lm_pc2_w <- lm(pc2 ~ f_mass + f_wing + f_tarsus + wing_tarsus, data = math_data_w)
summary(lm_pc2_w)

