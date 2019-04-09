diet_database = read.table("aviandietdatabase.txt",header=TRUE, sep = '\t', quote = '\"',
                           fill=T, stringsAsFactors = F)
#setwd("/Users/jbcook94/Avian Diet Database")

Eagles = read.csv("BIOL395BaldEagle.csv", header = T)
Owls = read.csv("BIOL395GreatHornedOwl.csv", header = T)
BarnOwls = read.csv("BarnOwls.csv", header = T)

hist(Eagles$Observation_Year_Begin)
hist(Owls$Observation_Year_Begin)

diet_sub = subset(diet_database, Common_Name == "Bald Eagle" & Location_Region == "Mexico")
diet_eagle = subset(diet_database, Common_Name == "Bald Eagle")
diet_owl = subset(diet_database, Common_Name == "Great Horned Owl")


library(ggplot2)

#Eagles
E1930 <- Eagles[Eagles$Observation_Year_Begin > 1930 & Eagles$Observation_Year_Begin <= 1939,]
E1960 <- Eagles[Eagles$Observation_Year_Begin > 1960 & Eagles$Observation_Year_Begin <= 1969,]
E1970 <- Eagles[Eagles$Observation_Year_Begin > 1970 & Eagles$Observation_Year_Begin <= 1979,]
E1980 <- Eagles[Eagles$Observation_Year_Begin > 1980 & Eagles$Observation_Year_Begin <= 1989,]
E1990 <- Eagles[Eagles$Observation_Year_Begin > 1990 & Eagles$Observation_Year_Begin <= 1999,]
E2000 <- Eagles[Eagles$Observation_Year_Begin > 2000 & Eagles$Observation_Year_Begin <= 2009,]
E1930$dec <- "1930-1939"
E1960$dec <- "1960-1969"
E1970$dec <- "1970-1979"
E1980$dec <- "1980-1989"
E1990$dec <- "1990-1999"
E2000$dec <- "2000-2009"
Eagles_by_decade = rbind(E1930, E1960, E1970, E1980, E1990, E2000)
plot(E1960$Prey_Order)
plot(E1970$Prey_Order)
plot(E1980$Prey_Order)

ggplot(aes(x = Observation_Year_Begin, y = Prey_Class), data= Eagles_by_decade) + 
  facet_wrap(~Eagles_by_decade$dec) + geom_line()

EaglesArizona <- subset(Eagles_by_decade, Location_Region == "Arizona")
EaglesWashington <- subset(Eagles_by_decade, Location_Region =="Washington")

#Owls
O1960 <- Owls[Owls$Observation_Year_Begin > 1960 & Owls$Observation_Year_Begin <= 1969,]
O1970 <- Owls[Owls$Observation_Year_Begin > 1970 & Owls$Observation_Year_Begin <= 1979,]
O1980 <- Owls[Owls$Observation_Year_Begin > 1980 & Owls$Observation_Year_Begin <= 1989,]
O1960$dec <- "1960-1969"
O1970$dec <- "1970-1979"
O1980$dec <- "1980-1989"
Owls_by_decade = rbind(O1960, O1970, O1980)
plot(O1960$Prey_Order)
plot(O1970$Prey_Order)
plot(O1980$Prey_Order)

ggplot(aes(x = Observation_Year_Begin, y = Prey_Order), data= Owls_by_decade) + 
  facet_wrap(~Owls_by_decade$dec) + geom_line()

#Barn Owls
BO1960 <- BarnOwls[BarnOwls$Observation_Year_Begin > 1960 & BarnOwls$Observation_Year_Begin <= 1969,]
BO1970 <- BarnOwls[BarnOwls$Observation_Year_Begin > 1970 & BarnOwls$Observation_Year_Begin <= 1979,]
BO1980 <- BarnOwls[BarnOwls$Observation_Year_Begin > 1980 & BarnOwls$Observation_Year_Begin <= 1989,]
BO1960$dec <- "1960-1969"
BO1970$dec <- "1970-1979"
BO1980$dec <- "1980-1989"
BarnOwls_by_decade = rbind(BO1960, BO1970, BO1980)


# dplyr tutorial
eagle_clean = Eagles %>% group_by(Observation_Year_Begin, Prey_Common_Name) %>%
  summarize(prey_sum = sum(Fraction_Diet))

E1930 %>% group_by(Prey_Common_Name) %>%
  summarize(prey_median = median(na.omit(Fraction_Diet)))
#Run filter command as well to control for prey items that are included in most or all of the sites

# group data by prey order median fraction diet and year
Eagles_by_prey_order = Eagles_by_decade %>% 
  group_by(Prey_Order, Observation_Year_Begin) %>% 
  summarize(prey_median = median(Fraction_Diet))
names(Eagles_by_prey_order) = c("Prey_Order", "Observation_Year_Begin", "Fraction_Diet")

#Raw fraction diet per observation year begin

EaglesPCA = Eagles_by_prey_order[,c("Prey_Order", "Fraction_Diet","Observation_Year_Begin")]
EaglesPCA = na.omit(EaglesPCA)
# rownames(EaglesPCA) = EaglesPCA[,1]
eagles.pca <- prcomp(EaglesPCA[,c(2:3)], center = TRUE, scale. = TRUE)
summary(eagles.pca)

# unclass(eagles.pca$x)

# lm(Prey_Order ~ Observation_Year_Begin + Fraction_Diet, data = EaglesPCA)
score = as.data.frame(eagles.pca$x)

ggplot(data = score, aes(x = PC1, y = PC2)) + 
  xlab("Fraction Diet") +
  ylab("Observation Year Begin") +
  geom_hline(yintercept = 0, colour = "black") + 
  geom_vline(xintercept = 0, colour = "black") + 
  theme_classic() +
  geom_point(color = "tomato")


autoplot(eagles.pca$sdev)

#Dr. Hurlbert's R Script

# General description of what the script does
# who wrote it
# date

# Load necessary libraries
library(dplyr)
library(tidyr)

# Source functions for summarizing and working with data
# (assuming your working directory is the dietdatabase repo)
source('scripts/database_summary_functions.R')


#Read in data (need to specify the actual path to this file)
Eagles = read.csv('z:/teaching/395/BIOL395BaldEagle.csv')

# Coarsen the taxonomic resolution of the prey data to Class
dietByClass = reclassifyPrey(Eagles, by = 'Class') %>%
  filter(Diet_Type == "Items") %>%
  select(Location_Region, Location_Specific, Habitat_type, Longitude_dd, Latitude_dd, 
         Observation_Year_Begin, Prey_Class, Frac_Diet, Study_Type, Item_Sample_Size)

# Convert to "wide" matrix format where each possible Prey_Class is its own column
dietMatrix = spread(dietByClass, Prey_Class, Frac_Diet)

dietMatrix = subset(dietMatrix, is.na(dietMatrix$Observation_Year_Begin) == FALSE)

# How many times does each prey Class category appear?
classCount = apply(dietMatrix[, 9:27], 2, function(x) sum(!is.na(x)))

# Restrict to diet Classes that were found in at least two studies
dietMat = dietMatrix[, names(classCount)[classCount >= 2]]

# Replace NAs with 0s
dietMat[is.na(dietMat)] = 0

# Now we're ready for the PCA!
pca = prcomp(dietMat)

# The principal component axes reflect different linear combinations of the
# different Prey Classes (e.g., PC1 might mean "more Aves and less Mammalia").
# The first few axes should explain most of the variation in diet composition
# between samples (rows in the dietMat).

# The PC values for each sample are given in pca$x

# Plot PC1 vs PC2 -- each data point is a study
plot(pca$x[,1], pca$x[,2], pch = 16)

# Now you can play with color coding these points by region or year/decade, etc.

# You will also want to interpret what PC1 and PC2 actually mean, which is given
# in pca$rotation

# For example

#               PC1
# Aves        -0.875
# Bivalvia    -0.010
# Mammalia    -0.163
# Teleostei    0.452

# implies that samples that fall to the right along the PC1 axis have greater representation by
# fish and less representation by birds, etc.

ggplot(data = score, aes(x = PC1, y = PC2)) + 
  xlab("Fraction Diet") +
  ylab("Observation Year Begin") +
  geom_hline(yintercept = 0, colour = "black") + 
  geom_vline(xintercept = 0, colour = "black") + 
  theme_classic() +
  geom_point(color = "tomato")

par(mar = rep(2, 4))
plot(pca)

pca_plot = cbind(pca$x, dietMatrix[,1:8])
rownames(pca_plot) <- pca_plot$Item_Sample_Size
ggbiplot(pca, groups = New_Regions$new, size = 2) + theme_classic()

pca_plot$new <- ""
sub1 = pca_plot[pca_plot$Location_Region == c("Alaska") | pca_plot$Location_Region == c("Canada") | pca_plot$Location_Region == c("Nebraska") | pca_plot$Location_Region == c("Oregon") | pca_plot$Location_Region == c("Washington") | pca_plot$Location_Region == c("Wyoming; Idaho; Montana") ,] 
sub1$new = "NorthWest"
sub2 = pca_plot[pca_plot$Location_Region == c("Arizona") | pca_plot$Location_Region == c("California") | pca_plot$Location_Region == c("Colorado") | pca_plot$Location_Region == c("Mexico") ,]
sub2$new = "SouthWest"
sub3 = pca_plot[pca_plot$Location_Region == c("Maine") | pca_plot$Location_Region == c("Minnesota") | pca_plot$Location_Region == c("Missouri") | pca_plot$Location_Region == c("New York") | pca_plot$Location_Region == c("Nova Scotia") | pca_plot$Location_Region == c("Wisconsin") ,]
sub3$new = "NorthEast"
sub4 = pca_plot[pca_plot$Location_Region == c("Chesapeake Bay") | pca_plot$Location_Region == c("Florida") | pca_plot$Location_Region == c("North America") | pca_plot$Location_Region == c("South Carolina") ,]
sub4$new = "SouthEast"
New_Regions = rbind(sub1, sub2, sub3, sub4)

ggbiplot(pca, groups = pca_by_decade$dec) + theme_classic()

pca_plot = subset(pca_plot, is.na(pca_plot$Observation_Year_Begin) == FALSE)

pca_plot$dec <- ""
pca1930 <- pca_plot[pca_plot$Observation_Year_Begin >= 1930 & pca_plot$Observation_Year_Begin <= 1939,]
pca1940 <- pca_plot[pca_plot$Observation_Year_Begin >= 1940 & pca_plot$Observation_Year_Begin <= 1949,]
pca1960 <- pca_plot[pca_plot$Observation_Year_Begin >= 1960 & pca_plot$Observation_Year_Begin <= 1969,]
pca1970 <- pca_plot[pca_plot$Observation_Year_Begin >= 1970 & pca_plot$Observation_Year_Begin <= 1979,]
pca1980 <- pca_plot[pca_plot$Observation_Year_Begin >= 1980 & pca_plot$Observation_Year_Begin <= 1989,]
pca1990 <- pca_plot[pca_plot$Observation_Year_Begin >= 1990 & pca_plot$Observation_Year_Begin <= 1999,]
pca2000 <- pca_plot[pca_plot$Observation_Year_Begin >= 2000 & pca_plot$Observation_Year_Begin <= 2009,]
pca1930$dec <- "1930-1939"
pca1940$dec <- "1940-1949"
pca1960$dec <- "1960-1969"
pca1970$dec <- "1970-1979"
pca1980$dec <- "1980-1989"
pca1990$dec <- "1990-1999"
pca2000$dec <- "2000-2009"
pca_by_decade = rbind(pca1930, pca1940,pca1960, pca1970, pca1980, pca1990, pca2000)

#Make histogram of prey class by region and by time