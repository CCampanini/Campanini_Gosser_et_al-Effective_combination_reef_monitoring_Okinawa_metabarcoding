library(vegan)     # For calculating Jaccard distance and db-RDA
library(ggplot2)   # For data visualization
library(readxl)
library(dplyr)
library(reshape2)
#load read abundance data and environmental variable data

Fish <- read_excel("E:/PostDoc Okinawa/eDNA Okinawa/Analysis/Matrix_fish.xlsx")
FishVis <- Fish[4:141]
row.names(FishVis) <- Fish$Site

Okinawa.env <- read_excel("E:/PostDoc Okinawa/eDNA Okinawa/Analysis/Okinawa_ESV_fish_metadata_rda.xlsx")
row.names(Okinawa.env) <- Okinawa.env$Samples
Fish.env <- Okinawa.env[Okinawa.env$source=="Vis",]

Fish.env$Location <- as.factor(Fish.env$Location)
Fish.env$depth <- as.factor(Fish.env$depth)
Fish.env$impact <- as.factor(Fish.env$impact)



jaccard_dist_fish <- vegdist(FishVis, method = "jaccard", binary = T)

jaccard_dist_fish

db_rda_fish <- capscale(jaccard_dist_fish ~ Location + impact + depth, data = Fish.env)

vif.cca(db_rda_fish)

anova(db_rda_fish)
anova(db_rda_fish, by = "terms")

# Perform PERMANOVA
permanova_result <- adonis2(jaccard_dist_fish ~ Location + impact + depth, data = Fish.env, permutations = 999)

# View the results
permanova_result


smry <- summary(db_rda_fish)
scrs <- scores(db_rda_fish)
df1  <- data.frame(smry$sites[,1:2]) # site scores for RDA1 and RDA2
df1$site <- rownames(df1)  #add site names
df1$Location <- Fish.env$Location
df1$impact <- Fish.env$impact
df1$depth <- Fish.env$depth
df2  <- data.frame(smry$biplot[,1:2])  # mapping environmental variables

percfs <- round(100*(summary(db_rda_fish)$cont$importance[2, 1:2]), 2)

rda.plotfs <- ggplot(df1, aes(x = CAP1, y = CAP2)) +
  geom_point(size = 3, aes(shape = depth, colour = Location)) +
  stat_ellipse(geom = "polygon", aes(fill = impact), alpha = 0.1) +
  scale_color_manual(values = c("#72d5de", "#e9b198", "#7cccee", "#dcd2a0", "#96b5ee", "#afc38a", "#e6afd3", "#8fc7a0", "#babdea", "#c8eebe", "#87c4b8", "#99e9d5", "#72d5de", "#e9b198", "#8fc7a0")) +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  #xlim(-2, 2) +
  #ylim(-2, 2) +
  labs(x = paste("RDA1 (", percfs[1], "%)"), y = paste("RDA2 (", percfs[2], "%)")) +
  coord_fixed()

rda.plotfs

genera <- unique(sub("^(\\w+).*", "\\1", colnames(FishVis)))
columns_to_merge <- grepl(paste(genera, collapse = "|"), colnames(FishVis))

# Create new columns for each group
for (prefix in genera) {
  group_columns <- grepl(paste0("^", prefix), colnames(FishVis))
  FishVis[, paste0(prefix)] <- rowSums(FishVis[, group_columns, drop = TRUE])
}

# Remove the original columns
FishVis_genera <- FishVis[139:205]

# Display the modified matrix
print(FishVis_genera)

jaccard_dist_fish_genera <- vegdist(FishVis, method = "jaccard", binary = T)

jaccard_dist_fish_genera

db_rda_fish_genera <- capscale(jaccard_dist_fish_genera ~ Location + impact + depth, data = Fish.env)

vif.cca(db_rda_fish_genera)

anova(db_rda_fish_genera)
anova(db_rda_fish_genera, by = "terms")

smry2 <- summary(db_rda_fish_genera)
scrs2 <- scores(db_rda_fish_genera)
df3  <- data.frame(smry2$sites[,1:2]) # site scores for RDA1 and RDA2
df3$site <- rownames(df3)  #add site names
df3$Location <- Fish.env$Location
df3$impact <- Fish.env$impact
df3$depth <- Fish.env$depth
df4  <- data.frame(smry2$biplot[,1:2])  # mapping environmental variables

percfg <- round(100*(summary(db_rda_fish_genera)$cont$importance[2, 1:2]), 2)

rda.plot.genera <- ggplot(df3, aes(x = CAP1, y = CAP2)) +
  geom_point(size = 3, aes(shape = depth, colour = Location)) +
  stat_ellipse(geom = "polygon", aes(fill = impact), alpha = 0.1) +
  scale_color_manual(values = c("#72d5de", "#e9b198", "#7cccee", "#dcd2a0", "#96b5ee", "#afc38a", "#e6afd3", "#8fc7a0", "#babdea", "#c8eebe", "#87c4b8", "#99e9d5", "#72d5de", "#e9b198", "#8fc7a0")) +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  #xlim(-2, 2) +
  #ylim(-2, 2) +
  labs(x = paste("RDA1 (", percfg[1], "%)"), y = paste("RDA2 (", percfg[2], "%)")) +
  coord_fixed()
rda.plot.genera

genera

#load read abundance data and environmental variable data

Fish2 <- read_excel("E:/PostDoc Okinawa/eDNA Okinawa/Analysis/Fish_eDNA.xlsx")
FisheDNA <- as.data.frame(t(Fish2[2:68]))
colnames(FisheDNA) <- Fish2$Species

Okinawa.env <- read_excel("E:/PostDoc Okinawa/eDNA Okinawa/Analysis/Okinawa_ESV_fish_metadata_rda.xlsx")
row.names(Okinawa.env) <- Okinawa.env$Samples
FisheDNA.env <- Okinawa.env[Okinawa.env$source=="eDNA",]

FisheDNA.env$Location <- as.factor(FisheDNA.env$Location)
FisheDNA.env$depth <- as.factor(FisheDNA.env$depth)
FisheDNA.env$impact <- as.factor(FisheDNA.env$impact)

# Extract the genera names from column titles
generaeDNA <- unique(sub("^(\\w+).*", "\\1", colnames(FisheDNA)))
generaeDNA

# Iterate over the prefixes
for (prefix in generaeDNA) {
  # Find columns that match the prefix
  group_columns_eDNA <- grepl(paste0("^", prefix), colnames(FisheDNA), ignore.case = TRUE)
  
  # Create a new column with row sums
  FisheDNA[prefix] <- rowSums(FisheDNA[, group_columns_eDNA, drop = FALSE])}

# Drop the original columns
FishgeneraeDNA <- FisheDNA[76:115]
# remove empty rows
# Calculate row sums
row_sums <- rowSums(FishgeneraeDNA)
zero_sum_rows <- which(row_sums == 0)
FishgeneraeDNA_filtered <- FishgeneraeDNA[-zero_sum_rows, ]

# Remove corresponding rows from the environmental data
env_data_filtered <- FisheDNA.env[-zero_sum_rows, ]


View(FishgeneraeDNA_filtered)

jaccard_dist_generaeDNA <- vegdist(FishgeneraeDNA_filtered, method = "jaccard", binary = T)

jaccard_dist_generaeDNA

db_rda_generaeDNA <- capscale(jaccard_dist_generaeDNA ~ Location + impact + depth, data = env_data_filtered)

vif.cca(db_rda_generaeDNA)

anova(db_rda_generaeDNA)
anova(db_rda_generaeDNA, by = "terms")

smry_FisheDNA_genera <- summary(db_rda_generaeDNA)
scrs_FisheDNA_genera <- scores(db_rda_generaeDNA)
df_FisheDNA_genera  <- data.frame(smry_FisheDNA_genera$sites[,1:2]) # site scores for RDA1 and RDA2
df_FisheDNA_genera$site <- rownames(df_FisheDNA_genera)  #add site names
df_FisheDNA_genera$Location <- env_data_filtered$Location
df_FisheDNA_genera$impact <- env_data_filtered$impact
df_FisheDNA_genera$depth <- env_data_filtered$depth
df_FisheDNA_genera2  <- data.frame(smry_FisheDNA_genera$biplot[,1:2])  # mapping environmental variables

perceDNA <- round(100*(summary(db_rda_generaeDNA)$cont$importance[2, 1:2]), 2)

rda.plot_FisheDNA_genera <- ggplot(df_FisheDNA_genera, aes(x = CAP1, y = CAP2)) +
  geom_point(size = 3, aes(shape = depth, colour = Location)) +
  stat_ellipse(geom = "polygon", aes(fill = impact), alpha = 0.1) +
  scale_color_manual(values = c("#72d5de", "#e9b198", "#7cccee", "#dcd2a0", "#96b5ee", "#afc38a", "#e6afd3", "#8fc7a0", "#babdea", "#c8eebe", "#87c4b8", "#99e9d5", "#72d5de", "#e9b198", "#8fc7a0")) +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  #xlim(-2, 2) +
  #ylim(-2, 2) +
  labs(x = paste("RDA1 (", perceDNA[1], "%)"), y = paste("RDA2 (", perceDNA[2], "%)")) +
  coord_fixed()
rda.plot_FisheDNA_genera

# Print the modified dataset
print(FisheDNA)

# Full join of Genera matrices
FishgeneraeDNA_filtered$name <- env_data_filtered$Samples
FishVis_genera$name <- Fish.env$Samples
joined_FG <- full_join(FishVis_genera, FishgeneraeDNA_filtered)
joined_FG[is.na(joined_FG)] <- 0
View(joined_FG)
row.names(joined_FG) <- joined_FG$name
joined_FG$name <- NULL

write.csv(joined_FG, file = "output.csv", row.names = TRUE)

fishtotal.env <- rbind(env_data_filtered, Fish.env)
Fishtotal.env$source <- as.factor(Fishtotal.env$source)

jaccard_dist_total <- vegdist(joined_FG, method = "jaccard", binary = T)

jaccard_dist_total

db_rda_total <- capscale(jaccard_dist_total ~ Location + source + depth, data = fishtotal.env)

vif.cca(db_rda_total)

anova(db_rda_total)
anova(db_rda_total, by = "terms")

smry_Fish_total <- summary(db_rda_total)
scrs_Fish_total <- scores(db_rda_total)
df_Fish_total  <- data.frame(smry_Fish_total$sites[,1:2]) # site scores for RDA1 and RDA2
df_Fish_total$site <- rownames(df_Fish_total)  #add site names
df_Fish_total$Location <- fishtotal.env$Location
df_Fish_total$source <- fishtotal.env$source
df_Fish_total$depth <- fishtotal.env$depth
df_Fish_total2  <- data.frame(smry_Fish_total$biplot[,1:2])  # mapping environmental variables

perctotal <- round(100*(summary(db_rda_total)$cont$importance[2, 1:2]), 2)

rda.plot_Fish_total <- ggplot(df_Fish_total, aes(x = CAP1, y = CAP2)) +
  geom_point(size = 3, aes(shape = depth, colour = Location)) +
  stat_ellipse(geom = "polygon", aes(fill = source), alpha = 0.1) +
  scale_color_manual(values = c("#72d5de", "#e9b198", "#7cccee", "#dcd2a0", "#96b5ee", "#afc38a", "#e6afd3", "#8fc7a0", "#babdea", "#c8eebe", "#87c4b8", "#99e9d5", "#72d5de", "#e9b198", "#8fc7a0")) +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  xlim(-2, 2) +
  ylim(-3.5, 4) +
  labs(x = paste("RDA1 (", perctotal[1], "%)"), y = paste("RDA2 (", perctotal[2], "%)")) +
  coord_fixed()
rda.plot_Fish_total


#Condense both tables into one for the locations

joined_FG$name <- fishtotal.env$Location
BFish <- as.data.frame(joined_FG)
BothFish <- aggregate(. ~ name, data = BFish, sum)
BothFish$name <- NULL

jaccard_dist_both <- vegdist(BothFish, method = "jaccard", binary = T)

jaccard_dist_both

db_rda_both <- capscale(jaccard_dist_both ~ Location + impact, data = fishtotal_env)

vif.cca(db_rda_both)

anova(db_rda_both)
anova(db_rda_both, by = "terms")

smry_Fish_both <- summary(db_rda_both)
scrs_Fish_both <- scores(db_rda_both)
df_Fish_both  <- data.frame(smry_Fish_both$sites[,1:2]) # site scores for RDA1 and RDA2
df_Fish_both$site <- rownames(df_Fish_both)  #add site names
df_Fish_both$Location <- fishtotal_env$Location
df_Fish_both$impact <- fishtotal_env$impact
df_Fish_both2  <- data.frame(smry_Fish_both$biplot[,1:2])  # mapping environmental variables

percboth <- round(100*(summary(db_rda_both)$cont$importance[2, 1:2]), 2)

rda.plot_Fish_both <- ggplot(df_Fish_both, aes(x = CAP1, y = CAP2)) +
  geom_point(size = 3, aes(shape = impact, colour = Location)) +
  #stat_ellipse(geom = "polygon", aes(fill = impact), alpha = 0.1) +
  scale_color_manual(values = c("#72d5de", "#e9b198", "#7cccee", "#dcd2a0", "#96b5ee", "#afc38a", "#e6afd3", "#8fc7a0", "#babdea", "#c8eebe", "#87c4b8", "#99e9d5", "#72d5de", "#e9b198", "#8fc7a0")) +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  #xlim(-2, 2) +
  #ylim(-3.5, 4) +
  labs(x = paste("RDA1 (", percboth[1], "%)"), y = paste("RDA2 (", percboth[2], "%)")) +
  coord_fixed()
rda.plot_Fish_both
