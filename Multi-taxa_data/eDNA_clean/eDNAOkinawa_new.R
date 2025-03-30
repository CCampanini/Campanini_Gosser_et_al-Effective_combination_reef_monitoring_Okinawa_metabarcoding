library(phyloseq)
library(ggtree)
library(tidyverse)
library(ggplot2)
library(vegan)
library(readr)
library(readxl)
library(ggridges)
library(cowplot)
library(viridisLite)
library(circlize)
library(ggrepel)
library(here)
#load read abundance data and environmental variable data

setwd("D:/PostDoc Okinawa/eDNA Okinawa")

#Otu table
Okinawa_Otu <- read_csv(file=here("Multi-taxa_data","eDNA_clean","Okinawa_ESV.csv"))
#Tax table
Okinawa_Tax <- read_csv(file=here("Multi-taxa_data","eDNA_clean","Okinawa_Taxa.csv"))

#environmental data
Okinawa_env <- read_excel(path=here("Multi-taxa_data","eDNA_clean","Okinawa_ESV_taxon_table_NCsub_4excluded_2_85percent_threshold_metadata_rda.xlsx"))

Okinawa_env$Location <- as.factor(Okinawa_env$Location)
Okinawa_env$depth <- as.factor(Okinawa_env$depth)
Okinawa_env$impact <- as.factor(Okinawa_env$impact)
Okinawa_env$pop_score <- as.factor(Okinawa_env$pop_score)
Okinawa_env$freshwater_input <- as.factor(Okinawa_env$freshwater_input)
Okinawa_env$fishing_pressure <- as.factor(Okinawa_env$fishing_pressure)
Okinawa_env$coast_score <- as.factor(Okinawa_env$coast_score)
Okinawa_env$human_score <- as.factor(Okinawa_env$human_score)
Okinawa_env$herma_score <- as.factor(Okinawa_env$herma_score)
Okinawa_env$other_pressures <- as.factor(Okinawa_env$other_pressures)

#Plot for taxonomic ranks
# Count occurrences of each taxonomic rank
all_counts <- Okinawa_Tax %>%
  count(Phylum, Class, Order, Family, Genus, Species, ID) %>%
  summarise(Phylum = sum(!is.na(Phylum)),
            Class = sum(!is.na(Class)),
            Order = sum(!is.na(Order)),
            Family = sum(!is.na(Family)),
            Genus = sum(!is.na(Genus)),
            Species = sum(!is.na(Species)),
            ESVs = sum(!is.na(ID)))

# Reshape the data for plotting
all_counts_long <- all_counts %>%
  pivot_longer(cols = everything(), names_to = "Taxonomic rank", values_to = "Count")

# Define the order of features
feature_order <- c("ESVs", "Phylum", "Class", "Order", "Family", "Genus", "Species")

# Order the bars based on the predefined order
all_counts_long$Feature <- factor(all_counts_long$`Taxonomic rank`, levels = feature_order)

# Count unique features for each column
unique_counts <- Okinawa_Tax %>%
  summarise(Phylum = n_distinct(Phylum),
            Class = n_distinct(Class),
            Order = n_distinct(Order),
            Family = n_distinct(Family),
            Genus = n_distinct(Genus),
            Species = n_distinct(Species), 
            ESVs = n_distinct(ID))


# Reshape the data for plotting
unique_counts_long <- unique_counts %>%
  pivot_longer(cols = everything(), names_to = "Taxonomic rank", values_to = "Count")
# Order the bars based on the predefined order

unique_counts_long$Feature <- factor(unique_counts_long$`Taxonomic rank`, levels = feature_order)

# Combine total and unique counts
combined_counts <- inner_join(all_counts_long, unique_counts_long, by = "Taxonomic rank")
# Reorder the levels of the Taxonomic rank factor
combined_counts$`Taxonomic rank` <- factor(combined_counts$`Taxonomic rank`, levels = feature_order)

# Combine counts into a single data frame
labels_df <- combined_counts %>%
  mutate(label = paste(Count.x, Count.y, sep = "/")) %>%
  select(`Taxonomic rank`, label)

# Create a stacked bar chart
ggplot(combined_counts, aes(x = `Taxonomic rank`)) +
     geom_bar(aes(y = Count.x, fill = "Total Counts"), stat = "identity", alpha = 0.5) +
     geom_bar(aes(y = Count.y, fill = "Unique Counts"), stat = "identity") +
     geom_text(aes(y = Count.x, label = Count.x), vjust = -0.5, size = 4, fontface = "bold") +
     geom_text(aes(y = Count.y, label = Count.y), vjust = -0.5, size = 4, fontface = "bold") +
     scale_fill_manual(values = c("Total Counts" = "#19a483", "Unique Counts" = "#19a483")) +
     labs(x = "Taxonomic rank", y = "Count") +
     theme_classic()

######
#Now starting with further analysis with Phyloseq
######
esv_mat <- as.matrix(column_to_rownames(Okinawa_Otu, "ID"))
tax_mat <- as.matrix(column_to_rownames(Okinawa_Tax, "ID"))
samples_df <- column_to_rownames(Okinawa_env, "Samples")

ESV = otu_table(esv_mat, taxa_are_rows = TRUE)
TAX = tax_table(tax_mat)
samples = sample_data(samples_df)

Ok <- phyloseq(ESV, TAX, samples)
metadata <- data.frame(sample_data(Ok))

#just to check it worked
Ok
sample_names(Ok)
sample_variables(Ok)
head(sample_data(Ok))

#cleaning up the taxonomy table to fix empty cells
tax <- data.frame(tax_table(Ok))

tax.clean <- data.frame(row.names = row.names(tax),
                        Phylum = str_replace(tax[,1], "D_1__",""),
                        Class = str_replace(tax[,2], "D_2__",""),
                        Order = str_replace(tax[,3], "D_3__",""),
                        Family = str_replace(tax[,4], "D_4__",""),
                        Genus = str_replace(tax[,5], "D_5__",""),
                        Species = str_replace(tax[,6], "D_6__",""),
                        stringsAsFactors = FALSE)
tax.clean[is.na(tax.clean)] <- ""

for (i in 1:6){ tax.clean[,i] <- as.character(tax.clean[,i])}
tax.clean[is.na(tax.clean)] <- ""
for (i in 1:nrow(tax.clean)){if (tax.clean[i,2] == ""){
  phylum <- paste("Phylum_", tax.clean[i,1], sep = "")
  tax.clean[i, 2:6] <- phylum
} else if (tax.clean[i,3] == ""){
  class <- paste("Class_", tax.clean[i,2], sep = "")
  tax.clean[i, 3:6] <- class
} else if (tax.clean[i,4] == ""){
  order <- paste("Order_", tax.clean[i,3], sep = "")
  tax.clean[i, 4:6] <- order
} else if (tax.clean[i,5] == ""){
  family <- paste("Family_", tax.clean[i,4], sep = "")
  tax.clean[i, 5:6] <- family
} else if (tax.clean[i,6] == ""){
  tax.clean$Species[i] <- paste("Genus",tax.clean$Genus[i], sep = "_")
}
}

tax_table(Ok) <- as.matrix(tax.clean)
View(tax_table(Ok))
#read abundance of the samples
plot_bar(Ok)

Ok %>% 
  psmelt() %>% 
  group_by(Sample) %>% 
  mutate(Proportion = Abundance / sum(Abundance, na.rm = TRUE)) %>% 
  filter(Proportion > 0) %>% 
  filter(!is.na(Phylum)) %>% 
  ggplot(aes(y = Phylum, x = log10(Proportion), fill = impact)) +
  scale_fill_manual(values = c(medium = "#FFB331",high = "#9F1111", low = "#009F81")) +
  ggridges::geom_density_ridges2(scale = 1, alpha = 0.5, show.legend = FALSE) +
  ggtitle("Compare distribution of relative abundances") +
  theme_classic()

#bar chart relative abundances
Okrel = transform_sample_counts(Ok, function(x) x / sum(x))
Ok2 <- psmelt(Okrel)
ggplot(Ok2, aes(x = Sample, y = Abundance, fill = Phylum)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +  # Using viridis color palette
  labs(x = "Sample", y = "Abundance", fill = "Phylum") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#bar chart esvs per phyla
# Summarize the number of unique ESVs per phylum
esv_counts <- Ok2 %>%
  group_by(Phylum) %>%
  summarize(ESV_Count = n_distinct(OTU))

# Create the bar plot
ggplot(esv_counts, aes(x = reorder(Phylum, -ESV_Count), y = ESV_Count, fill = Phylum)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(guide = "none") +  # Viridis palette, hiding legend if many phyla
  labs(x = "Phylum", y = "Count of ESVs", title = "Count of ESVs within Phyla") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Bar chart phyla per sample
# Summarizing the number of unique ESVs per sample and phylum
esv_counts_per_sample <- Ok %>%
  otu_table() %>%
  as.data.frame() %>%
  rownames_to_column(var = "OTU") %>%
  gather(key = "Sample", value = "Abundance", -OTU) %>%
  left_join(tax_table(Ok) %>% as.data.frame() %>% rownames_to_column(var = "OTU")) %>%
  filter(Abundance > 0) %>%
  group_by(Sample, Phylum) %>%
  summarize(ESV_Count = n_distinct(OTU), .groups = "drop")

# Bar chart for ESV counts per sample and phylum
ggplot(esv_counts_per_sample, aes(x = Location, y = ESV_Count, fill = Phylum)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d() +
  labs(x = "Sample", y = "Count of Unique ESVs", fill = "Phylum", 
       title = "Unique ESVs per Sample and Phylum") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Bar chart of ESV counts grouped by impact levels
Ok2 <- psmelt(Ok)  # Melt phyloseq object for plotting

# Count unique ESVs per sample and phylum, grouped by impact
esv_counts_per_sample <- Ok2 %>%
  filter(Abundance > 0) %>%
  group_by(Sample, Location, impact, Phylum) %>%
  summarize(ESV_Count = n_distinct(OTU), .groups = "drop")

# Ensure 'impact' is a factor with levels in the desired order
esv_counts_per_sample$impact <- factor(esv_counts_per_sample$impact, levels = c("low", "medium", "high"))

# Reorder 'Sample' based on 'impact'
esv_counts_per_sample$Location <- reorder(esv_counts_per_sample$Location, as.numeric(esv_counts_per_sample$impact))


# Bar chart with samples sorted by impact
ggplot(esv_counts_per_sample, aes(x = Location, y = ESV_Count, fill = Phylum)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d() +
  labs(y = "Count of Unique ESVs",fill = "Phylum") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Count unique ESVs per sample and phylum, grouped by impact
esv_counts_per_site <- Ok2 %>%
  filter(Abundance > 0) %>%
  group_by(Location, impact, Phylum) %>%
  summarize(ESV_Count = n_distinct(OTU), .groups = "drop")

esv_counts_per_site$Location<-ordered(esv_counts_per_site$Location,levels=c("Hizushi", "Sakubaru1", "Sakubaru2", "CapeZanpa", "Zatsun", "Yonama", "Mizugama","Sukuta", "Sunabe", "Awa", "GinowanPort1", "GinowanPort2"))
# Bar chart with samples sorted by impact
ggplot(esv_counts_per_site, aes(x = Location, y = ESV_Count, fill = Phylum)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d() +
  labs(y = "Count of Unique ESVs",fill = "Phylum") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



# Example: Hierarchical clustering tree based on taxonomic distances
# Read the phylogenetic tree
Ok_tree <- ape::read.tree(file=here("Multi-taxa_data",
                          "eDNA_clean",
                          "eDNAOkinawaTreenew.tre"))

#want to extract the phylum names for labelling
Ok_tree_df <- as_tibble(Ok_tree)
name_list <- c("Annelida","Arthropoda","Bacillariophyta","Bryozoa","Cercozoa","Chaetognatha","Chlorophyta",
                           "Chordata","Cnidaria","Amoebozoa","Echinodermata","Haptophyta","Mollusca","Nemertea",
   "Picozoa","Platyhelminthes","Porifera","Rhodophyta","Ochrophyta","Dinophyceae")

# Filter the dataframe
filtered_Ok_tree_df <- Ok_tree_df[Ok_tree_df$label %in% name_list, ]
#change stuff manually if necessary
#filtered_Ok_tree_df <- edit(filtered_Ok_tree_df)

#plot tree
plot <- ggtree(Ok_tree, branch.length = 'none', layout = "circular") + 
  geom_tiplab(size = 3, aes(angle = angle)) 

#add labels from your label dataframe
plot +  geom_cladelab(data = filtered_Ok_tree_df, mapping = aes(node = node, label = label), 
                      fontsize = 3, angle = 'auto', barsize = 5,
                      offset = 15, offset.text = 1, barcolor = viridis(20))
######
#Alpha diversity
######
# Shannon's H'
H <- diversity(t(otu_table(Ok)))
# Observed Richness
richness <- specnumber(t(otu_table(Ok)))
# Pielou's Evenness
evenness <- H/log(richness)
# Create alpha diversity dataframe including environmental data
alpha <- cbind(shannon = H, richness = richness, pielou = evenness, Okinawa_env)
head(alpha)
alpha$Location<-ordered(alpha$Location,levels=c("Hizushi", "Sakubaru1", "Sakubaru2", "CapeZanpa", "Zatsun", "Yonama", "Mizugama","Sukuta", "Sunabe", "Awa", "GinowanPort1", "GinowanPort2"))
#Plot alpha diversity
# Plot alpha diversity as boxplots based on Location
plot.shan_loc <- ggplot(alpha, aes(x = Location, y = shannon, fill = Location)) +
  geom_boxplot(color = "black", alpha = 0.5) +
  geom_jitter(width = 0.2, height = 0, color = "black", size = 2) +
  scale_fill_manual(values = c("#9F0162","#009F81","#FF5AAF","#00FCCF","#8400CD","#008DF9","#00C2F9","#FFB2FD","#A40122","#E20134","#FF6E3A","#FFC33B")) +
  ylab("Shannon index") +
  xlab("Location") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))


plot.rich_loc <- ggplot(alpha, aes(x = Location, y = richness, fill = Location)) +
  geom_boxplot(color = "black", alpha = 0.5) +
  geom_jitter(width = 0.2, height = 0, color = "black", size = 2) +
  scale_fill_manual(values = c("#9F0162","#009F81","#FF5AAF","#00FCCF","#8400CD","#008DF9","#00C2F9","#FFB2FD","#A40122","#E20134","#FF6E3A","#FFC33B")) +  #https://mk.bcgsc.ca/colorblind/palettes.mhtml#12-color-palette-for-colorbliness
  ylab("Species Richness") +
  xlab("Location") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))

plot.even_loc <- ggplot(alpha, aes(x = Location, y = pielou, fill = Location)) +
  geom_boxplot(color = "black", alpha = 0.5) +
  geom_jitter(width = 0.2, height = 0, color = "black", size = 2) +
  scale_fill_manual(values = c("#9F0162","#009F81","#FF5AAF","#00FCCF","#8400CD","#008DF9","#00C2F9","#FFB2FD","#A40122","#E20134","#FF6E3A","#FFC33B")) +
  ylab("Pielou's Evenness") +
  xlab("Location") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))

# Create a grid of box plots
legend <- get_legend(plot.shan_loc)
plot_grid(plot.shan_loc + theme(legend.position = "none"), plot.rich_loc + theme(legend.position = "none"), plot.even_loc + theme(legend.position = "none"), ncol = 3)      

#test for differences
# Kruskal-Wallis test for all components
kw_richness <- kruskal.test(richness ~ Location, data = alpha)
kw_richness
# Post hoc tests (Dunn test) for pairwise comparisons
library(dunn.test)

posthoc_richness <- dunn.test(alpha$richness, alpha$Location, method = "bonferroni")
# Convert list to dataframe
posthoc_richness_df <- bind_rows(posthoc_richness)
# Print the dataframe
print(posthoc_richness_df)


# Print results
print(alpha$richness)
print(kw_richness)
print(posthoc_richness)

# Extracting relevant values
richness_statistic <- kw_richness[["statistic"]]
richness_p_value <- kw_richness[["p.value"]]

# Creating a data frame
df_div_stat <- data.frame(Value = "Richness",
                          ChiSquare = richness_statistic,
                          P_Value = richness_p_value)

write.csv(df_div_stat, file = "Stat_test_adiversity_Ok.csv")
df_div_stat

#Plot alpha diversity as violinplots based on impact
#make sure the order is right
alpha$impact <- factor(alpha$impact, levels = c("low", "medium", "high"))

violin_rich_pressure <- ggplot(alpha, aes(x = impact, y = richness, 
                                   fill = impact,
                                   color = impact)) +
  geom_violin(alpha = 0.5, trim = F, draw_quantiles = T) +
  geom_jitter(width = 0.2, height = 0, size = 2,  alpha = 0.6) +
  ylab("Species Richness (#ESV)") +
  scale_fill_manual(values = c(medium = "#FFB331",high = "#9F1111", low = "#009F81"))+
  scale_color_manual(values= c(medium = "#FFB331",high = "#9F1111", low = "#009F81"))+
  theme_classic()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15))+
  guides(fill = "none",
         color = "none")


violin_rich_pressure

#based on depth
violin_rich_pressure_depth <- ggplot(alpha, aes(x = depth, y = richness, 
                                         fill = impact,
                                         color = impact)) +
  geom_violin(alpha = 0.5, trim = F, draw_quantiles = F) +
  geom_jitter(width = 0.2, height = 0, size = 2, alpha = 0.6) +
  facet_grid(~ impact)+
  ylab("Species Richness (#ESV)") +
  xlab("Depth within pressure level") +
  scale_x_discrete(label = c("Deep", "Shallow"))+
  scale_fill_manual(values = c(medium = "#FFB331",high = "#9F1111", low = "#009F81")) +
  scale_color_manual(values = c(medium = "#FFB331",high = "#9F1111", low = "#009F81")) +
  scale_alpha_manual(values=c(1,.6))+
  theme_classic()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15))
violin_rich_pressure_depth


# Plot alpha diversity as boxplots based on impact
plot.shan_impact <- ggplot(alpha, aes(x = impact, y = shannon, fill = impact)) +
  geom_boxplot(color = "black", alpha = 0.5) +
  geom_jitter(width = 0.2, height = 0, color = "black", size = 2) +
  scale_fill_manual(values = c(medium = "#FFB331",high = "#9F1111", low = "#009F81")) +
  ylab("Shannon index") +
  xlab("Impact level") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))


plot.rich_impact <- ggplot(alpha, aes(x = impact, y = richness, fill = impact)) +
  geom_boxplot(color = "black", alpha = 0.5) +
  geom_jitter(width = 0.2, height = 0, color = "black", size = 2) +
  scale_fill_manual(values = c(medium = "#FFB331",high = "#9F1111", low = "#009F81")) +
  ylab("Species Richness") +
  xlab("Impact level") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))

plot.even_impact <- ggplot(alpha, aes(x = impact, y = pielou, fill = impact)) +
  geom_boxplot(color = "black", alpha = 0.5) +
  geom_jitter(width = 0.2, height = 0, color = "black", size = 2) +
  scale_fill_manual(values = c(medium = "#FFB331",high = "#9F1111", low = "#009F81")) +
  ylab("Pielou's Evenness") +
  xlab("Impact level") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))

# Create a grid of box plots
legend <- get_legend(plot.shan_impact)
plot_grid(plot.shan_impact + theme(legend.position = "none"), plot.rich_impact + theme(legend.position = "none"), plot.even_impact + theme(legend.position = "none"), ncol = 3)

kw_richness_impact <- kruskal.test(richness ~ impact, data = alpha)

posthoc_richness_impact <- dunn.test(alpha$richness, alpha$impact, method = "bonferroni")
# Convert list to dataframe
posthoc_richness_impact_df <- bind_rows(posthoc_richness_impact)
# Print the dataframe
print(posthoc_richness_impact_df)


# Print results
print("Richness")
print(kw_richness_impact)
print(posthoc_richness_impact)

# Extracting relevant values
richness_statistic_impact <- kw_richness_impact[["statistic"]]
richness_p_value_impact <- kw_richness_impact[["p.value"]]

# Creating a data frame
df_div_stat_impact <- data.frame(Value = "Richness",
                          ChiSquare = richness_statistic_impact,
                          P_Value = richness_p_value_impact)

write.csv(df_div_stat, file = "Stat_test_diverity_Ok_impact.csv")
df_div_stat_impact

#Testing for differences in the combination of depth and imapct
# Create a composite factor
alpha$depth_impact <- interaction(alpha$depth, alpha$impact, sep = "_")
# Test for overall differences
kruskal_test <- kruskal.test(richness ~ depth_impact, data = alpha)
print(kruskal_test)
# Pairwise Dunn test for depth_impact
dunn_result <- dunn.test(alpha$richness, alpha$depth_impact, method = "bonferroni")
dunn_result_df <- bind_rows(dunn_result)
# Print the dataframe
print(dunn_result_df)

#####
# Beta diversity
#####

## Jaccard (presence/absence)
Ok.j <- t(otu_table(Ok))
Ok.jac <- vegdist(Ok.j, method = "jaccard", binary = T)


#Dendrogram
dendro <- hclust(d = Ok.jac, method = "complete")
plot(dendro, hang = -1, main = "Dendrogram", 
     ylab = "Similarity",
     xlab="", sub="")
cofresult <- cophenetic(dendro)
cor(cofresult, Ok.jac)


# NMDS
set.seed(69)
Ok_nmds = metaMDS(t(otu_table(Ok)), 
                  distance = "jac", 
                  k = 3, 
                  maxit = 999, 
                  trymax = 20, 
                  wascores = TRUE, 
                  tidy = TRUE)

stressplot(Ok_nmds)
#Quick overview plot
pl_Ok <- ordiplot(Ok_nmds, type = "none")
points(pl_Ok, "sites", pch=21, col="red", bg="yellow")
text(pl_Ok, "species", col="blue", cex=0.9)

#extract NMDS scores (x and y coordinates)
Ok_data.scores = as.data.frame(scores(Ok_nmds$points))
#add columns to data frame 
Ok_data.scores$depth = Okinawa_env$depth
Ok_data.scores$Location = Okinawa_env$Location
Ok_data.scores$impact = Okinawa_env$impact

stressplot(Ok_nmds)

Ok_data.scores <- Ok_data.scores %>%
  mutate(Site = case_when(Location == 'Hizushi' ~ "HI",
                          Location == 'Sakubaru1' ~ "S1",
                          Location == 'Sakubaru2' ~ "S2",
                          Location == 'CapeZanpa' ~ "ZP",
                          Location == 'Zatsun' ~ "ZA",
                          Location == 'Yonama' ~ "YO",
                          Location == 'Sunabe' ~ "SN",
                          Location == 'Mizugama' ~ "MI",
                          Location == 'GinowanPort1' ~ "G1",
                          Location == 'GinowanPort2' ~ "G2",
                          Location == 'Sukuta' ~ "SK",
                          Location == 'Awa' ~ "AW")
  )

Ok_data.scores <- Ok_data.scores %>%
  mutate(Pressure = as.factor(case_when(impact == 'low' ~ "Low",
                              impact == 'medium' ~ "Medium",
                              impact == 'high' ~ "High"))
  )

Ok_data.scores$Pressure <- factor(Ok_data.scores$Pressure,
                                 levels = c("Low","Medium","High"))

Ok_data.scores$Site<-ordered(Ok_data.scores$Site,
                     levels=c("HI", "S1", "S2", "ZP", "ZA", "YO", "MI","SK", "SN", "AW", "G1", "G2"))

Ok_data.scores <- Ok_data.scores %>% rename(Depth = depth)

plot.nmds = ggplot(Ok_data.scores, aes(x = MDS1, y = MDS2)) +
  geom_point(size = 2.5, aes(shape = Pressure, colour = Site), alpha = 0.8) + 
  stat_ellipse(geom = "polygon", aes(fill = Pressure), alpha = 0.2) +
  scale_color_manual(values = c("#8400CD","#00C2F9","#FFB2FD","#009F81","#FFC33B","#FF6E3A","#008DF9","#A40122","#E20134","#9F0162","#FF5AAF","#00FCCF"))) +
  scale_fill_manual(values = c(Low = "#009F81",Medium = "#FFB331",High = "#9F1111")) +
  annotate("text", x=Inf, y=-Inf, hjust = 1, vjust = -0.5,
           label=bquote(atop(Stress == .(round(Ok_nmds$stress,3)),
                             'Non-metric fit,'~ R^2~"= 0.987")),
           size=3)+
  labs(x = "NMDS1", colour = "Site", y = "NMDS2") +
  guides(color= guide_legend(order=1, title = "Site", ncol =1))+
  theme_classic()

plot.nmds 

ggsave("edna_nmds.tiff", 
       units ="in", width = 8, height = 6, dpi = 300, compression = 'lzw',
       path = here::here("Multi-taxa_data","eDNA_clean","Plots"))

ggsave("edna_nmds.png", 
       units ="in", width = 8, height = 6, dpi = 300,
       path = here::here("Multi-taxa_data","eDNA_clean","Plots"))


## PERMDISP
betad.Ok <- betadisper(Ok.jac, Okinawa_env$Location)
permutest(betad.Ok)
boxplot(betad.Ok)
plot(betad.Ok)


betad.Ok.i <- betadisper(Ok.jac, Okinawa_env$impact)
permutest(betad.Ok.i)
boxplot(betad.Ok.i)

### Boxplot of within-site distance from centroid visualization with `ggplot()`

#Create an input dataframe for the boxplot with mean relative cover by
#category and depth within site

betad.Ok$distances #Extract betadisper scores

distcent_df <- data.frame(Distance_to_centroid=betad.Ok$distances,
                          Location=Okinawa_env$Location, 
                          Pressure=Okinawa_env$impact) 
Location <- betad.Ok$group
distcent_df[,2]<-factor(distcent_df[,2],
                        levels=c("Hizushi", "Sakubaru1", "Sakubaru2", "CapeZanpa", "Zatsun", "Yonama", "Mizugama","Sukuta", "Sunabe", "Awa", "GinowanPort1", "GinowanPort2"))
distcent_df[,3]<-factor(distcent_df[,3],levels=c("low","medium","high"))

distcent_mean_df <- distcent_df %>%
  group_by(Location) %>% 
  summarise(`Mean distance to centroid`=round(mean(Distance_to_centroid),2),
            SD=round(mean(Distance_to_centroid),2)) %>%
  mutate(Pressure = case_when(Location == 'Hizushi' | 
                                Location == "Sakubaru1" | 
                                Location == "Sakubaru2" ~ "Low",
                              Location == "CapeZanpa" |
                                Location == 'Zatsun' | 
                                Location == "Yonama" | 
                                Location == "Mizugama" ~ "Medium",
                              Location == 'Sukuta' |
                                Location == 'Sunabe' | 
                                Location == "Awa" | 
                                Location == "GinowanPort1" |
                                Location == "GinowanPort2"~ "High"))

distcent_mean_df <- distcent_mean_df[,c(1,4,2,3)]
distcent_mean_df$Location <- c("HI", "S1", "S2", "ZP", "ZA", "YO", "MI","SK", "SN", "AW", "G1", "G2")

distcent_df <- distcent_df %>%
  mutate(Location = case_when(Location == "Hizushi" ~ "HI",
                              Location == "Sakubaru1" ~ "S1",
                              Location == "Sakubaru2" ~ "S2",
                              Location == "CapeZanpa" ~ "ZP",
                              Location == "Zatsun" ~ "ZA",
                              Location == "Yonama" ~ "YO",
                              Location == "Mizugama" ~ "MI",
                              Location == "Sukuta" ~ "SK",
                              Location == "Sunabe" ~ "SN",
                              Location == "Awa" ~ "AW",
                              Location == "GinowanPort1" ~ "G1",
                              Location == "GinowanPort2" ~ "G2"
  )
  )

distcent_df$Location<-ordered(distcent_df$Location,
                                   levels=c("HI", "S1", "S2", "ZP", "ZA", "YO", "MI","SK", "SN", "AW", "G1", "G2"))
  
  
edna_location_box <- ggplot(data=distcent_df,aes(x=Location,y=Distance_to_centroid))+ 
  geom_boxplot(aes(fill=Pressure,colour=Pressure),alpha=0.3)+
  geom_jitter(aes(fill=Pressure,color=Pressure),alpha=0.7)+
  scale_fill_manual(values= c("#009F81", "#FFB331","#9F1111"))+
  scale_color_manual(values= c("#009F81", "#FFB331","#9F1111"))+
  theme_classic()+
  labs(title="Boxplot of mean distance to centroid by location")+
  ylab(label="Distance to centroid")+
  theme(legend.position = "none", #remove legend
        plot.title  = element_text(hjust = 0.5, face= "bold", size =14),
        axis.text=element_text(size=14))

edna_location_box

ggpubr:: ggarrange(edna_location_box, 
                   gridExtra::tableGrob(distcent_mean_df), 
                   ncol = 2, widths = c(1,1))

edna_location_box <- ggplot(data=distcent_df,aes(x=Location,y=Distance_to_centroid))+ 
  geom_boxplot(aes(fill=Pressure,colour=Pressure),alpha=0.3)+
  geom_jitter(aes(fill=Pressure,color=Pressure),alpha=0.7)+
  scale_fill_manual(values= c("#009F81", "#FFB331","#9F1111"))+
  scale_color_manual(values= c("#009F81", "#FFB331","#9F1111"))+
  theme_classic()+
  ylab(label="Distance to centroid")+
  theme(legend.position = "none", #remove legend
        plot.title  = element_text(hjust = 0.5, face= "bold", size =14),
        axis.text=element_text(size=14))

edna_location_box

ggpubr:: ggarrange(edna_location_box, 
                   gridExtra::tableGrob(distcent_mean_df), 
                   ncol = 2, widths = c(1,1))


ggsave("eDNA_site_box.tiff", 
       units="in", width=12, height=4, dpi=300, compression = 'lzw',
       path = here ("Multi-taxa_data","eDNA_clean","Plots"))

ggsave("eDNA_site_box.png", 
       units="in", width=12, height=4, dpi=300,
       path = here ("Multi-taxa_data","eDNA_clean","Plots"))

## PAIRWISE PERMANOVA
library(pairwiseAdonis)
#Location
pperm.Ok <- pairwise.adonis2(Ok.jac ~ Location, data = Okinawa_env, permutations = 999)
pperm.Ok
#impact
pperm.i.Ok <- pairwise.adonis2(Ok.jac ~ impact, data = Okinawa_env, permutations = 999)
pperm.i.Ok

#dbrda

Ok_rda <- dbrda(Ok.jac ~ impact, data = Okinawa_env) 
plot(Ok_rda)

db.res.axis <- anova.cca(Ok_rda, by = "axis") 
db.res.axis

db.res.var <- anova.cca(Ok_rda, by = "term") 
db.res.var

db_r_quadr <- RsquareAdj(Ok_rda)
db_r_quadr

anova(Ok_rda, by = "term")

## Plot rda
plot(Ok_rda)
plot_Ok_rda <- Ok_rda %>%
  scores(tidy = TRUE) %>% 
  split(.[["score"]])

plot_Ok_rda$centroids
Ok_rda_scores <- as.data.frame(scores(plot_Ok_rda, display = "site"))

# Combine scores with original data
Ok_plot_data <- cbind(Okinawa_env, Ok_rda_scores)

Ok_plot_data <- Ok_plot_data %>% rename(Pressure = impact,
                                        Site = Location)
Ok_plot_data$Pressure <- factor(Ok_plot_data$Pressure,
                                  levels = c("low","medium","high"))

#Now add the environmental variables as arrows
arrowmat <- as.data.frame(plot_Ok_rda$centroids)
arrowmat$label <- c("High pressure","Low pressure", "Medium pressure")
#Plot results
plot_rda <- ggplot(Ok_plot_data) +
  geom_point(size = 3, aes(x = dbRDA1, y = dbRDA2, color = Site, shape = Pressure), alpha = 0.5) +
  #geom_text_repel(aes(label = Samples, color = Location),size = 3, box.padding = 0.5) +  # Add text labels without overlap
  labs(x = "dbRDA1",y = "dbRDA2", shape = "Pressure",color = "Site") +
  theme_classic() +
  scale_color_manual(values = c("#9F0162","#009F81","#FF5AAF","#00FCCF","#8400CD","#008DF9","#00C2F9","#FFB2FD","#A40122","#E20134","#FF6E3A","#FFC33B"))

# Plot arrows and labels
edna_rda <- plot_rda +  geom_segment(data = arrowmat, aes(x = 0, y = 0, xend = dbRDA1, yend = dbRDA2),
                         arrow = arrow(length = unit(0.1, "inches")), color = "gray50", size = 1.5, alpha = 0.2) +
    geom_text(data = arrowmat, aes(label = label, x = dbRDA1, y = dbRDA2), 
            size = 3, vjust = 1.2, hjust = 0.5, color = "black") +
  labs(x = "dbRDA1",
       y = "dbRDA2") +
  theme_classic()

edna_rda

ggsave("eDNA_rda.png", 
       units="in", width=8, height=6, dpi=300,
       path = here ("Multi-taxa_data","eDNA_clean","Plots"))

# TAXONOMIC RICHNESS PIE CHARTS

## Create pie charts based on taxonomic richness on the family level merge based on Location
Ok_merge <- merge_samples(Ok, "Location")
Ok_merge_fam <- tax_glom(Ok_merge, "Family")
otu_table(Ok_merge_fam) <- ifelse(otu_table(Ok_merge_fam) > 0 ,1,0)
Ok_merge_fam_melt <- psmelt(Ok_merge_fam)
colours <- viridis(20)


family_counts_total <- Ok_merge_fam_melt %>%
  group_by(Sample, Phylum) %>%
  summarize(TotalFamilies = sum(Abundance > 0)) %>%
  ungroup()

#Calculate the number of taxonomic families assigned at each site
family_counts_phylum <- family_counts_total %>%
  group_by(Phylum) %>%
  summarize(TotalFamilies = sum(TotalFamilies)) %>%
  ungroup()

# Create a pie chart with colored borders around Phylum groups
pie_chart_total <- ggplot(family_counts_phylum, 
                          aes(x = "", y = TotalFamilies, fill = Phylum)) +
  geom_bar(stat = "identity", colour = "black", size = 1) +  # Borders for Phylum
  coord_polar("y", start = 0) +
  ggtitle("Total Dataset by Phylum") +
  scale_fill_manual(values = colours) +  # Specify the color palette
  theme_void()
  theme_void()
pie_chart_total

## Create pie charts based on taxonomic richness on the family level for each location
#Calculate total families for each location
total_families <- Ok_merge_fam_melt %>%
  group_by(Sample, Phylum) %>%
  summarise(TotalFamilies = sum(Abundance > 0)) %>%
  ungroup() %>%
  group_by(Sample) %>%
  summarise(TotalFamilies = sum(TotalFamilies))


#Determine the threshold for each location
total_families <- total_families %>%
  mutate(Threshold = 0.1 * TotalFamilies)

#Filter phyla based on threshold for each location
family_counts <- Ok_merge_fam_melt %>%
  group_by(Sample, Phylum) %>%
  summarise(TotalFamilies = sum(Abundance > 0)) %>%
  ungroup() %>%
  left_join(total_families, by = "Sample")

# Create pie charts for each location with black borders
pie_charts <- family_counts %>%
  group_split(Sample) %>%
  map(~ {
    site_data <- .
    ggplot(site_data, aes(x = "", y = TotalFamilies.x, fill = Phylum)) +
      geom_bar(stat = "identity", colour = "black", size = 0.5) +  # Black borders around Phylum
      geom_text(aes(label = paste("Total Families:", unique(TotalFamilies.y))), 
                x = 1.5, y = max(site_data$TotalFamilies.x) / 2, vjust = -2, size = 4) +  # Adjusted text position
      coord_polar("y", start = 0) +
      ggtitle(unique(site_data$Sample)) +
      scale_fill_manual(values = ifelse(site_data$TotalFamilies.x > site_data$Threshold, colours, "grey50")) +
      theme_void() +
      theme(legend.position = "none", plot.margin = margin(2, 2, 2, 2, "cm"))
  })

# Check the first pie chart
pie_charts[[12]]


# Create a folder named "piecharts" if it doesn't exist
if (!file.exists("piecharts_site")) {
  dir.create("piecharts_site")}
# Save the pie charts as images in the "piecharts" folder
lapply(1:length(pie_charts), function(i) {
  ggsave(paste0("piecharts/pie_chart_site_", i, ".png"), pie_charts[[i]])})


# Venn diagram
ok_venn_df <- (Okinawa_Tax$ID)

Ok_merge_impact <- merge_samples(Ok, "impact")
Ok_merge_impact_melt <- psmelt(Ok_merge_impact)

Ok_esv_impact_venn <- Ok_merge_impact_melt[,c(1:3)] 
Ok_esv_impact_venn <- subset(Ok_esv_impact_venn,
                             Ok_esv_impact_venn$Abundance != 0) # delete rows w/ abundance=0
Ok_esv_impact_venn <- Ok_esv_impact_venn[,-3] # remove abundance

esv_high <- subset(Ok_esv_impact_venn,
                   Ok_esv_impact_venn$Sample== "high")$OTU
esv_low <- subset(Ok_esv_impact_venn,
                  Ok_esv_impact_venn$Sample== "low")$OTU
esv_medium <- subset(Ok_esv_impact_venn,
                     Ok_esv_impact_venn$Sample== "medium")$OTU

esv_venn_list <- list(low = esv_low,
                      medium = esv_medium,
                      high = esv_high
)

install.packages("ggvenn")
library(ggvenn)
ggvenn(
  esv_venn_list, 
  fill_color = c("#0073C2FF", "#EFC000FF", "#CD534CFF"),
  stroke_size = 0.5, set_name_size = 4
)

install.packages("eulerr")
library(eulerr)
esv_venn_extracted <- euler(c("Low" = 281,
                              "Medium" = 184,
                              "High" = 295,
                              "Low&High" = 52,
                              "Low&Medium" = 56,
                              "Medium&High" = 63,
                              "Low&Medium&High" = 164),
                            shape = "ellipse"
)

esv_impact_venn <- plot(esv_venn_extracted,
                        quantities = list(type = "counts"),
                        fill = c(low = "#009F81", medium = "#FFB331",high = "#9F1111"),alpha = 0.5,
                        edges = list(col = "white",
                                     alpha = 0,
                                     lex = 2))

esv_impact_venn


library(ggVennDiagram)
ggVennDiagram::ggVennDiagram(esv_venn_list, label_alpha = 0)

esv_counts_total <- Ok_merge_impact_melt %>%
  group_by(Sample) %>%
  summarize(TotalESV = sum(Abundance > 0)) %>%
  ungroup()


# ANCOMBC analysis for differential abundance of groups

#Change reference level to impact = "low"

Ok@sam_data[["impact"]] <- factor(Ok@sam_data[["impact"]], levels = c("low", "medium", "high"))


#If needed, install BiocManager and ANCOMBC (by changing chunk settings to eval = T)

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.19")

BiocManager::install("ANCOMBC")

devtools::install_github("FrederickHuangLin/ANCOMBC")

## ANCOM-BC2 analysis per phylum
library(ANCOMBC)
out = ancombc2(Ok, tax_level = "Phylum", pseudo_sens = FALSE, fix_formula = "impact",
               p_adj_method = "holm", s0_perc = 0.05, prv_cut = 0.10, lib_cut = 0,
               group = "impact", alpha = 0.05, verbose = TRUE, global = TRUE, pairwise = TRUE)

res_prim = out$res
res_global = out$res_global
res_pair = out$res_pair
row.names(res_pair)<-res_prim$taxon
res_dunn = out$res_dunn
res_trend = out$res_trend
pseudo_sens = out$pseudo_sens_tab



### remove genus having no significant p-values among comparisons
any_true_impact <- res_pair$diff_impactmedium == TRUE|
  res_pair$diff_impacthigh == TRUE|
  res_pair$diff_impacthigh_impactmedium == TRUE

res_pair_impact <- filter(res_pair,any_true_impact)

### preparation for drawing a boxplot of W（log fold change divided by standard error）
PG_mediumvslow <- data.frame(res_pair$W_impactmedium,
                             res_pair$q_impactmedium,
                             row.names = res_pair$taxon) %>%
  rename("W_statistics" = res_pair.W_impactmedium,
         "q_value" = res_pair.q_impactmedium) %>%
  mutate(comparison = "Impact\nlow_vs_medium",
         star=ifelse(q_value<.001, "***", 
                     ifelse(q_value<.01, "**",
                            ifelse(q_value<.05, "*", ""))))

PG_highvslow <- data.frame(res_pair$W_impacthigh,
                           res_pair$q_impacthigh,
                           row.names = res_pair$taxon) %>%
  rename("W_statistics" = res_pair.W_impacthigh,
         "q_value" = res_pair.q_impacthigh)%>%
  mutate(comparison = "Impact\nlow_vs_high",
         star=ifelse(q_value<.001, "***", 
                     ifelse(q_value<.01, "**",
                            ifelse(q_value<.05, "*", ""))))

PG_mediumvshigh <- data.frame(res_pair$W_impacthigh_impactmedium,
                              res_pair$q_impacthigh_impactmedium,
                              row.names = res_pair$taxon) %>%
  rename("W_statistics" = res_pair.W_impacthigh_impactmedium,
         "q_value" = res_pair.q_impacthigh_impactmedium)%>%
  mutate(comparison = "Impact\nmedium_vs_high",
         star=ifelse(q_value<.001, "***", 
                     ifelse(q_value<.01, "**",
                            ifelse(q_value<.05, "*", ""))))



### Combine dataframes into a dataframe, and set factorsimpact
df_ancom_impact <- rbind(PG_mediumvslow,PG_highvslow,PG_mediumvshigh) %>%
  mutate(taxon = rep(rownames(PG_mediumvslow),3))
df_ancom_impact$comparison <- factor(df_ancom_impact$comparison,levels = c("Impact\nlow_vs_medium","Impact\nlow_vs_high","Impact\nmedium_vs_high"))
df_ancom_impact$taxon <- factor(df_ancom_impact$taxon,levels = rev(rownames(PG_mediumvslow)))


### Extract taxonomy information from phyloseq object
taxonomy <- as.data.frame(tax_table(Ok))
taxonomy$ESVs <- rownames(taxonomy)


### Merge taxonomy with dataframe
df_ancom_impact_taxon <- merge(df_ancom_impact, taxonomy, by.x = "taxon", by.y = "ESVs", all.x = TRUE)


### plot impact
bar_ancom_impact <- "NULL"
bar_ancom_impact <- df_ancom_impact_taxon %>%
  ggplot(aes(x = taxon, y = W_statistics)) +
  geom_col(aes(fill = factor(ifelse(W_statistics > 0, "Positive", "Negative"))),linewidth = 5) +
  labs(x = "ESVs", y = "W_statistics") +
  facet_grid(. ~ comparison, labeller = labeller(. = c("pressure /n low vs medium pressure", 
                                                          "low vs high pressure",
                                                          "medium vs high")
  )) +
  geom_text(aes(y = W_statistics + 1 * sign(W_statistics), label = star), 
            vjust = 0.7, color = "black", size = 8, position = position_dodge(width = 0.5)) +
  theme_classic() +  
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12),  # Adjusts the size of x-axis labels
        axis.text.y = element_text(size = 12),  # Adjusts the size of y-axis labels
        axis.title.x = element_text(size = 14), # Adjusts the size of x-axis title
        axis.title.y = element_text(size = 14)) + # Adjusts the size of y-axis title) 
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#009F81", "Negative" = "#9F1111"))
bar_ancom_impact


df_ancom_impact_taxon_mod <- df_ancom_impact_taxon %>%
  mutate(comparison = case_when(comparison == "Impact\nlow_vs_medium" ~ "Pressure \n Low vs medium",
                              comparison == "Impact\nlow_vs_high" ~ "Pressure \n Low vs high",
                              comparison == "Impact\nmedium_vs_high"~ "Pressure \n Medium vs High"))
                         
bar_ancom_impact <- "NULL"
bar_ancom_impact <- df_ancom_impact_taxon_mod %>%
  ggplot(aes(x = taxon, y = W_statistics)) +
  geom_col(aes(fill = factor(ifelse(W_statistics > 0, "Positive", "Negative"))),linewidth = 5) +
  labs(x = "ESVs", y = "W_statistics") +
  facet_grid(. ~ comparison)+
  geom_text(aes(y = W_statistics + 1 * sign(W_statistics), label = star), 
            vjust = 0.7, color = "black", size = 8, position = position_dodge(width = 0.5)) +
  theme_classic() +  
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12),  # Adjusts the size of x-axis labels
        axis.text.y = element_text(size = 12),  # Adjusts the size of y-axis labels
        axis.title.x = element_text(size = 14), # Adjusts the size of x-axis title
        axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 12)) + # Adjusts the size of y-axis title) 
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#009F81", "Negative" = "#9F1111"))
bar_ancom_impact

### Export ANCOM-BC2 plot 
ggsave("ANCOM-BC2_period_Family.tiff",
       units="in", width=10, height=8, dpi=300, compression = 'lzw',
       path = here ("Multi-taxa_data","eDNA_clean","Plots"))

ggsave("ANCOM-BC2_period_Family.png",
       units="in", width=10, height=6, dpi=300,
       path = here ("Multi-taxa_data","eDNA_clean","Plots"))


## ANCOMB on the Family level
Ok@sam_data[["impact"]] <- factor(Ok@sam_data[["impact"]], 
                                  levels = c("low", "medium", "high"))

out_esv = ancombc2(Ok, tax_level = "Family", pseudo_sens = FALSE, fix_formula = "impact",
                   p_adj_method = "holm", s0_perc = 0.05, prv_cut = 0.10, lib_cut = 0,
                   group = "impact", alpha = 0.05, verbose = TRUE, global = TRUE, pairwise = TRUE)

res_prim_esv = out_esv$res
res_global_esv = out_esv$res_global
res_pair_esv = out_esv$res_pair
row.names(res_pair_esv)<-res_prim_esv$taxon
res_dunn_esv = out_esv$res_dunn
res_trend_esv = out_esv$res_trend
pseudo_sens_esv = out_esv$pseudo_sens_tab

### remove genus having no significant p-values among comparisons
any_true_impact_esv <- res_pair_esv$q_impactmedium <.05|
  res_pair_esv$q_impacthigh <.05|
  res_pair_esv$q_impacthigh_impactmedium <.05

res_pair_impact_esv <- filter(res_pair_esv,any_true_impact_esv)


### preparation for drawing a boxplot of W（log fold change divided by standard error）

PG_mediumvslow_esv <- data.frame(res_pair_impact_esv$W_impactmedium,
                                 res_pair_impact_esv$q_impactmedium,
                                 row.names = res_pair_impact_esv$taxon)  %>%
  rename("W_statistics" = res_pair_impact_esv.W_impactmedium,
         "q_value" = res_pair_impact_esv.q_impactmedium)%>%
  mutate(comparison = "Impact\nlow_vs_medium",
         star=ifelse(q_value<.001, "***", 
                     ifelse(q_value<.01, "**",
                            ifelse(q_value<.05, "*", ""))))

PG_highvslow_esv <- data.frame(res_pair_impact_esv$W_impacthigh,
                               res_pair_impact_esv$q_impacthigh,
                               row.names = res_pair_impact_esv$taxon) %>%
  rename("W_statistics" = res_pair_impact_esv.W_impacthigh,
         "q_value" = res_pair_impact_esv.q_impacthigh)%>%
  mutate(comparison = "Impact\nlow_vs_high",
         star=ifelse(q_value<.001, "***", 
                     ifelse(q_value<.01, "**",
                            ifelse(q_value<.05, "*", ""))))

PG_mediumvshigh_esv <- data.frame(res_pair_impact_esv$W_impacthigh_impactmedium,
                                  res_pair_impact_esv$q_impacthigh_impactmedium,
                                  row.names = res_pair_impact_esv$taxon) %>%
  rename("W_statistics" = res_pair_impact_esv.W_impacthigh_impactmedium,
         "q_value" = res_pair_impact_esv.q_impacthigh_impactmedium)%>%
  mutate(comparison = "Impact\nmedium_vs_high",
         star=ifelse(q_value<.001, "***", 
                     ifelse(q_value<.01, "**",
                            ifelse(q_value<.05, "*", ""))))


### Combine dataframes into a dataframe, and set factors impact

df_ancom_impact_esv <-rbind(PG_mediumvslow_esv,
                            PG_highvslow_esv,
                            PG_mediumvshigh_esv) %>%
  mutate(taxon = rep(rownames(PG_mediumvslow_esv),3))

df_ancom_impact_esv$comparison <- factor(df_ancom_impact_esv$comparison,
                                         levels = c("Impact\nlow_vs_medium",
                                                    "Impact\nlow_vs_high",
                                                    "Impact\nmedium_vs_high"))

df_ancom_impact_esv$taxon <- factor(df_ancom_impact_esv$taxon,
                                    levels = rev(rownames(PG_mediumvslow_esv)))

### Extract taxonomy information from phyloseq object
taxonomy_esv <- as.data.frame(tax_table(Ok))
taxonomy_esv$ESVs <- rownames(taxonomy_esv)


### Merge taxonomy with dataframe
df_ancom_impact_taxon_esv <- merge(df_ancom_impact_esv, taxonomy_esv, by.x = "taxon", by.y = "ESVs", all.x = TRUE)


### plot impact    
bar_ancom_impact_esv <- "NULL"
bar_ancom_impact_esv <- df_ancom_impact_taxon_esv %>%
  ggplot(aes(x = taxon, y = W_statistics)) +
  geom_col(aes(fill = factor(ifelse(W_statistics > 0, "Positive", "Negative"))),linewidth = 5) +
  labs(x = "ESVs", y = "W_statistics") +
  facet_grid(. ~ comparison) +
  geom_text(aes(y = W_statistics + 4 * sign(W_statistics), label = star), 
            vjust = 0.7, color = "black", size = 8, position = position_dodge(width = 0.5)) +
  theme_classic() +  
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12),  # Adjusts the size of x-axis labels
        axis.text.y = element_text(size = 12),  # Adjusts the size of y-axis labels
        axis.title.x = element_text(size = 14), # Adjusts the size of x-axis title
        axis.title.y = element_text(size = 14)) + # Adjusts the size of y-axis title) 
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#009F81", "Negative" = "#9F1111"))
bar_ancom_impact_esv

### Export ANCOM-BC2 plot 
ggsave("ANCOM-BC2_period__esv.tiff",
       units="in", width=16, height=12, dpi=300, compression = 'lzw',
       path = here ("Multi-taxa_data","eDNA","4-Output"))


### Extract and export ESV list with higher significance (***)

ESV_higher_ANCOM_BC2 <- subset(df_ancom_impact_taxon_esv,
                               df_ancom_impact_taxon_esv$star == "***")

writexl::write_xlsx(ESV_higher_ANCOM_BC2,
                    path = here ("Multi-taxa_data",
                                 "eDNA",
                                 "4-Output",
                                 "ESV_higher_ANCOM_BC2.xlsx"))




# Spatial mantel with restricted null model considering spatial autocorrelation

## Mantel test
library(fields)
library(reshape2)
library(ade4)
library(adespatial)
library(spdep)

Ok_dist <- read.csv(file=here("Multi-taxa_data","eDNA_clean","Ok_dist.csv"), row.names=1)
head(Ok_dist)

#create distance matrix
Dist.km <- as.dist(rdist.earth(Ok_dist, miles=F))
#correlation between straight-line geographic distance and dissimilarity in species composition
set.seed(69)
mantel(Dist.km, Ok.jac) 
## Prepare data for graphic
matrix.dist <- data.frame(x = melt(as.matrix(Dist.km))$value, 
                          y = melt(as.matrix(Ok.jac))$value)
## Plot
ggplot(matrix.dist) +
  geom_jitter( aes(x,y), shape = 21, size = 3, fill = "#8fc7a0") +
  labs(x = "Distance (km)", 
       y = "Dissimilarity (Jaccard)") +
  geom_smooth( aes(x,y), method = "glm") +
  theme_classic()

ggsave("eDNA_dissimil_distance.tiff", 
       units="in", width=6, height=4, dpi=300, compression = 'lzw',
       path = here ("Multi-taxa_data","eDNA_clean","Plots"))

ggsave("eDNA_dissimil_distance.png", 
       units="in", width=6, height=4, dpi=300,
       path = here ("Multi-taxa_data","eDNA_clean","Plots"))
## Mantel
mantel.Ok <- mantel.randtest(sqrt(Ok.jac), Dist.km)
mantel.Ok
## Minimum Spanning Tree
nb.Ok <- mst.nb(Dist.km) # calculate Minimum Spanning Tree network
plot(nb.Ok, Ok_dist)
## Converting
lw <- nb2listw(nb.Ok)
## Mantel test new
msr(mantel.Ok, lw, method = "pair")
