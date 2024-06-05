# Load libraries
library(ggtree)
library(ggplot2)
library(ape)
library(tidytree)
library(treeio)
library(dplyr)
library(extrafont)
loadfonts()
setwd("C:/Users/eb826/OneDrive - University of Exeter/PhD_EdouardMicrosporidia/Thesis/Chapter5_Gryllus bimaculatus/PhyloFisher")

# Read in your tree file (replace "treefile.nwk" with the path to your tree file)
tree <- read.tree("matrix.fas.treefile")

# Replace labels in the tree
new_labels <- c(
  "Amac" = "Allomyces macrogynus",
  "Amph" = "Amphiamblys sp.",
  "Aalg" = "Anncaliia algerae",
  "Aloc" = "Antonospora locustae",
  "Bden" = "Batrachochytrium dendrobatidis",
  "Cdik" = "Cucumispora dikerogammari",
  "Dmul" = "Dictyocoela muelleri",
  "Droe" = "Dictyocoela roeselum",
  "Eaed" = "Edhazardia aedis",
  "Ecun" = "Encephalitozoon cuniculi",
  "Ehel" = "Encephalitozoon hellem",
  "Eint" = "Encephalitozoon intestinalis",
  "Eroe" = "Encephalitozoon romaleae",
  "Ebien" = "Enterocytozoon bieneusi",
  "Ehep" = "Enterocytozoon hepatopenaei",
  "Ecan" = "Enterospora canceri",
  "Hmag" = "Hamiltosporidium magnivora",
  "Htva" = "Hamiltosporidium tvaerminnensis",
  "Heri" = "Hepatospora eriocheir",
  "Metch" = "Metchnikovella incurvata",
  "Mdap" = "Mitosporidium daphniae",
  "Np1E6" = "Nematocida ausubeli",
  "Ndis" = "Nematocida displodere",
  "Nspm5" = "Nematocida ironsii",
  "Nparm1" = "Nematocida parisii ERTm1",
  "Nant" = "Nosema antheraeae",
  "Nbom" = "Nosema bombycis",
  "Ngra" = "Nosema granulosis",
  "NYNPr" = "Nosema YNPr",
  "Ocol" = "Ordospora colligata",
  "Psac" = "Paramicrosporidium saccamoebae",
  "Psp" = "Albopleistophora grylli",
  "Pneu" = "Pseudoloma neurophilia",
  "Rall" = "Rozella allomycis",
  "Slop" = "Spraguea lophii",
  "Tcon" = "Astathelohania contejeani",
  "Thom" = "Trachipleistophora hominis",
  "Trat" = "Tubulinosema ratisbonensis",
  "Napi" = "Vairimorpha apis",
  "Ncer" = "Vairimorpha ceranae",
  "Vcul" = "Vavraia culicis",
  "Vcor" = "Vittaforma corneae"
)

# Define a function to replace labels in the tree object
replace_labels <- function(tree, new_labels) {
  for (i in 1:length(tree$tip.label)) {
    if (tree$tip.label[i] %in% names(new_labels)) {
      tree$tip.label[i] <- new_labels[tree$tip.label[i]]
    }
  }
  return(tree)
}

# Replace labels in the tree object
tree <- replace_labels(tree, new_labels)

# Define the outgroup
outgroup <- "Allomyces macrogynus"

# Reroot the tree with "Allomyces macrogynus" as the outgroup
tree <- root(tree, outgroup, resolve.root = TRUE)

# Collapse unsupported branches of trees into polytomy. This is not necessary
#tree <- as.polytomy(tree, feature='node.label', fun=function(x) as.numeric(x) < 50)

# This shortens your tree to fit tip labels. Adjust the factor for a better fit.
xlim_adj <- max(ggtree(tree)$data$x) * 1.9

# Define the data for geom_hilight
highlight_data <- data.frame(
  node_light = c(62, 58, 73, 81, 38, 45, 54),  # Node IDs
  fill_color = c("#00BC00", "#B66DFF", "#D82632", "#FFCA99", "#EFC000", "#00AACC", "#999999"))  # Fill colors

# Plot the tree with new labels
p <- ggtree(tree, ladderize = TRUE) + # ggtree with "ladderize = TRUE" makes the tree look more "balanced" by reordering the nodes
  geom_tiplab(size = 3.9, align=TRUE, linesize=.5, offset=0.1, fontface="italic", family="Times New Roman") + # geom_tiplab affects the way the "tips" are displayed (e.g., font, size, color, italics, etc.).
  geom_treescale(y = -0.95, fontsize = 3.9) +  
  geom_text2(aes(x = branch, label = as.numeric(label), 
                subset = !is.na(as.numeric(label)) & as.numeric(label) > 49 & as.numeric(label) <101), 
                vjust = -0.5, size = 3.2)  + # geom_text2 affects the text indicating the BS values. Here we choose not to display the values below fifty.
  #labs(title = "Phylogenomic Tree") +
  #theme(legend.text = element_text(size=8)) + # Position of the legend
  xlim(0, xlim_adj) +
  #geom_text(aes(label=node)) + # see the nodes number 
  geom_cladelabel(node = 62, label = "Nosematida", offset = .58, align = TRUE, fontsize = 5, color = "#00BC00", family = "Times New Roman") +
  geom_cladelabel(node = 58, label = "Enterocytozoonida", offset = .58, align = TRUE, fontsize = 5, color = "#B66DFF", family = "Times New Roman") +
  geom_cladelabel(node = 73, label = "Glugeida", offset = .58, align = TRUE, fontsize = 5, color = "#D82632", family = "Times New Roman") +
  geom_cladelabel(node = 81, label = "Orphan lineage", offset = .58, align = TRUE, fontsize = 5, color = "#FFCA99", family = "Times New Roman") +
  geom_cladelabel(node = 38, label = "Amblyosporida", offset = .58, align = TRUE, fontsize = 5, color = "#EFC000", family = "Times New Roman") +
  geom_cladelabel(node = 45, label = "Neopereziida", offset = .58, align = TRUE, fontsize = 5, color = "#00AACC", family = "Times New Roman") +
  geom_cladelabel(node = 54, label = "Ovavesiculida", offset = .58, align = TRUE, fontsize = 5, color = "#999999", family = "Times New Roman") +
  geom_hilight(data = highlight_data, aes(node = node_light, fill = fill_color), alpha = 0.4, align="both") +
  scale_fill_identity(guide = FALSE)

# Display the tree
p

ggsave("PhylogenomicTree_R_Psp.svg", p, dpi = 600, width = 14, height = 8, units = "in")

# Compute summary statistics for branch lengths
branch_length_summary <- summary(tree$edge.length)

# Filter out non-numeric support values and convert to numeric
numeric_support_values <- as.numeric(tree$node.label[!is.na(tree$node.label) & grepl("^\\d+", tree$node.label)])

if (!any(is.na(numeric_support_values))) {
  # Compute summary statistics for support values
  support_values_summary <- summary(numeric_support_values)
  
  # Prepare the summary text
  summary_text <- paste(
    "Summary of Phylogenetic Tree Data:",
    "Branch Lengths:",
    paste("Mean:", mean(branch_length_summary)),
    paste("Median:", median(branch_length_summary)),
    paste("Standard Deviation:", sd(branch_length_summary)),
    "Support Values:",
    paste("Mean:", mean(support_values_summary)),
    paste("Median:", median(support_values_summary)),
    paste("Standard Deviation:", sd(support_values_summary)),
    sep = "\n"
  )
} else {
  # If no valid numeric support values are found, provide a message indicating it
  summary_text <- "No valid numeric support values found."
}

# Print or use the summary text as needed
print(summary_text)