# Read in COP matrix
# author: Yuan Chai
# email: chaix026@umn.edu

# Steps:
# 1. Get COP matrix
# 2. Get WVA list of var_std
# 3. Match WVA to COP names
# 4. Create phylogenetic tree

rm(list=ls())

library(data.table)
library(Matrix)
library(ape)
library(phangorn)

# read in COP matrix
COP <- fread("./data_original/COP.tsv", sep="\t")
COP.list <- COP$V1
COP$V1 <- NULL
colnames(COP) <- rownames(COP) <- COP.list

# read in WVA data
load("./data_intermediate/01_1_WVA_data.Rdata")
WVA.DSW <- WVA.data.final
var.list <- unique(WVA.DSW$var_std)

# check matched varieties in the COP matrix
sum(!(var.list %in% COP.list))
sum(!(COP.list %in% var.list))

# Select COP matrix with only planted varieties
cop.sel <- (rownames(COP) %in% var.list)
COP.WVA <- COP[cop.sel, cop.sel, with=FALSE]
rownames(COP.WVA) <- colnames(COP.WVA)

# zero COP matrix for missing varieties
var.miss <- var.list[ ! (var.list %in% COP.list)]
COP.miss <- diag(length(var.miss))
rownames(COP.miss) <- colnames(COP.miss) <- var.miss

# Merge matrix
COP.final <- bdiag(as.matrix(COP.WVA), COP.miss)
rownames(COP.final) <- colnames(COP.final) <- c(rownames(COP.WVA), rownames(COP.miss))

# Preliminary tree
# Distance measures
var.dist <- as.dist(1-COP.final)
var.dist.sel <- as.dist(1-COP.WVA)


#### Phylogenetic Tree ####

# Phy Tree: UPGMA
PT.var.upgma <- upgma(var.dist)
plot(PT.var.upgma, show.tip.label = FALSE, type="phylogram", use.edge.length = FALSE, cex=0.8)
save(PT.var.upgma, file="./data_intermediate/01_2_PT.var.upgma.RData")

PT.var.sel.upgma <- upgma(var.dist.sel)
plot(PT.var.sel.upgma, show.tip.label = FALSE, type="phylogram", use.edge.length = FALSE, cex=0.8)
save(PT.var.sel.upgma, file="./data_intermediate/01_2_PT.var.sel.upgma.RData")
