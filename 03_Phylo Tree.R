#### Plot heatmap and Phylogenetic trees ####
# author: Yuan Chai
# email: chaix026@umn.edu


rm(list=ls())

library(data.table)
library(ggplot2)
library(ape)
library(gplots)
library(RColorBrewer)
library(ggdendro)
library(dendextend)
library(plotly)

## Reference: Tutorial: A short tutorial for decent heat maps in R
#  http://sebastianraschka.com/Articles/heatmaps_in_r.html

## ---- Read in data ----
# area-by-var data
load("./data_intermediate/01_4_WVA_Durum_panel.Rdata")
load("./data_intermediate/01_4_WVA_Spring_panel.Rdata")
load("./data_intermediate/01_4_WVA_Winter_panel.Rdata")
WVA.DSW.panel <- rbind(WVA.Durum.panel, WVA.Spring.panel, WVA.Winter.panel)

# phylogenetic tree
load("./data_intermediate/01_2_PT.var.sel.upgma.RData")      # upgma tree for selected (pedigree known varieties)
PT.var.sel.upgma.DSW <- PT.var.sel.upgma

## ---- Convert phylogenetic tree into Dendrogram ----
rep_tree_r <- root(PT.var.sel.upgma.DSW, 1, resolve.root = T)
rep_tree_r$edge.length[which(rep_tree_r$edge.length<=0)] <- 0.00001
rep_tree_um <- chronopl(rep_tree_r, lambda=0.1, tol=0)
rep_tree_d <- as.dendrogram(as.hclust.phylo(rep_tree_um))
rm(rep_tree_r, rep_tree_um)

## Get the order according to dendrogram tree
clade_order <- order.dendrogram(rep_tree_d)
clade_name <- labels(rep_tree_d)
clade_position <- data.frame(clade_name, clade_order)
clade_position <- clade_position[order(clade_position$clade_order),]

#---- Subset data ----
WVA.panel.sb <- WVA.DSW.panel[var_COP %in% clade_name]

#---- Plot heatmap: all US by year ----
# data matrix
WVA.panel.sb[, class3f := as.numeric(as.factor(class3))]
WVA.panel.sb[is.na(class3f), class3f := 0]
WVA.yr <- dcast(WVA.panel.sb, var_COP ~ year, value.var="class3f", fun.aggregate = max, na.rm=TRUE)
rownames(WVA.yr) <- WVA.yr$var_COP
WVA.yr.m <- data.matrix(WVA.yr)
new_order <- match(clade_position$clade_name,
                   row.names(WVA.yr.m))
WVA.yr.m <- WVA.yr.m[new_order, -1]
rm(WVA.yr)

#WVA.yr.m[is.na(WVA.yr.m)] <- 0

# Plot heatmap
# creates a own color palette
my_palette <- c("white", "#E34A33", "#31A354", "#3182BD")
col_breaks = c(-1, 0, 1, 2, 3)

# ---- Plot heatmap: all year all US

# Save plot
png("./plot/03_Heatmap_DSW.png", width = 8, height = 5, units = "in", res= 300)



heatmap.2(WVA.yr.m,
          Rowv=rep_tree_d,
          Colv=FALSE,
          dendrogram = "row",
          col=my_palette,
          breaks=col_breaks,
          trace = "none",
          labRow=FALSE,
          key=FALSE)
legend(x=0, y=0, xpd=TRUE,
       legend = c(NA, "Durum", "Spring", "Winter"),
       col = my_palette,
       # title="Market Class",
       bty = "n",
       box.lwd = 0,
       lty= 1,             
       lwd = 5,           
       cex=0.7
)

dev.off()


# ---- Plot heatmap: all year by state ----


# Method1:
WVA.st <- dcast(WVA.panel.sb, var_COP ~ state, value.var="class3f", fun.aggregate = max, na.rm=TRUE)
WVA.st[ WVA.st == -Inf] <- 0
rownames(WVA.st) <- WVA.st$var_COP
WVA.st.m <- data.matrix(WVA.st)
new_order <- match(clade_position$clade_name,
                   row.names(WVA.st.m))
WVA.st.m <- WVA.st.m[new_order, -1]
WVA.st.m[is.na(WVA.st.m)] <- 0
rm(WVA.st)

# Plot heatmap
# creates a own color palette
my_palette <- c("white", "#E34A33", "#31A354", "#3182BD")
col_breaks = c(-1, 0, 1, 2, 3)

# Save plot
png("./plot/03_01_Heatmap_by_State.png", width = 8, height = 5, units = "in", res= 300)

heatmap.2(WVA.st.m,
          Rowv=rep_tree_d,
          dendrogram = "both",
          col=my_palette,
          breaks=col_breaks,
          cexCol = 0.75,
          trace = "none",
          labRow = FALSE,
          key=FALSE)
legend(x=0, y=0, xpd=TRUE,
       legend = c(NA, "Durum", "Spring", "Winter"),
       col = my_palette,
       # title="Market Class",
       box.lwd = 0,
       box.col="white",
       lty= 1,             
       lwd = 5,           
       cex=.7
)

dev.off()