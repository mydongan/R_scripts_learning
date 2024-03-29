# phylip tree

library(optparse)

option_list = list(
  make_option(c("-t", "--tree"), type="character", metavar="character",
              help="The tree file"),
  make_option(c("-g", "--group"), type="character", metavar="character",
              help="The group file"),
  make_option(c("-m", "--mode"), type="character", metavar="character",default="none",
              help="The mode: ['none','daylight','fan'], default=%default"),
  make_option(c("-o", "--outprefix"), type="character", metavar="character",
              help="The prefix of output file")
)
opt = parse_args(OptionParser(option_list = option_list, usage = "This script is used to plot Tree!"))


library(treeio)
# library(tidytree)
library(ggtree)
library(ggsci)
library(ggplot2)
# library(dplyr)

pal <- pal_locuszoom()(7)
sh_pal <- c(pal[1],pal[2],pal[3])

setwd('H:/青岛欧易/3.项目/群体/DZOE20230778678-b1-黑鸡纵菌/NJtree')
#setwd('H:/青岛欧易/3.项目/群体/HT2021-23410-chick-DOE20228824/20230721')
dir()
#tree <- read.tree(opt$t)
tree <- read.tree("plink.tree")
#tree <- read.tree("outtree")
tree
# write.nexus(tree,file = "sang.nexus")
# tree <- read.nexus("plink.nexus")

#write order
d = fortify(tree)
d
d = subset(d, isTip)
# tree_order=with(d, label[order(y, decreasing=T)])
# write.table(tree_order,file="tree_order.xls",quote=F,sep="\t")

# annotation
#group <- read.table(opt$g,header=T,sep="\t")
group <-read.table("group.tsv",header = T, sep="\t",colClasses=c('character'))
group
groups <- split(as.vector(group$Sample), as.vector(group$Pop1))
groups

# ad info to tree
tree <- groupOTU(tree, groups)
tree
# plot
if (opt$m == "fan")
{
  p <- ggtree(tree,aes(color=group),branch.length = "none",layout = "fan",open.angle=10)
} else if (opt$m == "daylight")
{
  p2 <- ggtree(tree,aes(color=group),branch.length = "none",layout = "daylight",open.angle=10)
} else {
  p3 <- ggtree(tree,aes(color=group),branch.length = "none",size=0.2)
}
#p<-ggtree(tree,aes(color=group),branch.length="none",size=0.2)
p
tree_data = p3 +
  geom_tiplab(aes(color=group),size=4,hjust=-0.3)+
  # geom_tiplab(color="black",size=1,hjust=-0.3)+
  # geom_text2(aes(subset=!isTip,label=node),hjust=0.1,color="red") +
  # geom_point(aes(shape=group), size=3)+
  # geom_tippoint(aes(shape=Pop2,color=Pop1),size=2) +
  # scale_shape_manual(values = c(19,16,17,18,15)) +
  scale_color_manual(values = sh_pal) +
  # guides(color=guide_legend(order = 1)) +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size =  14),
        legend.key.size = unit(1,"cm"))
        #legend.key = element_blank())+
  #guides(color = guide_legend(override.aes = list(shape = "blank")))

tree_data

if (opt$m == "daylight")
{
  ggsave(paste("NJtree","_daylight.pdf",sep=""),width = 17,height = 17,units = "cm")
  ggsave(paste("NJtree","_daylight.png",sep=""),width = 17,height = 17,units = "cm")
} else if (opt$m == "fan")
{
  ggsave(paste("NJtree","_circular.pdf",sep=""),width = 17,height = 17,units = "cm")
  ggsave(paste("NJtree","_circular.png",sep=""),width = 17,height = 17,units = "cm")
} else {
  ggsave(paste("NJtree",".pdf",sep=""),width = 10,height = 10,units = "in")
  ggsave(paste("NJtree",".png",sep=""),width = 10,height = 10,units = "in")
}


?ggtree

ggtree(tree,aes(color=group),size=0.2,layout="slanted")+geom_treescale()
#one of 'rectangular', 'slanted', 'fan', 'circular', 'radial', 'equal_angle' or 'daylight'

ggtree(tree,aes(color = group)) + geom_tiplab() + theme(legend.position = "right")+geom_treescale(fontsize = 3, linesize = 0.5)
ggsave(paste("NJtree-adddistance",".pdf",sep=""),width = 10,height = 10,units = "in")
ggsave(paste("NJtree-adddistance",".png",sep=""),width = 10,height = 10,units = "in")

?geom_treescale
