#pie
data<-read.csv("sv.txt",sep='\t',header = T)
data
# > data
#    type id    length
# 1   CNV  0  23074337
# 2   DEL  0    203113
# 3   INS  0   3280086
# 4   INV  0  18476114
# 5   HDR  0  28472271
# 6  SNPs  0   2156854
# 7 TRANS  0  13198922
# 8   SYN  0 110503425
# 9 NOTAL  0 112854683
newdata<-data[1:7,]
pie(data$length,labels = data$type)
library(ggplot2)
library(reshape2)
library(cowplot)
library(tidyverse)
library("ggsci")
newpalette<-colorRampPalette(brewer.pal(12,"Set3"))(12) #ÉèÖÃÑÕÉ«
#install.packages('ragg')
library(ragg)
data1<-melt(newdata,id="type")
# ggplot(data,aes(x="",y=length,fill=type))+
#   geom_bar(position = "stack",stat = "identity")+
#   coord_polar(theta="y")
mycolor<-c("#FF990066","#91D1C266", "#00828066","#B09C8566","#CC000099","#E64B3566","#35800066")
#mycolor<-c("#1F77B4FF","#AEC7E8FF","#FF7F0EFF","#FFBB78FF","#2CA02CFF","#98DF8AFF","#D62728FF")  
ggplot(data1,aes(x=variable,y=value,fill=type))+
  geom_bar(position = "stack",stat = "identity",color = "black")+
  coord_polar(theta = "y")+
  theme(panel.grid = element_blank(),strip.background = element_blank())+
  #scale_fill_brewer(palette="Set3")+
  scale_fill_manual(values =c("red","#1E90FF","#7B68EE","#EE82EE","orange","green","yellow"))+
  #scale_fill_manual(values=mycolor)+
  theme_bw()