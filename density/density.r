
library(ggplot2)
library("ggsci")
library(ggplot2)
library(scales)
#install.packages("ggridges")
library(ggridges)
library(cowplot)
install.packages("pracma")
library(pracma)

key1<-read.csv("Dlon.kaks", sep = "\t",header = T)
key2<-read.csv("Bniv.kaks", sep = "\t",header = T)
key3<-read.csv("Rchi.kaks", sep = "\t",header = T)
key4<-read.csv("Otri.kaks", sep = "\t",header = T)
key5<-read.csv("Malb.kaks", sep = "\t",header = T)
key6<-read.csv("Gtri.kaks", sep = "\t",header = T)
key7<-read.csv("Ddru.kaks", sep = "\t",header = T)
key8<-read.csv("Csat.kaks", sep = "\t",header = T)
key9<-read.csv("Cobl.kaks", sep = "\t",header = T)


spe<- rep(c('Dlon-Dlon','Bniv-Bniv','Rchi-Rchi','Otri-Otri','Malb-Malb','Gtri-Gtri','Ddru-Ddru',
            'Csat-Csat','Cobl-Cobl'),c(nrow(key1),nrow(key2),nrow(key3),nrow(key4),nrow(key5),nrow(key6),
                                       nrow(key7),nrow(key8),nrow(key9)))
class(factor(spe))
length(factor(spe))
length(spe)
data<-rbind(key1,key2,key3,key4,key5,key6,key7,key8,key9)
dim(data)

data$key <-spe
head(data,2)

p<-ggplot(data = data,aes(x=Ks))+
  geom_density(aes(colour=key),alpha=0.28,key_glyph="smooth")+
  scale_x_continuous(expand = c(0,0),limits = c(0,3),breaks = pretty_breaks(n=6))+
  scale_y_continuous(expand=c(0,0))+
  theme_cowplot()+
  theme(axis.text = element_text(size=8),axis.title = element_text(size=10),
        legend.title=element_blank(),legend.text = element_text(size=8))+
  labs(x="Synonymous substitutions per synonymous site (Ks)",y="Density",size=10)+
  scale_color_manual(values=c("#FF0000","#00FF00","#FFFF00","#FFA500","#0000FF","#800080","#FFC0CB","#00FFFF","#008000"))

species<-levels(unique(p$data$key))
density_data<-layer_data(p)

# 初始化一个向量来存储每个组的最高点的横坐标值
max_x_values <- numeric(length(unique(data$key)))

# 找到每个组的最高点横坐标值
for (key in unique(density_data$group)) {
  subset_data <- subset(density_data, group == key)
  max_index <- which.max(subset_data$y)
  max_x_values[key] <- subset_data$x[max_index]
}
max_x_values #这个值更接近曲线的值
class(max_x_values)
outdata<-t(rbind(species,max_x_values))
colnames(outdata)<-c('Species','Ks peak value')
write.table(outdata,"ks_peaks.txt",sep="\t",quote = F,col.names = T,row.names = F)

# 
# x_max_value<-numeric(0)
# for (no in seq(1,9)){
#   data_key<-get(paste0('key',no))
#   density_key<-density(na.omit(data_key$Ks))
#   x_index<- which.max(density_key$y)
#   x_max_value<-cbind(x_max_value,density_key$x[x_index])
# }
# x_max_value

ggsave('ks_density.pdf')

ggplot(data,aes(x=Ks,y=key))+
  geom_density_ridges(aes(fill=key,group=key),size=0.5,alpha=0.7)+
  scale_fill_locuszoom()+
  scale_x_continuous(expand = c(0,0),limits=c(0,2.0),breaks = pretty_breaks(n=5))+
  theme_cowplot()+
  theme(legend.title = element_blank(),
        # change for plot
        legend.position = "none")+
  labs(x="Synonymous substitutions per synonymous site (Ks)",y="Density",size=1)+
  guides(fill = guide_legend(nrow = 9))+
  scale_fill_brewer(palette="Set3")+
  scale_color_brewer(palette="Set3")

ggsave("ks_wgd_staggered.pdf",heigh=17.8*0.618,width=17.8,units="cm")
ggsave("ks_wgd_staggered.png",heigh=20*0.618+1,width=20,units="cm")
