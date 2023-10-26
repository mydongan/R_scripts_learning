library(ggplot2)
library(tidyr)
data<-read.csv('snp-new.stat.xls',header = T,sep='\t')
data1<-pivot_longer(data,cols = number, values_to = 'value')
data1$group <- factor(data1$group, levels = c("K082T","Org-K082-p9","K086T","Org-K086-p9","S020T","Org-S020-p16","S037T","Org-S037-p11"))
data1$classification <- reorder(data1$classification, data1$value)
ggplot(data1,aes(x=group,y=value,fill=classification))+
  geom_bar(stat="identity",position = "fill")+
  theme_classic()+
  ylab("germline snp mutation variance(%)")+xlab("")+
  theme(legend.title = element_blank())+
  scale_y_continuous(expand=c(0,0))
ggsave('germline.snp.pdf')
ggsave('germline.snp.png')
