library(ggplot2)
library(reshape2)
gene<-read.csv("gene-expression.txt",sep ="\t",header=T,check.names  = F)
gene_long<-melt(gene,id.vars = "Gene",variable.name = "treatment", value.name = "value")

gene_long$value<-2^(-gene_long$value)
gene_long
gene_data_summary <- aggregate(value ~ Gene + treatment, data = gene_long, 
                               FUN = function(x) c(Mean = mean(x), SEM = sd(x) / sqrt(length(x))))
##aggregate后的value成了一个matrix列表，需要拆分成两列(mean and se),并将原来的一列删除
gene_data_summary2<-gene_data_summary %>%
  mutate(Mean = gene_data_summary$value[,1],
         SEM= gene_data_summary$value[,2]) %>% select(-value)


ggplot(data=gene_data_summary2,aes(x=treatment,y=Mean,fill=Gene))+
  geom_bar(stat = "identity",position = position_dodge())+
  geom_errorbar(aes(ymin=Mean-SEM,ymax=Mean+SEM),  #添加标准误差线
                position = position_dodge(width=0.9),width=0.2)+
  facet_wrap(~Gene,scales = "free_y")+  #独立的Y轴坐标
  theme(axis.text.x =element_text(angle=90))+ #x轴标签垂直
labs(y="2^(-deltaCT)")

