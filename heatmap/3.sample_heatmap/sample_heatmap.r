library("pheatmap")

tpm<-read.csv('snp.similarity.xls',sep="\t",row.names = 1,header = TRUE,check.names = F)
name<-'similarity'

if(length(colnames(tpm)) > 2){
	scale<-'row'
}else{
	scale<-'none'
}

pheatmap (tpm,scale=scale,cluster_rows = F,
          cluster_cols = F,border_color=NULL,color=colorRampPalette(c("blue", "white", "red"))(102),
          cellheight=10,cellwidth=30,show_rownames=T,fontsize = 5,
          filename=paste(name,"_heatmap.pdf",sep=""))

pheatmap (tpm,scale=scale,cluster_rows = F,
          cluster_cols = F,border_color=NULL,color=colorRampPalette(c("blue", "white", "red"))(102),
          cellheight=10,cellwidth=30,show_rownames=T,fontsize = 5,
          filename=paste(name,"_heatmap.png",sep=""))


