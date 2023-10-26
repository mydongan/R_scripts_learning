args<-commandArgs(T)
if(length(args) != 1){
	cat('one argument must be needed\n')
	quit()
}
library("pheatmap")
dir()

tpm<-read.csv('cnv.pro.expression.xls',sep="\t",row.names = 1,header = TRUE,check.names = F)
name<-'cnv-pro'

tpm
if(length(colnames(tpm)) > 2){
	scale<-'row'
}else{
	scale<-'none'
}

#Ä¬ÈÏºá×Ý×ø±ê¾ÛÀà
pheatmap (log10(tpm+0.0001),scale=scale,border_color=NULL,color=colorRampPalette(c("blue", "white", "red"))(102),
          cellheight=3,cellwidth=45,show_rownames=T,fontsize = 5,
          filename=paste(name,"_heatmap_with_id.pdf",sep=""))
pheatmap (log10(tpm+0.0001),scale=scale,border_color=NULL,color=colorRampPalette(c("blue", "white", "red"))(102),
          cellheight=3,cellwidth=45,show_rownames=T,fontsize = 5,
          filename=paste(name,"_heatmap_with_id.png",sep=""))



pheatmap (log10(tpm+0.0001),scale=scale,cluster_rows = F,
          cluster_cols = F,border_color=NULL,color=colorRampPalette(c("blue", "white", "red"))(102),
          cellheight=10,cellwidth=30,show_rownames=T,fontsize = 5,
          filename=paste(name,"_heatmap_with_id.pdf",sep=""))

pheatmap (log10(tpm+0.0001),scale=scale,cluster_rows = F,
          cluster_cols = F,border_color=NULL,color=colorRampPalette(c("blue", "white", "red"))(102),
          cellheight=10,cellwidth=30,show_rownames=T,fontsize = 5,
          filename=paste(name,"_heatmap_with_id.png",sep=""))


