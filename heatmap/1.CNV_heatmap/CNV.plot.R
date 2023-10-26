
allargs<- commandArgs(trailingOnly = T)
input<-allargs[1]
output <-allargs[2]

dd<-read.delim(input,header=T,sep='\t',check.names = F)
info<-read.delim("plussize",header = F, sep="\t")

library(plotrix);
library(colorRamps);

dat<-c();
for(i in 1:nrow(dd) ) {
  for(j in 5:length(dd[i,]) ) {
    samp <- colnames(dd)[j]
    chr <- dd[i,1]
    if(chr )
      str <- dd[i,2]+info[which(info[,1]==chr),3]
    en <- dd[i,3]+info[which(info[,1]==chr),3]
    value <- dd[i,j]
    data <- data.frame(sample=samp,start=str,end=en,CN=value)
    dat <- rbind(dat,data)
  }
}

my_palette <- t(col2rgb(colorRampPalette(c("blue", "white", "red"))(n = 5)))
colors<-color.scale(as.numeric(dat$CN),my_palette[,1],my_palette[,2],my_palette[,3]);
color.legend<-color.gradient(my_palette[,1],my_palette[,2],my_palette[,3],nslices=30);
lev <- seq(-1,1,length.out = 30)
getcolor <- function(CNV)
{
  #print(CNV) #CNV最大为1，最小为-1,特殊处理CNV=1或则-1的情况
  if(CNV==-1){return(color.legend[1])}else if(CNV == 1){return(color.legend[30])}
  for(i in 1:length(lev)-1)
  {
    if( CNV>=lev[i] && CNV<=lev[i+1] )
    {
      return(color.legend[1+i])
    }
  }
}
#getcolor(0.03)
colors <- c()
for(cn in dat$CN){
  colors=c(colors,getcolor(cn))
}

dat$color <- colors;


len <- (ncol(dd)-4)

sample=colnames(dd)[5:ncol(dd)]
max = info[nrow(info),2] + info[nrow(info),3]
per=(max/60)
chr=info[,1]

pos=c(as.numeric((info[,2]/2+info[,3]))/per)
#pos
pos[c(16:22)] <- pos[c(16:22)]-0.1
pos[22] <- 60
pos[21] <- 58.7
pos[20] <- 57.3

png(paste0(output,'.png'),width=1600,height=(len*20 + 120)) #默认单位是像素
par(mar=c(4,20,4,4))
#这行代码创建一个空白的绘图区域，并设置了绘图区域的边界、标签等属性。
plot(1:len,1:len,type="n",ylim=c(0,len+1),xlim=c(0,60),bty="n",yaxt="n",xaxt="n",xlab="Chromosome",ylab="",xaxs="i");
#这行代码在绘图区域的左侧创建了一个垂直的刻度线，并设置了刻度线的位置、标签等属性。
axis(2,at=(c(1:len)-0.5),labels=sample,las=2,col = "NA", col.ticks = "NA");
#这行代码在绘图区域的底部创建了一个水平的刻度线，并设置了刻度线的位置、标签等属性
axis(1,at=(pos),labels=chr,las=1,col = "NA", col.ticks = "NA");
#这行代码在绘图区域的底部创建了另一个水平的刻度线，并设置了刻度线的位置、标签等属性。
axis(1,at=(unique(as.numeric(c(max,info[,3])))/per),labels=NA,las=1,col = "black", col.ticks = "black");
#这个for循环用于绘制矩形，其中每个矩形代表数据集dat中的一行。矩形的位置和颜色根据数据集中的不同列来确定
for (i in 1:nrow(dat)) {
  subset<-dat[i,];
  x.left<-subset$start/per;
  x.right<-subset$end/per;
  y.top<- which(subset$sample == sample);
  y.bottom <- y.top-1
  col<-as.character(subset$color);
  rect(x.left,y.bottom,x.right,y.top,col=col,border="NA");
}
#这几行代码用于绘制边框线，创建了一个边界框，限定了图形的显示范围
lines(c(0,60), c(0,0), type = 'l')
lines(c(0,60), c(len,len), type = 'l')
lines(c(0,0), c(0,len), type = 'l')
lines(c(60,60), c(0,len), type = 'l')
#这行代码调用了一个名为color.legend()的函数，用于在绘图区域中创建一个颜色图例。
#函数的参数解释如下：
#49 和 len+1 是图例矩形的左下角坐标，表示图例在绘图区域中的位置。
# 59 和 len+1.5 是图例矩形的右上角坐标，表示图例矩形的大小。
# c(-1, 1) 是定义图例矩形中的颜色范围。这里使用了一个长度为2的向量，表示颜色的起始值和结束值。
# color.legend 是一个变量，用于指定颜色图例的颜色样式。根据代码的上下文，它应该是一个颜色渐变样式的定义。
# gradient="x" 是一个可选参数，指定渐变的方向。在这里，"x"表示水平方向。
color.legend(49,len+1,59,len+1.5,c(-1,1),color.legend,gradient="x");
dev.off()


pdf(paste0(output,'.pdf'),width=1600/72,height=(len*20 + 120)/72) #默认单位是英寸，与像素的转换关系英寸=像素/72
par(mar=c(4,20,4,4))
plot(1:len,1:len,type="n",ylim=c(0,len+1),xlim=c(0,60),bty="n",yaxt="n",xaxt="n",xlab="Chromosome",ylab="",xaxs="i");
axis(2,at=(c(1:len)-0.5),labels=sample,las=2,col = "NA", col.ticks = "NA");
axis(1,at=(pos),labels=chr,las=1,col = "NA", col.ticks = "NA");
axis(1,at=(unique(as.numeric(c(max,info[,3])))/per),labels=NA,las=1,col = "black", col.ticks = "black");
for (i in 1:nrow(dat)) {
  subset<-dat[i,];
  x.left<-subset$start/per;
  x.right<-subset$end/per;
  y.top<- which(subset$sample == sample);
  y.bottom <- y.top-1
  col<-as.character(subset$color);
  rect(x.left,y.bottom,x.right,y.top,col=col,border="NA");
}
lines(c(0,60), c(0,0), type = 'l')
lines(c(0,60), c(len,len), type = 'l')
lines(c(0,0), c(0,len), type = 'l')
lines(c(60,60), c(0,len), type = 'l')
color.legend(49,len+1,59,len+1.5,c(-1,1),color.legend,gradient="x");
dev.off()
