source("https://bioconductor.org/biocLite.R")
install.packages("UpSetR")
biocLite('UpSetR')
biocLite('ggplot2')
library(UpSetR)
library(ggplot2)
install.packages('UpSetR')
install.packages("UpSetR")
setRepositories()
biocLite('plyr')
biocLite('Rcpp')
library(plyr)
library(Rcpp)
setInternet2(TRUE)
help("Defunct")
version
install.packages("installr")
#library(installr)
#updateR()
movies <- read.csv(system.file("extdata", "movies.csv", package = "UpSetR"), 
                   header = T, sep = ";")
movies
dim(movies)
require(ggplot2); require(plyr); require(gridExtra); require(grid);

between <- function(row, min, max){
  newData <- (row["ReleaseDate"] < max) & (row["ReleaseDate"] > min)
}

plot1 <- function(mydata, x){
  myplot <- (ggplot(mydata, aes_string(x= x, fill = "color"))
             + geom_histogram() + scale_fill_identity()
             + theme(plot.margin = unit(c(0,0,0,0), "cm")))
}

plot2 <- function(mydata, x, y){
  myplot <- (ggplot(data = mydata, aes_string(x=x, y=y, colour = "color"), alpha = 0.5)
             + geom_point() + scale_color_identity()
             + theme_bw() + theme(plot.margin = unit(c(0,0,0,0), "cm")))
}

attributeplots <- list(gridrows = 55,
                       plots = list(list(plot = plot1, x= "ReleaseDate",  queries = FALSE),
                                    list(plot = plot1, x= "ReleaseDate", queries = TRUE),
                                    list(plot = plot2, x = "ReleaseDate", y = "AvgRating", queries = FALSE),
                                    list(plot = plot2, x = "ReleaseDate", y = "AvgRating", queries = TRUE)),
                       ncols = 3)

upset(movies, nsets = 7, nintersects = 30, mb.ratio = c(0.5, 0.5),
      order.by = c("freq", "degree"), decreasing = c(TRUE,FALSE))

upset(movies, sets = c("Drama", "Comedy", "Action", "Thriller", "Western", "Documentary"),
      queries = list(list(query = intersects, params = list("Drama", "Action")),
                     list(query = between, params = list(1970, 1980), color = "red", active = TRUE)))

upset(movies, attribute.plots = attributeplots,
      queries = list(list(query = between, params = list(1920, 1940)),
                     list(query = intersects, params = list("Drama"), color= "red"),
                     list(query = elements, params = list("ReleaseDate", 1990, 1991, 1992))),
      main.bar.color = "yellow")
upset(movies, nsets = 6, number.angles = 30, point.size = 3.5, line.size = 2, 
      mainbar.y.label = "Genre Intersections", sets.x.label = "Movies Per Genre", 
      text.scale = c(1.3, 1.3, 1, 1, 2, 0.75))
upset(movies, sets = c("Action", "Adventure", "Comedy", "Drama", "Mystery", 
                       "Thriller", "Romance", "War", "Western"), mb.ratio = c(0.55, 0.45), order.by = "freq")

upset(movies, sets = c("Action", "Adventure", "Comedy", "Drama", "Mystery", 
                       "Thriller", "Romance", "War", "Western"), mb.ratio = c(0.55, 0.45), order.by = "degree")

upset(movies, sets = c("Action", "Adventure", "Comedy", "Drama", "Mystery", 
                       "Thriller", "Romance", "War", "Western"), mb.ratio = c(0.55, 0.45), order.by = c("degree", 
                                                                                                        "freq"))
upset(movies, sets = c("Action", "Adventure", "Comedy", "Drama", "Mystery", 
                       "Thriller", "Romance", "War", "Western"), mb.ratio = c(0.55, 0.45), order.by = "freq", 
      keep.order = TRUE)

upset(movies, nintersects = 70, group.by = "sets", cutoff = 7)
upset(movies, empty.intersections = "on", order.by = "freq")
listInput <- list(one = c(1, 2, 3, 5, 7, 8, 11, 12, 13), two = c(1, 2, 4, 5, 

                                                                                                                                 10), three = c(1, 5, 6, 7, 8, 9, 10, 12, 13))
listInput
expressionInput <- c(one = 2, two = 1, three = 2, `one&two` = 1, `one&three` = 4, 
                     `two&three` = 1, `one&two&three` = 2)
expressionInput
upset(fromList(listInput), order.by = "freq")
?upset
upset(fromExpression(expressionInput), order.by = "freq")

setwd('F:/1-安冬/01-工作相关/02-工作/project/03_樱花')
dir()
ortholog<-read.csv('test2.csv', sep = ',')
head(ortholog)
upset(ortholog, nsets= 4, nintersects=NA,empty.intersections = "on", order.by = c("freq", "degree"), decreasing = c(TRUE,FALSE))
head(movies)
upset(ortholog, order.by = c("freq", "degree"), decreasing = c(TRUE,FALSE))


setwd('K:/1-安冬/欧易项目/售后项目/20231110-HT2020-12449-倪培燕-人重/Analysis/5.upsetR')
dir()
data<-read.delim("iPSC.mutantGene.xls",header = T,row.names = 1)
head(data)
pdf('iPSC_germline_mutantGene.upset.pdf')
upset(data, nsets= 10, nintersects=30,
      empty.intersections = "on", 
      order.by = c("freq", "degree"), 
      decreasing = c(TRUE,FALSE),
      sets.bar.color = rainbow(10),
      set_size.show = TRUE, #显示横bar数字，但不能显示全，不知道怎么搞
      set_size.numbers_size = 5.5, #横bar数字大小
      mb.ratio = c(0.6, 0.4),
      group.by = "degree") #数据按照“degree”或“set”来排序 
#?upset
dev.off()


datnum <- length(colnames(data))
pdf("iPSC_germline_mutantGene.upset.pdf", width=11, height=7, onefile=F)
upset(data, nsets=datnum, nintersects=30, 
      #number.angles=30, 
      point.size=2, line.size=1,
      mainbar.y.label="Intersection gene number", 
      sets.x.label="Total gene number", 
      text.scale=c(1.3,1.3,1,1,1.3,0.8), 
      mb.ratio=c(0.55, 0.45), order.by="freq", 
      show.numbers="yes", 
      sets.bar.color=rainbow(datnum),
      set_size.show = TRUE)
dev.off()

png("iPSC_germline_mutantGene.upset.png", width=3300, height=2100, res=300)
upset(data, nsets=datnum, nintersects=30, 
      #number.angles=30, 
      point.size=2, line.size=1,
      mainbar.y.label="Intersection gene number", 
      sets.x.label="Total gene number", 
      text.scale=c(1.3,1.3,1,1,1.3,0.8), 
      mb.ratio=c(0.55, 0.45), order.by="freq", 
      show.numbers="yes", 
      sets.bar.color=rainbow(datnum),
      set_size.show = TRUE)
dev.off()
upset(data, nsets=10,  #数据有8个组
      #sets = c("ET","DT","CH","CG","BH","BG","AH","AG"), #横bar的顺序
      nintersects = 30,    #设定显示多少个竖bar
      order.by = c("freq","degree"), #柱形图按照什么排序，freq代表频率，degree代表点连接的数量
      decreasing = c("T","F"), #上面两个选项的升降选项
      keep.order = T,   #横bar的顺序，默认是按照size，如果选T则按字母
      matrix.color = "blue",  #点的颜色
      #main.bar.color = "#F8766D", #竖bar颜色，#F8766D是R默认的红色，#00BFC4，#00BA38, #619CFF
      sets.bar.color = rainbow(10), #横bar颜色
      shade.alpha = 0.4, #点图中阴影深浅
      matrix.dot.alpha = 1, #灰点的透明值
      mainbar.y.label = "Shared OTU numbers", #竖bar名
      sets.x.label = "Total OTUs numbers", #横bar名
      point.size = 2.2, #点的大小
      line.size = 0.7, #线的粗细
      mb.ratio = c(0.7, 0.3), #竖bar图和点图的比例
      number.angles = 0,   #竖bar数字角度
      att.pos = "bottom",   #位置
      group.by = "degree", #数据按照“degree”或“set”来排序
      scale.intersections = "identity",  #竖bar度量方法，"identity","log10","log2"
      scale.sets = "identity",  #横bar度量方法，"identity","log10","log2"
      text.scale = 1, #字体大小
      set_size.show = TRUE, #显示横bar数字，但不能显示全，不知道怎么搞
      set_size.numbers_size = 6.5   #横bar数字大小
)
