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

setwd('F:/1-����/01-�������/02-����/project/03_ӣ��')
dir()
ortholog<-read.csv('test2.csv', sep = ',')
head(ortholog)
upset(ortholog, nsets= 4, nintersects=NA,empty.intersections = "on", order.by = c("freq", "degree"), decreasing = c(TRUE,FALSE))
head(movies)
upset(ortholog, order.by = c("freq", "degree"), decreasing = c(TRUE,FALSE))


setwd('K:/1-����/ŷ����Ŀ/�ۺ���Ŀ/20231110-HT2020-12449-������-����/Analysis/5.upsetR')
dir()
data<-read.delim("iPSC.mutantGene.xls",header = T,row.names = 1)
head(data)
pdf('iPSC_germline_mutantGene.upset.pdf')
upset(data, nsets= 10, nintersects=30,
      empty.intersections = "on", 
      order.by = c("freq", "degree"), 
      decreasing = c(TRUE,FALSE),
      sets.bar.color = rainbow(10),
      set_size.show = TRUE, #��ʾ��bar���֣���������ʾȫ����֪����ô��
      set_size.numbers_size = 5.5, #��bar���ִ�С
      mb.ratio = c(0.6, 0.4),
      group.by = "degree") #���ݰ��ա�degree����set�������� 
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
upset(data, nsets=10,  #������8����
      #sets = c("ET","DT","CH","CG","BH","BG","AH","AG"), #��bar��˳��
      nintersects = 30,    #�趨��ʾ���ٸ���bar
      order.by = c("freq","degree"), #����ͼ����ʲô����freq����Ƶ�ʣ�degree��������ӵ�����
      decreasing = c("T","F"), #��������ѡ�������ѡ��
      keep.order = T,   #��bar��˳��Ĭ���ǰ���size�����ѡT����ĸ
      matrix.color = "blue",  #�����ɫ
      #main.bar.color = "#F8766D", #��bar��ɫ��#F8766D��RĬ�ϵĺ�ɫ��#00BFC4��#00BA38, #619CFF
      sets.bar.color = rainbow(10), #��bar��ɫ
      shade.alpha = 0.4, #��ͼ����Ӱ��ǳ
      matrix.dot.alpha = 1, #�ҵ��͸��ֵ
      mainbar.y.label = "Shared OTU numbers", #��bar��
      sets.x.label = "Total OTUs numbers", #��bar��
      point.size = 2.2, #��Ĵ�С
      line.size = 0.7, #�ߵĴ�ϸ
      mb.ratio = c(0.7, 0.3), #��barͼ�͵�ͼ�ı���
      number.angles = 0,   #��bar���ֽǶ�
      att.pos = "bottom",   #λ��
      group.by = "degree", #���ݰ��ա�degree����set��������
      scale.intersections = "identity",  #��bar����������"identity","log10","log2"
      scale.sets = "identity",  #��bar����������"identity","log10","log2"
      text.scale = 1, #�����С
      set_size.show = TRUE, #��ʾ��bar���֣���������ʾȫ����֪����ô��
      set_size.numbers_size = 6.5   #��bar���ִ�С
)
