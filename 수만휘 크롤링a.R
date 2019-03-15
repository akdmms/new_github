if(!require(rvest)){install.packages('rvest');library(rvest)}
if(!require(httr)){install.packages('httr');library(httr)}
if(!require(RSelenium)){install.packages('RSelenium');library(RSelenium)}
if(!require(stringr)){install.packages('stringr');library(stringr)}


ch=wdman::chrome(port=4567L) ##포트 설정
remDr=remoteDriver(port=4567L, browserName='chrome')  ##크롬으로 실행
remDr$open()  ##열기
remDr$navigate('https://m.cafe.naver.com/ArticleList.nhn?search.clubid=10197921&search.menuid=3328&search.boardtype=L')  ##주소로 이동


a.addr <- c()    
i <- 1
while(1){   ##셀레니움 반복하기위해 사용
  i <- i + 1
  url_N<- remDr$getPageSource()[[1]]
  h <- read_html(url_N)
  source0 <- html_nodes(h,'strong.tit') %>% html_text()
  a.addr <- append(a.addr, source0)  
  
  
  if(length(source0) >500) break()
  element <- remDr$findElement(using="class",value="u_cbox_page_more")
  element$clickElement()
  Sys.sleep(0.1)
  print(i)
}

A<-unlist(source0)   ##N수생
A1<-data.frame(A)    ##N수생
B<-unlist(source0)   ##고3
B1<-data.frame(B)    ##고3
C<-unlist(source0)   ##고2
C1<-data.frame(C)    ##고2
D<-unlist(source0)   ##고1
D1<-data.frame(D)    ##고1
E<-unlist(source0)   ##중3
E1<-data.frame(E)    ##중3   중3은 모바일버전에서는 300개까지만 보여줌
############################
library(arules)
library(igraph)
library(combinat)
library(KoNLP)
library(RColorBrewer)
library(wordcloud)
gc()
useSejongDic()


write.table(D1,'smh.txt',row.names = F)
data<-readLines('smh.txt')
data1<-trimws(x =data)
data1
data2<-Filter(function(x){nchar(x)>=2},data1)
data2




tran<-Map(extractNoun,data2)
tran<-unique(tran)
tran<-sapply(tran,unique)
tran<-sapply(tran,function(x){Filter(function(y)
{nchar(y)<=4 && nchar(y)>1 &&is.hangul(y)},x)})
tran<-Filter(function(x){length(x)>=2},tran)
data11<-unlist(tran)
wordcount<-table(data11)



palete<-brewer.pal(9,'Set1')
x11()
wordcloud(
  names(wordcount),
  freq=wordcount,
  scale=c(5,1),
  rot.per=0.5,
  min.freq=2,
  random.order = F,
  random.color=T,
  colors=palete
)





