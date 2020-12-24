

library(tm)
df=read.csv("Desktop/IVEY/Business statistics/group project/main.csv")
corpus<-Corpus(VectorSource(df$tags))
corpus<-tm_map(corpus,tolower)
inspect(corpus)
toSpace<-content_transformer(function(x,pattern){return(gsub(pattern, " ",x))})
corpus<-tm_map(corpus, toSpace,",")
dtm<-DocumentTermMatrix(corpus)
freq<-colSums(as.matrix(dtm))
freq
ord<-order(freq,decreasing=TRUE)
freq[head(ord)]
freq[ord]

'tdm <- TermDocumentMatrix(corpus)'
mydata.df <- as.data.frame(inspect(dtm))
count<- as.data.frame(rowSums(mydata.df))
count$word = rownames(count)
colnames(count) <- c("count","word" )
count<-count[order(count$count, decreasing=TRUE), ]
count
###############################################################

summary(df)
plot(df)
glimpse(df)
library(skimr)
skim(df)
#to remove an outlier-create a new df. df_n<-filter(df,name!=nj)
plot(df$units_sold,df$rating_perc)
###how to remove a column, i want to remove rating_perc
df1<-df[-c(1,2,24,10)]
summary(df)
skim(df1)
df1_notag<-df1[-c(13)] #removed tag
df1_notag<-df1_notag[-c(22)] #removed merchant title
df1_notag<-df1_notag[-c(12,17)]#removed badge_fast_shipping,shipping_is_express because of singularity
summary(df1_notag)
Fullmodel<-lm(units_sold ~ .,data=df1_notag)
summary(Fullmodel)
df1_sig<-df1_notag[c(3,5,9:13)]
sigmodel<-lm(units_sold ~ ., data=df1_sig)
summary(sigmodel)
library(ggcorrplot)
ggcorrplot(cor(df1_sig[,c(1:7)]), title = "Correlation Plot ")

library(tm)
corpus<-Corpus(VectorSource(df1_sig$product_color))
inspect(corpus)
corpus<-tm_map(corpus,removePunctuation)
corpus<-tm_map(corpus,content_transformer(tolower))
dtm<-DocumentTermMatrix(corpus)
inspect(dtm)
freq<-colSums(as.matrix(dtm))
length(freq)
ord<-order(freq,decreasing=TRUE)
freq[head(ord)]
color<-data.frame(freq)
order(color$freq)
color1
######################color/below is for tags##############
tc<-Corpus(VectorSource(df$tags))
inspect(tc)
toSpace<-content_transformer(function(x,pattern){return(gsub(pattern," ",x))})
tc<-tm_map(tc, toSpace,",")
tc<-tm_map(tc,removePunctuation)
tc<-tm_map(tc,content_transformer(tolower))
dtc<-DocumentTermMatrix(tc)
inspect(dtc)
freqc<-colSums(as.matrix(dtc))
length(freqc)
ordc<-order(freqc,decreasing=TRUE)
freqc[head(ord)]
tags<-data.frame(freqc)
######
library(skimr)
skim(main)
data.frame(colnames(main))##to find out how may columns are there
library(ggcorrplot)
cor(main[,c(5,22:59)])
ggcorrplot(cor(main[,c(5,22:59)]))
summary(step(lm(units_sold~.,main),direction ="both"))
