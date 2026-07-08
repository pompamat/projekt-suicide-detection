getwd()
#importujemy dane do ramki danych
suicide_detection <- read.csv("Suicide_Detection_mini_mini.csv")
#suicide_detection[1:50 000]

str(suicide_detection)
str(suicide_detection[,c(2,3)])

#pozbycie sie pierwszej kolumny
suicide_data <- suicide_detection[,c(2,3)]
str(suicide_data)


#konwerujemy class(suicide/non-suicide) na factor
suicide_data$class <- factor(suicide_data$class)
table(suicide_data$class)
#non_suiside=373, suicide=371

library(tm)
#utworzenie korpusu danych, jako zbiór tekstów
#data_corpus<-VCorpus(VectorSource(Data$text))
suicide_corpus <- VCorpus(VectorSource(suicide_data$text))
print(suicide_corpus)
#korpus zawiera 744 wiadomości tekstowych
#inspect(suicide_corpus[1:3])

as.character(suicide_corpus[[2]])

#czyszczenie korpusu

#usuwamy litery
suicide_corpus_clean <- tm_map(suicide_corpus, removeNumbers)

#sprawdzamy czy działa
as.character(suicide_corpus[[2]])
as.character(suicide_corpus_clean[[2]])

#usuwamy białe znaki
suicide_corpus_clean <- tm_map(suicide_corpus_clean, stripWhitespace)

#duże litery -> na małe litery
suicide_corpus_clean <- tm_map(suicide_corpus_clean, content_transformer(tolower))

#sprawdzamy czy działa
as.character(suicide_corpus[[8]])
as.character(suicide_corpus_clean[[8]])

stopwords()
suicide_corpus_clean<-tm_map(suicide_corpus_clean,removeWords,stopwords())

#sprawdzamy
as.character(suicide_corpus[[8]])
as.character(suicide_corpus_clean[[8]])


as.character(suicide_corpus[[70]])
as.character(suicide_corpus_clean[[70]])

suicide_corpus_clean <- tm_map(suicide_corpus_clean, stripWhitespace)

suicide_corpus_clean <- tm_map(suicide_corpus_clean,removePunctuation)

stopwordsextra9 <- c("them","done","more","ing","ll","never","idk","past","du","everything","whenever","say","used","op","like","gets")
stopwordsextra8 <- c("dont","just","now","one","due","yet"," ive","ive ","get","why","hes","all","when","isnt","still","con")
stopwordsextra7 <- c("really","during","their","own","because","what","was","ever","isn't","had","up","down","once","from","she","he","i","you","told","without")
stopwordsextra6 <- c("want","if","going","go","goes","when","doesn't","here","don't"," 'll","'ll","emojis","new","old","bot","whether","memes","let","gather")
stopwordsextra5 <- c("won't","by","then","but","will","a","said","would","have","least","thus","everyone","mani","bit","every","always")
stopwordsextra4 <- c("one","with","at","very","off","how","on","do","am","be","has","you","could"," 's","else","there","receny","s","u","i")
stopwordsextra3 <- c("it","of","for","to","don't","much","been","so","in","not","or","any","can","now","can't","many","much")
stopwordsextra2 <- c("ive","cant","just","got","this","also","even","some","hes","theres","since","shes","wont","its","thats","that","the","anyone","dont","and")
stopwordsextra <- c("i'm","she's","is","are","im","my","mine","her","his","i","me","my","myself","we","our","ours","can't","didn't","didnt","i've")


suicide_corpus_clean<-tm_map(suicide_corpus_clean,removeWords,stopwordsextra)
suicide_corpus_clean<-tm_map(suicide_corpus_clean,removeWords,stopwordsextra2)
suicide_corpus_clean<-tm_map(suicide_corpus_clean,removeWords,stopwordsextra3)
suicide_corpus_clean<-tm_map(suicide_corpus_clean,removeWords,stopwordsextra4)
suicide_corpus_clean<-tm_map(suicide_corpus_clean,removeWords,stopwordsextra5)
suicide_corpus_clean<-tm_map(suicide_corpus_clean,removeWords,stopwordsextra6)
suicide_corpus_clean<-tm_map(suicide_corpus_clean,removeWords,stopwordsextra7)
suicide_corpus_clean<-tm_map(suicide_corpus_clean,removeWords,stopwordsextra8)
suicide_corpus_clean<-tm_map(suicide_corpus_clean,removeWords,stopwordsextra9)


#zle dziala
#suicide_corpus_clean <- tm_map(suicide_corpus_clean, stemDocument)

#suicide_corpus_clean <- tm_map(suicide_corpus_clean,removeWords,stopwords())
#koniec czyszczenia

suicide_corpus_clean <- tm_map(suicide_corpus_clean, stripWhitespace)
#zaczynamy zabawę
print("hello stop it!")
#

#tworzymy macierz dla naszego korpusu ktora przechowuje ilosc slow
macierz_dtm <- DocumentTermMatrix(suicide_corpus_clean)
macierz_dtm

#tworzymy zbiór uczący i testowy
#dl<-length(suicide_data[,1])
#dl
#tr<-0.75*dl
#tr
#te<-0.25*dl
#te
#trte<-tr+1
#trte

#70% 30%
a1=650
a2=a1+1
suicide_train<-macierz_dtm[1:a1,]
suicide_test<-macierz_dtm[a2:744,]
#744
#zapisujemy etykiety dla zbioru ucz i dla test
suicide_train_labels<-suicide_detection[1:a1,]$class
suicide_test_lables<-suicide_detection[a2:744,]$class

#sprawdzamy czy odsetek suicide jest podobny w zb ucz jak i test
prop.table(table(suicide_train_labels))
prop.table(table(suicide_test_lables))


library(wordcloud)
wordcloud(suicide_corpus_clean, min.freq = 40, random.order = FALSE)

#dzielimy dane na suicide oraz non-suicide
suicide <- subset(suicide_detection, class == "suicide")
non_suicide <- subset(suicide_detection, class == "non-suicide")


#wordcloud(suicide$text, min.freq = 25, scale = c(2,0.6))
#wordcloud(non_suicide$text, min.freq = 25, scale = c(2,0.6))
#
#tutaj
#

#zapisujemy słowa, które występują co najmniej 15 razy w zb uczacym
dane_freq_words <- findFreqTerms(suicide_train,6)
str(dane_freq_words)
dane_freq_words


#tworzymy macierz DTM
dane_dtm_freq_train <- suicide_train[,dane_freq_words]
dane_dtm_freq_test <- suicide_test[,dane_freq_words]

#funcja ktora konwertuje liczbe wyrazu w wiadmosci na yes jesli wystepuje no jesli nie

convert_counts <- function(x) {
  x<-ifelse(x>0,"Yes","No")
  return(x)
}

#stosujemy powyzsza funkcje do kolumn
#tu juz za dlugo dziala
nowe_dane_train <- apply(dane_dtm_freq_train, MARGIN = 2, convert_counts)
nowe_dane_test <- apply(dane_dtm_freq_test, MARGIN = 2, convert_counts)

length(suicide_train)
length(suicide_test)

#budujemy model do przewidywania typu wiadomosci
library(e1071)
dane_classifier<-naiveBayes(nowe_dane_train,suicide_train_labels)

dane_test_pred<-predict(dane_classifier, nowe_dane_test)

library(gmodels)
CrossTable(dane_test_pred, suicide_test_lables,
           prop.chisq=FALSE, prop.c=FALSE, prop.r=FALSE,
           dnn=c('predicted','actual'))

# 2031  802
# 212   1455

#spróbujmy z wygładzeniem Laplace'a

dane_classifier2<-naiveBayes(nowe_dane_train, suicide_train_labels, laplace=1)

dane_test_pred2<-predict(dane_classifier2, nowe_dane_test)

CrossTable(dane_test_pred2, suicide_test_lables,
           prop.chisq = FALSE, prop.c=FALSE, prop.r=FALSE,
           dnn=c('predicted','actual'))

