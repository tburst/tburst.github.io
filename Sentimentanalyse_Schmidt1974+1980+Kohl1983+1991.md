1. Vorbereitung der Sentimentanalyse
------------------------------------

### 1.1. Laden des Sentiment-Wörterbuchs

Laden der notwendigen Libraries

``` r
library(quanteda)
library(stringr)
library(ggplot2)
library(gridExtra)
library(plyr)
library(grid)
```

Laden der vordefinierten deutschen Sentiment-Wortlisten als Gesamttext (Download unter <http://wortschatz.uni-leipzig.de/download/SentiWS_v1.8c.zip> )

``` r
positive_sentiment<-textfile(
                      "C:\\JustTalk\\GermanSentimentWords\\SentiWS_v1.8c_Positive.txt", 
                       encoding="Utf-8")

negative_sentiment<-textfile(
                      "C:\\JustTalk\\GermanSentimentWords\\SentiWS_v1.8c_Negative.txt", 
                       encoding="Utf-8")

#Beispiel
unlist(strsplit(negative_sentiment@texts, '\n'))[[1]]
```

    ## [1] "Abbau|NN\t-0.058\tAbbaus,Abbaues,Abbauen,Abbaue"

Loop über die positive-sentiment Datei und speichern der Wörter in einem data.frame

``` r
#For loop über jede Zeile der Positiv-Sentiment-Textdatei und übertragen der Daten 
#in ein Dataframe Format.Jede Zeile enthält ein Wort, dessen "Stimmung" und die Wortart.

positiv_split <-unlist(strsplit(positive_sentiment@texts,"\n"))

positive_sentiment_list <- data.frame("Wort"=character(0), "Wortart" = character(0), 
                                      "Sentiment" = numeric(0))


for (positive_word in positiv_split ){
  #Abtrennen des Hauptwortes aus Stringzeile
  word <- str_match(positive_word,"(\\w+)\\|")[,2]
  #Speichern der Bedeutung
  sentiment = str_match(positive_word, "\\s+(\\-?\\d+.\\d+)")[,2]
  sentiment <- as.numeric(sentiment)
  #Speichern der Wortart
  wordart <- str_match(positive_word, "\\|(\\w+?)\\s")[,2]
  #Merge alle Eigenschaften in einem data.frame
  positive_sentiment_list<- rbind(positive_sentiment_list, 
                                  data.frame("Wort"=word, 
                                             "Sentiment"=sentiment,
                                             "Wortart"=wordart))
  if (length(unlist(strsplit(positive_word, "\t" ))) >= 3){
    additional_wordlist = unlist(strsplit(positive_word, "\t" ))[3]
    additional_wordlist = unlist(strsplit(additional_wordlist,","))
    for ( additional_word in additional_wordlist){
      word <- additional_word
      positive_sentiment_list<- rbind(positive_sentiment_list,
                                      data.frame("Wort" =word,
                                                 "Sentiment"=sentiment,
                                                 "Wortart"=wordart))
    }
  }
}
```

Loop über die negativ-sentiment Datei und speichern der Wörter in einem data.frame

``` r
#For loop über jede Zeile der Negative-Sentiment-Textdatei und übertragen der Daten 
#in ein Dataframe Format. Jede Zeile enthält ein Wort, dessen "Stimmung" und die Wortart.

negative_split <-unlist(strsplit(negative_sentiment@texts,"\n"))

negative_sentiment_list <- data.frame("Wort"=character(0), 
                                      "Wortart" = character(0),
                                      "Sentiment" = numeric(0))


for (negative_word in negative_split ){
  #Abtrennen des Hauptwortes aus Stringzeile
  word <- str_match(negative_word,"(\\w+)\\|")[,2]
  #Speichern der Bedeutung
  sentiment = str_match(negative_word, "\\s+(\\-?\\d+.\\d+)")[,2]
  sentiment <- as.numeric(sentiment)
  #Speichern der Wortart
  wordart <- str_match(negative_word, "\\|(\\w+?)\\s")[,2]
  #Merge alle Eigenschaften in einem data.frame
  negative_sentiment_list<- rbind(negative_sentiment_list, 
                                  data.frame("Wort"=word,
                                             "Sentiment"=sentiment,
                                             "Wortart"=wordart))
  if (length(unlist(strsplit(negative_word, "\t" ))) >= 3){
    additional_wordlist = unlist(strsplit(negative_word, "\t" ))[3]
    additional_wordlist = unlist(strsplit(additional_wordlist,","))
    for ( additional_word in additional_wordlist){
      word <- additional_word
      negative_sentiment_list<- rbind(negative_sentiment_list, 
                                      data.frame("Wort"=word,
                                                 "Sentiment"=sentiment,
                                                 "Wortart"=wordart))
    }
  }
}
```

Zusammenfügen des positiven und des negativen sentiment-dataframes

``` r
sentiment_list<- rbind(negative_sentiment_list, positive_sentiment_list)
sentiment_list <- rbind(sentiment_list,data.frame("Wort" = c("Ängste", "Ängsten"), 
                                                  "Sentiment" =  c(-0.5140,-0.5140), 
                                                  "Wortart" = c("NN","NN")))

#Beispiel
sentiment_list[1:10,]
```

    ##         Wort Sentiment Wortart
    ## 1      Abbau   -0.0580      NN
    ## 2     Abbaus   -0.0580      NN
    ## 3    Abbaues   -0.0580      NN
    ## 4    Abbauen   -0.0580      NN
    ## 5     Abbaue   -0.0580      NN
    ## 6    Abbruch   -0.0048      NN
    ## 7  Abbruches   -0.0048      NN
    ## 8   Abbrüche   -0.0048      NN
    ## 9   Abbruchs   -0.0048      NN
    ## 10 Abbrüchen   -0.0048      NN

``` r
str(sentiment_list)
```

    ## 'data.frame':    31283 obs. of  3 variables:
    ##  $ Wort     : Factor w/ 31084 levels "Abbau","Abbaus",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Sentiment: num  -0.058 -0.058 -0.058 -0.058 -0.058 -0.0048 -0.0048 -0.0048 -0.0048 -0.0048 ...
    ##  $ Wortart  : Factor w/ 4 levels "NN","VVINF","ADJX",..: 1 1 1 1 1 1 1 1 1 1 ...

### 1.2. Laden der Regierungserklärungen

Laden der eigenen Regierungserklärung als Textdatei und als einfache character variable

``` r
schmidt1980 <-textfile("C:\\JustTalk\\Regierungserklärungen\\1980_Schmidt_SPD.txt")
schmidt1980_plaintext <- schmidt1980@texts

schmidt1974 <-textfile("C:\\JustTalk\\Regierungserklärungen\\Schmidt_1974.txt",
                       encoding="Utf-8")
schmidt1974_plaintext <- schmidt1974@texts

kohl1983 <-textfile("C:\\JustTalk\\Regierungserklärungen\\1983_Helmut_Kohl_CDU.txt",
                    encoding="Utf-8")
kohl1983_plaintext <- kohl1983@texts

kohl1991 <-textfile("C:\\JustTalk\\Regierungserklärungen\\1991_Kohl_CDU.txt")
kohl1991_plaintext <- kohl1991@texts
```

Erstellen eines Corpus

``` r
mycorpus <- corpus(c(schmidt1974_plaintext,
                     schmidt1980_plaintext,
                     kohl1983_plaintext,
                     kohl1991_plaintext))
docvars(mycorpus, "Bundeskanzler") <- c("Schmidt","Schmidt","Kohl", "Kohl")
docvars(mycorpus, "Jahr") <- c("1974","1980","1983", "1991")
summary(mycorpus)
```

    ## Corpus consisting of 4 documents.
    ## 
    ##   Text Types Tokens Sentences Bundeskanzler Jahr
    ##  text1  2464   9069       456       Schmidt 1974
    ##  text2  3313  12202       669       Schmidt 1980
    ##  text3  3210  13293       818          Kohl 1983
    ##  text4  3827  17456       866          Kohl 1991
    ## 
    ## Source:  C:/JustTalk/* on x86-64 by Tobi
    ## Created: Wed Aug 10 12:43:11 2016
    ## Notes:

2. Analyse der Corpus-Daten
---------------------------

### 2.1.Integration von Sentiment-Daten in Corpus

Erstellen eines Sentiment-Dict und einer Document-Matrix

``` r
positive_sentiment <- sentiment_list$Wort[sentiment_list$Sentiment > 0]
negative_sentiment <- sentiment_list$Wort[sentiment_list$Sentiment < 0]

positive_sentiment_list <-  as.vector(positive_sentiment )
negative_sentiment_list <-  as.vector(negative_sentiment )

my_dict <- dictionary(list("positiv"=positive_sentiment_list ,
                           "negativ" = negative_sentiment_list))

mydfm_dict <- dfm(mycorpus, dictionary = my_dict)
```

    ## Creating a dfm from a corpus ...
    ##    ... lowercasing
    ##    ... tokenizing
    ##    ... indexing documents: 4 documents
    ##    ... indexing features: 7,440 feature types
    ##    ... applying a dictionary consisting of 2 keys
    ##    ... created a 4 x 2 sparse dfm
    ##    ... complete. 
    ## Elapsed time: 30.22 seconds.

``` r
mymatrix_dict <- as.data.frame(mydfm_dict)
mymatrix_dict$Bundeskanzler <- c("Schmidt","Schmidt","Kohl", "Kohl")
mymatrix_dict$Jahr <- c("1974","1980","1983", "1991")
mymatrix_dict$Partei <- c("SPD","SPD","CDU", "CDU")

mymatrix_dict
```

    ##       positiv negativ Bundeskanzler Jahr Partei
    ## text1     681     130       Schmidt 1974    SPD
    ## text2     842     222       Schmidt 1980    SPD
    ## text3    1087     236          Kohl 1983    CDU
    ## text4    1518     316          Kohl 1991    CDU

### 2.2.Verhältnis positiver Wörter zu negativen Wörtern

``` r
ggplot(aes(x=Jahr,y=positiv/negativ), data = mymatrix_dict ) + 
        geom_bar(stat = "identity",aes(fill=Bundeskanzler)) + 
        labs(x = "Jahr", y = "Anzahl positiver Wörter pro negativem Wort", 
             title = "Anzahl positiver Wörter im Verhältnis zu negativen Wörtern im      
             Gesamttext")+
        scale_fill_manual(values = c("Schmidt" = "#F44336", "Kohl" = "grey23"))+
        ylim(c(0,7))+
        geom_text(aes(y=positiv/negativ,
                      ymax=positiv/negativ, 
                      label=round(positiv/negativ,2)),
                  position= position_dodge(width=0.9), vjust=-.5)
```

![](Sentimentanalyse_Schmidt1974+1980+Kohl1983+1991_files/figure-markdown_github/unnamed-chunk-9-1.png)

### 2.2.Sentiment-Wertung einzelner Sätze

Regierungserklärung in Sätze aufteilen und Sentiment-Wertungen zuordnen.

``` r
#Schmidt 1974

#Trennen des Textes nach jedem Punkt. 
#Umwandeln der Liste in eine einfach character Variable für den foor-loop
text_split <- unlist(strsplit(schmidt1974@texts, "\\."))
#Erstellen eines leeren Dataframes zum abspeichern der Ergebnisse des foor-loops
text_dataframe_schmidt1974 <- data.frame("sentence" = character(0),
                                         "sentiment" = numeric(0))

for (sentence in text_split){
  #Für jeden betrachteten Satz wird eine leere sentiment variable erstellt
  sentiment <- vector()
  for ( word in unlist(strsplit(sentence," "))){
    #Für jedes Wort im betrachteten Satz werden 
    #zunächst potentiell anhängende Satzzeichen entfernt
    word_strip <- gsub("\n","",gsub(":","",gsub(";","",gsub(",", "", word))))
    #Wenn das Wort in dem Sentiment-Wörterbuch vorkommt, 
    #wird in der sentiment Variable die zugehörige Sentiment Wertung abgespeichert
    if ( word_strip %in% sentiment_list$Wort){
      sentiment <- append(sentiment,
                          sentiment_list$Sentiment[sentiment_list$Wort == word_strip] )
    }
  }
  #Nach dem loop über jedes Wort im betrachteten Satz wird 
  #die Summe aus allen gefundenen Sentiment-Wertungen genommen
  sentiment_result <- sum(sentiment)
  #Hinzufügen des betrachteten Satzes und 
  #der berechneten Sentiment Wertung zum dataframe
  text_dataframe_schmidt1974  <- rbind(text_dataframe_schmidt1974 , 
                                       data.frame("Sentence" =sentence,
                                                  "sentiment" = sentiment_result))
}


#Schmidt 1980

text_split <- unlist(strsplit(schmidt1980@texts, "\\."))
text_dataframe_schmidt1980  <- data.frame("sentence" = character(0),
                                          "sentiment" = numeric(0))

for (sentence in text_split){
  sentiment <- vector()
  for ( word in unlist(strsplit(sentence," "))){
    word_strip <- gsub("\n","",gsub(":","",gsub(";","",gsub(",", "", word))))
    if ( word_strip %in% sentiment_list$Wort){
      sentiment <- append(sentiment,
                          sentiment_list$Sentiment[sentiment_list$Wort == word_strip] )
    }
  }
  sentiment_result <- sum(sentiment)
  text_dataframe_schmidt1980 <- rbind(text_dataframe_schmidt1980, 
                                      data.frame("Sentence" =sentence, 
                                                 "sentiment" = sentiment_result))
}


#Kohl 1983

text_split <- unlist(strsplit(kohl1983@texts, "\\."))
text_dataframe_kohl1983  <- data.frame("sentence" = character(0),
                                       "sentiment" = numeric(0))

for (sentence in text_split){
  sentiment <- vector()
  for ( word in unlist(strsplit(sentence," "))){
    word_strip <- gsub("\n","",gsub(":","",gsub(";","",gsub(",", "", word))))
    if ( word_strip %in% sentiment_list$Wort){
      sentiment <- append(sentiment,
                          sentiment_list$Sentiment[sentiment_list$Wort == word_strip] )
    }
  }
  sentiment_result <- sum(sentiment)
  text_dataframe_kohl1983 <- rbind(text_dataframe_kohl1983, 
                                   data.frame("Sentence" =sentence,
                                              "sentiment" = sentiment_result))
}


#Kohl 1991

text_split <- unlist(strsplit(kohl1991@texts, "\\."))
text_dataframe_kohl1991  <- data.frame("sentence" = character(0),
                                       "sentiment" = numeric(0))

for (sentence in text_split){
  sentiment <- vector()
  for ( word in unlist(strsplit(sentence," "))){
    word_strip <- gsub("\n","",gsub(":","",gsub(";","",gsub(",", "", word))))
    if ( word_strip %in% sentiment_list$Wort){
      sentiment <- append(sentiment,
                          sentiment_list$Sentiment[sentiment_list$Wort == word_strip] )
    }
  }
  sentiment_result <- sum(sentiment)
  text_dataframe_kohl1991 <- rbind(text_dataframe_kohl1991, 
                                   data.frame("Sentence" =sentence,
                                              "sentiment" = sentiment_result))
}
```

### 2.2.1. Schmidt 1974

``` r
#Abspeichern aller Satz-Indizes, in denen 
#eine Sentiment-Wertung von ungleich Null gefunden wurde
sentence_index <- as.numeric(
                    row.names(
                      text_dataframe_schmidt1974[
                          text_dataframe_schmidt1974$sentiment != 0,]))
#Abspeichern aller Sätze, in denen eine Sentiment-Wertung von ungleich Null gefunden wurde
sentences <- text_dataframe_schmidt1974$Sentence[
                                        text_dataframe_schmidt1974$sentiment != 0]
#Abspeichern aller Sentiment-Wertungen, in denen 
#eine Sentiment-Wertung von ungleich Null gefunden wurde
sentiment <- text_dataframe_schmidt1974$sentiment[
                                        text_dataframe_schmidt1974$sentiment != 0]

#Zusammenfügen der variablen in einem dataframe
sentiment_sentences_schmidt1974 <- do.call(rbind, Map(data.frame, "Sentence"=sentences,
                                                      "Sentiment"=sentiment,
                                                      "Index" = sentence_index))

ggplot(aes(x = sentence_index, y= sentiment),data=sentiment_sentences_schmidt1974) + 
      geom_point() + 
      ylim(c(-1,1)) + 
      labs(x = "Satz-Index", y = "Stimmung", 
           title = "Stimmung der Sätze im Verlauf der Rede - Schmidt1974") + 
      geom_smooth()
```

![](Sentimentanalyse_Schmidt1974+1980+Kohl1983+1991_files/figure-markdown_github/unnamed-chunk-11-1.png)

### 2.2.2. Schmidt 1980

``` r
sentence_index <- as.numeric(
                    row.names(
                      text_dataframe_schmidt1980[
                          text_dataframe_schmidt1980$sentiment != 0,]))
sentences <- text_dataframe_schmidt1980$Sentence[
                                        text_dataframe_schmidt1980$sentiment != 0]
sentiment <- text_dataframe_schmidt1980$sentiment[
                                        text_dataframe_schmidt1980$sentiment != 0]

sentiment_sentences_schmidt1980 <- do.call(rbind, Map(data.frame, "Sentence"=sentences,
                                                      "Sentiment"=sentiment, 
                                                      "Index" = sentence_index))

ggplot(aes(x = sentence_index, y= sentiment), data=sentiment_sentences_schmidt1980) + 
      geom_point() + 
      ylim(c(-1,1)) + 
      labs(x = "Satz-Index", y = "Stimmung", 
           title = "Stimmung der Sätze im Verlauf der Rede - Schmidt1980") + 
      geom_smooth()
```

![](Sentimentanalyse_Schmidt1974+1980+Kohl1983+1991_files/figure-markdown_github/unnamed-chunk-12-1.png)

### 2.2.3. Kohl 1983

``` r
sentence_index <- as.numeric(
                    row.names(
                      text_dataframe_kohl1983[
                          text_dataframe_kohl1983$sentiment != 0,]))
sentences <- text_dataframe_kohl1983$Sentence[
                                     text_dataframe_kohl1983$sentiment != 0]
sentiment <- text_dataframe_kohl1983$sentiment[
                                     text_dataframe_kohl1983$sentiment != 0]

sentiment_sentences_kohl1983 <- do.call(rbind, Map(data.frame, "Sentence"=sentences,
                                                   "Sentiment"=sentiment,
                                                   "Index" = sentence_index))

ggplot(aes(x = sentence_index, y= sentiment), data=sentiment_sentences_kohl1983) + 
      geom_point() + 
      ylim(c(-1,1)) + 
      labs(x = "Satz-Index", y = "Stimmung",
           title = "Stimmung der Sätze im Verlauf der Rede - Kohl1983") + 
      geom_smooth()
```

![](Sentimentanalyse_Schmidt1974+1980+Kohl1983+1991_files/figure-markdown_github/unnamed-chunk-13-1.png)

### 2.2.4. Kohl 1991

``` r
sentence_index <- as.numeric(
                    row.names(
                      text_dataframe_kohl1991[
                          text_dataframe_kohl1991$sentiment != 0,]))
sentences <- text_dataframe_kohl1991$Sentence[
                                     text_dataframe_kohl1991$sentiment != 0]
sentiment <- text_dataframe_kohl1991$sentiment[
                                     text_dataframe_kohl1991$sentiment != 0]

sentiment_sentences_kohl1991 <- do.call(rbind, Map(data.frame, "Sentence"=sentences,
                                                   "Sentiment"=sentiment,
                                                   "Index" = sentence_index))

ggplot(aes(x = sentence_index, y= sentiment), data=sentiment_sentences_kohl1991) + 
      geom_point() + 
      ylim(c(-1,1)) + 
      labs(x = "Satz-Index", y = "Stimmung",
           title = "Stimmung der Sätze im Verlauf der Rede - Kohl1991") +
      geom_smooth()
```

![](Sentimentanalyse_Schmidt1974+1980+Kohl1983+1991_files/figure-markdown_github/unnamed-chunk-14-1.png)

### 2.3.Sentiment-Wertung einzelner Sätze - stimmungsstarke Sätze

Erstellen neuer Dataframes. Für jede Rede sind nur stärker stimmungsgeladene Sätze enthalten (Größer/Kleiner 0,25)

``` r
sentiment_sentences_schmidt1974_polarisierend <- subset(sentiment_sentences_schmidt1974,
                                                        Sentiment >= 0.25 | 
                                                        Sentiment <= -0.25  )
sentiment_sentences_schmidt1980_polarisierend <- subset(sentiment_sentences_schmidt1980,
                                                        Sentiment >= 0.25 | 
                                                        Sentiment <= -0.25  )
sentiment_sentences_kohl1983_polarisierend    <- subset(sentiment_sentences_kohl1983,
                                                        Sentiment >= 0.25 | 
                                                        Sentiment <= -0.25  )
sentiment_sentences_kohl1991_polarisierend    <- subset(sentiment_sentences_kohl1991,
                                                        Sentiment >= 0.25 | 
                                                        Sentiment <= -0.25  )
```

``` r
ggplot(aes(x = Index, y= Sentiment), data=sentiment_sentences_schmidt1974_polarisierend) + 
      geom_point() + 
      ylim(c(-1,1)) + 
      labs(x = "Satz-Index", y = "Stimmung", 
           title = "Stimmung der Sätze im Verlauf der Rede - Schmidt1974") + 
      geom_smooth()
```

![](Sentimentanalyse_Schmidt1974+1980+Kohl1983+1991_files/figure-markdown_github/unnamed-chunk-16-1.png)

``` r
ggplot(aes(x = Index, y= Sentiment), data=sentiment_sentences_schmidt1980_polarisierend) + 
      geom_point() +
      ylim(c(-1,1)) + 
      labs(x = "Satz-Index", y = "Stimmung", 
           title = "Stimmung der Sätze im Verlauf der Rede - Schmidt1980") + 
      geom_smooth()
```

![](Sentimentanalyse_Schmidt1974+1980+Kohl1983+1991_files/figure-markdown_github/unnamed-chunk-16-2.png)

``` r
ggplot(aes(x = Index, y= Sentiment), data=sentiment_sentences_kohl1983_polarisierend) + 
      geom_point() +
      ylim(c(-1,1)) + 
      labs(x = "Satz-Index", y = "Stimmung", 
           title = "Stimmung der Sätze im Verlauf der Rede - Kohl1983") + 
      geom_smooth()
```

![](Sentimentanalyse_Schmidt1974+1980+Kohl1983+1991_files/figure-markdown_github/unnamed-chunk-16-3.png)

``` r
ggplot(aes(x = Index, y= Sentiment), data=sentiment_sentences_kohl1991_polarisierend) + 
      geom_point() + 
      ylim(c(-1,1)) + 
      labs(x = "Satz-Index", y = "Stimmung",
           title = "Stimmung der Sätze im Verlauf der Rede - Kohl1991") + 
      geom_smooth()
```

![](Sentimentanalyse_Schmidt1974+1980+Kohl1983+1991_files/figure-markdown_github/unnamed-chunk-16-4.png)

### 2.4. Kombination von Sentiment und Konkordanz

Konkordanz-Analyse der zwanzig vorherigen und zwanzig nachfolgenden Worte ausgewählter Schlüsselbegriffe

``` r
#Berechnung der Konkordanz. Nutzung von valuetype = regex, 
#um auch ähnliche Worte und Schreibweisen aus dem gleichen Politikfeld zu finden
konkordanz_data_sowjet <- kwic(mycorpus, "sowjet",
                               window = 20,valuetype = "regex") 
konkordanz_data_amerika <- kwic(mycorpus, "amerika",
                                window = 20,valuetype = "regex")
konkordanz_data_europa <- kwic(mycorpus, "europa",
                               window = 20,valuetype = "regex") 
konkordanz_data_wirtschaft <- kwic(mycorpus, "wirtschaft",
                                   window = 20,valuetype = "regex")
konkordanz_data_zukunft <- kwic(mycorpus, "zukunft",
                                window = 20,valuetype = "regex")
konkordanz_data_außen <- kwic(mycorpus, "außen",
                              window = 20,valuetype = "regex")
konkordanz_data_unternehmen <- kwic(mycorpus, "unternehmen",
                                    window = 20,valuetype = "regex")
konkordanz_data_ausländer <- kwic(mycorpus, "ausländer",
                                  window = 20,valuetype = "regex")
konkordanz_data_sicherheit <- kwic(mycorpus, "sicherheit",
                                   window = 20,valuetype = "regex")
```

Erstellen einer Funktion zur Berechnung der durchschnittlichen Stimmungswerte für jeden einzelnen Kontext des angegebenen Schlüsselbegriffs(x)

``` r
konkordanz_sentiment_mean <- function(x){
  #Umwandeln der in die Funktion eingegebenen Konkordanzdaten in einen Dataframe
  konkordanz_data <- as.data.frame(x)
  #Hinzufügen einer Sentiment Variable
  konkordanz_data$Sentiment <- NA
  #Umwandeln der Sentiment Variable in eine numerische
  konkordanz_data$Sentiment <- as.numeric(konkordanz_data$Sentiment)
  #For loop für alle Zeilen in den Konkordanzdaten
  for (i in seq_along(1:length(row.names(konkordanz_data)))){
    #Für jede betrachtete Zeile wird eine leere sentiment variable erstellt
    sentiment <- vector()
    #Für die betrachtete zeile werden die vorherigen und die nachfolgenden
    #Konkordanzwörter zu einem einzelnen Textausdruck zusammengefügt
    #For loop geht jedes Wort in diesem Text durch
    for (word in unlist(strsplit(paste(konkordanz_data$contextPre[i],
                                       konkordanz_data$contextPost[i],sep= " ")," "))){
      #Kommt das betrachtete Wort im Sentiment-Wörterbuch vor,
      #wird die zugehörige sentiment-wertung abgespeichert
      if ( word %in% sentiment_list$Wort){
        sentiment <- append(sentiment,
                            sentiment_list$Sentiment[sentiment_list$Wort == word] )
      }
    }
    sentiment_result <- sum(sentiment)
    #An Stelle der betrachteten Zeile i wird das sentiment resultat
    #zum zuvor erstellten dataframe hinzugefügt
    konkordanz_data$Sentiment[i]  <- sentiment_result
  }
  #Hinzufügen von den zusätzlichen Variablen Bundeskanzler und Jahr zu dem dataframe
  konkordanz_data$Bundeskanzler[konkordanz_data$docname == "text1" |
                                  konkordanz_data$docname == "text2"] <- "Schmidt"
  konkordanz_data$Bundeskanzler[konkordanz_data$docname == "text3" | 
                                  konkordanz_data$docname == "text4"] <- "Kohl"
  konkordanz_data$Bundeskanzler <- as.factor(konkordanz_data$Bundeskanzler)
  konkordanz_data$Jahr[konkordanz_data$docname == "text1"] <- "1974"
  konkordanz_data$Jahr[konkordanz_data$docname == "text2"] <- "1980"
  konkordanz_data$Jahr[konkordanz_data$docname == "text3"] <- "1983"
  konkordanz_data$Jahr[konkordanz_data$docname == "text4"] <- "1991"
  konkordanz_data$Jahr <- as.factor(konkordanz_data$Jahr)
  #Mittels plyr library wird der Durchschnitt der einzelnen Sentiment-Wertungen
  #für die Kategorie Bundeskanzler nach Jahr berechnet 
  sum_konkordanz_data <- ddply(konkordanz_data,c("Bundeskanzler","Jahr"),
                               summarise, mean = mean(Sentiment) )
  return(sum_konkordanz_data)
}
```

Anwenden der erstellten Funktion auf die konkordanz-Daten der Schlüsselbegriffe

``` r
sowjet_sentiment      <- konkordanz_sentiment_mean(konkordanz_data_sowjet)
europa_sentiment      <- konkordanz_sentiment_mean(konkordanz_data_europa)
wirtschaft_sentiment  <- konkordanz_sentiment_mean(konkordanz_data_wirtschaft)
zukunft_sentiment     <- konkordanz_sentiment_mean(konkordanz_data_zukunft)
außen_sentiment       <- konkordanz_sentiment_mean(konkordanz_data_außen)
unternehmen_sentiment <- konkordanz_sentiment_mean(konkordanz_data_unternehmen)
ausländer_sentiment   <- konkordanz_sentiment_mean(konkordanz_data_ausländer)
sicherheit_sentiment  <- konkordanz_sentiment_mean(konkordanz_data_sicherheit)
```

Erstellen der Balkendiagramme zum Vergleich über die Jahre und zwischen den Bundeskanzlern

``` r
sowjet <- ggplot(sowjet_sentiment, aes(x = Jahr, y = mean, fill = Bundeskanzler)) + 
                geom_bar(stat="identity",position=position_dodge())+
                scale_fill_manual(values = c("Schmidt" = "#F44336", "Kohl" = "grey23")) +
                labs(x = "Jahr", y = "Durch. Stimmung",
                     title = "Durchschnittliche Stimmung des Wortkontextes - Sowjet  ")

europa <- ggplot(europa_sentiment, aes(x = Jahr, y = mean, fill = Bundeskanzler)) + 
                geom_bar(stat="identity",position=position_dodge())+
                scale_fill_manual(values = c("Schmidt" = "#F44336", "Kohl" = "grey23")) + 
                labs(x = "Jahr", y = "Durch. Stimmung", 
                     title = "Europa")+
                ylim(c(-0.1,0.25))

wirt <-   ggplot(wirtschaft_sentiment, aes(x = Jahr, y = mean, fill = Bundeskanzler)) +
                geom_bar(stat="identity",position=position_dodge())+
                scale_fill_manual(values = c("Schmidt" = "#F44336", "Kohl" = "grey23")) + 
                labs(x = "Jahr", y = "Durch. Stimmung", 
                     title = "Wirtschaft")+
                ylim(c(-0.1,0.25))

außen <-  ggplot(außen_sentiment, aes(x = Jahr, y = mean, fill = Bundeskanzler)) +  
                geom_bar(stat="identity",position=position_dodge())+
                scale_fill_manual(values = c("Schmidt" = "#F44336", "Kohl" = "grey23")) + 
                labs(x = "Jahr", y = "Durch. Stimmung", 
                     title = "Außen")+
                ylim(c(-0.1,0.25))

sicher <- ggplot(sicherheit_sentiment, aes(x = Jahr, y = mean, fill = Bundeskanzler)) +  
                geom_bar(stat="identity",position=position_dodge())+
                scale_fill_manual(values = c("Schmidt" = "#F44336", "Kohl" = "grey23")) + 
                labs(x = "Jahr", y = "Durch. Stimmung", 
                     title = "Sicherheit")+
                ylim(c(-0.1,0.25))

unter <-  ggplot(unternehmen_sentiment, aes(x = Jahr, y = mean, fill = Bundeskanzler)) +
                geom_bar(stat="identity",position=position_dodge())+
                scale_fill_manual(values = c("Schmidt" = "#F44336", "Kohl" = "grey23")) + 
                labs(x = "Jahr", y = "Durch. Stimmung",
                     title = "Unternehmen")+
                ylim(c(-0.2,0.15))

ausl <-   ggplot(ausländer_sentiment, aes(x = Jahr, y = mean, fill = Bundeskanzler)) +
                geom_bar(stat="identity",position=position_dodge())+
                scale_fill_manual(values = c("Schmidt" = "#F44336", "Kohl" = "grey23")) + 
                labs(x = "Jahr", y = "Durch. Stimmung", 
                     title = "Ausländer")+
                ylim(c(-0.2,0.15))
```

Anzeigen der erstellten Grafiken

``` r
sowjet
```

![](Sentimentanalyse_Schmidt1974+1980+Kohl1983+1991_files/figure-markdown_github/unnamed-chunk-21-1.png)

``` r
grid.arrange(europa, wirt, außen,sicher, ncol=2, nrow =2, top=textGrob("Ausgewählte Politikfelder",
                                                                       gp=gpar(fontsize=15,font=1)))
```

![](Sentimentanalyse_Schmidt1974+1980+Kohl1983+1991_files/figure-markdown_github/unnamed-chunk-21-2.png)

``` r
grid.arrange(unter, ausl, ncol=2, nrow =1,top=textGrob("Parteipolitische Streitthemen",
                                                       gp=gpar(fontsize=15,font=1)))
```

![](Sentimentanalyse_Schmidt1974+1980+Kohl1983+1991_files/figure-markdown_github/unnamed-chunk-21-3.png)
