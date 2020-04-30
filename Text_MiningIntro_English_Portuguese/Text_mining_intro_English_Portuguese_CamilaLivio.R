#### Text Mining -- the basics, by Camila Lívio 
#### 04/29/2020


#### Performing text mining on languages other than English -- the case of Portuguese!

##Let's get started
#### I added an extra step here using the library(rtweet) to show how you could potentially gather a data set from Twitter
#### If you have never used the package, you can check out the tutorial here: https://rtweet.info
library(rtweet)

token <- create_token(
  app= "BP project", 
  consumer_key = "******", ####You will have to substitute the "***" with your own information
  consumer_secret = "*****************", ####You can also play with my data set that lives in the R-Ladies Athens Repository
  access_token = "***************************", 
  access_secret = "*********************************")
token

dm <- search_tweets(
  "mulheres", n = 1000,lang='pt', include_rts = FALSE
) #### creating an object "dm" to store the data 

str(dm) #### have a look at the data and variables

#### A few text mining techniques to have a look at the data
#Remeber to load the packages first!!!!
#library(udpipe) is the package that allows you to perform text mining in *many* non-English languages 
library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
library(lattice)
library(udpipe)

### having a look only at the "text" variable
dm %>% group_by(text) %>% count() 

### since my data set is in Portuguese, I am going to install this language features
### read the details of the package and the supported languages here: https://cran.r-project.org/web/packages/udpipe/vignettes/udpipe-annotation.html
model <- udpipe_download_model(language = "portuguese") ### dowloading the language features
udmodel_port <- udpipe_load_model(file = 'portuguese-bosque-ud-2.4-190531.udpipe') ### loading the language package

s <- udpipe_annotate(udmodel_port, dm$text) #### tag the text column of my dm data set
x <- data.frame(s) ### then create an object "x" and transform it into a tagged data frame
str(x)

### counting parts of speech tags
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "lightpink", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")

#### it looks good, but try it using ggplot2:
example <- ggplot(stats, aes(x = key, y = freq, fill=freq)) +
  geom_bar(stat="identity") +
  ggtitle("UPOS (Universal Parts of Speech)\n frequency of occurrence")+
  theme_minimal()
example + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#### another way to visualize the parts of the speech -- lots of nouns!
table(x$upos)

### what are the most occurring nouns? "Mulheres" is the most frequent one -- very much expected! But check out the 20 most occurring adjectives as well:
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "pink", 
         main = "Most occurring nouns", xlab = "Freq")


### I still prefer ggplot2 for data viz. ## using ggplot now to plot the top 20 most frequent: 
top20 <- head(stats, n=20)
top20 ## clean, straightforward way to have a look at the data

example2 <- ggplot(top20, aes(x = key, y = freq, fill=key)) +
  geom_bar(stat="identity") +
  ggtitle("20 most frequent adjectives in the *mulheres* data set")+
  theme_gray()

example2 + theme(axis.text.x = element_text(angle = 70, hjust = 1))

### (EXTRA FEATURES BELOW)
####RAKE (Rapid Automatic Keyword Extraction) ### applying Jan Wijffels' code to the data set "dm"
#### I really like this plot because it allows you to check out the association between words
stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "cadetblue", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake")

#### Checking the co-occurrences now -- it allows us to see how words are used either in the same sentence or next to each other
cooc <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
                     term = "lemma", 
                     group = c("doc_id", "paragraph_id", "sentence_id"))
head(cooc)

### cool! Let's plot it!
library(igraph)
library(ggraph)
wordnetwork <- head(cooc, 30)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkblue", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "Cooccurrences within sentence", subtitle = "Nouns & Adjective")

### calculating the co-occurrence
wordnetwork <- head(cooc, 15)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc)) +
  geom_node_text(aes(label = name), col = "darkblue", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  labs(title = "Words following one another", subtitle = "Nouns & Adjective")

######Keyword correlations indicate how terms are just together in a same document/sentence. While co-occurrences focus on frequency, correlation measures between 2 terms can also be high even if 2 terms occur only a small number of times but always appear together.
######In the below example, we show how nouns and adjectives are correlated within each sentence of a document
x$id <- unique_identifier(x, fields = c("sentence_id", "doc_id"))
dtm <- subset(x, upos %in% c("NOUN", "ADJ"))
dtm <- document_term_frequencies(dtm, document = "id", term = "lemma")
dtm <- document_term_matrix(dtm)
dtm <- dtm_remove_lowfreq(dtm, minfreq = 5)
termcorrelations <- dtm_cor(dtm)
y <- as_cooccurrence(termcorrelations)
y <- subset(y, term1 < term2 & abs(cooc) > 0.2)
y <- y[order(abs(y$cooc), decreasing = TRUE), ]
head(y)

### Udpipe really makes your life easier when performing text mining on languages other than English! 






