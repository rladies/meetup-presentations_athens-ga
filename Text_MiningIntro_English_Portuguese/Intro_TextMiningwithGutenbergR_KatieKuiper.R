#intro to gutenbergr package and text mining by Katie Kuiper
#### 04/29/2020 testing this with some cool texts and authors of interest

#don't forget to install these packages first if they aren't already installed!

#load packages
library(ggplot2)
library(dplyr)
library(stringr)
library(gutenbergr)
library(tidyr)
library(tidytext)

##get info about gutenbergr
??gutenbergr

#get info about gutenberg metadata
gutenberg_metadata

#use this to filter by title of a work, for example The Merchant of Venice
#by William Shakespeare
MerchofVenice <- gutenberg_metadata %>% 
  filter(title== "Merchant of Venice")

#view it
View(MerchofVenice)


#check out the works of one author (a personal fave, William Faulkner) and filter it this way
Faulkner <- gutenberg_works(author == "Faulkner, William")
View(Faulkner)

#filtering with regular expression
Faulkner <- gutenberg_works(str_detect(author, "Faulkner"))
View(Shakespeare)


#you can also download books by their metadata id
merchant_of_venice <- gutenberg_download(12578)

#lets prep this data for a frequency list
cleaned_merchant <- merchant_of_venice %>%
  unnest_tokens(word, text)

#check it out
View(cleaned_merchant)

#lets get the top words in Merchant of Venice
top_words <- cleaned_merchant %>% count(word)

#prints out results in this data object, top words
View(top_words)

#you might have noticed this did not print out the words in order of frequency
#so lets check them out in order of most to least frequent
top_words <- cleaned_merchant %>% count(word, sort = TRUE)

#the top word is unsuprisingly "the" and other 
#function words that we might want to filter out
#we can do this using stop words list
#load stop_words
data(stop_words)

#new counts without function words
word_counts_content <- top_words %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)
#quick check of the results
word_counts_content


#plot it with all the words, (bc as a linguist I like to see the function words, too)
cleaned_merchant %>%
  count(word, sort = TRUE) %>% # frequency list
  filter(n >= 100) %>% # keep only words with greater than 100 frequency
  mutate(word = reorder(word, n)) %>% # order words by frequency
  ggplot(aes(word, n)) + # ggplot
  geom_col(aes(fill = word), color = "black", size = .1, show.legend = FALSE) + # bars
  scale_y_continuous(breaks = seq(0, 450, 50)) +
  coord_flip() + #use this or not to move flip coordinates
  ggtitle("Most Frequent Words in Merchant of Venice by W. Shakespeare") #title

