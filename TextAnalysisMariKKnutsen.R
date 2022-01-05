# Text mining and classification --------------------------------
## ECON4170
## 19/11/2021
## Mari Kristine Knutsen - candidate 130
-----------------------------------------------------------------
## Packages --------------------------------
# must-have
library(tidyverse) 
#text mining
library(tidytext)
# visualization
library(wordcloud)
library(RColorBrewer)
library(reshape2)
## for model-part with text data
library(tidymodels)
library(themis) 
library(textrecipes)
library(hardhat)

## function
"%notin%" <- function(x,y) !(x %in% y) 

## Import the data --------------------------------
setwd("~/R/ECON4170 Data Science for Economists /Data science project")

books <- read_delim("booksummaries.txt", 
                    delim = "\t", #tab seperated, it says so in the README 
                    escape_double = FALSE, 
                    col_names = FALSE,  
                    trim_ws = TRUE, col_select = c("X6","X7")) # only keeping genre and plot 

books <- books %>% 
  rename("genre"="X6","plot"="X7")  # new colnames

books %>% slice(.,1:5) # the first observations in the data frame

## Cleaning genre --------------------------------
books <- drop_na(books,genre) #removing the NA 
books <- books %>% 
  separate_rows(.,"genre",sep = ",", convert = FALSE) # Separate rows

#Removing "Freebase ID" from genre genre: 
books$genre <- books$genre %>%  
  str_replace_all("((\\S+)(?=:))","") 
# "\\s" any whitespace "\\S" non-whitespace
# "?=" followed by 
# ":" what separated the Freebase ID from genre. 

books$genre <- books$genre %>%   
  str_replace_all("([:punct:])|([:symbol:])","") # removing all punctuation and symbols 
books$genre <- books$genre %>%  
  str_replace_all("(^(\\s+))|((\\s+)$)","") # removing whitespace in the start or end 
books$genre <- books$genre %>%  
  str_to_lower(.,locale = "en") # string to title

books %>% slice(.,10:15)

## Finding and keeping the most frequent genres
wordcount <- books %>%  
  count(genre,sort = TRUE) #counting all the genres 

wordcount <- books %>%  
  count(genre,sort = TRUE) %>% 
  filter(,n>=400)               # filtering out the ones that labels 400 or more plots 

books <- books %>%  
  filter(books$genre%in%wordcount$genre) # changing the data frame

books %>% slice(.,1:5)

## Changing long genre names
books$genre <- books$genre %>%  
  str_replace_all("crime fiction","crime") %>% 
  str_replace_all("speculative fiction","speculativefic") %>%
  str_replace_all("science fiction","sciencefic") %>%
  str_replace_all("childrens literature","childrens") %>%
  str_replace_all("young adult literature","youngadult")%>%
  str_replace_all("historical novel","historicalfic")%>%
  str_replace_all("romance novel","romance")

## Multi-label data frame --------------------------------
books <- books %>%
  mutate(n=1) %>%   # just to make the spread easier
  distinct()           

books <- books %>% 
  group_by(genre) %>%
  spread(key=genre,value=n,fill=0) # spread to get the genres as colums with 1 and 0

books %>% select(-plot) %>% slice(.,15:20)

#### Plot 1: Number of genres per book --------------------------------
# count of number of genres per book

wordcount <- books %>%
  select(-plot) 

wordcount <- wordcount %>% 
  mutate(wordcount=rowSums(. == 1))        # taking the sum across the rows

wordcount <- wordcount%>%  
  count(wordcount,sort = TRUE) 

# plot of number of genres per book
ggplot(wordcount,aes(x=reorder(wordcount,-n),y=n))+
  geom_bar(stat="identity")+
  theme_classic()+
  theme(axis.text.y.left = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.text.x= element_text(size = 15),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        plot.title = element_text(hjust = 0.5,size=15),
        legend.position = "none")+
  geom_text(aes(label = n), vjust = -0.5,size=5)+
  labs(x="Number of genres",y="Number of books",title = "Plot 1: Number of genres per book")

#### Plot 2: Books per element_text()--------------------------

#count
wordcount <- books %>% 
  select(-plot)
wordcount <- wordcount %>% 
  summarise(.,across(everything(),sum)) %>% 
  pivot_longer(,cols=everything(),names_to = "genre",)

#more colors
mycolors_named <- setNames(object =scales::hue_pal()(14), nm = wordcount$genre)

#plot
ggplot(wordcount,aes(x=reorder(genre,value),y=value,fill=genre)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values = mycolors_named) +
  theme_classic() +
  coord_flip()+
  geom_text(aes(label = value), hjust = 1.2,size=4) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        axis.text.y = element_text(hjust= 1.0,size = 13),
        plot.title = element_text(hjust = 0.5,size=15),
        legend.position = "none")+
  labs(x="",y="",title = "Plot 2: Number of books per genre")

## Multi-class --------------------------------
## changing fiction 
books2 <- books %>% 
  filter(books$"fiction"==1 &
           (books$"childrens"==1|books$"crime"==1|
              books$"fantasy"==1| books$"historicalfic"==1|
              books$"horror"==1|books$"mystery"==1|books$"novel"==1|
              books$"romance"==1|books$"sciencefic"==1|
              books$"speculativefic"==1|books$"suspense"==1|
              books$"thriller"==1|books$"youngadult"==1))

books2$fiction <- books2$fiction%>%  
  str_replace_all("1","0") 

books1 <- books2 %>%  
  select(-plot) %>% 
  mutate_if(is.character,as.numeric )

books2 <- books2 %>% 
  select(plot)

books1 <- bind_cols(books2,books1)

books2 <- books %>% 
  filter(books$"fiction"==1 & books$"childrens"==0 & books$"crime"==0 &
           books$"fantasy"==0 & books$"historicalfic"==0 &
           books$"horror"==0 &books$"mystery"==0 &books$"novel"==0 &
           books$"romance"==0 & books$"sciencefic"==0 &
           books$"speculativefic"==0 & books$"suspense"==0 &
           books$"thriller"==0 & books$"youngadult"==0)

books3 <- books %>% 
  filter(books$"fiction"==0)

books4 <- bind_rows(books1,books2)
books <- bind_rows(books3,books4)

rm(books1,books2,books3,books4)

#### Plot 3: Number of genres per book with changed fiction  ---------------
# count of number of genres per book

wordcount <- books %>%
  select(-plot) 
wordcount <- wordcount %>% 
  mutate(wordcount=rowSums(. == 1))
wordcount <- wordcount%>%  
  count(wordcount,sort = TRUE) 

# plot of number of genres per book
ggplot(wordcount,aes(x=reorder(wordcount,-n),y=n))+
  geom_bar(stat="identity")+
  theme_classic()+
  theme(axis.text.y.left = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.text.x= element_text(size = 15),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        plot.title = element_text(hjust = 0.5,size=15),
        plot.subtitle = element_text(hjust = 0.5,size=12),
        legend.position = "none")+
  geom_text(aes(label = n), vjust = -0.2,size=5)+
  labs(x="Number of genres",y="Number of books",title = "Plot 3: Number of genre per book", 
       subtitle = "With changed fiction")
  

#### Plot 4: Number of book per genre with changed fiction  ------------------
#counting
wordcount <- books %>% 
  select(-plot)
wordcount <- wordcount %>% 
  summarise(.,across(everything(),sum)) %>% 
  pivot_longer(,cols=everything(),names_to = "genre",)

#plot
ggplot(wordcount,aes(x=reorder(genre,value),y=value,fill=genre)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values = mycolors_named) +
  theme_classic() +
  coord_flip()+
  geom_text(aes(label = value), hjust = 1.2,size=4) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        axis.text.y = element_text(hjust=1,size = 13),
        plot.title = element_text(hjust = 0.5,size=15),
        plot.subtitle = element_text(hjust = 0.5,size=12),
        legend.position = "none")+
  labs(x="",y="",title = "Plot 4: Number of books per genre",
       subtitle = "With changed fiction")

## Changing more genres --------------------------------
# speculative fiction
books2 <- books %>% 
  filter(books$"speculativefic"==1 &
           (books$"childrens"==1|books$"crime"==1|
              books$"fantasy"==1| books$"historicalfic"==1|
              books$"horror"==1|books$"mystery"==1|books$"novel"==1|
              books$"romance"==1|books$"sciencefic"==1|books$"suspense"==1|
              books$"thriller"==1|books$"youngadult"==1))
books2$speculativefic <- books2$speculativefic%>%  
  str_replace_all("1","0") 
books1 <- books2 %>%  
  select(-plot) %>% 
  mutate_if(is.character,as.numeric )
books2 <- books2 %>% 
  select(plot)
books1 <- bind_cols(books2,books1)
books2 <- books %>% 
  filter(books$"speculativefic"==1 & books$"childrens"==0 & books$"crime"==0 &
           books$"fantasy"==0 & books$"historicalfic"==0 &
           books$"horror"==0 &books$"mystery"==0 &books$"novel"==0 &
           books$"romance"==0 & books$"sciencefic"==0 &
           books$"fiction"==0 & books$"suspense"==0 &
           books$"thriller"==0 & books$"youngadult"==0)
books3 <- books %>% 
  filter(books$"speculativefic"==0)
books4 <- bind_rows(books1,books2)
books <- bind_rows(books3,books4)
rm(books1,books2,books3,books4)

# science fiction 
books2 <- books %>% 
  filter(books$"sciencefic"==1 &
           (books$"childrens"==1|books$"crime"==1|
              books$"fantasy"==1| books$"historicalfic"==1|
              books$"horror"==1|books$"mystery"==1|books$"novel"==1|
              books$"romance"==1|books$"suspense"==1|
              books$"thriller"==1|books$"youngadult"==1))
books2$sciencefic <- books2$sciencefic%>%  
  str_replace_all("1","0") 
books1 <- books2 %>%  
  select(-plot) %>% 
  mutate_if(is.character,as.numeric )
books2 <- books2 %>% 
  select(plot)
books1 <- bind_cols(books2,books1)
books2 <- books %>% 
  filter(books$"sciencefic"==1 & books$"childrens"==0 & books$"crime"==0 &
           books$"fantasy"==0 & books$"historicalfic"==0 &
           books$"horror"==0 &books$"mystery"==0 &books$"novel"==0 &
           books$"romance"==0 & books$"speculativefic"==0 &
           books$"fiction"==0 & books$"suspense"==0 &
           books$"thriller"==0 & books$"youngadult"==0)
books3 <- books %>% 
  filter(books$"sciencefic"==0)
books4 <- bind_rows(books1,books2)
books <- bind_rows(books3,books4)
rm(books1,books2,books3,books4)

# novel
books2 <- books %>% 
  filter(books$"novel"==1 &
           (books$"childrens"==1|books$"crime"==1|
              books$"fantasy"==1| books$"historicalfic"==1|
              books$"horror"==1|books$"mystery"==1|
              books$"romance"==1|books$"suspense"==1|
              books$"thriller"==1|books$"youngadult"==1))
books2$novel <- books2$novel%>%  
  str_replace_all("1","0") 
books1 <- books2 %>%  
  select(-plot) %>% 
  mutate_if(is.character,as.numeric )
books2 <- books2 %>% 
  select(plot)
books1 <- bind_cols(books2,books1)
books2 <- books %>% 
  filter(books$"novel"==1 & books$"childrens"==0 & books$"crime"==0 &
           books$"fantasy"==0 & books$"historicalfic"==0 &
           books$"horror"==0 &books$"mystery"==0 & books$"sciencefic"==0 &
           books$"romance"==0 & books$"speculativefic"==0 &
           books$"fiction"==0 & books$"suspense"==0 &
           books$"thriller"==0 & books$"youngadult"==0)
books3 <- books %>% 
  filter(books$"novel"==0)
books4 <- bind_rows(books1,books2)
books <- bind_rows(books3,books4)
rm(books1,books2,books3,books4)

# fantasy
books2 <- books %>% 
  filter(books$"fantasy"==1 &
           (books$"childrens"==1|books$"crime"==1| books$"historicalfic"==1|
              books$"horror"==1|books$"mystery"==1|
              books$"romance"==1|books$"suspense"==1|
              books$"thriller"==1|books$"youngadult"==1))
books2$fantasy <- books2$fantasy%>%  
  str_replace_all("1","0") 
books1 <- books2 %>%  
  select(-plot) %>% 
  mutate_if(is.character,as.numeric )
books2 <- books2 %>% 
  select(plot)
books1 <- bind_cols(books2,books1)
books2 <- books %>% 
  filter(books$"fantasy"==1 & books$"childrens"==0 & books$"crime"==0 &
           books$"novel"==0 & books$"historicalfic"==0 &
           books$"horror"==0 &books$"mystery"==0 & books$"sciencefic"==0 &
           books$"romance"==0 & books$"speculativefic"==0 &
           books$"fiction"==0 & books$"suspense"==0 &
           books$"thriller"==0 & books$"youngadult"==0)
books3 <- books %>% 
  filter(books$"fantasy"==0)
books4 <- bind_rows(books1,books2)
books <- bind_rows(books3,books4)
rm(books1,books2,books3,books4)

# childrens
books2 <- books %>% 
  filter(books$"childrens"==1 &
           (books$"crime"==1| books$"historicalfic"==1|
              books$"horror"==1|books$"mystery"==1|
              books$"romance"==1|books$"suspense"==1|
              books$"thriller"==1|books$"youngadult"==1))
books2$childrens <- books2$childrens%>%  
  str_replace_all("1","0") 
books1 <- books2 %>%  
  select(-plot) %>% 
  mutate_if(is.character,as.numeric )
books2 <- books2 %>% 
  select(plot)
books1 <- bind_cols(books2,books1)
books2 <- books %>% 
  filter(books$"childrens"==1 & books$"fantasy"==0 & books$"crime"==0 &
           books$"novel"==0 & books$"historicalfic"==0 &
           books$"horror"==0 &books$"mystery"==0 & books$"sciencefic"==0 &
           books$"romance"==0 & books$"speculativefic"==0 &
           books$"fiction"==0 & books$"suspense"==0 &
           books$"thriller"==0 & books$"youngadult"==0)
books3 <- books %>% 
  filter(books$"childrens"==0)
books4 <- bind_rows(books1,books2)
books <- bind_rows(books3,books4)
rm(books1,books2,books3,books4)

#### Plot 5: Number of genres per book with changed genres -------------------
# count of number of genres per book
wordcount <- books %>%
  select(-plot) 
wordcount <- wordcount %>% 
  mutate(wordcount=rowSums(. == 1))
wordcount <- wordcount%>%  
  count(wordcount,sort = TRUE) 

# plot of number of genres per book
ggplot(wordcount,aes(x=reorder(wordcount,-n),y=n))+
  geom_bar(stat="identity")+
  theme_classic()+
  theme(axis.text.y.left = element_blank(),
              axis.ticks.y.left = element_blank(),
              axis.text.x= element_text(size = 15),
              axis.title.x = element_text(size=12),
              axis.title.y = element_text(size=12),
              plot.title = element_text(hjust = 0.5,size=15),
              plot.subtitle = element_text(hjust = 0.5,size=12),
              legend.position = "none")+
  geom_text(aes(label = n), vjust = -0.2,size=5)+
  labs(x="Number of genres",y="Number of books",title = "Plot 5: Number of genre per book",
       subtitle = "After changing Speculativefic, sciencefic, novel, fantasy & childrens")
#### Plot 6: Number of books per genre after isolating more genres ------------
#counting
wordcount <- books %>% 
  select(-plot)
wordcount <- wordcount %>% 
  summarise(.,across(everything(),sum)) %>% 
  pivot_longer(,cols=everything(),names_to = "genre",)

#plot
ggplot(wordcount,aes(x=reorder(genre,value),y=value,fill=genre)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values = mycolors_named) +
  theme_classic() +
  coord_flip()+
  geom_text(aes(label = value), hjust = 1.2,size=4) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        axis.text.y = element_text(hjust=1,size = 13),
        plot.title = element_text(hjust = 0.5,size=15),
        plot.subtitle = element_text(hjust = 0.5,size=12),
        legend.position = "none")+
  labs(x="",y="",title = "Plot 6: Number of books per genre",
       subtitle = "After changing Speculativefic, sciencefic, novel, fantasy & childrens")

## Removing the rest of multilabels --------------------------------
genre <- books %>% 
  select(-plot)
genre <- genre %>% 
  mutate(wordcount=rowSums(. == 1))
books <- books %>% 
  select(plot)
books <- bind_cols(books, genre)
books <- books %>% 
  filter (wordcount==1) %>% 
  select(-wordcount)

#### Plot 7: Multi-class, only one genre per book --------------------
# count of number of genres per book
wordcount <- books %>%
  select(-plot) 
wordcount <- wordcount %>% 
  mutate(wordcount=rowSums(. == 1))
wordcount <- wordcount%>%  
  count(wordcount,sort = TRUE) 

wordcount

# plot of number of genres per book
ggplot(wordcount,aes(x=reorder(wordcount,-n),y=n))+
  geom_bar(stat="identity")+
  theme_classic()+
  theme(axis.text.y.left = element_blank(),
              axis.ticks.y.left = element_blank(),
              axis.text.x= element_text(size = 15),
              axis.title.x = element_text(size=12),
              axis.title.y = element_text(size=12),
              plot.title = element_text(hjust = 0.5,size=15),
              legend.position = "none")+
  geom_text(aes(label = n), vjust = -0.2)+
  labs(x="Genres",y="Books",title = "Plot 7: Only one genre per book")
  
#### Plot 8: Multi-class Number of books per genre  ------------
#counting
wordcount <- books %>% 
  select(-plot)
wordcount <- wordcount %>% 
  summarise(.,across(everything(),sum)) %>% 
  pivot_longer(,cols=everything(),names_to = "genre")

#plot
ggplot(wordcount,aes(x=reorder(genre,value),y=value,fill=genre)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values = mycolors_named) +
  theme_classic() +
  coord_flip()+
  geom_text(aes(label = value), hjust = 1,size=4) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        axis.text.y = element_text(hjust=1,size = 13),
        plot.title = element_text(hjust = 0.5,size=15),
        plot.subtitle = element_text(hjust = 0.5,size=12),
        legend.position = "none")+
  labs(x="",y="",title = "Plot 8: Number of books per genre",
       subtitle = "Multi-class")

## Gathering the data --------------------------------

df <- books %>% 
  gather(.,"genre","num",2:15) %>% 
  filter(num==1) %>% 
  select(-num)
df %>% slice(.,10:12)

## Sentiments --------------------------------
get_sentiments("nrc")

tidy_df <- df %>% # df with all 14 genre 
  group_by(genre) %>% 
  ungroup() %>%
  unnest_tokens(word, plot)

df_sentiment <- tidy_df %>%
  inner_join(get_sentiments("nrc")) %>%
  count(genre, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) 

df_sentiment %>% slice(.,1:5)

### Plot 9: Sentiments in this data frame -------------------------------- 
s_plot <- df_sentiment %>% 
  gather(., key = "sentiment", value = "value",-c(genre,negative,positive)) %>% 
  ggplot(., aes(x=genre,y=value, fill = sentiment)) +
  geom_bar(stat="identity",position = "fill")+
  theme_classic()+
  coord_flip()+
  theme(legend.position ="bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        plot.title = element_text(hjust=0.1,size=15))+
  labs(y = "",
       x = "",
       title = "Plot 9: Share of different sentiments in the genres")

s_plot

## Wordcloud --------------
set.seed(12)
tidy_df %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100,random.order=FALSE))

#with sentiments 
tidy_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("darkGreen", "lightBlue"),
                   max.words = 100,random.order = FALSE)

## Comparing genres ---------------------------

# mystery, crime, speculativefic, horror, thriller

# cleaning out symbols, filtering out stop words and names from plot.
df$plot <- df$plot %>%  
  str_replace_all("([:punct:])|([:symbol:])|([:digit:])","")

names <- c("david", "john", "peter","alice","poirot", "alex","harry","jack" ,"falco","jesse","michael","bosch", "richard","charles","lee","de","stephen","elizabeth","henry","arthur","aubrey","theseus","flavia", "anita","roland","jean","darren","buffy","louis","marius","zoey","archie","philip","jane","sookie", "nora","rachel","jamie","paul","edward","erik","claire","darcy","eric","silvia","james","max","york","sam","thomas","ben","jacky","emma", "julia","tom","olaf","natalie","garret","billy","abby")
genres <- c("mystery","crime","speculativefic","horror","thriller")


tidy_df <- df %>% # df with all 14 genre 
  group_by(genre) %>% 
  ungroup() %>%
  unnest_tokens(word, plot)

tidy_df <- tidy_df %>% 
  filter(tidy_df$word%notin%names)

#taking a subset to study 
df_sub <- tidy_df %>%
  group_by(genre) %>%
  anti_join(stop_words) %>% 
  count(genre,word, sort = TRUE) %>%
  top_n(100) %>%
  ungroup %>%
  filter(genre%in%genres) %>% 
  mutate(n=word)

df_sub <- df_sub %>% 
  group_by(genre) %>%
  spread(genre,n,fill=0) %>% 
  select(-word)

# taking the intercect between the genres, keeping in mind it is one less
# as one of the intercects are 0 in my table 
intersect(df_sub$crime,df_sub$horror)

# intersection between most frequent words in crime plots and the other genres  
length(intersect(df_sub$crime,df_sub$horror))-1 
length(intersect(df_sub$crime,df_sub$mystery))-1 
length(intersect(df_sub$crime,df_sub$speculativefic))-1 
length(intersect(df_sub$crime,df_sub$thriller))-1 
# highest similarity between crime and mystery

# intersection between most frequent words in horror plots and the other genres
length(intersect(df_sub$horror,df_sub$mystery))-1 
length(intersect(df_sub$horror,df_sub$thriller))-1 
length(intersect(df_sub$horror,df_sub$speculativefic))-1 
# highest similarity between horror and speculativefic

# intersection between most frequent words in mystery plots and the other genres
length(intersect(df_sub$mystery,df_sub$thriller))-1 
length(intersect(df_sub$mystery,df_sub$speculativefic))-1 
# highest similarity between crime and mystery

# intersection between most frequent words in thriller plots and the other genres
length(intersect(df_sub$thriller,df_sub$speculativefic))-1 
# highest similarity between thriller and speculativefic

#changing horror and thriller to speculative
#changing crime to mystery

#### Plot 10: Merging similar genres -----------------------------------
df$genre <- df$genre %>%  
  str_replace_all("horror","speculativefic") %>% 
  str_replace_all("thriller","speculativefic") %>% 
  str_replace_all("crime","mystery")

#removing suspense as it only label to 41 books
df <- df %>% 
  filter(genre!="suspense") 

#plot
df %>% count(genre) %>% 
ggplot(.,aes(x=reorder(genre,n),y=n,fill=genre)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values = mycolors_named) +
  theme_classic() +
  coord_flip()+
  geom_text(aes(label = n), hjust = 1,size=4) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        axis.text.y = element_text(hjust=1,size = 13),
        plot.title = element_text(hjust = 0.5,size=15),
        plot.subtitle = element_text(hjust = 0.5,size=12),
        legend.position = "none")+
  labs(x="",y="",title = "Plot 10: Number of books per genre",
       subtitle = "With new merged genres")

## TF-IDF ----------------------
tidy_df <- df %>%
  unnest_tokens(word, plot) %>%
  anti_join(stop_words) 

tidy_df <- tidy_df %>%
  count(genre, word, sort = TRUE)

book_tf_idf <- tidy_df %>%
  bind_tf_idf(word, genre, n) %>%
  arrange(desc(tf_idf))
book_tf_idf

#### Plot 11: Most frequent words --------------------------------

book_tf_idf %>%
ggplot() +
  geom_histogram(aes(y=tf, fill = genre),show.legend = FALSE) +
  coord_flip()+
  ylim(NA,0.001)+
  theme_light()+
  theme(strip.text = element_text(size=10))+
  facet_wrap(~genre, ncol = 2, scales = "free_y")+
  labs(y = "",x = "",   title = "Plot 11: Term frequency")

#### Plot 12: TF-IDF ---------------------

book_tf_idf %>%
  group_by(genre) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = genre)) +
  geom_col(show.legend = FALSE) +
  theme_classic()+
  theme(axis.text.y.left = element_text(size=12),
        axis.text.x.bottom = element_text(size=5),
        strip.text = element_text(size=10),
        plot.title = element_text(size=14))+
  facet_wrap(~genre, ncol = 5, scales = "free_y") +
  labs(y = "",
       x = "",
       title = "Plot 12: TF-IDF in each genre")

# Clasification part --------------------------------
#cleaning a bit in enviorment:
rm(wordcount,df_sentiment,df_sub,s_plot,tidy_df,genres,book_tf_idf,genre)
## Removing genres ---------------------------------------------
# Removing the least frequent genres: romance and historical fiction,
# under 600 plots (have already removed suspense earlier)
df1 <- df %>% 
  filter(genre!="romance") %>% 
  filter(genre!="historicalfic")

df1 %>% count(genre) %>% arrange(-n)

## First try: Classification ------------------------------
# https://github.com/andrew-couch/Tidy-Tuesday/blob/master/Season%201/Scripts/TidyTuesdayMulticlassClassification.Rmd
# https://smltar.com/mlclassification.html#

set.seed(1234)
split_df <- initial_split(df1, strata = genre) 
# stratify after genre to ensure each genre have the same proportion in test and train

train_df1 <- training(split_df)
test_df1<- testing(split_df)

train_df1 <- recipe(genre~., data = train_df1) %>% #making the genres the same size
  step_downsample(genre) %>% 
  prep() %>% #calculate returns recipe
  juice() #apply - returns data frame 

train_df1 %>% count(genre)

set.seed(1234)
k_folds1 <- vfold_cv(train_df1,strata=genre)  

# preprocess text

df1_rec <-
  recipe(genre ~ plot, data = train_df1) %>%
  step_tokenize(plot) %>%
  step_stopwords(plot) %>% 
  step_tokenfilter(plot, max_tokens = 1000) %>%
  step_tfidf(plot)

# the model
lasso_model <- multinom_reg(penalty = tune(), mixture = 1) %>% #mixture=1 is pure lasso
  set_mode("classification") %>%
  set_engine("glmnet")

# grid 

logistic_grid <- grid_regular(parameters(lasso_model), levels = 20)

sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")

#workflow:

lasso_wf1 <- workflow() %>%
  add_recipe(df1_rec, blueprint = sparse_bp) %>%
  add_model(lasso_model)

# Define tuning process - takes a few minutes to run

lasso_res1 <- tune_grid(
  lasso_wf1,
  k_folds1,
  grid = logistic_grid,
  control = control_resamples(save_pred = TRUE))

# test accuracy

lasso_best_acc1 <- lasso_res1[] %>%
  show_best("accuracy")

lasso_best_acc1


########## confusion matrix
lasso_res1 %>%
  collect_predictions() %>%
  filter(penalty == lasso_best_acc1$penalty) %>%
  filter(id == "Fold03") %>%
  conf_mat(genre, .pred_class) %>%
  autoplot(type = "heatmap") +
  scale_y_discrete(labels = function(x) str_wrap(x, 20)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 20))+
  scale_fill_gradient2(high="darkgreen", guide="colorbar")

## Second try - Going back to the data --------------------

# new df
df2 <- df1 %>% 
  filter(genre!="speculativefic") %>% 
  filter(genre!="novel") %>% 
  filter(genre!="youngadult")

df2 %>% count(genre) %>% 
  ggplot(.,aes(x=reorder(genre,n),y=n,fill=genre)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values = mycolors_named) +
  theme_classic() +
  coord_flip()+
  geom_text(aes(label = n), hjust = 1.2,size=4) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        axis.text.y = element_text(hjust=1,size = 15),
        plot.title = element_text(hjust = 0.5,size=15),
        plot.subtitle = element_text(hjust = 0.5,size=12),
        legend.position = "none")+
  labs(x="",y="",title = "Plot 13: Number of books per genre",
       subtitle = "With significantly reduced genres")
## Second try - classification ----------------------------
set.seed(1234)
split_df <- initial_split(df2, strata = genre) 

train_df2 <- training(split_df)
test_df2<- testing(split_df)

train_df2 <- recipe(genre~., data = train_df2) %>% #making the genres the same size
  step_downsample(genre) %>% 
  prep() %>% #calculate returns recipe
  juice() #apply - returns data frame 

train_df2 %>% count(genre)

set.seed(1234)
k_folds2 <- vfold_cv(train_df2,strata=genre)  

# preprocess text

df2_rec <-
  recipe(genre ~ plot, data = train_df2) %>%
  step_tokenize(plot) %>%
  step_stopwords(plot) %>% 
  step_tokenfilter(plot, max_tokens = 1000) %>%
  step_tfidf(plot)

# the model
lasso_model <- multinom_reg(penalty = tune(), mixture = 1) %>% #mixture=1 is pure lasso
  set_mode("classification") %>%
  set_engine("glmnet")

# grid 

logistic_grid <- grid_regular(parameters(lasso_model), levels = 20)

sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")

#workflow:

lasso_wf2 <- workflow() %>%
  add_recipe(df2_rec, blueprint = sparse_bp) %>%
  add_model(lasso_model)

# Define tuning process - takes a few minutes to run

lasso_res2 <- tune_grid(
  lasso_wf2,
  k_folds2,
  grid = logistic_grid,
  control = control_resamples(save_pred = TRUE))

# test accuracy

lasso_best_acc2 <- lasso_res2 %>%
  show_best("accuracy")

lasso_best_acc2

# confusion matrix
lasso_res2 %>%
  collect_predictions() %>%
  filter(penalty == lasso_best_acc2$penalty) %>%
  filter(id == "Fold01") %>%
  conf_mat(genre, .pred_class) %>%
  autoplot(type = "heatmap") +
  scale_y_discrete(labels = function(x) str_wrap(x, 20)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 20))+
  scale_fill_gradient2(high="darkgreen", guide="colorbar")

# NB   ------ 
# Not run - some of the other models that I tried-------------------------------

set.seed(1234)
split_df <- initial_split(df1, strata = genre) 
# stratify after genre to ensure each genre have the same proportion in test and train

train_df1 <- training(split_df)
test_df1<- testing(split_df)

train_df1 <- recipe(genre~., data = train_df1) %>% #making the genres the same size
  step_downsample(genre) %>% 
  prep() %>% #calculate returns recipe
  juice() #apply - returns data frame 

train_df1 %>% count(genre)

set.seed(1234)
k_folds1 <- vfold_cv(train_df1,strata=genre)  

# preprocess text

df1_rec <-
  recipe(genre ~ plot, data = train_df1) %>%
  step_tokenize(plot) %>%
  step_stopwords(plot) %>% 
  step_tokenfilter(plot, max_tokens = 1000) %>%
  step_tfidf(plot)

# the other models 
knn_model <- nearest_neighbor(neighbors = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("kknn")  

tree_model <- 
  decision_tree(cost_complexity = tune(),tree_depth = tune())  %>% 
  set_mode("classification") %>% 
  set_engine("rpart")

# grid 

knn_grid <- grid_regular(parameters(knn_model), levels = 5, filter = c(neighbors > 1))
tree_grid <- grid_regular(cost_complexity(),tree_depth(), levels = 5)


#workflows:

knn_wf1 <- workflow() %>%
  add_recipe(df1_rec) %>%
  add_model(knn_model)

tree_wf1 <- workflow() %>%
  add_recipe(df1_rec) %>%
  add_model(tree_model)

# Define tuning process 

knn_res1 <- tune_grid(
  knn_wf1,
  k_folds1,
  grid = knn_grid,
  control = control_resamples(save_pred = TRUE)) 
#NB, this takes a while to run

tree_res1 <- tune_grid(
  tree_wf1, 
  k_folds1, 
  grid = tree_grid,
  control = control_resamples(save_pred = TRUE))
# NB, this takes  a while to run

# test accuracy

knn_best_acc1 <- knn_res1 %>%
  show_best("accuracy")

knn_best_acc1

tree_best_acc1 <- tree_res1 %>%
  show_best("accuracy")

tree_best_acc1