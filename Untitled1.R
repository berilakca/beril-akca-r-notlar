2+2
3+5
#i??lemi tamamlamak i??in ctrl+enter
#not almak i??in altgr+3
log(64)
add_two <- function(x) {x+2}
add_two(5)
var <- "ceren"
print(var)
var
var_1 <- 100
var_1

var_2 <- c(1, 2, 3)
var_2
var_3 <- c("Ceren", "Beril")
var_3
var <- 1:100
var
var_2[2]
var[c(8, 10, 20)]
var < 50
var ==50

var != 43

numbers <- 1:50
print(numbers)
length(numbers)
numbers[seq(2, 10, by =5)]
df <- data.frame(
  weight = c(48, 57, 54),
  name = c("berra", "ceren", "beril")
)
print(df)
df$weight
df$name
df["name"]


x <- c(5, 6, 7)
y <- c(18, 18, 19)

plot(x, y,
     xlab = "x ekseni",
     ylab = "y ekseni" )

x <- c(1, 3, 2)
y <- x+5
plot(x, y,
     xlab = "beril ekseni",
     ylab = "ceren ekseni")
barplot(x, y,
        xlab = "beril ekseni",
        ylab = "ceren ekseni",
        col = "pink",
        border = "purple")



EXERCISE 1

days <- c("tuesday", "wednesday", "thursday", "friday", "saturday", "sunday", "monday")
var <- rep(days, 9000)
var[c(9, 54, 306, 8999)]



EXERCISE 2

numbers <- 1:100
sum([numbers %% 3 == 0 | numbers %% 5 == 0])


EXERCISE 3

seq(5, 365, 5)





Q1

rnorm(50, 20, 2)
dist<-round(rnorm(50, 20, 2))
sample(dist, 10)



library("LearnBayes")
View(studentdata)

df<- studentdata
dim(df)

is.na(df)
 sum(is.na(df))

df<- na.omit(df) 
dim(df)

gender<- df$Gender
length(gender[gender=="female"])

nrow(filter(df, gender=="female"))

#vekt??r??n uzunlu??u: length()
#bir datafeamein uzunlupu: nrow() , ncol()

plot(df$Gender, df$Height)

library(readr)

df_covid <- read_csv("http://kelesonur.github.io/compec-r/covid_sayi.csv")
head(df_covid)
View(df_covid)

boxplot(df_covid$gunluk_olum ~ df_covid$tarih)

df_covid$tarih[df_covid$tarih < "2020-07-21"]

df_covid[df_covid$tarih == "2020-07-21", 2]
df_covid[df_covid$tarih == "2020-07-21", 4]

yeni_df<-filter(df_covid, tarih > "2020-07-20")


library(dplyr)

boxplot(yeni_df$gunluk_test ~ yeni_df$tarih)


library(gapminder)

View(gapminder)
 
df<- gapminder

filter(df,country == "Turkey"& year== "1952")
df<- mutate(df, pop_million = pop / 1000000)
View(df)

install.packages("magrittr")
library(magrittr)

## %>% pipe i??areti ctrl+shift+m 

df %>% filter(country == "Turkey" & year=="2007") %>% select(pop)

library(magrittr)
library(dplyr)




HOMEWORK 1
days <- c("Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Monday")
days_rep<- rep(days, 9000)
days_rep[c(9, 54, 306, 8999)]




HOMEWORK 2
numbers<- 1:100
numbers_div <- numbers[numbers%%3==0 | numbers%%5==0]
numbers_div



HOMEWORK 3
days<- 1:365
measurement_days<- days[days%%5==0]
measurement_days



PRACTICE 1
dist<- round(rnorm(n=50, mean=20, sd=2))
dist
sample(dist, 10)



KURA
ikramiye_names<- c("Ceren", "Beril", "Berra", "S??la", "??yk??", "Irem", "Didem")
sample(ikramiye_names, 1)


PRACTICE 2
install.packages(LearnBayes)
library(LearnBayes)
df<-studentdata
View(df)
dim(df)
sum(is.na(df))
df2<-na.omit(df)
sum(is.na(df2))
View(df2)



PRACTICE 3
df2<-df
length(df$Gender[gender=="female"])


PRACTICE 4
df<-mutate(df, Height = Height*2.54)
mutate(df, Height = Height*2.54)
install.packages("dplyr")
library(dplyr)
View(dplyr)

df<- gapminder
unique(df$year)
mtcars %>%
  group_by(cyl) %>% 
  summarize(ortalama=mean(mpg))  

View(mtcars)

model<-lm(mpg~ cyl + wt, data=mtcars)
model

install.packages("rtweet") 
install.packages("wordcloud")
install.packages("stopwords")
install.packages("syuzhet")
install.packages("xlsx")

library(stringr)    
library(dplyr)     
library(magrittr)  
library(ggplot2)    
library(readr)      
library(rtweet)    
library(wordcloud) 
library(stopwords)
library(syuzhet) 
library(xlsx)


my_tweets<-read_csv("http://kelesonur.github.io/compec-r/tweets_ince.csv")
View(my_tweets)
clean_tweets <- function(x) {
  x %>%
    str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>%
    str_replace_all("&amp;", "and") %>%
    str_replace("RT @[a-z,A-Z]*: ","") %>%
    str_remove_all("[[:punct:]]") %>%
    str_replace_all("@[a-z,A-Z]*","") %>%
    str_replace_all("#[a-z,A-Z]*","") %>%
    str_remove_all("^RT:? ") %>%
    str_remove_all("@[[:alnum:]]+") %>%
    str_remove_all("#[[:alnum:]]+") %>%
    str_replace_all("\\\n", " ") %>%
    str_to_lower() %>%
    str_trim("both")
}

clean_tweet = gsub("&amp", "", my_tweets$text)
clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
clean_tweet = gsub("@\\w+", "", clean_tweet)
clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
clean_tweet = gsub("[[:digit:]]", "", clean_tweet)
clean_tweet = gsub("http\\w+", "", clean_tweet)
clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet)

my_tweets$text_clean <- clean_tweet %>% clean_tweets
stop_turkish <- data.frame(word = stopwords::stopwords("tr", source = "stopwords-iso"), stringsAsFactors = FALSE)

head(stop_turkish)

install.packages("stringr")

install.packages("tidytext")
library(tidytext)

tweets_clean <- my_tweets %>% 
  select(text_clean) %>% 
  unnest_tokens(word, text_clean) %>% 
  anti_join(stop_words) %>% 
  anti_join(stop_turkish)

tweets_clean %<>% rename(Word = word)`

stop_turkish <- data.frame(word = stopwords::stopwords("tr", source = "stopwords-iso"), stringsAsFactors = FALSE)

head(stop_turkish)


library(tidytext)

tweets_clean <- my_tweets %>% 
  select(text_clean) %>% 
  unnest_tokens(word, text_clean) %>% 
  anti_join(stop_words) %>% 
  anti_join(stop_turkish)

tweets_clean %<>% rename(Word = word)

View(tweets_clean)

words <- tweets_clean %>%
  count(Word, sort = TRUE) %>%
  ungroup()

head(words)

library(wordcloud) 
library(RColorBrewer)

wordcloud(words = words$Word, freq = words$n, min.freq = 1, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))
View(words)

read.csv("http://kelesonur.github.io/compec-r/Turkish-tr-NRC-VAD-Lexicon.txt")
Lexicon <- read_delim(file = "http://kelesonur.github.io/compec-r/Turkish-tr-NRC-VAD-Lexicon.txt", "\t", 
                      locale = locale(date_names = "tr", encoding = "UTF-8"))
library(readr)
library(magrittr)
Lexicon %<>% rename(turkishword = "Turkish-tr")
TR_Lexicon <- Lexicon %>% select(-Word)
TR_Lexicon %<>% rename(Word = turkishword)
TR_Lexicon = TR_Lexicon[!duplicated(TR_Lexicon$Word),]

.
