setwd("/Users/carlosarotine/Desktop")
project_accidents <- read.csv("US Accidents (project).csv")

project_accidents$Amenity <- as.numeric(as.logical(project_accidents$Amenity_1 == TRUE))
project_accidents$Bump <- as.numeric(as.logical(project_accidents$Bump_1 == TRUE))
project_accidents$Crossing <- as.numeric(as.logical(project_accidents$Crossing_1 == TRUE))
project_accidents$Give_way <- as.numeric(as.logical(project_accidents$Give_Way_1 == TRUE))
project_accidents$Junction <- as.numeric(as.logical(project_accidents$Junction_1 == TRUE))
project_accidents$No_exit <- as.numeric(as.logical(project_accidents$No_Exit_1 == TRUE))
project_accidents$Railway <- as.numeric(as.logical(project_accidents$Railway_1 == TRUE))
project_accidents$Roundabout <- as.numeric(as.logical(project_accidents$Roundabout_1 == TRUE))
project_accidents$Station <- as.numeric(as.logical(project_accidents$Station_1 == TRUE))
project_accidents$Stop <- as.numeric(as.logical(project_accidents$Stop_1 == TRUE))
project_accidents$Traffic_Calming <- as.numeric(as.logical(project_accidents$Traffic_Calming_1 == TRUE))
project_accidents$Traffic_Signal <- as.numeric(as.logical(project_accidents$Traffic_Signal_1 == TRUE))
project_accidents$Turning_Loop <- as.numeric(as.logical(project_accidents$Turning_Loop_1 == TRUE))
project_accidents$Night <- ifelse(project_accidents$Civil_Twilight_1 == "Night", 1, 0)
project_accidents$Day <- ifelse(project_accidents$Civil_Twilight_1 == "Day", 1, 0)



project_accidents$Visibility <- as.numeric(as.character(project_accidents$Visibility.mi._1))
project_accidents$Temp <- as.numeric(as.character(project_accidents$Temperature.F._1))
project_accidents$Wind_chill <- as.numeric(as.character(project_accidents$Wind_Chill.F._1))
project_accidents$Humidity <- as.numeric(as.character(project_accidents$Humidity..._1))
project_accidents$Wind_speed <- as.numeric(as.character(project_accidents$Wind_Speed.mph._1))
project_accidents$Pressure <- as.numeric(as.character(project_accidents$Pressure.in._1))
project_accidents$Precipitation <- as.numeric(as.character(project_accidents$Precipitation.in._1))

project_accidents$Severity_1 <- factor(project_accidents$Severity_1 , levels = c(4 , 3 , 2 , 1))


table(project_accidents$Amenity)
table(project_accidents$Bump)
table(project_accidents$Crossing)
table(project_accidents$Give_way)
table(project_accidents$Junction)
table(project_accidents$No_exit)
table(project_accidents$Railway)
table(project_accidents$Roundabout)
table(project_accidents$Station)
table(project_accidents$Stop)
table(project_accidents$Traffic_Calming)
table(project_accidents$Traffic_Signal)
table(project_accidents$Turning_Loop)


text <- read.csv("Traffictechnologytoday_Transport_TTIT202201.csv", stringsAsFactors = FALSE)

library(tm)
library(SnowballC)
length(stopwords("english"))

library(stringr)

count <- text$Heading
count_length <- length (strsplit(count, "")[[1]])
``
corpus = Corpus(VectorSource(text$Heading))
corpus$content[1]

corpus = tm_map(corpus , tolower)
corpus$content[1]

corpus = tm_map(corpus , removePunctuation)

stopwords("english")[1:20]

corpus = tm_map(corpus , removeWords , stopwords("english"))
corpus$content[1]

corpus = tm_map(corpus , stemDocument)

frequencies <- DocumentTermMatrix(corpus)
frequencies 

findFreqTerms(frequencies, lowfreq = 1000)

findFreqTerms(frequencies, lowfreq = 500)

findFreqTerms(frequencies, lowfreq = 200)

corpus$content[1:100]

ggplot(data = text) +
  geom_histogram(mapping = aes(y = Heading)) 

library(tidytext)

df <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE)

df %>%
  filter(grepl("text", text)) %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  ggplot(aes(x = n, y = word)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_discrete(drop = FALSE)

df_count <- df %>%
  filter(grepl("text", text)) %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE)

View(df_count)

install.packages("wordcloud")
library(wordcloud)
library(dplyr)

tokens <- df %>%
  unnest_tokens(word , text)

word_counts <- tokens %>%
  count(word)

wordcloud(words = word_counts$word, 
          freq = word_counts$n, 
          min.freq = 1,
          max.words = 100,
          random.order = FALSE,
          rot.per=0.35,
          colors = brewer.pal(8 , "Dark2"))

grades <- read.csv("TextbookGrades.csv")

install.packages("tree")
library(tree)

set.seed(123)
ind <- sample(2, nrow(text), replace = TRUE, prob = c(0.7, 0.3))
ind
table(ind)
7469/10586

train_set <- text[ind==1,]
View(train_set)


test_set <- text[ind==2, ]



library(nnet)  
set.seed(123)
trainIndex <- sample(1:nrow(project_accidents) , size = 0.7 * nrow(project_accidents), replace = FALSE)
trainData <- project_accidents[trainIndex , ]
testData <- project_accidents[-trainIndex , ]

NN <- nnet(
  Severity_1 ~ Amenity + Bump + Crossing + Give_way +
    Junction + No_exit + Railway + Roundabout + Station +
    Stop + Traffic_Calming + Traffic_Signal +
    Visibility + Temp + Humidity + Wind_speed +
    Pressure + Precipitation + Night, 
  data = trainData, 
  size = 25,
  linout = TRUE,
  maxit = 250
)

install.packages("devtools")

library(devtools)

source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

plot.nnet(NN, col = "red")

confusionMatrix(predicted, testData$Severity_1)

plot(NN , rep = "best")

library(neuralnet)

plot.nnet(NN , col = "blue", arrow.length = 0.1, arrow.width = 0.1)

neuralnet::plot.neuralnet(NN, col = "blue", arrow.length = 0.1, arrow.width = 0.1)

plot(NN , col = "red")

install.packages("caret")
library(caret)

install.packages("NeuralNetTools")

library(ggplot2)
library(NeuralNetTools)

ggplot(buildNeuralNet(NN)) + geom_connection_layer()


predicted <- predict(NN, newdata = testData, type = "class")

confusionMatrix(predicted, testData$Severity_1)

train_levels <- levels(trainData$Severity_1)
testData$Severity_1 <- factor(testData$Severity_1, levels = train_levels)

levels(project_accidents$Severity_1)

lapply(project_accidents , function(x) if (is.factor(x)) levels(x))

predicted_factor <- factor(predicted, levels = levels(testData$Severity_1))

# Check the factor levels of predicted_factor and testData$Severity_1
identical(levels(predicted_factor), levels(testData$Severity_1))

predicted <- factor(predict(NN, testData, type = "class"), levels = levels(testData$Severity_1))

