Allpackages <- c("ggplot2", "e1071", "caret", "irlba", "randomForest", "dplyr", "devtools", "quanteda")

#install.packages(Allpackages)

for(i in 1:length(Allpackages)){
  require(Allpackages[i], character.only = TRUE)
}

spam.data <- read.csv(
  file = "spam.csv",
  stringsAsFactors = FALSE,
  sep = ",",
  col.names = c("Label", "Text", " ", " ", " ")
)

spam.data <- spam.data[,1:2]

# Learn if missing data?
length(which(!complete.cases(spam.data)))

#Change to factor if wanted
#spam.data$Label <- as.factor(spam.data$Label)

#Proportinal amount of data
prop.table(table(spam.data$Label))

spam.data$TextLength <- nchar(spam.data$Text)
summary(spam.data$TextLength)

ggplot(spam.data, aes(x = TextLength, fill = Label)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Text count", x = "Length of Text",
       title = "Distribution of Text Lengths with Class Labels")

set.seed(32984)
indexes <- createDataPartition(spam.data$Label, times = 1, p = .7, list = FALSE)

#View(indexes)
train.data <- spam.data[indexes,]
test.data <- spam.data[-indexes, ]

prop.table(table(train.data$Label))
prop.table(table(test.data$Label))

train.data.tokens[357]

train.data.tokens <- tokens(
  x = train.data$Text, 
  what = "word",
  remove_numbers = TRUE,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_hyphens = TRUE,
  remove_url = TRUE
)

train.data.tokens[357]

train.data.tokens <- tokens_tolower(train.data.tokens)

train.data.tokens[357]

train.data.tokens <- tokens_select(
  x = train.data.tokens,
  pattern = stopwords(),
  selection = "remove"
)

train.data.tokens[357]

train.data.tokens <- tokens_wordstem(
  x = train.data.tokens,
  language = "english"
)

train.data.tokens[357]

train.data.tokens.dfm <- dfm(
  x = train.data.tokens
)

train.data.tokens.matrix <-
  as.matrix(
    train.data.tokens.dfm
  )

View(train.data.tokens.matrix[1:10,1:10])

dim(train.data.tokens.matrix)

colnames(train.data.tokens.matrix)[1:50]

#Free, WINNER, Won look like typical words that are spam





