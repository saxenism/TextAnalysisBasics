#Installing all packages that we might require!! And yup, I'm being overtly optimistic with randomForest.. LolXDD

install.packages(c("ggplot2", "e1071", "caret", "quanteda", "irlba", "randomForest"))


#Loading up the CSV data(thanks Kaggle) into RStudio
spam.raw <- read.csv("spam.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-16")
View(spam.raw)

#Clean the data(remove the unnecessary columns)
spam.raw <- spam.raw[,1:2]
names(spam.raw) <- c("Label", "Text")
View(spam.raw)


#Check for any missing value
length(which(!complete.cases(spam.raw)))


#Converting class Label into a factor!!
spam.raw$Label <- as.factor(spam.raw$Label)

#Now let's get a rough idea as to the distribution of texts between spam and hams
prop.table(table(spam.raw$Label))

#Result is:
#ham      spam 
#0.8659368 0.1340632

#Taking an idea of Text length distributions in the dataset
spam.raw$TextLength <- nchar(spam.raw$Text)
summary(spam.raw$TextLength)
#Result is:
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2.00   36.00   61.00   80.12  121.00  910.00 


# Visualize distribution with ggplot2, adding segmentation for ham/spam.
library(ggplot2)

ggplot(spam.raw, aes(x = TextLength, fill = Label)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Text Count", x = "Length of Text",
       title = "Distribution of Text Lengths with Class Labels")

#we are to split our data into a training set and test set and we will use the caret package to make sure that it's a stratified split.
library(caret)

#Creating the standard 70%/30% stratified split!!
set.seed(32984)
indexes <- createDataPartition(spam.raw$Label, times = 1,
                               p = 0.7, list = FALSE)

train <- spam.raw[indexes,]
test <- spam.raw[-indexes,]

#Veifying the proportions of both new arrays..
prop.table(table(train$Label))
#      ham      spam 
#0.8659318 0.1340682 
prop.table(table(test$Label))
#      ham      spam 
#0.8659485 0.1340515 

#Ohk, now we know Caret is actually pretty awesome!! Fuse me :P


#Ok, now we would like to consider what is to be removed from our text(data pre-processing or data wrangling ya jo bhi hai!)
#We would remove
                #Casing
                #Punctuations
                #Numbers
                #Stopwords and symbols
                #And similar words (like running, run, ran, runs)


#An example to see what happens before tokenisation and pre-processing
train$Text[357]
#[1] "Dear reached railway. What happen to you"

library(quanteda)

# Tokenize SMS text messages.
train.tokens <- tokens(train$Text, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)


#After tokenisation and pre-processing (Matlab thodi si hi!!)
train.tokens[[357]]
#[1] "Dear"    "reached" "railway" "What"    "happen"  "to"      "you" 

train.tokens <- tokens_tolower(train.tokens)
train.tokens[[357]]
#[1] "dear"    "reached" "railway" "what"    "happen"  "to"      "you"   


#Dekho Idhar mast saare stopwords nikaal diye!! R is just so damn awesome!!
train.tokens <- tokens_select(train.tokens, stopwords(), 
                              selection = "remove")

stopwords()


#Now, perform stemming -> removal of similar waale words!! (run, running types)
train.tokens <- tokens_wordstem(train.tokens, language = "english")

#Let's again have a look at the previous example
train.tokens[[357]]
#[1] "dear"    "reach"   "railway" "happen" 



# Create our term-incidence-matrix....Finally!!
train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)

#dfm matlab kuch nai document frequency matrix hota hai, and noe we need to convert it to something that we can work on, namelt a matrix!
train.tokens.matrix <- as.matrix(train.tokens.dfm)
#viewing a small portion of the HUGE HUGE Matrix:
View(train.tokens.matrix[1:20, 1:100])
dim(train.tokens.matrix)
#Thius is the resuklt: [1] 3901 5773 -> The dimension of our matrix!!

#Inspecting the column names obtained due to stemming!!
colnames(train.tokens.matrix)[1:50]
