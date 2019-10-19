#Installing all packages that we might require!! And yup, I'm being overtly optimistic with randomForest.. LolXDD

install.packages(c("ggplot2", "e1071", "caret", "quanteda", "irlba", "randomForest"))
install.packages(c("lsa", "SnowballC"))


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
#kind of random number generate karne k liyeuj
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


#Defining our functions for TF 
term.frequency <- function(row){
  row/sum(row)
}

#Defining the function for IDF
inverse.doc.freq <- function(col) {
  corpus.size <- length(col)
  doc.count <- length(which(col > 0))
  
  log10(corpus.size / doc.count)
}

#Function for calculating Tf*Idf
tf.idf <- function(tf, idf) {
  tf * idf
}

# First step, normalize all documents via TF.
train.tokens.df <- apply(train.tokens.matrix, 1, term.frequency)
#apply kind of R ka loop hota hai..
dim(train.tokens.df)
#[1] 5773 3901 -> This means the matrix is transposed rn
View(train.tokens.df[1:20, 1:100])


# Second step, calculate the IDF vector 
train.tokens.idf <- apply(train.tokens.matrix, 2, inverse.doc.freq)
str(train.tokens.idf)

# Lastly, calculate TF-IDF for our training corpus.
train.tokens.tfidf <-  apply(train.tokens.df, 2, tf.idf, idf = train.tokens.idf)
dim(train.tokens.tfidf)
#[1] 5773 3901 -> Still transposed!!
View(train.tokens.tfidf[1:25, 1:25])


# Transpose the matrix
train.tokens.tfidf <- t(train.tokens.tfidf)
dim(train.tokens.tfidf)
#[1] 3901 5773 -> Back to normaL!!
View(train.tokens.tfidf[1:25, 1:25])


# Check for incopmlete cases.
#incomplete cases isliye honge kyuki, agar kisi text mein sirf smileys, sirf numbers ya surf stopwords hue to wo string null ho jaani hai!!
incomplete.cases <- which(!complete.cases(train.tokens.tfidf))
train$Text[incomplete.cases]


# Fix incomplete cases
train.tokens.tfidf[incomplete.cases,] <- rep(0.0, ncol(train.tokens.tfidf))
dim(train.tokens.tfidf)
sum(which(!complete.cases(train.tokens.tfidf)))


# Make a clean data frame using the same process as before.
train.tokens.tfidf.df <- cbind(Label = train$Label, data.frame(train.tokens.tfidf))
names(train.tokens.tfidf.df) <- make.names(names(train.tokens.tfidf.df))
View(train.tokens.tfidf.df[1:50, 1:50])
     
#Implementing n-grams!!
?tokens_ngrams #Function from quanteda to add n-grams from tokens!! 
train.tokens <- tokens_ngrams(train.tokens, n = 1:2)
train.tokens[[357]]


#Phir wahi...pehle dfm mein and then into a matrix!
train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)
train.tokens.matrix <- as.matrix(train.tokens.dfm)
train.tokens.dfm
#Document-feature matrix of: 3,901 documents, 43,158 features (99.9% sparse). 
#99.9% LOL!!!
#So, while we gain more insight, but our space complexity has exploded!!

#Ab dobara iss matrix ka tf-idf nikalte hain!!

# Normalize all documents via TF.
train.tokens.df <- apply(train.tokens.matrix, 1, term.frequency)


# Calculate the IDF vector that we will use for training and test data!
train.tokens.idf <- apply(train.tokens.matrix, 2, inverse.doc.freq)


# Calculate TF-IDF for the training corpus or the spam text 
train.tokens.tfidf <-  apply(train.tokens.df, 2, tf.idf, 
                             idf = train.tokens.idf)


# Transpose the matrix
train.tokens.tfidf <- t(train.tokens.tfidf)


# Fix incomplete cases
incomplete.cases <- which(!complete.cases(train.tokens.tfidf))
train.tokens.tfidf[incomplete.cases,] <- rep(0.0, ncol(train.tokens.tfidf))


# Make a clean data frame.
train.tokens.tfidf.df <- cbind(Label = train$Label, data.frame(train.tokens.tfidf))
names(train.tokens.tfidf.df) <- make.names(names(train.tokens.tfidf.df))
View(train.tokens.tfidf.df[1:50, 1:50])


# Cleans up unused objects in memory.
gc()
#Very very necessary!! Bhai mera 7 GB Ram lag rha hai abi :/
