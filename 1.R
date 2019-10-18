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



