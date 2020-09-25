# Setting a working directory
readLines(con = "C:/Users/Kaushik Ahir/Documents/sem-3/ML/German-Credit_1.csv", n = 5)

# Importing a CSV file
df1 <- read.table(file ="C:/Users/Kaushik Ahir/Documents/sem-3/ML/German-Credit_1.csv", header = T, sep = ",")
head(df1)

# When we know that the delimiter is a coma

df1 <- read.csv(file = "C:/Users/Kaushik Ahir/Documents/sem-3/ML/German-Credit_1.csv", header = T)
head(df1)

# Importing Sheets
#Libraries

install.packages(XLConnect)
library(XLConnect)
#Merging two datasets

df1 <- read.csv(file = "C:/Users/Kaushik Ahir/Documents/sem-3/ML/German-Credit_1.csv", header = T)
head(df1)
df2 <- read.csv(file = "C:/Users/Kaushik Ahir/Documents/sem-3/ML/German-Credit_2.csv", header = T)
head(df2)
colnames(df1)

# Merge both the data frames, df1 and df2 by the common variable OBS
df3 <- merge(x = df1, y = df2, by = "OBS", all = T)
df3
head(df3)
tail(df3)
summary(df1)
str(df1)
summary(df2)
summary(df3)

# CONVERT variable into appropriate data
# Create a vector of all the column names that you know have categorical attributes
num_Attr <- c("DURATION","AMOUNT","INSTALL_RATE","AGE","NUM_CREDITS","NUM_DEPENDENTS")

# setdiff() function is a set operation that returns a vector of all the elements that other than the intersection of the two vectors
cat_Attr <- setdiff(x = colnames(df3), y = num_Attr)

# Use the as.character() function to convert the OBS variable as it is just a record number
# Whenever you convert numeric attributes into factors, do not forget to convert them into characters first
# When you convert a numeric attribute into a factor and then reconvert it into a numeric the values of the original attribute change

df3$OBS <- as.character(df3$OBS)
df3$OBS
test_vec <-c(0,1,1,1,1,0,0,0,1,0,1,0,1,0)
test_vec
fac_vec <- as.factor(test_vec)
fac_vec
reconverted_vec <- as.numeric(fac_vec)
reconverted_vec

# Convert the numerically encoded the variables to a factor by first converting it into a character
df3$RESPONSE <- as.factor(as.character(df3$RESPONSE))

# Use the apply function to convert all the numerically encoded categorical variable.
# Then replace them in the original dataframe
df_cat <- subset(df3,select = cat_Attr)
df3[,cat_Attr] <- data.frame(apply(df_cat, 2, function(x) as.factor(as.character(x))))
str(df3)

# Handling missing values
colSums(is.na(x = df3))
sum(is.na(df3))

#Dropping the records with missing values
df4 <- na.omit(df3)
dim(df3)
dim(df4)
sum(is.na(df4))

# Inputing/Subtituting missing values
library(DMwR)
manyNAs(df3, 0.1)
df3_imputed <- centralImputation(data = df3) #Central Imputation
sum(is.na(df3_imputed))
df3_imputed1 <- knnImputation(data = df3, k=5) #KNN Imputation
sum(is.na(df3_imputed1))

# Binning / Discretizing the variable
library(infotheo)
x <- c(5,6,7,8,8,8,8,8,11,20,21,22)
length(x)
x0 <- discretize(x, disc = "equalfreq", nbins = 4)
table(x0)
x1 <- discretize(x, disc = "equalwidth", nbins = 4)
table(x1)

# Binning the AMOUNT variable from the given dataset
AmtBin <- discretize(df3_imputed$AMOUNT, disc = "equalfreq",nbins = 4)
table(AmtBin)
AmtBin <- discretize(df3_imputed$AMOUNT, disc = "equalwidth",nbins = 4)
table(AmtBin)

#Dummy variables
library(dummies)
df_ex <- datasets::warpbreaks
table(df_ex$tension)
dummy_ex <- dummy(df_ex$tension)
head(dummy_ex)
df_catc<- subset(df3_imputed,select = cat_Attr)
df_cat_dummies <-data.frame(apply(df_cat, 2, function(x) dummy(x)))
dim(df_cat_dummies)

# Standardizing the data
library(vegan)
df_num <- df3_imputed[, num_Attr]
df_num2 <- decostand(x = df_num, method = "range") #Using range method
summary(df_num2)
df_num3 <- decostand(x = df_num, method = "standardize") #Using z score method
summary(df_num3)
df_final <-cbind(df_num3,df_cat)
head(df_final)

# Train Test Split
rows <- seq(1,1000,1)
set.seed(123)
trainRows <- sample(rows,600)
train_data <- df_final[trainRows,]
test_data <- df_final[-c(trainRows),]
dim(train_data)
dim(test_data)

# Build model
lm_model <- lm(AMOUNT~DURATION, data=train_data)

summary(lm_model)




# BASIC DATA VISUALIZATIONS

df <- df_final # Store the final dataset in the df variable


## Histogram

hist(df$AGE)

hist(df$AGE,col = "yellow")


## Box plot

boxplot(df$AGE,horizontal = TRUE)

boxplot(AMOUNT~RESPONSE, data = df, xlab ="TARGET", ylab = "AMOUNT", main =
          "Continuous v/s Categorical")

## Bar plot

barplot(table(df$RESPONSE)) #should have unique level and count

barplot(table(df$RESPONSE),col = "Green")

## Scatter Plot
plot(x=df$AGE,y =df$AMOUNT ,xlab = "DURATION",ylab="AMOUNT",main= "Continuo
us v/s Continuous")

## Scatter Plot                                 
plot(x=df$AGE,y =df$AMOUNT ,xlab = "DURATION",ylab="AMOUNT",main= "Continuous v/s Continuous")
