### ---------------------------------------------------------------------------------------------------------- ###
### ------------------------------------   Beginning of group20_code.R   ------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------- ###

### Title         : Statistics and Methodology Group Project
### Group Number  : 20
### Group Member  :
###                 1. Alvaro Melendez Gutierrez    (SNR: 2047635 | ANR: u203902)
###                 2. Fernando Catala Resquin      (SNR: 2048042 | ANR: u270800)
###                 3. Francesco Lauria             (SNR: 2041074 | ANR: u819639)
###                 4. Konstantinos Soiledis        (SNR: 2037001 | ANR: u226393)
###                 5. Miftahul Ridwan              (SNR: 2040778 | ANR: u989303)
### Created       : 2019 - Sept - 28
### Modified      : 2019 - Oct - 9

### ---------------------------------------------------------------------------------------------------------- ###
### --------------------------------------------   Preliminary   --------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------- ###
# Please kindly set the working directory to this "code" directory

# Defining data directory and load necessary library
dataDir       <- "../data/"
filename      <- "wvs_data.rds"
source("studentFunctions.R")
library(MASS)
library(MLmetrics)
# Set the seed
set.seed(235711)
#  Read the data ###
dat0          <-  readRDS(paste0(dataDir,filename))


### ---------------------------------------------------------------------------------------------------------- ###
### -----------------------------------------   end of Preliminary   ----------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------- ###


### ---------------------------------------------------------------------------------------------------------- ###
### ----------------------------------------------   Question   ---------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------- ###


### ---------------------------------------------------------------------------------------------------------- ###
### 1. Data Cleaning ----------------------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------- ###

### --- 1.1 List all variable to be used --------------------------------------------------------------------- ###
# Create subset for the data set
dat1                    <- dat0[, c("V2", "V10", "V11","V23","V57", "V58", "V59", "V240","V242","V248")]


### --- 1.2 Proportion of missing data for each variable ----------------------------------------------------- ###
# Proportion of Missing Data
cm                      <- colSums(is.na(dat1))
pm                      <- colMeans(is.na(dat1))
cm
pm


### --- 1.3 Check for Univariate Outliers -------------------------------------------------------------------- ###
# Detect Outliers using Boxplot Outliers
Outliers                <- lapply(dat1[ , c("V10", "V11","V23", "V58", "V59", "V242","V248")], FUN = bpOutliers)
# Report the Possible and Probable Value
Outliers$V10$probable
Outliers$V10$possible
Outliers$V11$probable
Outliers$V11$possible
Outliers$V23$probable
Outliers$V23$possible
Outliers$V58$probable  -> childrenOl #save into a variable the outliers to treat
Outliers$V58$possible
Outliers$V59$probable
Outliers$V59$possible
Outliers$V242$probable
Outliers$V242$possible
Outliers$V248$probable
Outliers$V248$possible
boxplot(dat0$V58, range = 3)


### --- 1.4 Treatment of univariate outliers ----------------------------------------------------------------- ###
# Winsorize the univariate outliers
children0       <- dat1$V58 #extract the children feature 
children1       <- winsorizeOutliers(x = children0, ol = childrenOl)
# Replace the value in subset
dat1$V58        <- winsorizeOutliers(x = children0, ol = childrenOl)


### --- 1.5 Check for multivariate outliers ------------------------------------------------------------------ ###
# Split only the continous variable to detect Multivariate Outliers
dat2            <- dat1[ ,c("V10", "V11","V23", "V58", "V59", "V242","V248")]
rownames(dat2)  <- NULL # restart the row number
prob            <- 0.99
ratio           <- 0.75
# Flag outliers using Mahalanobis Distance
## Calculate statistics to obtain MEAN and COVARIANCE
stats           <- cov.mcd(dat2, quantile.used = floor(ratio * nrow(dat2)))
## Apply Mahalanobis formula with split data, mean and covariance
MD              <- mahalanobis(x = dat2, center = stats$center, cov = stats$cov)
## Finding the CRITERIA for cutoff value
criteria        <- qchisq(prob, df = ncol(dat2))
## Flagging the multivariate outliers
dat1$MD = round(MD, 3)                      # we round Mahalanobis distance to have usable values.
dat1$OL = FALSE                             # we set an entire new column.
dat1$OL[dat1$MD > criteria] = TRUE          # we set to TRUE the values bigger than the critera.
## Counting Multivariate Outliers
sum(dat1$OL == TRUE)                        # returns the number of row flagged as outliers. 
sum(dat1$OL == FALSE)                       # returns the number of row is not flagged as outliers. 
## Save the flagged row numbers for reporting purpose
row_flagged     <- data.frame(which(dat1$OL == TRUE))
# Removing the row flagged as outliers
dat1 = dat1[dat1$OL == FALSE,]
dat1 = subset(dat1, select = -c(OL, MD))    # remove extra column produced when detecting multivariate outliers


### ---------------------------------------------------------------------------------------------------------- ###
### --------------------------------------   end of 1. Data Cleaning   --------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------- ###


### ---------------------------------------------------------------------------------------------------------- ###
### 2. Exploratory Data Analysis ----------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------- ###

### --- 2.1 Countries represented in the data ---------------------------------------------------------------- ###
# Country represented in the data:
unique(dat1$V2)


### --- 2.2 Sample sizes for each country -------------------------------------------------------------------- ###
# Sample size of each country
table(dat1$V2)


### --- 2.3 Report of means, medians, standard deviations, and ranges of each continuous variable ------------ ###
# The means, medians, standard deviations, and ranges of each continuous variable.
dat2                    <- dat1[ ,c("V10", "V11","V23", "V59", "V242", "V248")]
sd_subset               <- lapply(dat2, FUN = sd)
mean_subset             <- lapply(dat2, FUN = mean)
median_subset           <- lapply(dat2, FUN = median)
max_subset              <- lapply(dat2, FUN = max)
min_subset              <- lapply(dat2, FUN = min)

descriptives_df         <- do.call(rbind, Map(data.frame,mean=mean_subset, median=median_subset, sd=sd_subset))
descriptives_df$range   <- paste(min_subset, "-", max_subset)
View(descriptives_df)


### --- 2.4 Frequency tables of each categorical variable ---------------------------------------------------- ###
# Frequency table for Categorical Variable:
View(table(dat1$V2))    # Frequency table for Country
View(table(dat1$V57))   # Frequency table for Marital Status
View(table(dat1$V240))  # Freqeuncy table for Sex


### --- 2.5 Histograms of each variable ---------------------------------------------------------------------- ###
# Histogram for Each Variable
## Histogram for Country
dat1$Country = as.character(dat1$V2, levels = c('156', '276', '356', '643', '840'), labels = c('China', 'Germany', 'India', 'Russia', 'USA'))
dat1$Country = factor(dat1$V2, levels = c('156', '276', '356', '643', '840'), labels = c('China', 'Germany', 'India', 'Russia', 'USA'))
plot(dat1$Country, main='Countries', las=1, col= c("#333333"))
## Histogram for Feeling of Happiness
dat1$FoH = as.character(dat1$V10, levels = c('1', '2', '3', '4'), labels = c('Very Happy', 'Rather Happy', 'Not Very Happy', 'Not Happy at All'))
dat1$FoH = factor(dat1$V10, levels = c('1', '2', '3', '4'), labels = c('Very Happy', 'Rather Happy', 'Not Very Happy', 'Not Happy at All'))
plot(dat1$FoH, main='Feeling of Happiness', las=1, col=c("#333333"))
## Histogram for State of Health
dat1$SoH = as.character(dat1$V11, levels = c('1', '2', '3', '4'), labels = c('Very Good', 'Good', 'Fair', 'Poor'))
dat1$SoH = factor(dat1$V11, levels = c('1', '2', '3', '4'), labels = c('Very Good', 'Good', 'Fair', 'Poor'))
plot(dat1$SoH, main='State of Health', las=1, col=c("#333333"))
## Histogram for Satifaction with Life
dat1$SwL = as.character(dat1$V23, levels = c('1', '2', '3', '4','5','6','7','8','9','10'), labels = c('Completely Dissatisfied', '2', '3', '4','5','6','7','8','9','Completely Satisfied'))
dat1$SwL = factor(dat1$V23, levels = c('1', '2', '3', '4','5','6','7','8','9','10'), labels = c('Completely\nDissatisfied', '2', '3', '4','5','6','7','8','9','Completely\nSatisfied'))
plot(dat1$SwL, main='Satisfaction with Life', las=1, col=c("#333333"))
## Histogram for Marital Status
dat1$Marital = as.character(dat1$V57, levels = c('1', '2', '3', '4', '5', '6'), labels = c('Married', 'Living Together', 'Divorced', 'Separated', 'Widowed', 'Single'))
dat1$Marital = factor(dat1$V57, levels = c('1', '2', '3', '4', '5', '6'), labels = c('Married', 'Living Together', 'Divorced', 'Separated', 'Widowed', 'Single'))
plot(dat1$Marital, main='Marital Status', las=1, col=c("#333333"))
## Histogram for Children
dat1$Child = as.character(dat1$V58, levels = c('0', '1', '2', '3', '4', '5', '6', '7', '8'), labels = c('No Children', '1', '2', '3', '4', '5', '6', '7', '8 or more children'))
dat1$Child = factor(dat1$V58, levels = c('0', '1', '2', '3', '4', '5', '6', '7', '8'), labels = c('No Children', '1', '2', '3', '4', '5', '6', '7', '8 or more\nchildren'))
plot(dat1$Child, main='Children', las=1, col=c("#333333"))
## Histogram for Satisfaction with Financial Situation of Household
dat1$Fstat = as.character(dat1$V59, levels = c('1', '2', '3', '4','5','6','7','8','9','10'), labels = c('Completely Dissatisfied', '2', '3', '4','5','6','7','8','9','Completely Satisfied'))
dat1$Fstat = factor(dat1$V59, levels = c('1', '2', '3', '4','5','6','7','8','9','10'), labels = c('Completely\nDissatisfied', '2', '3', '4','5','6','7','8','9','Completely\nSatisfied'))
plot(dat1$Fstat, main='Satisfaction with Financial Situation', las=1, col=c("#333333"))
## Histogram for Sex
dat1$Sex = as.character(dat1$V240, levels = c('1', '2'), labels = c('Male', 'Female'))
dat1$Sex = factor(dat1$V240, levels = c('1', '2'), labels = c('Male','Female'))
plot(dat1$Sex, main='Sex', las=1, col=c("#333333"))
## Histogram for Age
hist(dat1$V242, xlab = "Age", ylab = "", main= "Age", col = c("#333333"))
## Histogram for Education Level
dat1$Edu = as.character(dat1$V248, levels = c('1','2','3','4','5','6','7','8','9'), labels = c('No Formal Education','Incomplete Primary School', 'Complete Primary School', 'Incomplete Secondary School: Technical/Vocational Type', 'Complete Secondary School: Technical/Vocational Type','Incomplete Secondary School: University-preparatory Type','Complete Secondary School: University-preparatory Type','University-Level: without degree','University: with degree'))
dat1$Edu = factor(dat1$V248, levels = c('1','2','3','4','5','6','7','8','9'), labels = c('No Formal\nEducation','Incomplete\nPrimary School', 'Complete\nPrimary School', 'Incomplete\nTechnical/Vocational', 'Complete\nTechnical/Vocational','Incomplete\nUniv-prep','Complete\nUniv-prep','University:\nwithout degree','University:\nwith degree'))
plot(dat1$Edu, main='Education Level', las=1, col=c("#333333"))
## Removing extra column
dat1 = subset(dat1, select = -c(Country, FoH, SoH, SwL, Marital, Child, Fstat, Sex, Edu))


### --- 2.6 Kernel density plots of each continuous variable ------------------------------------------------- ###
# Kernel Density Plot for Each Continous Variable
plot(density(dat1$V10)) 
plot(density(dat1$V11)) 
plot(density(dat1$V23)) 
plot(density(dat1$V59)) 
plot(density(dat1$V242)) 
plot(density(dat1$V248)) 


### ---------------------------------------------------------------------------------------------------------- ###
### --------------------------------   end of 2. Exploratory Data Analysis   --------------------------------- ###
### ---------------------------------------------------------------------------------------------------------- ###


### ---------------------------------------------------------------------------------------------------------- ###
### 3. Multiple Linear Regression ---------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------- ###

### --- Question 1 to Question 3 ----------------------------------------------------------------------------- ###
# Insert a new column for Country Name
dat1$Country_name <- factor(dat1$V2, levels = c(840, 276, 643,156,356), labels = c("USA", "Germany", "Russia","China","India"))
# Multiple Linear Regression for Country on the Feeling of Happiness (V10)
out1 <- lm(V10 ~ Country_name -1, data = dat1)
summary.cellMeans(out1)


### --- Question 4 to Question 7 ----------------------------------------------------------------------------- ###
# Multiple Linear Regression for Country after controling for Health (V11) on the Feeling of Happiness (V10)
out2 <- lm(V10 ~ Country_name + V11 -1, data = dat1)
summary.cellMeans(out2)


### ---------------------------------------------------------------------------------------------------------- ###
### --------------------------------   end of 3. Multiple Linear Regression   -------------------------------- ###
### ---------------------------------------------------------------------------------------------------------- ###


### ---------------------------------------------------------------------------------------------------------- ###
### 4. Predictive Modeling ----------------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------- ###

### --- Question 1 and Question 2 ---------------------------------------------------------------------------- ###
# Transform the categorical features in factor
dat1$maritalStatus    <- factor(dat1$V57, levels = c(1, 2, 3,4,5,6), labels = c("Married","Livinig Together as Married","Divorced", "Seperated","Widowed","Single"))
dat1$SexF             <- factor(dat1$V240, levels = c(1,2),labels=c("Male","Female"))
dat1$Country_name # this variable has been created for the task before.
# Divide the data to train and test set
ind                   <- sample(1 : nrow(dat1))
train                 <- dat1[ind[1 : 10114], ]
test                  <- dat1[ind[10115 : nrow(dat1)], ]

# Linear regression for model 1: 
## Sex, Satisfaction with Life, and Country on Satisfaction with Financial Situation of Household 
out1                  <- lm(V59~SexF + V23 + Country_name-1,data=dat1)
summary.cellMeans(out1)
# Linear regression for model 2: 
## Sex, Marital Status, and Children on Satisfaction with Financial Situation of Household 
out2                  <- lm(V59~SexF + maritalStatus + V58 -1, data=dat1) 
summary.cellMeans(out2)
# Linear regression for model 1: 
## Sex, Age, and Education Level on Satisfaction with Financial Situation of Household 
out3                  <-lm(V59~SexF + V242 + V248 -1,data=dat1)
summary.cellMeans(out3)


### --- 4.3 Cross-validation error (CVE) from each model ----------------------------------------------------- ###
# 10-fold Cross Validation on each model
cve2                   <- cv.lm(data   = train,
                                models = c("V59~SexF+V23+Country_name - 1",
                                           "V59~SexF+maritalStatus+V58 -1",
                                           "V59~SexF+V242+V248 -1"),
                                K      = 10,
                                seed   = 235711)
cve2


### --- 4.4 Best performing model ---------------------------------------------------------------------------- ###
# Return the lowest MSE among 3 models
cve2[which.min(cve2)]


### --- 4.5 Estimated prediction error of the best performing model ------------------------------------------ ###
# Estimated Prediction Error of Model 1
p1                   <- predict(out1, newdata = test)
MSE(y_pred=p1,y_true=test$V59)


### ---------------------------------------------------------------------------------------------------------- ###
### -----------------------------------   end of 4. Predictive Modeling   ------------------------------------ ###
### ---------------------------------------------------------------------------------------------------------- ###


### ---------------------------------------------------------------------------------------------------------- ###
### ------------------------------------------   end of Question   ------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------- ###


### ---------------------------------------------------------------------------------------------------------- ###
### ---------------------------------------   end of group20_code.R   ---------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------- ###
