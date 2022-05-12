library(class)
library(caret)
library(readxl)
library(plyr)

# Load the data set
wineData<-read_excel("/Users/sarahilmy/Desktop/IIT/Year 2 Semester 2/Data Mining and ML/CW/Whitewine_v2.xlsx") 
str(wineData)
table(wineData$quality)

#---------------------------------Preprocessing---------------------------------

# Get scaling parameters
scalingParams <- preProcess(wineData, method=c("scale"))
print(scalingParams)
# Scale the data set
scaledData <- predict(scalingParams, wineData)
summary(scaledData)

# Check for outliers in the data set and flag them as FALSE
findOutliers <- function(x, na.rm = FALSE){
  # quantile() returns the 1st and 3rd quartile for the data set 
  quantiles <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  # interQuartileRange is the difference between the 1st and 3rd quartile
  interQuartileRange <- diff(quantiles)
  # check if x lies between 
  x > quantiles[1] - 1.5*interQuartileRange & x < quantiles[2] + 
    1.5*interQuartileRange
}

# outlierData is a dataframe containing TRUE/FALSE flags for the data set
outlierData <- colwise(findOutliers)(wineData)

# rowSums() returns the sum of the rows. 
# It contains the number of 'TRUE' values in each row.
# ncol() returns the number of columns of outlierData
# Return subset of all rows with all 12 'TRUE' values
transformedData <- subset(wineData, rowSums(outlierData) == ncol(outlierData))

str(transformedData)

#---------------------------------NbClust---------------------------------------

library(NbClust)

dataSetForNBClust <- transformedData

dataSetForNBClust$quality=NULL

set.seed(26)
# This takes 10 - 15 minutes? 13 minutes
clusterNo <- NbClust(dataSetForNBClust,distance="euclidean", min.nc=2,max.nc=10,
                     method="kmeans",index="all")

print(clusterNo)

#----------------------------------Elbow---------------------------------------- 

dataSetForElbow <- transformedData

k = 2:10
set.seed(42)	

# WSS = sapply(k, function(k) {kmeans(dataSetForElbow[1:11], centers=k)$tot.withinss})

# plot(k, WSS, type="l", xlab= "Number of k", ylab="Within sum of squares")

fviz_nbclust(transformedData, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")

#------------------------------------Gap----------------------------------------

set.seed(1226)
fviz_nbclust(transformedData, kmeans, nstart = 25,  method = "gap_stat", nboot = 100)+
  labs(subtitle = "Gap statistic method")

#--------------------------------Silhouette-------------------------------------

library(factoextra) # clustering algorithms & visualization - fviz
fviz_nbclust(transformedData, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

#----------------------------K means analysis-----------------------------------

quality <- transformedData$quality

# K means analysis with k = 2 
kMeans2 <- kmeans(transformedData,2)

kMeans2
kMeans2$centers
kMeans2$withinss
kMeans2$betweenss
kMeans2$totss
(kMeans2$betweenss)/(kMeans2$totss)

# Display confusion matrix
table(quality, kMeans2$cluster)  

# K means analysis with k = 3 
kMeans3 <- kmeans(transformedData,3)

kMeans3
kMeans3$centers
kMeans3$withinss
kMeans3$betweenss
kMeans3$totss
(kMeans3$betweenss)/(kMeans3$totss)

# Display confusion matrix
table(quality, kMeans3$cluster)  

# K means analysis with k = 4 
kMeans4 <- kmeans(transformedData,4)

kMeans4
kMeans4$centers
kMeans4$withinss
kMeans4$betweenss
kMeans4$totss
(kMeans4$betweenss)/(kMeans4$totss)

# Display confusion matrix
table(quality, kMeans4$cluster)  

#-----------------------------------PCA-----------------------------------------

library(tidyverse)   # data manipulation and visualization
library(gridExtra)  # plot arrangement
library(ggplot2)     # for ggplot and qplot
library(rlang)         # is needed in tidyverse package

# Calculate eigenvalues & eigenvectors
wineData.cov <- cov(transformedData)
wineData.eigen <- eigen(wineData.cov)
str(wineData.eigen)

# Extract the loadings
(phi <- wineData.eigen$vectors[,1:2])

phi <- -phi
row.names(phi) <- c("Fixed acidity", "Volatile acidity", "Citric acid", 
                    "Residual sugar", "Chlorides", "Free sulfur dioxide", 
                    "Total sulfur dioxide", "Density", "pH", "Sulphates", 
                    "Alcohol", "Quality")
colnames(phi) <- c("PC1", "PC2")
phi

# Calculate Principal Components scores
PC1 <- as.matrix(transformedData) %*% phi[,1] 
# %*% in R does matrix multiplication
PC2 <- as.matrix(transformedData) %*% phi[,2]

# Create data frame with Principal Components scores
PC <- data.frame(Quality = row.names(transformedData), PC1, PC2)
head(PC)

# Plot Principal Components for each Quality
ggplot(PC, aes(PC1, PC2)) + 
modelr::geom_ref_line(h = 0) +
modelr::geom_ref_line(v = 0) +
geom_text(aes(label = Quality), size = 3) +
xlab("First Principal Component") + 
ylab("Second Principal Component") + 
ggtitle("First Two Principal Components of Transformed Data")

# PCs from PCA
# use princomp to return all scores
pca <- princomp(transformedData, cor=T) # principal components analysis using correlation matrix
pc.comp <- pca$scores
pc.comp1 <- -1*pc.comp[,1]
pc.comp2 <- -1*pc.comp[,2]

pc_combined <- cbind(pc.comp1, pc.comp2)

df_new <- as.data.frame(cbind(transformedData$quality,pc_combined))
names(df_new)[1] <- 'quality'
df_new$class <- as.factor(ifelse(df_new$quality==1,'AB','NO'))

prc <- princomp(transformedData)
summary(prc)

#------------------------------PCA + K Means------------------------------------

PCDataSet <- transformedData

PCDataSet$`fixed acidity`=NULL
PCDataSet$`volatile acidity`=NULL
#PCDataSet$`citric acid`=NULL
#PCDataSet$`residual sugar`=NULL
#PCDataSet$chlorides=NULL
#PCDataSet$`free sulfur dioxide`=NULL
#PCDataSet$`total sulfur dioxide`=NULL
#PCDataSet$density=NULL
#PCDataSet$pH=NULL
#PCDataSet$sulphates=NULL
#PCDataSet$alcohol=NULL

# K means analysis with k = 4 
PCKMeans <- kmeans(PCDataSet,4)

PCKMeans
PCKMeans$centers
PCKMeans$withinss
PCKMeans$betweenss
PCKMeans$totss
(PCKMeans$betweenss)/(PCKMeans$totss)

# Display confusion matrix
table(quality, PCKMeans$cluster)  



