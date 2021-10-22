#######Task 1-Data Preparation - Student Name: Shobhan Mitra,Student ID:45522156

#Task1.1#
##Import Data into a dataframe##
#Import Packages with Library Command#
library("ggplot2") # Data visualization
library("plotly") # Interactive data visualizations
library("psych") # For correlation visualizations
library("caret") # Machine learning
library("party") # Decision Tree
library("class")
#Import data into an R data frame
ilpd <- read.csv("E:/UQ/2019SEM2/INFS7203 - Data Mining/R PROJECTS/45522156/Data/Indian Liver Patient Dataset(ILPD).csv", sep = ",")

##Task1.2##
#Assigning names to columns
names (ilpd) <- c("Age", "Gender", "TB", "DB", "Alkphos", "Sgpt", "Sgot", "TP", "Albumin", "AG_Ratio", "Class")

##Task1.3##
#Finding the Median of column"AG_Ratio"
OmitNA<-na.omit(ilpd$AG_Ratio)
print(median(OmitNA))
#Replace NA from AG_Ratio column by median value
ilpd$AG_Ratio[is.na(ilpd$AG_Ratio)]<- 0.94

##Task1.4##
#Replace all"2" in column  "Class" with "0"
ilpd$Class[(ilpd$Class == "2")]<- "0"

##Task1.5##
#Change type of class column from integer to factor
class<-as.factor(ilpd$class)

##Task1.6##
#save dataframe into a file as.Rda
saveRDS(ilpd, file = "ilpd_processed.Rda")
