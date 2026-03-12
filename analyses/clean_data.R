library(leaflet)
library(htmltools)
library(sp)
library(dplyr)
library(htmlwidgets)

if (!dir.exists("local_authority/")) {
  dir.create("local_authority/")
} 

gc <- read.csv("data/premises-licence-register_06082025.csv", header = TRUE)
geo <- read.csv("data/ONSPD_Online_Latest_Centroids_1060347089026503474.csv", header = TRUE)
data_all <- clean_data(gc,geo)
write.csv(data_all, file = "data/cleaned_premises_licence_data.csv", row.names = FALSE)

length(data_all$ID)
length(unique(data_all$Account.Name))
Account_Name_counts <- as.data.frame(table(data_all$Account.Name))
colnames(Account_Name_counts) <- c("Account Name", "Frequency")
Account_Name_counts

write.csv(Account_Name_counts, file = "data/Account_Name_counts.csv", row.names = FALSE)


# Premises Activity distribution
table_activity <- as.data.frame(table(data_all$Premises.Activity))
table_activity$Percentage <- round((table_activity$Freq / sum(table_activity$Freq)) * 100, 2)
colnames(table_activity) <- c("Premises Activity", "Frequency", "Percentage")  
table_activity <- table_activity[order(-table_activity$Frequency), ]
table_activity
