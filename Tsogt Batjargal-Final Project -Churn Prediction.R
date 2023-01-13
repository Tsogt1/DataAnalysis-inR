#Libraries
library(rio)
library(mice)
library(skimr)
library(missMDA)
library(tidyverse) #stringr ggplot2 dplyr
library(factoextra)
library(caret)
library(GGally)

#Manual grouped all churn reasons into 4 
link = 'Telco_customer_churn.xlsx'

df <- import(link)


#######Data Cleaning

#Drop duplicates
df <- unique(df)
dim(distinct(df))

#Checking standart devition
apply(df, 2, sd)

df$`Multiple Lines` <- replace(df$`Multiple Lines`,
                               df$`Multiple Lines`=='No phone service', 'No')

#Detecting and filling out missing values
apply(is.na(df) |
        df$`Total Charges` == '', 2, which) -> cond

cond <- unlist(cond)

df[cond,]$`Total Charges` <- df[cond, "Tenure Months"] *
  df[cond, "Monthly Charges"]


#Examine missing values
md.pattern(df,
           rotate.names = T)

#Original dataset without reasons
df_original <- df

df <- df_original


#Drop unnecessary variables
df_full <- df[ ,-c(1:9,29,30,33)]  #Removing unnecessary variables

df_empty <- df_full %>% 
  filter(is.na (df_full$`Main Reason`))

df <- drop_na(df_full) 
###Clustering

#Encoding character variables for clustering
all_dummies <- dummyVars(" ~. ",
                         data = df , 
                         fullRank = T, #Encoding
                         contrasts = T)

df_transformed <- data.frame(predict(all_dummies, newdata = df ))

#Use the k-means clustering
#1. Determine the ideal number of clusters

fviz_nbclust(x = df_transformed,
             FUNcluster = kmeans)
#Result 2 clusters
kmeans(x = df_transformed, centers = 2) -> KM

#a) Facto Extra plot
fviz_cluster(object = KM,
             data = df_transformed,
             geom = 'point',
             stand = F)

#Add the clusters to the data
df$Cluster <- KM$cluster

#df$Main_Reason <- df_original$`Main Reason`

##################################

#Main Reason vs Cluster
table(df$`Main Reason`)

agg <- drop_na(count(df, `Main Reason`, Cluster))

agg <- agg %>% 
  group_by(Cluster) %>%
  mutate(Percentage = n/sum(n)*100)%>%
  mutate(Percentage = sprintf("%0.1f", Percentage)) %>%
  arrange(agg$Cluster)

agg$Cluster <- as.character(agg$Cluster)
head(agg)

ggplot(agg) +
  geom_bar(aes(x = `Main Reason`, y = Percentage, fill = Cluster),
           stat = "identity") +facet_wrap(~Cluster) + theme_bw()

ggplot(data=agg, mapping = aes(x = `Main Reason`, y = Percentage)) +
  geom_point() +facet_wrap(~Cluster) + theme_bw()


#Now using corrgram
library(corrgram)
corrgram(df,
         upper.panel = panel.cor)
#We have classified our customers by using Clustering (Also it treats outlier values separately)
#In this case most of our most valuable customers are in cluster 1 
#But churn reasons percentages were same in both 
#I will do analysis on cluster 1  

#Drop Main reasons

df_cluster1 <- df [, -c(22)]

#My model is
#Cluster~ df_cluster1 (1) + df_cluster (2) + df_cluster (3) +...

#Make Cond a categorical variable
df_cluster1$Cluster <- as.factor(df_cluster1$Cluster)

#0. Fixing the initial randomization
set.seed(100)

#1. Split the data into training and testing
train_positions <- createDataPartition(y = df_cluster1$Cluster, #Target
                                       p = .8,        #Training %
                                       list = F)     #Avoid a list output

training <- df_cluster1[train_positions,]
testing <- df_cluster1[-train_positions,]

#2. Cross-validation and model tuning options
library(DMwR)

fit_control <- trainControl(method = 'repeatedcv',
                            number = 10,
                            repeats = 2,
                            search = 'random',
                            sampling = 'smote') #to account for unbalanced

#3. Fit an algorithm to your data
train(Cluster ~ .,
      data       = training,
      method     = 'ctree',
      preProcess = c(), #'center','scale' | 'range', 'corr'
      tuneLength =  10,
      trControl  = fit_control) -> model_fit
#######################################
train(Cluster ~ .,
      data       = training,
      na.action = na.pass,
      method     = 'rf',
      preProcess = c(), #'center','scale' | 'range', 'corr'
      tuneLength =  10,
      trControl  = fit_control) -> model_fit

####################################### Numeric value only, remove high importance!!!!!!!!

#4. Final model
model_fit$finalModel

plot(model_fit$finalModel)

summary(model_fit)
#4b. Predictions
#Fill out other Main_Reasons with NA for cluster1

#Extracting first row from original df
df_pred <- df_cluster1[1,-c(22)]

#Predicting
predict(model_fit, df_pred)


#5. Model performance
#Predictions for testing values
testing_pred <- predict(model_fit, testing) 
testing_pred <- as.data.frame(testing_pred)

#Checking performance
postResample(testing_pred, testing$Cluster)

#0.9839142 !in testing

#Extracting last column
df_empty <- df_empty[ ,-c(22)]

#Predictions for whole data 
final_pred <- predict(model_fit, df_empty) 
final_pred <- as.data.frame(final_pred)

colnames(final_pred) = 'Cluster'


df_empty <- cbind(df_empty,final_pred)
df <- rbind(df_cluster1, df_empty)

varImp(model_fit, scale = T)
#6. Variable Importance for training

V = caret::varImp(model_fit)

ggplot2::ggplot(V, aes(x=reorder(rownames(V),Cluster))) +
  geom_point( color="blue", size=1, alpha=0.6)


#Checking performance
postResample(final_pred, df_cluster1$Cluster)

#Confusion Matrix
confusionMatrix(final_pred$final_pred, df_cluster1$Main_Reason)


####################Visualization############


plot(df$`Tenure Months`~df$`Monthly Charges`,main='Tenure vs. Monthly charges',xlab='charges',ylab='Tenure',col=2)
plot(df$`Tenure Months`~df$`Total Charges` ,main='Tenure vs.Total Charges',xlab='charges',ylab='Tenure',col=3 )
plot(df$`Monthly Charges`~df$`Total Charges`, main='Monthly charges vs. Total charges',xlab='Total',ylab='Monthly',col=5)

df %>%
  group_by(Cluster) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total)) -> df_complete_plot

barplot(df_complete_plot$Total~df_complete_plot$Cluster,
        main='Clustering results with customer count',
        xlab='Cluster',
        ylab='Count',
        col=c("red","blue"),
        legend = rownames(df_complete_plot$Total))
##########

par(mfrow=c(1,1))

b<-hist(df$`Tenure Months`,xlim=c(0,80),breaks=10,main='Tenure freq histo',ylab='freq',xlab='Tenure',col=2)
text(b$mids,b$counts,labels=b$counts, adj=c(0.5, 1))

e<-hist(df$`Total Charges`,xlim=c(0,10000),breaks=8,main='Total Charges freq histo',ylab='freq',xlab='Total Charges',col=3 )
text(e$mids,e$counts,labels=e$counts, adj=c(0.5,1))

t<-hist(df$`Monthly Charges`,xlim=c(0,120),breaks=8, main='Monthly charges freq histo',ylab='freq',xlab='Monthly charges',col=5)
text(t$mids,t$counts,labels=t$counts, adj=c(0.5, 1))

t<-hist(df$`CLTV-CustomerLifeTimeValue`,xlim=c(2000,6600),breaks=10, main='Customer Lifetime Value Score freq histo',ylab='freq',xlab='CLTV Score',col=5)
text(t$mids,t$counts,labels=t$counts, adj=c(0.5, 1))

t<-hist(df$Cluster,breaks=10, main='Customer Lifetime Value Score freq histo',ylab='freq',xlab='CLTV Score',col=5)
text(t$mids,t$counts,labels=t$counts, adj=c(0.5, 1))
