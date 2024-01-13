install.packages("tidyverse")
library(tidyverse)
#the dataset was uploaded and imported.
#the imported data was read and viewed
installed.packages()
data()
#get a general info
glimpse(prevalence_by_mental_and_substance_use_disorder)
#get column names
print(colnames(prevalence_by_mental_and_substance_use_disorder))
#what are the components of Entity, Code columns and then both
prevalence_by_mental_and_substance_use_disorder %>% 
  select("Entity")
prevalence_by_mental_and_substance_use_disorder %>% 
  select("Code")
prevalence_by_mental_and_substance_use_disorder %>% 
  select("Entity","Code")
#determine if ghana is in the dataset
prevalence_by_mental_and_substance_use_disorder %>% 
  filter(Entity == 'Ghana') %>% 
  # n=50 was added to set the numberof rows to be displayed
  print(n=50)
#determine if Nigeria is in the dataset
prevalence_by_mental_and_substance_use_disorder %>% 
  filter(Entity == 'Nigeria')
#what is data can be found at column 5000.this  displays as a tibble
prevalence_by_mental_and_substance_use_disorder[5000,]
#to display as a standard data frame
as.data.frame(prevalence_by_mental_and_substance_use_disorder[5000,])
#what is the value at column 4000 for Prevalence - Drug use disorders - Sex: Both - Age: Age-standardized (Percent)
prevalence_by_mental_and_substance_use_disorder[4000,'Prevalence - Drug use disorders - Sex: Both - Age: Age-standardized (Percent)']
#extract data for Ghana
Ghana_prevalence_df <- prevalence_by_mental_and_substance_use_disorder %>% 
  filter(Entity == 'Ghana')
print(Ghana_prevalence_df)
#extract data for Nigeria
Nigeria_prevalence_df <- prevalence_by_mental_and_substance_use_disorder %>% 
  filter(Entity == 'Nigeria')
print(Nigeria_prevalence_df)
#extract data for UK
UK_prevalence_df<-prevalence_by_mental_and_substance_use_disorder %>% filter(Entity == 'United Kingdom')
#find the Sum of Nigeria depressive disorders
Nigeria_depression_disorder_sum<-sum(Nigeria_prevalence_df$`Prevalence - Depressive disorders - Sex: Both - Age: Age-standardized (Percent)`)
print(Nigeria_depression_disorder_sum)
#find the Sum of Ghana depressive disorders
Ghana_depression_disorder_sum<-sum(Ghana_prevalence_df$`Prevalence - Depressive disorders - Sex: Both - Age: Age-standardized (Percent)`)
print(Ghana_depression_disorder_sum)
#find the Sum of UK depressive disorders
UK_depression_disorder_sum <-sum(UK_prevalence_df$`Prevalence - Depressive disorders - Sex: Both - Age: Age-standardized (Percent)`)
print(UK_depression_disorder_sum)
#find the mean of UK depressive disorders
UK_depressive_disorders_mean <-mean(UK_prevalence_df$`Prevalence - Depressive disorders - Sex: Both - Age: Age-standardized (Percent)`)
print(UK_depressive_disorders_mean)
#just to select depressive disorders column
UK_depressive_disorders <-UK_prevalence_df %>% select('Prevalence - Depressive disorders - Sex: Both - Age: Age-standardized (Percent)')
print(UK_depressive_disorders,n=50)
#find the mean of Ghana depressive disorders
Ghana_depressive_disorder_mean <-mean(Ghana_prevalence_df$`Prevalence - Depressive disorders - Sex: Both - Age: Age-standardized (Percent)`)
print(Ghana_depressive_disorder_mean)
#find the mean of Nigeria depressive disorders
Nigeria_depressive_disorder_mean<-mean(Nigeria_prevalence_df$`Prevalence - Depressive disorders - Sex: Both - Age: Age-standardized (Percent)`)
print(Nigeria_depressive_disorder_mean)
#create a dataframe with country and corresponding mean
mean_data_df<-data.frame(Country=c('Ghana','Nigeria','UK'),Mean=c(Ghana_depressive_disorder_mean,Nigeria_depressive_disorder_mean,UK_depressive_disorders_mean))
print(mean_data_df)
# ggplot(data=mean_data_df)+geom_bar(mapping=aes(x='Mean',fill='Country'))+
#   labs(title='Mean Data',subtitle = 'For 3 countries',caption = 'from kaggle dataset')+
#   annotate("text",x=20,y=1500,label = "The sample",color="purple",fontface="bold",size=4.5,angle=25)
#find the cumulative some of depressive disorders in the 3 entities
Nigeria_depression_disorder_cum_sum <-cumsum(Nigeria_prevalence_df$`Prevalence - Depressive disorders - Sex: Both - Age: Age-standardized (Percent)`)
print(Nigeria_depression_disorder_cum_sum)
Ghana_depression_disorder_cum_sum <- cumsum(Ghana_prevalence_df$`Prevalence - Depressive disorders - Sex: Both - Age: Age-standardized (Percent)`)
print(Ghana_depression_disorder_cum_sum)
UK_depression_disorder_cum_sum <- cumsum(UK_prevalence_df$`Prevalence - Depressive disorders - Sex: Both - Age: Age-standardized (Percent)`)
options(max.print = 50) #this was added to display the max result
print(UK_depression_disorder_cum_sum)
#sort the depressive disorders in the 3 entities in descending order to display first 10
Nigeria_depression_disorder_sorted <- data.frame(arrange(Nigeria_prevalence_df,desc(`Prevalence - Depressive disorders - Sex: Both - Age: Age-standardized (Percent)`)))
options(max.print = 50)#the desc makes it descending order. for ascending remove desc
print(Nigeria_depression_disorder_sorted)#OR
# Nigeria_depression_disorder_sorted1 <-data.frame(Nigeria_prevalence_df[order(-Nigeria_prevalence_df$`Prevalence - Depressive disorders - Sex: Both - Age: Age-standardized (Percent)`),])
# print(Nigeria_depression_disorder_sorted1)#THE MINUS sign infront of nigeria makes it descending order
Ghana_depression_disorder_sorted <- data.frame(arrange(Ghana_prevalence_df,desc('Prevalence - Depressive disorders - Sex: Both - Age: Age-standardized (Percent)')))
print(head(Ghana_depression_disorder_sorted),10)#OR
# Ghana_depression_disorder_sorted1 <- data.frame(Ghana_prevalence_df[order(-Ghana_prevalence_df$`Prevalence - Depressive disorders - Sex: Both - Age: Age-standardized (Percent)`),])
# print(head(Ghana_depression_disorder_sorted1),10)
UK_depression_disorder_sorted <- data.frame(arrange(UK_prevalence_df,desc('Prevalence - Depressive disorders - Sex: Both - Age: Age-standardized (Percent)')))
print(head(UK_depression_disorder_sorted),10)#OR
# UK_depression_disorder_sorted1 <- data.frame(UK_prevalence_df[order(-UK_prevalence_df$`Prevalence - Depressive disorders - Sex: Both - Age: Age-standardized (Percent)`),])
# print(UK_depression_disorder_sorted1)
#find the sum of depressive disorder for each country in the dataset
#earlier errors was due to not using back ticks
#used the aggregate function.the group by didnt give expected result
depressive_disorder_sum_by_country <- aggregate(`Prevalence - Depressive disorders - Sex: Both - Age: Age-standardized (Percent)`~`Entity`,data = `prevalence_by_mental_and_substance_use_disorder`,sum)
print(as.data.frame(depressive_disorder_sum_by_country))
# see group_by method below
# depressive_disorder_sum_by_country1 <- `prevalence_by_mental_and_substance_use_disorder` %>%
#    drop_na() %>% group_by('Entity') %>% summarise(total=sum(`Prevalence - Depressive disorders - Sex: Both - Age: Age-standardized (Percent)`))
#  print(as.data.frame(depressive_disorder_sum_by_country1))
