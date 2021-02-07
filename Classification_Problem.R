#20/1/2021
#Business Problem: You have just hired 100 employees and you want to deploy these employees 
#in different teams in the data science department.You conducted a test and scored them on
#5 skills that are: Python, R, Tableau, PowerBI and Statistics.On the basis of these scores
#you want to segment the employees so that you can deploy them in suitable teams.
#------------------------------------------------------------------------------------
# read the data
library("readxl")
df<- read_xlsx("Cluster Data 1.xlsx")

#Summary 
summary(df)
str(df)
names(df)

#Univariate Analysis
#Numerical Variable
hist(df$age)
#Most the employees in the given dataset ranges from 22-24. Overall age distribution is 22-28

hist(df$experience)
#Most of the employees are having 0-1 years of experience. Range 0-6 years

hist(df$number_projects)
#Most of the employees are having 1 project experience.

#Categorical Variable
table(df$formal_education_stats)
prop.table(table(df$formal_education_stats))
# 40 employees are having formal education in stats
pie(table(df$formal_education_stats))

table(df$formal_education_coding)
prop.table(table(df$formal_education_coding))
# 54 employees are having formal education in coding
pie(table(df$formal_education_coding))


table(df$certification_tableau)
prop.table(table(df$certification_tableau))
# 37 employees are having certification in tableau
pie(table(df$certification_tableau))


# Univariate Analysis of Test Scores
table(df$python)
prop.table(table(df$python))
barplot(table(df$python))

table(df$r)
prop.table(table(df$r))
barplot(table(df$r))

table(df$tableau)
prop.table(table(df$tableau))
barplot(table(df$tableau))

table(df$powerbi)
prop.table(table(df$powerbi))
barplot(table(df$powerbi))

table(df$stats)
prop.table(table(df$stats))
barplot(table(df$stats))

#Finding Optimal Clusters
names(df)
#Creating a dataframe with test scores only.
data1<- df[,c("python","r","tableau","powerbi","stats")]
names(data1)

# Set the seed
set.seed(50)

#Developing the Elbow plot to understand the optimal number of clusters
library(factoextra)
fviz_nbclust(data1,FUN=hcut,method="wss")
# From the graph we can see 4 is the optimal number of clusters. As after 4 there not much 
# drop in the WSS.

#Creating the proximity matrix
d<- dist(data1)
d
# Creating Hierarchical Clusters
hrcluster<- hclust(d,method= "ward.D2")
hrcluster

#Creating row level cluster
members<- cutree(hrcluster,k=4)
members

#Assigning Clusters to each employee
df$clusternumber<- members
View(df)

#Profiling of the Clusters
#Creating cluster level story

#Counting number of employees in each cluster
cluster_count<-aggregate(df[1],by= list(df$clusternumber),length)
cluster_count

#Understanding the test scores of each clusters
cluster_testscore<-aggregate(df[2:6],by= list(df$clusternumber),mean)
cluster_testscore

#Cluster 1:May be good with visualization. As thier scores are very high in PowerBI and Tableau
#Cluster 2:May be good with Modelling and statiscal analysis. Thier scores are high in Python,R and Stats.
#Cluster 3:May be good with coding.Their scores are high in R and python.
#Cluster 4:May be good with data exploration. As their scores are average in R and PowerBI.

#Further checking clusters against other demographics

#Aggregating the numerical variables
Cluster_num<-aggregate(df[7:9],by= list(df$clusternumber),mean)
Cluster_num
#Cluster 2  has the highest experience.
#Cluster 1 and 4 have same age distribution

#Aggregating the categorical variables

Cluster_cat<-aggregate(df[10:12],by= list(df$clusternumber),sum)
Cluster_cat
#Cluster 1 has the highest number of employees with Tableau Certification.
#Cluster 2 has the highest number of employees with formal education in stats and coding.
#Cluster 3 has the highest number of employees with formal education in coding.
#Cluster 4 has only two employees having formal education in stats.


# Finally, We could the cluster formation by the Hierarchical methodology is good. 

#Saving the Clusters
write.csv(df,"final_cluster.csv")









