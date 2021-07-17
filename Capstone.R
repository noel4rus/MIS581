# Install RPOstgresSQL package and the DBI library in order to pull data from Postgres
install.packages('RPostgreSQL')
library(DBI)

# Create a connection to Postgres 
con<-dbConnect(RPostgres::Postgres(),dbname='MIS581',
host='localhost',
port=5433,
user='postgres',
password='Safe4Now')

#View table to ensure proper connection to server has been made
dbListTables(con) 

# Create a query to retrieve the necessary information for the tests 
qry<-"select * from (
select region19,
             case when retpln31 = 1 or retpln42 = 1 or retpln53 = 1 then 1 else 0 end as retire_flg,
			 case when hasfsa31 = 1 then 1 else 0 end as hasfsa_flg,
             case when sicpay31 = 1 or sicpay42 = 1 or sicpay53=1 then 1 else 0 end as sicpay_flg,
			 case when paydr31 = 1 or paydr42 = 1 or paydr53 = 1 then 1 else 0 end as paydr_flg,
			 case when payvac31 = 1 or payvac42 = 1 or payvac53 = 1 then 1 else 0 end as payvac_flg,
case when chgj3142=4 and chgj4253=4 then 1
     when (chgj3142 not in (-15,-1,4) and ychj3142 in (5,6,7,8,9,10))
	   or (chgj4253 not in (-15,-1,4) and ychj4253 in (5,6,7,8,9,10)) then 0
	  else -1 end as job_satisfaction,
case when inscov19 in (1,2) or dntins19 =1 then 1 else 0 end as med_benefits,
case when retpln31=1 or retpln42=1 or retpln53=1 or hasfsa31=1 or
          sicpay31=1 or sicpay42=1 or sicpay53=1 or paydr31 = 1 or 
		  paydr42 = 1 or paydr53 = 1 or payvac31 = 1 or payvac42 = 1 or 
		  payvac53 = 1 then 1 else 0 end as fringe_benefits
from public.h212
where empst31=1
and empst42=1
and empst53=1
) s
where job_satisfaction <> -1"

# Display the query 
qry

# Retrieve the query results amd store the results in a data frame named my_df 
query_res <- dbGetQuery(con, qry)
my_df <- as.data.frame(query_res)

# Perform summary statistics on my_df */
summary(my_df)

# Create subsets for each region to use in a matrix */
rs1<-nrow(my_df[my_df$region19==1 & my_df$job_satisfaction==1,])
rs2<-nrow(my_df[my_df$region19==2 & my_df$job_satisfaction==1,])
rs3<-nrow(my_df[my_df$region19==3 & my_df$job_satisfaction==1,])
rs4<-nrow(my_df[my_df$region19==4 & my_df$job_satisfaction==1,])
rd1<-nrow(my_df[my_df$region19==1 & my_df$job_satisfaction==0,])
rd2<-nrow(my_df[my_df$region19==2 & my_df$job_satisfaction==0,])
rd3<-nrow(my_df[my_df$region19==3 & my_df$job_satisfaction==0,])
rd4<-nrow(my_df[my_df$region19==4 & my_df$job_satisfaction==0,])
Values=matrix(c(rs1,rd1,rs2,rd2,rs3,rd3,rs4,rd4),nrow=2,ncol=4)

# Display a bar plot of the regions per job satisfaction */
barplot(Values,main="Job Satisfaction by Region",names.arg=c("Northeast","Midwest","South","West"),ylab="Job Satisfaction",col=c("Green","Red"),font.lab=1,cex.names=0.8,las=2,cex.axis=0.8)
legend("topright",c("Satisfied","Dissatisfied"),lty=1:1,cex=0.5,lwd=c(0,0),col=c("green","red"),text.font = 1)

# Create subsets for medical benefits and job satisfaction to use in a matrix */
mb1<-nrow(my_df[my_df$med_benefits==1 & my_df$job_satisfaction==1,])
mb2<-nrow(my_df[my_df$med_benefits==0 & my_df$job_satisfaction==1,])
mb3<-nrow(my_df[my_df$med_benefits==1 & my_df$job_satisfaction==0,])
mb4<-nrow(my_df[my_df$med_benefits==0 & my_df$job_satisfaction==0,])
Values=matrix(c(mb1,mb3,mb2,mb4),nrow=2,ncol=2)

# Display a bar plot of the medical benefits per job satisfaction */
barplot(Values,main="Job Satisfaction by Med Benefits",names.arg=c("Med","No Med"),ylab="Job Satisfaction",col=c("Green","Red"),font.lab=1,cex.names=0.8,las=2,cex.axis=0.8)
legend("topright",c("Satisfied","Dissatisfied"),lty=1:1,cex=0.7,lwd=c(0,0),col=c("green","red"),text.font = 1)

# Create subsets for fringe benefits and job satisfaction to use in a matrix */
fb1<-nrow(my_df[my_df$fringe_benefits==1 & my_df$job_satisfaction==1,])
fb2<-nrow(my_df[my_df$fringe_benefits==0 & my_df$job_satisfaction==1,])
fb3<-nrow(my_df[my_df$fringe_benefits==1 & my_df$job_satisfaction==0,])
fb4<-nrow(my_df[my_df$fringe_benefits==0 & my_df$job_satisfaction==0,])
Values=matrix(c(fb1,fb3,fb2,fb4),nrow=2,ncol=2)

# Display a bar plot of the fringe benefits per job satisfaction */
barplot(Values,main="Job Satisfaction by Med Benefits",names.arg=c("Fringe","No Fringe"),ylab="Job Satisfaction",col=c("Green","Red"),font.lab=1,cex.names=0.8,las=2,cex.axis=0.8)
legend("topright",c("Satisfied","Dissatisfied"),lty=1:1,cex=0.7,lwd=c(0,0),col=c("green","red"),text.font = 1)

# Load ggplot2 library to use the glm function */
library(ggplot2)  

hypothesis1.fit <- lm(my_df$job_satisfaction ~ my_df$med_benefits, data=my_df)  #Run linear regression
options(scipen=999)
summary(hypothesis1.fit)  #Provide summary statistics on logistic regression model

hypothesis2.fit <- lm(my_df$job_satisfaction ~ my_df$fringe_benefits, data=my_df)  #Run linear regression
options(scipen=999)
summary(hypothesis2.fit)  #Provide summary statistics on logistic regression model

# Create two subset data frames of my_df for job satisfied and job dissatisfied */
job_satisfied_df<-my_df[which(my_df$job_satisfaction==1),c(2,3,4,5,6)]
job_dissatisfied_df<-my_df[which(my_df$job_satisfaction==0),c(2,3,4,5,6)]

# Create a data frame to use for hypothesis 1 */
hypothesis2.1<-data.frame(
job_mood=rep(c("satisfied","dissatisfied")),
retire_pln=c(job_satisfied_df$retire_flg,job_dissatisfied_df$retire_flg),
hasfsa=c(job_satisfied_df$hasfsa_flg,job_dissatisfied_df$hasfsa_flg),
sicpay=c(job_satisfied_df$hasfsa_flg,job_dissatisfied_df$sicpay_flg),
paydr=c(job_satisfied_df$hasfsa_flg,job_dissatisfied_df$paydr_flg),
payvac=c(job_satisfied_df$hasfsa_flg,job_dissatisfied_df$payvac_flg))

logit.reg <-glm(hypothesis2.1$job_mood ~ hypothesis2.1$retire_pln + hypothesis2.1$hasfsa + hypothesis2.1$sicpay + hypothesis2.1$paydr + hypothesis2.1$payvac, 
data=hypothesis2.1, family=binomial)  #Run logistic regression
options(scipen=999)
summary(logit.reg)  #Provide summary statistics on logistic regression model