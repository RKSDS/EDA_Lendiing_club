library(scales)
summary(loan_ds_clean)
#we will be considering the funded values because that is what is given by bank and all the 
#opearation and calculations will be on that

#Histogram plot for Loan Amount Requested
ggplot(loan_ds, aes(loan_amnt, col="red")) + geom_histogram(binwidth = 1000) +
  labs(x="Loan Amount Requested", y="Frequency", title="Loan Amount Requested Frequency", col="Bin 0f 1000")
#from this it is clear that we have maximum loan application for amount 10000
#and the range is from 5500 - 15000 where most values lie



#Histogram plot for Loan Amount Funded
ggplot(loan_ds, aes(funded_amnt, col="red")) + geom_histogram(binwidth = 1000) +
  labs(x="Loan Amount Funded", y="Frequency", title="Loan Amount Funded Frequency", col="Bin 0f 1000")
#from this it is clear that we have maximum loan application for amount 9600
#and the range is from 5400 - 15000 where most values lie
#this follows the same structure as the loan requested plot



#This shows there are few outliers and hence those needs to examined
#why they are not funded
ggplot(loan_ds, aes(diff_loan_amt_req_funded)) + geom_histogram(binwidth = 1000)
#This shows there is almost no difference in request amount and funded amount



#histogram for terms for which the loan has been taken
ggplot(loan_ds, aes(term, fill=term)) + geom_bar(stat = "count")
#the most number of loan request are for 36 months term.. that is almost 3 times the 60 month term



#Histogram for interest rates at which loans have been given
ggplot(loan_ds, aes(int_rate, col="red")) + geom_histogram(binwidth = 1) + scale_x_discrete(name="Interest Rates", limits=seq(0,30,1))
#we can safely assume the maximum number of loans are given at around 11% interest rate

#histogram showing interest rates with different colored bars for different terms
ggplot(loan_ds, aes(int_rate, fill=as.factor(term))) + geom_histogram(binwidth = 1, position = "dodge") +
  scale_x_discrete(name="Interest Rates", limits=seq(0,30,1)) +
  labs(y="Frequency", title="Interest Rate for Different terms of Loan", fill="Terms")
#at higher interest rate very few low termed loans are present

#stacked chart with ratio of number of loans passed in each term
ggplot(loan_ds, aes(int_rate, fill=as.factor(term))) + geom_histogram(binwidth = 1, position = "fill")
#Number of Loans with higher term are given on higher interest rates and vice versa

#Box plot for Interest rates in different terms
ggplot(loan_ds, aes(term, int_rate)) + geom_boxplot() +
  labs(x="Loan Term", y="Interest Rates", title="Box plot for Interest rates in different terms")
#clearly we can see for higher term the median interest rate is high
#so we can say the lower terms incur higher interest rates while higher loan terms are given low inetrest rate

#Scatter Plot showing distribution of funded amount vs interest rate in different terms
ggplot(loan_ds, aes(funded_amnt, int_rate, col=term)) + geom_point() + facet_wrap(~term)
#Density in 36months plot is higher



#Histogram for installments
ggplot(loan_ds, aes(installment, col="red")) + geom_histogram(binwidth = 100) +
  scale_x_discrete(name = "Intallments", limits=seq(0,1300,100)) 
#clearly most of the installments are between 167 and 430

#Scatter plot showing funded amount vs the installment being paid. The color represents different term
ggplot(loan_ds, aes(funded_amnt, installment)) + geom_point(aes(col=term)) +
  scale_y_discrete(name = "Interest Rate", limits = seq(0,2000,100)) +
  scale_x_discrete(name = "Funded Amount", limits = seq(0,35000,5000)) + labs(col="Terms in Months")
#From this graph we can identify two isntallments for 60 months term is lesser compared to 36 months which is obvious.



#Bar Plot for grades with terms
ggplot(loan_ds, aes(grade,fill=term)) + geom_bar(position = "fill") +
  labs(x="Grade", y="Percent of 36 vs 60 month term in each grade", col="Grades", title="Ratio of 36 to 60 month term in each grade.") 
#here it can be seen that as the Grade increases the 60months term loan are more in proportion to 36 months.

#Bar plot showing percentage of each group among total
ggplot(loan_ds, aes(grade)) + geom_bar(aes(y=(..count..)/sum(..count..))) +
  scale_y_continuous(labels = percent) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25, hjust=0.25) +
  labs(x="Grade", y="Frequency Percent", col="Grades", title="Grade Frequency for loans") 
#From graph it can be seen that most of the loans are graded B
#and as the grade increases the number of loans also decreases

#Box plot showing distribution of funded amount among groups
ggplot(loan_ds, aes(grade, funded_amnt, fill=grade)) + geom_boxplot() +
  labs(x="Grade", y="Funded Amount", fill="Grades", title="Funded amount in each grade") +
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3,show.legend = T) 
#From the graph we can see the funding amount median is going up as the grade increases
#Need to add value for mean and median on the graph

#Box plot for interest rates for each grade in both the terms
ggplot(loan_ds, aes(grade,int_rate, fill=grade)) + geom_boxplot() +
  labs(x="Grade", y="Interest Rate", fill="Grades", title="Interest Rate range for eacg grade") +
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3,show.legend = T)
#As the grades are increasing the interest rate is incresing 
#implying for more funded amount the interest rate is higher
#There is clear trend of increase which we didn't see in above plots
#THerefore it could be said that the Grades decide what is interest rate on loan.

#Installments for each grade
ggplot(loan_ds, aes(grade, installment, fill=grade)) + geom_boxplot() +
  labs(x="Grade", y="Funded Amount", fill="Grades", title="Grade Frequency for loans") +
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3,show.legend = T)
#from the above plots we can conclude that the grades are nothing but 
#order for interest rate



#bar plot for loans in each sub_grade 
ggplot(loan_ds, aes(sub_grade)) + geom_bar()
#From the plot B3 tops the chart followed by A4 

#box plot for interest rates in each sub grade
ggplot(loan_ds, aes(sub_grade, int_rate, fill=sub_grade)) + geom_boxplot()
ggplot(loan_ds, aes(loan_ds$sub_grade, loan_ds$int_rate, fill=loan_ds$sub_grade)) + geom_boxplot() + facet_wrap(~loan_ds$term)
#From these plots it can be seen that the sub grops are nothing but a way to again divide the grades into
#5 groups and they also follow increasing trend in each grade



#bar chart for employment length
ggplot(loan_ds, aes(emp_length)) + geom_bar()
#clearly people with 10 and above years of experince have applied for maximum number of loan

ggplot(loan_ds, aes(emp_length, fill=term)) + geom_bar()
#clearly in both the terms number of loans given to 10+ years experince people is max

ggplot(loan_ds, aes(emp_length, funded_amnt)) + geom_boxplot() + facet_wrap(~ term)
#median here is clearly higher for 10 or 10+ years experience people
#which implies they apply for high amount loan

ggplot(loan_ds, aes(emp_length, int_rate)) + geom_boxplot() + facet_wrap(~ term)

ggplot(loan_ds, aes(emp_length, fill=grade)) + geom_bar(position = "fill") 


ggplot(loan_ds, aes(verification_status)) + geom_bar()

ggplot(loan_ds, aes(verification_status, fill=term)) + geom_bar() #without verification loan given high risk 36months category

ggplot(loan_ds, aes(verification_status, funded_amnt)) + geom_boxplot()
#for not verified income sources the loan amount ranges form 5000-13000
#Not given maximum loan

ggplot(loan_ds, aes(verification_status, int_rate)) + geom_boxplot()
#Interest rate is also normal.. but in 36 term it looks bit higher

ggplot(loan_ds, aes(verification_status, fill=grade)) + geom_bar(position = "fill") 
