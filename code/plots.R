
summary(loan_ds_clean)

#histogram for terms for which the loan has been taken
ggplot(loan_ds_clean, aes(term, fill=term)) + geom_bar(stat = "count") +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.25, hjust=0.50) +
  labs(x="Terms", y="Frequency", title="Term Frequency", fill="Term")
#the most number of loan request are for 36 months term.. that is almost 3 times the 60 month term

#we will be considering the funded values because that is what is given by bank and all the 
#opearation and calculations will be on that


#Data sepration on loan_status to measure quantiles for different quantitative columns
charged_off_data <- loan_ds_clean %>% filter(loan_status=="Charged Off")
Current_data <- loan_ds_clean %>% filter(loan_status=="Current")
Fully_paid_data <- loan_ds_clean %>% filter(loan_status=="Fully Paid")

quantile(charged_off_data$funded_amnt, seq(0,1,0.1))
quantile(loan_ds_clean$funded_amnt, seq(0,1,0.1))
#The charged off median loan_amnt is 10000
# and if we considered data between 1st and 3rd quantile then the range is 
#between 5400 - 15000 where most of the loans have been given and charged off aswell
#as well as fully paid.

#Histogram for loan Status
ggplot(loan_ds_clean, aes(loan_status, fill=loan_status)) + geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.25, hjust=0.50) +
  labs(x="Loan Status", y="Frequency", title="Loan Status Frequency", fill="Loan Status")
#Number of the loans getting fully paid is maximum

ggplot(loan_ds_clean, aes(term, fill=loan_status)) + geom_bar(position="fill") +
  labs(x="Term", y="% of Loans", title="Loan Status Frequency across Term", fill="Loan Status")


#funded amount
ggplot(loan_ds_clean, aes(funded_amnt)) + geom_histogram(binwidth = 1000) + geom_freqpoly(aes(col="red"), binwidth=1000) +
  labs(x="Funded Amount", y="Frequency", title="Funded Amount Frequency")

#segmented funded amount
ggplot(loan_ds_clean, aes(loan_status, funded_amnt, fill=loan_status)) + geom_boxplot() + 
  scale_y_discrete(name="Funded Amount", limits=seq(0,35000, 2000)) +
  labs(x="Loan Status", title="Funded Amount Segmentation for Loan Status", fill="Loan Status") + facet_wrap(~term)
#for 36 months term the range of funded amount in which most of the loan is charged off is 5000-12000
#for 36 months term the range of funded amount in which most of the loan is charged off is 9000-20000
#From this graph it cannot be said that the loan taken in these range will be charged off.



#Interest Rate
ggplot(loan_ds_clean, aes(int_rate)) + geom_histogram(binwidth = 1) + geom_freqpoly(aes(col="red"), binwidth=1) +
  labs(x="Interest Rate", y="Frequency", title="Interest Rate Frequency")

ggplot(loan_ds_clean, aes(int_rate, fill=loan_status)) + geom_histogram(binwidth = 1,position = "fill") +
  scale_x_discrete(name="Interest Rate", limits=seq(5,25, 1))
  labs(x="Interest Rate", y="Percentage", title="Loan Status ratio for Interest Rate")
#here we can see the increase in interest rate caused increase in charge off aswell.

ggplot(loan_ds_clean, aes(loan_status, int_rate, fill=loan_status)) + geom_boxplot() +
  scale_y_discrete(name="Interest Rate", limits=seq(5,25, 1)) +
  labs(x="Loan Status", title="Interest Rates Segmentation for Loan Status", fill="Loan Status")

ggplot(loan_ds_clean, aes(loan_status, int_rate, fill=term)) + geom_boxplot() +
  scale_y_discrete(name="Interest Rate", limits=seq(5,25, 1)) +
  labs(x="Loan Status", title="Interest Rates Segmentation for Loan Status and Term", fill="Term")
#in this plot we can see that for charged off loans the interest rate has been high
#in case of both the terms


#Grades
ggplot(loan_ds_clean, aes(grade, fill=grade)) + geom_bar() +
  labs(x="Grades", y="Frequency", title="Grade Frequency", fill="Grade")
#B, A and C grades completely dominate here

ggplot(loan_ds_clean, aes(grade, fill=loan_status)) + geom_bar(position = "Fill") + facet_wrap(~ term) +
  labs(x="Grades", y="Loan Status Percent", title="Loan Status percentage in each Grade", fill="Loan Status")
#From this plot it can be seen that charged off loan status is increasing as
#the grades increases and seem maximum in E, F and G irrespective of terms.
  

#employment length
ggplot(loan_ds_clean, aes(emp_length, fill=emp_length)) + geom_bar()

ggplot(loan_ds_clean, aes(emp_length, fill=loan_status)) + geom_bar(position = "fill") + facet_wrap(~term)

ggplot(loan_ds_clean, aes(emp_length, fill=loan_status)) + geom_bar(position = "fill") + facet_grid(grade~term)




#Home Ownership
ggplot(loan_ds_clean, aes(home_ownership, fill=home_ownership)) + geom_bar()

ggplot(loan_ds_clean, aes(home_ownership, fill=loan_status)) + geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot(loan_ds_clean, aes(home_ownership, fill=loan_status)) + geom_bar(position="fill") + facet_grid(grade~emp_length)


#Annual Income
ggplot(loan_ds_clean, aes(annual_inc)) + geom_histogram(binwidth = 5000, na.rm = T)

ggplot(loan_ds_clean, aes(annual_inc, fill=loan_status)) + geom_histogram(binwidth = 1000, position = "Fill",na.rm = T)

ggplot(loan_ds_clean, aes(annual_inc, fill=loan_status)) + geom_histogram(binwidth = 1000, position = "Fill",na.rm = T) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_grid(~term)

ggplot(loan_ds_clean, aes(loan_status, annual_inc, fill=loan_status)) + geom_boxplot(na.rm = T) 
ggplot(loan_ds_clean, aes(loan_status, annual_inc, fill=loan_status)) + geom_boxplot(na.rm = T) + facet_wrap(~term)

ggplot(loan_ds_clean, aes(int_rate, annual_inc, col=loan_status)) + geom_point(na.rm = T) + geom_smooth(na.rm = T)



ggplot(loan_ds_clean, aes(loan_status, annual_inc, fill=loan_status)) + 
  geom_boxplot(na.rm = T) + facet_wrap(~grade)


#verification Status
ggplot(loan_ds_clean, aes(verification_status, fill=verification_status)) + geom_bar()


ggplot(loan_ds_clean, aes(verification_status, fill=loan_status)) + geom_bar(position="fill") 
  facet_wrap(~term)

ggplot(loan_ds_clean, aes(verification_status, fill=loan_status)) + geom_bar(position="fill") +
  facet_wrap(grade~term)


ggplot(loan_ds_clean, aes(verification_status, fill=loan_status)) + geom_bar(position="fill") +
  facet_wrap(emp_length~term)
#no clear findings

#purpose
ggplot(loan_ds_clean, aes(purpose, fill=purpose)) + geom_bar()

ggplot(loan_ds_clean, aes(purpose, fill=loan_status)) + geom_bar() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot(loan_ds_clean, aes(purpose, fill=loan_status)) + geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + facet_wrap(~term)


ggplot(loan_ds_clean, aes(purpose, fill=loan_status)) + geom_bar() + facet_wrap(~term) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
#comparing both the above graphs it is clear that the debt_cosolidation is the purpose where most number of 
#charge off is happening but when we compare the numbers of charge to number of paid loans we find that in 
#small business for both 30 and 60 termed loans and education purpose with 36 terms have max the charged offs.

#addr_state
ggplot(loan_ds_clean, aes(addr_state)) + geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

summary(loan_ds_clean$addr_state)
ggplot(loan_ds_clean, aes(addr_state, fill=loan_status)) + geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




#Public Bankruptcy Filed


ggplot(loan_ds_clean, aes(pub_rec_bankruptcies)) + geom_bar() +
  labs(x="Public Bankruptcy Filed", y="Count", title="Public Bankruptcy Filed vs. Count")

#the plot indiacates there are more people with No bankruptcy filed. Lets do a bit more Analysis.

ggplot(loan_ds_clean, aes(x = pub_rec_bankruptcies, fill = loan_status)) + geom_bar(position = "fill") +
  labs(x="Public Bankruptcy Filed", y="% of population", title="Public Bankruptcy Filed Vs. Loan Status", fill="Loan Status")

# The above plot indicates people who have filed for bankruptcy tend to default more. 
#Also it is evident that more the no. of bankrupties filed more the chance of default.

ggplot(loan_ds_clean, aes(x = pub_rec_bankruptcies, fill = loan_status)) +
  geom_bar(position = "fill") + facet_grid(grade~emp_length) + facet_wrap(~term)      


#--------------------------------------------------------------------------------------------------------------
# Revolving Utility

ggplot(loan_ds_clean, aes(revol_util,col = "red")) + geom_histogram(binwidth = 10, na.rm = TRUE)

# Above plot indicates that count of the revolving utility percentage is increasing till 50% and then again falls down.

ggplot(loan_ds_clean, aes(revol_util,col = "black" ,fill = loan_status)) + 
  geom_histogram(position = "fill", binwidth = 5, na.rm = TRUE) +
  labs(x="Revolving Utility", y="% of population", title="Revolving Utility vs. Loan Status")

# The above plot indicates that more the % of revolving utility the higher chance of defaulting. 

#----------------------------------------------------------------------------------------------------------------------

# Number of derogatory public records

ggplot(loan_ds_clean, aes(pub_rec)) + geom_bar() +
  labs(x="Number of derogatory public records", y="Count", title="Number of derogatory public records vs. Count")

#the plot indiacates there are more people with No derogatory public records. Lets do a bit more Analysis.

ggplot(loan_ds_clean, aes(x = pub_rec, fill = loan_status)) + geom_bar(position = "fill") +
  labs(x="Number of derogatory public records", y="% of population", title="Number of derogatory public records Vs. Loan Status")

summary(as.factor(loan_ds_clean$pub_rec))

# The above plot indicates people who have derogatory public records of 1 or 2 times tend to default more. 
# there are very less records for people with 3 and 4 derogatory public records (just 7 records for 3 and 2 records for 4)
# as the records are very less for 3 and 4 we cannot conclude any thing about them.
# Less records for 3 and 4 derogatory public records may be due to that people are becomeing careful after two
# derogatory public records. But we cannt conclude anything from it.

ggplot(loan_ds_clean, aes(x = pub_rec, fill = loan_status)) +
  geom_bar(position = "fill")  + facet_wrap(~verification_status)


#----------------------------------------------------------------------------------------------------------
# Number of open credit lines

ggplot(loan_ds_clean, aes(open_acc)) + geom_bar() +
  labs(x="Number of open credit lines", y="Count", title="Number of open credit lines vs. Count")
# from the above graph we can see that count of open credit lines increases till 7 and then falls.

ggplot(loan_ds_clean, aes(x = open_acc, fill = loan_status)) + geom_bar(position = "fill")  +
  labs(x="Number of open credit lines", y="% of population", title="Number of open credit lines Vs. Loan Status")

#could not find any insight out of the data


#------------------------------------------------------------------------------------------------------------
# The number of months since the borrower's last delinquency.

ggplot(loan_ds_clean, aes(as.numeric(mths_since_last_delinq))) + geom_histogram(binwidth = 3, na.rm = T)

ggplot(loan_ds_clean, aes(as.numeric(mths_since_last_delinq), fill = loan_status)) + geom_histogram(binwidth = 3, na.rm = T)

ggplot(loan_ds_clean, aes(as.numeric(mths_since_last_delinq), fill = loan_status)) + geom_histogram(binwidth = 3, na.rm = T, position = "fill")

#could not find any insight out of the data

#--------------------------------------------------------------------------------------------------------
# The number of inquiries in past 6 months

summary(as.factor(loan_ds_clean$inq_last_6mths))

ggplot(loan_ds_clean, aes(x = inq_last_6mths, fill = loan_status)) + geom_bar() +
  labs(x="Number of inquiries in past 6 months", y="Count", title="Number of inquiries in past 6 months vs. Count")

ggplot(loan_ds_clean, aes(x = inq_last_6mths, fill = loan_status)) + geom_bar() + facet_wrap(~emp_length) +
  labs(x="Number of inquiries in past 6 months", y="Count", title="Number of inquiries in past 6 months vs. Count")
  
  ggplot(loan_ds_clean, aes(x = inq_last_6mths, fill = loan_status)) + geom_bar(position = "fill") + facet_wrap(~emp_length)
  labs(x="Number of inquiries in past 6 months", y="Count", title="Number of inquiries in past 6 months vs. Count")

# Count of people making more no. of enquiries are reducing.

ggplot(loan_ds_clean, aes(x = as.factor(inq_last_6mths), fill = loan_status)) + geom_bar(position = "fill") +
  labs(x="Number of inquiries in past 6 months", y="% of population", title="Number of inquiries in past 6 months vs. Loan Status")

#From the plot we can conclude that as the no. of enqiries since past 6 month increases the chance of defaulting increases.

#----------------------------------------------------------------------------------------------------------
# What is the field "credit_ln_year"
#----------------------------------------------------------------------------------------------------------
# No of Delinquents since last 2 years

summary(as.factor(loan_ds_clean$delinq_2yrs))

ggplot(loan_ds_clean, aes(x = delinq_2yrs)) + geom_bar() +
  labs(x="No of Delinquents in last 2 years", y="Count", title="No of Delinquents in last 2 years vs. Count")

ggplot(loan_ds_clean, aes(x = as.factor(delinq_2yrs), fill = loan_status)) + geom_bar(position = "fill") +
  labs(x="No of Delinquents in last 2 years", y="% of population", title="No of Delinquents in last 2 years vs. Loan Status")

# There is no much indication of "No of Delinquents in last 2 years" making any effect in Loan status. 
# It remains almostsame accross values 0,1,2,3,4 and decreases for 5,6 and again increases for 7,8,9.
# But the results for 5,6,7,8,9 and 11 can be ignored as the data for them is very less and again this could be because
# there are very less chances of being delinquent for more no of times in real scenario.
# Hence No conclusion can be drawn from this.
#------------------------------------------------------------------------------------------------------------
# DTI

ggplot(loan_ds_clean, aes(x = dti, col = "red")) + geom_histogram() +
  labs(x="DTI", y="Count", title="DTI vs. Count")

# The plot shows DTI is more concerntarted between 15-25 %

ggplot(loan_ds_clean, aes(x = dti,col = "black",  fill = loan_status)) + geom_histogram(position = "fill") +
  labs(x="DTI", y="% of population", title="DTI vs. Loan Status")

# The plot shows the charged off increases as DTI increase. 

ggplot(loan_ds_clean, aes(y = dti, x = loan_status)) + geom_boxplot() +
  labs(x="Loan Status", y="DTI", title="DTI vs. Loan Status")

# the plot shows DTI is more concerntarted between 10-20 %
ggplot(loan_ds_clean, aes(x = dti,col = "black",  fill = loan_status)) + geom_histogram(position = "fill") + facet_wrap(~grade)
labs(x="DTI", y="% of population", title="DTI vs. Loan Status Vs Grade")

#------------------------------------------------------------------------------------------------------------
# Credit Holder since No. of Years

ggplot(loan_ds_clean, aes(x = credit_holder_since, col = "red")) + geom_histogram() +
  labs(x="Credit Holder since No. of Years", y="Count", title="Credit Holder since No. of Years  vs. Count")

ggplot(loan_ds_clean, aes(x = credit_holder_since,col = "black",  fill = loan_status)) + geom_histogram(position = "fill") +
  labs(x="Credit Holder since No. of Years", y="% of population", title="Credit Holder since No. of Years vs. Loan Status")

ggplot(loan_ds_clean, aes(y = credit_holder_since, x = loan_status)) + geom_boxplot() +
  labs(x="Loan Status", y="Credit Holder since No. of Years", title="Credit Holder since No. of Years vs. Loan Status")

ggplot(loan_ds_clean, aes(x = credit_holder_since,col = "black",  fill = loan_status)) + geom_histogram(position = "fill") + facet_wrap(~grade)
labs(x="Credit Holder since No. of Years", y="% of population", title="Credit Holder since No. of Years vs. Loan Status Vs Grade")



#Correleation between various numeric columns
cor_df <- read.csv("./Data/semi_cleaned_data.csv", stringsAsFactors = F)
cor_df <- cor_df[,-c(1,2,3, 24, 49)]
cormat <- round(cor(cor_df[sapply(cor_df, is.numeric)],use = "pairwise.complete.obs"),2)

corrplot(cormat, method = "square", type = "lower")

cormat <- round(cor(cor_df[sapply(cor_df, is.numeric)],use = "na.or.complete"),2)

corrplot(cormat, method = "square", type = "lower")

ggplot(melted_cormat, aes(melted_cormat$Var1, melted_cormat$Var2)) 
