#import some important libraries to use on data
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(corrplot)

#import data from the csv file
loan_ds <- read.csv("./Data/loan.csv", stringsAsFactors = F)
#structure of the data frame..
str(loan_ds)
#View the data frame
View(loan_ds)

#summary of initial data frame
summary(loan_ds)

#store all column names as a vector
col_names <- colnames(loan_ds)

# here removing the columns which has more than 39500 NAs, has only one value, has one value or na
for(i in length(col_names):1)
{
  if (sum(is.na(loan_ds[,i])) > 39500)
  {
    loan_ds <- loan_ds[,-i]
  }
  else if(length(unique(loan_ds[,i]))==1)
  {
    loan_ds <- loan_ds[,-i]
  }
  else if((class(loan_ds[,i])=="integer") & (max(loan_ds[,i], na.rm = T)==min(loan_ds[,i], na.rm = T))) 
  {
    loan_ds <- loan_ds[,-i]
  }
}

#summary after cleaning the dataset
summary(loan_ds)

#find if the id is unique
sum(duplicated(loan_ds$id))
#This is id which is of no use for analysis
#we can remove this field
#adding it to remove_field which can be used to remove the column later on.
remove_fields <- c("id")

#find if the member_id is unique
sum(duplicated(loan_ds$member_id))  
#This is id which is of no use for analysis
#we can remove this field
#adding it to remove_field which can be used to remove the column later on.
remove_fields <- append(remove_fields, "member_id")

#sanity check
sum(is.na(loan_ds$loan_amnt))
sum(loan_ds$loan_amnt==0) #logically this should never be 0 but just to check if data is all fine
#this will give the number of unique loan amounts requested from bank.
length(unique(loan_ds$loan_amnt))
#we need to see if the amount funded is defaulted or not hence removing this
remove_fields <- append(remove_fields, "loan_amnt")


#sanity check
sum(is.na(loan_ds$funded_amnt))
sum(loan_ds$funded_amnt==0) #logically this could be 0 but those will go under rejected cases.
#this will give the number of unique loan amounts commited by bank.
length(unique(loan_ds$funded_amnt))


# This is getting considered with funded amount
remove_fields <- append(remove_fields, "funded_amnt_inv")


#unique terms for loan
unique(loan_ds$term)
#remove spaces from the entry
loan_ds$term <- gsub(" ", "", loan_ds$term)
#bringing the duration to numbers
loan_ds$term <- as.factor(as.character(loan_ds$term))


#check the interest rate column for NA
sum(is.na(loan_ds$int_rate))
#check the interest rate column for empty values
sum(loan_ds$int_rate %in% c("", " "))
#convert string to number
loan_ds$int_rate <- as.numeric(gsub("%", "", loan_ds$int_rate))
#check if convertion went fine for all
sum(is.na(loan_ds$int_rate))

#This is directly proportional to interest rate and principal hence not considering
remove_fields <- append(remove_fields, "installment")


#sanity check for grade
sum(is.na(loan_ds$grade))
#check how many empty entries are there
sum(loan_ds$grade %in% c("", " "))
#unique grades
unique(loan_ds$grade)
#convert it to factor as this is an ordered category
loan_ds$grade <- as.factor(loan_ds$grade)


#sanity check for grade
sum(is.na(loan_ds$sub_grade))
#check how many empty entries are there
sum(loan_ds$sub_grade %in% c("", " "))
#unique grades
unique(loan_ds$sub_grade)
#Getting rid of this as grades covers the top level categorization
remove_fields <- append(remove_fields, "sub_grade")


#sanity check for Employer Title
sum(is.na(loan_ds$emp_title))
#check how many empty entries are there
sum(loan_ds$emp_title %in% c("", " "))
#replacing all the entries with NA where data is empty
loan_ds$emp_title[loan_ds$emp_title %in% c("", " ")] <- NA
#converting all the data into single format i.e. no punctuations or space and to lower case
loan_ds$emp_title <- tolower(gsub(" ", "", loan_ds$emp_title))
loan_ds$emp_title <- gsub("[[:punct:]]","",loan_ds$emp_title)
#unique Titles
length(unique(loan_ds$emp_title)) #26446
#wont be of much use as employer directly do not make a person defaulter
remove_fields <- append(remove_fields, "emp_title")


#find Unique entries for employement duration
unique(loan_ds$emp_length)
#from above statement it is clear that n/a should be removed.
#count number of n/a
length(loan_ds$emp_length[loan_ds$emp_length == "n/a"])
#Remove years from emp_length
loan_ds$emp_length <- gsub(" years| year", "", loan_ds$emp_length)
#Replace all n/a with NA (could be from individual person not working in company)
#considering all the values mentioned as < 1 as 0
#considering all the values mentioned as 10+ as 10
loan_ds$emp_length <- sapply(loan_ds$emp_length, function(x){
  if (x=="n/a") {return(NA)} 
  else if (x=="< 1") {return(0)} 
  else if (x=="10+"){return(10)} 
  else {return(as.numeric(x))}})
#check how many na values are there it should be same as n/a values
sum(is.na(loan_ds$emp_length))
#convert to factors
loan_ds$emp_length <- as.factor(loan_ds$emp_length)


#unique type of home ownerships
unique(loan_ds$home_ownership)
#converting to factor
loan_ds$home_ownership <- as.factor(loan_ds$home_ownership)


#sanity check for annual income
sum(is.na(loan_ds$annual_inc))
##check how many 0 entries are there
sum(loan_ds$annual_inc==0)
#getting rid of outliers
loan_ds$annual_inc[loan_ds$annual_inc %in% boxplot.stats(loan_ds$annual_inc)$out] <- NA


#Sanity check for verification status
sum(is.na(loan_ds$verification_status))
#check how many empty entries are there
sum(loan_ds$verification_status %in% c("", " "))
#unique entries for income source verification status
unique(loan_ds$verification_status)
#converting to factors
loan_ds$verification_status <- as.factor(loan_ds$verification_status)


#Sanity check for issue date
sum(is.na(loan_ds$issue_d))
#check how many empty entries are there
sum(loan_ds$issue_d %in% c("", " "))
#separate the year and month
loan_ds$issue_d <- as.Date(paste("01-", loan_ds$issue_d, sep=""), format="%d-%b-%y")
#get Month from date
issue_month <- format(loan_ds$issue_d, "%b")
#get year from date
issue_year <- as.numeric(format(loan_ds$issue_d,"%Y"))
#qet quarter from date
issue_quarter <- quarters(loan_ds$issue_d)
#combining all the derived data to main df
loan_ds <- cbind(loan_ds, issue_month, issue_quarter, issue_year)
#converting the month column to an ordered category
loan_ds$issue_month <- factor(loan_ds$issue_month, 
                              levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                       "Jul", "Aug", "Sep", "Oct","Nov",  "Dec"))
remove_fields <- append(remove_fields, "issue_d")


#Sanity check for loan status
sum(is.na(loan_ds$loan_status))
#check how many empty entries are there
sum(loan_ds$loan_status %in% c("", " "))
#unique values for loan_status
unique(loan_ds$loan_status)
#convert to factor
loan_ds$loan_status <- as.factor(loan_ds$loan_status)


# dont have any use as this is as unique as id and doesn't make sense in analysis
remove_fields <- append(remove_fields, "url")

# description doesn't make much sense here again.. this is personalised message from applicant.
# it can be used by minning this to understand the underling sentiment/cause of request. But we are not 
#executing it here
remove_fields <- append(remove_fields, "desc")    


#Sanity check for Purpose
#unique values for pupose
unique(loan_ds$purpose)
#All are valid values, no NA or blank value reported 
#converting to factor
loan_ds$purpose <- as.factor(loan_ds$purpose)


#Sanity check for loan title
sum(is.na(loan_ds$title))
#check how many empty entries are there
sum(loan_ds$title %in% c("", " "))
#no of unique entries in column
length(unique(loan_ds$title)) # may not be of much use.. Individual way of naming the loan.. hence removing
remove_fields <- append(remove_fields, "title")


#Sanity check for area zip code
sum(is.na(loan_ds$zip_code))
#check how many empty entries are there
sum(loan_ds$zip_code %in% c("", " "))
#number of unique zipcode
length(unique(loan_ds$zip_code)) #823
#this cannot be used alone
remove_fields <- append(remove_fields, "zip_code")


#Sanity check for addr_state
sum(is.na(loan_ds$addr_state))
#check how many empty entries are there
sum(loan_ds$addr_state %in% c("", " "))
#number of unique addr_state
length(unique(loan_ds$addr_state)) #50
#convert to factor
loan_ds$addr_state <- as.factor(loan_ds$addr_state)


#Sanity check for dti
sum(is.na(loan_ds$dti))
#number of 0s for dti
sum(loan_ds$dti==0) #183


#no. of unique delequency events  for past 2 years of loan account
#the high the bad
unique(loan_ds$delinq_2yrs)
#number of 0s for delinq_2yrs
sum(loan_ds$delinq_2yrs==0) #35405
#convert to factor
loan_ds$delinq_2yrs <- as.factor(loan_ds$delinq_2yrs)


##this is the date on which first account opened for the user
#Sanity check for issue date
sum(is.na(loan_ds$earliest_cr_line))
#check how many empty entries are there
sum(loan_ds$earliest_cr_line %in% c("", " "))
#number of unique earliest_cr_line
unique(loan_ds$earliest_cr_line)
#separate the year and month
loan_ds <- separate(loan_ds, earliest_cr_line, into = c("crdit_ln_month", "credit_ln_year"), sep = "-")
#remove month as it is old data and months would be too much to consider for calculation
remove_fields <- append(remove_fields, "crdit_ln_month")
#converting year from yy to yyyy format. Date funtion is not used as the date rages before 1970 as well
loan_ds$credit_ln_year <- sapply(loan_ds$credit_ln_year, function(x) {if(as.numeric(x)>17) {paste("19",x, sep="")} else{paste("20",x,sep="")}})
#conevrt all entries to numbers
loan_ds$credit_ln_year <- as.numeric(loan_ds$credit_ln_year)
#check if all convertion went fine or not
sum(is.na(loan_ds$credit_ln_year))


#sanity check
unique(loan_ds$inq_last_6mths)
#All are valid values, no NA or blank value reported 
#Converting to factor
loan_ds$inq_last_6mths <- as.factor(loan_ds$inq_last_6mths)


#sanity check
unique(loan_ds$mths_since_last_delinq)
#check how many are NA values
sum(is.na(loan_ds$mths_since_last_delinq)) #25682
#convert to factors
loan_ds$mths_since_last_delinq <- as.factor(loan_ds$mths_since_last_delinq)
#60+ % data is not available hence removing from the analysis
remove_fields <- append(remove_fields, "mths_since_last_delinq")


#sanity check
unique(loan_ds$mths_since_last_record)
#check how many are NA values
sum(is.na(loan_ds$mths_since_last_record)) #36931
#mostly the values are NA almost no use of column
remove_fields <- append(remove_fields, "mths_since_last_record")


#total number of OPEN accounts (operational)
unique(loan_ds$open_acc)
#All valid values observed, no NA or blank value reported
#convert to factors
loan_ds$open_acc <- as.factor(loan_ds$open_acc)


#sanity check
unique(loan_ds$pub_rec)
#All valid values, no NA or blank value reported
#check how many of the entries are 0
sum(loan_ds$pub_rec==0) #37601
#convert to factor
loan_ds$pub_rec <- as.factor(loan_ds$pub_rec)


#installment - payment for particular month
#more mining is required here to give insight of any kind
sum(is.na(loan_ds$revol_bal))
#most of the entries are 0
sum(loan_ds$revol_bal==0) #994
#removing outliers
loan_ds$revol_bal[loan_ds$revol_bal %in% boxplot.stats(loan_ds$revol_bal)$out] <- NA


#sanity check
#check number of na entries in column
sum(is.na(loan_ds$revol_util)) #50
#check number of empty entry in column
sum(loan_ds$revol_util=="")
#check how many of the entries are 0
sum(loan_ds$revol_util=="0")
#convert the % to numbers 
loan_ds$revol_util <- as.numeric(gsub("%","",loan_ds$revol_util))
#check if all convertion went fine. Not considering NAs
sum(is.na(loan_ds$revol_util)) #50


#sanity check
#check number of na entries in column
unique(loan_ds$total_acc)
#all valid entries found
#remove_fields <- append(remove_fields, "total_acc")

#sanity check
#check number of na entries in column
sum(is.na(loan_ds$out_prncp))
#check how many of the entries are 0
sum(loan_ds$out_prncp==0)
#getting rid of this as this attribute will be funded amount + interest - total_pymnt
remove_fields <- append(remove_fields, "out_prncp")


#sanity check
#check number of na entries in column
sum(is.na(loan_ds$out_prncp_inv))
#check how many of the entries are 0
sum(loan_ds$out_prncp_inv==0)
#its more useful for analysis as coverred in out_prncp, hence removing from here
remove_fields <- append(remove_fields, "out_prncp_inv")


#sanity check
#check number of na entries in column
sum(is.na(loan_ds$total_pymnt))
#check how many of the entries are 0
sum(loan_ds$total_pymnt==0) #16


#sanity check
#check number of na entries in column
sum(is.na(loan_ds$total_pymnt_inv))
#check how many of the entries are 0
sum(loan_ds$total_pymnt_inv==0)
#its not useful for analysis as coverred in total_pymnt, hence removing from here
remove_fields <- append(remove_fields, "total_pymnt_inv")


#sanity check
#check number of na entries in column (Number of public record bankruptcies)
sum(is.na(loan_ds$pub_rec_bankruptcies))  #697
#check how many of the entries are empty
sum(loan_ds$pub_rec_bankruptcies %in% c("", " "))  #No of Blanks in the Column
#unique values in the column
unique(loan_ds$pub_rec_bankruptcies) # Listing Unique values
# converting column as factor
loan_ds$pub_rec_bankruptcies <- as.factor(loan_ds$pub_rec_bankruptcies)


#sanity check
#check number of na entries in column last_credit_pull_d
sum(is.na(loan_ds$last_credit_pull_d))
#check how many of the entries are empty
sum(loan_ds$last_credit_pull_d %in% c("", " ")) #2
#this cannot be used as it might give very old date or date for closed loans
remove_fields <- append(remove_fields, "last_credit_pull_d")


#sanity check
#check number of na entries in column next_pymnt_d (Next scheduled payment date)
sum(is.na(loan_ds$next_pymnt_d))
#check how many of the entries are empty
sum(loan_ds$next_pymnt_d %in% c("", " ")) #38577
#As lot of fields are Blank hence ignoring the field for analysis
remove_fields <- append(remove_fields, "next_pymnt_d")


#sanity check
#check number of na entries in column last_pymnt_amnt (Last total payment amount received)
sum(is.na(loan_ds$last_pymnt_amnt))
#check how many of the entries are empty
sum(loan_ds$last_pymnt_amnt %in% c("", " "))
#check how many of the entries are 0
sum(loan_ds$last_pymnt_amnt==0) #74
#this doesn't give historical insight of customer behaviour
remove_fields <- append(remove_fields, "last_pymnt_amnt")

   
#sanity check
#check number of na entries in column last_pymnt_d (Last month payment was received)
sum(is.na(loan_ds$last_pymnt_d))
#check how many of the entries are empty
sum(loan_ds$last_pymnt_d %in% c("", " "))
#remove all the spaces
loan_ds$last_pymnt_d <- tolower(gsub(" ", "", loan_ds$last_pymnt_d))
#this doesn't give historical insight of customer behaviour
remove_fields <- append(remove_fields, "last_pymnt_d")


#sanity check
#check number of na entries in column collection_recovery_fee
sum(is.na(loan_ds$collection_recovery_fee))
#check how many of the entries are empty
sum(loan_ds$collection_recovery_fee %in% c("", " "))
#check how many of the entries are 0
sum(loan_ds$collection_recovery_fee==0)#35935
# From Business point of view this comes after the user starts defaulting/becomes delinquent, 
#hence not keeping in study
remove_fields <- append(remove_fields, "collection_recovery_fee")


#sanity check
#check number of na entries recoveries (Recovery after charge off)
sum(is.na(loan_ds$recoveries))
#check how many of the entries are empty
sum(loan_ds$recoveries %in% c("", " "))
#check how many of the entries are 0
sum(loan_ds$recoveries==0)#35499
# From Business point of view this comes after the user starts defaulting/becomes delinquent, 
#hence not keeping in study
remove_fields <- append(remove_fields, "recoveries")


#sanity check
#check number of na entries total_rec_late_fee (Late fees received to date)
sum(is.na(loan_ds$total_rec_late_fee))
#check how many of the entries are empty
sum(loan_ds$total_rec_late_fee %in% c("", " "))
#check how many of the entries are 0s
sum(loan_ds$total_rec_late_fee==0)#37671
# This can be analysed as people with zero late fee tend not to deffault and repay in time.
# But this comes after Loan is given hence removing this.
remove_fields <- append(remove_fields, "total_rec_late_fee")


#sanity check
#check number of na entries total_rec_int (Interest received to date)
sum(is.na(loan_ds$total_rec_int))
#check how many of the entries are empty
sum(loan_ds$total_rec_int %in% c("", " "))
#check how many of the entries are 0s
sum(loan_ds$total_rec_int==0)#71
remove_fields <- append(remove_fields, "total_rec_int")



#Summary for total_rec_prncp (Principal received to date)
sum(is.na(loan_ds$total_rec_prncp))
#number of empty entry
sum(loan_ds$total_rec_prncp %in% c("", " "))
#number of 0 entry in the column
sum(loan_ds$total_rec_prncp==0)#74
# This comes after Loan is given and also
#this is not going to tell historical behaviour of borrower
remove_fields <- append(remove_fields, "total_rec_prncp")

#derive number of years with credit line from issued year and 1st credit issued year
credit_holder_since <- issue_year - loan_ds$credit_ln_year
#combine the derived data with main data frame

loan_ds <- cbind(loan_ds, credit_holder_since)
remove_fields <- append(remove_fields, "credit_ln_year")
remove_fields <- append(remove_fields, "issue_year")

#write the semi-cleaned dataset to csv
write.csv(loan_ds, "./Data/semi_cleaned_data.csv")

#Correleation between various numeric columns
#reading from file beacuse we need to include all the 
#formating and remove all the factors again
cor_df <- read.csv("./Data/semi_cleaned_data.csv", stringsAsFactors = F)
cor_df <- cor_df[,-c(1,2,3, 24, 49)]

cormat <- round(cor(cor_df[sapply(cor_df, is.numeric)],use = "na.or.complete"),2)
corrplot(cormat, method = "square", type = "lower")


#remove_fields
loan_ds_unwanted_fields <- loan_ds[ , (names(loan_ds) %in% remove_fields)]
#data frame with only usable columns
loan_ds_clean <- loan_ds[ , !(names(loan_ds) %in% remove_fields)]

#creating segmented data for each loan_status
charged_off_data <- loan_ds_clean %>% filter(loan_status=="Charged Off")
Current_data <- loan_ds_clean %>% filter(loan_status=="Current")
Fully_paid_data <- loan_ds_clean %>% filter(loan_status=="Fully Paid")

#summary the data we are going to consider for EDA
summary(loan_ds_clean)

#writing clean data to csv
write.csv(loan_ds_clean, "./Data/cleaned_data.csv")


#----------------------------------------------------GRAPHS and PLOTS-----------------------------------------------------------------#


#Bar plot for terms for which the loan has been taken
ggplot(loan_ds_clean, aes(term, fill=term)) + geom_bar(stat = "count") +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.25, hjust=0.50) +
  labs(x="Terms", y="Frequency", title="Term Frequency", fill="Term")
#the most number of loan request are for 36 months term.. that is almost 3 times the 60 month term


#Bar Plot for loan Status
ggplot(loan_ds_clean, aes(loan_status, fill=loan_status)) + geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.25, hjust=0.50) +
  labs(x="Loan Status", y="Frequency", title="Loan Status Frequency", fill="Loan Status")
#Number of the loans getting fully paid is maximum

#Loan Status probability across Term
ggplot(loan_ds_clean, aes(term, fill=loan_status)) + geom_bar(position="fill") +
  labs(x="Term", y="% of Loans", title="Loan Status Frequency across Term", fill="Loan Status")


#------------------------------------------Funded Amount------------------------------------------#
#funded amount histogram
ggplot(loan_ds_clean, aes(funded_amnt)) + geom_histogram(binwidth = 1000) + geom_freqpoly(aes(col="red"), binwidth=1000) +
  labs(x="Funded Amount", y="Frequency", title="Funded Amount Frequency")

#segmented funded amount
ggplot(loan_ds_clean, aes(loan_status, funded_amnt, fill=loan_status)) + geom_boxplot() + 
  scale_y_discrete(name="Funded Amount", limits=seq(0,35000, 2000)) +
  labs(x="Loan Status", title="Funded Amount Segmentation for Loan Status", fill="Loan Status") + facet_wrap(~term)
#for 36 months term the range of funded amount in which most of the loan is charged off is 5000-12000
#for 36 months term the range of funded amount in which most of the loan is charged off is 9000-20000
#From this graph it cannot be said that the loan taken in these range will be charged off.
#From this it can be concluded the amount being funded for 36 months is less than 60 months term.
#also the funded amount range is alsmost same for charged off aswellas fully paid

#we are considering the funded values because that is what is given by bank and all the 
#opearation and calculations will be on that

quantile(charged_off_data$funded_amnt, seq(0,1,0.05))
quantile(Fully_paid_data$funded_amnt, seq(0,1,0.05))
quantile(loan_ds_clean$funded_amnt, seq(0,1,0.05))
#The charged off median loan_amnt is 10000
# and if we considered data between 1st and 3rd quantile then the range is 
#between 5400 - 15000 where most of the loans have been given and charged off aswell
#as well as fully paid.



#------------------------------------------Interest Rate------------------------------------------#
#histogram and frequency poplygon for interest rate
ggplot(loan_ds_clean, aes(int_rate, col="pink")) + geom_histogram(binwidth = 1) + geom_freqpoly(aes(col="red"), binwidth=1) +
  labs(x="Interest Rate", y="Frequency", title="Interest Rate Frequency") + scale_x_discrete(limits=seq(5,26,1))
#it can be seen 9% to 16% interest rate is the range in which most of loan have been given

#ratio of loan status across interest rates.
ggplot(loan_ds_clean, aes(int_rate, fill=loan_status)) + geom_histogram(binwidth = 1,position = "fill") +
  scale_x_discrete(name="Interest Rate", limits=seq(5,25, 1)) +
labs(x="Interest Rate", y="Percentage", title="Loan Status ratio for Interest Rate", fill="Loan Status")
#here we can see the trend that as the interest rate is increasing the probability of charging off is also increasing
#This could be said that as the probability of charge off is high the interest rate would be high

#box plot showing the range of rates where most of the loans are fully paid, currently or charged off
ggplot(loan_ds_clean, aes(loan_status, int_rate, fill=loan_status)) + geom_boxplot() +
  scale_y_discrete(name="Interest Rate", limits=seq(5,25, 1)) +
  labs(x="Loan Status", title="Interest Rates Segmentation for Loan Status", fill="Loan Status")

#box plot showing the range of rates for different terms where most of the loans are fully paid, currently or charged off
ggplot(loan_ds_clean, aes(loan_status, int_rate, fill=term)) + geom_boxplot() +
  scale_y_discrete(name="Interest Rate", limits=seq(5,25, 1)) +
  labs(x="Loan Status", title="Interest Rates Segmentation for Loan Status and Term", fill="Term")
#in this plot we can confirm what we observed above as well as we can see that the interest rate is going higher for 
#60 months term compared to 36 months term

#Scatter plot to show the relation between funded amount and interest rate across terms
ggplot(loan_ds_clean, aes(funded_amnt, int_rate)) + geom_point(na.rm = T) + geom_smooth(na.rm = T) + facet_wrap(~term) +
labs(y="Interest Rate", x="Funded Amount", title="Interest Rates Vs Funded Amount for Terms")
#From the graph it can be observed that the interest rate has decreased as the amount has increased till 15k apprx.
#and then increased till 38k and started dropping again in 36 months term
#where as the interest rate almost remain constant till 8 or 9k and shows a increasing trend as amount increases 


#------------------------------------------Grades & Sub Grades------------------------------------------#
#Bar Plot for grades
ggplot(loan_ds_clean, aes(grade, fill=grade)) + geom_bar() +
  geom_text(aes(label=..count..), stat = "count", vjust=-0.35) +
  labs(x="Grades", y="Frequency", title="Grade Frequency", fill="Grade")
#B, A and C grades respectively dominate here

#Stacked Bar chart of Loan Status in each grade
ggplot(loan_ds_clean, aes(grade, fill=loan_status)) + geom_bar() + 
  labs(x="Grades", y="Loan Status Percent", title="Loan Status percentage in each Grade", fill="Loan Status") +
  theme(axis.text.x = element_text(angle = 60, size=6, hjust = 1)) 

#Stacked Bar chart showing percentage of Loan Status in each grade
ggplot(loan_ds_clean, aes(grade, fill=loan_status)) + geom_bar(position = "Fill") +
  labs(x="Grades", y="Loan Status Percent", title="Loan Status percentage in each Grade", fill="Loan Status") +
  theme(axis.text.x = element_text(angle = 60, size=6, hjust = 1)) 

#Stacked Bar chart showing percentage of Loan Status in each grade for each term
ggplot(loan_ds_clean, aes(grade, fill=loan_status)) + geom_bar(position = "Fill") +
  labs(x="Grades", y="Loan Status Percent", title="Loan Status percentage in each Grade and Term", fill="Loan Status") +
  theme(axis.text.x = element_text(angle = 60, size=6, hjust = 1)) + facet_wrap(~term)

#Stacked Bar chart of Loan Status in each sub-grade
ggplot(loan_ds, aes(sub_grade, fill=loan_status)) + geom_bar() + 
  labs(x="Sub Grades", y="Loan Status Percent", title="Loan Status percentage in each Sub Grade", fill="Loan Status") +
  theme(axis.text.x = element_text(angle = 60, size=6, hjust = 1)) 

#Stacked Bar chart showing percentage of Loan Status in each sub-grade
ggplot(loan_ds, aes(sub_grade, fill=loan_status)) + geom_bar(position = "Fill") +
  labs(x="Sub Grades", y="Loan Status Percent", title="Loan Status percentage in each Sub Grade", fill="Loan Status") +
  theme(axis.text.x = element_text(angle = 60, size=6, hjust = 1)) 
#From these plots it can be seen that charged off loan status is increasing as
#the grades increases and seem maximum in E, F and G irrespective of terms.


#------------------------------------------Employment Length------------------------------------------#

#Bar plot for employment duration
ggplot(loan_ds_clean, aes(emp_length, fill=emp_length)) + geom_bar() +
  geom_text(aes(label=..count..), stat = "count", vjust=-0.35) +
  labs(x="Years of Employment", y="Frequency", title="Years of Employment Frequency", fill="Years")
#There are maximum application from 10 or 10 + years of experienced person followed by 0 or < 1 years

#stacked bar chart of loan status for years of experience
ggplot(loan_ds_clean, aes(emp_length, fill=loan_status)) + geom_bar(position="fill") +
  labs(x="Years of Employment", y="% of Loan Status", title="Probability of Loan Status for Years of Employment", fill="Loan Status")

#stacked bar chart of loan status for years of experience categorized into terms
ggplot(loan_ds_clean, aes(emp_length, fill=loan_status)) + geom_bar(position = "fill") + facet_wrap(~term) +
  labs(x="Years of Employment", y="% of Loan Status", title="Probability of Loan Status for Years of Employment and Term of Loan", fill="Loan Status")
#From above plots we can see that people who do not show experience have a chance 20-25% of defaulting 

#stacked bar chart of loan status for years of experience categorized into terms and grades
ggplot(loan_ds_clean, aes(emp_length, fill=loan_status)) + geom_bar(position = "fill") + facet_grid(grade~term) +
  labs(x="Years of Employment", y="% of Loan Status", title="Probability of Loan Status for Years of Employment, Grade and Term", fill="Loan Status")
#we can see the same trend that the increase in grade and 60 months terms are more probable to default.
#also some one having less years of employment and taking loan in grade G is more probable to default.


#------------------------------------------Home Ownership------------------------------------------#

#Bar plot for home ownership
ggplot(loan_ds_clean, aes(home_ownership, fill=home_ownership)) + geom_bar() +
  geom_text(aes(label=..count..), stat = "count", vjust=-0.35) +
  labs(x="Ownership", y="Frequency", title="Home Ownership Frequency", fill="Type of OwnerShip")

#Stacked bar chart showing percentage of loand status for each type of ownership
ggplot(loan_ds_clean, aes(home_ownership, fill=loan_status)) + geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
labs(x="Ownership", y="% of Loan Status", title="Loan status Percentage for each Home Ownership", fill="Type of OwnerShip")

#Stacked bar chart showing percentage of loand status for each type of ownership
ggplot(loan_ds_clean, aes(home_ownership, fill=loan_status)) + geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + facet_wrap(~term) +
labs(x="Ownership", y="count", title="Loan status for each Home Ownership and Term", fill="Type of OwnerShip")

#Stacked Bar plot showing loan status for different grades with different house ownership condition
ggplot(loan_ds_clean, aes(grade, fill=loan_status)) + geom_bar() + facet_wrap(~home_ownership) +
  labs(x="Grades", y="Count", title="Loan Status ratio for diferent Grades for different ownerships", fill="Loan Status")
  
#Stacked Bar plot showing loan status percentage for different grades with different house ownership condition
ggplot(loan_ds_clean, aes(grade, fill=loan_status)) + geom_bar(position = "fill") + facet_wrap(~home_ownership) +
  labs(x="Grades", y="Percentage", title="Loan Status ratio for diferent Grades for different ownerships", fill="Loan Status")
#From this plot it can be seen that who owns a house and has other status for their ownership have a 50% chance of defaulting
# in G and F category respectively.

#Stacked Bar plot showing loan status for different employement length with different house ownership condition
ggplot(loan_ds_clean, aes(emp_length, fill=loan_status)) + geom_bar() + facet_wrap(~home_ownership) +
  labs(x="Interest Rate", y="Percentage", title="Loan Status vs Employment Length vs Home Owenership", fill="Loan Status")
  
#Stacked Bar plot showing loan status percent for different employement length with different house ownership condition
ggplot(loan_ds_clean, aes(emp_length, fill=loan_status)) + geom_bar(position = "fill") + facet_wrap(~home_ownership) +
labs(x="Interest Rate", y="Percentage", title="Loan Status ratio for Employment Length and Home Owenership", fill="Loan Status")
#nothing new can be derived here.

#------------------------------------------Annual Income------------------------------------------#
#Annual Income Histogram
ggplot(loan_ds_clean, aes(annual_inc, col="pink")) + geom_histogram(binwidth = 5000, na.rm = T) +
  geom_freqpoly(aes(col="blue"), binwidth=5000, na.rm = T) +
  labs(x="Annual Income", y="Frequency", title="Annual Income Frequency") +
  scale_x_discrete(limits=seq(0,150000,5000)) + theme(axis.text.x = element_text(angle=60,hjust = 1))
#Mostly the Annual Income is arounf 55K

#Histogram showing Loan Status percentage of loans applied in each 2000 Annual Income segment from 0-150000
ggplot(loan_ds_clean, aes(annual_inc, fill=loan_status)) + geom_histogram(binwidth = 2000, position = "Fill",na.rm = T) +
  labs(x="Annual Income", y="% of Loan Status", title="Loan Status percentage for Annual Income", fill="Loan Status") +
  scale_x_discrete(limits=seq(0,150000,5000)) + theme(axis.text.x = element_text(angle=60,hjust = 1))
#we can see that the probability if loan defaulting is higher for low income group

#Histogram showing Loan Status percentage of loans applied in each 2000 Annual Income segment from 0-150000 for different terms
ggplot(loan_ds_clean, aes(annual_inc, fill=loan_status)) + geom_histogram(binwidth = 2000, position = "Fill",na.rm = T) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + facet_grid(~term) + 
  labs(x="Annual Income", y="% of Loan Status", title="Loan Status percentage for Annual Income and Term", fill="Loan Status")
#The condition is more aggressive or clear for 60 months termed loans

#Boxplot showing the range of annual income for different loan status in different terms
ggplot(loan_ds_clean, aes(loan_status, annual_inc, fill=loan_status)) + geom_boxplot(na.rm = T) + facet_wrap(~term) +
  labs(x="Loan Status", y="Annual Income", title="Loan Status vs Annual Income vs Term", fill="Loan Status")
#clearly the annnual income for the charged off cases is lesser than other cases and short also.


#------------------------------------------Verification Status------------------------------------------#

#Bar chart for verification status
ggplot(loan_ds_clean, aes(verification_status, fill=verification_status)) + geom_bar() +
  geom_text(aes(label=..count..), stat = "count", vjust=-0.35) +
  labs(x="Verfication Status", y="Count", title="Verification Status Frequency", fill="Verification Status")
#Most of the incomes are not not verified

#Stacked Bar chart for loan status in verification status
ggplot(loan_ds_clean, aes(verification_status, fill=loan_status)) + geom_bar() +
  labs(x="Verfication Status", y="Count", title="Loan Status count in Verification Status", fill="Loan Status")

#Stacked Bar chart showing loan status percentage in verification status
ggplot(loan_ds_clean, aes(verification_status, fill=loan_status)) + geom_bar(position="fill") +
  labs(x="Verfication Status", y="% Loan Status", title="Loan Status Percentage in Verification Status", fill="Loan Status")
#Definitely we can see the Percentage of default is max for verified income group

#Stacked Bar chart showing loan status percentage in verification status for each term
ggplot(loan_ds_clean, aes(verification_status, fill=loan_status)) + geom_bar(position="fill") + facet_wrap(~term) +
  labs(x="Verfication Status", y="% Loan Status", title="Loan Status Percentage in Verification Status and Term", fill="Loan Status")
  
#We cannot support statement from above graph, when we see it across term

#Stacked Bar chart showing loan status percentage in verification status for each house ownership type
ggplot(loan_ds_clean, aes(verification_status, fill=loan_status)) + geom_bar(position="fill")+ facet_wrap(~home_ownership) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x="Verfication Status", y="% Loan Status", title="Loan Status Percentage in Verification Status and Home Ownership", fill="Loan Status")
#Others and none have negligible instances compared to others 
#From this graph it can be concluded that for loans having Mortgage, owned and rented ownership the verified status shows 
#more probability of defaulting than others.

#a histogram showing annual income for various verification status and for each status how the loan status varies from low to high income
ggplot(loan_ds_clean, aes(annual_inc, fill=loan_status)) + geom_histogram(binwidth=2000, position = "Fill",na.rm = T) + 
  facet_wrap(~verification_status) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x="Annual Income", y="% Loan Status", title="Loan Status Percentage Vs Annual Income Vs Verification Status", fill="Loan Status") 
#From this graph we can definitely say the low income segment are more probable to default but if we consider verification status
#also then the low income with verified income source seems to have more probability to default than others.

#------------------------------------------Purpose------------------------------------------#

#Bar plot for pupose
ggplot(loan_ds_clean, aes(purpose, fill=purpose)) + geom_bar() +
  geom_text(aes(label=..count..), stat = "count", vjust=-0.35) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x="Pupose of Loan", y="Count", title="Pupose Frequency", fill="Purpose")
#Most of the loans are for debt_consolidation

#stacked bar chart showing percentage of loan status for each purpose
ggplot(loan_ds_clean, aes(purpose, fill=loan_status)) + geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x="Pupose of Loan", y="% Loan Status", title="Pupose vs % Loan status Frequency", fill="Loan Status")
#small business has 25% to default of all other purposes

#stacked bar chart showing percentage of loan status for each purpose in each term
ggplot(loan_ds_clean, aes(purpose, fill=loan_status)) + geom_bar(position = "fill") + facet_wrap(~term) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x="Pupose of Loan", y="% Loan Status", title="Pupose vs % Loan status vs Term", fill="Loan Status")
#comparing both the above graphs it is clear that the debt_cosolidation is the purpose where most number of 
#charge off is happening but when we compare the numbers of charge to number of paid loans we find that in 
#small business for both 30 and 60 termed loans and education purpose with 36 terms have max the charged offs.


#------------------------------------------State------------------------------------------#

#bar chart showing number of loans applied from each state
ggplot(loan_ds_clean, aes(addr_state, fill=addr_state)) + geom_bar() +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_text(aes(label=..count..),angle=90, stat = "count", vjust=0.5, size=3, hjust=-0.25) +
  labs(x="States", y="Count", title="State Loan Frequency", fill="State")
#most come from CA

#stacked bar chart for loan status in each country
ggplot(loan_ds_clean, aes(addr_state, fill=loan_status)) + geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(x="States", y="% Loan Status", title="State Vs Percent Loan Status", fill="Loan Status")
# Nothing Clearly indicated in plot


#------------------------------------------Public Bankruptcy Filed------------------------------------------#

#Bar chart for public bankruptcy
ggplot(loan_ds_clean, aes(pub_rec_bankruptcies, fill=pub_rec_bankruptcies)) + geom_bar() +
  geom_text(aes(label=..count..), stat = "count", vjust=-0.35) +
  labs(x="Public Bankruptcy Filed", y="Count", title="Public Bankruptcy Filed vs. Count", fill="Number of Bankruptcy")
#the plot indiacates there are more people with No bankruptcy filed.

#stacked bar chart showing % of each loan status for each number of bankruptcies
ggplot(loan_ds_clean, aes(x = pub_rec_bankruptcies, fill = loan_status)) + geom_bar(position = "fill") +
  labs(x="Public Bankruptcy Filed", y="% of population", 
       title="Public Bankruptcy Filed Vs. Loan Status", fill="Loan Status")
# The above plot indicates people who have filed for bankruptcy tend to default more. 
#Also it is evident that more the no. of bankrupties filed more the chance to default.

#------------------------------------------Debt to Income Ratio------------------------------------------#
# Usually this is represented in percentage and as the dept increases or the income decreases this value
#will increase

#Histogram showing the spread of number of loans for dti
ggplot(loan_ds_clean, aes(x = dti, col = "red")) + geom_histogram(binwidth = 1) +
  scale_x_discrete(limits=seq(0,30,1)) + theme(axis.text.x = element_text(angle=60,hjust = 1)) +
  labs(x="DTI", y="Count", title="DTI vs. Count")
# The plot shows major number of loans are for DTI 5-25 %

#stacked bar chart showing the percentage of loan status for envery dti percent
ggplot(loan_ds_clean, aes(x = dti,fill = loan_status)) + 
  scale_x_discrete(limits=seq(0,30,1)) + geom_histogram(binwidth=0.3,position = "fill") +
  theme(axis.text.x = element_text(angle=60,hjust = 1)) +
  labs(x="DTI", y="% of population", title="DTI vs. Loan Status", fill= "Loan Status")
# The plot shows the charged off increases as DTI increase. 

#box plot showing the range of DTI for each loan status
ggplot(loan_ds_clean, aes(y = dti, x = loan_status, fill= loan_status)) + geom_boxplot() +
  labs(x="Loan Status", y="DTI", title="DTI vs. Loan Status", fill="Loan Status")

# the plot shows DTI is more concerntarted between 10-20 %
ggplot(loan_ds_clean, aes(x = dti,col = "black",  fill = loan_status)) + 
  geom_histogram(binwidth=1, position = "fill") + facet_wrap(~grade) +
labs(x="DTI", y="% of population", title="DTI vs. Loan Status Vs Grade")


#------------------------------------------Revolving Utility------------------------------------------#
# Percent of credit utilization by user out of total given credit

#histogram showing the number of loan for revolving utility
ggplot(loan_ds_clean, aes(revol_util,col = "red")) + geom_histogram(binwidth = 10, na.rm = TRUE) +
  scale_x_discrete(limits=seq(0,100,10)) + theme(axis.text.x = element_text(angle=60,hjust = 1)) +
  labs(x="Revolving Utility", y="Count", title="Revolving Utility vs. Count")
# Above plot indicates that count of the revolving utility percentage is increasing till 50% and then again falls down.
#also most of the population is 10-90% of credit limit

#histogram showing the loan status as percentage of loans for revolving utility
ggplot(loan_ds_clean, aes(revol_util ,fill = loan_status)) + 
  geom_histogram(position = "fill", binwidth = 5, na.rm = TRUE) +
  labs(x="Revolving Utility", y="% of population", title="Revolving Utility vs. Loan Status", fill="Loan Status")
# The above plot indicates that more the % of revolving utility the higher chance of defaulting. 

#-------------------------------No of Delinquents since last 2 years-----------------------------------#

# There are only 12 deliquencies observed i.e. 0-12
#Bar chart plot showing the number of loans for deliquencies in 2 years.
ggplot(loan_ds_clean, aes(delinq_2yrs, fill=delinq_2yrs)) + geom_bar() +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_text(aes(label=..count..), stat = "count", size=3, vjust=-0.35) +
  labs(x="No of Delinquents in last 2 years", y="Count", title="No of Delinquents in last 2 years vs. Count")
#Most of the loan requests do not have deliquencies.

#stacked Bar chart showing the loan status as percentage of loan for each deliquency
ggplot(loan_ds_clean, aes(delinq_2yrs, fill = loan_status)) + geom_bar(position = "fill") +
  labs(x="No of Delinquents in last 2 years", y="% of population", 
       title="No of Delinquents in last 2 years vs. Loan Status", fill="Loan Status")

# There is no much indication of "No of Delinquents in last 2 years" making any effect in Loan status. 
# It remains almostsame accross values 0,1,2,3,4 and decreases for 5,6 and again increases for 7,8,9.
# But the results for 5,6,7,8,9 and 11 can be ignored as the data for them is very less and again this could be because
# there are very less chances of being delinquent for more no of times in real scenario.
# Hence No conclusion can be drawn from this.

#-------------------------------No of inquiries in past 6 months-----------------------------------#

#Bar chart showing number of loans with number of inquiries in past 6 months
ggplot(loan_ds_clean, aes(x = inq_last_6mths, fill=inq_last_6mths)) + geom_bar() +
  geom_text(aes(label=..count..), stat = "count", size=3, vjust=-0.35) +
  labs(x="Number of inquiries in past 6 months", y="Count", title="Inquiry Count Frequency", fill="Inquiry Count")

#Bar chart showing loan status as count of loans for each inquiries count in past 6 months
ggplot(loan_ds_clean, aes(x = inq_last_6mths, fill = loan_status)) + geom_bar() +
  labs(x="Number of inquiries in past 6 months", y="Count", title="Inquiry Count")
# Count of people making more no. of enquiries are reducing.

#Bar chart showing loan status as percentage of loans for each inquiries count in past 6 months
ggplot(loan_ds_clean, aes(x = inq_last_6mths, fill = loan_status)) + geom_bar(position = "fill") +
  labs(x="Number of inquiries in past 6 months", y="Count", title="Number of inquiries in past 6 months vs. Count") 

#Bar chart showing loan status as percentage of loans for each inquiries count in past 6 months in each term
ggplot(loan_ds_clean, aes(x = as.factor(inq_last_6mths), fill = loan_status)) + geom_bar(position = "fill") + facet_wrap(~term) +
  labs(x="Number of inquiries in past 6 months", y="% of population", 
       title="Number of inquiries in past 6 months vs. Loan Status Vs Term")

#From the plot we can conclude that as the no. of enqiries since past 6 month increases the chance of defaulting increases.

#-------------------------------Number of open credit lines-----------------------------------#

#Histogram showing the number of loans with number of open accounts
ggplot(loan_ds_clean, aes(as.numeric(open_acc))) + geom_histogram(binwidth = 1) +
  scale_x_discrete(limits=seq(0,40,1)) + theme(axis.text.x = element_text(angle = 60)) +
  labs(x="Number of open credit lines", y="Count", title="Number of open credit lines vs. Count")
# from the above graph we can see that count of open credit lines increases till 7 and then falls smoothly.

ggplot(loan_ds_clean, aes(as.numeric(open_acc), fill = loan_status)) + geom_histogram(binwidth = 1,position = "fill")  +
  scale_x_discrete(limits=seq(0,40,1)) + theme(axis.text.x = element_text(angle = 60)) +
  labs(x="Number of open credit lines", y="% of population", title="Number of open credit lines Vs. Loan Status")

#could not find any insight out of the data

#-------------------------------Number of derogatory public records-----------------------------------#

#bar plot showing number of loans for number of derogatory public records
ggplot(loan_ds_clean, aes(pub_rec, fill=pub_rec)) + geom_bar() +
  geom_text(aes(label=..count..), stat = "count", vjust=-0.35, size=3) +
  labs(x="Number of derogatory public records", y="Count", title="Number of derogatory public records vs. Count", fill="No. of Records")
#the plot indiacates there are more people with No derogatory public records. Lets do a bit more Analysis.

#Stacked bar chart showing the loan status as percent of total loan for each count of public records
ggplot(loan_ds_clean, aes(x = pub_rec, fill = loan_status)) + geom_bar(position = "fill") +
  labs(x="Number of derogatory public records", y="% of population", 
       title="Number of derogatory public records Vs. Loan Status", fill="Loan Status")

# The above plot indicates people who have derogatory public records of 1 or 2 times tend to default more. 
# there are very less records for people with 3 and 4 derogatory public records (just 7 records for 3 and 2 records for 4)
# as the records are very less for 3 and 4 we cannot conclude any thing about them.
# Less records for 3 and 4 derogatory public records may be due to that people are becomeing careful after two
# derogatory public records. But we cannt conclude anything from it.

#-------------------------------Revolving Balance-----------------------------------#
#The amount that is not paid for this month bill and that is getting forwarded to next month

#Histogram showing the number of loans for in revolving balance 
ggplot(loan_ds_clean, aes(revol_bal)) + geom_histogram(binwidth = 1000, na.rm = T) +
   theme(axis.text.x = element_text(angle = 60)) +
  labs(x="Revolving Banlance", y="Count", title="Revolving Banlance vs. Count")
# from the above graph we can see that count of loan decreases for increase in revolving balance.

#Histogram showing the number of loans for each loan status for the revolving balance 
ggplot(loan_ds_clean, aes(revol_bal, fill=loan_status)) + geom_histogram(binwidth = 1000, na.rm = T) +
  theme(axis.text.x = element_text(angle = 60)) +
  labs(x="Revolving Banlance", y="Count", title="Revolving Banlance vs. Count", fill="Loan Status")
#This also shows the count of defaulted loans are decreasing as the number of total loans decreases 
#with increase in revolving balance

#Histogram showing the number of loans with number of open accounts
ggplot(loan_ds_clean, aes(revol_bal, fill=loan_status)) + geom_histogram(binwidth = 1000, na.rm = T, position = "fill") +
  theme(axis.text.x = element_text(angle = 60)) +
  labs(x="Revolving Banlance", y="Count", title="Revolving Banlance vs. Count", fill="Loan Status")
#Nothing can be concluded from this. It looks like the probability of defaulting increases but the 
#decline is very low to point out any thing here

#-------------------------------Total Account-----------------------------------#
#Total account with creditholder open + closed

#Histogram showing the number of loans vs total accounts 
ggplot(loan_ds_clean, aes(total_acc)) + geom_histogram(binwidth = 1, na.rm = T) +
  theme(axis.text.x = element_text(angle = 60)) +
  labs(x="Total Account", y="Count", title="Total Account vs. Count")
# Around 20 number of accounts represent the median here i.e. maximum number of loans have 20 total accounts 

#Histogm showing the number of loans for each loan status for numebr of total accounts 
ggplot(loan_ds_clean, aes(total_acc, fill=loan_status)) + geom_histogram(binwidth = 1, na.rm = T) +
  theme(axis.text.x = element_text(angle = 60)) +
  labs(x="Total Account", y="Count", title="Total Account vs. Count", fill="Loan Status")
#This also shows the count of defaulted loans are follow the same pattern of the as population histogram

#Histogram showing the percent of total loans for loan status vs Total account
ggplot(loan_ds_clean, aes(total_acc, fill=loan_status)) + geom_histogram(binwidth = 1, na.rm = T, position = "fill") +
  theme(axis.text.x = element_text(angle = 60)) +
  labs(x="Total Account", y="Count", title="Total Account vs. Count", fill="Loan Status")
#Nothing can be concluded from this. It looks like the probability of defaulting increases but the 
#decline is very low to point out any thing here

#-------------------------------Total Payment-----------------------------------#
#The total amount paid by the borrower for the credit taken

#Histogram showing the number of loans vs total payment
ggplot(loan_ds_clean, aes(total_pymnt)) + geom_histogram(binwidth = 1000, na.rm = T) +
  theme(axis.text.x = element_text(angle = 60)) +
  labs(x="Total Payment", y="Count", title="Total Payment vs. Count")
# Around maximum number of loans have made a total payment of around 6k 

#Histogm showing the number of loans for each loan status for total payment
ggplot(loan_ds_clean, aes(total_pymnt, fill=loan_status)) + geom_histogram(binwidth = 1000, na.rm = T) +
  theme(axis.text.x = element_text(angle = 60)) +
  labs(x="Total Payment", y="Count", title="Total Payment vs. Count", fill="Loan Status")
#This also shows that if number of total payment is less the number of charged off loans are more

#Histogram showing the percent of total loans for loan status vs Total payment
ggplot(loan_ds_clean, aes(total_pymnt, fill=loan_status)) + geom_histogram(binwidth = 1000, na.rm = T, position = "fill") +
  theme(axis.text.x = element_text(angle = 60)) +
  labs(x="Total Payment", y="Count", title="Total Payment vs. Count", fill="Loan Status")
#Here clearly if the total payment made is less the charge off posibility is high
#but while looking for a current loan we wont be able to tell what is going to be the 
#future collection and the same applies for total principal received, total interest received and total late fee received.

#-------------------------------No. of Years since Credit Holder-----------------------------------#
# Credit Holder since No. of Years

#histogram showing numebr of loans for number of years of credit holding
ggplot(loan_ds_clean, aes(x = credit_holder_since)) + geom_histogram(binwidth = 1) +
  labs(x="Credit Holder since No. of Years", y="Count", title="Credit Holder since No. of Years  vs. Count")
#Around 12 or 13 years is years for which maximum numebr of users are holding credit

#histogram showing numebr of loans for number of years of credit holding
ggplot(loan_ds_clean, aes(x = credit_holder_since,  fill = loan_status)) + geom_histogram(position = "fill", binwidth = 1) +
  labs(x="Credit Holder since No. of Years", y="% of population", title="Credit Holder since No. of Years vs. Loan Status", fill="Loan Status")

#box plot showing the range of years for the used who are showing different loan status
ggplot(loan_ds_clean, aes(y = credit_holder_since, x = loan_status)) + geom_boxplot() +
  labs(x="Loan Status", y="Credit Holder since No. of Years", title="Credit Holder since No. of Years vs. Loan Status")
#charged off and fully paid acquire almost same range
#Nothing can be told about loan status defaulting here

