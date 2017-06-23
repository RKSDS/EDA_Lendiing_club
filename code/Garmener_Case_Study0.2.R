#import some important libraries to use on data
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)

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



sum(is.na(loan_ds$loan_amnt))
sum(loan_ds$loan_amnt==0) #logically this should never be 0 but just to check if data is all fine
#this will give the number of unique loan amounts requested from bank.
length(unique(loan_ds$loan_amnt))
#we need to see if the amount funded is defaulted or not hence removing this
remove_fields <- append(remove_fields, "loan_amnt")



sum(is.na(loan_ds$funded_amnt))
sum(loan_ds$funded_amnt==0) #logically this could be 0 but those will go under rejected cases.
#this will give the number of unique loan amounts commited by bank.
length(unique(loan_ds$funded_amnt))


# This is getting considered with funded amount
remove_fields <- append(remove_fields, "funded_amnt_inv")



#unique terms for loan
unique(loan_ds$term)
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
sum(loan_ds$grade %in% c("", " "))
#unique grades
unique(loan_ds$grade)
#convert it to factor as this is an ordered category
loan_ds$grade <- as.factor(loan_ds$grade)



#sanity check for grade
sum(is.na(loan_ds$sub_grade))
sum(loan_ds$sub_grade %in% c("", " "))
#unique grades
unique(loan_ds$sub_grade)
#Getting rid of this as grades covers the top level categorization
remove_fields <- append(remove_fields, "sub_grade")




#sanity check for Employer Title
sum(is.na(loan_ds$emp_title))
sum(loan_ds$emp_title %in% c("", " "))
#replacing all the entries with NA where data is empty
loan_ds$emp_title[loan_ds$emp_title %in% c("", " ")] <- NA   ###can we convert this field as factor after clean up? ## discussion required
#converting all the data into single format i.e. no punctuations or space and to lower case
loan_ds$emp_title <- tolower(gsub(" ", "", loan_ds$emp_title))
loan_ds$emp_title <- gsub("[[:punct:]]","",loan_ds$emp_title)
#unique Titles    ----- It should be unique Titles
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
#Replace all n/a with NA
#sapply(loan_ds$emp_length, function(x){
#  if (x=="n/a") {NA} 
#  else if (x=="< 1") {0} 
#  else if (x=="10+"){10} 
#  else {as.numeric(x)}})
#Replace all n/a with NA
loan_ds$emp_length[loan_ds$emp_length == "n/a"] <- NA #--- could be from individual person not working in company
#considering all the values mentioned as < 1 as 0
loan_ds$emp_length[loan_ds$emp_length == "< 1"] <- "0"
#considering all the values mentioned as 10+ as 10
loan_ds$emp_length[loan_ds$emp_length == "10+"] <- "10"
#check how many na values are there it should be same as n/a values
sum(is.na(loan_ds$emp_length))
#convert the values from string to numeric
loan_ds$emp_length <- as.numeric(loan_ds$emp_length)
#convert to factors
loan_ds$emp_length <- as.factor(loan_ds$emp_length)



#unique type of home ownerships
unique(loan_ds$home_ownership)
loan_ds$home_ownership <- as.factor(loan_ds$home_ownership)



#sanity check for annual income
sum(is.na(loan_ds$annual_inc))
sum(loan_ds$annual_inc==0)
#getting rid of outliers
loan_ds$annual_inc[loan_ds$annual_inc %in% boxplot.stats(loan_ds$annual_inc)$out] <- NA



#Sanity check for verification status
sum(is.na(loan_ds$verification_status))
sum(loan_ds$verification_status %in% c("", " "))
#unique entries for income source verification status
unique(loan_ds$verification_status)
#converting to factors
loan_ds$verification_status <- as.factor(loan_ds$verification_status)

#checking various the metrics across verification status
loan_ds %>% group_by(verification_status) %>% summarise(mean=mean(funded_amnt), 
                                                        median=median(funded_amnt), 
                                                        st_dev=sd(funded_amnt))



#Sanity check for issue date
sum(is.na(loan_ds$issue_d))
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



#Sanity check for loan status
sum(is.na(loan_ds$loan_status))
sum(loan_ds$loan_status %in% c("", " "))
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
sum(is.na(loan_ds$purpose))
sum(loan_ds$purpose %in% c("", " "))
unique(loan_ds$purpose)
#converting to factor
loan_ds$purpose <- as.factor(loan_ds$purpose)



#Sanity check for loan title
sum(is.na(loan_ds$title))
sum(loan_ds$title %in% c("", " "))
length(unique(loan_ds$title)) # may not be of much use.. Individual way of naming the loan.. hence removing
remove_fields <- append(remove_fields, "title")


#Sanity check for area zip code
sum(is.na(loan_ds$zip_code))
sum(loan_ds$zip_code %in% c("", " "))
length(unique(loan_ds$zip_code))
#this cannot be used alone. But we can use it in conjunction with state. 
remove_fields <- append(remove_fields, "zip_code")



#Sanity check for addr_state
sum(is.na(loan_ds$addr_state))
sum(loan_ds$addr_state %in% c("", " "))
length(unique(loan_ds$addr_state))
#convert to factor
loan_ds$addr_state <- as.factor(loan_ds$addr_state)



#Sanity check for dti
#less value mean good possibility the payment will be done by borrower in time.
sum(is.na(loan_ds$dti))
sum(loan_ds$dti==0)



#no. of unique delequency events  for past 2 years of loan account
#the high the bad
unique(loan_ds$delinq_2yrs)
sum(loan_ds$delinq_2yrs==0)
#convert to factor
loan_ds$delinq_2yrs <- as.factor(loan_ds$delinq_2yrs)



##this is the date on which first account opened for the user
#Sanity check for issue date
sum(is.na(loan_ds$earliest_cr_line))
sum(loan_ds$earliest_cr_line %in% c("", " "))
unique(loan_ds$earliest_cr_line)
#separate the year and month
loan_ds <- separate(loan_ds, earliest_cr_line, into = c("crdit_ln_month", "credit_ln_year"), sep = "-")
remove_fields <- append(remove_fields, "crdit_ln_month")
loan_ds$credit_ln_year <- sapply(loan_ds$credit_ln_year, function(x) {if(as.numeric(x)>17) {paste("19",x, sep="")} else{paste("20",x,sep="")}})
loan_ds$credit_ln_year <- as.numeric(loan_ds$credit_ln_year)
sum(is.na(loan_ds$credit_ln_year))
#convert to date and analyse when the loans are getting release most
#if time permits will include in study



#sanity check
unique(loan_ds$inq_last_6mths)
#All are valid values, no NA or blank value reported 
#Converting to factor
loan_ds$inq_last_6mths <- as.factor(loan_ds$inq_last_6mths)



#sanity check
unique(loan_ds$mths_since_last_delinq) # this is the factor which has high gravity in deciding the defaulters
sum(is.na(loan_ds$mths_since_last_delinq)) 
#convert to factors
loan_ds$mths_since_last_delinq <- as.factor(loan_ds$mths_since_last_delinq)
# If there is no information on delinquency we assume the borrower is
#paying the installments properly or the loan has been paid completely.
#also we have to keep in mind that the higher value means user has not delenquent in that much days.
#If time permits will be including in study



#sanity check
sum(is.na(loan_ds$mths_since_last_record))
unique(loan_ds$mths_since_last_record)
loan_ds$mths_since_last_record <- as.factor(loan_ds$mths_since_last_record)
#mostly the values are NA almost no use of column
remove_fields <- append(remove_fields, "mths_since_last_record")



#total number of OPEN accounts (operational)
unique(loan_ds$open_acc)
#convert to factors
loan_ds$open_acc <- as.factor(loan_ds$open_acc)



#sanity check
unique(loan_ds$pub_rec)
sum(loan_ds$pub_rec==0)
#most of the entries are 0
#convert to factor
loan_ds$pub_rec <- as.factor(loan_ds$pub_rec)



#installment - payment for particular month
#more mining is required here to give insight of any kind
sum(is.na(loan_ds$revol_bal))
sum(loan_ds$revol_bal==0)
#removing as revol_util is better column to study utilization
remove_fields <- append(remove_fields, "total_acc")



#sanity check
#here the if the credit used is less then the chances of making payment is more
#but there is not too much profit to bank.
#bank need to make sure the utilization happens and the payments is also done for it.
sum(is.na(loan_ds$revol_util))
sum(loan_ds$revol_util=="")
sum(loan_ds$revol_util=="0")
#as we do see 0% untilization we are not assuming blank to represent 0%
#they will be represented by NAs
loan_ds$revol_util <- as.numeric(gsub("%","",loan_ds$revol_util))
sum(is.na(loan_ds$revol_util))



#total number of accounts associated with the memeber. (open+closed)
#the high the closed the better
sum(is.na(loan_ds$total_acc))
sum(loan_ds$total_acc==0) #should not be 0 and that is the case
unique(loan_ds$total_acc)
remove_fields <- append(remove_fields, "total_acc")


#the less the better
sum(is.na(loan_ds$out_prncp))
sum(loan_ds$out_prncp==0)
#getting rid of this as there is very limited insight for current scenario
remove_fields <- append(remove_fields, "out_prncp")


#The less the better
#This has almost no use as this portion is already covered in out_prncp
#the difference in out_prncp_inv is what bank will keep.
sum(loan_ds$out_prncp_inv==0)
max(loan_ds$out_prncp_inv)
#its more useful for individual analysis hence removing from here
remove_fields <- append(remove_fields, "out_prncp_inv")



#payments received to date for total funded amount
sum(is.na(loan_ds$total_pymnt))
sum(loan_ds$total_pymnt==0) #not good or recently loan started
#this is also not useful column for current analysis
remove_fields <- append(remove_fields, "total_pymnt")


#total_funded - total_payment low = better

#investors portion received till date.
#total funded by investor - total_pymt_inv low=better
sum(is.na(loan_ds$total_pymnt_inv))
sum(loan_ds$total_pymnt_inv==0)
remove_fields <- append(remove_fields, "total_pymnt_inv")


#find unique value for the column where it is not empty
unique(loan_ds$next_pymnt_d[(loan_ds$next_pymnt_d!="")])
remove_fields <- append(remove_fields, "next_pymnt_d")
#might not be useful as there are only two unique dates for the column and many are empty

loan_ds$last_pymnt_d
#this is not useful column as it could be representing a closed cr line
remove_fields <- append(remove_fields, "last_pymnt_d")



#Summary for pub_rec_bankruptcies (Number of public record bankruptcies)
sum(is.na(loan_ds$pub_rec_bankruptcies))  #No of NA in the column
sum(loan_ds$pub_rec_bankruptcies %in% c("", " "))  #No of Blanks in the Column
unique(loan_ds$pub_rec_bankruptcies) # Listing Unique values
loan_ds$pub_rec_bankruptcies <- as.factor(loan_ds$pub_rec_bankruptcies) # converting column as factor



#Summary for last_credit_pull_d
sum(is.na(loan_ds$last_credit_pull_d))
sum(loan_ds$last_credit_pull_d %in% c("", " "))
str(loan_ds$last_credit_pull_d)
#this cannot be used as it might give very old date or date for closed loans



#Summary for next_pymnt_d (Next scheduled payment date)
sum(is.na(loan_ds$next_pymnt_d))
sum(loan_ds$next_pymnt_d %in% c("", " ")) #38577
#As lot of fields are Blank hence ignoring the field for analysis
remove_fields <- append(remove_fields, "next_pymnt_d")


#Summary for last_pymnt_amnt (Last total payment amount received)
sum(is.na(loan_ds$last_pymnt_amnt))
sum(loan_ds$last_pymnt_amnt %in% c("", " "))
sum(loan_ds$last_pymnt_amnt==0) #74
# this field can be used to calculate profit Loss.
#this doesn't give historical insight of customer behaviour
remove_fields <- append(remove_fields, "last_pymnt_amnt")

   

#Summary for last_pymnt_d (Last month payment was received)
sum(is.na(loan_ds$last_pymnt_d))
sum(loan_ds$last_pymnt_d %in% c("", " "))
loan_ds$last_pymnt_d <- tolower(gsub(" ", "", loan_ds$last_pymnt_d))
#this doesn't give historical insight of customer behaviour
remove_fields <- append(remove_fields, "last_pymnt_d")


#Summary for collection_recovery_fee
sum(is.na(loan_ds$collection_recovery_fee))
sum(loan_ds$collection_recovery_fee %in% c("", " "))
sum(loan_ds$collection_recovery_fee==0)#35935
# From Business point of view this comes after the user starts defaulting/becomes delinquent, 
#hence not keeping in study
remove_fields <- append(remove_fields, "collection_recovery_fee")


#Summary for recoveries (Recovery after charge off)
sum(is.na(loan_ds$recoveries))
sum(loan_ds$recoveries %in% c("", " "))
sum(loan_ds$recoveries==0)#35499
# This also has higher no. of values with zero also this is after default.
#hence not keeping in study
remove_fields <- append(remove_fields, "recoveries")


#Summary for total_rec_late_fee (Late fees received to date)
sum(is.na(loan_ds$total_rec_late_fee))
sum(loan_ds$total_rec_late_fee %in% c("", " "))
sum(loan_ds$total_rec_late_fee==0)#37671
# This can be analysed as people with zero late fee tend not to deffault and repay in time.
# But this comes after Loan is given hence removing this.
remove_fields <- append(remove_fields, "total_rec_late_fee")



#Summary for total_rec_int (Interest received to date)
sum(is.na(loan_ds$total_rec_int))
sum(loan_ds$total_rec_int %in% c("", " "))
sum(loan_ds$total_rec_int==0)#71
#This is not going to tell historical behaviour of borrower
#But this comes after Loan is given hence removing it
remove_fields <- append(remove_fields, "total_rec_int")



#Summary for total_rec_prncp (Principal received to date)
sum(is.na(loan_ds$total_rec_prncp))
sum(loan_ds$total_rec_prncp %in% c("", " "))
sum(loan_ds$total_rec_prncp==0)#74
# This comes after Loan is given and also
#this is not going to tell historical behaviour of borrower
remove_fields <- append(remove_fields, "total_rec_prncp")

credit_holder_since <- issue_year - loan_ds$credit_ln_year
loan_ds <- cbind(loan_ds, credit_holder_since)


#remove_fields
loan_ds_unwanted_fields <- loan_ds[ , (names(loan_ds) %in% remove_fields)]
loan_ds_clean <- loan_ds[ , !(names(loan_ds) %in% remove_fields)]

summary(loan_ds_clean)

write.csv(loan_ds_clean, "./Data/cleaned_data.csv")

