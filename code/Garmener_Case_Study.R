#import some important libraries to use on data
library(dplyr)
library(tidyr)
library(ggplot2)

#import data from the csv file
loan_ds <- read.csv("loan.csv", stringsAsFactors = F)
#structure of the data frame..
str(loan_ds)
#View the data frame
View(loan_ds)

#summary of initial data frame
summary(loan_ds)

#store all column names as a vector
col_names <- colnames(loan_ds)

colSums(is.na(loan_ds))

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
#minimum amount asked
min(loan_ds$loan_amnt) #500
#minimum amount asked
max(loan_ds$loan_amnt) #35000


sum(is.na(loan_ds$funded_amnt))
sum(loan_ds$funded_amnt==0) #logically this could be 0 but those will go under rejected cases.
#this will give the number of unique loan amounts commited by bank.
length(unique(loan_ds$funded_amnt))
#minimum amount commited
min(loan_ds$funded_amnt) #500
#minimum amount commited
max(loan_ds$funded_amnt) #35000

#find difference in loan asked and commited for each request
diff_loan_amt_req_funded <- loan_ds$loan_amnt - loan_ds$funded_amnt
#In total there are 1849 cases where the loan amount committed is not as requested amount
sum(diff_loan_amt_req_funded!=0)
loan_ds <- cbind(loan_ds, diff_loan_amt_req_funded)

sum(is.na(loan_ds$funded_amnt_inv))
sum(loan_ds$funded_amnt_inv==0) # number of cases where the invertors have not invested
#minimum amount funded
min(loan_ds$funded_amnt_inv) #0
#minimum amount funded
max(loan_ds$funded_amnt_inv) #35000

#find difference in loan commited and invested for each request
diff_loan_amt_funded_inv <- loan_ds$funded_amnt - loan_ds$funded_amnt_inv
#In total there are 1849 cases where the loan amount invested is not as committed
sum(diff_loan_amt_funded_inv!=0)
loan_ds <- cbind(loan_ds, diff_loan_amt_funded_inv)


#unique terms for loan
unique(loan_ds$term)
#bringing the duration to numbers
loan_ds$term <- as.integer(gsub("months", "", loan_ds$term))
loan_ds$term <- as.factor(loan_ds$term)


#check the interest rate column
sum(is.na(loan_ds$int_rate))
sum(loan_ds$funded_amnt_inv %in% c("", " "))
#convert string to number
loan_ds$int_rate <- as.numeric(gsub("%", "", loan_ds$int_rate))
#check if convertion went fine for all
length(unique(loan_ds$int_rate))
sum(is.na(loan_ds$int_rate))
sum(loan_ds$int_rate==0)


#sanity check for installment
sum(is.na(loan_ds$installment))
sum(is.na(loan_ds$installment==0))
length(unique(loan_ds$installment))


#sanity check for grade
sum(is.na(loan_ds$grade))
sum(loan_ds$grade %in% c("", " "))
#unique grades
unique(loan_ds$grade).
#convert it to factor as this is an ordered category
loan_ds$grade <- as.factor(loan_ds$grade)

#sanity check for grade
sum(is.na(loan_ds$sub_grade))
sum(loan_ds$sub_grade %in% c("", " "))
#unique grades
unique(loan_ds$sub_grade)
#convert it to factor as this is an ordered category
loan_ds$sub_grade <- as.factor(loan_ds$sub_grade)


#sanity check for Employer name
sum(is.na(loan_ds$emp_title))
sum(loan_ds$emp_title %in% c("", " "))
#replacing all the entries with NA where data is empty
loan_ds$emp_title[loan_ds$emp_title %in% c("", " ")] <- NA
#converting all the data into single format i.e. no punctuations or space and to lower case
loan_ds$emp_title <- tolower(gsub(" ", "", loan_ds$emp_title))
loan_ds$emp_title <- gsub("[[:punct:]]","",loan_ds$emp_title)
#unique grades
length(unique(loan_ds$emp_title)) #26446
#wont be of much use as employer directly do not make a person defaulter
remove_fields <- append(remove_fields, "emp_title")

#find Unique entries for employement duration
unique(loan_ds$emp_length)
#from above statement it is clear that n/a should be removed.
#as an assumption we could make it 0???????????
loan_ds$emp_length[loan_ds$emp_length == "n/a"] <- NA
loan_ds$emp_length[loan_ds$emp_length == "< 1 year"] <- "0"
loan_ds$emp_length[loan_ds$emp_length == "10+ years"] <- "10"
loan_ds$emp_length <- as.numeric(gsub(" years|year", "", loan_ds$emp_length))
unique(loan_ds$emp_length)
loan_ds$emp_length <- as.factor(loan_ds$emp_length)

#unique type of home ownerships
unique(loan_ds$home_ownership)
loan_ds$home_ownership <- as.factor(loan_ds$home_ownership)


#sanity check for annual income
sum(is.na(loan_ds$annual_inc))
sum(loan_ds$annual_inc==0)
#max of annual income
max(loan_ds$annual_inc)
#min of annual income
min(loan_ds$annual_inc)


#Sanity check for verification status
sum(is.na(loan_ds$verification_status))
sum(loan_ds$verification_status %in% c("", " "))
#unique entries for income source verification status
unique(loan_ds$verification_status)
loan_ds$verification_status <- as.factor(loan_ds$verification_status)

#Sanity check for verification status
sum(is.na(loan_ds$issue_d))
sum(loan_ds$issue_d %in% c("", " "))
unique(loan_ds$issue_d)
#separate the year and month
loan_ds <- separate(loan_ds,issue_d, into=c("issued_month","issued_year"), sep="-")
#factoring the month with order to months
loan_ds$issued_month <- factor(loan_ds$issued_month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), ordered=T)
#factor years
loan_ds$issued_year <- as.factor(as.numeric(paste("20",sep="",loan_ds$issued_year)))


#Sanity check for verification status
sum(is.na(loan_ds$loan_status))
sum(loan_ds$loan_status %in% c("", " "))
unique(loan_ds$loan_status)
loan_ds$loan_status <- as.factor(loan_ds$loan_status)


# dont have any use as this is as unique as id and doesn't make sense in analysis
remove_fields <- append(remove_fields, "url")

# description doesn't make much sense here again.. this is personalised message from applicant.
# it can be used by minning this to understand the underling sentiment/cause of request. But we are not executing it here
remove_fields <- append(remove_fields, "desc")


#Sanity check for verification status
sum(is.na(loan_ds$purpose))
sum(loan_ds$purpose %in% c("", " "))
unique(loan_ds$purpose)
loan_ds$purpose <- as.factor(loan_ds$purpose)

#Sanity check for verification status
sum(is.na(loan_ds$title))
sum(loan_ds$title %in% c("", " "))
length(unique(loan_ds$title)) # may not be of much use.. Individual way of naming the loan
remove_fields <- append(remove_fields, "title")


#Sanity check for verification status
sum(is.na(loan_ds$zip_code))
sum(loan_ds$zip_code %in% c("", " "))
length(unique(loan_ds$zip_code))
#this cannot be used alone. But we can use it in conjustion with state.


#Sanity check for addr_state
sum(is.na(loan_ds$addr_state))
sum(loan_ds$addr_state %in% c("", " "))
length(unique(loan_ds$addr_state))
loan_ds$addr_state <- as.factor(loan_ds$addr_state)


#Sanity check for addr_state
sum(is.na(loan_ds$dti))
sum(loan_ds$dti==0)
min(loan_ds$dti)
max(loan_ds$dti)


#no. of unique delequency events  for past 2 years of loan account
unique(loan_ds$delinq_2yrs)
loan_ds$delinq_2yrs <- as.factor(loan_ds$delinq_2yrs)


#this is the date on which first account opened for the user
loan_ds <- separate(loan_ds,earliest_cr_line, into=c("first_acc_open_month","first_acc_open_year"), sep="-")
#factoring the month with order to months
loan_ds$first_acc_open_month <- factor(loan_ds$first_acc_open_month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), ordered=T)

#Function to conver yy year format to YYYY format and numeric
openedyear <- function(year) {
  if(as.numeric(year) < 17)
  {
    return(as.numeric(paste("20",year,sep = "")))
  }
  else
  {
    return(as.numeric(paste("19",year,sep="")))
  }
}

loan_ds$first_acc_open_year <- sapply(loan_ds$first_acc_open_year, FUN=function(x) openedyear(x))
#factor years
loan_ds$first_acc_open_year <- as.factor(loan_ds$first_acc_open_year)


unique(loan_ds$inq_last_6mths)
loan_ds$inq_last_6mths <- as.factor(loan_ds$inq_last_6mths)
# this is not going to add any value to analysis until and unless we assume 
# The more the enquiry the more the chances the borrower knows abt the payments he/she need to do.



sum(is.na(loan_ds$mths_since_last_delinq)) # If there is no information on delinquency we assume the borrower is
#paying the installments properly or the loan has been paid completely.
unique(loan_ds$mths_since_last_delinq) # this is the factor which has high gravity in deciding the defaulters
loan_ds$mths_since_last_delinq <- as.factor(loan_ds$mths_since_last_delinq)


sum(is.na(loan_ds$mths_since_last_record))
unique(loan_ds$mths_since_last_record)
loan_ds$mths_since_last_record <- as.factor(loan_ds$mths_since_last_record)
#mostly the values are NA almost no use of column

#total number of OPEN accounts (operational)
sum(is.na(loan_ds$open_acc))
sum(loan_ds$open_acc==0)
unique(loan_ds$open_acc)

sum(is.na(loan_ds$pub_rec))
sum(loan_ds$pub_rec!=0)
unique(loan_ds$pub_rec) #useful column in identifying risky borrowers
loan_ds$pub_rec <- as.factor(loan_ds$pub_rec)


#this is if low then the borrower is less risky
sum(is.na(loan_ds$revol_bal))
max(loan_ds$revol_bal)
min(loan_ds$revol_bal)
sum(loan_ds$revol_bal==0)

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
#the high the closed the better?????????
sum(is.na(loan_ds$total_acc))
sum(loan_ds$total_acc==0) #should not be 0 and that is the case
unique(loan_ds$total_acc)


#the less the better
sum(is.na(loan_ds$out_prncp))
sum(loan_ds$out_prncp==0)
max(loan_ds$out_prncp)

#The less the better
#This has almost no use as this portion is already covered in out_prncp
#the difference in out_prncp_inv is what bank will keep.
sum(loan_ds$out_prncp_inv==0)
max(loan_ds$out_prncp_inv)
remove_fields <- append(remove_fields, out_prncp_inv)

#payments received to date for total funded amount
sum(is.na(loan_ds$total_pymnt))
sum(loan_ds$total_pymnt==0) #not good or recently loan started


#total_funded - total_payment low = better

#investors portion received till date.
#total funded by investor - total_pymt_inv low=better
sum(is.na(loan_ds$total_pymnt_inv))
sum(loan_ds$total_pymnt_inv==0)

#the more the better
sum(is.na(loan_ds$total_rec_prncp))
sum(loan_ds$total_rec_prncp==0)

#the more the better
sum(is.na(loan_ds$total_rec_int))
sum(loan_ds$total_rec_int==0)


#find unique value for the column where it is not empty
unique(loan_ds$next_pymnt_d[(loan_ds$next_pymnt_d!="")])
#might not be useful as there are only two unique dates for the column and many are empty

loan_ds$last_pymnt_d #this along with credit pull will give the number of months of deliquency.. which should be same with 
#mths_since_last_delinq

loan_ds$last_credit_pull_d





remove_fields
loan_ds_unwanted_fields <- loan_ds[ , (names(loan_ds) %in% remove_fields)]
loan_ds_clean <- loan_ds[ , !(names(loan_ds) %in% remove_fields)]

summary(loan_ds_clean)



