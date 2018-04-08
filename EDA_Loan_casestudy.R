##***************************************************************************##
##                                                                           ##
## Project        : A Case Study on Lending Club Loans                       ##
## Objective      : To minimise the risk of losing money while lending to    ##
##                  customers by identifying risky loan applicants using     ##
##                  Exploratory Data Analysis                                ##
## Date           : 1-Apr-2018                                               ##
## Version        : 2.0                                                      ##
## Author(s)      : Chaithanya Reddy, Poovarasan Murugesan                   ##
##                  Pranesh Ramaiya, Jayakumar Sekar                         ##
##                                                                           ##
##***************************************************************************##

# Check and Import required libraries
options(warn = -1)
libs = c("tidyverse", "lubridate", "formattable","corrplot", "cowplot", 
         "data.table")
install.lib <- libs[!libs %in% installed.packages()]
for (pkg in install.lib) 
  install.packages(pkg, dependencies = T)
loadlib     <- lapply(libs, library, character.only = T) # load them
remove(list = ls())
options(warn = 0)


# Import input file
loan <- loan_master <- read_csv("loan.csv")


# Looking at the data
dim(loan)                                    # 39717 obs. of  111 variables
str(loan)
glimpse(loan)
summary(loan)
# Checking for duplicates
length(unique(loan$id)) != dim(loan)[1]
# 39717 IDs matches with 39717 total observations. So no duplicates
length(unique(loan$member_id)) != dim(loan)[1]
# 39717 matches. No duplicates. No borrower got 2 different loans. 

# Note : We are interested in variables collected during the loan application
# process. All the other variables that are  collected after the loan issued, 
# is  not of much interest  since our focus is to  identify the risky  loan 
# applicants during the application process based on the information they 
# provide as well as on the variables we derive from them.

##***************************************************************************##
#                                                                             #
#                             Data Cleaning                                   #
#                                                                             #
##***************************************************************************##
# Issues identified
#  * NA values
#  * Unnecessary columns (Columns that are of not much use for Analysis)
#  * Dates as strings
#  * Loan term as string
#  * Percentages as strings
##***************************************************************************##


# Issue 1: NA values

colSums(is.na(loan))

# There are quite a large number of NAs in columns. Identify and remove columns
# where the entire values are NA (i.e 39717 NAs).

table(colSums(is.na(loan))) 

# 54 columns - All NAs, 1 column - 38577 NA
# 1 column  - 36931 NAs, 1 column - 25682 NA

loan <- loan %>% select(-which(colMeans(is.na(loan)) > 0.5))


# Issue 2: Unnecessary columns with not much weightage for analysis
# id                      - Loan Id :not required for analysis
# member id               - Member Id : not required for analysis
# emp_title               - Can be used for text analysis to identify 
#                         - Not required for analysis 
# desc                    - Can be used for text analysis to identify
#                           patterns. But for now we will ignore.
# pymnt_plan              - only 1 constant 'n' value
# url                     - Not required
# zip_code                - Masked ZIP code. Not required.
# Initial_list_status     - only 1 constant 'f' value
# out_prncp,out_prncp_inv - Not required
# cols 47 thru 52 and 54  - Either single constant value or Huge NA values
# check using table command

sapply(loan[, c(47:52, 54)], table)

# Remove all the above identified columns

loan <- loan %>%
  select(-c(id, member_id, funded_amnt, funded_amnt_inv, emp_title, pymnt_plan,
            url, desc, title, zip_code, initial_list_status, out_prncp_inv,
            total_rec_int, total_rec_late_fee, collection_recovery_fee,
            out_prncp, total_pymnt, total_pymnt_inv, total_rec_prncp,
            recoveries, last_pymnt_d, last_pymnt_amnt, policy_code,            
            last_credit_pull_d, collections_12_mths_ex_med, 
            application_type, acc_now_delinq, tax_liens, 
            chargeoff_within_12_mths, delinq_amnt,
            pub_rec_bankruptcies))

# Again check for NA values 
colSums(is.na(loan))

# revol_util column has 50 NAs (<0.1 %). Replace them with 0s. 
loan$revol_util[is.na(loan$revol_util)] <- 0

# Check for complete records 
sum(complete.cases(loan))               
# 39717 matches with 39717 observations. All NAs have been taken care.


# Issue 3: Dates as strings
# Coerce date time character columns into standard Date time objects
# Date columns - issue_d, earliest_cr_line

loan$issue_d          <-
  parse_date_time(loan$issue_d, orders = c("my", "ym"))

# Use parse_date_time2 with cutoff_2000 parameter to handle dates 
# with year less than 1968. Cutoff set to 45 - Years less than 
# 45 will be prefixed with century 20 and greater than 45 with century 19. 

loan$earliest_cr_line <-
  parse_date_time2(loan$earliest_cr_line, orders = c("my"), cutoff_2000 = 45L)


# Issue 4: Loan term as string - Extract numbers from the term column 
# Convert to character since this would be traeted as a factor

loan$term       <-  as.character(str_extract(loan$term, "[[:digit:]]+"))


# Issue 5:  Percentages as strings - Convert % columns to numeric

loan$int_rate   <-  as.numeric(str_extract(loan$int_rate, "\\d+\\.*\\d*"))
loan$revol_util <-  as.numeric(str_extract(loan$revol_util, "\\d+\\.*\\d*"))


# Issue 6:  Loan term as strings - emp_length 10+ years = 10 , <1 year = 0 

summary(factor(loan$emp_length))

# Looking at the trend, proportion of loans decrease as the employment length 
# decreases. Going by the trend, the n/a values (2%) seems to be fitting into 
# the employment length of 1 years bucket. 
loan$emp_length <- str_replace_all(loan$emp_length, "n/a", "1")
loan$emp_length <- str_replace_all(loan$emp_length, "\\< 1", "0")
loan$emp_length <- as.numeric(str_extract(loan$emp_length,  "[[:digit:]]+"))
summary(factor(loan$emp_length))


# Issue 7:  Outliers
# Outliers in continuos varibales will be handled during Univariate analysis 
# of each variable

# We will remove all the  current ongoing records as we are interested in only 
# fully paid and charged off records to identify pattern in risky applicants. 
loan <-  loan[loan$loan_status != "Current",]


# Converting to factors
# grade, sub_grade, home_ownership, verification_status, loan_status , 
# addr_state, purpose
loan <- loan %>% mutate_if(is.character,as.factor)

# Assign Levels to factors

setattr(loan$grade,"levels",c(LETTERS[1:7]))
setattr(loan$loan_status,"levels", c("Charged Off" ,"Fully Paid"))

str(loan)   # 38577 obs. of  23 variables

##***************************************************************************##
#                                                                             #
#                         Derive New variables                                #
#                                                                             #
##***************************************************************************##

# Business Driven metric
# Age of credit line as of loan issue date
loan$credit_line_age <- year(loan$issue_d) - year(loan$earliest_cr_line)

# Monthly installment to Monthly Income ratio
loan$iti <-  round((loan$installment / (loan$annual_inc / 12)) * 100, 2)

# Data Driven metric
loan$issue_year <-  year(loan$issue_d)

str(loan) # 38577 obs. of  26 variables

##***************************************************************************##
#                                                                             #
#                               Metadata                                      #
#                                                                             #
##***************************************************************************##

# Customer Demographic Variables - 
# home_ownership , annual_inc, verification_status, addr_state,  

# Loan variables - 
# loan amount, interest rate, loan status, loan grade, loan sub-grade,
# dti, iti, issue date, term, installment, purpose

# Customer's other financial variables - 
# dti, iti, credit_line_age, total_acc, revol_util, revol_bal,pub_rec
# inq_last_6mths, earliest_cr_line, delinq_2yrs

##***************************************************************************##
#                                                                             #
#                            Common Functions                                 #
#                                                                             #
##***************************************************************************##

# Setting the theme of plots
plot_theme <- theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14,face = 'bold'),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12))

# Univariate plots Categorical

CatUnivar <- function(feature, xlabel, ylabel = "Proportion") {
  print(percent(prop.table(table(feature))))
  loan %>% count(var = feature) %>% 
    ungroup() %>%
    mutate(pct = percent(prop.table(n), 2)) %>%
    ggplot(aes(x = var, y = pct)) +
    geom_bar(stat = 'identity', fill = "#F8766D") + 
    geom_text(aes(y = pct,  
                  label = pct),    
              vjust = -0.5, 
              size = 3) +
    labs(x = xlabel, y = ylabel, title = paste(ylabel, "by", xlabel)) +
    plot_theme
}


# Bivariate plots 
CatBivar <-  function(yvar, xvar, xlabel, ylabel) {
  as.data.frame(percent(prop.table(table(yvar, xvar), 2))) %>%
    ggplot(aes(x = xvar, y = Freq,  fill = yvar)) +
    geom_col( position = "fill" ) +
    geom_text(aes(label = Freq),
              position = position_fill(vjust = .5),  
              size = 2.5) +
    labs(x = xlabel, y = "Proportion", 
         title = paste(ylabel,"Proportion vs", xlabel)) +
    plot_theme
}


# Continuous Univariate plots 
ContUnivar <- function(yfeature, ylabel) {
    ggplot(loan, aes(x = "", y = yfeature)) +
    geom_boxplot(fill = "#F8766D", outlier.colour = "red", outlier.shape = 1) +
    stat_boxplot(geom = "errorbar", width = 0.5) +
    labs( y = ylabel, title = paste(ylabel, "Distribution")) +
    plot_theme
}


ContCatBivar <- function(xfeature, yfeature, xlabel, ylabel) {
  ggplot(loan, aes(x = xfeature, y = yfeature, fill = xfeature)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 1, show.legend = F) + 
    stat_boxplot(geom = "errorbar", width = 0.5) +
    labs(x = xlabel, y = ylabel, title = paste(ylabel, "over", xlabel)) +
    plot_theme
}

HistPlot <- function(feature, xlabel, bwidth){
  ggplot(loan, aes(x = feature, fill = loan_status)) +
    geom_histogram(binwidth = bwidth, position = "fill") + 
    labs(x = xlabel)
}

##***************************************************************************##
#                                                                             #
#                         Univariate Analysis                                 #
#                       (Categorical Variables)                               #
#                                                                             #
##***************************************************************************##

p1 <- CatUnivar(loan$term, "Term") 
p2 <- CatUnivar(loan$grade, "Grade")
p3 <- CatUnivar(loan$home_ownership, "Home Ownership")
p4 <- CatUnivar(loan$verification_status, "Verification Status") 
p5 <- CatUnivar(loan$loan_status, "Loan Status")
p6 <- CatUnivar(loan$issue_year, "Issue year") 

plot_grid(p1, p2, p3, p4, p5, p6)

# Loan term
# 36 months  60 months
#   75%        25% 

# Grade 
# Grade B seems to have highest loans followed by A the number decrease 
# with Grade G having least loans.

# Home Ownership
# MORTGAGE   NONE    OTHER      OWN      RENT 
#  44.12%   0.01%    0.25%     7.71%     47.9% 
# Majority (~92%) of the borrowers are either in Mortgaged or Rented houses

# Verification Status
# Not Verified   Source Verified    Verified 
# 43%               25%             32% 

# Loan Status
# Charged Off  Fully Paid 
#  15%         85%

# Year of issue
# There is a considerable raise in the number of loans from 2007 through 2011

p1 <- CatUnivar(loan$sub_grade, "Sub Grade")
p2 <- CatUnivar(factor(loan$emp_length), "Employment Experience")

plot_grid(p1,p2, align = 'v', ncol = 1)

# Sub grade
# Gives a similar picture like the grade but at a granular level. 
# Looking at a more granular level, within grades the proportion varies by
# levels. In A & B grade the proportion increases (from 1 to 5) as the level 
# increases whereas in most of the other grades the proportion decreases 
# as the level increases

# Employment Experience
# People with less years of employment experience are the ones who have 
# requested for more loans 
# Note : The value is more in 10 years since the experience spread is more as
# it includes all 10+ years records.

p1 <- CatUnivar(loan$purpose, "Loan Purpose") +  coord_flip()
# Top 5 purposes for which loans were applied
# 1. debt_consolidation  - 46.80%        
# 2. credit_card  - 13.03%    
# 3. other - 10.02% 
# 4. home_improvement - 7.45%
# 5. major_purchase - 5.57%

p2 <- CatUnivar(loan$addr_state, "State")
# Most borrowers are from state of California (18%) followed by Newyork (10%)
# followed by Florida(7%). Together they constitue 35% of the loans.


##***************************************************************************##
#                                                                             #
#                         Univariate Analysis                                 #
#                        (Continuos Variables)                                #
#                                                                             #
##***************************************************************************##

summary(loan)

p1 <- ContUnivar(loan$loan_amnt, "Loan Amount" )
p2 <- ContUnivar(loan$int_rate, "Interest Rate") 
p3 <- ContUnivar(loan$dti, "Debt to income ratio")  
p4 <- ContUnivar(loan$credit_line_age, "Credit Line Age") 
p5 <- ContUnivar(loan$annual_inc, "Anuual income") 
p6 <- ContUnivar(loan$iti, "Installment to income ratio") 
p7 <- ContUnivar(loan$revol_bal, "Revolving balance")
p8 <- ContUnivar(loan$revol_util, "Revolving utilization rate")

plot_grid(p1, p2, p3, p4, p5, p6, p7 ,p8, ncol = 4)

# Loan amount:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 500    5300    9600   11047   15000   35000 
# Loan amounts range from $500 to $35000 with an average of $9600
# Outliers greater than $30000 (only 1% records) are capped to $30000 
loan[which(loan$loan_amnt > 30000),]$loan_amnt <-  30000

# Interest rate:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.00    8.00   11.00   11.36   14.00   24.00 
# Interest rates range from 5% to 24% with and average of 11% 
# Outliers greater than 23 (0.5% records) are capped to 23
loan[which(loan$int_rate > 22),]$int_rate <-  22

# Debt-to-Income ratio:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.00    8.13   13.37   13.27   18.56   29.99 
# Lending Club borrowers has dti ranging from 0 to 30 
# with an average of 13. 

# Credit line age (derived metric) :
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.0     9.0    13.0    13.7    17.0    51.0 
# Lending Club borrowers has  credit history ranging from 3 to 51 years
# with an average of 13 years 
# Outliers greater than 29 (only 2% records) are capped at 29
loan[which(loan$credit_line_age > 29),]$credit_line_age <-  29


# Annual income : 
# Considering only incomes < 100000 to ignore the outliers
# Min.   1st Qu.  Median    Mean   3rd Qu.    Max. 
# 4000   40000    58868   68778   82000    6000000 
# Average of $58k of annual personal income
# Outliers greater than 150K (only 8% records) are capped at 150k. 
# Should not be impacting the analysis on the target variable since we are 
# just capping
loan[which(loan$annual_inc > 150000),]$annual_inc <-  150000


# Installment to income(monthly) ratio (derived metric): 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.080   3.500   5.770   6.563   8.810  32.030
# Lending Club borrowers has iti ranging from 0 to 32
# with an average of 6. 
# Outliers in iti > 17 capped to 17%. 
loan[which(loan$iti > 17),]$iti <-  17

# Revolving utilization balance
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0    3650    8762   13289   16912  149588 
# Outliers in revol_bal > 40000 capped to 40000. 
loan[which(loan$revol_bal > 40000),]$revol_bal <-  40000

# Revolving utilization rate
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   25.10   49.00   48.64   72.20   99.90 

# Lets check the variables by plotting them again 
p1 <- ContUnivar(loan$loan_amnt, "Loan Amount" )
p2 <- ContUnivar(loan$int_rate, "Interest Rate") 
p3 <- ContUnivar(loan$dti, "Debt to income ratio")  
p4 <- ContUnivar(loan$credit_line_age, "Credit Line Age") 
p5 <- ContUnivar(loan$annual_inc, "Annual income") 
p6 <- ContUnivar(loan$iti, "Installment to income ratio")
p7 <- ContUnivar(loan$revol_bal, "Revolving balance")
p8 <- ContUnivar(loan$revol_util, "Revolving utilization rate")

plot_grid(p1, p2, p3, p4, p5, p6, p7 ,p8, ncol = 4)


##***************************************************************************##
#                                                                             #
#                         Multivariate Analysis                               #
#                        (Categorical Variables)                              #
#                                                                             #
##***************************************************************************##

p1 <- CatBivar(loan$loan_status, loan$grade, "Grade","Loan Status") + 
      theme(legend.position = 'none')
p2 <- CatBivar(loan$loan_status, loan$purpose, "Purpose", "Loan Status") + 
      theme(legend.position = 'none') + coord_flip()
p3 <- CatBivar(loan$loan_status, loan$credit_line_age,
               "Credit line age", "Loan Status") + 
      theme(legend.position = 'none')
p4 <- CatBivar(loan$loan_status, loan$term, "Term", "Loan Status") 

plot_grid(plot_grid(p1, p2, rel_widths = c(1, 2)), 
          plot_grid(p3,p4, rel_widths = c(3, 1)), ncol = 1)

CatBivar(loan$loan_status, loan$sub_grade, "Sub grade", "Loan Status") +
  coord_flip()
# Term 
# More Charge off in 60 month tenures than 36 months tenure. 

# Grade & Sub grade- 
# There is a clear relationship between the grade assigned by Lending Club 
# and the loan status as follows. 94 % of A-grade loans are fully paid. 
# This percentage gradually lowers down to 66% for G-grade loans. 

# Credit line age - 
# More risk of defaulting with borrowers of Credit line age <=5 

# Loan purpose 
# The less risky loan purpose are wedding loans/major purpose loans
# with a 90% repayment rate. 
# And the most risky is small businesses funding, with a 73% repayment rate

p1 <- CatBivar(loan$loan_status, loan$home_ownership,
               "Home Ownership", "Loan Status") +
      theme(legend.position = 'none')
p2 <- CatBivar(loan$loan_status, loan$issue_year, 
               "Issue year", "Loan Status") +
      theme(legend.position = 'none')
p3 <- CatBivar(loan$loan_status, loan$verification_status, 
      "Verification Status", "Loan Status") + theme(legend.position = 'none')
p4 <- CatBivar(loan$loan_status, loan$emp_length, 
               "Employment Length", "Loan Status")
      
plot_grid(plot_grid(p1, p2, rel_widths = c(1.5, 2)), 
          plot_grid(p3,p4, rel_widths = c(1.5, 2)), ncol = 1)


# Home Ownership - Other category 18% followed by Rent 15%. Least is 
# Mortgage 14%
# No major impact of Default rate based on Verification Status 
# or issue year

# Employment Length
# No major variations here. The value is more in 10 years since 
# the experience spread is more asit includes all 10+ years records.

CatBivar(loan$loan_status, loan$addr_state, "State", "Loan Status")
# State doesn't show much insights except Nebraska state 
# where 60% of loans are carged off followed by Nevada state with 
#  23 % of loans charged off. 
nrow(filter(loan, addr_state == "NE" | addr_state == "NV"))/dim(loan)[1]
# But since they constitute 1% of loans, we will ignore the State variable

ggplot(loan, aes(x = issue_year,  col = purpose)) +
  geom_line(stat = "count", aes(linetype = purpose), size = 1.2) + 
  scale_y_log10()  +
  labs(x = "Issue year" , y = "# of loans(log10)", 
       title = " Year on Year Loan Frequeny by Purpose") +
  plot_theme

##***************************************************************************##
# Insights Summary                                                            #
##***************************************************************************##

# Defaulting rate depends on the following variables:
  # 1) Grade
  # 2) Purpose
  # 3) Term

# Some Correlation on Defaulting rate with Credit line age

# No Major insights from : 
  # State
  # Home Ownership
  # Verification Status
  # Employment Length


##***************************************************************************##
#                                                                             #
#                         Multivariate Analysis                               #
#                  (Continuous & Categorical Variables)                       #
#                                                                             #
##***************************************************************************##

# Convert grades to numbers inorder to use in correlation plot, 
# since these are ordered categorical variables
loan$grade_level <-  
  as.numeric(loan$grade)
# Convert target variable Status to numeric
loan$status_level <-  
  as.numeric(ifelse(loan$loan_status == "Charged Off", "1", "0"))

corrplot(cor(na.omit(loan[, c(1,3,4, 9,15,16,18:27)])), method = "circle")

# "pub_rec"  shows no major correlation
# Correlation of Categorical variables - Grades & Status on the below: 
  # 1) Revolving utilization rate
  # 2) Loan Amount
  # 3) Credit Line age
  # 4) Annual income
  # 5) Interest rate
  # 6) Installment to income ratio
  # 7) Inquiries since last 6 months
  # 8) dti

# Lets check the correlation of each: 

p1 <- ContCatBivar(loan$grade, loan$revol_util, 
                   "Grade", "Revolving util rate")
p2 <- ContCatBivar(loan$grade, loan$loan_amnt, 
                   "Grade", "Loan Amount")
p3 <- ContCatBivar(loan$grade, loan$credit_line_age, 
                   "Grade", "Credit Line Age")
p4 <- ContCatBivar(loan$loan_status, loan$revol_util, 
                   "Status", "Revolving util rate")
p5 <- ContCatBivar(loan$loan_status, loan$loan_amnt, 
                   "Status", "Loan Amount")
p6 <- ContCatBivar(loan$loan_status, loan$credit_line_age, 
                   "Status", "Credit Line Age")

plot_grid(p1, p2, p3, p4, p5, p6)

# Grades are dependant on revolving utilization rate. Also higher the 
# revolving utilization rate higher is the probabilty of default

# Grade A seems to be assigned for lesser loan amount(Though there are few 
# outliers) and Grade G seems to be assigned for higher loan amounts
# One of the derived metric is the credit line age. This also seems to be 
# another driving factor for assigning Grades. Higher Grades (grade A)  
# are assigned for applicants with higher credit_line_age and  all the way
# upto G grade is assigned as the credit_line_age decreases. 
# But Loan amount and Credit line age some slight correlation 
# on the status.  

p1 <- ContCatBivar(loan$grade, loan$annual_inc, 
                   "Grade", "Annual Income")
p2 <- ContCatBivar(loan$grade, loan$iti, 
                   "Grade", "Installment to Income ratio")
p3 <- ContCatBivar(loan$grade, loan$int_rate, 
                   "Grade", "Interest rate")
p4 <- ContCatBivar(loan$loan_status, loan$annual_inc, 
                   "Grade", "Annual Income")
p5 <- ContCatBivar(loan$loan_status, loan$iti, 
                   "Status", "Installment to Income ratio")
p6 <- ContCatBivar(loan$loan_status, loan$int_rate, 
                   "Status", "Interest rate")

plot_grid(p1, p2, p3, p4, p5, p6) 

# iti & interest rates increases are least for Grades A and highest for 
# grade E
# Defaulting rate is more when the annual income is low, 
# Installment to income ratio is high and interest rate is high

p1 <- ContCatBivar(loan$grade, loan$inq_last_6mths, 
                   "Grade", "Inquiries since last 6 month")
p2 <- ContCatBivar(loan$grade, loan$dti, 
                   "Grade", "dti")
p3 <- ContCatBivar(loan$grade, loan$revol_bal, 
                   "Status", "Revolving balance")
p4 <- ContCatBivar(loan$loan_status, loan$inq_last_6mths, 
                   "Status", "Inquiries since last 6 month")
p5 <- ContCatBivar(loan$loan_status, loan$dti, 
                   "Status", "dti")
p6 <- ContCatBivar(loan$loan_status, loan$revol_bal, 
                   "Status", "Revolving balance")

plot_grid(p1, p2, p3, p4, p5, p6)

# Inquiries since last 6 months, dti  & revolving balance 
# shows some slight dependancy on the status and grade.  

p1 <- ContCatBivar(loan$grade, loan$delinq_2yrs, 
                   "Grade", "delinq_2yrs")
p2 <- ContCatBivar(loan$grade, loan$open_acc, 
                   "Grade", "open_acc")
p3 <- ContCatBivar(loan$grade, loan$total_acc, 
                   "Grade", "total_acc")
p4 <- ContCatBivar(loan$loan_status, loan$delinq_2yrs, 
                   "Status", "delinq_2yrs")
p5 <- ContCatBivar(loan$loan_status, loan$open_acc, 
                   "Status", "open_acc")
p6 <- ContCatBivar(loan$loan_status, loan$total_acc, 
                   "Status", "total_acc")

plot_grid(p1, p2, p3, p4, p5, p6)


# Some dependancy of status with delinq_2yrs
# No major insights from : 
  # open_acc
  # total_acc

p1 <- HistPlot(loan$revol_util,"Revolving Utilization rate", 10) + 
  theme(legend.position = 'none')
p2 <- HistPlot(loan$int_rate,"Interest Rate", 5)  + 
  theme(legend.position = 'none')
p3 <- HistPlot(loan$iti, "Installment to Income ratio", 5)  + 
  theme(legend.position = 'none')
p4 <- HistPlot(loan$annual_inc,"Annual income", 10000)

plot_grid(p1,p2,p3,p4)

p1 <- HistPlot(loan$loan_amnt,"Loan Amount", 5000) + 
  theme(legend.position = 'none')
p2 <- HistPlot(loan$credit_line_age,"Credit Line Age", 10)  + 
  theme(legend.position = 'none')
p3 <- HistPlot(loan$inq_last_6mths, "Inq. Since last 6 months", 1)  + 
  theme(legend.position = 'none')
p4 <- HistPlot(loan$delinq_2yrs,"Delinq_2 years", 2)

plot_grid(p1,p2,p3,p4)

##***************************************************************************##
# Insights Summary                                                            #
##***************************************************************************##

# Defaulting rate depends on the following variables:

# Strong correlation in the below order :  
  # 1) Interest rate
  # 2) Installment to income ratio
  # 3) Revolving utilization rate
  # 4) Annual income

# Some correlation on the variables below : 
  # Credit line Age  
  # Loan Amount
  # inq_last_6mths
  # delinq_2yrs
  # dti
  # Revolving balance

# No major insights from : 
  # open_acc
  # total_acc

##***************************************************************************##
#                                                                             #
#                         Segmented Analysis                                  #
#                                                                             #
##***************************************************************************##

# Lets drill down our analyis on the below variables: 
  # 1) Grade
  # 2) Purpose
  # 3) Term
  # 4) Interest rate
  # 5) Installment to income ratio
  # 6) Revolving utilization rate
  # 7) Annual income


loan$int_rate_seg <- ifelse(loan$int_rate <= 8,"LowInt",
                            ifelse(loan$int_rate > 8 & loan$int_rate <= 15,
                                 "MedInt","HighInt"))
loan$int_rate_seg <- factor(loan$int_rate_seg , levels = c("LowInt", "MedInt", "HighInt"))

loan$iti_ratio_seg <- ifelse(loan$iti <= 5,"LowITI",
                            ifelse(loan$iti > 5 & loan$iti <= 15,
                                   "MedITI","HighITI"))
loan$iti_ratio_seg <- factor(loan$iti_ratio_seg , levels = c("LowITI", "MedITI", "HighITI"))

loan$ann_inc_seg <- ifelse(loan$annual_inc <= 50000,"LowInc",
                             ifelse(loan$annual_inc > 50000 & loan$annual_inc <= 100000,
                                    "MedInc","HighInc"))
loan$ann_inc_seg <- factor(loan$ann_inc_seg , levels = c("LowInc", "MedInc", "HighInc"))


loan$revol_util_seg <- ifelse(loan$revol_util <= 40,"LowRUtil",
                           ifelse(loan$revol_util > 40 & loan$revol_util <= 70,
                                  "MedRUtil","HighRUtil"))
loan$revol_util_seg <- factor(loan$revol_util_seg , levels = c("LowRUtil", "MedRUtil", "HighRUtil"))

# Focusing on segments where maximum loans were issued. 
# Top 3 Loan Purposes by proportion
# debt_consolidation
# credit_card
# other

loan_debt <-  loan %>% 
  filter(purpose == "debt_consolidation")

p1 <- CatBivar(loan_debt$loan_status, loan_debt$grade, "Grade", "Loan Status") +
      theme(legend.position = 'none')
p2 <- CatBivar(loan_debt$loan_status, loan_debt$revol_util_seg, "Revolving Util Rate", "Loan Status") + 
      theme(legend.position = 'none')
p3 <- CatBivar(loan_debt$loan_status, loan_debt$int_rate_seg, "Interest rate", "Loan Status") +
      theme(legend.position = 'none')
p4 <- CatBivar(loan_debt$loan_status, loan_debt$iti_ratio_seg, "ITI", "Loan Status") + 
      theme(legend.position = 'none')
p5 <- CatBivar(loan_debt$loan_status, loan_debt$ann_inc_seg, "Annual Income", "Loan Status") + 
      theme(legend.position = 'none')
p6 <- CatBivar(loan_debt$loan_status, loan_debt$term, "Term", "Loan Status")

plot_grid(p1, p2, p3, p4, p5, p6)

loan_credit <-  loan %>% 
  filter(purpose == "credit_card")

p1 <- CatBivar(loan_credit$loan_status, loan_credit$grade, "Grade", "Loan Status") +
  theme(legend.position = 'none')

p2 <- CatBivar(loan_credit$loan_status, loan_credit$revol_util_seg, "Revolving Util Rate", "Loan Status") +
  theme(legend.position = 'none')
p3 <- CatBivar(loan_credit$loan_status, loan_credit$int_rate_seg, "Interest rate", "Loan Status") +
  theme(legend.position = 'none')
p4 <- CatBivar(loan_credit$loan_status, loan_credit$iti_ratio_seg, "ITI", "Loan Status") +
  theme(legend.position = 'none')
p5 <- CatBivar(loan_credit$loan_status, loan_credit$ann_inc_seg, "Annual Income", "Loan Status") +
  theme(legend.position = 'none')
p6 <- CatBivar(loan_credit$loan_status, loan_credit$term, "Term", "Loan Status")

plot_grid(p1, p2, p3, p4, p5, p6)


loan_other <-  loan %>% 
  filter(purpose == "other")

p1 <- CatBivar(loan_other$loan_status, loan_other$grade, "Grade", "Loan Status") +
  theme(legend.position = 'none')
p2 <- CatBivar(loan_other$loan_status, loan_other$revol_util_seg, "Revolving Util Rate", "Loan Status") +
  theme(legend.position = 'none')
p3 <- CatBivar(loan_other$loan_status, loan_other$int_rate_seg, "Interest rate", "Loan Status") +
  theme(legend.position = 'none')
p4 <- CatBivar(loan_other$loan_status, loan_other$iti_ratio_seg, "ITI", "Loan Status") +
  theme(legend.position = 'none')
p5 <- CatBivar(loan_other$loan_status, loan_other$ann_inc_seg, "Annual Income", "Loan Status") +
  theme(legend.position = 'none')
p6 <- CatBivar(loan_other$loan_status, loan_other$term, "Term", "Loan Status")

plot_grid(p1, p2, p3, p4, p5, p6)

loan_segment1 <-  loan %>% 
  filter(purpose %in% c("debt_consolidation","credit_card", "other") &
           (grade %in% c("E", "F", "G") |  int_rate_seg == "HighInt") &
           revol_util_seg == "HighRUtil" & 
           term == 60 & 
           iti_ratio_seg == "HighITI" &
           annual_inc < 40000)
percent(prop.table(table(loan_segment1$loan_status)))


# Top 3 Loan Purposes by Defaulting rate
# Lets focus on other segments where there are maximum charge offs 
# Small business, renewable energy and educational

loan_default <-  loan %>% 
  filter(purpose %in% c("small_business", "renewable_energy", "Educational"))

p1 <- CatBivar(loan_default$loan_status, loan_default$grade, "Grade", "Loan Status")
p2 <- CatBivar(loan_default$loan_status, loan_default$term, "Term", "Loan Status")
p3 <- CatBivar(loan_default$loan_status, loan_default$int_rate_seg, "Interest rate", "Loan Status")
p4 <- CatBivar(loan_default$loan_status, loan_default$iti_ratio_seg, "ITI", "Loan Status")
p5 <- CatBivar(loan_default$loan_status, loan_default$ann_inc_seg, "Annual Income", "Loan Status")
p6 <- CatBivar(loan_default$loan_status, loan_default$revol_util_seg, "Revolving Util Rate", "Loan Status")

plot_grid(p1, p2, p3, p4, p5, p6)

loan_segment2 <-  loan %>% 
  filter(purpose %in% c("small_business", "renewable_energy", "Educational") & 
         (grade %in% c("E", "F", "G") | int_rate_seg == "HighInt") &
         revol_util_seg == "HighRUtil" & 
         term == 60 & 
         iti_ratio_seg == "HighITI" &
         ann_inc_seg == "LowInc")
percent(prop.table(table(loan_segment2$loan_status)))

##***************************************************************************##
# Insights Summary                                                            #
##***************************************************************************##
# For : 
# grades E, F, G,
# higher interest rates ( >15 %) 
# higher loan tenure(60 months) 
# higher installment to income ratio (>15%)
# Lower Income (< 50 K)

# Defaulting rate is 80% for "small_business", "renewable_energy", 
# "Educational"  loans:
# 
# Defaulting rate increases to 43%. ~1 out of 2 borrowers For "debt_consolidation",
# "credit_card", "other" loans:

# Though the impact of the  above identified features on the defaulting rate may vary 
# based on loan purposes, those are the common most strongly influencing variables

# Lets quantify using numbers on the continuous variables

Charged_Off_loans <-  loan %>% 
  filter(loan_status == "Charged Off") 

Fully_Paid_loans <-  loan %>% 
  filter(loan_status == "Fully Paid")

# For the above datasets: 
# How does interest rate look like?
median(Charged_Off_loans$int_rate)
median(Fully_Paid_loans$int_rate)
# Charged off loans paid (13%), on average a higher interest rate
# than fully paid loans(11%)

# How does the annual income look like?
median(Charged_Off_loans$annual_inc)
median(Fully_Paid_loans$annual_inc)
# Fully paid loans - Average of $60K annual income against Charged off loans 
# at $53K annual income

# How does the installment to income ratio look like?
median(Charged_Off_loans$iti)
median(Fully_Paid_loans$iti)
# Fully paid loans paid 1% lesser iti ratio on an avearge

# How does the revolving utilization rate look like?
median(Charged_Off_loans$revol_util)
median(Fully_Paid_loans$revol_util)
# Revolving utilization rate is more (average 58.20%) for charged off loans
# than that of fully paid loans (average 47.50%)

##***************************************************************************##
#                                                                             #
#                         Putting it all together                             #
#                                                                             #
##***************************************************************************##

ggplot(loan, aes(x = issue_d, y = int_rate, col = grade)) +
  geom_smooth(aes(col = grade)) + 
  labs(x = "Issue year" , y = "Interest rate", 
       title = " Year on Year Interest rate by grade") +
  plot_theme

# Over years interest rates for Grade D through E has been seen increasing 
# especially post 2011 there is a steep rise. 

ggplot(loan, aes(x = int_rate, y = annual_inc)) + 
  geom_point(aes(fill = grade), shape = 22) + 
  facet_grid(term~loan_status)

# Charge off more for 60 month tenure, lesser annual income, 
# higher interest rates, lower grades (E,F,G)

##***************************************************************************##
#                                                                             #
#                             Summary                                         #
#                                                                             #
##***************************************************************************##

# The study results show that below are the strong varaibles impacting 
# defaulting rate
  # 1) Grade
  # 2) Purpose
  # 3) Term
  # 4) Interest rate
  # 5) Installment to income ratio
  # 6) Revolving utilization rate
  # 7) Annual income

# There is a clear relationship between the grade assigned by Lending Club and 
# the probability of default.
# 94.4% of A-grade loans were reimbursed. This percentage gradually decreases 
# to 61.8% for G-grade loans. 

# Loan purpose is also a factor explaining default: wedding is the less risky 
# loan purpose and small business is the riskiest. 

# Defaulting rate depends on Loan tenure. Higher the loan tenure (60 months)
# higher is the defaulting rate.

# On an average Charged off loans paid higher interest rates (13%) than the 
# fully paid loans (11%). i.e higher the interest rate, the higher 
# the default of probability is. Over years interest rates for Grade D through E 
# has been seen increasing especially post 2011 there is a steep rise. 

# Borrower characteristics, such as annual income, installment to income ratio
# Revolving utilization rate also relevant variables. 

# Fully paid borrowers have less Installment to income ratio compared to the 
# defaulting ones

# Fully paid borrowers have an average of $60K annualincome against 
# defaulting borrowers with only an average of $53K annual income

# Revolving utilization rate is more (average 58.20%) for charged off loans
# than that of fully paid loans (average 47.50%)
#************************************End of file*******************************