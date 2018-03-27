###############################################################################
##                                                                           ##
## Project        : A Case Study on Lending Club Loan                        ##
## Objective      : To minimise the risk of losing money while lending to    ##
##                  customers by identifying risky loan applicants using     ##
##                  Exploratory Data Analysis                                ##
## Date           : 1-Apr-2018                                               ##
## Version        : 1.0                                                      ##
## Author(s)      : Chaithanya Reddy, Poovarasan Murugesan                   ##
##                  Pranesh Ramaiya, Jayakumar Sekar                         ##
##                                                                           ##
###############################################################################

#Include the required libraries
library(tidyverse)
library(lubridate)
library(formattable)
library(corrplot)
library(ggedit)

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
# 39717 matches. No duplicates. No member got 2 different loans. 

# Note : We are interested in variables collected during the loan application
# process. All the other variables that are collected after the loan has been 
# issued, is  not of much interest  since our focus is to  identify the risky 
# loan applicants during the application process

###############################################################################
#                                                                             #
#                             Data Cleaning                                   #
#                                                                             #
###############################################################################
# Issues identified
#  * NA values
#  * Columns that are of not much use or Unnecessary columns
#  * Dates as strings
#  * Loan term as string
#  * Percentages as strings
###############################################################################


# Issue 1: NA values

colSums(is.na(loan))

# There are quite a large number of NAs in columns. Identify and remove columns
# where the entire values are NA (i.e 39717 NAs).

table(colSums(is.na(loan))) 

# 54 columns - All NAs, 1 column - 38577 NA
# 1 column  - 36931 NAs, 1 column - 25682 NA

loan <- loan %>% select(-which(colMeans(is.na(loan)) > 0.5))

# revol_util column has 50 NAs (<0.1 %). Replace them with 0s. 
loan$revol_util[is.na(loan$revol_util)] <- 0

# Check for complete records 
sum(complete.cases(loan))               
# 39717 matches with 39717 observations. All NAs have been taken care.


# Issue 2: Unnecessary columns with not much weightage for analysis
# id                      - Loan Id :not required for analysis
# member id               - Member Id : not required for analysis
# emp_title                
# desc                    - Can be used for text analysis to identify
#                            patterns. But for now we will ignore.
# pymnt_plan              - only 1 constant 'n' value
# url                     - Not required
# zip_code                - Masked ZIP code. Not required.
# Initial_list_status     - only 1 constant 'f' value
# out_prncp,out_prncp_inv -
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
loan$emp_length <- str_replace_all(loan$emp_length, "n/a", "0")
loan$emp_length <- str_replace_all(loan$emp_length, "\\< 1", "0")
loan$emp_length <- as.character(str_extract(loan$emp_length,  "[[:digit:]]+"))
summary(loan$emp_length)


# Issue 7:  Outliers
# Outliers in continuos varibales will be handled during univariate analysis 
# of each variable

# We will remove all the  current ongoing records as we are interested in only 
# fully paid and charged off records to identify pattern in risky applicants. 
loan <-  loan[loan$loan_status != "Current",]


# Converting to factors
# grade, sub_grade, home_ownership, verification_status, loan_status , 
# addr_state, purpose
loan <- loan %>% mutate_if(is.character,as.factor)

# Assign Levels to factors

levels(loan$grade) <- c(LETTERS[1:7])
levels(loan$loan_status) <- c("Charged Off" ,"Fully Paid")

str(loan)   # 38577 obs. of  23 variables

###############################################################################
#                                                                             #
#                         Derive New variables                                #
#                                                                             #
###############################################################################

# Age of credit line as of loan issue date
loan$credit_line_age <- year(loan$issue_d) - year(loan$earliest_cr_line)

# Monthly installment to Monthly Income ratio
loan$iti <-  round((loan$installment / (loan$annual_inc / 12)) * 100, 2)

str(loan)

# Metadata

# Customer Demographic Variables - 
# home_ownership , annual_inc, verification_status, addr_state, purpose 

# Loan Financial variables - 
# loan amount, interest rate, loan status, loan grade, loan sub-grade,
# dti, iti, issue date, term, installment

###############################################################################
#                                                                             #
#                         Univariate Analysis                                 #
#                       (Categorical Variables)                               #
#                                                                             #
###############################################################################

plot_theme <- theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 22,face = 'bold'),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x  = element_text(size = 10),
        axis.text.y  = element_text(size = 12))

univar <- function(feature, xlabel, ylabel = "Proportion", df = loan) {
  print(percent(prop.table(table(feature))))
  df %>% count(var = feature) %>% 
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

univar(loan$term, "Term")  
# 36 months  60 months
#   75%        25% 

univar(loan$purpose, "Loan Purpose") +  coord_flip()
# Top 5 purposes based on the number of loans applied
# 1. debt_consolidation  - 46.80%        
# 2. credit_card  - 13.03%    
# 3. other - 10.02% 
# 4. home_improvement - 7.45%
# 5. major_purchase - 5.57%

univar(loan$grade, "Grade")
univar(loan$sub_grade, "Sub Grade")
# Grade B seems to have highest loans followed by A
# the number decrease with Grade E having least loans.

univar(loan$home_ownership, "Home Ownership") 
# MORTGAGE   NONE    OTHER      OWN      RENT 
#  44.12%   0.01%    0.25%     7.71%     47.9% 
# Majority (~92%) of the borrowers are either in Mortgaged or Rented houses

univar(loan$verification_status, "Verification Status") 
# Not Verified   Source Verified    Verified 
# 43%               25%             32% 

univar(factor(loan$emp_length), "Employment Experience")
# People with less years of employment experience are the ones who have 
# requested for more loans 
# Note : The value is more in 10 years since the experience spread is more as
# it includes all 10+ years records

univar(year(loan$issue_d), "Issue year") 
# There is a considerable raise in the number of loans from 2007 through 2011

univar(loan$addr_state, "State")
# Most borrowers are from state of California (18%) followed by  Newyork (10%)
#  followed by Florida(7%). Together they constitue 35% of the loans.

univar(loan$loan_status , "Loan Status") 
# Charged Off  Fully Paid 
#  15%         85%


###############################################################################
#                                                                             #
#                         Univariate Analysis                                 #
#                       (Continuos Variables)                                 #
#                                                                             #
###############################################################################

summary(loan)

# All outliers are not treated since they may help in identifying the impact
# on the target variable Loan Status


# Loan amount:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 500    5300    9600   11047   15000   35000 
# Loan amounts range from $500 to $35000 with an average of $9600
boxplot(loan$loan_amnt, notch = T)
# Outliers greater than $30000 (only 1% records) are capped to $30000 
loan[which(loan$loan_amnt > 30000),]$loan_amnt <-  30000

# Interest rate:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.00    8.00   11.00   11.36   14.00   24.00 
# Interest rates range from 5% to 24% with and average of 11% 
boxplot(loan$int_rate, notch = T)
# Outliers greater than 23 (0.5% records) are capped to 23
loan[which(loan$int_rate > 22),]$int_rate <-  22

# Debt-to-Income ratio:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.00    8.13   13.37   13.27   18.56   29.99 
# Lending Club borrowers has dti ranging from 0 to 30 
# with an average of 13. 
boxplot(loan$dti, notch = T)

# Credit line age (derived metric) :
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.0     9.0    13.0    13.7    17.0    51.0 
# Lending Club borrowers has  credit history ranging from 3 to 51 years
# with an average of 13 years 
boxplot(loan$credit_line_age, notch = T)
# Outliers greater than 29 (only 2% records) are capped at 29
loan[which(loan$credit_line_age > 29),]$credit_line_age <-  29


# Annual income : 
# Considering only incomes < 100000 to ignore the outliers
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4000   40000   58868   68624   82000 2000000 
# Average of $58k of annual personal income
boxplot(loan$annual_inc, notch = T)
# Outliers > 120K (8% records) not treated since it may help in identifying
# the impact on the target variable Loan Status

# Installment to income(monthly) ratio (derived metric): 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.080   3.500   5.770   6.563   8.810  32.030
# Lending Club borrowers has iti ranging from 0 to 32
# with an average of 6. 
boxplot(loan$iti, notch = T)
# Outliers in iti > . 
loan[which(loan$iti > 17),]$iti <-  17

###############################################################################
#                                                                             #
#                         Multivariate Analysis                               #
#                        (Categorical Variables)                              #
#                                                                             #
###############################################################################

bivar <-  function(status, xvar, xlabel) {
  as.data.frame(percent(prop.table(table(status, xvar), 2))) %>%
  ggplot(aes(x = xvar, y = Freq,  fill = status)) +
    geom_col( position = "fill" ) +
    geom_text(aes(label = Freq),
                  position = position_fill(vjust = .5),  
              size = 3) +
    labs(x = xlabel, y = "Proportion", title = paste("Loan Status Proportion vs", xlabel)) +
    plot_theme
  }

bivar(loan$loan_status, loan$grade, "Grade")
bivar(loan$loan_status, loan$sub_grade, "Sub grade") + coord_flip()
# There is a clear relationship between the grade assigned by Lending Club 
# and the loan status as follows. 94 % of A-grade loans are fully paid. 
# This percentage gradually lowers down to 66% for G-grade loans. 

bivar(loan$loan_status, loan$purpose, "purpose") + coord_flip()
# The less risky loan purpose are wedding loans/major purpose loans
# with a 90% repayment rate. 
# And the most risky is small businesses funding, with a 73% repayment rate

bivar(loan$loan_status, loan$emp_length, "Employment Length")
# No major variations here

bivar(loan$loan_status, loan$home_ownership, "Home Ownership")
# No major variations here

bivar(loan$loan_status, loan$verification_status, "Verification Status")
# No major variations here

bivar(loan$loan_status, loan$credit_line_age, "Credit line age") %>%
  remove_geom('text',1)
bivar(loan$loan_status, year(loan$issue_d), "Issue year")

###############################################################################
#                                                                             #
#                         Multivariate Analysis                               #
#                        (Continuous Variables)                               #
#                                                                             #
###############################################################################

# Convert grades to numbers inorder to use in correlation plot, 
# since these are ordered categorical variables
loan$grade_level <-  as.numeric(loan$grade)
# Convert target variable Status to numeric
loan$status_level <-  as.numeric(ifelse(loan$loan_status == "Charged Off", "1", "0"))

corr <- cor(na.omit(loan[, c(1,3,4, 9,15,16,18:27)]))
corrplot(corr, method = "circle")

# On the basis of borrower's desired Loan amount and Revolving line 
# utilization rate, Grades are determined, as seen by the positive 
# correlation between them. 

boxplot(loan$revol_util~loan$grade, notch = T)
boxplot(loan$loan_amnt~loan$grade, notch = T)

# One of the derived metric is the credit line age. This also seems to be 
# another driving factor for assigning Grades. Higher Grades (grade A)  
# is assigned for applicants with higher credit_line_age and  all the way
# upto G grade is assigned as the credit_line_age decreases. In addition higher 
# this metric higher is the emp_length and higher is the total number 
# of credit lines.
boxplot(loan$credit_line_age~loan$grade, notch = T)
boxplot(as.numeric(loan$emp_length)~loan$grade, notch = T)
# Shows no major variations

boxplot(loan$dti~loan$grade, notch = F)
# No Significant variations except for A & G grades
# Surprisingly dti shows no correlation to Grades here. 
# One reason could be because Grades are assigned based 
# on other demographic factors as well, such as credit score, credit 
# history, age etc which we are not aware. 

boxplot(loan$iti~loan$grade, notch = F)
# Lowest Installment to income ratio in Grade A and highest  iti in 
# Grade G

# To Summarize Grades assigned based on Loan amount, Revolving line 
# utilization rate, dti, earliest_cr_line (credit line age), total 
# number of credit lines and other demographic factors such as
# credit score

boxplot(loan$iti~loan$loan_status, notch = F)

# Interest rates are then determined based on the grades as seen from 
# the +ve correlation. Grade A loans has lowest interest rate 
# all the way up to Grade E with highest interest rate. 

boxplot(loan$int_rate~loan$grade, notch = F)

# Over years interest rates for Grade D through E has seen increase 
# especially post 2011 there is a steep rise. 

ggplot(loan, aes(x = issue_d, y = int_rate, col = grade)) +
  geom_smooth(aes(col = grade)) + 
  labs(x = "Issue year" , y = "Interest rate", 
       title = " Year on Year Interest rate by grade") +
  plot_theme

# As expected Loan amount and installment have Positive correlation. 

# Higher Loan amounts given to borrowers with higher revolving 
# credit balance.
ggplot(loan, aes(x = loan_amnt, y = revol_bal)) + 
  geom_smooth() 

# Higher Loan amounts given to borrowers with higher tenure(60 months) 
# with higher interest rates. 
ggplot(loan, aes(x = int_rate, y = loan_amnt)) + 
  geom_point(aes(fill = term), shape = 22) 

# Higher the loan amount (higher installments) and lower the 
# monthly income, higher is the derived metric Monthly Installment to 
# Monthly Income ratio (iti) and vice versa

ggplot(loan, aes(x = int_rate, y = loan_amnt)) + 
  geom_point(aes(fill = grade), shape = 22) + 
  facet_grid(term~loan_status)
# Int rate are determined based on the grades 
# The density of loan decreases as we move from grade A to garde G
# More loans under 36 month tenure 
# More charge off loans in 60 month tenure in grades E, F, G

ggplot(filter(loan, annual_inc < 200000), aes(x = annual_inc, y = credit_line_age)) + 
  geom_point(aes(fill = term), shape = 22) + 
  facet_grid(loan_status~grade)
# Credit line age for grades A through D seems widely distributed with max age ~48

ggplot(loan, aes(x = year(issue_d),  col = purpose)) +
  geom_line(stat = "count", aes(linetype = purpose), size = 1.2) + 
  scale_y_log10()  +
  labs(x = "Issue year" , y = "# of loans(log10)", 
       title = " Year on Year Loan Frequeny by Purpose") +
  plot_theme

ggplot(loan, aes(x = issue_d, y = int_rate, col = grade)) +
  geom_line(stat = "identity", aes(linetype = grade), size = 1.2) + 
  labs(x = "Issue year" , y = "# of loans(log10)", 
       title = " Year on Year Loan Frequeny by Purpose") +
  plot_theme

###############################################################################
#                                                                             #
#                Categorical vs Continuous variables Analysis                 #
#                                                                             #
###############################################################################







###############################################################################
#                                                                             #
#                         Bivariate Analysis                                  #
#                                                                             #
###############################################################################
# by(loan$emp_length, loan$loan_status, summary)

# library(sm)
# 
# sm.density.compare(loan$emp_length, loan$loan_status)

# Categorical vs Categorical
# xtabs( ~ grade + loan_status, data = loan)

###############################################################################
#                                                                             #
#                         Segmented Analysis                                  #
#                                                                             #
###############################################################################

Charged_Off_loans <-  loan %>% 
  filter(loan_status == "Charged Off") 

Fully_Paid_loans <-  loan %>% 
  filter(loan_status == "Fully Paid")

# How does interest rate look for the above datasets? 

summary(Charged_Off_loans$int_rate)
summary(Fully_Paid_loans$int_rate)

# As expected Charged off loans paid  (13%), on average a higher interest rate than 
# fully paid loans(11%)


summary(Charged_Off_loans$annual_inc)
summary(Fully_Paid_loans$annual_inc)


summary(Charged_Off_loans$loan_amnt)
summary(Fully_Paid_loans$loan_amnt)


summary(Charged_Off_loans$iti)
summary(Fully_Paid_loans$iti)

# More charge offs in the below segments: 
# Small business, renewable energy and educational

loan_segment1 <-  loan %>% 
  filter(purpose %in% c("small_business", "renewable_energy", "Educational"))  

loan_segment1$term <- as.numeric(as.character(loan_segment1$term))
loan_segment1$emp_length <- as.numeric(loan_segment1$emp_length )
#corr plot
corr <- cor(na.omit(loan_segment1[, c(1,2,3,4, 7, 9,15,16,18:27)]))
corrplot(corr, method = "circle")

# Status >>> positively correlated to 
# 1. term
# 2. int_rate
# 3. iti
# 4. grade
# 5. revolving utilization rate
                                 
loan_segment2 <-  loan_segment1 %>% filter((term == 60 & int_rate > 20 & iti > 20) |
                  (grade %in% c("E", "F", "G") & revol_util > 50 ))   
prop.table(table(loan_segment2$loan_status))

#  Defaulting rate more for higher interest rates ( > 15 %) for higher 
#  loan tenure(60 months) and for higher installment to income ratio (>15)


# Lets focus on other segments where maximum loans were issued. 

loan_segment3 <-  loan %>% 
  filter(purpose %in% c("debt_consolidation"))

#  "credit_card", "other"))  

loan_segment3$term <- as.numeric(loan_segment3$term)
loan_segment3$emp_length <- as.numeric(loan_segment3$emp_length )

corr <- cor(na.omit(loan_segment3[, c(1,2,3,4, 7, 9,15,16,18:27)]))
corrplot(corr, method = "circle")

loan_segment3 <-  loan_segment3 %>% filter(grade %in% c("D", "E", "F", "G") & term == 60 & 
                  purpose %in% c("debt_consolidation", "credit_card", "other")  &
                  iti > 15)



loan_segment1 <-  loan %>% filter(iti > 20)

loan_segment1 <-  loan %>% filter(grade %in% c("D", "E", "F", "G"))

loan_segment1 <-  loan %>% filter(int_rate > 15)

loan_segment1 <-  loan %>% filter(term == 60 )

loan_segment1 <- loan %>% filter(purpose %in% c("small_business", "renewable_energy", "Educational")) 
loan_segment <-  loan %>% filter()               
prop.table(table(loan_segment$loan_status))


loan_segment1 <-  loan %>% filter(purpose %in% c("debt_consolidation", "credit_card", "other", 
                                                 "home_improvement", "major_purchase", 
                                                 "small_business"))

loan_segment2 <-  loan_segment1 %>% filter(loan_status == "Charged Off")


prop.table(table(round(loan_segment2$iti,0)))

write_csv(loan, "loan_updated.csv")


#The study results show that there is a clear relationship between the grade assigned by Lending Club and the probability of default. 94.4% of A-grade loans were reimbursed. This percentage gradually decreases to 61.8% for G-grade loans. The interest rate assigned depends on the grade assigned and the higher the interest rate, the higher the default probability is. Loan purpose is also a factor explaining default: wedding is the less risky loan purpose and small business is the riskiest. Borrower characteristics, such as annual income, current housing situation, credit history, and borrower indebtedness are relevant variables. No statistically significant differences are found in loan amount or length of employment#


