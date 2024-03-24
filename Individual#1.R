# Student name:  Julia Tsai

# sample R file to upload for individual assignment
# use setwd() to set the working directory
# do not use choose.file() 
# please place the R code for each question in the space provided below.
# R submission files that do not follow the required format will have points deducted.
library(data.table)
library(quantmod)
library(xts)

setwd("E:/MQM_Courses/Term5/Financial_Risk_Management/FRM/data")
load("MQM530_IndividualAssignment1.RData")

### question 2 start ###
data_van <- merge(vfiax_price, vfiax_dividend, by = "Date", all.x = TRUE)
data_van$Dividend <-  na.fill(data_van$Dividend, 0)
str(data_van)
## daily discrete return: ((Pt + Dt)/Pt-1) -1
data_van[, ret_van := (Price + Dividend ) / shift(Price, n = 1) - 1]

answer2 = round(data_van[Date == "2023-02-16"]$ret_van, 4)  ### replace NA with R code generating the answer 
cat("Answer Q2:", answer2,'\n')        
### question 2 end   ###

### question 3 start ###
data_van[, logret_van := log( 1 + ret_van )]

answer3 = round(data_van[Date == "2023-02-16"]$logret_van, 4)  ### replace NA with R code generating the answer 
cat("Answer Q3:", answer3,'\n')        
### question 3 end   ###

### question 4 start ###
vfiax <- getSymbols("vfiax", src="yahoo", from="2022-12-31",
                    to="2024-01-01", auto.assign=FALSE)[,6]
data_yahoo <- as.data.table(vfiax)
names(data_yahoo) <- c("Date","AdjPrice")
data_yahoo[, logret_yahoo := c(NA,diff(log(AdjPrice))) ]
data_yahoo[, ret_yahoo := exp(logret_yahoo)-1 ]

answer4 = round(data_yahoo[Date == "2023-02-16"]$ret_yahoo, 4) ### replace NA with R code generating the answer 
cat("Answer Q4:", answer4,'\n')        
### question 4 end   ###

### question 5 start ###
tmp1 <- data_van[, .(Date,ret_van) ]
tmp2 <- data_yahoo[, .(Date,ret_yahoo) ]
tmp <- merge(tmp1, tmp2, by = "Date")
tmp[, dif := abs(ret_van - ret_yahoo)]
tmp <- na.omit(tmp)

answer5 = max(tmp$dif) ### replace NA with R code generating the answer 
cat("Answer Q5:", answer5,'\n')        
### question 5 end   ###

### question 6 start ###
logret.xts <- as.xts(x=data_yahoo$logret_yahoo, order.by=data_yahoo$Date)
logret_monthly <- apply.monthly(logret.xts,sum)
ret_monthly <- exp(logret_monthly) - 1

answer6 = round(ret_monthly["2023-12-29"],4) ### replace NA with R code generating the answer 
cat("Answer Q6:", answer6,'\n')        
### question 6 end   ###

### Verify your answers ###
check_for_answer <- function(question_num) {
  # check environment if variable containing the answer is present in R environment
  # in try catch block below
  tryCatch({
    if (exists(paste0("answer",question_num))){
      answer <- get(paste0("answer",question_num))  
    }
    else{
      answer <- NA
    }
  })
  return(answer)
}
answers = rep(NA, 6)
for (i in c(2,3,4,5,6)){
  answers[i] = check_for_answer(i)
}
answers
