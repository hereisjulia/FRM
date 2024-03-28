# Student name:  Julia Tsai

# sample R file to upload for individual assignment
# use setwd() to set the working directory
# do not use choose.file() 
# please place the R code for each question in the space provided below.
# R submission files that do not follow the required format will have points deducted.
library(data.table)
library(quantmod)
library(xts)
library(MASS)

start_date <- "2002-03-01"
end_date <- "2023-01-01"
tkr <- "GME"
data <- getSymbols(tkr,src="yahoo",from=start_date,to=end_date,auto.assign=FALSE)
data <- data[,6]                     # this keeps only the adjust closing price of GME
df <- as.data.table(data)            # create a data.table with two columns
names(df) <- c("date","adjprice")    # first column is 'date', second column is 'price'
df[, logret := c(NA,diff(log(adjprice)))]  # calculate daily log return
df[, ret    := exp(logret)-1 ]       # calculate daily discrete return
df <- df[-1]                         # remove first observation, with NA in logret and ret

#----------------------------------------------------
### question 2 start ###
DATE <- function(yyyy,mm,dd) {
  dte  <- as.Date(sprintf("%i-%i-%i",yyyy,mm,dd),format="%Y-%m-%d")
  return(dte)
}
logret <- df[date<DATE(2020,12,31)]$logret

fit_n <- fitdistr(logret, densfun = "normal")
AIC_n <- 2*length(fit_n$estimate) - 2*fit_n$loglik

answer2 = round(AIC_n)              ### replace NA with R code generating the answer 
cat("Answer Q2:", answer2,'\n')        
### question 2 end   ###

#----------------------------------------------------
### question 3 start ###
fit_t <- fitdistr(logret, densfun = "t")
AIC_t <- 2*length(fit_t$estimate) - 2*fit_t$loglik

answer3 = round(AIC_t)                ### replace NA with R code generating the answer 
cat("Answer Q3:", answer3,'\n')        
### question 3 end   ###

#----------------------------------------------------
### question 4 start ###
rtms <- function(n,df,m,s) {
  return( ( rt(n,df) * s + m ) )
}
CalcVaRES <- function(r,alpha) {
  VaR <- quantile(r,1-alpha)
  ES <- mean(r[r<VaR])
  VaR_ES <- c(VaR,ES)
  names(VaR_ES) <- c("VaR","ES")
  return(VaR_ES)
}

tdf <- fit_t$estimate[[3]]
m <- fit_t$estimate[[1]]
s <- fit_t$estimate[[2]]
nsim <- 100000
set.seed(437)
sim <- rtms(nsim,tdf,m,s)
alpha <- 0.95
VaR_ES <- CalcVaRES(sim,alpha)

answer4 = round(VaR_ES[[1]],4)   ### replace NA with R code generating the answer 
cat("Answer Q4:", answer4,'\n')        
### question 4 end   ###

#----------------------------------------------------
### question 5 start ###
initial <- 100000
loss <- sim*initial
loss_Var_ES <- CalcVaRES(loss, alpha)

answer5 = round(loss_Var_ES[[1]])      ### replace NA with R code generating the answer 
cat("Answer Q5:", answer5,'\n')        
### question 5 end   ###

#----------------------------------------------------
### question 6 start ###
istart <- sum(df$date<=DATE(2020,12,31))
iend   <- sum(df$date<=DATE(2021,6,30))
df$VaR.t <- NA
set.seed(437)
for (idt in c((istart-1):(iend-1))) {
  tmp <- df$logret[c(1:idt)]
  #
  # add your code here
  # estimate the IID~t distribution for tmp
  fit_t <- fitdistr(tmp, densfun = "t")
  # simulate 100,000 times from this estimated model
  nsim <- 100000
  tdf <- fit_t$estimate[[3]]
  m <- fit_t$estimate[[1]]
  s <- fit_t$estimate[[2]]
  sim <- rtms(nsim,tdf,m,s)
  # find the VaR at the 95 percent confidence level using the CalcVaRES() function
  alpha <- 0.95
  VaR_ES <- CalcVaRES(sim,alpha)
  # store the VaR in the following day
  df$VaR.t[(idt+1)] <- VaR_ES[[1]]
}
df.q6 <- df[c((istart+1):iend)]   # displays the results for 2021-01-04 to 2021-06-30

answer6 = sum(df.q6$logret < df.q6$VaR.t)   ### replace NA with R code generating the answer 
cat("Answer Q6:", answer6,'\n')        
### question 6 end   ###




answers = rep(NA, 6)
for (i in 2:6){
  answers[i] = check_for_answer(i)
}
answers
