# Student name:  Julia Tsai

# sample R file to upload for individual assignment
# use setwd() to set the working directory
# do not use choose.file() 
# please place the R code for each question in the space provided below.
# R submission files that do not follow the required format will have points deducted.
library(data.table)
library(MASS)
library(moments)
library(metRology)
library(quantmod)

setwd("E:/MQM_Courses/Term5/Financial_Risk_Management/FRM/data")
load("MQM530_IndividualAssignment2.RData",verbose=TRUE)
logret <- as.vector(log(1+lcap_data$lcap))

### question 2 start ###


answer2 = round(fitdistr(logret, densfun = "normal")$sd[[1]], 4)               ### replace NA with R code generating the answer 
cat("Answer Q2:", answer2,'\n')        
### question 2 end   ###

### question 3 start ###
plot_normal <- function(data,showplot=TRUE) {
  if (!require(ggplot2)) install.packages('ggplot2')
  if (!require(MASS)) install.packages('MASS')
  library(ggplot2)
  library(MASS)
  # r is a vector of data
  r <- as.vector(data)
  r <- na.omit(r)
  r_min  <- min(r)
  r_max  <- max(r)
  n_fit <- fitdistr(r,"normal")
  m <- n_fit$estimate[1]
  s <- n_fit$estimate[2]
  # find limits of x-axis in plot, to nearest 0.05
  plot_lim <-  max(abs(r_min),abs(r_max))
  plot_lim <- (round(plot_lim*100/5)+1)*5/100
  df <- data.frame(r=r)
  g <-ggplot(df,aes(r)) +
    geom_histogram(aes(y=..density..),
                   alpha=0.8) +
    scale_x_continuous(name="Returns",limits=c(-plot_lim,plot_lim)) +
    scale_y_continuous(name="Density") + 
    stat_function(fun = dnorm,
                  args = list(mean = m, sd = s),
                  lwd=1,col = 'red') 
  if (showplot) g
  return(g)
}
plot_normal(logret) 
answer3 = "Graph B"          ### replace NA with R code generating the answer 
cat("Answer Q3:", answer3,'\n')        
### question 3 end   ###

### question 4 start ###
skewness(logret)
kurtosis(logret)
jarque.test(logret)$statistic

answer4 = round(jarque.test(logret)$statistic,3)  ### replace NA with R code generating the answer 
cat("Answer Q4:", answer4,'\n')        
### question 4 end   ###

### question 5 start ###
fitdistr(logret, densfun = "t")$estimate[[3]]

answer5 = round(fitdistr(logret, densfun = "t")$estimate[[3]], 3) ### replace NA with R code generating the answer 
cat("Answer Q5:", answer5,'\n')        
### question 5 end   ###

### question 6 start ###
plot_t <- function(data,showplot=TRUE) {
  if (!require(ggplot2)) install.packages('ggplot2')
  if (!require(MASS)) install.packages('MASS')
  if (!require(metRology)) install.packages('metRology')
  library(ggplot2)
  library(MASS)
  library(metRology)
  # r is a vector of data
  r <- as.vector(data)
  r <- na.omit(r)
  dt2 <- function(x,t_mu,t_sigma,t_df) {
    dt( (x-t_mu)/t_sigma, df=t_df ) / t_sigma
  } 
  t_fit <- fitdistr(r,"t")
  m <- t_fit$estimate[1]
  s <- t_fit$estimate[2]
  tdf <- t_fit$estimate[3]
  r_min  <- min(r)
  r_max  <- max(r)
  # find limits of x-axis in plot, to nearest 0.05
  plot_lim <- max(abs(r_min),abs(r_max))
  plot_lim <- (round(plot_lim*100/5)+1)*5/100
  df <- data.frame(r=r)
  g <- ggplot(df,aes(r)) +
    geom_histogram(aes(y=..density..),
                   alpha=0.8) +
    scale_x_continuous(name="Returns",limits=c(-plot_lim,plot_lim)) +
    scale_y_continuous(name="Density") + 
    stat_function(fun = dt2,
                  args = list(m, s, tdf),
                  lwd=1,col = 'red') 
  if (showplot) g
  return(g)
}

plot_t(logret)

answer6 = "Graph A"          ### replace NA with R code generating the answer 
cat("Answer Q6:", answer6,'\n')        
### question 6 end   ###

### question 7 start ###
fit_n <- fitdistr(logret, "normal")
fit_t <- fitdistr(logret, "t")

AIC_n <- 2*length(fit_n$estimate) - 2*fit_n$loglik
AIC_t <- 2*length(fit_t$estimate) - 2*fit_t$loglik
cat("AIC of normal = ",AIC_n,'\n')
cat("AIC of t = ",AIC_t,'\n')
AIC_t <= AIC_n

answer7 = "t-distribution"         ### replace NA with R code generating the answer 
cat("Answer Q7:", answer7,'\n')        
### question 7 end   ###

### question 8 start ###
rtms <- function(n,df,m,s) {
  return( ( rt(n,df) * s + m ) )
}
# simulate 100,000 for 5 year return
tdf <- fit_t$estimate[[3]]
m <- fit_t$estimate[[1]]
s <- fit_t$estimate[[2]]
nsim <- 100000
set.seed(437)
sim <- rtms(nsim,tdf,m,s)

# strategy 1
initial <- 2000000

value <- rep(2000000, nsim)
for(i in 1:5){
  value <- value - 100000
  value <- value * (1+sim)
}

CalcVaRES <- function(r,alpha) {
  VaR <- quantile(r,1-alpha)
  ES <- mean(r[r<VaR])
  VaR_ES <- c(VaR,ES)
  names(VaR_ES) <- c("VaR","ES")
  return(VaR_ES)
}
alpha <- 0.95
VaR_ES <- CalcVaRES(value,alpha)

answer8 = round(VaR_ES[[1]])   ### replace NA with R code generating the answer 
cat("Answer Q8:", answer8,'\n')        
### question 8 end   ###

### question 9 start ###
data.table(year = seq(0,5),
           cash = c(initial, rep(0,5)),
           STRIP = c(0, -377204, rep(100000,4)),
           stock = c(0, -1522796, rep(0,4))
           )
STRIP.cost <- 377204
value2 <- rep(initial - 100000 - STRIP.cost, nsim)
sim < - rep(0, nsim)
nper <- 5
for (i in 1:nper) {
  sim <- sim+rtms(nsim,tdf,m,s)
  }
value2 <- value2 * (1 + sim)
VaR_ES2 <- CalcVaRES(value2,alpha)

answer9 = round(VaR_ES2[[1]])   ### replace NA with R code generating the answer 
cat("Answer Q9:", answer9,'\n')        
### question 9 end   ###



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
answers = rep(NA, 9)
for (i in 2:9){
  answers[i] = check_for_answer(i)
}
answers
