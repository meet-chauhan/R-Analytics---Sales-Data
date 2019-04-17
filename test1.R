

setwd("C:/Users/meetr/Desktop/Fall 2018/R Analytics INSY 5392/Meet")

data <- read.csv("C:/Users/meetr/Desktop/Fall 2018/R Analytics INSY 5392/Meet/dataset.csv")
rebate= data$rebate
adspent = data$ad.spent
xmas = data$xmas
y = data$sales

plot(y,adspent)
plot(rebate,y)
plot(xmas,y)

ad= lm(y~adspent)
re= lm(y~rebate)
x = lm(y~xmas)



functionPara = function(para)
{
  b0 = para[1]
  b1 = para[2]
  b2 = para[3]
  b3 = para[4]
  b4 = para[5]
  b5 = para[6]
  b6 = para[7]
  r1 = para[8]
  r2 = para[9]
  if(r1==0 && r2 != 0)
    fullModel = (b0 + b1 * rebate+ b2 * ((1-exp(-r2*adspent))/r2)
                 + b3*xmas + b4*rebate*adspent + b5*rebate*xmas + b6*adspent*xmas) 
  else
    if(r2==0 && r1 != 0)
      fullModel = (b0 + b1 * ((1-exp(-r1*rebate))/r1) + b2 * adspent
                   + b3*xmas + b4*rebate*adspent + b5*rebate*xmas + b6*adspent*xmas) 
  else
    if(r1==0 && r2==0)
      fullModel = (b0 + b1 * rebate + b2 * adspent + b3*xmas 
                   + b4*rebate*adspent + b5*rebate*xmas + b6*adspent*xmas)
  else
    fullModel = (b0 + b1 * ((1-exp(-r1*rebate))/r1) + b2 * ((1-exp(-r2*adspent))/r2)
                 + b3*xmas + b4*rebate*adspent + b5*rebate*xmas + b6*adspent*xmas)
  
  SSR = sum((fullModel - y)^2)
  
  return(SSR)
}



result = nlminb(start = c(0.05,0.05,0.05,-0.05,0.05,0.05,0.05,0.05,0.05),functionPara, control =list(iter.max=1000,eval.max = 2000, trace = 2))
result

obj = result$objective
l = log(obj)
n = nrow(data)
lg = -n/2*((l)+ log(2*pi)+1-log(n))
lg

###################################################

functionPara1 = function(para)
{
  
  b1 = para[1]
  b2 = para[2]
  b3 = para[3]
  b4 = para[4]
  b5 = para[5]
  b6 = para[6]
  r1 = para[7]
  r2 = para[8]
  
  if(r1==0 && r2 != 0)
    fullModel = (b1 * rebate+ b2 * ((1-exp(-r2*adspent))/r2)
                 + b3*xmas + b4*rebate*adspent + b5*rebate*xmas + b6*adspent*xmas) 
  else
    if(r2==0 && r1 != 0)
      fullModel = ( b1 * ((1-exp(-r1*rebate))/r1) + b2 * adspent
                    + b3*xmas + b4*rebate*adspent + b5*rebate*xmas + b6*adspent*xmas) 
       
  else
    if(r1==0 && r2==0)
      fullModel = (b1 * rebate + b2 * adspent+ b3*xmas 
                   + b4*rebate*adspent + b5*rebate*xmas + b6*adspent*xmas)
  else
    fullModel = (b1 * ((1-exp(-r1*rebate))/r1) + b2 * ((1-exp(-r2*adspent))/r2)
                 + b3*xmas + b4*rebate*adspent + b5*rebate*xmas + b6*adspent*xmas)
  
  SSR = sum((fullModel - y)^2)
  return(SSR)
}



result1 = nlminb(start = c(0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05),functionPara1, control =list(iter.max=1000,eval.max = 2000, trace = 2))
result1

obj1 = result1$objective
l1 = log(obj1)
n = nrow(data)
lr_bo = -n/2*((l1)+ log(2*pi)+1-log(n))
lr_bo

lrt_bo= 2*(lg-lr_bo)
#lrt_bo= 2*(lr_bo-lg)
lrt_bo


chi_test_bo = 1-pchisq(lrt_bo,df =1)
chi_test_bo


################################################################

functionPara2 = function(para)
{
  b0 = para[1]
  b2 = para[2]
  b3 = para[3]
  b4 = para[4]
  b5 = para[5]
  b6 = para[6]
  r2 = para[7]
  if(r2 != 0)
    fullModel = (b0 + b2 * ((1-exp(-r2*adspent))/r2)
                 + b3*xmas + b4*rebate*adspent + b5*rebate*xmas + b6*adspent*xmas) 
  else
    if(r2==0)
      fullModel = (b0 + b2 * adspent
                   + b3*xmas + b4*rebate*adspent + b5*rebate*xmas + b6*adspent*xmas) 
  
  
  SSR = sum((fullModel - y)^2)
  return(SSR)
}



result2 = nlminb(start = c(0.05,0.05,0.05,0.05,0.05,0.05,0.05),functionPara2, control =list(iter.max=1000,eval.max = 2000, trace = 2))
result2

obj2 = result2$objective
l2 = log(obj2)
n = nrow(data)
lr_b1 = -n/2*((l2)+ log(2*pi)+1-log(n))
lr_b1

lrt_b1= 2*(lg-lr_b1)
lrt_b1

chi_test_b1 = 1-pchisq(lrt_b1,df =2)
chi_test_b1

##########################################################


functionPara3 = function(para)
{
  b0 = para[1]
  b1 = para[2]
  b3 = para[3]
  b4 = para[4]
  b5 = para[5]
  b6 = para[6]
  r1 = para[7]
  
  if(r1==0 )
    fullModel = (b0 + b1 * rebate+ b3*xmas + b4*rebate*adspent + b5*rebate*xmas + b6*adspent*xmas) 
  
  
  else
    fullModel = (b0 + b1 * ((1-exp(-r1*rebate))/r1) + b3*xmas + b4*rebate*adspent + b5*rebate*xmas + b6*adspent*xmas)
  
  SSR = sum((fullModel - y)^2)
  return(SSR)
}



result3 = nlminb(start = c(0.05,0.05,0.05,0.05,0.05,0.05,0.05),functionPara3, control =list(iter.max=1000,eval.max = 2000, trace = 2))
result3

obj3 = result3$objective
l3 = log(obj3)
n = nrow(data)
l_b2 = -n/2*((l3)+ log(2*pi)+1-log(n))
l_b2

lrt_b2= 2*(lg-l_b2)
lrt_b2

chi_test_b2 = 1-pchisq(lrt_b2,df =2)
chi_test_b2


##########
chi_test_bo
chi_test_b1
chi_test_b2
##########

##################################################################

functionPara4 = function(para)
{
  b0 = para[1]
  b1 = para[2]
  b2 = para[3]
  b4 = para[4]
  b5 = para[5]
  b6 = para[6]
  r1 = para[7]
  r2 = para[8]
  
  if(r1==0 && r2 != 0)
    fullModel = (b0 + b1 * rebate+ b2 * ((1-exp(-r2*adspent))/r2)
                 + b4*rebate*adspent + b5*rebate*xmas + b6*adspent*xmas) 
  else
    if(r2==0 && r1 != 0)
      fullModel = (b0 + b1 * ((1-exp(-r1*rebate))/r1) + b2 * adspent
                   + b4*rebate*adspent + b5*rebate*xmas + b6*adspent*xmas) 
  else
    if(r1==0 && r2==0)
      fullModel = (b0 + b1 * rebate + b2 * adspent 
                   + b4*rebate*adspent + b5*rebate*xmas + b6*adspent*xmas)
  else
    fullModel = (b0 + b1 * ((1-exp(-r1*rebate))/r1) + b2 * ((1-exp(-r2*adspent))/r2)
                 + b4*rebate*adspent + b5*rebate*xmas + b6*adspent*xmas)
  
  SSR = sum((fullModel - y)^2)
  return(SSR)
}



result4 = nlminb(start = c(0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05),functionPara4, control =list(iter.max=1000,eval.max = 2000, trace = 2))
result4

obj4 = result4$objective
l4 = log(obj4)
n = nrow(data)
l_b3 = -n/2*((l4)+ log(2*pi)+1-log(n))
l_b3

#rt_b3= 2*(l_b3-lg)
lrt_b3= 2*(lg-l_b3)
lrt_b3

chi_test_b3 = 1-pchisq(lrt_b3,df =1)
chi_test_b3

#####################################################################

functionPara5 = function(para)
{
  b0 = para[1]
  b1 = para[2]
  b2 = para[3]
  b3 = para[4]
  b5 = para[5]
  b6 = para[6]
  r1 = para[7]
  r2 = para[8]
  if(r1==0 && r2 != 0)
    fullModel = (b0 + b1 * rebate+ b2 * ((1-exp(-r2*adspent))/r2)
                 + b3*xmas + b5*rebate*xmas + b6*adspent*xmas) 
  else
    if(r2==0 && r1 != 0)
      fullModel = (b0 + b1 * ((1-exp(-r1*rebate))/r1) + b2 * adspent
                   + b3*xmas + b5*rebate*xmas + b6*adspent*xmas) 
  else
    if(r1==0 && r2==0)
      fullModel = (b0 + b1 * rebate + b2 * adspent+ b3*xmas 
                   + b5*rebate*xmas + b6*adspent*xmas)
  else
    fullModel = (b0 + b1 * ((1-exp(-r1*rebate))/r1) + b2 * ((1-exp(-r2*adspent))/r2)
                 + b3*xmas + b5*rebate*xmas + b6*adspent*xmas)
  
  SSR = sum((fullModel - y)^2)
  return(SSR)
}



result5 = nlminb(start = c(0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05),functionPara5, control =list(iter.max=1000,eval.max = 2000, trace = 2))
result5

obj5 = result5$objective
l5 = log(obj5)
n = nrow(data)
l_b4 = -n/2*((l5)+ log(2*pi)+1-log(n))
l_b4

lrt_b4= 2*(lg-l_b4)
lrt_b4

chi_test_b4 = 1-pchisq(lrt_b4,df =1)
chi_test_b4

#######################################################

functionPara6 = function(para)
{
  b0 = para[1]
  b1 = para[2]
  b2 = para[3]
  b3 = para[4]
  b4 = para[5]
  b6 = para[6]
  r1 = para[7]
  r2 = para[8]
  if(r1==0 && r2 != 0)
    fullModel = (b0 + b1 * rebate+ b2 * ((1-exp(-r2*adspent))/r2)
                 + b3*xmas + b4*rebate*adspent + b6*adspent*xmas) 
  else
    if(r2==0 && r1 != 0)
      fullModel = (b0 + b1 * ((1-exp(-r1*rebate))/r1) + b2 * adspent
                   + b3*xmas + b4*rebate*adspent + b6*adspent*xmas) 
  else
    if(r1==0 && r2==0)
      fullModel = (b0 + b1 * rebate + b2 * adspent+ b3*xmas 
                   + b4*rebate*adspent + b6*adspent*xmas)
  else
    fullModel = (b0 + b1 * ((1-exp(-r1*rebate))/r1) + b2 * ((1-exp(-r2*adspent))/r2)
                 + b3*xmas + b4*rebate*adspent + b6*adspent*xmas)
  
  SSR = sum((fullModel - y)^2)
  return(SSR)
}



result6 = nlminb(start = c(0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05),functionPara6, control =list(iter.max=1000,eval.max = 2000, trace = 2))
result6

obj6 = result6$objective
l6 = log(obj6)
n = nrow(data)
l_b5 = -n/2*((l6)+ log(2*pi)+1-log(n))
l_b5

lrt_b5= 2*(lg-l_b5)
lrt_b5

chi_test_b5 = 1-pchisq(lrt_b5,df =1)
chi_test_b5

#############################################

functionPara6 = function(para)
{
  b0 = para[1]
  b1 = para[2]
  b2 = para[3]
  b3 = para[4]
  b4 = para[5]
  b5 = para[6]
  r1 = para[7]
  r2 = para[8]
  if(r1==0 && r2 != 0)
    fullModel = (b0 + b1 * rebate+ b2 * ((1-exp(-r2*adspent))/r2)
                 + b3*xmas + b4*rebate*adspent + b5*rebate*xmas) 
  else
    if(r2==0 && r1 != 0)
      fullModel = (b0 + b1 * ((1-exp(-r1*rebate))/r1) + b2 * adspent
                   + b3*xmas + b4*rebate*adspent + b5*rebate*xmas) 
  else
    if(r1==0 && r2==0)
      fullModel = (b0 + b1 * rebate + b2 * adspent+ b3*xmas 
                   + b4*rebate*adspent + b5*rebate*xmas)
  else
    fullModel = (b0 + b1 * ((1-exp(-r1*rebate))/r1) + b2 * ((1-exp(-r2*adspent))/r2)
                 + b3*xmas + b4*rebate*adspent + b5*rebate*xmas)
  
  SSR = sum((fullModel - y)^2)
  return(SSR)
}



result7 = nlminb(start = c(0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05),functionPara6, control =list(iter.max=1000,eval.max = 2000))
result7

obj7 = result7$objective
l7 = log(obj7)
n = nrow(data)
l_b6 = -n/2*((l7)+ log(2*pi)+1-log(n))
l_b6

lrt_b6= 2*(lg-l_b6)
lrt_b6

chi_test_b6 = 1-pchisq(lrt_b6,df =1)
chi_test_b6

############################

functionPara7 = function(para)
{
  b0 = para[1]
  b1 = para[2]
  b2 = para[3]
  b3 = para[4]
  b4 = para[5]
  b5 = para[6]
  b6 = para[7]
  r2 = para[8]
  if(r2 != 0)
    fullModel = (b0 + b1 * rebate+ b2 * ((1-exp(-r2*adspent))/r2)
                 + b3*xmas + b4*rebate*adspent + b5*rebate*xmas + b6*adspent*xmas) 
  else
    if(r2==0)
      fullModel = (b0 + b1 * (rebate) + b2 * adspent
                   + b3*xmas + b4*rebate*adspent + b5*rebate*xmas + b6*adspent*xmas) 
  
  
  SSR = sum((fullModel - y)^2)
  return(SSR)
}



result8 = nlminb(start = c(0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05),functionPara7, control =list(iter.max=1000,eval.max = 2000, trace = 2))
result8

obj8 = result8$objective
l8 = log(obj8)
n = nrow(data)
l_r1 = -n/2*((l8)+ log(2*pi)+1-log(n))
l_r1

lrt_r1= 2*(lg-l_r1)
lrt_r1

chi_test_r1 = 1-pchisq(lrt_r1,df =1)
chi_test_r1

###############################################

functionPara8 = function(para)
{
  b0 = para[1]
  b1 = para[2]
  b2 = para[3]
  b3 = para[4]
  b4 = para[5]
  b5 = para[6]
  b6 = para[7]
  r1 = para[8]
  
  if(r1==0)
    fullModel = (b0 + b1 * rebate+ b2 * (adspent)
                 + b3*xmas + b4*rebate*adspent + b5*rebate*xmas + b6*adspent*xmas) 
  else
    if(r1 != 0)
      fullModel = (b0 + b1 * ((1-exp(-r1*rebate))/r1) + b2 * adspent
                   + b3*xmas + b4*rebate*adspent + b5*rebate*xmas + b6*adspent*xmas) 
  
  
  SSR = sum((fullModel - y)^2)
  return(SSR)
}



result9 = nlminb(start = c(0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05),functionPara8, control =list(iter.max=1000,eval.max = 2000, trace = 2))
result9

obj9 = result9$objective
l9 = log(obj9)
n = nrow(data)
l_r2 = -n/2*((l9)+ log(2*pi)+1-log(n))
l_r2

lrt_r2= 2*(lg-l_r2)
lrt_r2

chi_test_r2 = 1-pchisq(lrt_r2,df =1)
chi_test_r2


##################################
functionPara9 = function(para)
{
  b0 = para[1]
  b1 = para[2]
  b2 = para[3]
  b3 = para[4]
  b5 = para[5]
  b6 = para[6]
  r1 = para[7]
  
  if(r1==0)
    finalmodel <<- (b0 + b1 * rebate+ b2 * (adspent)
                 + b3*xmas + b5*rebate*xmas + b6*adspent*xmas) 
  else
    if(r1 != 0)
      finalmodel<<- (b0 + b1 * ((1-exp(-r1*rebate))/r1) + b2 * adspent
                   + b3*xmas + b5*rebate*xmas + b6*adspent*xmas) 
  
  
  SSR = sum((y-finalmodel)^2)
  
  return(SSR)
}



result10 = nlminb(start = c(0.05,0.05,0.05,0.05,0.05,0.05,0.05),functionPara9, control =list(iter.max=1000,eval.max = 2000, trace = 2))
result10

obj10 = result10$objective
l_10 = log(obj10)
n = nrow(data)
l_final = -n/2*((l_10)+ log(2*pi)+1-log(n))
l_final

###############################################

functionPara11 = function(para)
{
  b0 = para[1]
  b1 = para[2]
  b2 = para[3]
  b3 = para[4]
  b4 = para[5]
  b5 = para[6]
  b6 = para[7]
  r1 = para[8]
  r2 = para[9]
  if(r1==0 && r2 != 0)
    fullModel11 <<- (b0 + b1 * rebate+ b2 * ((1-exp(-r2*adspent))/r2)
                 + b3*xmas + b4*rebate*adspent + b5*rebate*xmas + b6*adspent*xmas) 
  else
    if(r2==0 && r1 != 0)
      fullModel11 <<- (b0 + b1 * ((1-exp(-r1*rebate))/r1) + b2 * adspent
                   + b3*xmas + b4*rebate*adspent + b5*rebate*xmas + b6*adspent*xmas) 
  else
    if(r1==0 && r2==0)
      fullModel11 <<- (b0 + b1 * rebate + b2 * adspent + b3*xmas 
                   + b4*rebate*adspent + b5*rebate*xmas + b6*adspent*xmas)
  else
    fullModel11 <<- (b0 + b1 * ((1-exp(-r1*rebate))/r1) + b2 * ((1-exp(-r2*adspent))/r2)
                 + b3*xmas + b4*rebate*adspent + b5*rebate*xmas + b6*adspent*xmas)
  
  SSR = sum((fullModel11 - y)^2)
  
  return(SSR)
}


functionPara11(para)
#result11 = nlminb(start = c(0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05),functionPara11, control =list(iter.max=1000,eval.max = 2000, trace = 2))
#result11

final= finalmodel
full = fullModel11
#########  RESULTS #####################

chi_test_bo
chi_test_b1
chi_test_b2
chi_test_b3
chi_test_b4
chi_test_b5
chi_test_b6
chi_test_r2
chi_test_r1


###########################################
a = adspent*rebate
plot(y, adspent, main="Scatterplot Example",   xlab="Sales ", ylab="ADSpent ", pch=19)
abline(lm(y~adspent), col="red") # regression line (y~x) 
lines(lowess(y,adspent), col="blue") # lowess line (x,y)

plot(a,y, main="Scatterplot Example",   xlab="Sales ", ylab="rebate", pch=19)
abline(lm(y~a), col="red") # regression line (y~x) 
lines(lowess(a,y), col="blue") # lowess line (x,y)




#install.packages("interplot")
library(interplot)

m_cyl <- lm(y ~ adspent * rebate)
summary(m_cyl)


interplot(m = m_cyl, var1 = "adspent", var2 = "rebate")

m_cyl <- lm(y ~ adspent * xmas)
summary(m_cyl)
interplot(m = m_cyl, var1 = "adspent", var2 = "xmas")
###################
#https://cran.r-project.org/web/packages/interplot/vignettes/interplot-vignette.html
###################

#install.packages('corrplot')
res <- cor(data)
res<-round(res, 4)
res
#install.packages('corrplot')
library(corrplot)
corrplot(res, type = "upper", order = "hclust")