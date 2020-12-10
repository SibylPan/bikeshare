library(corrplot)
library(ggplot2) 
library(knitr) 
library(kableExtra)
library(GGally) 
library(MASS) 
library(class) 
library(caret) 
library(purrr) 
library(tidyr) 
library(reshape) 
library(naniar) 
library(dplyr)
library(devtools)
library(tidyverse)
library(broom)
library(ggthemes)
library(glmnet)
library(leaps)

setwd("C:/Users/SPan/Downloads/stat508/final_project/data/bike-sharing-demand")

train <- read.csv("train.csv")
test <- read.csv("test.csv")

summary(test)
summary(train)

# combine Train and Test Data set
test$registered=0
test$casual=0
test$count=0
data=rbind(train,test)

# check missing values: no missing values
table(is.na(data))
# str(data)

# summarize ride counts
sumstat <- data %>%
  select(registered,casual,count)%>%
  # Find the mean, st. dev., min, and max for each variable
  summarise_each(funs(mean, sd, median,min, max)) %>%
  # Move summary stats to columns
  gather(key, value, everything()) %>%
  separate(key, into = c("variable", "stat"), sep = "_") %>%
  spread(stat, value) %>%
  # Set order of summary statistics
  select(variable, mean, median, sd, min, max) %>%
  # Round all numeric variables to one decimal point
  mutate_each(funs(round(., 2)), -variable)

write.csv(sumstat,"sumstat.csv",row.names = FALSE)

# convert to factor variable
data$season=factor(data$season,levels=c(1,2,3,4),labels = c("Spring","Summer","Fall","Winter"))
data$weather=factor(data$weather,levels=c(1,2,3,4),labels = c("Clear","Mist","Light Snow and Rain","Heavy Rain"))
data$holiday=factor(data$holiday,levels=c(0,1),labels = c("Not holiday","Holiday"))
data$workingday=factor(data$workingday,levels=c(0,1),labels = c("Not Workday","Workday"))

train$season=factor(train$season,levels=c(1,2,3,4),labels = c("Spring","Summer","Fall","Winter"))
train$weather=factor(train$weather,levels=c(1,2,3,4),labels = c("Clear","Mist","Light Snow and Rain","Heavy Rain"))
train$holiday=factor(train$holiday,levels=c(0,1),labels = c("Not holiday","Holiday"))
train$workingday=factor(train$workingday,levels=c(0,1),labels = c("Not Workday","Workday"))

test$season=factor(test$season,levels=c(1,2,3,4),labels = c("Spring","Summer","Fall","Winter"))
test$weather=factor(test$weather,levels=c(1,2,3,4),labels = c("Clear","Mist","Light Snow and Rain","Heavy Rain"))
test$holiday=factor(test$holiday,levels=c(0,1),labels = c("Not holiday","Holiday"))
test$workingday=factor(test$workingday,levels=c(0,1),labels = c("Not Workday","Workday"))

# histograms: factor
data%>%
  select(season, holiday, workingday,weather) %>%
  keep(is.factor) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.85),legend.position="none")

# histograms: numeric
data %>%
  select(atemp,humidity,temp,windspeed) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(bins=(nrow(data))^(1/4)) +
  theme(legend.position="none")

# histograms: outcome variable in training set
train %>%
  select(registered,casual,count) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(bins=(nrow(data))^(1/3)) +
  theme(legend.position="none")

# time trend
train$hour <- as.factor(substring(train$datetime, 12, 13))
test$hour <- as.factor(substring(test$datetime, 12, 13))
data$hour <- as.factor(substring(data$datetime, 12, 13))

train$day <- as.factor(weekdays(as.Date(train$datetime)))
train$day  <- factor(train$day , levels= c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
train<-train[order(train$day), ]
test$day <- as.factor(weekdays(as.Date(test$datetime)))
data$day <- as.factor(weekdays(as.Date(data$datetime)))

train$year <- as.factor(format(as.Date(as.Date(train$datetime), format="%d/%m/%Y"),"%Y"))
test$year <- as.factor(format(as.Date(as.Date(test$datetime), format="%d/%m/%Y"),"%Y"))
data$year <- as.factor(format(as.Date(as.Date(data$datetime), format="%d/%m/%Y"),"%Y"))

train$month <- as.factor(format(as.Date(as.Date(train$datetime), format="%d/%m/%Y"),"%m"))
test$month<- as.factor(format(as.Date(as.Date(test$datetime), format="%d/%m/%Y"),"%m"))
data$month <- as.factor(format(as.Date(as.Date(data$datetime), format="%d/%m/%Y"),"%m"))

train$date <- as.Date(train$datetime)
test$date <- as.Date(test$datetime)
data$date <- as.Date(data$datetime)

# overall time trend
counts_by_date <- aggregate(list(train["registered"], train["casual"], train["count"]), by=train["date"], sum)
time_plot <- ggplot(counts_by_date, aes(x=date))+
  geom_line(aes(y = registered, colour = "Registered Daily Rentals"))+
  geom_line(aes(y = casual, colour = "Casual Daily Rentals"))+
  geom_line(aes(y = count, colour = "Count Daily Rentals"))+
  geom_smooth(aes(y = registered, colour = "Registered Smooth"))+
  geom_smooth(aes(y = casual, colour = "Casual Smooth"))+
  geom_smooth(aes(y = count, colour = "Count Smooth"))+
  scale_colour_manual(values=c("Registered Daily Rentals"="grey68", 
                               "Registered Smooth"="red", 
                               "Casual Daily Rentals"="grey68", 
                               "Casual Smooth"="blue", 
                               "Count Daily Rentals"="grey68", 
                               "Count Smooth"="yellow"))
time_plot + labs(y="Daily Rentals Initiated", title="Demand Trends Over Time") + theme(legend.position = "bottom")

# by year
par(mfrow=c(1, 2))
boxplot(train$registered~train$year,xlab="Year", ylab="Registered Rentals by Year")
boxplot(train$casual~train$year,xlab="Year", ylab="Casual Rentals by Year")

# by month
par(mfrow=c(2, 1))
boxplot(train$registered~train$month,xlab="Month", ylab="Registered Rentals by Month")
boxplot(train$casual~train$month,xlab="Month", ylab="Casual Rentals by Month")

# by day of the week
par(mfrow=c(1,2))
boxplot(casual~day, data=train, xlab="Day", main = "Casual Rentals by Day", horizontal = T)
boxplot(registered~day, data=train, xlab="Day", main = "Registered Rentals by Day", horizontal = T)

# by hour
par(mfrow=c(2, 1))
boxplot(registered~hour, data=train, xlab="Hour",main = "Registered Rentals by Hour")
boxplot(casual~hour, data=train, xlab="Hour",main = "Casual Rentals by Hour")
    
# correlation plot
num_cols <- unlist(lapply(train, is.numeric))
train_num <- train[ , num_cols]                     
corr<-cor(train_num)

# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(train_num)

par(mfrow=c(1,1))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corr, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE )


train_orig=train
train$registered=train$registered+1
train$casual=train$casual+1

train.r=train[sample(nrow(train), 5000), ]
drop <- c("casual","count")
train.r= train.r[,!(names(train.r) %in% drop)]

train.c=train[sample(nrow(train), 5000), ]
drop <- c("registered","count")
train.c= train.c[,!(names(train.c) %in% drop)]

# transform registered
var <- c("temp","atemp","humidity","windspeed","season","holiday","workingday","weather")

### No Transformation
train.r %>% 
  gather(key = "Variable", value = "Value", var) %>%
  group_by(Variable) %>%
  mutate(
    Value = residuals(lm(train.r$registered ~ Value)),
    Variable = paste0("residual of\nregistered ~ ", Variable)) %>%
  ggRespEval()
### Log
train.r %>% 
  gather(key = "Variable", value = "Value", var) %>%
  group_by(Variable) %>%
  mutate(
    Value = residuals(lm(log(train.r$registered) ~ Value)),
    Variable = paste0("residual of\nlog(registered) ~ ", Variable)) %>%
  ggRespEval()
### sqrt
train.r %>% 
  gather(key = "Variable", value = "Value", var) %>%
  group_by(Variable) %>%
  mutate(
    Value = residuals(lm(sqrt(train.r$registered) ~ Value)),
    Variable = paste0("residual of\nsqrt(registered) ~ ", Variable)) %>%
  ggRespEval()

### 1/registered 
train.r  %>% 
  gather(key = "Variable", value = "Value", var) %>%
  group_by(Variable) %>%
  mutate(
    Value = residuals(lm(1/train.r$registered ~ Value)),
    Variable = paste0("residual of\n1/registered ~ ", Variable)) %>%
  ggRespEval()

# transform casual 
### No Transformation
train.c %>% 
  # select (-drop.cols))%>% 
  gather(key = "Variable", value = "Value", var) %>%
  group_by(Variable) %>%
  mutate(
    Value = residuals(lm(train.c$casual ~ Value)),
    Variable = paste0("residual of\ncasual ~ ", Variable)) %>%
  ggRespEval()
### Log
train.c %>% 
  gather(key = "Variable", value = "Value", var) %>%
  group_by(Variable) %>%
  mutate(
    Value = residuals(lm(log(train.c$casual) ~ Value)),
    Variable = paste0("residual of\nlog(casual) ~ ", Variable)) %>%
  ggRespEval()
### sqrt
train.c %>% 
  gather(key = "Variable", value = "Value", var) %>%
  group_by(Variable) %>%
  mutate(
    Value = residuals(lm(sqrt(train.c$casual) ~ Value)),
    Variable = paste0("residual of\nsqrt(casual) ~ ", Variable)) %>%
  ggRespEval()

### 1/casual 
train.c  %>% 
  gather(key = "Variable", value = "Value", var) %>%
  group_by(Variable) %>%
  mutate(
    Value = residuals(lm(1/train.c$casual ~ Value)),
    Variable = paste0("residual of\n1/casual ~ ", Variable)) %>%
  ggRespEval()


train$rtreg <- sqrt(train$registered)
train$rtcas <- sqrt(train$casual)

data$rtreg <- sqrt(data$registered)
data$rtcas <- sqrt(data$casual)

test$rtreg <- 0
test$rtcas <- 0

train$hour=as.integer(train$hour) # convert hour to integer
test$hour=as.integer(test$hour) # modifying in both train and test data set
data$hour=as.integer(data$hour)

d=rpart(rtreg~hour,data=train)
fancyRpartPlot(d)

#binning hours
library(rpart)
library(rattle) 
library(rpart.plot)
library(RColorBrewer)
reg.bin=rpart(registered~hour,data=train)
fancyRpartPlot(reg.bin, main="registered", palettes=c("Greys"),type=2)
cas.bin=rpart(casual~hour,data=train)
fancyRpartPlot(cas.bin, main="casual", palettes=c("Greys"),type=2)
cou.bin=rpart(count~hour,data=train)
fancyRpartPlot(cou.bin, main="count", palettes=c("Greys"),type=2)

###hour bins for registered
data$hr_reg=0
data$hr_reg[data$hour<7]=1
data$hr_reg[data$hour>=21]=2
data$hr_reg[data$hour>8 & data$hour<17]=3
data$hr_reg[data$hour==7]=4
data$hr_reg[data$hour==8]=5
data$hr_reg[data$hour>=19 & data$hour<21]=6
data$hr_reg[data$hour>=17 & data$hour<19]=7
train$hr_reg=0
train$hr_reg[train$hour<7]=1
train$hr_reg[train$hour>=21]=2
train$hr_reg[train$hour>8 & train$hour<17]=3
train$hr_reg[train$hour==7]=4
train$hr_reg[train$hour==8]=5
train$hr_reg[train$hour>=19 & train$hour<21]=6
train$hr_reg[train$hour>=17 & train$hour<19]=7
test$hr_reg=0
test$hr_reg[data$hour<7]=1
test$hr_reg[data$hour>=21]=2
test$hr_reg[data$hour>8 & data$hour<17]=3
test$hr_reg[data$hour==7]=4
test$hr_reg[data$hour==8]=5
test$hr_reg[data$hour>=19 & data$hour<21]=6
test$hr_reg[data$hour>=17 & data$hour<19]=7

#hour bins for casual
data$hr_cas=0
data$hr_cas[data$hour<=7]=1
data$hr_cas[data$hour==8 | data$hour==9]=2
data$hr_cas[data$hour>=20]=3
data$hr_cas[data$hour>9 & data$hour<20]=4
train$hr_cas=0
train$hr_cas[train$hour<=7]=1
train$hr_cas[train$hour==8 | train$hour==9]=2
train$hr_cas[train$hour>=20]=3
train$hr_cas[train$hour>9 & train$hour<20]=4
test$hr_cas=0
test$hr_cas[data$hour<=7]=1
test$hr_cas[data$hour==8 | data$hour==9]=2
test$hr_cas[data$hour>=20]=3
test$hr_cas[data$hour>9 & data$hour<20]=4

#hour bins for count
data$hr_cou=0
data$hr_cou[data$hour<7]=1
data$hr_cou[data$hour>=21]=2
data$hr_cou[data$hour>8 & data$hour<16]=3
data$hr_cou[data$hour==7]=4
data$hr_cou[data$hour==8]=5
data$hr_cou[data$hour>=19 & data$hour<21]=6
data$hr_cou[data$hour==16]=7
data$hr_cou[data$hour>=17 & data$hour<19]=8
train$hr_cou=0
train$hr_cou[train$hour<7]=1
train$hr_cou[train$hour>=21]=2
train$hr_cou[train$hour>8 & train$hour<16]=3
train$hr_cou[train$hour==7]=4
train$hr_cou[train$hour==8]=5
train$hr_cou[train$hour>=19 & train$hour<21]=6
train$hr_cou[train$hour==16]=7
train$hr_cou[train$hour>=17 & train$hour<19]=8
test$hr_cou=0
test$hr_cou[data$hour<7]=1
test$hr_cou[data$hour>=21]=2
test$hr_cou[data$hour>8 & data$hour<16]=3
test$hr_cou[data$hour==7]=4
test$hr_cou[data$hour==8]=5
test$hr_cou[data$hour>=19 & data$hour<21]=6
test$hr_cou[data$hour==16]=7
test$hr_cou[data$hour>=17 & data$hour<19]=8

#split the train variable into its own train & test sets
set.seed(1)
train.index <- createDataPartition(paste(train$holiday,train$season,train$weather,train$workingday), p = 0.8, list = FALSE)
newtrain <- train[train.index,]
newtest <- train[-train.index,]

########################
#Ridge for rtcas
x=model.matrix(rtcas~datetime+season+holiday+workingday+weather+temp+atemp+humidity+windspeed+hr_cas+day+year+month+date,train)[,-1]
y=train$rtcas
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x[train.index,],y[train.index],alpha=0,lambda=grid)
cv.out=cv.glmnet(x[train.index,],y[train.index],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ytest=newtest$rtcas
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[-train.index,])
mean((ridge.pred-ytest)^2)

#Ridge for rtreg
x2=model.matrix(rtreg~datetime+season+holiday+workingday+weather+temp+atemp+humidity+windspeed+hr_reg+day+year+month+date,train)[,-1]
y2=train$rtreg
ridge.mod2=glmnet(x2[train.index,],y2[train.index],alpha=0,lambda=grid)
cv.out2=cv.glmnet(x2[train.index,],y2[train.index],alpha=0)
plot(cv.out2)
bestlam2=cv.out2$lambda.min
bestlam2
ytest2=newtest$rtreg
ridge.pred2=predict(ridge.mod2,s=bestlam2,newx=x2[-train.index,])
mean((ridge.pred2-ytest2)^2)

#Lasso for rtcas
lasso.mod=glmnet(x[train.index,],y[train.index],alpha=1,lambda=grid)
plot(lasso.mod)
cv.out3=cv.glmnet(x[train.index,],y[train.index],alpha=1)
plot(cv.out3)
bestlam3=cv.out3$lambda.min
bestlam3
lasso.pred=predict(lasso.mod,s=bestlam3,newx=x[-train.index,])
mean((lasso.pred-ytest)^2)

#Lasso for rtreg
lasso.mod2=glmnet(x2[train.index,],y2[train.index],alpha=1,lambda=grid)
plot(lasso.mod2)
cv.out4=cv.glmnet(x2[train.index,],y2[train.index],alpha=1)
plot(cv.out4)
bestlam4=cv.out4$lambda.min
bestlam4
lasso.pred2=predict(lasso.mod2,s=bestlam4,newx=x2[-train.index,])
mean((lasso.pred2-ytest2)^2)

########################
#Best Subsets for rtcas:
regfit.best=regsubsets(rtcas~datetime+season+holiday+workingday+weather+temp+atemp+humidity+windspeed+hr_cas+day+year+month+date,data=newtrain,nvmax=14,really.big=TRUE)
test.mat=model.matrix(rtcas~datetime+season+holiday+workingday+weather+temp+atemp+humidity+windspeed+hr_cas+day+year+month+date,data=newtest)
val.errors=rep(NA,14)
for(i in 1:14){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((newtest$rtcas-pred)^2) }
val.errors
a<-which.min(val.errors)
plot(val.errors,type='b')
points(a, val.errors[a], col="red", cex=2, pch=20)

#Best Subsets for rtreg:
regfit.best2=regsubsets(rtreg~datetime+season+holiday+workingday+weather+temp+atemp+humidity+windspeed+hr_reg+day+year+month+date,data=newtrain,nvmax=14,really.big=TRUE)
test.mat2=model.matrix(rtreg~datetime+season+holiday+workingday+weather+temp+atemp+humidity+windspeed+hr_reg+day+year+month+date,data=newtest)
val.errors2=rep(NA,14)
for(i in 1:14){
  coefi2=coef(regfit.best2,id=i)
  pred2=test.mat[,names(coefi2)]%*%coefi2
  val.errors2[i]=mean((newtest$rtreg-pred)^2) }
val.errors2
b<-which.min(val.errors2)
plot(val.errors2,type='b')
points(b, val.errors[b], col="red", cex=2, pch=20)

########################
#Stepwise for rtcas
#forward
regfit.fwdcas=regsubsets(rtcas~datetime+season+holiday+workingday+weather+temp+atemp+humidity+windspeed+hr_cas+day+year+month+date,data=newtrain,nvmax=14,method="forward",really.big=TRUE)
test.matfwdcas=model.matrix(rtcas~datetime+season+holiday+workingday+weather+temp+atemp+humidity+windspeed+hr_cas+day+year+month+date,data=newtest,method="forward")
val.errors3=rep(NA,14)
for(i in 1:14){
  coefi3=coef(regfit.fwdcas,id=i)
  pred3=test.matfwdcas[,names(coefi3)]%*%coefi3
  val.errors3[i]=mean((newtest$rtcas-pred)^2) }
val.errors3
c<-which.min(val.errors3)
plot(val.errors3,type='b')
points(c, val.errors3[c], col="red", cex=2, pch=20)

#backward
regfit.bwdcas=regsubsets(rtcas~datetime+season+holiday+workingday+weather+temp+atemp+humidity+windspeed+hr_cas+day+year+month+date,data=newtrain,nvmax=14,method="backward",really.big=TRUE)
test.matbwdcas=model.matrix(rtcas~datetime+season+holiday+workingday+weather+temp+atemp+humidity+windspeed+hr_cas+day+year+month+date,data=newtest,method="backward")
val.errors4=rep(NA,14)
for(i in 1:14){
  coefi4=coef(regfit.bwdcas,id=i)
  pred=test.matbwdcas[,names(coefi4)]%*%coefi4
  val.errors4[i]=mean((newtest$rtcas-pred)^2) }
val.errors4
d<-which.min(val.errors4)
plot(val.errors4,type='b')
points(d, val.errors4[d], col="red", cex=2, pch=20)

#Stepwise for rtreg:
#forward
regfit.fwdreg=regsubsets(rtreg~datetime+season+holiday+workingday+weather+temp+atemp+humidity+windspeed+hr_reg+day+year+month+date,data=newtrain,nvmax=14,method="forward",really.big=TRUE)
test.matfwdreg=model.matrix(rtreg~datetime+season+holiday+workingday+weather+temp+atemp+humidity+windspeed+hr_reg+day+year+month+date,data=newtest,method="forward")
val.errors5=rep(NA,14)
for(i in 1:14){
  coefi5=coef(regfit.fwdreg,id=i)
  pred5=test.matfwdreg[,names(coefi5)]%*%coefi5
  val.errors5[i]=mean((newtest$rtreg-pred)^2) }
val.errors5
e<-which.min(val.errors5)
plot(val.errors5,type='b')
points(e, val.errors3[e], col="red", cex=2, pch=20)

#backward
regfit.bwdreg=regsubsets(rtcas~datetime+season+holiday+workingday+weather+temp+atemp+humidity+windspeed+hr_reg+day+year+month+date,data=newtrain,nvmax=14,method="backward",really.big=TRUE)
test.matbwdreg=model.matrix(rtcas~datetime+season+holiday+workingday+weather+temp+atemp+humidity+windspeed+hr_reg+day+year+month+date,data=newtest,method="backward")
val.errors6=rep(NA,14)
for(i in 1:14){
  coefi6=coef(regfit.bwdreg,id=i)
  pred=test.matbwdreg[,names(coefi6)]%*%coefi6
  val.errors6[i]=mean((newtest$rtreg-pred)^2) }
val.errors6
f<-which.min(val.errors6)
plot(val.errors6,type='b')
points(f, val.errors4[f], col="red", cex=2, pch=20)


####################
#Random forest
####################
library(randomForest)
newtrain <- newtrain[,-1]
newtest <- newtest[,-1]

#casual
oob.err.cas <- matrix(, nrow = 4, ncol = 8)
test.err.cas <- matrix(, nrow = 4, ncol = 8)
formula.cas <- rtcas~season+holiday+workingday+weather+temp+atemp+humidity+windspeed+day+year+hour
set.seed(1)
tree=c(250,300,350,400)
try=c(3,4,5,6,7,8,9,10)
for(i in 1:4){
  for(j in 1:8) {
    ntree=tree[i]
    mtry=try[j]
    fit=randomForest(formula.cas,data=newtrain,mtry=mtry,ntree=ntree)
    oob.err.cas[i,j]=fit$mse[ntree]
    pred=predict(fit,newdata=newtest)
    test.err.cas[i,j]=with(newtest,mean((rtcas-pred)^2))
  }
}
par(mfrow=c(1,2))
matplot(t(test.err.cas),type="l",main = "rtcas", xlab='mtry', xaxt="n",ylab='Mean Squared Error (test)', col=1:4)
legend('topright', inset=.05, legend=c("250","300","350","400"), 
       pch=19, horiz=TRUE, col=1:4, title="ntree")
axis(1, at=1:8, labels=seq(3,10,by=1))
##minimum err occurred at ntree=300 ,mtry=5,test error=1.077

set.seed(1)
rf.cas=randomForest(formula.cas,data=train[,-1],mtry=5,importance=TRUE,ntree=300)
rf.cas
importance(rf.cas)
varImpPlot(rf.cas)
pred.cas=predict(rf.cas,newdata=test[,-1])

#registered
oob.err.reg<- matrix(, nrow = 4, ncol = 8)
test.err.reg<- matrix(, nrow = 4, ncol = 8)
formula.reg <- rtreg~season+holiday+workingday+weather+temp+atemp+humidity+windspeed+day+year+hour
set.seed(1)
tree=c(250,300,350,400)
try=c(3,4,5,6,7,8,9,10)
for(i in 1:4){
  for(j in 1:8) {
    ntree=tree[i]
    mtry=try[j]
    fit=randomForest(formula.reg,data=newtrain,mtry=mtry,ntree=ntree)
    oob.err.reg[i,j]=fit$mse[mtry]
    pred=predict(fit,newdata=newtest)
    test.err.reg[i,j]=with(newtest,mean((rtreg-pred)^2))
  }
}

matplot(t(test.err.reg),type="l",main = "rtreg", xlab='mtry', xaxt="n",ylab='Mean Squared Error (test)', col=1:4)
legend('topright', inset=.05, legend=c("250","300","350","400"), 
       pch=19, horiz=TRUE, col=1:4, title="ntree")
axis(1, at=1:8, labels=seq(3,10,by=1))
##minimum err occurred at ntree=400, mtry=7,test error=1.534

set.seed(1)
rf.reg=randomForest(formula.reg,data=train[,-1],mtry=7,importance=TRUE,ntree=400)
rf.reg
importance(rf.reg)
varImpPlot(rf.reg)
pred.reg=predict(rf.reg,newdata=test[,-1])

#final prediction
pred.rf.count=(pred.cas)^2+(pred.reg)^2

########################
#boosting
########################
library(gbm)
#casual
set.seed(1)
shr=c(0.01,0.05,0.1)
depth=c(1,2,3,4,5,6)
n.trees=seq(from=100,to=10000,by=100)
for (i in 1:3) {
  for (j in 1:6) {
  shrinkage=shr[i]
  interaction.depth=depth[j]
  boost.cas=gbm(formula.cas,data=newtrain,distribution="gaussian",
                n.trees=10000,interaction.depth=interaction.depth,shrinkage=shrinkage,verbose=F)
  predmat=predict(boost.cas,newdata=newtest,n.trees=n.trees)
  berr=with(newtest,apply((predmat-rtcas)^2,2,mean))
  assign(paste("predmat.cas", i,j, sep = ""), predmat) 
  assign(paste("berr.cas", i,j,sep = ""), berr)
  }
}

par(mfrow=c(1,3))
matplot(n.trees,cbind(berr.cas11,berr.cas11,berr.cas13,berr.cas14,berr.cas15,berr.cas16),type="l",pch=19,
        col=c("red","blue","green","orange","purple","navy"),ylab="Mean Squared Error",xlab="# Trees",main="Boosting Test Error (shrinkage=0.01)")
legend("topright",title="Depth",legend=c("1","2","3","4","5","6"),pch=19,inset=0.02,
       col=c("red","blue","green","orange","purple","navy"))
abline(h=min(test.err.cas),col="black")

matplot(n.trees,cbind(berr.cas21,berr.cas21,berr.cas23,berr.cas24,berr.cas25,berr.cas26),type="l",pch=19,
        col=c("red","blue","green","orange","purple","navy"),ylab="Mean Squared Error",xlab="# Trees",main="Boosting Test Error (shrinkage=0.05)")
legend("topright",title="Depth",legend=c("1","2","3","4","5","6"),pch=19,inset=0.02,
       col=c("red","blue","green","orange","purple","navy"))
abline(h=min(test.err.cas),col="black")

matplot(n.trees,cbind(berr.cas31,berr.cas31,berr.cas33,berr.cas34,berr.cas35,berr.cas36),type="l",pch=19,
        col=c("red","blue","green","orange","purple","navy"),ylab="Mean Squared Error",xlab="# Trees",main="Boosting Test Error (shrinkage=0.1)")
legend("topright",title="Depth",legend=c("1","2","3","4","5","6"),pch=19,inset=0.02,
       col=c("red","blue","green","orange","purple","navy"))
abline(h=min(test.err.cas),col="black")

par(mfrow=c(1,1))
matplot(n.trees,cbind(berr.cas16,berr.cas26,berr.cas36),type="l",pch=19,
        col=c("red","blue","green"),ylab="Mean Squared Error",xlab="# Trees",main="Boosting Test Error (depth=6)")
legend("topright",title="Shrinkage",legend=c("0.01","0.05","0.1"),pch=19,inset=0.02,
       col=c("red","blue","green"),cex=0.8)
abline(h=min(test.err.cas),col="black")

#choose shrinkage=0.05, n.tree=5000, depth=6
boost.cas=gbm(formula.cas,data=train[,-1],distribution="gaussian",
              n.trees=5000,interaction.depth=6,shrinkage=0.05,verbose=F)
pred.boost.cas=predict(boost.cas,newdata=test[,-1],n.trees=5000)

#registered
set.seed(1)
shr=c(0.01,0.05,0.1)
n.trees=seq(from=100,to=10000,by=100)
depth=c(1,2,3,4,5,6)
for (i in 1:3) {
  for (j in 1:6) {
  shrinkage=shr[i]
  interaction.depth=depth[j]
  boost.reg=gbm(formula.reg,data=newtrain,distribution="gaussian",
                n.trees=10000,interaction.depth=interaction.depth,shrinkage=shrinkage,verbose=F)
  predmat=predict(boost.reg,newdata=newtest,n.trees=n.trees)
  berr=with(newtest,apply((predmat-rtreg)^2,2,mean))
  assign(paste("predmat.reg", i,j, sep = ""), predmat) 
  assign(paste("berr.reg", i,j, sep = ""), berr)
  }
}

par(mfrow=c(1,3))
matplot(n.trees,cbind(berr.reg11,berr.reg11,berr.reg13,berr.reg14,berr.reg15,berr.reg16),type="l",pch=19,
        col=c("red","blue","green","orange","purple","navy"),ylab="Mean Squared Error",xlab="# Trees",main="Boosting Test Error (shrinkage=0.01)")
legend("topright",title="Depth",legend=c("1","2","3","4","5","6"),pch=19,inset=0.02,
       col=c("red","blue","green","orange","purple","navy"))
abline(h=min(test.err.reg),col="black")

matplot(n.trees,cbind(berr.reg21,berr.reg21,berr.reg23,berr.reg24,berr.reg25,berr.reg26),type="l",pch=19,
        col=c("red","blue","green","orange","purple","navy"),ylab="Mean Squared Error",xlab="# Trees",main="Boosting Test Error (shrinkage=0.05)")
legend("topright",title="Depth",legend=c("1","2","3","4","5","6"),pch=19,inset=0.02,
       col=c("red","blue","green","orange","purple","navy"))
abline(h=min(test.err.reg),col="black")

matplot(n.trees,cbind(berr.reg31,berr.reg31,berr.reg33,berr.reg34,berr.reg35,berr.reg36),type="l",pch=19,
        col=c("red","blue","green","orange","purple","navy"),ylab="Mean Squared Error",xlab="# Trees",main="Boosting Test Error (shrinkage=0.1)")
legend("topright",title="Depth",legend=c("1","2","3","4","5","6"),pch=19,inset=0.02,
       col=c("red","blue","green","orange","purple","navy"))
abline(h=min(test.err.reg),col="black")

par(mfrow=c(1,1))
matplot(n.trees,cbind(berr.reg16,berr.reg26,berr.reg36),type="l",pch=19,
        col=c("red","blue","green"),ylab="Mean Squared Error",xlab="# Trees",main="Boosting Test Error (depth=6)")
legend("topright",title="Shrinkage",legend=c("0.01","0.05","0.1"),pch=19,inset=0.02,
       col=c("red","blue","green"),cex=0.8)
abline(h=min(test.err.reg),col="black")
#choose shrinkage=0.05, n.tree=5000,depth=6

boost.reg=gbm(formula.reg,data=train[,-1],distribution="gaussian",
              n.trees=5000,interaction.depth=6,shrinkage=0.05,verbose=F)
pred.boost.reg=predict(boost.reg,newdata=test[,-1],n.trees=5000)

#final prediction
pred.boost.count=(pred.boost.cas)^2+(pred.boost.reg)^2

##########################
#save results for kaggle submission
##########################
result <- data.frame(datetime = test$datetime, count=pred.rf.count)
write.csv(result, file="submit_result_trans_nohrbin_rf.csv",row.names=FALSE)

result2 <- data.frame(datetime = test$datetime, count=pred.boost.count)
write.csv(result2, file="submit_result_trans_nohrbin_boost.csv",row.names=FALSE)

##########################
#Non-linear models
##########################

library(gam)

newtrain %>%
  select(temp, humidity,rtreg,windspeed) %>%
  gather(-rtreg, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = rtreg)) +
  geom_point() +
  stat_smooth() +
  facet_wrap(~ var, scales = "free") +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
  theme_bw()

newtrain %>%
  select(temp,humidity,rtcas,windspeed) %>%
  gather(-rtcas, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = rtcas)) +
  geom_point() +
  stat_smooth() +
  facet_wrap(~ var, scales = "free") +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
  theme_bw()

##################################
# model for registered rentals
##################################

# linear 
fit.lm <- lm(rtreg~season+holiday+workingday+weather+temp+humidity+windspeed+hr_reg+day+year, data = newtrain)
yhat.lm <- predict(fit.lm, newtest)
mse.lm <- mean((newtest$rtreg - yhat.lm)^2)
mse.lm
summary(fit.lm)

tidy_lmfit <- tidy(fit.lm)
write.csv(tidy_lmfit, file="regression1.csv",row.names=FALSE)

# polynomial 2
fit.1 <- lm(rtreg ~ season+holiday+workingday+weather+hr_reg+day+year +
              poly(temp,2)+poly(humidity,2)+poly(windspeed,2), data = newtrain)
yhat.1 <- predict(fit.1, newtest)
mse.1 <- mean((newtest$rtreg - yhat.1)^2)
mse.1

# polynomial 3
fit.1 <- lm(rtreg ~ season+holiday+workingday+weather+hr_reg+day+year +
              poly(temp,3)+poly(humidity,3)+poly(windspeed,3), data = newtrain)
yhat.1 <- predict(fit.1, newtest)
mse.1 <- mean((newtest$rtreg - yhat.1)^2)
mse.1

# polynomial 4
fit.1 <- lm(rtreg ~ season+holiday+workingday+weather+hr_reg+day+year +
              poly(temp,4)+poly(humidity,4)+poly(windspeed,4), data = newtrain)
yhat.1 <- predict(fit.1, newtest)
mse.1 <- mean((newtest$rtreg - yhat.1)^2)
mse.1

# polynomial 3 -holiday -windspeed
fit.1 <- lm(rtreg ~ season+workingday+weather+hr_reg+day+year +
              poly(temp,3)+poly(humidity,3), data = newtrain)
yhat.1 <- predict(fit.1, newtest)
mse.1 <- mean((newtest$rtreg - yhat.1)^2)
mse.1

# interactions temp*humidity
fit.1 <- lm(rtreg ~ season+holiday+workingday+weather+hr_reg+day+year +
              poly(temp,3)+poly(humidity,3)+poly(windspeed,3)+poly(temp,3)*poly(humidity,3), data = newtrain)
yhat.1 <- predict(fit.1, newtest)
mse.1 <- mean((newtest$rtreg - yhat.1)^2)
mse.1

# interactions temp*season
fit.1 <- lm(rtreg ~ season+holiday+workingday+weather+hr_reg+day+year +
              poly(temp,3)+poly(humidity,3)+poly(windspeed,3)+poly(temp,3)*season, data = newtrain)
yhat.1 <- predict(fit.1, newtest)
mse.1 <- mean((newtest$rtreg - yhat.1)^2)
mse.1

# interactions humidity*season
fit.1 <- lm(rtreg ~ season+holiday+workingday+weather+hr_reg+day+year +
              poly(temp,3)+poly(humidity,3)+poly(windspeed,3)+poly(humidity,3)*season, data = newtrain)
yhat.1 <- predict(fit.1, newtest)
mse.1 <- mean((newtest$rtreg - yhat.1)^2)
mse.1

# gam
# smoothing splines
gam <- glm(data = newtrain, rtreg ~ as.factor(season) + as.factor(hr_reg) + as.factor(day) + 
                   as.factor(holiday) + as.factor(weather) + s(temp)  + s(humidity) + s(windspeed))
summary(gam2)
preds=predict(gam,newdata=newtest)
mse <- mean((newtest$rtreg - preds)^2)
mse
plot(newtest$rtreg, preds, xlab = "Actual", ylab = "Predicted", main = "Actual vs Predicted")
abline(0,1,col="red")

# local regression
gam <- glm(data = newtrain, rtreg ~ as.factor(season) + as.factor(hr_reg) + as.factor(day) + 
             as.factor(holiday) + as.factor(weather) + lo(temp,span=0.7)  +lo(humidity,span=0.7) + lo(windspeed,span=0.7))
summary(gam2)
preds=predict(gam,newdata=newtest)
mse <- mean((newtest$rtreg - preds)^2)
mse


##################################
# model for casual rentals
##################################

# linear 
fit.lm <- lm(rtcas~season+holiday+workingday+weather+temp+humidity+windspeed+hr_reg+day+year, data = newtrain)
yhat.lm <- predict(fit.lm, newtest)
mse.lm <- mean((newtest$rtcas - yhat.lm)^2)
mse.lm
summary(fit.lm)

tidy_lmfit <- tidy(fit.lm)
write.csv(tidy_lmfit, file="regression1.csv",row.names=FALSE)

# polynomial 2
fit.1 <- lm(rtcas ~ season+holiday+workingday+weather+hr_reg+day+year +
              poly(temp,2)+poly(humidity,2)+poly(windspeed,2), data = newtrain)
yhat.1 <- predict(fit.1, newtest)
mse.1 <- mean((newtest$rtcas - yhat.1)^2)
mse.1

# polynomial 3
fit.1 <- lm(rtcas ~ season+holiday+workingday+weather+hr_reg+day+year +
              poly(temp,3)+poly(humidity,3)+poly(windspeed,3), data = newtrain)
yhat.1 <- predict(fit.1, newtest)
mse.1 <- mean((newtest$rtcas - yhat.1)^2)
mse.1

# polynomial 4
fit.1 <- lm(rtcas ~ season+holiday+workingday+weather+hr_reg+day+year +
              poly(temp,4)+poly(humidity,4)+poly(windspeed,4), data = newtrain)
yhat.1 <- predict(fit.1, newtest)
mse.1 <- mean((newtest$rtcas - yhat.1)^2)
mse.1

# polynomial 5
fit.1 <- lm(rtcas ~ season+holiday+workingday+weather+hr_reg+day+year +
              poly(temp,5)+poly(humidity,5)+poly(windspeed,5), data = newtrain)
yhat.1 <- predict(fit.1, newtest)
mse.1 <- mean((newtest$rtcas - yhat.1)^2)
mse.1

# polynomial 3 -holiday -windspeed
fit.1 <- lm(rtcas ~ season+workingday+weather+hr_reg+day+year +
              poly(temp,3)+poly(humidity,3), data = newtrain)
yhat.1 <- predict(fit.1, newtest)
mse.1 <- mean((newtest$rtcas - yhat.1)^2)
mse.1

# interactions temp*humidity
fit.1 <- lm(rtcas ~ season+holiday+workingday+weather+hr_reg+day+year +
              poly(temp,3)+poly(humidity,3)+poly(windspeed,3)+poly(temp,3)*poly(humidity,3), data = newtrain)
yhat.1 <- predict(fit.1, newtest)
mse.1 <- mean((newtest$rtcas - yhat.1)^2)
mse.1

# interactions temp*season
fit.1 <- lm(rtcas ~ season+holiday+workingday+weather+hr_reg+day+year +
              poly(temp,3)+poly(humidity,3)+poly(windspeed,3)+poly(temp,3)*season, data = newtrain)
yhat.1 <- predict(fit.1, newtest)
mse.1 <- mean((newtest$rtcas - yhat.1)^2)
mse.1

# interactions humidity*season
fit.1 <- lm(rtcas ~ season+holiday+workingday+weather+hr_reg+day+year +
              poly(temp,3)+poly(humidity,3)+poly(windspeed,3)+poly(humidity,3)*season, data = newtrain)
yhat.1 <- predict(fit.1, newtest)
mse.1 <- mean((newtest$rtcas - yhat.1)^2)
mse.1

# gam
# smoothing splines
gam <- glm(data = newtrain, rtcas ~ as.factor(season) + as.factor(hr_reg) + as.factor(day) + 
             as.factor(holiday) + as.factor(weather) + s(temp)  + s(humidity) + s(windspeed))
summary(gam2)
preds=predict(gam,newdata=newtest)
mse <- mean((newtest$rtcas - preds)^2)
mse
plot(newtest$rtcas, preds, xlab = "Actual", ylab = "Predicted", main = "Actual vs Predicted")
abline(0,1,col="red")

# local regression
gam <- glm(data = newtrain, rtcas ~ as.factor(season) + as.factor(hr_reg) + as.factor(day) + 
             as.factor(holiday) + as.factor(weather) + lo(temp,span=0.7)  +lo(humidity,span=0.7) + lo(windspeed,span=0.7))
summary(gam2)
preds=predict(gam,newdata=newtest)
mse <- mean((newtest$rtcas - preds)^2)
mse


