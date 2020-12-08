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

train.index <- createDataPartition(paste(train$holiday,train$season,train$weather,train$workingday), p = 0.8, list = FALSE)
newtrain <- train[train.index,]
newtest <- train[-train.index,]

d=rpart(rtreg~hour,data=train)
fancyRpartPlot(d)

library(rpart)
library(rattle) 
library(rpart.plot)
library(RColorBrewer)
logreg.bin=rpart(logreg~hour,data=train)
fancyRpartPlot(logreg.bin)
logcas.bin=rpart(logcas~hour,data=train)
fancyRpartPlot(logcas.bin)
logcou.bin=rpart(logcou~hour,data=train)
fancyRpartPlot(logcou.bin)

#hour bins for registered
data$hr_reg=0
data$hr_reg[data$hour<2]=1
data$hr_reg[data$hour>2 & data$hour<6]=2
data$hr_reg[data$hour==6]=3
data$hr_reg[data$hour>=22]=4
data$hr_reg[data$hour>6 & data$hour<16]=5
data$hr_reg[data$hour==20 | data$hour==21]=6
data$hr_reg[data$hour>15 & data$hour<20]=7

#hour bins for casual
data$hr_cas=0
data$hr_cas[data$hour>1 & data$hour<7]=1
data$hr_cas[data$hour<2]=2
data$hr_cas[data$hour==7]=3
data$hr_cas[data$hour>=20]=4
data$hr_cas[data$hour>7 & data$hour<11]=5
data$hr_cas[data$hour>10 & data$hour<20]=6

#hour bins for count
data$hr_cou=0
data$hr_cou[data$hour>1 & data$hour<6]=1
data$hr_cou[data$hour<2]=2
data$hr_cou[data$hour==6]=3
data$hr_cou[data$hour>=21]=4
data$hr_cou[data$hour>6 & data$hour<16]=5
data$hr_cou[data$hour>15 & data$hour<21]=6
