
#################################Employee Absenteeism###############################################
#Remove all the objects stored
rm(list=ls())

#set & check working directory
setwd("F:/ed_project_2/R")
getwd()

#Load Libaries
x=c("ggplot2","corrgram","DMwR","caret","randomForest","unbalanced","C50","MASS","ridge",
    "dummies","e1071","xlsx","gbm","rpart","data.table","Information","ROSE","inTrees",
    "DataCombine","sampling","reshape","dplyr","plyr")

#Install pakages(x)
lapply(x,require,character.only=T)
rm(x)

#Reading Employee Absenteeism DATA
library(xlsx)
data=read.xlsx("Absenteeism_at_work_Project.xls",sheetIndex = 1,header=T)



#######################Exploratory Data Analysis[EDA]################################
#Observe top 5 rows
head(data)

#Getting the dimensions of data
dim(data)

#fetching Structure Of data
str(data)

#Retrieving Column names of train and test data.
colnames(data)

#get the length of unique value counts of columns:
for (i in 1:ncol(data)) {
  print(colnames(data[i]))
  print(length(unique(data[i])[,1]))
}
#Summary
summary(data)

######################################## Missing Value Analysis #################################
#Get number of missing values & their missing percentage
missing_val = data.frame(apply(data,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_cnt"
row.names(missing_val) = NULL
missing_val$Missing_perc = (missing_val$Missing_cnt/nrow(data)) * 100
missing_val = missing_val[order(-missing_val$Missing_perc),]
missing_val = missing_val[,c(2,1,3)]
missing_val
#There are missing values in almost all the columns of the dataset, although in small amount.
#We'll drop all the missing value rows for target variable and We will imputenull values for all other features.

# Droping observation in which "Absenteeism time in hours" has missing value
data=data[!(is.na(data$Absenteeism.time.in.hours)),]

# Droping observation in which "Month_of_absence" has missing value
data = data[!is.na(data$Month.of.absence),]


#####Imputing Missing Values####
#Note:
#Impute missing values for all the independent featues(exept Average_Workload).
#Replace missing of any employee with information of same employee from other instances.
#for example if 'Age' of employee-x is missing, then impute it with 'Age' from other instance of employee-X.

col=colnames(data)
col=col[col!=c("ID","Work.load.Average.day.")]

#Loop to impute missing values for all the independent featues(exept Average_Workload)
for (i in unique(data$ID)) {
  for (j in col) {
   data[(data$ID==i) & (is.na(data[j])),j] = max(data[data["ID"]==i,j],na.rm = T)
  }
}

##### Now let's analyze which is the best way to impute missing values for 'Average_Workload'####

##lets plot scatter plot between 'ID' &'Average_workload'
ggplot(data,aes(x=ID, y=Work.load.Average.day.))+ geom_point()
#The above scatter plot shows 'Average_workload' is not depends on "ID".

##lets plot scatter plot between 'Month_of_absence' &'Average_workload'
ggplot(data,aes(x=Month.of.absence, y=Work.load.Average.day.))+geom_point()
#From this plot, we can conclude that 'Average_Workload' is distributed mostly by month.
#So,let's impute missing 'Average_Workload' by mode of that month

##Impute "Average_Workload" based on corresponding "month's workload"
for (i in unique(data$Month.of.absence)) {
  mod=mean(data[(data["Month.of.absence"]==i),"Work.load.Average.day."],na.rm = T)
  data[(data["Month.of.absence"]==i) & (is.na(data["Work.load.Average.day."])),"Work.load.Average.day."]=mod
}

#check for missing values
sum(is.na(data))
anyNA(data)

emp=data #stage-1 backup
#data=emp


###########################Variable Identification ##########################################

continuous_var = c('Distance.from.Residence.to.Work', 'Service.time', 'Age', 'Height', 
                    'Work.load.Average.day.', 'Transportation.expense','Hit.target', 'Weight',
                    'Body.mass.index', 'Absenteeism.time.in.hours')

catagorical_var = c('ID','Reason.for.absence','Month.of.absence','Day.of.the.week',
                     'Seasons','Disciplinary.failure', 'Education', 'Social.drinker',
                     'Social.smoker', 'Son', 'Pet')

#Converting data to proper formats (continuous & catagorical)
for (i in catagorical_var) {
  data[,i]=as.factor(data[,i])
}

#Confirming the changes type
str(data)

########################## Data Visualisation #################################################
####Univariate Analysis####

###Distribution of catagorical features using bar plot
bar1 = ggplot(data = emp, aes(x = ID)) + geom_bar() + ggtitle("Count of ID") + theme_bw()
bar2 = ggplot(data = emp, aes(x = Reason.for.absence)) + geom_bar() + 
       ggtitle("Count of Reason for absence") + theme_bw()
bar3 = ggplot(data = emp, aes(x = Month.of.absence)) + geom_bar() + ggtitle("Count of Month") + theme_bw()
bar4 = ggplot(data = emp, aes(x = Disciplinary.failure)) + geom_bar() + 
       ggtitle("Count of Disciplinary failure") + theme_bw()
bar5 = ggplot(data = emp, aes(x = Education)) + geom_bar() + ggtitle("Count of Education") + theme_bw()
bar6 = ggplot(data = emp, aes(x = Day.of.the.week)) + geom_bar() + ggtitle("days of week") + theme_bw()
bar7 = ggplot(data = emp, aes(x = Social.smoker)) + geom_bar() + 
       ggtitle("Count of Social smoker") + theme_bw()
bar8 = ggplot(data = emp, aes(x = Seasons)) + geom_bar() + ggtitle("Count of Seasons") + theme_bw()
bar9 = ggplot(data = emp, aes(x = Son)) + geom_bar() + ggtitle("Count of sons") + theme_bw()
bar10= ggplot(data = emp, aes(x = Pet)) + geom_bar() + ggtitle("Count of pets") + theme_bw()
bar11= ggplot(data = emp, aes(x = Social.drinker)) + geom_bar() + ggtitle("Count of social drinker") + theme_bw()

gridExtra::grid.arrange(bar1,bar2,bar3,bar4,bar5,bar6,bar7,bar8,bar9,ncol=3)
gridExtra::grid.arrange(bar10,bar11,ncol=1)



###Check the distribution of numerical data using histogram
hist1 = ggplot(data = data, aes(x =Transportation.expense)) + 
        ggtitle("Transportation.expense") + geom_histogram(bins = 25)
hist2 = ggplot(data = data, aes(x =Height)) + 
        ggtitle("Distribution of Height") + geom_histogram(bins = 25)
hist3 = ggplot(data = emp, aes(x =Body.mass.index)) + 
        ggtitle("Distribution of Body.mass.index") + geom_histogram(bins = 25)
hist4 = ggplot(data = data, aes(x =Weight)) + 
        ggtitle("Distribution of Weight") + geom_histogram(bins = 25)
hist5 = ggplot(data = data, aes(x =Distance.from.Residence.to.Work)) + 
        ggtitle("Distribution of Distance.from.Residence.to.Work") + geom_histogram(bins = 25)
hist6 = ggplot(data = data, aes(x =Service.time)) + 
        ggtitle("Distribution of Service.time") + geom_histogram(bins = 25)
hist7 = ggplot(data = data, aes(x =Work.load.Average.day.)) + 
        ggtitle("Distribution of Work.load.Average.day.") + geom_histogram(bins = 25)
hist8 = ggplot(data = data, aes(x =Hit.target)) + 
        ggtitle("Distribution of Hit.target") + geom_histogram(bins = 25)
hist9 = ggplot(data = data, aes(x =Age)) + 
        ggtitle("Distribution of Age") + geom_histogram(bins = 25)
hist10= ggplot(data = data, aes(x =Absenteeism.time.in.hours)) + 
        ggtitle("Distribution of Absenteeism.time.in.hours") + geom_histogram(bins = 25)

gridExtra::grid.arrange(hist1,hist2,hist3,hist4,hist5,hist6,hist7,hist8,hist9,ncol=3)
hist10

#Infer:
#People over 40+ years of age tends to take less leaves compare to others.
#A very large portion of the population have only passed ‘High School’.
#More then half of the employees in the company are ‘social drinker’.
#Only a very few portion of the employees in the company are ‘social smoker’

#### KDE plot ( Normality Check)
library("kdensity")
par(mfrow=c(5,4))
par(mar=rep(2,4))
plot(density(emp$ID))
plot(density(emp$Reason.for.absence))
plot(density(emp$Month.of.absence))
plot(density(emp$Day.of.the.week))
plot(density(emp$Seasons))
plot(density(emp$Transportation.expense))
plot(density(emp$Distance.from.Residence.to.Work))
plot(density(emp$Service.time))
plot(density(emp$Age))
plot(density(emp$Work.load.Average.day.))
plot(density(emp$Hit.target))
plot(density(emp$Absenteeism.time.in.hours))
plot(density(emp$Body.mass.index))
plot(density(emp$Height))
plot(density(emp$Weight))
plot(density(emp$Social.smoker))
plot(density(emp$Social.drinker))
plot(density(emp$Son))
plot(density(emp$Education))
plot(density(emp$Disciplinary.failure))
par(mfrow=c(1,1))
#we can observe that none of the features follow Gaussian distribution.


####Bivariate Analysis####

#The relationship btwn independent continuous variables and Target variable.
g1=ggplot(emp, aes(x=Distance.from.Residence.to.Work,y=Absenteeism.time.in.hours)) +geom_point()+geom_smooth()
g2=ggplot(emp, aes(x=Service.time ,y=Absenteeism.time.in.hours)) +geom_point()+geom_smooth()
g3=ggplot(emp, aes(x=Age ,y=Absenteeism.time.in.hours)) +geom_point()+geom_smooth()
g4=ggplot(emp, aes(x=Height ,y=Absenteeism.time.in.hours)) +geom_point()+geom_smooth()
g5=ggplot(emp, aes(x=Work.load.Average.day. ,y=Absenteeism.time.in.hours)) +geom_point()+geom_smooth()
g6=ggplot(emp, aes(x=Hit.target ,y=Absenteeism.time.in.hours)) +geom_point()+geom_smooth()
g7=ggplot(emp, aes(x=Weight ,y=Absenteeism.time.in.hours)) +geom_point()+geom_smooth()
g8=ggplot(emp, aes(x=Body.mass.index ,y=Absenteeism.time.in.hours)) +geom_point()+geom_smooth()
g9=ggplot(emp, aes(x=Transportation.expense ,y=Absenteeism.time.in.hours)) +geom_point()+geom_smooth()

gridExtra::grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,ncol=3)

#ALso, as we can see, 'Absenteeism_time_in_hours' are 0 in 36 places.
#This could be result of cancelled or withdrwan leaves. Lets drop these observations

#removing the observation in whic absent hour is zero
data=data[data$Absenteeism.time.in.hours>0,]


############################ Outlier Analysis######################################################
#Check for outliers using boxplots
emp_cnt=data[continuous_var]
for(i in 1:ncol(emp_cnt)) {
  assign(paste0("box",i), ggplot(data = emp_cnt, aes_string(y = emp_cnt[,i])) +
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour = "red", fill = "grey", outlier.size = 1) +
           labs(y = colnames(emp_cnt[i])) +
           ggtitle(colnames(emp_cnt[i])))
}

#Arrange the plots in grids
gridExtra::grid.arrange(box1,box2,box3,box4,box5,box6,ncol=3)
gridExtra::grid.arrange(box7,box8,box9,box10,ncol=2)
#From boxplot Outliers are found in 'Service_time', 'Age', 'Average_workload', 
#'Transportation_expense', 'Hit_target', 'Absenteeism_time_in_hours', 'Height'.

##Loop to Remove outliers using boxplot method
c_out=continuous_var[-c(2,3)] #excluding  'Service_time' & 'Age'

for(i in c_out)
{
  print(i)
  val = data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
  #print(length(val))
  data = data[which(!data[,i] %in% val),]
}

rownames(data)=NULL #resetting row index


############################ Feature Selection##############################################
#Correlation Plot
library(corrgram)
corrgram(data[,continuous_var] , order =F, upper.panel = panel.pie ,
         text.panel = panel.txt , main = "Correlation Plot")

#correlation matrix
cor(data[continuous_var])
#This shows that there is multicollinearity in the dataset. "Body_mass_index" and "Weight" are highly correlated

#droping corelated variable
data = subset(data,select=-c(Body.mass.index))

# Updating the Continous Variables and Categorical Variables after droping some variables
continuous_var=continuous_var[-9]


########## ANOVA Test###################
a_var=c(catagorical_var, "Absenteeism.time.in.hours")
absent=data[a_var]

summary(aov(formula=Absenteeism.time.in.hours~ID, data = absent))
summary(aov(formula=Absenteeism.time.in.hours~Reason.for.absence, data = absent))
summary(aov(formula=Absenteeism.time.in.hours~Month.of.absence, data = absent))
summary(aov(formula=Absenteeism.time.in.hours~Day.of.the.week, data = absent))
summary(aov(formula=Absenteeism.time.in.hours~Seasons, data = absent))
summary(aov(formula=Absenteeism.time.in.hours~Education, data = absent))
summary(aov(formula=Absenteeism.time.in.hours~Social.smoker, data = absent))
summary(aov(formula=Absenteeism.time.in.hours~Social.drinker, data = absent))
summary(aov(formula=Absenteeism.time.in.hours~Son, data = absent))
summary(aov(formula=Absenteeism.time.in.hours~Pet, data = absent))


############################### Feature Scaling #############################################
#since none of the features are normally distributed,so we are Normalizing the data
x_data=data     #stage-zero backup

#Nomalisation
cnames=continuous_var[-9]                 #removing 'Absenteeism.time.in.hours'

for (i in cnames) {
  print(i)
  x_data[,i] = (x_data[,i] - min(x_data[,i]))/(max(x_data[,i])-min(x_data[,i]))
}

############################# CREATING DUMMIES FOR CATEGORICAL VARIABLES ####################################
df1=x_data   #stage-2 backup
#x_data=df1

library(fastDummies)
d1 = fastDummies::dummy_cols(x_data[catagorical_var] )
d1=subset(d1,select=-c(ID,Reason.for.absence,Month.of.absence,Day.of.the.week,Seasons,
                       Disciplinary.failure,Education,Social.drinker,Social.smoker,Son,Pet ))
d2=cbind(x_data,d1)
x_data=subset(d2,select=-c(ID,Reason.for.absence,Month.of.absence,Day.of.the.week,Seasons,
                           Disciplinary.failure,Education,Social.drinker,Social.smoker,Son,Pet))

######## Splitting the data into train and test#####
rmExcept(c("x_data","data","emp","df1","catagorical_var","continuous_var","cnames"))
set.seed(542)
train_index = sample(1:nrow(x_data), 0.8 * nrow(x_data))
train = x_data[train_index,]
test = x_data[-train_index,]

x_train = subset(train,select = -c(Absenteeism.time.in.hours))
x_test = subset(test,select = -c(Absenteeism.time.in.hours))

y_train = subset(train,select = c(Absenteeism.time.in.hours))
y_test = subset(test,select = c(Absenteeism.time.in.hours))


###################### DIMENSION REDUCTION USING PCA ###############################
#principal component analysis
prin_comp = prcomp(x_train)

#compute standard deviation of each principal component
std_dev = prin_comp$sdev

#compute variance
pr_var = std_dev^2
#proportion of variance explained
prop_varex = pr_var/sum(pr_var)

#cdf plot for principle components
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#add a training set with principal components
x_train.data = data.frame( prin_comp$x)

# From the above plot selecting 37 components since it explains almost 95+ % data variance
x_train.data =x_train.data[,1:37]

#transform test into PCA
x_test.data = predict(prin_comp, newdata = x_test)
x_test.data = as.data.frame(x_test.data)

#select the first 37 components
x_test.data=x_test.data[,1:37]


######################################## Machine learning model##########################################
x_train.data$Absenteeism.time.in.hours = y_train$Absenteeism.time.in.hours
x_test.data$Absenteeism.time.in.hours = y_test$Absenteeism.time.in.hours


#######################1.Decision Tree##################################################
#Build decsion tree using rpart
fit_DT = rpart(Absenteeism.time.in.hours ~., data = x_train.data, method = 'anova')
#Predict the test cases
pred_DT = predict(fit_DT,x_test.data[,-38])
#Calcuate MAE, RMSE, R-sqaured for testing data
print(postResample(pred = pred_DT, obs = x_test.data[,38]))

#Plot a graph for actual vs predicted values
plot(x_test.data$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(pred_DT,col="blue")
#rmse=2.929

############################2.Random Forest############################################
#Train the model using training data
fit_RF = randomForest(Absenteeism.time.in.hours ~., data = x_train.data, ntree = 500)
#Predict the test cases
pred_RF = predict(fit_RF,x_test.data[,-38])
#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = pred_RF, obs = x_test.data[,38]))

#Plot a graph for actual vs predicted values
plot(x_test.data$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(pred_RF,col="blue")
#rmse=2.351

############################3.Linear Regression######################################
set.seed(100)
#Develop Model on training data
fit_LR = lm(Absenteeism.time.in.hours ~ ., data = x_train.data)
#Lets predict for testing data
pred_LR = predict(fit_LR,x_test.data[,-38])
# Results 
print(postResample(pred = pred_LR, obs =x_test.data[,38]))

#Plot a graph for actual vs predicted values
plot(x_test.data$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(pred_LR,col="blue")
#rmse=2.498

########################4.Ridge regression##########################################
library(ridge)
Ridge = linearRidge(Absenteeism.time.in.hours~ ., data = x_train.data)
#Lets predict for testing data
pred_Ridge = predict(Ridge,x_test.data[,-38])
# Results 
print(postResample(pred = pred_Ridge, obs =x_test.data[,38]))
#rmse=2.358


#########################5.KNN Regressor#####################################################
#Develop Model on training data
KNN = knnreg(Absenteeism.time.in.hours~ ., data = x_train.data)
#Lets predict for testing data
pred_KNN = predict(KNN,x_test.data[,-38])
# Results 
print(postResample(pred = pred_KNN, obs =x_test.data[,38]))

#Plot a graph for actual vs predicted values
plot(x_test.data$Absenteeism.time.in.hours,type="l",lty=2,col="red")
lines(pred_KNN,col="blue")
#rmse=2.500

######################6.Support Vector Regression ##########################################
#Develop Model on training data
fit_SVM = svm(Absenteeism.time.in.hours ~ ., data = x_train.data)
#Lets predict for testing data
pred_SVM = predict(fit_SVM,x_test.data[,-38])
# Results 
print(postResample(pred = pred_SVM, obs =x_test.data[,38]))
#rmse=2.404

######################7.Gradient Boosting##################################################
fit_GBDT = gbm(Absenteeism.time.in.hours~., data = x_train.data, n.trees = 500, interaction.depth = 2)
#Lets predict for testing data
pred_GBDT = predict(fit_GBDT,x_test.data[,-38], n.trees = 500)
# For testing data 
print(postResample(pred = pred_GBDT, obs = x_test.data[,38]))
#rmse=2.703

#####################################2010##########################################
####Looking at models performance, we can say that 'Random forest' is the best model based on RMSE value
##TEST DATA PREDICTIONS FOR YEAR 2010
Absent_prediction=df1[-train_index,]
Absent_prediction$Predicted_Absent_hours=pred_RF
head(Absent_prediction)  #Sample output(with actual counts and predicted counts)

#Predicted absence hours of 2010
sum(Absent_prediction$Predicted_Absent_hours)

#Actual absence hours of 2010
sum(Absent_prediction$Absenteeism.time.in.hours)

#Predicted absence hours per month[2010]
aggre.months =  ddply(Absent_prediction, c("Month.of.absence"), function(x) colSums(x[c("Absenteeism.time.in.hours","Predicted_Absent_hours")]))
aggre.months


######################## PREDICTIONS FOR YEAR 2011 #########################################
#data for 2011
#Service and Age will be added by 1

data_2011 = data
data_2011$Service.time = data$Service.time + 1
data_2011$Age = data$Age + 1
data_2011=select(data_2011,-c("Absenteeism.time.in.hours"))

#Nomalisation
for (i in cnames) {
  print(i)
  data_2011[,i] = (data_2011[,i] - min(data_2011[,i]))/(max(data_2011[,i])-min(data_2011[,i]))
}

#get dummies
library(fastDummies)
d1 = fastDummies::dummy_cols(data_2011[catagorical_var] )
d1=select(d1,-catagorical_var)
d2=cbind(data_2011,d1)
emp_2011=select(d2,-catagorical_var)

#Using PCA
prin_comp = prcomp(emp_2011)
emp_2011 = data.frame( prin_comp$x)
emp_2011 =emp_2011[,1:37]  # selecting 37 components since it explains almost 95+ % data variance

#predicting the 2011 model
predict_2011_absence = predict(fit_RF,emp_2011)

#Absent prediction 2011
Predit_2011=data_2011
Predit_2011$Absent_hours_2011=predict_2011_absence
head(Predit_2011)

#Predicted absence hours per month of 2011
monthly_absence =  ddply(Predit_2011, c("Month.of.absence"), function(x) colSums(x[c("Absent_hours_2011")]))
monthly_absence


###*******************MONTHLY LOSSES PREDICTED FOR YEAR 2011 PER MONTH*************##########

#In a month excluding weekend 22 days are working days.
#there are 36 employee in the xyz company            
#8 hoursof work per day 
Total_Monthly_hours = 22*8*36

# total losses % = (absent_hours / Total_Monthly_hours)*100
monthly_absence$monthly_loss_percentage = (monthly_absence$Absent_hours_2011 /Total_Monthly_hours) * 100
print(monthly_absence)

#Looking at the above results, we can observe that most likely,
#the company would incur most of loss in the month of 'March', followed by 'October' and 'July'.*

#saving output results
write.csv(Predit_2011,"R_Employee Absenteeism_2011.csv",row.names = FALSE)   
write.csv(monthly_absence,"R_Monthly_loss.csv",row.names = FALSE)

##############################################################################################
#Hereby, concluding the project with above predictions.
#Regards
#SWAROOP H
###############################################################################################
