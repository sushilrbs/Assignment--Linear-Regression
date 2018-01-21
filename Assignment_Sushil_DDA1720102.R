library(MASS)
library(dplyr)
library(stringi)
library(tidyr)
library(car)

# 1.Set working directory
setwd("E:/Personal/IIIT-B/PredictiveAnalysis/Assignment- Linear Regression")

# 2.Import the data into dataframe

geelyauto <-read.csv("CarPrice_Assignment.csv",stringsAsFactors = FALSE)

# 3.View the structure of Dataframe
str(geelyauto)

 # 4 Split carname varibale in 2 categories(carcompany and ModelName)

geelyauto <- separate(geelyauto, col = CarName, into = c("carcompany","modelname"), sep = " ")

###################---------------------

#### Duplicate check on unique ID(No action required)

sum(duplicated(geelyauto$car_ID))

########### Handle  categorical variables having 2 levels ##############

#Let us see the structure of variable "fueltype".
str(geelyauto$fueltype)
summary(factor(geelyauto$fueltype))
#diesel=0 and gas=1 (Assign numerical value and convert to numeric)

geelyauto$fueltype<-ifelse(geelyauto$fueltype=="diesel",0,1)
geelyauto$fueltype<-as.numeric(geelyauto$fueltype)


#Let us see the structure of variable "aspiration".
str(geelyauto$aspiration)
summary(factor(geelyauto$aspiration))
#std=0 and turbo=1 (Assign numerical value and convert to numeric)

geelyauto$aspiration<-ifelse(geelyauto$aspiration=="std",0,1)
geelyauto$aspiration<-as.numeric(geelyauto$aspiration)


#Let us see the structure of variable "doornumber".
str(geelyauto$doornumber)
summary(factor(geelyauto$doornumber))
#four=115 and two=1 (Assign numerical value and convert to numeric)

geelyauto$doornumber<-ifelse(geelyauto$doornumber=="four",0,1)
geelyauto$doornumber<-as.numeric(geelyauto$doornumber)

#Let us see the structure of variable "enginelocation".
str(geelyauto$enginelocation)
summary(factor(geelyauto$enginelocation))
#front=115 and rear=1

geelyauto$enginelocation<-ifelse(geelyauto$enginelocation=="front",0,1)
geelyauto$enginelocation<-as.numeric(geelyauto$enginelocation)


####### Now Handle categorical variables having more than 3 levels. 

# 4 #DUMMY VARIABLE CREATION.
#Let us see the structure of variable "carbody".
str(geelyauto$carbody)
summary(factor(geelyauto$carbody))
#Converting "carbody" into dummies . 
dummy_1 <- data.frame(model.matrix( ~carbody, data = geelyauto))
#check the dummy_1 data frame.
View(dummy_1)
#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "carbody". 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "carbody" column
geelyauto_1 <- cbind(geelyauto[,-8], dummy_1)
View(geelyauto_1)


#Let us see the structure of variable "drivewheel".
str(geelyauto_1$drivewheel)
summary(factor(geelyauto_1$drivewheel))
#Converting "drivewheel" into dummies . 
dummy_1 <- data.frame(model.matrix( ~drivewheel, data = geelyauto_1))
#check the dummy_1 data frame.
View(dummy_1)
#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "drivewheel". 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "drivewheel" column
geelyauto_2 <- cbind(geelyauto_1[,-8], dummy_1)
View(geelyauto_2)


#Let us see the structure of variable "enginetype".
str(geelyauto_2$enginetype)
summary(factor(geelyauto_2$enginetype))
#Converting "enginetype" into dummies . 
dummy_1 <- data.frame(model.matrix( ~enginetype, data = geelyauto_2))
#check the dummy_1 data frame.
View(dummy_1)
#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "enginetype". 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "enginetype" column
geelyauto_3 <- cbind(geelyauto_2[,-14], dummy_1)
View(geelyauto_3)

#Let us see the structure of variable "cylindernumber".
str(geelyauto_3$cylindernumber)
summary(factor(geelyauto_3$cylindernumber))
#Converting "cylindernumber" into dummies . 
dummy_1 <- data.frame(model.matrix( ~cylindernumber, data = geelyauto_3))
#check the dummy_1 data frame.
View(dummy_1)
#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "cylindernumber". 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "cylindernumber" column
geelyauto_4 <- cbind(geelyauto_3[,-14], dummy_1)
View(geelyauto_4)

#Let us see the structure of variable "fuelsystem".
str(geelyauto_4$fuelsystem)
summary(factor(geelyauto_4$fuelsystem))
#Converting "fuelsystem" into dummies . 
dummy_1 <- data.frame(model.matrix( ~fuelsystem, data = geelyauto_4))
#check the dummy_1 data frame.
View(dummy_1)
#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "fuelsystem". 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "fuelsystem" column
geelyauto_5 <- cbind(geelyauto_4[,-15], dummy_1)
View(geelyauto_5)

 ################Data Correction #################

#### Correction on Car Company name
#(There are few car company names which are duplicate or names are not correct)
 levels(as.factor(geelyauto_5$carcompany))
 #from maxda to mazda
 #from Nissan to nissan
 #from porcshce to porsche
 #from toyouta to toyota
 #from vokswagen to volkswagen
 #from vw to volkswagen
 #from alfa-romero to alfa-romeo
 
 geelyauto_5$carcompany<-ifelse(geelyauto_5$carcompany=="maxda", "mazda", 
                   ifelse(geelyauto_5$carcompany=="Nissan", "nissan", 
                   ifelse(geelyauto_5$carcompany=="porcshce", "porsche", 
                   ifelse(geelyauto_5$carcompany=="toyouta", "toyota", 
                   ifelse(geelyauto_5$carcompany=="vokswagen", "volkswagen", 
                   ifelse(geelyauto_5$carcompany=="vw", "volkswagen",
                   ifelse(geelyauto_5$carcompany=="alfa-romero", "alfa-romeo",
                   geelyauto_5$carcompany)))))))
 

 #Let us see the structure of variable "carcompany" and create dummy varibales
 str(geelyauto_5$carcompany)
 summary(factor(geelyauto_5$carcompany))
 #Converting "carcompany" into dummies . 
 dummy_1 <- data.frame(model.matrix( ~carcompany, data = geelyauto_5))
 #check the dummy_1 data frame.
 View(dummy_1)
 #This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "carcompany". 
 dummy_1 <- dummy_1[,-1]
 # Combine the dummy variables to the main data set, after removing the original categorical "carcompany" column
 geelyauto_6 <- cbind(geelyauto_5[,-3], dummy_1)
 View(geelyauto_6)
 
 #### Removing car_ID and car Modelname from data frame.
 # car_ID variable is identity variable and will not have impact on car price so removing from DF
 # Scatter plot used check the relation with car_ID and price
 scatter.smooth(x=geelyauto_5$car_ID, y=geelyauto_5$price, main="Price ~ car_ID")
 geelyauto_6 <- geelyauto_6[,-1]
 
 
 #As Suggested on data preparation section, need to consider only car company name in model building
 #So removing car model from Data frame
 geelyauto_6 <- geelyauto_6[,-2]
 
 #### Outliers test #################

 # On wheelbase (Outliers) 
 #Using Box plot for identiying outliers
 boxplot(geelyauto_6$wheelbase, main="Wheel base", sub=paste("Outlier rows: ", boxplot.stats(geelyauto_6$wheelbase)$out)) 
 quantile(geelyauto_6$wheelbase,seq(0,1,0.01))
 
 #{ Note that there is a jump on 100% Therefore, we cap all values above 115.544 (100%) to 115.544. 
 geelyauto_6$wheelbase[which(geelyauto_6$wheelbase>115.544)]<-115.544
 
 # On carlength (no outliers)
 #Using Box plot for identiying outliers
 boxplot(geelyauto_6$carlength, main="Car Length", sub=paste("Outlier rows: ", boxplot.stats(geelyauto_6$carlength)$out)) 
 quantile(geelyauto_6$carlength,seq(0,1,0.01))

 # On carwidth (no outliers)
 boxplot(geelyauto_6$carwidth, main="Car Width", sub=paste("Outlier rows: ", boxplot.stats(geelyauto_6$carwidth)$out)) 
 quantile(geelyauto_6$carwidth,seq(0,1,0.01))
 
 # On carheight (no outliers)
 boxplot(geelyauto_6$carheight, main="Car Height", sub=paste("Outlier rows: ", boxplot.stats(geelyauto_6$carheight)$out)) 
 quantile(geelyauto_6$carheight,seq(0,1,0.01))
 
 # On curbweight (no outliers)
 boxplot(geelyauto_6$curbweight, main="Car Weight", sub=paste("Outlier rows: ", boxplot.stats(geelyauto_6$curbweight)$out)) 
 quantile(geelyauto_6$curbweight,seq(0,1,0.01))
 
 # On enginesize (Outliers)
 boxplot(geelyauto_6$enginesize, main="Engine Size", sub=paste("Outlier rows: ", boxplot.stats(geelyauto_6$enginesize)$out)) 
 quantile(geelyauto_6$enginesize,seq(0,1,0.01))
 
 
 #{Note that there is a jump between 32% and 33%. But, we cannot have 67% outliers. 
 #So, we check the next change and it is between 49% and 50% but we can not have 50% outliers
 #So we check the next change and it is between 93% and 94% , we are observing couple of jumps and its bussiness call to decide the floor value
 #Here we will cap all values above 209.00(96% value) to 209.00
 geelyauto_6$enginesize[which(geelyauto_6$enginesize>209.00)]<-209.00
  
 # On boreratio (no outliers)
 boxplot(geelyauto_6$boreratio, main="Bore Ratio", sub=paste("Outlier rows: ", boxplot.stats(geelyauto_6$boreratio)$out)) 
 quantile(geelyauto_6$boreratio,seq(0,1,0.01))
 
 # On stroke (outliers)
 #{Note that there is a jump between 0% and 1%. 
 #here we will cap all values below 2.6400 to 2.6400. 
 boxplot(geelyauto_6$stroke, main="Stroke", sub=paste("Outlier rows: ", boxplot.stats(geelyauto_6$stroke)$out)) 
 quantile(geelyauto_6$stroke,seq(0,1,0.01))
 geelyauto_6$stroke[which(geelyauto_6$stroke<2.6400)]<-2.6400
 
 
 # On compressionratio (no outliers)
 boxplot(geelyauto_6$compressionratio, main="compressionratio", sub=paste("Outlier rows: ", boxplot.stats(geelyauto_6$compressionratio)$out)) 
 quantile(geelyauto_6$compressionratio,seq(0,1,0.01))
 
 #{ Note that there is a jump on 99% and 100% Therefore, we cap all values above 207.00 (99%) to 207.00. 
 # On horsepower (Outliers)
 boxplot(geelyauto_6$horsepower, main="Horsepower", sub=paste("Outlier rows: ", boxplot.stats(geelyauto_6$horsepower)$out)) 
 quantile(geelyauto_6$horsepower,seq(0,1,0.01))
 geelyauto_6$horsepower[which(geelyauto_6$horsepower>207.00)]<-207.00

 # On peakrpm (no outliers)
 boxplot(geelyauto_6$peakrpm, main="peakrpm", sub=paste("Outlier rows: ", boxplot.stats(geelyauto_6$peakrpm)$out)) 
 quantile(geelyauto_6$peakrpm,seq(0,1,0.01))
 
 # On citympg(outliers)
 boxplot(geelyauto_6$citympg, main="citympg", sub=paste("Outlier rows: ", boxplot.stats(geelyauto_6$citympg)$out)) 
 quantile(geelyauto_6$citympg,seq(0,1,0.01))
 
#{ Note that there is a jump on 99% and 100% Therefore, we cap all values above 38.0 (98%) to 38.0. 
geelyauto_6$citympg[which(geelyauto_6$citympg>38.0)]<-38.0
 
# On highwaympg(no outliers)
boxplot(geelyauto_6$highwaympg, main="highwaympg", sub=paste("Outlier rows: ", boxplot.stats(geelyauto_6$highwaympg)$out)) 
quantile(geelyauto_6$highwaympg,seq(0,1,0.01))

 
 ################binning on symboling variable###########

# So, let us create three new levels by binning the levels of "symboling" into  
# 3 equal bins. Create three levels(high,mid,low) which will include factor levels 1,2 and 3 of the variable symboling.
summary(as.factor(geelyauto_6$symboling))
#high(2,3),mid(0,1),low(-1,-2)

geelyauto_6$symboling<-as.factor(geelyauto_6$symboling)

levels(geelyauto_6$symboling)[1:2] <- "low"
levels(geelyauto_6$symboling)[2:3] <- "mid"
levels(geelyauto_6$symboling)[3:4] <- "high"

#The next step is to create dummy variables to convert the categorical to numerical

dummy_1 <- data.frame(model.matrix( ~symboling, data = geelyauto_6))
#check the dummy_1 data frame.
View(dummy_1)
#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "symboling". 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "symboling" column
geelyauto_7 <- cbind(geelyauto_6[,-1], dummy_1)
View(geelyauto_7)


######################################
##Create derived matrics (could be created but i didn't create on this analysis)
########################################
#################Modelling ###################

#Lets check the structure of dataset
str(geelyauto_7)

#Note that all the variables are of int/numeric type which is the requirement of model building. Now, we are
#all set to build the model on the geelyauto_7 dataset. So, let's start.

# Separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(geelyauto_7), 0.7*nrow(geelyauto_7))
train = geelyauto_7[trainindices,]
test = geelyauto_7[-trainindices,]

# Build model 1 containing all variables
#############Model 1 ###########################
model_1 <-lm(price~.,data=train)
summary(model_1)
## R-squared:  0.9782 and Adjusted R-squared:  0.9641 , p-value: < 2.2e-16

####################################
# 1.Note that there are quite a number of insignificant variables. It is not worthwhile to build the model including all the variables, 
# so eventually we will exclude the insignificant ones.
# 2.Notice that there are many insignificant variables in the model. 
# we will use step-wise function to remove the extremely insignificant variables in the beginning itself.
# 3. Now, use the step wise function to remove the insignificant variables.

#####################  stepAIC  ######################
step <- stepAIC(model_1, direction="both")

## stepAIC makes multiple calls and checks which variables are significant and will be on the last call.
# Now, type "step" to check the model to be built after application of stepwise function, thereby removing the extremely insignificant variables.
step

#Use variables on modelling which are extracted from stepAIC( from 66 varibales, new model will have 39 varibales)

################## Model 2 #####################
model_2 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
     wheelbase + carwidth + curbweight + enginesize + stroke + 
     compressionratio + peakrpm + highwaympg + carbodyhatchback + 
     carbodysedan + carbodywagon + drivewheelfwd + enginetypel + 
     enginetypeohcf + cylindernumberfive + cylindernumberfour + 
     cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + carcompanybmw + 
     carcompanybuick + carcompanydodge + carcompanyhonda + carcompanyisuzu + 
     carcompanyjaguar + carcompanymazda + carcompanymercury + 
     carcompanymitsubishi + carcompanynissan + carcompanyplymouth + 
     carcompanyrenault + carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
     carcompanyvolvo + enginetypedohcv, data = train)

# Let us look at the summary of the model
summary(model_2)

## R-squared:  0.9772 and Adjusted R-squared:  0.9686 , p-value: < 2.2e-16
# If we compare model_1 and model_2, adjusted R-squared has increased from the previous model
# Note that there are still insignificant variables present in the model that need to be removed one by one after checking VIF and P value

###########VIF function###########
# Next, we will apply the VIF function to check the multicollinearity of the independent variables and remove 
# the variables with VIF>2 in order of their insignificance.

vif(model_2)

# Note that the variables fueltype,compressionratio,curbweight,enginesize,carwidth,cylindernumbersix,cylindernumberfour,wheelbase have the highest VIFs, 
# but cannot be removed since these are highly significant(p-value< 0.05). So, the variable with next 
# highest VIF is highwaympg. It is insignificant as well. Hence, highwaympg can be removed from the model.

################## Model 3 #####################
model_3 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                wheelbase + carwidth + curbweight + enginesize + stroke + 
                compressionratio + peakrpm + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelfwd + enginetypel + 
                enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + carcompanybmw + 
                carcompanybuick + carcompanydodge + carcompanyhonda + carcompanyisuzu + 
                carcompanyjaguar + carcompanymazda + carcompanymercury + 
                carcompanymitsubishi + carcompanynissan + carcompanyplymouth + 
                carcompanyrenault + carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + enginetypedohcv, data = train)

## Now check adjusted R value on new model
summary(model_3)
## R-squared:  0.9767 and Adjusted R-squared:  0.9682 , p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# Check VIF of new model
vif(model_3)

#So, the variable with highest VIF is fueltype. It is insignificant as well(p>0.05). Hence, fueltype can be removed from the model.

################## Model 4 #####################
model_4 <- lm(formula = price ~ aspiration + enginelocation + 
                wheelbase + carwidth + curbweight + enginesize + stroke + 
                compressionratio + peakrpm + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelfwd + enginetypel + 
                enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + carcompanybmw + 
                carcompanybuick + carcompanydodge + carcompanyhonda + carcompanyisuzu + 
                carcompanyjaguar + carcompanymazda + carcompanymercury + 
                carcompanymitsubishi + carcompanynissan + carcompanyplymouth + 
                carcompanyrenault + carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + enginetypedohcv, data = train)

## Now check adjusted R value on new model
summary(model_4)
## R-squared:  0.9759 and Adjusted R-squared:  0.9673 , p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# Check VIF of new model
vif(model_4)

# Note that the variables curbweight,enginesize,carwidth,cylindernumbersix,cylindernumberfour have the highest VIFs, 
# but cannot be removed since these are highly significant(p-value< 0.05). So, the variable with next 
# highest VIF is wheelbase. It is insignificant as well(p-value> 0.05). Hence, wheelbase can be removed from the model.


################## Model 5 #####################
model_5 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + enginesize + stroke + 
                compressionratio + peakrpm + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelfwd + enginetypel + 
                enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + carcompanybmw + 
                carcompanybuick + carcompanydodge + carcompanyhonda + carcompanyisuzu + 
                carcompanyjaguar + carcompanymazda + carcompanymercury + 
                carcompanymitsubishi + carcompanynissan + carcompanyplymouth + 
                carcompanyrenault + carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + enginetypedohcv, data = train)

## Now check adjusted R value on new model
summary(model_5)
## R-squared:  0.9751 and Adjusted R-squared:  0.9666 , p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# Check VIF of new model for verifying next insignificant variable
vif(model_5)

# Note that the variables curbweight,enginesize,carwidth,cylindernumbersix,cylindernumberfour have the highest VIFs, 
# but cannot be removed since these are highly significant(p-value< 0.05). So, the variable with next 
# highest VIF is fuelsystemmpfi. It is insignificant as well(p-value> 0.05). Hence, fuelsystemmpfi can be removed from the model.

################## Model 6 #####################
model_6 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + enginesize + stroke + 
                compressionratio + peakrpm + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelfwd + enginetypel + 
                enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + carcompanybmw + 
                carcompanybuick + carcompanydodge + carcompanyhonda + carcompanyisuzu + 
                carcompanyjaguar + carcompanymazda + carcompanymercury + 
                carcompanymitsubishi + carcompanynissan + carcompanyplymouth + 
                carcompanyrenault + carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + enginetypedohcv, data = train)
## Now check adjusted R value on new model
summary(model_6)
## R-squared:  0.9747 and Adjusted R-squared:  0.9665 , p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# Check VIF of new model for verifying next insignificant variable
vif(model_6)

# Note that the variables curbweight,enginesize,carwidth,cylindernumbersix,cylindernumberfour have the highest VIFs, 
# but cannot be removed since these are highly significant(p-value< 0.05). So, the variable with next 
# highest VIF is carbodysedan. It is insignificant as well(p-value> 0.05). Hence, carbodysedan can be removed from the model.


################## Model 7 #####################
model_7 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + enginesize + stroke + 
                compressionratio + peakrpm + carbodyhatchback + 
                carbodywagon + drivewheelfwd + enginetypel + 
                enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + carcompanybmw + 
                carcompanybuick + carcompanydodge + carcompanyhonda + carcompanyisuzu + 
                carcompanyjaguar + carcompanymazda + carcompanymercury + 
                carcompanymitsubishi + carcompanynissan + carcompanyplymouth + 
                carcompanyrenault + carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + enginetypedohcv, data = train)

## Now check adjusted R value on new model
summary(model_7)
## R-squared:  0.9746 and Adjusted R-squared:  0.9667 , p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# Check VIF of new model for verifying next insignificant variable
vif(model_7)

# Note that the variables curbweight,enginesize,carwidth,cylindernumbersix,cylindernumberfour,carcompanytoyota,stroke,carcompanyhonda,carcompanybuick,drivewheelfwd,carcompanynissan
# enginetypeohcf,carcompanymazda,have the highest VIFs,but cannot be removed since these are highly significant(p-value< 0.05). So, the variable with next 
# highest VIF is peakrpm. It is insignificant as well(p-value> 0.05). Hence, peakrpm can be removed from the model.


################## Model 8 #####################
model_8 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + enginesize + stroke + 
                compressionratio + carbodyhatchback + 
                carbodywagon + drivewheelfwd + enginetypel + 
                enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + carcompanybmw + 
                carcompanybuick + carcompanydodge + carcompanyhonda + carcompanyisuzu + 
                carcompanyjaguar + carcompanymazda + carcompanymercury + 
                carcompanymitsubishi + carcompanynissan + carcompanyplymouth + 
                carcompanyrenault + carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + enginetypedohcv, data = train)

## Now check adjusted R value on new model
summary(model_8)
## R-squared:  0.9738 and Adjusted R-squared:  0.9659 , p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# Check VIF of new model for verifying next insignificant variable
vif(model_8)

# Note that the variables curbweight,enginesize,carwidth,cylindernumbersix,cylindernumberfour,carcompanytoyota,stroke,carcompanyhonda,carcompanybuick,drivewheelfwd,carcompanynissan
# enginetypeohcf,carcompanymazda,cylindernumberfive,carcompanymitsubishi,carcompanyvolkswagen have the highest VIFs,but cannot be removed since these are highly significant(p-value< 0.05). So, the variable with next 
# highest VIF is fuelsystem2bbl. It is insignificant as well(p-value> 0.05). Hence, fuelsystem2bbl can be removed from the model.


################## Model 9 #####################
model_9 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + enginesize + stroke + 
                compressionratio + carbodyhatchback + 
                carbodywagon + drivewheelfwd + enginetypel + 
                enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + carcompanybmw + 
                carcompanybuick + carcompanydodge + carcompanyhonda + carcompanyisuzu + 
                carcompanyjaguar + carcompanymazda + carcompanymercury + 
                carcompanymitsubishi + carcompanynissan + carcompanyplymouth + 
                carcompanyrenault + carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + enginetypedohcv, data = train)

## Now check adjusted R value on new model
summary(model_9)
## R-squared:  0.9738 and Adjusted R-squared:  0.9662 , p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# Check VIF of new model for verifying next insignificant variable
vif(model_9)


# Note that the variables curbweight,enginesize,carwidth,cylindernumbersix,cylindernumberfour,carcompanytoyota,stroke,carcompanyhonda,carcompanybuick,drivewheelfwd,carcompanynissan,
# enginetypeohcf,carcompanymazda,cylindernumberfive,carcompanymitsubishi,carcompanyvolkswagen,carcompanyvolvo,enginetypel,carcompanyplymouth,carcompanysaab,carcompanyjaguar,
# enginelocation,carcompanydodge,aspiration have the highest VIFs,but cannot be removed since these are highly significant(p-value< 0.05).

# Also, notice that the variables curbweight and enginesize still have the highest VIFs with very high significance since the beginning. 
# So, it will be a good idea to check their correlation as they might be highly correlated.

# Check the correlation of the variables curbweight and enginesize.Use cor().
cor(train$curbweight,train$enginesize)

# Note that the correlation is ~ 86%, indicating that the variables are highly correlated. 
# So, remove the variable with lower significance level out of the two and build model_10.(Remove enginesize)


################## Model 10 #####################
model_10 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + stroke + 
                compressionratio + carbodyhatchback + 
                carbodywagon + drivewheelfwd + enginetypel + 
                enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + carcompanybmw + 
                carcompanybuick + carcompanydodge + carcompanyhonda + carcompanyisuzu + 
                carcompanyjaguar + carcompanymazda + carcompanymercury + 
                carcompanymitsubishi + carcompanynissan + carcompanyplymouth + 
                carcompanyrenault + carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + enginetypedohcv, data = train)

## Now check adjusted R value on new model
summary(model_10)
## R-squared:  0.9716 and Adjusted R-squared:  0.9636 , p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# Check VIF of new model for verifying next insignificant variable
vif(model_10)

# Note that the variables curbweight,carwidth,cylindernumberfour,carcompanytoyota,cylindernumbersix have the highest VIFs,
# but cannot be removed since these are highly significant(p-value< 0.05).
# So, the variable with next highest VIF is stroke. It is insignificant as well(p-value> 0.05). Hence, stroke can be removed from the model.


################## Model 11 #####################
model_11 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + compressionratio + carbodyhatchback + 
                 carbodywagon + drivewheelfwd + enginetypel + 
                 enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + carcompanybmw + 
                 carcompanybuick + carcompanydodge + carcompanyhonda + carcompanyisuzu + 
                 carcompanyjaguar + carcompanymazda + carcompanymercury + 
                 carcompanymitsubishi + carcompanynissan + carcompanyplymouth + 
                 carcompanyrenault + carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
                 carcompanyvolvo + enginetypedohcv, data = train)

## Now check adjusted R value on new model
summary(model_11)
## R-squared:  0.9706 and Adjusted R-squared:  0.9627 , p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# Check VIF of new model for verifying next insignificant variable
vif(model_11)

# Note that the variables curbweight,carwidth,cylindernumberfour,carcompanytoyota,drivewheelfwd,carcompanynissan,carcompanyhonda,carcompanymazda,enginetypeohcf,carcompanybuick,cylindernumberfive,
# carcompanymitsubishi,carcompanyvolkswagen have the highest VIFs,but cannot be removed since these are highly significant(p-value< 0.05).

# Also, notice that the variables curbweight and carwidth still have the highest VIFs with very high significance since the beginning. 
# So, it will be a good idea to check their correlation as they might be highly correlated.

# Check the correlation of the variables curbweight and carwidth.Use cor().
cor(train$curbweight,train$carwidth)

# Note that the correlation is ~ 87%, indicating that the variables are highly correlated. 
# So, remove the variable with lower significance level out of the two and build model_12.(Remove carwidth)

################## Model 12 #####################
model_12 <- lm(formula = price ~ aspiration + enginelocation + 
                 curbweight + compressionratio + carbodyhatchback + 
                 carbodywagon + drivewheelfwd + enginetypel + 
                 enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + carcompanybmw + 
                 carcompanybuick + carcompanydodge + carcompanyhonda + carcompanyisuzu + 
                 carcompanyjaguar + carcompanymazda + carcompanymercury + 
                 carcompanymitsubishi + carcompanynissan + carcompanyplymouth + 
                 carcompanyrenault + carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
                 carcompanyvolvo + enginetypedohcv, data = train)

## Now check adjusted R value on new model
summary(model_12)
## R-squared:  0.9657 and Adjusted R-squared:  0.9569 , p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# Check VIF of new model for verifying next insignificant variable
vif(model_12)
#Checking variable which have vif>2 and are insignificant
# So, the variable with next highest VIF is carcompanyvolvo and  It is insignificant as well(p-value> 0.05). Hence, stroke can be removed from the model.


################## Model 13 #####################
model_13 <- lm(formula = price ~ aspiration + enginelocation + 
                 curbweight + compressionratio + carbodyhatchback + 
                 carbodywagon + drivewheelfwd + enginetypel + 
                 enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + carcompanybmw + 
                 carcompanybuick + carcompanydodge + carcompanyhonda + carcompanyisuzu + 
                 carcompanyjaguar + carcompanymazda + carcompanymercury + 
                 carcompanymitsubishi + carcompanynissan + carcompanyplymouth + 
                 carcompanyrenault + carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
                 enginetypedohcv, data = train)

## Now check adjusted R value on new model
summary(model_13)
## R-squared:  0.9651 and Adjusted R-squared:  0.9565 , p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# Check VIF of new model for verifying next insignificant variable
vif(model_13)
# All varibales having vif>2 are significant.Checked correlation as well among varibales having high VIF but didn't find high correlation on them
# So, now we will remove variables on the basis of p-values (or significance).variable.

# Note that the variable aspiration has the highest p-value(or lowest significance). So, remove this variable. Use model_14<-lm().

################## Model 14 #####################
model_14 <- lm(formula = price ~ enginelocation + 
                 curbweight + compressionratio + carbodyhatchback + 
                 carbodywagon + drivewheelfwd + enginetypel + 
                 enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + carcompanybmw + 
                 carcompanybuick + carcompanydodge + carcompanyhonda + carcompanyisuzu + 
                 carcompanyjaguar + carcompanymazda + carcompanymercury + 
                 carcompanymitsubishi + carcompanynissan + carcompanyplymouth + 
                 carcompanyrenault + carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
                 enginetypedohcv, data = train)

## Now check adjusted R value on new model
summary(model_14)
## R-squared:  0.964 and Adjusted R-squared:  0.9555 , p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# Check VIF of new model for verifying next insignificant variable
vif(model_14)

# All varibales having vif>2 are significant.Checked correlation as well among varibales having high VIF but didn't find high correlation on them
# So, now we will remove variables on the basis of p-values (or significance).variable.
# Note that the variable carbodyhatchback has the highest p-value(or lowest significance). So, remove this variable. Use model_15<-lm().


################## Model 15 #####################
model_15 <- lm(formula = price ~ enginelocation + 
                 curbweight + compressionratio + 
                 carbodywagon + drivewheelfwd + enginetypel + 
                 enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + carcompanybmw + 
                 carcompanybuick + carcompanydodge + carcompanyhonda + carcompanyisuzu + 
                 carcompanyjaguar + carcompanymazda + carcompanymercury + 
                 carcompanymitsubishi + carcompanynissan + carcompanyplymouth + 
                 carcompanyrenault + carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
                 enginetypedohcv, data = train)


## Now check adjusted R value on new model
summary(model_15)
## R-squared:  0.964 and Adjusted R-squared:  0.9559 , p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# Check again VIF of new model for verifying next insignificant variable.

vif(model_15)

# All varibales having vif>2 are significant.Checked correlation as well among varibales having high VIF but didn't find high correlation on them
# So, now we will remove variables on the basis of p-values (or significance).variable.
# Note that the variable carcompanyjaguar has the highest p-value(or lowest significance). So, remove this variable. Use model_16<-lm().


################## Model 16 #####################
model_16 <- lm(formula = price ~ enginelocation + 
                 curbweight + compressionratio + 
                 carbodywagon + drivewheelfwd + enginetypel + 
                 enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + carcompanybmw + 
                 carcompanybuick + carcompanydodge + carcompanyhonda + carcompanyisuzu + 
                 carcompanymazda + carcompanymercury + 
                 carcompanymitsubishi + carcompanynissan + carcompanyplymouth + 
                 carcompanyrenault + carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
                 enginetypedohcv, data = train)


## Now check adjusted R value on new model
summary(model_16)
## R-squared:  0.9628 and Adjusted R-squared:  0.9549, p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# Check again VIF of new model for verifying next insignificant variable.

vif(model_16)

# All varibales having vif>2 are significant.Checked correlation as well among varibales having high VIF but didn't find high correlation on them
# So, now we will remove variables on the basis of p-values (or significance).variable.
# Note that the variable carcompanymercury has the highest p-value(or lowest significance). So, remove this variable. Use model_17<-lm().

################## Model 17 #####################
model_17 <- lm(formula = price ~ enginelocation + 
                 curbweight + compressionratio + 
                 carbodywagon + drivewheelfwd + enginetypel + 
                 enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + carcompanybmw + 
                 carcompanybuick + carcompanydodge + carcompanyhonda + carcompanyisuzu + 
                 carcompanymazda + 
                 carcompanymitsubishi + carcompanynissan + carcompanyplymouth + 
                 carcompanyrenault + carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
                 enginetypedohcv, data = train)


## Now check adjusted R value on new model
summary(model_17)
## R-squared:  0.9625 and Adjusted R-squared:  0.9548, p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# Check again VIF of new model for verifying next insignificant variable.

vif(model_17)

# All varibales having vif>2 are significant.Checked correlation as well among varibales having high VIF but didn't find high correlation on them.
# So, now we will remove variables on the basis of p-values (or significance).variable.
# Note that the variable enginetypedohcv has the highest p-value(or lowest significance). So, remove this variable. Use model_18<-lm().



################## Model 18 #####################
model_18 <- lm(formula = price ~ enginelocation + 
                 curbweight + compressionratio + 
                 carbodywagon + drivewheelfwd + enginetypel + 
                 enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + carcompanybmw + 
                 carcompanybuick + carcompanydodge + carcompanyhonda + carcompanyisuzu + 
                 carcompanymazda + 
                 carcompanymitsubishi + carcompanynissan + carcompanyplymouth + 
                 carcompanyrenault + carcompanysaab + carcompanytoyota + carcompanyvolkswagen,
                 data = train)

## Now check adjusted R value on new model
summary(model_18)
## R-squared:  0.9615 and Adjusted R-squared:  0.9541, p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# Check again VIF of new model for verifying next insignificant variable.

vif(model_18)

# All varibales having vif>2 are significant.Checked correlation as well among varibales having high VIF but didn't find high correlation on them.
# So, now we will remove variables on the basis of p-values (or significance).variable.
# Note that the variable compressionratio has the highest p-value(or lowest significance). So, remove this variable. Use model_19<-lm().


################## Model 19 #####################
model_19 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + drivewheelfwd + enginetypel + 
                 enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + carcompanybmw + 
                 carcompanybuick + carcompanydodge + carcompanyhonda + carcompanyisuzu + 
                 carcompanymazda + 
                 carcompanymitsubishi + carcompanynissan + carcompanyplymouth + 
                 carcompanyrenault + carcompanysaab + carcompanytoyota + carcompanyvolkswagen,
                 data = train)

## Now check adjusted R value on new model
summary(model_19)
## R-squared:  0.9604 and Adjusted R-squared:  0.9531, p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# So, now we will remove variables on the basis of p-values (or significance).variable.
# The variable carcompanyisuzu has the highest p-value(or lowest significance). So, remove this variable. Use model_20<-lm().


################## Model 20 #####################
model_20 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + drivewheelfwd + enginetypel + 
                 enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanyhonda + carcompanymazda + 
                 carcompanymitsubishi + carcompanynissan + carcompanyplymouth + 
                 carcompanyrenault + carcompanysaab + carcompanytoyota + carcompanyvolkswagen,
               data = train)

## Now check adjusted R value on new model
summary(model_20)
## R-squared:  0.9574 and Adjusted R-squared:  0.95, p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# So, now we will remove variables on the basis of p-values (or significance).variable.
# Note that the variable carcompanysaab has the highest p-value(or lowest significance). So, remove this variable. Use model_21<-lm().



################## Model 21 #####################
model_21 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + drivewheelfwd + enginetypel + 
                 enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanyhonda + carcompanymazda + 
                 carcompanymitsubishi + carcompanynissan + carcompanyplymouth + 
                 carcompanyrenault + carcompanytoyota + carcompanyvolkswagen,
               data = train)

## Now check adjusted R value on new model
summary(model_21)
## R-squared:  0.9544 and Adjusted R-squared:  0.9469, p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# So, now we will remove variables on the basis of p-values (or significance).variable.
# Note that the variable carcompanyhonda has the highest p-value(or lowest significance). So, remove this variable. Use model_22<-lm().


################## Model 22 #####################
model_22 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + drivewheelfwd + enginetypel + 
                 enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanymazda + 
                 carcompanymitsubishi + carcompanynissan + carcompanyplymouth + 
                 carcompanyrenault + carcompanytoyota + carcompanyvolkswagen,
               data = train)

## Now check adjusted R value on new model
summary(model_22)
## R-squared:  0.9512 and Adjusted R-squared:  0.9437, p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# So, now we will remove variables on the basis of p-values (or significance).variable.
# Note that the variable carcompanynissan has the highest p-value(or lowest significance). So, remove this variable. Use model_23<-lm().



################## Model 23 #####################
model_23 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + drivewheelfwd + enginetypel + 
                 enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanymazda + 
                 carcompanymitsubishi + carcompanyplymouth + 
                 carcompanyrenault + carcompanytoyota + carcompanyvolkswagen,
               data = train)

## Now check adjusted R value on new model
summary(model_23)
## R-squared:  0.9498 and Adjusted R-squared:  0.9425, p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# So, now we will remove variables on the basis of p-values (or significance).variable.
# Note that the variable carcompanyplymouth has the highest p-value(or lowest significance). So, remove this variable. Use model_24<-lm().




################## Model 24 #####################
model_24 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + drivewheelfwd + enginetypel + 
                 enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanymazda + carcompanymitsubishi + 
                 carcompanyrenault + carcompanytoyota + carcompanyvolkswagen,
               data = train)

## Now check adjusted R value on new model
summary(model_24)
## R-squared:  0.9478 and Adjusted R-squared:  0.9407, p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# So, now we will remove variables on the basis of p-values (or significance).variable.
# Note that the variable enginetypeohcf has the highest p-value(or lowest significance). So, remove this variable. Use model_25<-lm().


################## Model 25 #####################
model_25 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + drivewheelfwd + enginetypel + 
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanymazda + carcompanymitsubishi + 
                 carcompanyrenault + carcompanytoyota + carcompanyvolkswagen,
               data = train)

## Now check adjusted R value on new model
summary(model_25)
## R-squared:  0.9462 and Adjusted R-squared:  0.9394, p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# So, now we will remove variables on the basis of p-values (or significance).variable.
# Note that the variable carcompanyrenault has the highest p-value(or lowest significance). So, remove this variable. Use model_26<-lm().




################## Model 26 #####################
model_26 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + drivewheelfwd + enginetypel + 
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + carcompanybmw + carcompanybuick + 
                 carcompanydodge + carcompanymazda + carcompanymitsubishi + 
                 carcompanytoyota + carcompanyvolkswagen,
               data = train)

## Now check adjusted R value on new model
summary(model_26)
## R-squared:  0.9446 and Adjusted R-squared:  0.9381, p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# So, now we will remove variables on the basis of p-values (or significance).variable.
# Note that the variable carcompanydodge has the highest p-value(or lowest significance). So, remove this variable. Use model_27<-lm().



################## Model 27 #####################
model_27 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + drivewheelfwd + enginetypel + 
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + carcompanybmw + carcompanybuick + 
                 carcompanymazda + carcompanymitsubishi + 
                 carcompanytoyota + carcompanyvolkswagen,
               data = train)

## Now check adjusted R value on new model
summary(model_27)
## R-squared:  0.9432 and Adjusted R-squared:  0.937, p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# So, now we will remove variables on the basis of p-values (or significance).variable.
# Note that the variable carcompanyvolkswagen has the highest p-value(or lowest significance). So, remove this variable. Use model_28<-lm().


################## Model 28 #####################
model_28 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + drivewheelfwd + enginetypel + 
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + carcompanybmw + carcompanybuick + 
                 carcompanymazda + carcompanymitsubishi + 
                 carcompanytoyota, data = train)

## Now check adjusted R value on new model
summary(model_28)
## R-squared:  0.9432 and Adjusted R-squared:  0.937, p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# So, now we will remove variables on the basis of p-values (or significance).variable.
# Note that the variable drivewheelfwd has the highest p-value(or lowest significance). So, remove this variable. Use model_29<-lm().


################## Model 29 #####################
model_29 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + enginetypel + 
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + carcompanybmw + carcompanybuick + 
                 carcompanymazda + carcompanymitsubishi + 
                 carcompanytoyota, data = train)

## Now check adjusted R value on new model
summary(model_29)
## R-squared:  0.9393 and Adjusted R-squared:  0.9337, p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# So, now we will remove variables on the basis of p-values (or significance).variable.
# Note that the variable carcompanymitsubishi has the highest p-value(or lowest significance). So, remove this variable. Use model_30<-lm().


################## Model 30 #####################
model_30 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + enginetypel + 
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + carcompanybmw + carcompanybuick + 
                 carcompanymazda + carcompanytoyota, data = train)

## Now check adjusted R value on new model
summary(model_30)
## R-squared:  0.9361 and Adjusted R-squared:  0.9308, p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# So, now we will remove variables on the basis of p-values (or significance).variable.
# Note that the variable carcompanymazda has the highest p-value(or lowest significance). So, remove this variable. Use model_31<-lm().



################## Model 31 #####################
model_31 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + enginetypel + 
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + carcompanybmw + carcompanybuick + 
                 carcompanytoyota, data = train)

## Now check adjusted R value on new model
summary(model_31)
## R-squared:  0.9336 and Adjusted R-squared:  0.9285, p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# Now all variables have well significance and less p-value.

#Lets remove a varibale(carcompanytoyota) which have high p-value () and have well significance and create a new model_32

################## Model 32 #####################
model_32 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + enginetypel + 
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + carcompanybmw + carcompanybuick, data = train)

## Now check adjusted R value on new model
summary(model_32)
## R-squared:  0.9278 and Adjusted R-squared:  0.9229, p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable
# Now all variables have well significance and less p-value.

#Lets remove a varibale(enginetypel) which have high p-value () and have well significance and create a new model_33


################## Model 33 #####################
model_33 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + carcompanybmw + carcompanybuick, data = train)

## Now check adjusted R value on new model
summary(model_33)
## R-squared:  0.9206 and Adjusted R-squared:  0.9159, p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable

#Lets remove a varibale(cylindernumbersix) which have high p-value () and have less significance and create a new model_34

################## Model 34 #####################
model_34 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + cylindernumberfive + cylindernumberfour + 
                 carcompanybmw + carcompanybuick, data = train)

## Now check adjusted R value on new model
summary(model_34)
## R-squared:  0.9143 and Adjusted R-squared:  0.9099, p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable

#Lets remove a varibale(cylindernumberfive) which have high p-value () and have less significance and create a new model_35


################## Model 35 #####################
model_35 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + cylindernumberfour + 
                 carcompanybmw + carcompanybuick, data = train)

## Now check adjusted R value on new model
summary(model_35)
## R-squared:  0.9084 and Adjusted R-squared:  0.9043, p-value: < 2.2e-16
#There is not much reduction on adjusted R value so we are good to remove this variable

#Lets remove a varibale(cylindernumberfour) which have high p-value () and have well significance and create a new model_36

################## Model 36 #####################
model_36 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + carcompanybmw + carcompanybuick, 
                 data = train)

## Now check adjusted R value on new model
summary(model_36)
## R-squared:  0.8978 and Adjusted R-squared:  0.894, p-value: < 2.2e-16
#There is not much  reduction on adjusted R value so we are good to remove this variable
# All varibales are significant and p-value is less so this model could be best fit model but 
# lets remove few more variable and find the impact before we decide all variables are best suited for model
# Let me remove carbodywagon since it has estimated and t value negative and create model_37

################## Model 37 #####################
model_37 <- lm(formula = price ~ enginelocation + 
                 curbweight + carcompanybmw + carcompanybuick, 
               data = train)
## Now check adjusted R value on new model
summary(model_37)
## R-squared:  0.8816 and Adjusted R-squared:  0.8782, p-value: < 2.2e-16
#There is not much  reduction on adjusted R value so we are good to remove this variable

# All varibales are significant and p-value is less so this model could be best fit model but 
# lets remove few more variable and find the impact on Adjuested R value before decide all variables are best suited for model
# Let me remove available varaibles on model one by one and check adjuested R value
# Adjusted R-squared value has decreased abruptly when we remove available variable from the model so this is the final model for prediction

############# Here we can say, Final model is model_37 but it has not confimed unless we check on test data ###################################

##############  Model Evaluation ###################
  
# Now, use model_37 to predict the price value for test data. 
############# Prediction 1 ##################
Predict_1<-predict(model_37, test[, -10])


# In order to check, how accurate are the predictions of the model,
# we need to find the r-squared value between the predicted and actual values of price.
# R-squared is defined as the square of correlation between actual and predicted values of the variable. 

(cor(test$price, Predict_1))^2 ##0.8234591
# Now r-squared value for the model is 88% while the r-squared value for the test dataset is 82%.
# Generally, a deviation of +(-) 5% in the R-squared value of the model for the test data is acceptable. 
# However, here deviation is (>5%) so we need to go back to upper model where we removed the variable.


############# Prediction 2 using model_36 ##################

# Now, use model_36 to predict the price value for test data. 

Predict_2<-predict(model_36, test[, -10])

(cor(test$price, Predict_2))^2 ####0.8382702
summary(model_36)  # 89%
# Now r-squared value for the model is 89% while the r-squared value for the test dataset is 84%.
# Generally, a deviation of +(-) 5% in the R-squared value of the model for the test data is acceptable. 
# Here deviation is (~5%) so we can confirm , this would be the final model and variables on this model will be final varibales

################## FINAL MODEL IS model_36 ####################
## FINAL vARIABLES are - enginelocation ,curbweight, carbodywagon , carcompanybmw ,carcompanybuick




##### Lets Evalutae our model on whole data set ######

# We can check if a model works well for whole dataset data in many different ways.Here we are using graphical representation to know about. We pay great attention to regression results, such as slope coefficients, p-values, or R2 that tell us how well a model represents on given data. 
# Residuals could show how poorly a model represents data. Residuals are leftover of the outcome variable after fitting a model (predictors) to data and they could reveal unexplained patterns in the data by the fitted model. 
# Using this information, we check, if linear regression assumptions are met or improvement is required.
# Here we are running analysis on whole data set  geelyauto_7
geelyautofit = lm(price ~ enginelocation+curbweight+carbodywagon+carcompanybmw+carcompanybuick, geelyauto_7)
par(mfrow=c(2,2))
plot(geelyautofit)

# Here is our observation on different generated graphs
# 1. Residuals vs Fitted :-We could find equally spread residuals around a linear line without distinct patterns, so it is a good indication that it does not have non-linear relationships

# 2. Normal Q-Q :- It's good residuals are lined well on the straight dashed line.

# 3. Scale-Location :- It's good we could see a horizontal line with equally (randomly) spread points.

# 4. Residuals vs Leverage :- The plot identified the influential observation on #17 and #75 rows where price could have outliers so Lets remove those remove and run our test again


#### Lets remove ourlier from dataset and generate "Residuals vs Leverage" graph again

# let me handle outlier on price  or remove these 2 rows from dataset and check slope coefficients, p-values, or R2. Here i am not using quatile function to handle ourlier but removing those rows from dataset

# On Price (Outliers) 
#Using Box plot for identiying outliers
boxplot(geelyauto_7$price, main="Wheel base", sub=paste("Outlier rows: ", boxplot.stats(geelyauto_7$price)$out)) 

## Remove 17 and 75 rows from data set and check model,
geelyauto_7 <- geelyauto_7[-c(17, 75), ] 

## Lets draw the plot again
geelyautofit = lm(price ~ enginelocation+curbweight+carbodywagon+carcompanybmw+carcompanybuick, geelyauto_7)
par(mfrow=c(2,2))
plot(geelyautofit)

####Check "Residuals vs Leverage" and its look fine now.
#### Lets check R and Adjusted R value
### Here created train1 and test1 data set from new dataset for predicting the values
set.seed(100)
trainindices= sample(1:nrow(geelyauto_7), 0.7*nrow(geelyauto_7))
train1 = geelyauto_7[trainindices,]
test1 = geelyauto_7[-trainindices,]

model_final <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + carcompanybmw + carcompanybuick, 
               data = train1)

summary(model_final)

## R-squared:  0.8903 and Adjusted R-squared:  0.8863, p-value: < 2.2e-16
#There is not much  reduction on adjusted R value so we are good to remove those 2 rows from dataset

############# Prediction 3 using model_final ##################

# Now, use model_final to predict the price value for test data. 

Predict_3<-predict(model_final, test[, -10])

(cor(test$price, Predict_3))^2 ####0.8238364
summary(model_final)  # 89%
# Now r-squared value for the model is 88% while the r-squared value for the test dataset is 82%.# Generally, a deviation of +(-) 5% in the R-squared value of the model for the test data is acceptable. 
# Here deviation is more than (~5%) so  it is always a subjective call to take

################## FINAL MODEL IS model_36 or model_final ####################
## FINAL vARIABLES are - enginelocation ,curbweight, carbodywagon , carcompanybmw ,carcompanybuick
