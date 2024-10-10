library(readxl)
library(ggplot2)
library(ggfortify)
install.packages("corrplot")
library(corrplot)
library("rquery")
library(RColorBrewer)
Data <- read_excel("C:/Users/Admin/Downloads/Data.xlsx")
library(caTools)
set.seed(150)
split = sample.split(Data$ID, SplitRatio = 0.62)
# Create training and testing sets
Data_train = subset(Data, split == TRUE)
Data_test = subset(Data, split == FALSE)
library(writexl)
write_xlsx(Data_train,"C:/Users/Admin/Downloads\\Data_train.xlsx")
Data <- read_excel("C:/Users/Admin/Downloads/Data_train.xlsx")

summary(Data)

ID=Data$ID
Ward=Data$Ward
Ward=as.factor(Ward)
ASAP	= Data$ASAP
Bedrooms	= Data$Bedrooms
Area	= Data$Area
Price	= Data$Price
Ward_Price	= Data$Ward_Price
Population = Data$Population

#ggplot chi dung cho categorical Var. Cai nay ban nho phai xai lenh as.factor nhu tren
ggplot(data=Data, aes(x=Population,y=Price, fill=Population, col =Population)) +geom_jitter(alpha=0.3)

cor(Bedrooms,Price)

ReturnReg<-lm(Price~ASAP+Bedrooms+Area+Ward_Price+Population)
autoplot(ReturnReg)
summary(ReturnReg)
#Detect multicollinearity
library(car)
vif(ReturnReg)
#Perform White's Test: p-value>0.05 -> fail to reject H0 -> Heteroscedasticity is not present in the regression model
library(lmtest)
bptest(ReturnReg, ~ Area*Bedrooms*Population*ASAP*Ward_Price + I(Area^2)+I(Bedrooms^2)+I(Population^2)+I(ASAP^2)+I(Ward_Price^2), data=Data)
#Test for autocorrelation
dwtest(ReturnReg)

Data1 = Data[,4:9]
View(Data1)
res <- cor(Data1)

corrplot(res, type="full", order="hclust",col=brewer.pal(n=8, name="RdYlBu"))
res
rquery.cormat(Data1, type="full")

cor.test(Population,Price)
plot(Population, Price , col='red')
ReturnReg1 = lm(Price~Population)
summary(ReturnReg1)
abline(ReturnReg1,col='darkred',lwd=1.5)

error = Data_Test$error

library(readxl)
library(ggplot2)
library(ggfortify)

Data <- read_excel('/Users/apple/Documents/daxong.xlsx')

View(Data)
summary(Data)

ASAP	= Data$ASAP
Bedrooms	= Data$Bedrooms
Area	= Data$Area
Price	= Data$Price
Street_Price	= Data$Street_Price
Population = Data$Population


ASAP = as.factor(ASAP)
District = Data$District
District = as.factor(District)
# Pearson correlation coefficient
cor(Rooms, Price)
cor(Area, Price)
cor(Population, Price)
cor(District_Price, Price)

cor.test(Rooms, Price)
cor.test(Area, Price)
cor.test(Population, Price)
cor.test(District_Price, Price)

ggplot(data=Data, aes(x=District,y=Price, fill=District, col =District)) + geom_boxplot(col='black')+geom_jitter(alpha=0.3)
# Simple Linear Regression
lm(Rooms ~ Price)
reg <- lm(Rooms ~ Price)
summary(reg)
lm(Area ~ Price)
reg <- lm(Area ~ Price)
summary(reg)
reg <- lm(Population ~ Price)
summary(reg)
reg <- lm(District_Price ~ Price)
summary(reg)

ReturnReg<-lm(Price~ASAP+Rooms+Area+District_Price+Population)
autoplot(ReturnReg)
summary(ReturnReg)
#Joint hypothesis testing using the F-statistic


Data <- read_excel('/Users/apple/Documents/daxong.xlsx')

View(Data)
summary(Data)

ASAP	= Data$ASAP
Bedrooms	= Data$Bedrooms
Area	= Data$Area
Price	= Data$Price
Ward_Price	= Data$Ward_Price
Population = Data$Population

Data1 = Data$Price+Data$ASAP+Data$Bedrooms+Data$
res <- cor(Data1)

ReturnReg<-lm(Price~Bedrooms+Area+Population)
autoplot(ReturnReg)
summary(ReturnReg)

ACF(ReturnReg)
library(car)
linearHypothesis(ReturnReg, c("Bedrooms=0", "Area=0","Population=0"))
summary(ReturnReg)$fstatistic
#Perform White's Test: p-value>0.05 -> fail to reject H0 -> Heteroscedasticity is not present in the regression model
library(lmtest)
bptest(ReturnReg, ~ Area*Bedrooms*Population*ASAP*Ward_Price + I(Area^2)+I(Bedrooms^2)+I(Population^2)+I(ASAP^2)+I(Ward_Price^2), data=Data)
#Perform Jarque-Bera test
library(tseries)
library(moments)
jarque.bera.test(Population)
jarque.bera.test(ReturnReg$residuals)
agostino.test(ReturnReg$residuals)
anscombe.test(ReturnReg$residuals)
#Perform Durbin-Watson test for autocorrelation
dwtest(ReturnReg)
# Using White Modified Standard Error Estimates
library(sandwich)
# heteroskedasticity-consistent estimation of the covariance matrix
coeftest(ReturnReg ,vcov. = vcovHC(ReturnReg ,type="HC1"))
# Newey–West heteroscedasticity and autocorrelation
coeftest(ReturnReg ,vcov. = NeweyWest(ReturnReg ,lag = 6,adjust=T,prewhite=F)) #cho nay k hieu vsao lag=6 luon
pacf(Price,lag.max=10)
plot(Area, Price , col='green')
ReturnReg1<-lm(Price~Area)
summary(ReturnReg1)
abline(ReturnReg1,col='darkred',lwd=1.5)
# Augmented Dickey-Fuller test
adf.test(DHP)
