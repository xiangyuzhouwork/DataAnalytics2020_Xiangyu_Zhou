EPI_data <- read.csv('C:\\Users\\xiangyu zhou\\Desktop\\data analytics\\9.11\\EPI_data.csv',header=TRUE)
View(EPI_data)
attach(EPI_data) 
fix(EPI_data) 
EPI <- EPI_data$EPI
EPI

plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 
help("qqnorm")
par(pty="s") 
qqnorm(EPI); qqline(EPI)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

plot(ecdf(EPI_data$EPI),do.points=FALSE,verticals = TRUE) 
plot(ecdf(EPI_data$EPI),do.points=TRUE,verticals = TRUE)
help("qqnorm")
help("qqplot") 
qqnorm(EPI_data$EPI)
qqline(EPI_data$EPI) 
x <- seq(30,95,1)
x
x2 <-seq(30,95,2)
x2
x2 <-seq(30,96,2)
x2
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)


multivariate <- read.csv('C:\\Users\\xiangyu zhou\\Desktop\\data analytics\\9.22\\multivariate.csv',header=TRUE)
head(multivariate)
attach(multivariate)
help(lm)
mm <-lm(Homeowners~Immigrant)
summary(mm)
plot(Homeowners~Immigrant)
help(abline)
abline(mm)
abline(mm,col=2,lwd=3)
newImmigrantdata <- data.frame(Immigrant = c(0,  20))
predict(mm,newImmigrantdata)
abline(mm)
abline(mm,col=3,lwd=3)
attributes(mm)
mm$coefficients

plot(mtcars$wt,mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data = mtcars)
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()
plot(pressure$temperature , pressure$pressure , type ="l")
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature,pressure$pressure/2,col="red")
points(pressure$temperature,pressure$pressure/2,col="blue")
library(ggplot2)
qplot(pressure$temperature,pressure$pressure,geom = "line")
qplot(temperature,pressure,data=pressure,geom="line")
ggplot(pressure,aes(x=temperature,y=pressure))+geom_line()+geom_point()

barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))
qplot(factor(mtcars$cyl))
qplot(factor(cyl),data = mtcars)
ggplot(mtcars,aes(x=factor(cyl)))+geom_bar()


hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10)
hist(mtcars$mpg, breaks = 5)
hist(mtcars$mpg, breaks = 12)
qplot(mpg,data = mtcars, binwidth = 4)
ggplot(mtcars, aes(x=mpg))+geom_histogram(binwidth = 4)
ggplot(mtcars, aes(x=mpg))+geom_histogram(binwidth = 5)

plot(ToothGrowth$supp,ToothGrowth$len)
boxplot(len~supp,data = ToothGrowth)
boxplot(len~supp+dose,data = ToothGrowth)
library(ggplot2)
qplot(ToothGrowth$supp,ToothGrowth$len,geom = "boxplot")
qplot(supp,len,data = ToothGrowth,geom = "boxplot")
ggplot(ToothGrowth,aes(x=supp,y=len))+geom_boxplot()
qplot(interaction(ToothGrowth$supp,ToothGrowth$dose), ToothGrowth$len, geom="boxplot")
qplot(interaction(supp,dose),len,data=ToothGrowth,geom = "boxplot")
ggplot(ToothGrowth,aes(x=interaction(supp,dose),y=len))+geom_boxplot()


