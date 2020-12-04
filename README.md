# Project report of the dataset <i>mtcars</i> <br><p> Dany SONETHAVY | Jérémy TANG</p>

<p> First application of studying the dataset mtcars using our course.</p>

## Summary

1. [Data Exploration](##-1.-Data-Exploration)
2. [Data Visualization](##-2.-Data-Visualization)
3. [Testing Hypothesis](##-3.-Test-Hypothesis)
4. [Fitting Model](##-4.-Fitting-Model)


## 1. Data Exploration 

<p> Firstly, we select the dataset and we display the summary of the dataset <i>mtcars</i>. </p>

```r
data(mtcars)
attach(mtcars)

#need to install packages ggplot2 and ggpubr 
theme_set(theme_pubr())

#Data Exploration
names(mtcars)
summary(mtcars)
```



## 2. Data Visualization

<p>We are making multiple charts in order to understand the role of each variable and to make our coming hypothesis.</p>

```R
##Automatic vs Manual (figure 1)
typeT <- c("Automatic","Manual")
motors <- table(mtcars$am)
piepercent<- round(100*motors/sum(motors), 1)
pie(x=motors,
    main ="Motor type pie chart (%)",
    labels = piepercent,
    col = c("#6FD08C","#FBA04B"))

legend("topright", c("Automatic","Manual"), cex = 0.8,
       fill = c("#6FD08C","#FBA04B"))


## Miles/gallon histograms according to the number of cylinders (4,6,8) (figure 2)
par(mfrow=c(1,3))
attach(mtcars)
vals <- tapply(mtcars[mtcars$cyl==4,]$mpg,mtcars[mtcars$cyl==4,]$am,mean)
bp<-barplot(vals,
            col =c("#FFDDD2","#D2DEFF"),
            main ="Miles/gallon histogram with 4 cylinders ",
            names.arg=c("Automatic","Manual"),
            xlab = "Type of transmission",
            ylim=c(0,30))
text(bp,10,round(vals,1), pos=3)

attach(mtcars)
vals <- tapply(mtcars[mtcars$cyl==6,]$mpg,mtcars[mtcars$cyl==6,]$am,mean)
bp<-barplot(vals,
            col =c("#FFDDD2","#D2DEFF"),
            main ="Miles/gallon histogram with 6 cylinders ",
            names.arg=c("Automatic","Manual"),
            xlab = "Type of transmission",
            ylim=c(0,22))
text(bp,10,round(vals,1), pos=3)

attach(mtcars)
vals <- tapply(mtcars[mtcars$cyl==8,]$mpg,mtcars[mtcars$cyl==8,]$am,mean)
bp<-barplot(vals,
            col =c("#FFDDD2","#D2DEFF"),
            main ="Miles/gallon histogram with 8 cylinders ",
            names.arg=c("Automatic","Manual"),
            xlab = "Type of transmission",
            ylim=c(0,18))
text(bp,10,round(vals,1), pos=3)
par(mfrow=c(1,1))

##Boxplot mpg~am (figure 3)
boxplot(mpg~am,data=mtcars, main="Miles per gallon according to the type of motor",
        xlab="Type of motor", ylab="Miles Per Gallon", names= c("Automatic","Manual"), col = "#ED4A31") 

##Density~mpg(Number of cylinders= {4,6,8}) (figure 4)
a <- ggplot(data = mtcars, aes(x = mpg, color = cyl)) + geom_histogram(aes(y = ..density..), 
                                                                       colour="black", fill="white") +
    geom_density(alpha = 0.3, fill = "#FB4D3D") + facet_grid(vars(cyl)) + labs(x ="Miles per Gallon (mpg)", y="Density")
a

##Boxplot mpg~cyl (figure 5)
boxplot(mpg~cyl,data=mtcars, main="Miles per gallon according to the number of cylinders",
        xlab="Number of cylinders", ylab="Miles Per Gallon", names= c("4","6","8"), col = "#ED4A31") 

#Plot mpg~wt(Number of cylinders= {4,6,8}) (figure 6)
mt <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
    geom_point()

mt + facet_grid(vars(cyl), scales = "free") + labs(x="Weight", y="Miles per Gallon (mpg)", colour = "Number of cylinders") +
    stat_ellipse()

#Plot mpg~hp(Number of cylinders= {4,6,8}) (figure 7)
mt <- ggplot(mtcars, aes(hp, mpg, colour = factor(cyl))) +
    geom_point()

mt + facet_grid(vars(cyl), scales = "free") + labs(x="Horsepower", y="Miles per Gallon", colour = "Number of cylinders : ") +
    stat_ellipse()+ ggtitle("Miles per Gallon/Horsepower according to the number of cylinders")

#Boxplot mpg~vs (figure 8)
boxplot(mpg~vs,data=mtcars, main="Miles per Gallon according to the engine",
        xlab="Engine", ylab="Miles Per Gallon", names=c("V-shaped","Straight"), col = "#ED4A31") 

##Plot Miles per Gallon/Horsepower according to the Engine (figure 9)
mt <- ggplot(mtcars, aes(hp, mpg, colour = factor(vs))) +
    geom_point()

mt + facet_grid(vars(vs), scales = "free") + labs(x="Horsepower", y="Miles per Gallon", colour = "0 : V-shaped | 1 : Straight") +
    stat_ellipse()+ ggtitle("Miles per Gallon/Horsepower according to the Engine")

```

## 3. Testing Hypothesis

<p> We noticed that the weight of a car seems to be related to the miles/gallon so we decided to deal with these variables in depth.
</p>


```R
# Testing hypothesis

## Looking for further information between miles/gallon and weight
cor.test(mtcars$wt, mtcars$mpg)

### We found a correlation coefficient almost equal to -0.87 (close to -1) which demonstrates a true relationship between the
### weight and the miles/gallon.
```

<p>After that we decided to display the plot using <i>x = weight</i> and <i>y = mpg</i> .</p>

```R
## Display the plot using x = weight and y = miles/gallon (figure 10)
plot(mpg ~ wt,main="Graph miles per gallon - weight", xlab="Weight", ylab="Miles per gallon")



### This plot shows a strong, negative, linear association between weight and miles per gallon which is coherent with the
### value of the correlation coefficient .
```

Finally we made two hypothesis :
    • H0 (null hypothesis) : Heavier cars consume more fuels.
    • H1 (alternative hypothesis) : Heavier cars consume less or same amount of fuel than the others.

## 4. Fitting Model

It fits with linear models, we used 2 different ways to implement the linear regression.

```R
#Fitting model

## Using the abline function
plot(mpg ~ wt,main="Graph miles per gallon - weight", xlab="Weight", ylab="Miles per gallon")
abline(lm(mpg ~ wt))

## Plot mpg~wt (figure 13)
ggplot(mtcars, aes(x=wt, y=mpg)) + 
    geom_point()+
    geom_smooth(method=lm, se=FALSE) + labs(x="Weight(1000 lbs)", y="Miles per Gallon (mpg)") + ggtitle("Linear regression mpg/weight")

fit <- lm(mpg ~ wt, mtcars) #Execute the linear regression
coef(fit) # Calculate the slope and the intercept of the line

R_sq <- cor(mtcars$wt, mtcars$mpg)^2 #Calculate R-square the coefficient of determination
R_sq

## Plot wt~mpg(Number of cylinders= {4,6,8}) + linear regression (figure 14)
mtcars$cyl <- as.factor(mtcars$cyl)
ggplot(mtcars, aes(x=wt, y=mpg, color = cyl)) + 
    geom_point()+
    geom_smooth(method=lm, se=FALSE) + labs(color = "Number of cylinders", x="Weight(1000 lbs)", y="Miles per Gallon (mpg)")+ ggtitle("Linear regression mpg/weight")

fit1 <- lm(mtcars[mtcars$cyl==4,]$mpg ~ mtcars[mtcars$cyl==4,]$wt, mtcars) #Execute the linear regression #1
coef(fit1) # Calculate the slope and the intercept of the line #1
R_sq1 <- cor(mtcars[mtcars$cyl==4,]$wt, mtcars[mtcars$cyl==4,]$mpg)^2 #Calculate R-square the coefficient of determination #1
R_sq1
fit2 <- lm(mtcars[mtcars$cyl==6,]$mpg ~ mtcars[mtcars$cyl==6,]$wt, mtcars) #Execute the linear regression #2
coef(fit2) # Calculate the slope and the intercept of the line #2
R_sq2 <- cor(mtcars[mtcars$cyl==6,]$wt, mtcars[mtcars$cyl==6,]$mpg)^2 #Calculate R-square the coefficient of determination #2
R_sq2
fit3 <- lm(mtcars[mtcars$cyl==8,]$mpg ~ mtcars[mtcars$cyl==8,]$wt, mtcars) #Execute the linear regression #3
coef(fit3) # Calculate the slope and the intercept of the line #3
R_sq3 <- cor(mtcars[mtcars$cyl==8,]$wt, mtcars[mtcars$cyl==8,]$mpg)^2 #Calculate R-square the coefficient of determination #3
R_sq3

## Using the scatter.smooth function
scatter.smooth(x=mtcars$wt, y=mtcars$mpg, main="Graph miles per gallon - weight",xlab ="Weight",ylab="Miles per gallon")
```


