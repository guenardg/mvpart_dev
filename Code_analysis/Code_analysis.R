##
### mvpart code analysis
##
## rm(list=ls())

library(devtools)

install_github("git@github.com:guenardg/mvpart.git")

library(magrittr)
library(stringr)
## library(mvpart) ## detach("package:mvpart",unload=TRUE)
## library(mvpartCLEAN) ## detach("package:mvpartCLEAN",unload=TRUE)

if(FALSE) {
  ## List all C functions that are called by R code:
  
  rdir <- "../package/mvpart/R"
  
  rfiles <- list.files(rdir)
  write.csv(rfiles, file="rfiles.txt")
  
  CLog <- matrix(as.character(),ncol=2L)
  
  ## i=rfiles[4L]
  for(i in rfiles) {
    rdir %>%
      file.path(i) %>%
      scan(
        what = character(),
        sep = "\n",
        quiet = TRUE
      ) %>%
      str_extract(
        pattern = ".C\\([\"'_.a-zA-Z0-9]{0,}"
      ) %>%
      .[!is.na(.)] -> tmp
    if(length(tmp)) {
      sapply(
        tmp,
        function(x) substr(x,5L,nchar(x) - 1L)
      ) %>%
        unname %>%
        {cbind(file=i,cname=.)} %>%
        {rbind(CLog,.)} -> CLog
    }
  }
  
  CallLog <- matrix(as.character(),ncol=2L)
  
  ## i=rfiles[4L]
  for(i in rfiles) {
    rdir %>%
      file.path(i) %>%
      scan(
        what = character(),
        sep = "\n",
        quiet = TRUE
      ) %>%
      str_extract(
        pattern = ".Call\\([\"'._a-zA-Z0-9]{0,}"
      ) %>%
      .[!is.na(.)] -> tmp
    if(length(tmp))
      sapply(
        tmp,
        function(x) substr(x,8L,nchar(x) - 1L)
      ) %>%
      unname %>%
      {cbind(file=i,cname=.)} %>%
      {rbind(CallLog,.)} -> CallLog
  }
  
  rm(i,tmp)
  
  CLog
  CallLog
  
  rm(Clog, CallLog)
}

library(rpart)

## Examples:

## car.test.frame

## Load the data set:
data("car.test.frame")

## Number of rows and columns in the data table:
dim(car.test.frame)

## First six rows of the data table:
head(car.test.frame)

## Summary of the data table:
summary(car.test.frame)

## A few plots of the data within the data frame:
plot(Mileage ~ Weight, data=car.test.frame, las=1L)
plot(Disp. ~ HP, data=car.test.frame, log="xy", las=1L)
plot(Price/1000 ~ Type, data=car.test.frame, las=1L,
     ylab="Price (1000$)", ylim=c(5,30))

## kyphosis

## Load the data set:
data("kyphosis")

## Number of rows and columns in the data table:
dim(kyphosis)

## First six rows of the data table:
head(kyphosis)

## Summary of the data table:
summary(kyphosis)

## A few plots of the data within the data frame:
plot(Number ~ Age, data=kyphosis, las=1L)
plot(Start ~ as.factor(Number), data=kyphosis, las=1L)
plot(Age/12 ~ Kyphosis, data=kyphosis, las=1L, ylab="Age (years)")
plot(Number ~ Kyphosis, data=kyphosis, las=1L)
plot(Age/12 ~ Start, data=kyphosis, las=1L, ylab="Age (years)")

## solder

## Load the data set:
data("solder")

## Number of rows and columns in the data table:
dim(solder)

## First six rows of the data table:
head(solder)

## Summary of the data table:
summary(solder)

## A few plots of the data within the data frame:
plot(skips ~ Opening, data=solder, las=1L)
plot(skips ~ Solder, data=solder, las=1L)
plot(skips ~ Mask, data=solder, las=1L)
plot(skips ~ PadType, data=solder, las=1L)
plot(skips ~ as.factor(Panel), data=solder, las=1L, xlab="Panel number")

## Spider

library(mvpart)

## Load the data set:
data("spider")

## Number of rows and columns in the data table:
dim(spider)

## First six rows of the data table:
head(spider)

## Splitting the table into the responses (species, Y)
## and the descriptors (X):
Y <- spider[,1L:12L]
X <- spider[,13L:18L]

## Summary of the species data table:
summary(Y)

## Summary of the descriptors data table:
summary(X)

## Showing the species data using a principal component analysis:
prY <- princomp(Y)

## Data table of the species loading for the first two principal components:
data.frame(
  AX1 = prY$loadings[,1L],
  AX2 = prY$loadings[,2L]
) -> dat

par(mar=c(4.25,4.25,1.25,1.25))
plot(AX2 ~ AX1, data=dat, asp=1, xlim=c(-0.4,0.8), ylim=c(-0.4,0.8))
abline(v=0, lty=3L)
abline(h=0, lty=3L)
text(x=dat$AX1, y=dat$AX2 + 0.03, labels=colnames(Y), cex=0.75)

## Descriptor: Water
par(mfrow=c(3L,2L), mar=c(4.25,4.25,1.25,1.25))
plot(arct.lute ~ water, data = spider, ylim=c(0,9), pch=21L, bg="red")
plot(pard.lugu ~ water, data = spider, ylim=c(0,9), pch=21L, bg="orange")
plot(zora.spin ~ water, data = spider, ylim=c(0,9), pch=21L, bg="yellow")
plot(aulo.albi ~ water, data = spider, ylim=c(0,9), pch=21L, bg="green")
plot(troc.terr ~ water, data = spider, ylim=c(0,9), pch=21L, bg="blue")
plot(alop.cune ~ water, data = spider, ylim=c(0,9), pch=21L, bg="purple")

## Descriptor: Sand
par(mfrow=c(3L,2L), mar=c(4.25,4.25,1.25,1.25))
plot(arct.lute ~ sand, data = spider, ylim=c(0,9), pch=21L, bg="red")
plot(pard.lugu ~ sand, data = spider, ylim=c(0,9), pch=21L, bg="orange")
plot(zora.spin ~ sand, data = spider, ylim=c(0,9), pch=21L, bg="yellow")
plot(aulo.albi ~ sand, data = spider, ylim=c(0,9), pch=21L, bg="green")
plot(troc.terr ~ sand, data = spider, ylim=c(0,9), pch=21L, bg="blue")
plot(alop.cune ~ sand, data = spider, ylim=c(0,9), pch=21L, bg="purple")

## Examples for rpart:

## Load the car data set:
data("car.test.frame")

## Estimating a regression tree with a single descriptor:
rt.auto1 <- rpart(Mileage ~ Weight, data=car.test.frame)

## Print the model:
rt.auto1

## Summarize the model:
summary(rt.auto1)

## Plotting the model:
par(mar=c(1,1,1,1))
plot(rt.auto1)

## Showing the tree labels
text(rt.auto1)

## Load the spider data set:
data(spider)

# Splitting the table into the responses (species, Y)
## and the descriptors (X):
Y <- data.matrix(spider[,1L:12L])
X <- spider[,13L:18L]
## Note: the multivariate response needs to procesed using function data.matrix
## before being used as a multivariate response by function rpart.

## Estimating a multivariate regression tree with multiple descriptors:
rt.spider1 <- rpart(Y ~ water + twigs + reft + herbs + moss + sand,
                    data = X, method="mrt")

## Summarized the multivariate regression tree
summary(rt.spider1)

## Plotting the model with the labels
par(mar=c(1,1,1,1))
plot(rt.spider1)
text(rt.spider1)

## Estimating a multivariate regression tree, using the Manhattan dissimilarity
## metric, with multiple descriptors
rt.spider2 <- rpart(Y ~ water + twigs + reft + herbs + moss + sand,
                    data = X, method="mrt", dissim="man")

## Summarized the multivariate regression tree
summary(rt.spider2)

## Plotting the model with the labels
par(mar=c(1,1,1,1))
plot(rt.spider2)
text(rt.spider2)

## Transforming the response on the basis of the Bray-Curtis dissimilarity:
Y_bray <- gdist(spider[,1L:12L], method="bray", full=TRUE, sq=TRUE)

## Estimating a multivariate regression tree, using method "dist"
rt.spider3 <- rpart(Y_bray ~ water + twigs + reft + herbs + moss + sand,
                    data = X, method="dist")

## Summarized the multivariate regression tree
summary(rt.spider3)

## Plotting the model with the labels
par(mar=c(1,1,1,1))
plot(rt.spider3)
text(rt.spider3)

### Fitted values and predictions:

## Obtain the fitted values and residuals:
pred.auto1 <- predict(rt.auto1)
res.auto1 <- residuals(rt.auto1)

## Plotting the fitted car mileage with respect to the observed values:
rng <- range(car.test.frame$Mileage, pred.auto1)  ## Same range.

par(mar=c(4.25,4.25,1.25,1.25))
plot(x = car.test.frame$Mileage, y = pred.auto1, xlim=rng, ylim=rng, asp=1)
abline(0,1)

## 
rng <- max(abs(res.auto1))*c(-1,1)  ## Symmetric range for the residuals
plot(x = pred.auto1, y = res.auto1, ylim=rng, xlab="Fitted",
     ylab="Residuals")
abline(h=0, lty=3L)

## A classification tree using a binary response variable:
data(kyphosis)
Kyp1 <- rpart(Kyphosis ~ Age + Number + Start, data=kyphosis)

## Predictions as class probabilities (the default):
Kyp1.prob <- predict(Kyp1, type="prob")
plot(x = kyphosis$Kyphosis, y = Kyp1.prob[,"present"])

## Predictions as level numbers:
par(mar=c(4.25,4.25,1.25,4.25))
Kyp1.vect <- predict(Kyp1, type="vector")
plot(x = kyphosis$Kyphosis, y = as.factor(Kyp1.vect))

## Predictions as factors
Kyp1.fact <- predict(Kyp1, type="class")
plot(x = kyphosis$Kyphosis, y = Kyp1.fact)

## Predictions as level number, class frequencies, and probabilities:
Kyp1.matr <- predict(Kyp1, type="matrix")
head(Kyp1.matr, n=10L)  ## The first ten predictions

## Example of external validation using a subset:
data(iris)

set.seed(1234567L)

iris.sub <- c(sample(1L:50L, 25L),sample(51L:100L,25L),sample(101L:150L,25L))
iris1 <- rpart(Species ~ ., data=iris, subset=iris.sub)

iris1.out <- predict(iris1, iris[-iris.sub,], type="class")

table(iris1.out, iris[-iris.sub, "Species"])

## Regression examples with mostly categorical descriptors:
data(solder)
solder1 <- rpart(skips ~ Opening + Solder + Mask + PadType + as.factor(Panel),
                 data = solder, method="anova")

solder1

summary(solder1)

solder1.fit <- predict(solder1)
solder1.res <- residuals(solder1)

rng <- max(abs(solder1.res))*c(-1,1)  ## Symmetric range for the residuals
plot(x = solder1.fit, y = solder1.res, ylim=rng, xlab="Fitted",
     ylab="Residuals")
abline(h=0, lty=3L)

## R-square plots:
rsq(rt.auto1)
rsq(Kyp1)
rsq(rt.spider1)
rsq(rt.spider2)
## rsq(rt.spider3)  ## Won't work for method = "dist"
rsq(iris1)
rsq(solder1)

## Mean and variance plots (for regression trees only):
meanvar(rt.auto1)
meanvar(solder1)


### mvpart examples:

## Load the spider data set:
data(spider)

# Splitting the table into the responses (species, Y)
## and the descriptors (X):
Y <- data.matrix(spider[,1L:12L])
X <- spider[,13L:18L]
## Note: the multivariate response needs to procesed using function data.matrix
## before being used as a multivariate response by function rpart.

## Multivariate regression tree on the spider species response matrix and all
## the descriptors in the spider data set.

## Using the default settings:
mvpart(Y ~ herbs + reft + moss + sand + twigs + water, data=X)

## Pick-the optimal tree size using cross-validation:
mvpart(Y ~ herbs + reft + moss + sand + twigs + water, data=X, xv="p")

## Pick-the optimal tree size and generate a PCA plot (interactive):
spider1 <- mvpart(Y ~ herbs + reft + moss + sand + twigs + water, data=X,
                  xv="1se", pca=TRUE)

## interactive PCA plot of saved multivariate tree
rpart.pca(spider1, interact=TRUE, wgt.ave=TRUE)


### rpart.pca examples:

## Load the spider data set:
data(spider)

## Splitting the table into the responses (species, Y)
## and the descriptors (X):
Y <- data.matrix(spider[,1L:12L])
X <- spider[,13L:18L]
## Note: the multivariate response needs to procesed using function data.matrix
## before being used as a multivariate response by function rpart.

## Multivariate regression tree on the spider species response matrix and all
## the descriptors in the spider data set.

## Using the default settings:
spider1 <- mvpart(Y ~ herbs + reft + moss + sand + twigs + water, data=X)

## Non-interactive PCA plot:
rpart.pca(spider1)

## Interactive PCA plot:
rpart.pca(spider1, wgt.ave=TRUE, interact=TRUE)


### printcp examples

## Load the car data set:
data("car.test.frame")

## Estimating a regression tree with a single descriptor:
rt.auto1 <- rpart(Mileage ~ Weight, data=car.test.frame)

## Print the model:
rt.auto1

## Print the cp table:
printcp(rt.auto1)

### xdiss examples:

## Load the spider data set:
data(spider)

## Getting the table of the first six species:
Y <- data.matrix(spider[1L:6L,1L:12L])

## Calculate the dissimilarity measures:
xdiss(Y, method="manhattan")  ## Default
xdiss(Y, method="euclidean")
xdiss(Y, method="canberra")
xdiss(Y, method="bray")
xdiss(Y, method="kulczynski")
xdiss(Y, method="gower")
xdiss(Y, method="maximum")
xdiss(Y, method="binary")
xdiss(Y, method="chisq")
xdiss(Y, method="chord")

## Rendu ici...
