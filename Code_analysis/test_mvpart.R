
library(mvpart)
detach("package:mvpart", unload=TRUE)
## rm(list=ls())

## Load the spider data set:
data(spider)

## Splitting the table into the responses (species, Y)
## and the descriptors (X):
Y <- data.matrix(spider[, 1L:12L])
X <- spider[, 13L:18L]
## Note: the multivariate response needs to process using function
## data.matrix before being used as a multivariate response by function
## rpart.

## Multivariate regression tree on the spider species response matrix and all
## the descriptors in the spider data set.

## Using the default settings:
mvpart(Y ~ herbs + reft + moss + sand + twigs + water, data = X)

## Pick-the optimal tree size using cross-validation:
mvpart(Y ~ herbs + reft + moss + sand + twigs + water, data = X, xv = "p")

## Pick-the optimal tree size and generate a PCA plot (interactive):
spider1 <- mvpart(Y ~ herbs + reft + moss + sand + twigs + water, data = X,
                  xv = "1se", pca = TRUE)
summary(spider1)



mvpart:::formatg(ff$yval2[rows,], digits - 3L)


## object <- spider1

## interactive PCA plot of saved multivariate tree
rpart.pca(spider1, interact = TRUE, wgt.ave = TRUE)
