data("iris")
str(iris)
summary(iris)
set.seed(111)
ind <- sample(2, nrow(iris),
              replace = TRUE,
              prob = c(0.8, 0.2))
training <- iris[ind==1,]
testing <- iris[ind==2,]
library(psych)
pairs.panels(training[,-5],
             gap = 0,
             bg = c("red", "yellow", "blue")[training$Species],
             pch=21)
pc <- prcomp(training[,-5],
             center = TRUE,
             scale. = TRUE)
attributes(pc)
pc$center
pc$scale
print(pc)
summary(pc)
pairs.panels(pc$x,
             gap=0,
             bg = c("red", "yellow", "blue")[training$Species],
             pch=21)
library(devtools)
remotes::install_github("vqv/ggbiplot", force=TRUE)
library(ggbiplot)
g <- ggbiplot(pc,
              obs.scale = 1,
              var.scale = 1,
              groups = training$Species,
              ellipse = TRUE,
              circle = TRUE,
              ellipse.prob = 0.68)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)
trg <- predict(pc, training)
trg <- data.frame(trg, training[5])

tst <- predict(pc, testing)
tst <- data.frame(tst, testing[5])
library(nnet)
trg$Species <- relevel(trg$Species, ref = "setosa")
mymodel <- multinom(Species~PC1+PC2, data = trg)
summary(mymodel)
p <- predict(mymodel, trg)
tab <- table(p, trg$Species)
tab
p1 <- predict(mymodel, tst)
print(tst)
tab1 <- table(p1, tst$Species)
tab1

new_row <- data.frame( Sepal.Length = 5.2, Sepal.Width = 3.3, Petal.Length = 3.9 , Petal.Width = 1.4)
new_row <- predict(pc, new_row)
print(new_row)

p2 <- predict(mymodel, new_row)
p2

