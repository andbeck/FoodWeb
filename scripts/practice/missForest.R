## using the missForest package
library(missForest)
data("iris")
data("esoph")

summary(iris)
# artifiicially remove 10% of the entries using prodNA()
iris.mis <- prodNA(iris, noNA = 0.1)
summary(iris.mis) # Na values present and evenly spread
# now impute the data using missForest
iris.imp <- missForest(iris.mis)
# now call upon iris.imp$ximp for the imputed dataset
summary(iris.imp$ximp)


