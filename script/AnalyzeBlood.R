## BTEC330 loic soumani project2
## Install necessary packages


install.packages("ggplot2")
library(ggplot2)

install.packages("scatterplot3d")
library(scatterplot3d)

## Read data
IBS1 <- read.csv("data/RobinsonEtAl_Sup1.csv", header = TRUE)
head(IBS1)
write.csv(IBS1, "data_output/output.csv")

##  Single Regressions for BMI vs. IL10
##  Data was obtained from Robinson, et al. 2019 (doi: https://doi.org/10.1101/608208)
##  https://statquest.org/2017/10/30/statquest-multiple-regression-in-r/
##  http://www.sthda.com/english/articles/40-regression-analysis/167-simple-linear-regression-in-r/
##  http://r-statistics.co/Linear-Regression.html

## single regression test
single.regression <- lm(BMI ~ IL10 , data=IBS1)
summary(single.regression)



## Scatterplots
## https://www.statmethods.net/graphs/scatterplot.html


ggplot(IBS1, aes(x=BMI, y=IL10)) +
  geom_point() +    
  geom_smooth(method=lm) 

ggplot(IBS1, aes(x=BMI, y=IL10)) +
  geom_point() +    
  geom_smooth(method=lm)  

## http://www.sthda.com/english/wiki/scatterplot3d-3d-graphics-r-software-and-data-visualization

