 BTEC330 F2019 Project2 Loic soumani
## AnalyzeIL10 by Loic Soumani

### Single and multiple regressions, and scatterplots for clinical IL10 and gene expression data.
([AnalyzeIL10.R](../master/AnalyzeIL10.R)) will allow you to load a comma-delimited .csv with various datapoints, perform single and multiple regressions of Body Mass Index (BMI) vs. variables from the Complete Blood Count with Differential (CBC-D) results, and produce 2-D and 3-D scatterplots for the results. 

## IL10 parameter:
 Interleukin-10 (encoded by IL10) is an important regulatory cytokine with both immunosuppressive and immunostimulatory properties. It can inhibit the functions of T cells and antigen presenting cells (APCs) but promotes B cell–mediated functions, enhancing survival, proliferation, differentiation, and antibody production.It is a key anti-inflammatory cytokine that can inhibit proinflammatory responses of both innate and adaptive immune cells which means impeding pathogen clearance and ameliorating immunopathology.
  ### Kapsenberg, M. L.. 2003. Dendritic-cell control of pathogen-driven T-cell polarization. Nat. Rev. Immunol. 3: 984-993 
  ### /https://www.sciencedirect.com/topics/neuroscience/interleukin-10 
  
 ## Install necessary packages
install.packages("ggplot2")
library(ggplot2)

## Read data
IBS <- read.csv("data/RobinsonEtAl_Sup1.csv", header = TRUE)
head(IBS)
write.csv(IBS, "data_output/IL10.csv")
##  Single Regressions 
##  Data obtained from Robinson, et al. 2019 (doi: https://doi.org/10.1101/608208)
##  https://statquest.org/2017/10/30/statquest-multiple-regression-in-r/
##  http://www.sthda.com/english/articles/40-regression-analysis/167-simple-linear-regression-in-r/
##  http://r-statistics.co/Linear-Regression.html

## Single Regression Test, BMI vs. IL10 parameter
IL10.regression <- lm(BMI ~ IL10, data = IBS)
summary(IL10.regression)
## Output the results to a file
## http://www.cookbook-r.com/Data_input_and_output/Writing_text_and_output_from_analyses_to_a_file/
sink('data_output/IL10_regression.txt', append = TRUE)
print(IL10.regression)
sink()

## ANOVA: IBS-subtype vs. IL10 parameter

## http://www.sthda.com/english/wiki/one-way-anova-test-in-r
IL10.aov <- aov(IL10 ~ IBS.subtype, data = IBS)
summary(IL10.aov)
sink('data_output/IL10_anova.txt', append = TRUE)
print(IL10.aov)
sink()
## Print scatterplot and box plots as .png files into "fig_output" project directory.
## http://www.sthda.com/english/wiki/ggsave-save-a-ggplot-r-software-and-data-visualization

## Scatterplots

## https://www.statmethods.net/graphs/scatterplot.html

ggplot(IBS, aes(x = BMI, y = IL10)) +
  geom_point() +    
  geom_smooth(method = lm) 

png("fig_output/IL10_scatterplot.png")
IL10_scatterplot <- ggplot(IBS, aes(x = BMI, y = IL10)) +
  geom_point() +    
  geom_smooth(method = lm) 

print(IL10_scatterplot)
dev.off()

## Box plots
## https://www.statmethods.net/graphs/boxplot.html

boxplot(IL10 ~ IBS.subtype, data = IBS, main="IL10 by IBS subtype", 
        xlab = "IBS.subtype", ylab = "IL10"
)

png("fig_output/IL10_boxplot.png")
IL10_boxplot <- boxplot(IL10 ~ IBS.subtype, data = IBS, main="IL10 by IBS subtype", 
                       xlab = "IBS.subtype", ylab = "IL10"
)
print(IL10_boxplot)
dev.off()

IBS$IL10_result <- "NA"

## Assign "HIGH", "NORMAL", or "LOW" based on clinical range to the LDH_result parameter

IBS$IL10_result[IBS$IL10 > 4] <- "HIGH"

IBS$IL10_result[IBS$IL10 <= 4 & IBS$IL10 >= 0.25] <- "NORMAL"

IBS$IL10_result[IBS$IL10 < 0.25] <- "LOW"

write.csv(IBS, "data_output/IL10_result.csv")

### Results of single regression, BMI x IL10 scatterplot

> single.regression <- lm(BMI ~ IL10, data=IBS1)
> print(single.regression)

Call:
lm(formula = BMI ~ IL10, data = IBS1)

Coefficients:
  (Intercept)         IL10  
      31.9454        -0.5004  


ggplot(IBS1, aes(x=BMI, y=IL10)) +
  geom_point() +    
  geom_smooth(method=lm) 

![BMI_Cortisol](../master/Images/CORTxBMI.png?sanitize=true)

### Results of single regression, BMI x C-Reactive Protein (CRP)

> single.regression <- lm(BMI ~ CRP, data=IBS1)
> print(single.regression)

Call:
lm(formula = BMI ~ IL10 + CRP, data = IBS1)

Coefficients:
  (Intercept)          IL10            CRP  
      30.7936        -0.5231         0.6042  



![BMI_CRP](../master/Images/BMIxCRP.png?sanitize=true)
##
##
### Results of multiple regression, BMI x IL10 + C-Reactive Protein (CRP)

> fit1 <- lm(BMI ~ IL10 + CRP, data=IBS1)
> summary(fit1)

Call:
lm(formula = BMI ~ IL10 + CRP, data = IBS1)

Residuals:
    Min      1Q  Median      3Q     Max 
-9.1378 -3.4448 -0.9904  2.3330 20.6056 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)    30.7936     1.4134  21.787  < 2e-16 ***
 IL10            -0.5231     0.1233  -4.244 4.72e-05 ***
CRP             0.6042     0.1534   3.938 0.000147 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 5.354 on 106 degrees of freedom
  (2 observations deleted due to missingness)
Multiple R-squared:  0.232,	Adjusted R-squared:  0.2175 
F-statistic: 16.01 on 2 and 106 DF,  p-value: 8.388e-07

s3d <- scatterplot3d(IBS$BMI, IBS$IL10, IBS$CRP,  pch=16, color="steelblue", box="TRUE", highlight.3d=FALSE, type="h", main="BMI x IL10 x CRP")
fit <- lm(IL10 ~ BMI + CRP, data=IBS)
s3d$plane3d(fit)

![BMI_Cortisol_CRP_3d-scatterplot](../master/Images/MultipleRegression_3way.png?sanitize=true)

## RESULT OF SINGLE REGRESSION BMI, IL10 BOXPLOT FROM ANOVA

ggplot(IBS, aes(x = BMI, y = IL10)) + geom_point() + geom_smooth(method = lm) 
> png("fig_output/IL10_boxplot.png")
> Lymphocytes_boxplot <- ggplot(IBS, aes(x = BMI, y = IL10)) + geom_point() + geom_smooth(method = lm) 
> print(IL10_boxplot)

Call:
lm(formula = BMI ~ IL10, data = IBS1)

Coefficients:
(Intercept)         IL10 
     31.9454        -0.5004
