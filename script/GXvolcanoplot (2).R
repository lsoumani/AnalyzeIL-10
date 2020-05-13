## BTEC395 S2020 GX LOIC SOUMANI


## Set working directory to source file location


## Read in the table of fold changes
FCdata <- read.csv("../data/FC.csv", row.names = 1, header = FALSE)

## Read in the table of expression data
IBS <- read.csv("../data/GXdata.csv", header = TRUE)

## Access only the columns with RNA Expression (subsetting)
names(IBS)[28:277]

## Make a list of anova(lm()) results for bloodwork parameter
storage <- list()

for(i in names(IBS)[28:277]){
  storage[[i]]  <- anova(lm(get(i) ~ IL10, IBS))
}

## Extract the p-values into a new list
pVals <- list()

for(i in names(storage)){
  pVals[[i]] <- -(log10(storage[[i]]$'Pr(>F)'))
}

## Convert the pValues list into a data frame. 

DFpvalues <- data.frame(matrix(unlist(pVals), nrow=length(pVals), byrow=T))

## Combine the results dataframes and write column labels

VolcanoPlotData <- cbind(FCdata, DFpvalues)
names(VolcanoPlotData)[1] <- paste("log2(slopeDiff)")
names(VolcanoPlotData)[2] <- paste("-log10(Pval)")

## Add a column to evaluate significance

VolcanoPlotData$Sig <- ifelse(VolcanoPlotData$`-log10(Pval)` > 1.3, "Sig", "Insig");
  
## Make a volcano-style scatterplot for these results

install.packages("ggplot2")
library(ggplot2)
# library(ggrepel)

png("../fig_output/IL10plot.png")
IL10plot <- ggplot(VolcanoPlotData, aes(x = `log2(slopeDiff)`, y = `-log10(Pval)`, label=rownames(VolcanoPlotData), color=Sig)) +
  geom_point(aes(color = Sig)) +
  scale_color_manual(values = c("grey", "red")) +
  theme_bw(base_size = 12) + theme(legend.position = "bottom") +
  geom_text(aes(x = `log2(slopeDiff)`,y = `-log10(Pval)`, fontface = 1, size=3,  label=row.names(VolcanoPlotData)))

print(IL10plot + ggtitle("Gene Expression vs. IL-10 Level"))
dev.off()



