specaccum(comm, method = "exact", permutations = 100,
          conditioned =TRUE, gamma = "jack1", w = NULL, subset, ...)
?BCI
?specaccum
library(vegan)

data(BCI) #load example dataset: A data frame with 50 plots (rows) of 1 hectare with
#counts of trees on each plot with total of 225 species (columns). 
#SO FOR MY DATA IT WOULD BE COLUMNS: INDIVIDUALS & ROWS: DAYS & COUNTS: NUMBER OF
# INDEPENDENT EVENTS

sp1 <- specaccum(BCI)
sp2 <- specaccum(BCI, "random")
sp2
summary(sp2)
plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sp2, col="yellow", add=TRUE, pch="+")

#######################


