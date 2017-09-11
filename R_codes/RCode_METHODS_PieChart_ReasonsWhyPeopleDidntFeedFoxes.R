mydata <- read.table(file = "Reasons why people didnt feed foxes.txt", sep='\t', header = TRUE) #import dataset

c1<-(mydata$Reason)
c2<-(mydata$Percentage)
N<-(mydata$N)
lbls <- (mydata$Reason)
colours <- c("white","grey90", "grey80", "grey70", "grey60", "grey50", "grey40", "grey30", "grey20", "grey10", "black") #create colour palette for B&W printing


### PLOT 3: LABELS ARE % ONLY, LEGEND IS CATEGORY AND SAMPLE SIZE IN ITALICS & BRACKETS ###
lbls <- paste(c2, "%", sep="") # ad percents and % sign to labels 
pie(c2, col=colours, labels = lbls, 
    main="Reasons why people didnt feed foxes", cex=1)
temp <- legend(1.15, 0.4, legend = c1,
               text.width = max(strwidth(paste(c1, N, " ()"))),
               fill=colours, bty="n", xjust = 0, yjust = 1)
text(temp$text$x +strwidth(c1) + strwidth(" "), temp$text$y,
     paste0("(", "N=", N, ")"), font=3, adj=c(0,.5)) #bty="n" gets rid of frame around legend
#font=3 means italic, font=1 is normal, font=2 is bold.

