setwd("E:/Statistics 2016/Methods chapter - stats and plots/Methods - Unidentified categories - 030715")

mydata <- read.table(file = "UnidentifiedCategs.txt", sep='\t', header = TRUE) #import dataset

str(mydata)
sum(mydata$total)

#make basic pie chart
c1<-(mydata$Reason)
c2<-(mydata$total)
lbls <- (mydata$Reason)
pie(c2, labels = lbls, main="Reasons fox photos could not be individually identified")


### PLOT 1: LABELS ARE CATEGORY AND %, LEGEND IS SAMPLE SIZE###
c2<-(mydata$total)
pct <- round(c2/sum(c2)*100) #calculate percentages
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls, "%", sep="") # ad % to labels 
colours <- c("white","grey70","grey40","black") #create colour palette for B&W printing

pie(c2, col=colours, labels = lbls, 
    main="Reasons fox photos could not be individually identified", cex=1)
legend(1.25, 0.3, c2, cex=0.9, 
       fill=colours)


### PLOT 2: LABELS ARE % and SAMPLE SIZE, LEGEND IS CATEGORY ###
lbls <- paste(pct, "%, ", "N=", c2, sep="") # ad percents and % sign to labels 
pie(c2, col=colours, labels = lbls, 
    main="Reasons fox photos could not be individually identified", cex=1)
legend(1.25, 0.3, c2, cex=0.9, 
       fill=colours)


### PLOT 3: LABELS ARE % ONLY, LEGEND IS CATEGORY AND SAMPLE SIZE IN ITALICS & BRACKETS ###
lbls <- paste(pct, "%", sep="") # add percents and % sign to labels 
pie(c2, col=colours, labels = lbls, 
    #main="Reasons fox photos could not be individually identified", cex=1
    )
temp <- legend(1, 0.3, legend = c1,
               text.width = max(strwidth(paste(c1, c2, " ()"))),
               fill=colours, bty="n", xjust = 0, yjust = 1)
text(temp$text$x +strwidth(c1) + strwidth(" "), temp$text$y,
     paste0("(", "N=", c2, ")"), font=3, adj=c(0,.5)) #bty="n" gets rid of frame around legend
                                                      #font=3 means italic, font=1 is normal, font=2 is bold.

# saves most recently drawn plot as the following file name in the current working directory
dev.print(jpeg, "Rplot_Reasons photos unidentified - new corrected percentages.jpeg", res=700, 
          height=6, width=10.5, units="in") # save as jpeg



#to make text bold or italic
pie(c2, xlab= expression(bla~italic(bli)~bla~bold(blom)~italic(bla)))
legend('topleft',legend=expression(ABC,italic(ABC),bolditalic(ABC),Delta*italic(ABC))) 