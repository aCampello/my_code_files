library(maptools)

# Open cumulative frequency matrix file of RD (CRD, Cumulative RD)
rd<-readAsciiGrid("CRD_2308n1.asc")

# Open cumulative frequency matrix file of ID (CID, Cumulative ID)
id<-readAsciiGrid("CID_2308n1.asc")

# Open matrix file specifying the 5% cumulative frequency isopleth interval
# of the UD in which any quadrat lies (CUD, Cumulative UD)
ud<-readAsciiGrid("CUD_2308n1.asc")

# Open matrix file specifying the space use intensity of all quadrats of the
# virtual grid used in UD computation, expressed as integers values in µ% 
# (10-8 probability), useful for further analyses e.g. habitat selection, volume 
# overlap between HRs...
allud<- readAsciiGrid("UD_2308n1.asc")

par(mfrow = c(2,2), mar=c(0,0,2,0))
image(rd)
title("rd")
image(id)
title("id")
image(ud)
title("ud")
image(allud)
title("allud")
dev.off()