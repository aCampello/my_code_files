

#--------------------------------------------------------------------------------------------
# Old closeness code: when computed across all components (so including isolates and pairs)
#--------------------------------------------------------------------------------------------#

########### Have deleted gconly=FALSE but this should be in all of these lines.


# Territory 1
T1attr <- subset(attr, DaysSeen>4 & Territory==1)
T1attr <-T1attr[order(T1attr$Territory, T1attr$SeasonID, T1attr$id), ] # IMPORTANT to order by Territory, Season and ID
T1attr$closeness <-  c(data.frame(closeness_w(T1spr_net, gconly=FALSE))$closeness, #gconly=F means don't just calc closeness within the main component
                       data.frame(closeness_w(T1sum_net, gconly=FALSE))$closeness,
                       data.frame(closeness_w(T1aut_net, gconly=FALSE))$closeness,
                       data.frame(closeness_w(T1win_net, gconly=FALSE))$closeness)

# Territory 2
T2attr <- subset(attr, DaysSeen>4 & Territory==2)
T2attr <-T2attr[order(T2attr$Territory, T2attr$SeasonID, T2attr$id), ] # IMPORTANT to order by Territory, Season and ID
T2attr$closeness <-  c(data.frame(closeness_w(T2spr_net, gconly=FALSE))$closeness, #gconly=F means don't just calc closeness within the main component
                       data.frame(closeness_w(T2sum_net, gconly=FALSE))$closeness,
                       data.frame(closeness_w(T2aut_net, gconly=FALSE))$closeness,
                       data.frame(closeness_w(T2win_net, gconly=FALSE))$closeness)

# Territory 3
T3attr <- subset(attr, DaysSeen>4 & Territory==3)
T3attr <-T3attr[order(T3attr$Territory, T3attr$SeasonID, T3attr$id), ] # IMPORTANT to order by Territory, Season and ID
T3attr$closeness <-  c(data.frame(closeness_w(T3spr_net))$closeness, #gconly=F means don't just calc closeness within the main component
                       data.frame(closeness_w(T3sum_net))$closeness,
                       data.frame(closeness_w(T3aut_net))$closeness,
                       c((data.frame(closeness_w(T3win_net))$closeness), "0")) # Last fox had no assocs (see below)

# Territory 4
T4attr <- subset(attr, DaysSeen>4 & Territory==4)
T4attr <-T4attr[order(T4attr$Territory, T4attr$SeasonID, T4attr$id), ] # IMPORTANT to order by Territory, Season and ID

# As spring & summer networks contain only 4 nodes tnet assumes it's a longitudinal network and not a matrix 
# So I need to make the edgelist manually:
T4spr_EL <- sna::as.edgelist.sna(T4spr_net) # convert matrix to edgelist in sna
T4spr_tnet <- as.tnet(T4spr_EL, type="weighted one-mode tnet") # convert edgelist to tnet object

T4sum_EL <- sna::as.edgelist.sna(T4sum_net) # convert matrix to edgelist in sna
T4sum_tnet <- as.tnet(T4sum_EL, type="weighted one-mode tnet") # convert edgelist to tnet object

T4attr$closeness <-  c(data.frame(closeness_w(T4spr_tnet))$closeness,               # matrix is 4x4 so tnet treats as longitudinal (see comment above) - have to convert to tnet object manually
                       data.frame(closeness_w(T4sum_tnet))$closeness,               # ditto
                       c((data.frame(closeness_w(T4aut_net))$closeness), "0"),      # last col contains only zeros
                       c((data.frame(closeness_w(T4win_net))$closeness), "0", "0")) # last 2 cols contain only zeros

# Territory 5
T5attr <- subset(attr, DaysSeen>4 & Territory==5)
T5attr <-T5attr[order(T5attr$Territory, T5attr$SeasonID, T5attr$id), ] # IMPORTANT to order by Territory, Season and ID
T5attr$closeness <-  c(c((data.frame(closeness_w(T5spr_net))$closeness), "0"), #gconly=F means don't just calc closeness within the main component
                       c((data.frame(closeness_w(T5sum_net))$closeness), "0", "0"),
                       data.frame(closeness_w(T5aut_net))$closeness,
                       data.frame(closeness_w(T5win_net))$closeness)

# Territory 6
T6attr <- subset(attr, DaysSeen>4 & Territory==6)
T6attr <-T6attr[order(T6attr$Territory, T6attr$SeasonID, T6attr$id), ] # IMPORTANT to order by Territory, Season and ID

T6attr$closeness <-  c(c((data.frame(closeness_w(T6spr_net))$closeness), "0"), #gconly=F means don't just calc closeness within the main component
                       data.frame(closeness_w(T6sum_net))$closeness,
                       c((data.frame(closeness_w(T6aut_net))$closeness), "0"),
                       data.frame(closeness_w(T6win_net))$closeness)

# Territory 7
T7attr <- subset(attr, DaysSeen>4 & Territory==7)
T7attr <-T7attr[order(T7attr$Territory, T7attr$SeasonID, T7attr$id), ] # IMPORTANT to order by Territory, Season and ID
T7spr_EL <- sna::as.edgelist.sna(T7spr_net) # convert matrix to edgelist in sna
T7spr_tnet <- as.tnet(T7spr_EL, type="weighted one-mode tnet") # convert edgelist to tnet object
T7sum_EL <- sna::as.edgelist.sna(T7sum_net) # convert matrix to edgelist in sna
T7sum_tnet <- as.tnet(T7sum_EL, type="weighted one-mode tnet") # convert edgelist to tnet object

T7attr$closeness <-  c(data.frame(closeness_w(T7spr_tnet))$closeness, #gconly=F means don't just calc closeness within the main component
                       data.frame(closeness_w(T7sum_tnet))$closeness,
                       c((data.frame(closeness_w(T7aut_net))$closeness), "0"),
                       c((data.frame(closeness_w(T7win_net))$closeness), "0"))
#------------------------------------------------------------------------------------------------------

# Combine all territories
tALLattr_closeness <- rbind(T1attr, T2attr, T3attr, T4attr, T5attr, T6attr, T7attr)
tALLattr_closeness$closeness <- as.numeric(tALLattr_closeness$closeness)