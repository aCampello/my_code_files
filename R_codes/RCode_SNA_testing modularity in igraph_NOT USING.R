### Modularity - NOT USING
#= Modularity as Q
descdist(global_network_data_PAs$modQ, discrete = FALSE, boot=5000) # distrib looks lognormal
qqp((global_network_data_PAs$modQ), "norm", main="normal")
qqp(global_network_data_PAs$modQ, "lnorm", main="lognormal") 
gamma <- fitdistr(global_network_data_PAs$modQ+1, "gamma", start=NULL) 
qqp(global_network_data_PAs$modQ, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]], main="gamma") # gamma is poor 
# does not suit any distrib and too many zeros - don't use
a<-glm(modQ+1 ~ fSeason, data=global_network_data_PAs, family=Gamma(link="log"))

#= Modularity as N clusters
descdist(global_network_data_PAs$Nclust, discrete = FALSE, boot=5000) # NORMAL OR GAMMA
qqp((global_network_data_PAs$Nclust), "norm", main="normal")
qqp(global_network_data_PAs$Nclust, "lnorm", main="lognormal") 
gamma <- fitdistr(global_network_data_PAs$Nclust, "gamma", start=NULL) 
qqp(global_network_data_PAs$Nclust, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]], main="gamma") # gamma is poor 
# normal and gamma approx the same goodness of fit so use NORMAL
# model
a <-lm(Nclust~fSeason, data=global_network_data_PAs)
summary(a)
plot(fitted(a), resid(a))
lines(smooth.spline(fitted(a), resid(a))) 
qqnorm(resid(a))
qqline(resid(a))
visreg::visreg(a)
# Fit is OK but no sig diffs


#------------------------------------------------------------------------


# TRIED MAKING LOOP TO CALC N CLUSTERS FROM RANDOM NETWORKS BUT KEPT FAILING - MAYBE DUE TO TOO MANY ISOLATES
# tried increasting maxiter but couldnt work out where in code it should be slotted in. GIVE UP and use density.

detach(package:sna)
library(igraph)
Nclust_intercept_perm <- rep(NA,perm)
Nclust_sum_perm <- rep(NA,perm)
Nclust_aut_perm <- rep(NA,perm)
Nclust_win_perm <- rep(NA,perm)

for(i in 1:perm){ 
  input_Nclust <- rbind(
    data.frame(Territory=1, SeasonID=1, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T1spr_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T1spr_rand[i,,])),
    data.frame(Territory=1, SeasonID=2, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T1sum_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T1sum_rand[i,,])),
    data.frame(Territory=1, SeasonID=3, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T1aut_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T1aut_rand[i,,])),
    data.frame(Territory=1, SeasonID=4, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T1win_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T1win_rand[i,,])),
    data.frame(Territory=2, SeasonID=1, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T2spr_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T2spr_rand[i,,])),
    #data.frame(Territory=2, SeasonID=2, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T2sum_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T2sum_rand[i,,])),
    data.frame(Territory=2, SeasonID=3, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T2aut_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T2aut_rand[i,,])),
    data.frame(Territory=2, SeasonID=4, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T2win_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T2win_rand[i,,])),
    data.frame(Territory=3, SeasonID=1, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T3spr_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T3spr_rand[i,,])),
    data.frame(Territory=3, SeasonID=2, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T3sum_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T3sum_rand[i,,])),
    data.frame(Territory=3, SeasonID=3, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T3aut_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T3aut_rand[i,,])),
    data.frame(Territory=3, SeasonID=4, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T3win_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T3win_rand[i,,])),
    #data.frame(Territory=4, SeasonID=1, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T4spr_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T4spr_rand[i,,])),
    #data.frame(Territory=4, SeasonID=2, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T4sum_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T4sum_rand[i,,])),
    data.frame(Territory=4, SeasonID=3, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T4aut_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T4aut_rand[i,,])),
    data.frame(Territory=4, SeasonID=4, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T4win_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T4win_rand[i,,])),
    data.frame(Territory=5, SeasonID=1, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T5spr_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T5spr_rand[i,,])),
    data.frame(Territory=5, SeasonID=2, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T5sum_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T5sum_rand[i,,])),
    data.frame(Territory=5, SeasonID=3, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T5aut_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T5aut_rand[i,,])),
    #data.frame(Territory=5, SeasonID=4, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T5win_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T5win_rand[i,,])),
    data.frame(Territory=6, SeasonID=1, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T6spr_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T6spr_rand[i,,])),
    data.frame(Territory=6, SeasonID=2, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T6sum_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T6sum_rand[i,,])),
    data.frame(Territory=6, SeasonID=3, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T6aut_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T6aut_rand[i,,])),
    data.frame(Territory=6, SeasonID=4, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T6win_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T6win_rand[i,,])),
    data.frame(Territory=7, SeasonID=1, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T7spr_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T7spr_rand[i,,])),
    data.frame(Territory=7, SeasonID=2, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T7sum_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T7sum_rand[i,,])),
    data.frame(Territory=7, SeasonID=3, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T7aut_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T7aut_rand[i,,])),
    data.frame(Territory=7, SeasonID=4, Nclust=max(membership(cluster_leading_eigen(graph.adjacency(T7win_rand[i,,],mode="undirected",weighted=T,diag=F))))/nrow(T7win_rand[i,,])))
  # make factors
  input_Nclust$fSeason <- factor(input_Nclust$SeasonID)
  # Run model
  model_tmp  <-lm(Nclust ~ fSeason, data=input_Nclust)
  a <- coef(summary(model_tmp))
  Nclust_intercept_perm[i] <- a[1,1]
  Nclust_sum_perm[i] <- a[2,1]
  Nclust_aut_perm[i] <- a[3,1]
  Nclust_win_perm[i] <- a[4,1]
  # update prog bar
  setTxtProgressBar(pb, i)
}
