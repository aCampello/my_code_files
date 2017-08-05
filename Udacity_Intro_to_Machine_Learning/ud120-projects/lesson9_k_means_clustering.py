# -*- coding: utf-8 -*-
"""
Created on Fri Jul 28 11:03:01 2017

Udacity intro to machine learning: Lesson 9 k-means clustering

http://scikit-learn.org/stable/modules/generated/sklearn.cluster.KMeans.html#sklearn.cluster.KMeans
"""

# More detailed notes on the most important parameters for k-means clustering in sklearn.
# The main notes are in PyCode_Useful bits of code.py

""" K-means clustering parameters:
    
    n_clusters=8    # Number of clusters
                    # !! The most important parameter in k-means clustering !!
                    # default=8 
                    # Almost always change it to a value that makes sense 
                      for your data.
                    
    max_iter=300    # N times algorithm iterates through the 'update centroids,
                      reassign points' process.
                    # default=300
                    # The default value is suitable in most situations and most
                      of the time the algorithm will terminate before 300.
    
    n_init=10       # N times algorithm initialises - with new centroid start 
                      positions. Diff start positions can give diff clusterings,
                      so you need to repeat the algorithm several times, so any
                      one of those clusterings might be wrong, e.g. due to:
                      
                      CONVERGENCE TO A LOCAL MINIMUM: 
                      Where 2 centroids' start positions cause them to end up in 
                      the same cluster, contradicting the obvious cluster 
                      structure of the data set: see Udacity Machine Learning 
                      lesson 9 quiz 15-16 & https://en.wikipedia.org/wiki/K-means_clustering.
                      Larger numbers of centroids produce more local minima. 
                      
                      This is why it's important to initialise the algorithm
                      multiple times, as in general the ensemble of all the 
                      clusterings will be accurate.
                      
                    # default=10
                    # 10 is usually OK but increase if you think clustering 
                      might be particularly prone to errors for your dataset
                      e.g. uniform distrib. or with many / messy clusters.
"""

    