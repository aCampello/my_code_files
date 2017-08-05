# -*- coding: utf-8 -*-
"""
Created on Wed Aug 02 17:09:06 2017

Udacity intro to machine learning Lesson 16: video 2 
(taken from the flow diagram on the Udacity summary slide: 
 https://classroom.udacity.com/courses/ud120/lessons/3033138574/concepts/30962787190923

"""

###   MACHINE LEARNING SUMMARY   ###

"""
# These steps are roughly in order but you can cycle back through them to 
  change things as you work on your analysis.

1. DATASET or QUESTION that inspired the analysis
    # Define the question and identify the appropriate dataset to answer it.
    # Do you have enough data/features to answer the question?
    
2. FEATURE SELECTION: to extract info from your dataset.
    # Exploration: inspect features for correlations, outliers, cleaning /
      imputation (= replacing missing data with substituted values e.g. 'NaN' or 0).
    # Creation: by hand, e.g. POIs added from news articles and finance sheets.
    # Representation: text vectorization (represents language as word 
      frequencies), discretisation (converting continuous variables to discrete ones).
    # Scaling: using MinMaxScaler, StandardScaler or mean subtraction.
    # Selection: using SelectKBest, SelectPercentile, recursive feature selection
      using Regularisation (Lasso Regression), max_df in TfIdf vectorizer.
    # Transformations: PCA (and maybe Independent Component Analysis - while
       PCA helps compress data, ICA helps separate it into independent sub-elements).
    
3. Choose the ALGORITHM and PARAMETER TUNES for that algorithm
    - By feeding the chosen features into it and optimising the fit.
    - Supervised (labels)
        - Continuous / ordered output:
            - linear regression
            - lasso regression, 
            - decision tree regression
            - SV regression
        - Discrete / non-ordered output:
            - naive bayes
            - SVM
            - decision tree
            - k-nearest neighbours
            - logistic regression
            - ensemble methods
            - LDA (Linear discriminant analysis)
    - Unsupervised (no labels)
        - k-means clustering
        - spectral clustering
        - PCA
        - mixture models / the expectation-maximization (EM) algorithm (both assume random effects on each datapoint - see the EM wiki)
        - outlier detection
    - TUNING the algorithm
        - parameters of algorithm
        - visual inspection (plot decision surface/colour coded scatter, look for outliers)
        - monitor effects of changing parameters on performance on test data (accuracy or F1-scores)
        - GridSearchCV for automatic parameter tuning

4. VALIDATION - make sure findings are trustworthy.
    # validate
        - train/test split
        - k-fold
        - visualise
    # pick metric/s
        - SSE / R-Squared
        - precision / recall
        - F1 score
        - ROC curves (most common way to visualize the performance of a binary classifier http://scikit-learn.org/stable/modules/generated/sklearn.metrics.roc_curve.html)
        - custom
        - Variance-Bias tradeoff
    
"""

