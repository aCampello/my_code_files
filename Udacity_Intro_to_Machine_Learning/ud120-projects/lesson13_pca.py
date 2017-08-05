# -*- coding: utf-8 -*-
"""
Created on Mon Jul 31 20:05:04 2017

Udacity intro to machine learning Lesson 13: PCA

See more detailed code for the sklearn 'eigenfaces' PCA and SVM example in 
eigenfaces.py, saved in: 
    C:\Users\User\Documents\S2DS_Bootcamp_2017\Online_course_notes\Udacity_Intro_to_Machine_Learning\ud120-projects\pca

"""

### WHEN TO USE PCA:
    
""" 1. You (think you might) have latent features driving patterns in the data 
       e.g. big shots @ Enron.
    
    2. Dimensionality reduction: helps visualise high dimensional data (= with 
       many features). Scatter plots can only display 2 features so you could
       use the first 2 PCs for a scatter plot, which might make it easier to 
       visualise e.g. k-means clustering.
       
    3. To reduce noise in the data (you hope that PC1 and PC2 capture the
       strongest patterns and the remaining PCs only represent noise, so by
       discarding these extra PCs you get rid of that noise.
       
    4. As preprocessing before you use another algorithm - dimensionality 
       reduction again. Algorithms work better with fewer inputs: lower 
       complexity, lower variance, faster).
    
    Be open to the possible no. components that might be relevant and find the
    best number by experimenting and monitoring changes in the accuracy (f1 score)
    - see example of PCA in sklearn below).
"""    
    

### DIMENSIONS

""" Two-dimensional data varies in 2 dimensions irregulary, so not all points
    fall along a single line (some scatter).
    
    One-dimensional data only varies in one dimension and can be mapped in a
    completely straight line, e.g. only varies in x with a constant y would
    produce a straight horizontal line. 
    
    Small deviations from a single line are noise.
    
    Data is still 1D if it follows a single straight but sloping line, as 
    the axes can be rotated and shifted using PCA. 
"""


### HOW PCA WORKS

""" PCA places the origin at the centre of the data point cloud, aligns the 
    x-axis with the principal axis of variation (the slope of the points) and 
    the y-axis orthogonal to this along the dimension with less variation. 
        The new x-axis (vector) is x1 (x-prime)
        The new y-axis (vector) is y1 (y-prime)
        These axes represent the principal component of the data.
        
    PCA returns an importance/spread value (eigenvalues) for each new axis. 
    - The spread is usually very large for x1 and much smaller for y1, which 
      suggests PCA is helpful at explaining the data. 
    - If there is little difference in spread between x1 and y1 we don't gain 
      much from running PCA. This occurs when there's high variation around both
      x1 and y1, e.g. when the data is in a circular blob.
"""

# PCA can be used to condense features into fewer variables. 
"""   To do this PCA 'plots' the features against each other (in multiple
      dimensions if there are >2 features), reprojects the data onto the 
      dimension of maximal variance (= the principal component), and then 
      projects/compresses all the points onto this line. You can then use this
      principal component as an input feature.
      
      Projecting/compressing along the dimension of MAX variance minimises info
      loss (=  the distance from new line to each point's original position).
      
      When running PCA on >2 variables it will combine them into new features
      and rank their relative quality (PC1, PC2...). This becomes harder to 
      interpret as each PC can contain bits and pieces from any/all of the
      input features.
"""
# PCA also works like an unsupervised learning technique to help learn about
# the patterns of variation in a dataset and perhaps see if there are any
# latent features driving this variation.
""" e.g. if you recorded N rooms, sq footage, neighbourhood crime and school 
    rating and wanted to know which influenced house price, a PCA might
    output 2 principal components, suggesting there are 2 latent features
    driving the variation in house price.
    
    Note PCA doesn't tell you which features are included in each component
    so you have to figure out what they are yourself, perhaps using SelectKBest.
    
    The max number of PCs that a PCA will return is:
        min(n_features, n_data_points)
"""    
    

### Implementing PCA in sklearn

# Best demonstrated using a worked example of using PCA followed by SVM for
# facial recognition of celebrity photos. The example is called 'eigenfaces', from: 
# http://scikit-learn.org/stable/auto_examples/applications/face_recognition.html

from sklearn.decomposition import PCA

# split into a training and testing set
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.25, random_state=42)

""""""
## Compute a PCA on the face dataset (treated as unlabeled dataset) 
    # = unsupervised feature extraction / dimensionality reduction
    
# Set n_components to 150 (seems high but the image files in this example each 
# contain 1850 features (pixels), so 1850 to 150 is a compression factor of >10.
n_components=150

# CREATE & FIT THE PCA: works out what the PCs are
pca = PCA(n_components=n_components,svd_solver='randomized', whiten=True).fit(X_train)

# Save the PCs of the face data as 'eigenfaces' by reshaping the PCs from 
# lists to squares (so they look like pictures - these are plottable)
eigenfaces = pca.components_.reshape((n_components, h, w))  # h=height, w=width

# TRANSFORM THE PCA: transforms the data into the principal components representation 
# (essential step so you can use the PCA outputs in an algorithm e.g. SVM)

# training set
X_train_pca = pca.transform(X_train)
# test set 
""" NOTE: don't need to run pca.fit() on the test data, as we have to use the same 
    PCs that were found from the training data to transform both the training
    and testing sets! (Udacity intro to ML less14 vid6) """
X_test_pca  = pca.transform(X_test)


""""""
### Access the attributes of the PCA (for interest)
print pca.explained_variance_ratio_     # get the eigenvalues (% variation in data)
pca.components_    # list of PCs - as many PCs as you asked for in the code above.
                   # gives directional info (the [x,y] vector of the PC relative to the original x axis)
first_pc = pca.components_[0]           # access the first PC
second_pc = pca.components_[1]          # the second PC


""""""
### Train a SVM classification model on the PCs

from sklearn.svm import SVC

# specify parameter grid for use with GridSearchCV (see validation section in 'python_code_glossary.py')
# (here we test different values of C and gamma)
param_grid = {'C': [1e3, 5e3, 1e4, 5e4, 1e5],
              'gamma': [0.0001, 0.0005, 0.001, 0.005, 0.01, 0.1], }

# make the SVM classifier
clf = GridSearchCV(SVC(kernel='rbf', class_weight='balanced'), param_grid)
# fit it
clf = clf.fit(X_train_pca, y_train)  # X_train_pca is the pca.transformed training data

print "Best estimator found by grid search:", clf.best_estimator_


""""""
### Quantitative evaluation of the model quality on the test set

# The F1 score (0-1)
""" A measure of the classifier's performance, where higher is better. 
    Calculated as the weighted average of the precision and recall.
    
    Performance (F1 score) varies with the number of PCs - it might increase
    as you add more PCs at first, but then decline as you add too many. 
    
    Find the optimal number of PCs by increasing/decreasing the n_components 
    parameter for the PCA, re-fitting the SVM and checking the f1 score.
    
    DONT do feature selection BEFORE you run the PCA (unless you're certain 
    that some features are completely irrelevant) as you'd be throwing out
    information that PCA would otherwise be able to 'rescue' by incorporating
    into one of the new PCs (Udacity intro mach learn lesson 13 video 38 answer).
""" 
    
# Predict the labels in the test set (i.e. identify who appears in a given picture).
y_pred = clf.predict(X_test_pca)  # X_test_pca is the pca.transformed test data

# print report showing precision, recall, F1-score snd support for each classification
from sklearn.metrics import classification_report
print classification_report(y_test, y_pred, target_names=target_names)

# print confusion matrix (used to calculate precision & recall)
from sklearn.metrics import confusion_matrix
print confusion_matrix(y_test, y_pred, labels=range(n_classes)) # n_classes = N unique labels/classes/people in dataset - to help layout the matrix correctly..

# to get n_classes:
target_names = mydata.target_names   # get the names of the targets (labels) in the dataset
n_classes = target_names.shape[0]

###############################################################################
