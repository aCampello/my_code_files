# -*- coding: utf-8 -*-
"""
Created on Fri Jul 28 14:39:51 2017

@author: User
"""

# Feature scaling formula
""" 
    Rescales the values for a feature/s, e.g. height, between 0-1.
    So features with large/small ranges don't have more/less influence on the 
    result (e.g. weight varies more than height (as it's measured in smaller 
    units), so will have greater influence: Udacity mach learn lesson 10 vid 8).
    
    X1 = (X - Xmin) / (Xmax - Xmin) 

    X1 = the new rescaled feature
    X = the value to rescale
    Xmin and Xmax = the minimum and maximum values from the whole dataset.
    
    Scaling can be a problem if you have outliers with extreme values.
    
"""
# e.g if X=140, Xmin=115 and Xmax=175:
newx = (140-115)/(175-115.)  # float
# returns 0.416

# My function to take a list and return a rescaled list.
def featureScaling(arr):   # arr=array
    result = []
    x_min = min(arr)
    x_max = max(arr)
    if x_min == x_max:
        print "All data points have the same value so will be rescaled to 0.5"
        for num in arr:
            result.append(0.5)
    else:
        for num in arr:
            result.append((float(num) - x_min) / (x_max - x_min))
    return result

# test
data = [115, 140, 175]
data = [100, 100, 100]
featureScaling(data)

# Note sklearn has a function to do this...

###########

### Feature Scaling in sklearn

# sklearn page on preprocessing data / scaling features to a range:
    # http://scikit-learn.org/stable/modules/preprocessing.html

# MinMaxScaler
# Transforms features by scaling each feature to a given range.
# requires a NUMPY ARRAY
# requires FLOAT numbers.


### Prepare the data

# each element in the array is a training point, which is itself a list of 
# features, e.g. sex,age,height would be ([[F,30,157], [M,27,170]])
import numpy
weights = numpy.array([[115.], [140.], [175.]])   
# this no.array contains one feature (weight) and 3 training points for 3 
# different people, as FLOATs.


### Make the scaler
from sklearn.preprocessing import MinMaxScaler
scaler = MinMaxScaler()


### Run the rescaling function

##  FIT finds the xmin and xmax and TRANSFORM rescales each datapoint    
# (can also implement fit & transform separately)
rescaled_weight = scaler.fit_transform(weights)  

# output:
#    array([[ 0.        ],
#       [ 0.41666667],
#       [ 1.        ]])
    