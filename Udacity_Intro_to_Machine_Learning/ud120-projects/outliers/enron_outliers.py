#!/usr/bin/python

""" Exploring outliers in the Enron dataset """


import pickle
import sys
import matplotlib.pyplot
cd C:\Users\User\Documents\S2DS_Bootcamp_2017\Online_course_notes\Udacity_Intro_to_Machine_Learning\ud120-projects\tools\
from feature_format import featureFormat, targetFeatureSplit


### read in data dictionary, convert to an sklearn-ready numpy array
data_dict = pickle.load( open("../final_project/final_project_dataset.pkl", "r") )
features = ["salary", "bonus"]
data = featureFormat(data_dict, features)  
# data = an N x 2 numpy array, with N datapoints and 2 features

# visualise data as a scatterplot to spot outliers
for point in data:
    salary = point[0]
    bonus = point[1]
    matplotlib.pyplot.scatter( salary, bonus )
matplotlib.pyplot.xlabel("salary")
matplotlib.pyplot.ylabel("bonus")
matplotlib.pyplot.show()

""" One outlier is immediately clear - it's on almost a different scale to 
    the other points. 
    Now the question is to identify the source. We found the original data 
    source to be very helpful for this identification; you can find that PDF
    in final_project/enron61702insiderpay.pdf 
    
    What’s the name of the dictionary key of this data point? (e.g. if this is
    Ken Lay, the answer would be “LAY KENNETH L”). """
    
# it's the 'total' column: total salary = $10,529,459, total bonus = $29,900,000

""" Remove TOTAL from the dictionary. 
    A quick way to remove a key-value pair from a dictionary is: 
        dictionary.pop( key, 0 ) 
    Remove the outlier using this code and then call featureFormat(). 
    Now redraw the scatterplot without the TOTAL outlier. 
    Are all the outliers gone? """

# the dictionary was imported as 'data_dict'
data_dict.pop('TOTAL', 0)

# remove the TOTAL key-value pair
features = ["salary", "bonus"]

# re-run the featureFormat() function
data = featureFormat(data_dict, features)  

# redraw the scatterplot
for point in data:
    salary = point[0]
    bonus = point[1]
    matplotlib.pyplot.scatter( salary, bonus )
matplotlib.pyplot.xlabel("salary")
matplotlib.pyplot.ylabel("bonus")
matplotlib.pyplot.show()

# there are still about 4 outliers
# consulted the PDF of finance information to find the two with salaries over 
# $1,000,000 and bonuses over $5,000,000:
# SKILLING JEFFREY K
# LAY KENNETH L

###############################################################################