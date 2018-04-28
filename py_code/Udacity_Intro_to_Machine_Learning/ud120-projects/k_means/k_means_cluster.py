#!/usr/bin/python 

""" 
    Skeleton code for k-means clustering mini-project.
"""


# Set up the workspace
import pickle
import numpy
import matplotlib.pyplot as plt
import sys
sys.path.append("../tools/")
from feature_format import featureFormat, targetFeatureSplit

###############################################################################

# Udacity plotting code to help visualize clusters
def Draw(pred, features, poi, mark_poi=False, name="image.png", f1_name="feature 1", f2_name="feature 2"):    
    """ some plotting code designed to help you visualize your clusters """
    ### plot each cluster with a different color--add more colors for
    ### drawing more than five clusters
    colors = ["b", "c", "k", "m", "g"]
    for ii, pp in enumerate(pred):
        plt.scatter(features[ii][0], features[ii][1], color = colors[pred[ii]])

    ### if you like, place red stars over points that are POIs (just for funsies)
    if mark_poi:
        for ii, pp in enumerate(pred):
            if poi[ii]:
                plt.scatter(features[ii][0], features[ii][1], color="r", marker="*")
    plt.xlabel(f1_name)
    plt.ylabel(f2_name)
    plt.savefig(name)
    plt.show()

###############################################################################

# Prepare the data

### load in the dict of dicts containing all the data on each person in the dataset
data_dict = pickle.load( open("../final_project/final_project_dataset.pkl", "r") )
### Remove the TOTAL row as it's an outlier
data_dict.pop("TOTAL", 0)


### the input features we want to use 
### can be any key in the person-level dictionary (salary, director_fees, etc.) 

# make list of labels for the features
feature_1 = "salary"
feature_2 = "exercised_stock_options"
poi  = "poi"
features_list = [poi, feature_1, feature_2]  # poi is 1st as that's the target (that we want to predict)

# featureFormat converts a dictionary to numpy array   
data = featureFormat(data_dict, features_list )

# targetFeatureSplit splits the numpy array into a target list and a feature list.
poi, finance_features = targetFeatureSplit( data )

###############################################################################

""" Deploy k-means clustering on the financial_features data, with 2 clusters 
    specified as a parameter. Store your cluster predictions to a list called 
    pred, so that the Draw() command at the bottom of the script works 
    properly. In the scatterplot that pops up, are the clusters what you 
    expected? """
    
# plot the 2 features salary and exercised_stock_options in finance_features
for f1, f2 in finance_features:
    plt.scatter( f1, f2 )
plt.show()

# cluster the data and create predictions of the cluster labels
from sklearn.cluster import KMeans
kmeans = KMeans(n_clusters=2) # 2 clusters
kmeans.fit(finance_features)  # finance_features=the numpy array of features to cluster
pred = kmeans.predict(finance_features)

# plot the clusters and save the figure in the wd as "clusters.pdf"
try:
    Draw(pred, finance_features, poi, mark_poi=False, name="clusters.pdf", f1_name=feature_1, f2_name=feature_2)
except NameError:
    print "no predictions object named pred found, no clusters to plot"
    
###############################################################################

""" Add a third feature to features_list, “total_payments". 
    Now rerun clustering, using 3 input features instead of 2 (obviously we 
    can still only visualize the original 2 dimensions). 
    Compare the plot with the clusterings to the one you obtained with 2 input
    features. Do any points switch clusters? How many? 
    This new clustering, using 3 features, couldn’t have been guessed by 
    eye--it was the k-means algorithm that identified it.
"""

# make list of labels for the features, with a 3rd feature for total_payments
feature_1 = "salary"
feature_2 = "exercised_stock_options"
feature_3 = "total_payments" 
poi  = "poi"
features_list = [poi, feature_1, feature_2, feature_3]  # poi is 1st as that's the target

# re-prepare the data   
data = featureFormat(data_dict, features_list )
poi, finance_features = targetFeatureSplit( data )


# plot the 3 features salary and exercised_stock_options in finance_features
# the original plot code above won't work as it was written for 2 features:
    # says "ValueError:  too many values to unpack"
# have to change [for f1, f2 in finance_features] to [for f1, f2, _ in...]
for f1, f2, _ in finance_features:
    plt.scatter( f1, f2)  # we can still only visualise the original 2 dimensions
plt.show()


# cluster the data and create predictions of the cluster labels
from sklearn.cluster import KMeans
kmeans = KMeans(n_clusters=2) # 2 clusters
kmeans.fit(finance_features)  # finance_features=the numpy array of features to cluster
pred = kmeans.predict(finance_features)


# plot the new clusters and save the figure in the wd as "clusters_3f.pdf"
try:
    Draw(pred, finance_features, poi, mark_poi=False, name="clusters_3f.pdf", f1_name=feature_1, f2_name=feature_2)
except NameError:
    print "no predictions object named pred found, no clusters to plot"

# 4 points changed cluster when the total_payments feature was considered.

##############################################################################

### Sneak preview of Feature Scaling

# feature scaling is a type of feature preprocessing that you should perform
# before some classification and regression tasks.

""" What are the maximum and minimum values taken by the 
    “exercised_stock_options” feature used in this example? """

# load in the dict of dicts containing all the data on each person in the dataset
# (finance_features shows empty values as zeros, whereas data_dict shows 'NaN')
data_dict = pickle.load( open("./final_project/final_project_dataset.pkl", "r") )
# Remove the TOTAL row as it's an outlier
data_dict.pop("TOTAL", 0)

# Function to search within dict of dict and return the MAX value of a feature
def findmax(data_dict, feature):
    """ Search within a dict of dicts and return the MAXIMUM value 
        of a given feature, e.g. "salary" """
    result = ["", 0]
    for person in data_dict:
        if data_dict[person][feature] != 'NaN':
            if data_dict[person][feature] > result[1]:
                result[0] = person
                result[1] = data_dict[person][feature]
    return result

# Function to search within dict of dict and return the MIN value of a feature
def findmin(data_dict, feature):
    """ Search within a dict of dicts and return the MINIMUM value 
        of a given feature, e.g. "salary" """
    # start with the maximum value, using the findmax function above
    max = findmax(data_dict, feature)
    result = ["", max]   
    for person in data_dict:
        if data_dict[person][feature] != 'NaN':
            if data_dict[person][feature] < result[1]:
                result[0] = person
                result[1] = data_dict[person][feature]
    return result

# Min and Max exercised_stock_options, and their keys
findmax(data_dict, "exercised_stock_options")  # ['LAY KENNETH L', 34348384]
findmin(data_dict, "exercised_stock_options")  # ['BELFER ROBERT', 3285]

##############################################################################

""" What are the maximum and minimum values for salary? """
 
# Min and Max exercised_stock_options, and their keys
findmax(data_dict, "salary")  # ['SKILLING JEFFREY K', 1111258]
findmin(data_dict, "salary")  # ['BANNANTINE JAMES M', 477]

##############################################################################

""" Apply feature scaling to your k-means clustering code from the last 
    lesson, on the “salary” and “exercised_stock_options” features (use only 
    these two features). """

from sklearn.preprocessing import MinMaxScaler
scaler = MinMaxScaler()
rescaled_data = scaler.fit_transform(finance_features)

##############################################################################

""" What would be the rescaled value of a "salary" feature that had an 
    original value of $200,000, and an "exercised_stock_options" feature of 
    $1 million? (Be sure to represent these numbers as floats, not integers!) 
"""

# Take the min/max for salary and exercised_stock_options from above and use MinMaxScaler
from sklearn.preprocessing import MinMaxScaler
import numpy
salaries = numpy.array([[477.],[200000.],[1111258.]])
scaler = MinMaxScaler()
scaler.fit_transform(salaries)  # 200,000 is rescaled to 0.179

stock_options = numpy.array([[3285.],[34348384.],[1000000.]])
scaler.fit_transform(stock_options)  # 1,000,000 is rescaled to 0.029

##############################################################################
