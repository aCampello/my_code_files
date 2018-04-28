#!/usr/bin/python

# the training set contains 90 data points, so:
# predictions, ages and net_worths are lists each containing 90 elements
# create a function outlierCleaner to return a list called cleaned_data that 
# contains just 81 elements, i.e. the 90% of the training points with the 
# smallest errors (90*0.9=81). So we remove the 10% with the largest errors.

def outlierCleaner(predictions, ages, net_worths):
    """
        Clean away the 10% of points that have the largest
        residual errors (difference between the prediction
        and the actual net worth).
        
        # predictions = a list of predicted targets that come from your regression
        # ages = list of ages in the training set
        # net_worths = is the actual value of the net worths in the training set

        Return a list of tuples named cleaned_data where 
        each tuple is of the form (age, net_worth, error).
    """
    
    # create empty list to store the tuples
    cleaned_data = []  
    
    # loop through each of the 90 datapoints and creates a tuple for each 
    # point containing the age, net worth and error (x=datapoint)
    for x in range(0, len(predictions)):
        new = ages[x], net_worths[x], predictions[x]-net_worths[x]
        cleaned_data.append(tuple(new))
        
    # sort cleaned_data by error size (which is stored in tuple index 2)
    cleaned_data.sort(key=lambda tup: tup[2])
    
    # return the 81 tuples in the cleaned data with the smallest errors
    return cleaned_data[:81]

