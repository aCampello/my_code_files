#!/usr/bin/python

""" 
    Starter code for exploring the Enron dataset (emails + finances, E+F);
    this code loads up the dataset.
    
    The aggregated Enron E+F dataset is stored in a dictionary, where 
    each key = a person’s name 
    and the value is a dictionary, {features_dict}, containing all features 
    associated with that person.

    The dataset has the form:
    enron_data["LASTNAME FIRSTNAME MIDDLEINITIAL"] = { features_dict }
    
    The dataset is stored as a pickle file, which is a handy way to store and
    load python objects directly. 
    
    You should explore features_dict as part of the mini-project,
    but here's an example to get you started:
    enron_data["SKILLING JEFFREY K"]["bonus"]
    
"""
### Import the data
import pickle
enron_data = pickle.load(open("./final_project/final_project_dataset.pkl", "r"))


### Explore the data
enron_data["SKILLING JEFFREY K"]["bonus"] # = 5600000


""" How many data points (people) are in the dataset? """
len(enron_data)  # = 146

""" How many features are available for each person? """
len(enron_data["SKILLING JEFFREY K"])  # = 21

##############################################################################

""" The “poi” feature records whether the person is a person of interest.
    How many POIs are there in the E+F dataset? -
    Count the number of entries in the dictionary where
    data[person_name]["poi"]== True """

def count_poi(index):
    total = 0
    for key in index:
        print index[key]["poi"]
        if index[key]["poi"] == True:
            total += 1
    return total
count_poi(enron_data)  # total = 18 persons of interest

##############################################################################

""" Like any dict of dicts, individual people/features can be accessed like so:
    enron_data["LASTNAME FIRSTNAME"]["feature_name"]
    or
    enron_data["LASTNAME FIRSTNAME MIDDLEINITIAL"]["feature_name"]

    What is the total value of the stock belonging to James Prentice?
"""

enron_data["PRENTICE JAMES"]["total_stock_value"]  # = 1095040

##############################################################################

""" How many email messages do we have from Wesley Colwell to persons of 
    interest? """

enron_data["COLWELL WESLEY"]['from_this_person_to_poi']  # = 11

##############################################################################

""" What’s the value of stock options exercised by Jeffrey K Skilling? """

enron_data["SKILLING JEFFREY K"]['exercised_stock_options']  # = 19250000

##############################################################################

""" How many folks in this dataset have a quantified salary? 
    What about a known email address? """

# salary
def count_sal(index):
    total = 0
    for key in index:
        print index[key]["salary"]
        if index[key]["salary"] != 'NaN':
            total += 1
    return total
count_sal(enron_data)    # = 95

# email
def count_em(index):
    total = 0
    for key in index:
        print index[key]["email_address"]
        if index[key]["email_address"] != 'NaN':
            total += 1
    return total
count_em(enron_data)    # = 111

##############################################################################

""" See if a certain poi (person of interest) appears in dataset """

"KRAUTZ MICHAEL" in enron_data   # returns False

##############################################################################

""" How many people in the E+F dataset (as it currently exists) have “NaN”
    for their total payments? 
    What percentage of people in the dataset as a whole is this? """
    
def count_NaNs(index):
    total = 0
    for key in index:
        if index[key]["total_payments"] == 'NaN':
            total += 1
    return total
count_NaNs(enron_data)    # = 21 

# % 
len(enron_data)  # = 146 people in the dataset
21/146.0  # = 14.4 %   (use a float to get decimal answer)
    
##############################################################################

""" How many POIs in the E+F dataset have “NaN” for their total payments? 
    What percentage of POI’s as a whole is this? """
    
def count_NaNs_poi(index):
    total = 0
    for key in index:
        if index[key]["poi"] == True:
            if index[key]["total_payments"] == 'NaN':
                total += 1
    return total
count_NaNs_poi(enron_data)    # = 0

# % 
count_poi(enron_data)   # there are 18 poi in dataset (using the function i wrote earlier, above)
0/18  # = 0%  

# So no training points would have "NaN" for total_payments when the class 
# label is "POI"
# = all POIs have a value for total_payments, and only non-POIs could have 'NaN'.
# This means the machine learning algorithm would class all datapoints with 
# total_payments='NaN' as non-POI.


##############################################################################
    
""" After adding in 10 more datapoints that are all POIs and all have
    total_payments = 'NaN', some of the values change:
"""
# Original number of people in dataset = 146
# New total number of people in dataset = 156     

# Original number of pois = 18
# New number of pois = 28

# Original number of non-pois = 146 ppl-18 pois = 128
# New number of non-pois = 156 ppl-28 pois = 128 (the same)

# Original number of people with total_payments='NaN' = 21 (21/146.0 = 14%)
# New number of people with total_payments='NaN' = 31 (31/156.0 = 20%)

# Original number of pois with total_payments='NaN' = 0 (0/18.0 = 0%)
# New number of pois with total_payments='NaN' = 10 (10/28.0 = 36%)

# Original number of non-pois with total_payments='NaN' = 0 (14/128.0 = 11%)
# New number of non-pois with total_payments='NaN' = 11% as before


# Before adding new data, 11% non-POIs and 0% POIs had total_payments='NaN',
#   so 'NaN' would indicate they were a non-POI.

# After adding new data, 11% non-POIs and 36% POIs had total_payments='NaN', 
#   so 'NaN's were more common among POIs than non-POIs, and could indicate 
#   the person is a POI.

##############################################################################

