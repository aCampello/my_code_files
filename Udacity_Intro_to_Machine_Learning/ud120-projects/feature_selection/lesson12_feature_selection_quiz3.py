#!/usr/bin/python

""" Udacity intro to machine learning: Lesson 12 feature selection
    Quiz 3: A New Enron Feature

    # For a given email, assess whether it is to/from/cc'd to a POI and return 
      a trio of booleans for to/from/cc e.g. true/false/true 
      (= the 'first code file' below).
      
    # This can be used to count the number of emails each person received from
      a POI.
      (= the 'second code file' below).
"""

#============ 
# FIRST CODE FILE poi_flag_email.py that I edited (marked with #######) to 
# make the from_emails part of the boolean
#============ 

import sys
import reader
import poi_emails

def getToFromStrings(f):
    '''
    The imported reader.py file contains functions that we've created to help
    parse e-mails from the corpus. .getAddresses() reads in the opening lines
    of an e-mail to find the To: From: and CC: strings, while the
    .parseAddresses() line takes each string and extracts the e-mail addresses
    as a list.
    '''
    f.seek(0)
    to_string, from_string, cc_string   = reader.getAddresses(f)
    to_emails   = reader.parseAddresses( to_string )
    from_emails = reader.parseAddresses( from_string )
    cc_emails   = reader.parseAddresses( cc_string )

    return to_emails, from_emails, cc_emails


### POI flag an email

def poiFlagEmail(f):
    """ given an email file f,
        return a trio of booleans for whether that email is
        to, from, or cc'ing a poi """

    to_emails, from_emails, cc_emails = getToFromStrings(f)

    ### poi_emails.poiEmails() returns a list of all POIs' email addresses.
    poi_email_list = poi_emails.poiEmails()

    to_poi = False
    from_poi = False
    cc_poi   = False

    ### to_poi and cc_poi are boolean variables which flag whether the email
    ### under inspection is addressed to a POI, or if a POI is in cc,
    ### respectively. You don't have to change this code at all.

    ### There can be many "to" emails, but only one "from", so the
    ### "to" processing needs to be a little more complicated
    if to_emails:
        ctr = 0
        while not to_poi and ctr < len(to_emails):
            if to_emails[ctr] in poi_email_list:
                to_poi = True
            ctr += 1
    if cc_emails:
        ctr = 0
        while not cc_poi and ctr < len(cc_emails):
            if cc_emails[ctr] in poi_email_list:
                cc_poi = True
            ctr += 1

    #################################
    ######## your code below ########
    ### set from_poi to True if #####
    ### the email is from a POI #####
    #################################
    if from_emails:
        ctr = 0
        while not from_poi and ctr < len(from_emails):
            if from_emails[ctr] in poi_email_list:
                from_poi = True
            ctr += 1
    # print to_poi, from_poi, cc_poi   # used a print statement to test the code
    #################################
    return to_poi, from_poi, cc_poi


#============ 
# SECOND CODE FILE studentMain.py used to implement the code 
# (using some other imported Udacity functions/data)
#============ 

#!/usr/bin/python

import os
import sys
import zipfile
from poi_flag_email import poiFlagEmail, getToFromStrings

data_dict = {}

with zipfile.ZipFile('emails.zip', "r") as z:
    z.extractall()

for email_message in os.listdir("emails"):
    if email_message == ".DS_Store":
        continue
    message = open(os.getcwd()+"/emails/"+email_message, "r")
    to_addresses, from_addresses, cc_addresses = getToFromStrings(message) 
    
    to_poi, from_poi, cc_poi = poiFlagEmail(message)
    
    for recipient in to_addresses:
        # initialize counter
        if recipient not in data_dict:
            data_dict[recipient] = {"from_poi_to_this_person":0}
        # add to count
        if from_poi:
                data_dict[recipient]["from_poi_to_this_person"] += 1

    message.close()

for item in data_dict:
    print item, data_dict[item]
    
#######################################################    

""" Run this code to get the output (works in the Udacity python interpreter,
    not here as i dont have the data) """
def submitData():
    return data_dict


# Output is a dictionary with recipients as keys and the N emails they received
# from a POI as a value:
    
#  {'ken.rice@enron.com': {'from_poi_to_this_person': 0}, 
#   'sheila.knudsen@enron.com': {'from_poi_to_this_person': 1}, 
#   'mark.bernstein@enron.com': {'from_poi_to_this_person': 0}, ...}
     
######################################################