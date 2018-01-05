#!/usr/bin/python

""" Processes raw email data ready for use in machine learning algorithms """

import os
import pickle
import re
import sys

cd C:\Users\User\Documents\my_code_files\Udacity_Intro_to_Machine_Learning\ud120-projects\tools
sys.path.append( "../tools/" ) # this line works intermittently
from parse_out_email_text import parseOutText

"""
    Starter code to process the emails from Sara and Chris to extract
    the features and get the documents ready for classification.

    The list of all the emails from Sara are in the from_sara list
    likewise for emails from Chris (from_chris)

    The actual documents are in the Enron email dataset, which
    you downloaded/unpacked in Part 0 of the first mini-project. If you have
    not obtained the Enron email corpus, run startup.py in the tools folder.

    The data is stored in lists and packed away in pickle files at the end.
"""

# Get the data
cd C:\Users\User\Documents\my_code_files\Udacity_Intro_to_Machine_Learning\ud120-projects\text_learning
from_sara  = open("from_sara.txt", "r")
from_chris = open("from_chris.txt", "r")

from_data = []
word_data = []

### temp_counter is a way to speed up the development--there are
### thousands of emails from Sara and Chris, so running over all of them
### can take a long time
### temp_counter helps you only look at the first 200 emails in the list so you
### can iterate your modifications quicker
# temp_counter = 0


for name, from_person in [("sara", from_sara), ("chris", from_chris)]:
    for path in from_person:
        ### only look at first 200 emails when developing
        ### once everything is working, remove this line to run over full dataset
        #temp_counter += 1
        #if temp_counter < 200:

            """ Note failure to populate the from_data and word_data lists could be due
                to incorrect path here - try restarting the kernel if so """
            path = os.path.join('..', path[:-1])
            print path

            # open the email
            email = open(path, "r")

            ### use parseOutText to extract the text from the opened email
            stemmed_text = parseOutText(email)

            ### use str.replace() to remove any instances of the words
            ### ["sara", "shackleton", "chris", "germani"]
            blacklist = ["sara", "shackleton", "chris", "germani", "sshacklensf", "cgermannsf"]  # NOTE added "sshacklensf" and "cgermannsf" for Lesson 12 Quiz 29+30.
            for word in blacklist:
                stemmed_text = stemmed_text.replace(word, "")

            ### append the text to word_data
            word_data.append(stemmed_text)

            ### append a 0 to from_data if email is from Sara, and 1 if email is from Chris
            if name == "sara":
                from_data.append(0)
            else:
                from_data.append(1)
            email.close()

print "emails processed"
from_sara.close()
from_chris.close()

# SAVE:
# pickle.dump( word_data, open("your_word_data.pkl", "w") )
# pickle.dump( from_data, open("your_email_authors.pkl", "w") )


# Retrieve the data at a later stage
cd C:\Users\User\Documents\my_code_files\Udacity_Intro_to_Machine_Learning\ud120-projects\text_learning
word_data = pickle.load(open("your_word_data.pkl", "rb"))
from_data = pickle.load(open("your_email_authors.pkl", "rb"))


###############################################################################

""" Udacity quiz 21: What is the string at word_data[152] ? """

word_data[152]  # = 'tjonesnsf stephani and sam need nymex calendar '

###############################################################################

### Quiz 20 & 21: TfIdf vectorization

""" Transform the word_data into a tf-idf matrix using the sklearn TfIdf
    transformation. Use the tf-idf Vectorizer class for the transformation.

    Remove english stopwords using the sklearn's stop word list (not NLTK).

    You can access the mapping between words and feature numbers using
    vectorizer.get_feature_names(), which returns a list of all the
    words in the vocabulary. How many different words are there? """


from sklearn.feature_extraction.text import TfidfVectorizer

# Create a vectorizer
vectorizer = TfidfVectorizer(stop_words="english")  # remove english stopwords

# Train and transform it
vectorizer.fit_transform(word_data)   

# Use get_feature_names applied to the VECTORIZER (not the matrix output)
# to find number of words
print "Num words:", len(vectorizer.get_feature_names())
        # Num words: 3078 (on the max200 dataset; 38757 on the full dataset)


""" What is word number 34597 in your TfIdf? """
vectorizer.get_feature_names()[34597]    # = 'stephaniethank'

###############################################################################