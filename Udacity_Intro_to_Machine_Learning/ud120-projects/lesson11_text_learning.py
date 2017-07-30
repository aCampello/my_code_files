# -*- coding: utf-8 -*-
"""
Created on Sat Jul 29 17:29:06 2017

Udacity intro to machine learning lesson 11: Text Processing

    How to pre-process / prepare text data before you pass it into a machine 
    learning algorithm.

http://scikit-learn.org/stable/modules/generated/sklearn.feature_extraction.text.CountVectorizer.html
"""

###############################################################################

# Text processing

""" Typical process for preparing text for machine learning algorithms:
    
    1. First stem each word in the corpus
    2. Then use a VECTORIZER to convert the corpus into a usable format 
        that can be used for machine:
        a) create a vectorizer that excludes stopwords.
        b) fit the vectorizer to your dataset (here it works out which words 
             appear in the corpus, and indexes them - minus the stopwords).
        c) transform the vectorizer (here it records the frequency counts 
             for each stem word).
    
    Then the data in the vectorizer is ready for use as input for algorithms.
    
"""

###############################################################################

### STEMMING

# one of the first steps in text processing
# takes a string, so do stemming before creating a bag of words representation.

""" Stemming functions strip down words to their root or 'stem'
    e.g. 'respon' is the stem of respond, response and unresponsive.
    
    This reduces these similar words from 3 dimensions (a 3 dimensional input
    space) into one dimension ('respon'), without losing much information.
    
    This produces a much cleaner body of vocabulary that can be used in 
    classifiers and regressions. 
    
    There are several stemmers. One is the snowball stemmer. """

# Snowball stemmer
from nltk.stem.snowball import SnowballStemmer
stemmer = SnowballStemmer("english")
stemmer.stem("responsiveness")   # output = 'respons'
stemmer.stem("responsivity")   # output = 'respons'
stemmer.stem("unresponsive")   # output = 'unrespons' <-

###############################################################################

### The BAG OF WORDS representation

""" A Bag Of Words is a representation of data for use in a machine learning 
    algorithm. 

    Stores a list of 'words' (vectors) and counts the number of times each one 
    appear in a corpus (text).
    
    The number of words/features/vectors in the bag of words = the number of 
    dimensions (that will be passed to a machine learning algorithm): 
        100 words = 100 dimensions. 
    
    As bag of words uses the same list of words for each doc in the corpus, the
    output is same length/no.dimensions for each doc, regardless of its length.
    This makes all docs in a corpus comparable, and means you can use the
    same algorithm to analyse the whole corpus, regardless of the length of 
    each individual document. 
 """

# The sklearn function CountVectorizer makes a bag of words
from sklearn.feature_extraction.text import CountVectorizer

# Prepare the data
""" CountVectorizer takes lists of strings as its input
     - a corpus (body of documents) such as emails """
input_list = [string1, string2, string3] 

# create the vectorizer
vectorizer = CountVectorizer()

# fit the vectorizer
bag_of_words = vectorizer.fit(input_list)

# transform the vectorizer 
bag_of_words = vectorizer.transform(input_list)

print bag_of_words   # get a list of tuples and integers, e.g:
    # (1, 7)   1        # means "in document number 1 word 7 occurs 1 time".
    # (1, 8)   3        # "in doc 1 word 8 occurs 3 times"
# doc 1 refers to the second string in the input_list, as indexing starts at 0.

# Find the feature (word) number of any word in the corpus
# e.g. if you think word #7 is "hello", test your hypothesis here:
print vectorizer.vocabulary_.get("hello")    # returns the word number of "hello"
""" See lesson on Feature Selection for how to look up feature numbers 
    and return the word """


###############################################################################

### TfIdf representation

# Two-step process: 
    # first weights words within docs using Tf
    # then uses Idf to weight words within the corpus, based on the results of Tf.

# Tf = term frequency                (up-weights words by freq of occurence in a DOC)
# Idf = inverse document frequency   (down-weights words by freq of occurrence in the CORPUS)

# So the result of TfIdf is that words RARE within a corpus have MORE WEIGHT
# than common ones.
# Because rare words help distinguish messages/senders of messages more.


# Tfidf vectorizer
# http://scikit-learn.org/stable/modules/generated/sklearn.feature_extraction.text.TfidfVectorizer.html

from sklearn.feature_extraction.text import TfidfVectorizer   

# Create a vectorizer
vectorizer = TfidfVectorizer(stop_words="english")  # remove english stopwords

# Fit and transform it - same as running vectorizer.fit() and then vectorizer.transform()
tfidf = vectorizer.fit_transform(word_data)

# Use get_feature_names applied to the VECTORIZER (not the matrix output)
# to find number of words
print "Num words:", len(vectorizer.get_feature_names())   # Num words: 3078
    
# What is word number 34597 in the vectorizer:
vectorizer.get_feature_names()[34597]    # = 'stephaniethank'

###############################################################################


### STOPWORDS

# Removing stopwords from a vectorizer
from nltk.corpus import stopwords    #nltk = the Natural Language Tool Kit
sw = stopwords.words("english")  # get the set of English stopwords
""" NOTE: if this produces an error <Resource u'corpora/stopwords' not found>
    it's because NLTK needs a corpus to extract the stopwords from.
    Do the following:
        
import nltk
nltk.download()    # Pops up a list of downloadables. Select "all-corpora" 
                     and download. 
                   # Then you can try running stopwords.words() again.
"""
sw = stopwords.words("english") 

len(sw)  # there are 153 stopwords

# see what some of the stopwords are
sw[0]  # = 'i'
sw[10]  # = 'yours'


###############################################################################











