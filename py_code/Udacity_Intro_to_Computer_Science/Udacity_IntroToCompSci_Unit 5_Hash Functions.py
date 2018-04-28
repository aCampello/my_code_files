# -*- coding: utf-8 -*-
"""
Created on Tue Jul 11 14:44:52 2017

Udacity Intro to Comp Sci Unit 5: Lesson 18: How Programs Run

Hash Tables (= dictionaries) & Hash Functions
"""

# Searching for keywords within lists (running lookups) can be slow.
# Hash functions increase the speed of lookups by reducing the number of
# places where a keyword is searched for.

# An index is a list of elements where each element is a key (keyword) and
# a value, such as a list of URLS that contain that keyword. Lookups from indexes
# must look at every element in the index (slow).

# A hash table changes an index into a structure where each element is a bucket,
# and each bucket contains a list of elements, and each of these elements is a key (keyword)
# and a value (list of URLS).

# Then when a keyword is searched for it's only necessary to look in one
# bucket, rather than through the whole index.

# Create an empty hash table
def make_hashtable(nbuckets):   # nbuckets = desired no. buckets
    result = []
    for i in range(0,nbuckets):
        result.append([])
    return result
# test
make_hashtable(5)    # makes empty hash table with 5 buckets.


#  Assign a keyword a bucket position based on each letter in the word.
def hash_string(keyword,buckets):   # the keyword is a key
    """ lookup a keyword in a hash table
    with a given number of buckets"""
    tot = 0
    for s in keyword:   # loop through each character in the keyword
        tot += ord(s)   # adds the ord of the character to the total
    return tot % buckets
# returns a number representing they keyword's position in the hashtable, which is calculated as the total ords for all characters in the keyword % the no. buckets. Imagine a circle with buckets number of markers around the edge, e.g. if have 12 buckets and the string number is 14 you go round the circle once (to get to 12) and then move two more markers (to get to 14) so the answer is the remainder (=2).

# Get the bucket that contains the keyword
# returns the whole bucket, e.g. [['Bill', 17]], nand ot the bucket number.
def hashtable_get_bucket(htable,keyword):
    return htable[hash_string(keyword,len(htable))]

# Add a key and it's associated value to a hashtable in the correct bucket.
def hashtable_add(htable,key,value):
    bucket = hashtable_get_bucket(htable, key) # find the bucket
    bucket.append([key, value])

# test the hashtable_add code
table = make_hashtable(5)
hashtable_add(table,'Bill', 17) # Bill is the key, 17 is the value
hashtable_add(table,'Chloe', 4)
hashtable_add(table,'Adam', 20)
print(table) # returns: [[], [['Chloe', 4], ['Adam', 20]], [['Bill', 17]], [], []]
# Chloe and Adam are in the same bucket


# Helper function to avoid repeated code in hash_lookup and hash_update (before, these functions contained for loops to go through each entry in a bucket searching for where entry[0]==key).

# Loop through entries in a given bucket and return the entry where the first value is the key.
def bucket_find(bucket, key):
    for entry in bucket:
        if entry[0] == key:
            return entry
    return None   # returns none if there the keyword does not appear in that bucket.

# See if the hashtable contains the keyword and if it does return the value associated with it.
def hashtable_lookup(htable,key):
    bucket = hashtable_get_bucket(htable,key)
    entry = bucket_find(bucket, key)
    if entry:
        return entry[1]
    else:
        return None

# test
hashtable_lookup(table, 'Bill')  # returns: 17.
hashtable_get_bucket(table, 'Bill') # returns: [['Bill', 17]]
hashtable_get_bucket(table, 'Chloe') # returns: [['Chloe', 4], ['Adam', 20]] # as Chloe and Adam share a bucket.

# If the key is already in the table, update the value associated with key to a new value. Otherwise, add a new entry for the key and value.
def hashtable_update(htable, key, value):
    bucket = hashtable_get_bucket(htable, key)   # find the bucket containing the keyword
    entry = bucket_find(bucket, key)  # identify the one with the keyword
    if entry:         # same as if entry is not None
        entry[1] = value            # update the value
    else:
        bucket.append([key, value])  # if key not already in hash table, add it.

# test
hashtable_update(table,'Bill',15)  # change 17 to 15 for key Bill.
hashtable_lookup(table, 'Bill')  # returns 15
