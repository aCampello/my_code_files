# -*- coding: utf-8 -*-
"""
Created on Wed Jul 12 11:08:57 2017

Udacity Intro to Comp Sci Unit 6: Lesson 22: How To Have Infinite Power
"""
# Recursive procedures have a base case, where if the base case is true the procedure ends,
# and a recursive case, where if the base case is false it runs a <block>, and continues looping until the base case is true (e.g. for word choices in grammars/sentence structure in unit 1).

# Recursive procedures are often more elegant and easier to get correct than
# iterative (loop) procedures, but they are also slower/ more expensive, so are
# not always the best choice if working with large inputs or when worried about performance.

# E.g. recursive procedure: factorial
def factorial(n):
    # Define the base case, which here is when n=0 & the output can only be 1.
    if n == 0:
        return 1
    # Define the recursive case, which executes if the base case is False.
    else:           # for n>0, factorial(n) = n*factorial(n-1)
        print("n=", n)
        return n * factorial(n-1)   # calling the same function within itself
                                    # is OK, because we defined a base case
# test
factorial(3)  # = 6
# Explanation:
# Step 1. The base case n==0 is False, so the else statement is executed, returning 3*factorial(2)
# Step 2. To find the result of factorial(2), n is now 2, so the base case is still False, the else returns 2*factorial(1)
# Step 3. To find the result of factorial(1), n is now 1, so the base case is still False, the else returns 1*factorial(0)
# Step 4. To find the result of factorial(0), n is now 0, so the base case is True and returns 1
# Step 5. So the result of factorial(3) is 6, because:
# Because:
factorial(0) = 1
# Then:
factorial(1) = 1*factorial(0) = 1
# And:
factorial(2) = 2*factorial(1) = 2
# So:
factorial(3) = 3*factorial(2) = 6



# Recursive procedure to determine if a string is a pallindrome
def is_palindrome(s):
    if s == "":         # base case: the empty string
        return True
    else:               # recursive case: keeps calling is_palindrome until it
                        # returns false OR the base case is true.
        if s[0] == s[-1]:       # compares first letter with last letter (-1
                                # or -i in a loop returns the mirror of a position)
            return is_palindrome(s[1:-1])   # removes first and last letter from string
        else:
            return False
# test
print(is_palindrome('level'))


### Fibonacci numbers
# 2 base cases
fibonacci(0) = 0
fibonacci(1) = 1
# 1 recursive case
fibonacci(n) = fibonacci(n-1) + fibonacci(n-2)

# Recursive procedure to calculate fibonacci numbers
def fibonacci(n):
    if n == 0 or n == 1:
        return n
    else:
        return fibonacci(n-1) + fibonacci(n-2)
# Note that using a recursive method is not efficient for calculating fibonacci numbers,
# because there are lots of duplicate computation occurring when large numbers are calculated,
# e.g. to calc fib(34) you must first calc fib(33) and fib(32), and to calc fib(33)
# you must first calculate fib(32) and fib(31), so fib(32) is calculated twice.
# This increases *exponentially*:
    # fibonacci(36) would result in fibonacci(34) being calculated 2 times,
    # fib(33) 3 times, fib(32) 5 times, fib(31) 8 times, fib(30) 13 times and so on....
# So a While Loop would be more efficient by reducing duplicate computation.

# Iterative procedure to calculate fibonacci numbers
def fibonacci(n):
    current = 0  # current number
    after = 1  # next number
    for i in range(0,n):    # for each number from 0-n
        current, after = after, current+after   # multiple assignment: this calculates
                                                # the values on the right first and then
                                                # assigns them to current and after on the left.
    return current  # once n is reached in the for loop, the output is current.

# Explanation of the loop:
# e.g. for fibonacci(0):
    for i in 0:0:
        # first loop
        current=0
        after=1
        # end of loop (as 0:0 is a loop of length 1)
    returns 0+1=1
# same for fibonacci(1)  # 0:1 is a loop of length 1, so returns 0+1=1

# e.g. for fibonacci(3):
    for i in 0:3:
        # first loop
        current=0
        after=1
        # next loop (current is now 0+1=1)
        current=1
        after=1
        # next loop (current is now 1+1=2)
        current=2
        after=1
        # next loop (current is now 2+1=3)
        current=3
        after=2
        # no more loops: returns 3+2=5
    returns 5

# Another recursive procedure
# calculates no. hexamesters needed to gather a target number of udacity users,
# where in the first hex you start with n users and they each recruit 'spread' number of users.
def hexes_to_udaciousness(n, spread, target):
    if n>= target:
        return 0
    else:
        return 1 + hexes_to_udaciousness(n*(1+spread), spread, target)

hexes_to_udaciousness(5000, 2, 150000)  # returns 4
