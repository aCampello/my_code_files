# -*- coding: utf-8 -*-
"""
Created on Tue Jul 25 14:42:38 2017

PLOTTING HELP

From http://www.ast.uct.ac.za/~sarblyth/pythonGuide/PythonPlottingBeginnersGuide.pdf
"""

# Standard plotting packages
import numpy as np
import pylab as pl

# more advanced plotting
import matplotlib.pyplot as plt

# Example data
x = [1, 2, 3, 4, 5]
y = [1, 4, 9, 16, 25]


### lineplot
pl.plot(x, y)   # use pylab to plot x and y


### scatterplot: just add an extra parameter to specify the point type
pl.plot(x, y, 'o')    # shows circular points


# Formatting colours and symbols
pl.plot(x, y, 'r')     # red line ('b' would be blue line)
pl.plot(x,y, 'r--')    # red dashed line
pl.plot(x, y, 'ro')    # red points ('rs' = red squares, 'rx' = red crosses)

# keys for other colours:
b blue
g green
r red
c cyan
m magenta
y yellow
k black
w white

# keys for other markers:
's' square marker
'p' pentagon marker
'*' star marker
'h' hexagon1 marker
'H' hexagon2 marker
'+' plus marker
'x' x marker
'D' diamond marker
'd' thin diamond marker


# axis labels
pl.xlabel('put text here')
pl.ylabel('put text here')
pl.title('Put plot title here')


# set axis limits
pl.xlim(x_low, x_high)  # e.g. pl.xlim(0.0, 7.0)
pl.ylim(y_low, y_high)  # e.g. pl.ylim(0.0, 30.)


### Plotting more than one plot on the same set of axes

# First define the x and y arrays for each plot:
x1 = [1, 2, 3, 4, 5]
y1 = [1, 4, 9, 16, 25]
x2 = [1, 2, 4, 6, 8]
y2 = [2, 4, 8, 12, 16]

# Then use pylab to plot x and y
pl.plot(x1, y1, 'r')
pl.plot(x2, y2, 'go')


# NOTE: plots are automatically shown after each cell of code is run, so to
# add formatting, select and run all lines at once and include:
pl.show()
# at the end to display the completed plot, e.g...
pl.plot(x1, y1, 'r')
pl.plot(x2, y2, 'go')
pl.title('My plot')
pl.xlim(0.0, 7.0)
pl.ylim(0.0, 30.)
pl.show()


### Histogram

# make array of 1000 random numbers with gaussian distrib, mean = 5.0 and rms = 3.0.
data = np.random.normal(5.0, 3.0, 1000)

pl.hist(data)       # make a histogram of the data array
pl.ylabel('ydata')   # make plot labels
pl.xlabel('xdata')   # make plot labels
pl.show()           # show the plot


### Plotting more than one plot per canvas
# first make a figure and then specify subplots:
fig1 = pl.figure(1)
pl.subplot(211)     # 211 creates a fig with 2 rows & 1 col, and draws in the top row.
pl.subplot(212)     # 212 creates a fig with 2 rows & 1 col, and draws in the bottom row.


# save as png
fig.savefig('fig1.png', bbox_inches='tight')


# add common axis labels to grid of subplots
fig, ax = plt.subplots(nrows=3, ncols=2, sharex=True, sharey=True, figsize=(10, 8))
fig.text(0.5, 0.06, 'Year', ha='center')
fig.text(0.06, 0.5, '% Total annual investment (£)', va='center', rotation='vertical')




# force axis labels to be integers not floats

from  matplotlib.ticker import FuncFormatter

# then paste this line after the plot code, e.g.
fig, ax = plt.subplots(nrows=3, ncols=2, sharex=True, sharey=True, figsize=(10, 8))
plt.subplot(321)
plt.plot(spend_comms.year, spend_comms.spend_inflation_adj, label = "Communication")
plt.gca().xaxis.set_major_formatter(FuncFormatter(lambda x, _: int(x)))

