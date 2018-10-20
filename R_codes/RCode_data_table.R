m = matrix(seq(0, 4, 1), nrow=3, ncol=5)
print(m)


data(mtcars)
head(mtcars)
str(mtcars)

# subsetting
df[row_indexes, col_indexes] 
#df[row_indexes,] # selected rows, all cols
#df[, col_indexes] # selected cols, all rows

# select first 2 columns (mtcars[,1:2] does same thing as mtcars[1:2])
mtcars[,1:2]

# select first 2 rows, all columns
mtcars[1:2,]

# select first 2 rows, col 7 only
mtcars[1:2, 7]

# select first 2 rows, first 7 columns
mtcars[1:2, c(1:7)]

# all rows with mpg>20 and hp > 100
mtcars[mtcars$mpg > 20 & mtcars$hp > 100,]

### standard summary functions

unique(mtcars$cyl)

# apply
#2=columns
apply(mtcars, 2, mean) # one mean for each col
#1=rows
apply(mtcars, 1, mean) # one mean per row; doesnt make much sense

# lapply
lapply(mtcars$gear, mean) # mean gear for every row

# tapply
# useful to split a dataset into groups and summarise each group by applying a function, e.g, mean
tapply(Summary Variable, Group Variable, Function)
# mean hp for every cyl
tapply(mtcars$hp, mtcars$cyl, mean)
dt[, mean(hp), by=cyl]
dt[, mean(hp), by=.(am, cyl)] # use dot .() to group over multiple cols
# to label col heading
dt[, .(mean = mean(hp)), by=.(am, cyl)] 
# SQL equivalent is: 'SELECT am, cyl, AVG(hp) FROM mtcars GROUP BY 1,2;'
# filters
dt[mpg>5, .(mean=mean(hp)), by=.(am, cyl)]
# SQL equivalent is: 'SELECT am, cyl, AVG(hp) FROM mtcars WHERE mpg>5 GROUP BY 1,2;'

# summarise all columns by the mean within each number of cylinders
aggregate(mtcars, list(cylinders = mtcars$cyl, binary = mtcars$am), mean)
dt[,mean,]

### data table
library(data.table)


dt <- data.table(mtcars)
dt
class(dt)

# get mean mpg
dt[,mean(mpg)]
#mtcars[,mean(mpg)] # doesnt work with normal data frames


