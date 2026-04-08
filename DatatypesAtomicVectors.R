# Data types and atomic vectors
# 7/04/2026
# GFP
#-----------------------------------

# Using the assignment operator

x <- 3 # preferred assignment operator
y = 4 # legal, but should be used only in functions
y = y + 1.1
print(y)
y <- y + 1.1

#convention for naming variables
z <- 5 # begin variable name with lower case
plantHeight <- 10 # option "camelcase"
plant.Height <- 11 #avoid periods in variable names
plant_height <- 12 # snake case - preferred
. <- 6.6 # reserve period for a generic holding variable

# one dimensional atomic vectors
# the combine functions
z <- c(2,3,6,7,4,9)
typeof(z)
print(z)
is.numeric(z)
is.character(z)

#character strings bracketed by single or double quotes
y <- c("perch", "bass", "trout")
print(y)
y <- c("this is only 'one' character string",
       'a second')
print (y)
is.character(y)

# building logicals
# Boolean, not with quotes, all caps)
x <- c(TRUE, TRUE, FALSE) # don't abbreviate
typeof(x)

#length
length(x)

