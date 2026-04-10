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
typeof(x)

#names
z <- runif(6)

#names are optional attribute not initially assigned
names(z)
print(z)

# add names after variables is created
names(z) <- c("ant","wasp","bee","humile","polistes",'bumble')
print(z)

# add names when variable is built (quotes are optional at this point)

x <- c(gold=3, silver=5, lead=1)
print(x)

#reset names
names(x) <- NULL
print(x)

#names can be added for only a few elements
#names do not have to be distinct, but often are
names(x) <- c("copper", "zinc")

#using brackets to specify particular elements of an atomic vector
y <- c(10,12,15)
y[2] #specifies second element only
y[c(1,3)]
y[3] <-100
print(y)

#special elements in a vector

# NA to indicate missing values (even at excel)
y[3] <- NA
typeof(z)
length(z)
typeof(z[3])

z1 <- NA
typeof(z1)

is.na(y) # logical operator to find missing values
mean(y) # will not work because of missing values
!is.na(y) # will find elements that are not missing
mean(!is.na(y)) # wrong because calculating on TRUE FALSE
mean(y[!is.na(y)]) # correct way to eliminate NA

#NaN, -Inf, and Inf from numeric division
z1 <- 0/0
print(z1)
typeof(z1)
z1 <- 1/0
print(z1)
typeof(z1)

# NULL is an object that is nothing
# a reserved word in R
z <- NULL
length(z)
is.null(z)

