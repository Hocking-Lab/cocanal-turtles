# getting help
#?function
#??word
#google! 
#stackoverflow
?sum
??sum
  
getwd()  ## if in project, directory will be project
set.seed(614623) ##insures randomness produces same result given multiple people run same script - starts at same spot

##
## Introduction to data types and classes  
## 

## 

## THE BASIC INGREDIENTS OF R: VARIABLES AND ASSIGNMENT

# examples of simple assignment
x <- 5
y <- 4
# the variables can be used in other operations 
x+y
# including defining new variables
z <- x + y 
z
# which can then be passed to other functions
sqrt(z)

# example of vector assignment
tree.heights <- c(4.3, 7.1, 6.3, 5.2, 3.2, 2.1) 
tree.heights
tree.heights*2
tree.heights^2 # = tree.heights**2
sum(tree.heights)
mean(tree.heights)
max.height <- max(tree.heights)
max.height
which(tree.heights==max.height)  ## "which of tree heights is equal to the max height", returns index
which.max(tree.heights) ## returns location (index) in object that is the max value

# indexing
length(tree.heights)
tree.heights[1] # first element
tree.heights[c(1,4,6)]

tree.heights[1:3] # a subset of elements 1 to 3
tree.heights[-c(1:3)] #remove elements 1 to 3
tree.heights[-1:3] ## will not work; wants negative 1st number

tree.heights[c(5,3,2)] # a subset of elements 5,3,2: note the ordering
tree.heights[which.max(tree.heights)] ## will give the max tree height

# examples of character variable assignment
name <- "Todd Gack"
name
# these can be assigned to a vector of character variables
cities <- c("Leicester","Newcastle","London","Durham", "Exeter")
cities


# an example of a logical variable
northern <- c(FALSE, TRUE, FALSE, TRUE, FALSE) # also 0s and 1s
northern
# this can be used to subset other variables
cities[northern] ## will give those elements that were then also set to TRUE

## Info Box
##### Example Script #####
## these commands will not do anything  
## Load libraries
install.packages("GISTools")
library(GISTools)
## Load functions 
source("my.functions.R") ## loads any of that code into current working console, just added 2 functions to workspace (look in environment)
ls() ## list everything in workspace
## load some data
my.data <- read.csv(file = "richTable.csv")
rm(list = ls()) ## remove object
head(my.data)
View(my.data)
class(my.data) ## type of object; commonly used (not with spatial data)
str(my.data) #structure; observation #, variable # and type of data each is
summary(my.data) #min, quartiles, median, mean, # of NA's
dim(my.data) ## dimensions

## apply a function written in My.functions.R 
cube.root(my.data[1:3,"richness"]) ## apply cube root to entities 1-3 in column richness
## apply another R function
cube.root(my.data$richness[1:3]) ## $ grabs 1 column via name from data frame; city @ roads $ length, if multiple subsets...
row.tot <- rowSums(my.data)
row.tot

my.data <- na.omit(my.data) ## removes rows that have NA values (only keeps complete cases!!; so ... don't do this unless verrrrryyyyyyyyyy sure, YOU CAN LOSE EVERYTHING); NA action tells which rows omitted
dim(my.data)


## DATA TYPES AND DATA CLASSES

## Data Types in R

character(8)
# conversion
as.character("8") #treats as a character object
# tests
is.character(8) 
is.character("8")

numeric(8)
character(8)
# conversions
as.numeric(c("1980","-8","Geography")) ## R doesn't know how to make "Geography" a numeric value
as.numeric(c(FALSE,TRUE))
sum(tree.heights > 3) ## number of entities that meet that classification
# tests
is.numeric(c(8, 8))
is.numeric(c(8, 8, 8, "8"))

logical(7)  ## "give us 7 logicals"
# conversion
as.logical(c(7, 5, 0, -4, 5))
# TRUE and FALSE can be converted to 1 and 0; nonzero is TRUE
as.logical(c(7,5,0,-4,5)) * 1 ## [1] 1 1 0 1 1 as.logical(c(7,5,0,-4,5)) + 0 ## [1] 1 1 0 1 1
# different ways to declare TRUE and FALSE
as.logical(c("True","T","FALSE","Raspberry","9","0", 0)) ## can't have vector that mixes characters and numbers so converts all to one or other... in this case, all characters

data <- c(3, 6, 9, 99, 54, 32, -102) 
# a logical test
index <- (data > 10)
index
# used to subset data
data[index]
sum(data)
sum(data[index])

## Data Classes in R

# testing and conversion
tmp <- data.frame(a=10:15, b=15:20) 
class(tmp)
is.vector(tmp)

# defining matrices
matrix(ncol = 2, nrow = 2)
matrix(10, ncol = 2, nrow = 2) 
matrix(1:6)
matrix(1:6, ncol = 2)
as.matrix(6:3)

flow <- matrix(c(2000, 999, 543, 1243, 212, 45, 654, 168, 109), 
               c(3,3), 
               byrow=TRUE) #want to fill by rows first (zigzag by row)
flow
flow <- matrix(c(2000, 999, 543, 1243, 212, 45, 654, 168, 109), 
               c(3,3), 
               byrow=FALSE)
flow
# Rows and columns can have names, not just 1,2,3,...
colnames(flow) <- c("Leicester", "Liverpool"," Elsewhere") 
rownames(flow) <- c("Leicester", "Liverpool", "Elsewhere") 
# examine the matrix
flow

z <- c(6,7,8)
names(z) <- c("Newcastle","London","Manchester") 
z
class(z)


# and functions exist to summarise
rowSums(flow) 
colSums(flow)
colMeans(flow)

?apply  # apply a function to a dataset (want to do something to every row or column/or element; could use a for loop; or use an apply function; lapply, sapply, mapply)
apply(X=flow,
      MARGIN = 2,
      FUN = mean) # X = data, Margin = work with row (1) or column (2), FUN = function

apply(flow, 2, mean)


# a vector assignment
house.type <- c("Bungalow", "Flat", "Flat", "Detached", "Flat", "Terrace", "Terrace") # R will often assume characters are factors
# a factor assignment
house.type <- factor(c("Bungalow", "Flat",
"Flat", "Detached", "Flat", "Terrace", "Terrace"), levels=c("Bungalow","Flat","Detached","Semi","Terrace"))
house.type
# table can be used to summarise
table(house.type)
# 'levels' control what can be assigned 
house.type <- factor(c("People Carrier", "Flat",
"Flat", "Hatchback", "Flat", "Terrace", "Terrace"),
levels=c("Bungalow","Flat","Detached","Semi","Terrace")) 
house.type

income <-factor(c("High", "High", "Low", "Low", "Low", "Medium", "Low", "Medium"), levels=c("Low", "Medium", "High"))
income > "Low"
# ‘levels’ in ‘ordered’ defines a relative order
income <-ordered (c("High", "High", "Low", "Low", "Low", "Medium", "Low", "Medium"), levels=c("Low", "Medium", "High"))
income > "Low"

# lists
tmp.list <- list("Todd Gack",c(2005, 2009), "Lecturer", matrix(c(6,3,1,2), c(2,2))) #put together a bunch of diff types of elements (characters, matrix, etc.)
tmp.list
dim(tmp.list) # lists do not have dimensions, but have a length
length(tmp.list)

# NOTE INDEXING DIFFERS FOR LISTS
# elements of the list can be selected
tmp.list[[4]] #index an element of list
tmp.list[[2]]
tmp.list[[2]][1] #index something within an element
tmp.list[[4]][1,2] #index within a matrix that is an element

employee <- list(name="Todd Gack", start.year = 2005, position="Professor") #names diff. elemnts of list and can still index
employee
append(tmp.list, list(c(7,6,9,1)))
lapply(tmp.list[[2]], is.numeric) #apply that returns a list and can apply to list
lapply(tmp.list, length)

## READING, WRITING, LOADING AND SAVING DATA

## Text Files
mat <- matrix(runif(10), nrow=10, ncol=10) #r uniform function, draw # from uniform distribution
rownames(mat) <- paste("name", 1:10, sep="_") #??
dput(mat, "deleteMELater.txt") # write a text file that looks like the object
mat <- dget("deleteMELater.txt")
write.csv(mat, file = "test.csv")
write.csv(mat, file = "test.csv", row.names = F)
mat <- read.csv("test.csv")

mat

## R Data Files

# this will save everything in the workspace
save(list = ls(), file = "MyData.RData") #save everything that you did, start script where left off
# this will save just mat
save(list = "mat", file = "MyData.RData")
# this will save appling and georgia.polys 
save(list = c("mat", "tmp.list"), file = "MyData.RData")
load("MyData.RData") ## very helpful if have to load data (do something that takes a lot of time)


## PLOTTING

## Basic Plot Tools

x1 <- rnorm(100) 
y1 <- rnorm(100) 
plot(x1,y1)
plot(x1,y1,pch=16, col='red', main="My Plot", xlab="Var 1")

x2 <- seq(0,2*pi,len=100)
y2 <- sin(x2)
plot(x2,y2,type='l')
plot(x2,y2,type='l', lwd=3, col='darkgreen')

plot(x2,y2,type='l', col='red', lwd=3, ylim=c(-1.2,1.2))
y2r <- y2 + rnorm(100,0,0.1)
points(x2,y2r, pch=16, col='blue')

y4 <- cos(x2)
plot(x2, y2, type='l', lwd=3, col='darkgreen') 
lines(x2, y4, lwd=3, lty=2, col='darkblue')

## Info Box
par(mfrow = c(1,2)) 
# reset
par(mfrow = c(1,1))

x2 <- seq(0,2*pi,len=100)
y2 <- sin(x2)
y4 <- cos(x2)
# specify the plot order: see ?par for more information 
par(mfrow = c(1,2)) ## split plotting window
# plot #1
plot(y2,y4)
polygon(y2,y4,col='lightgreen')
# plot #2: this time with ‘asp’ to set the aspect ratio of the axes
plot(y2,y4, asp=1, type='n') 
polygon(y2,y4,col='lightgreen')


library(GISTools)
data(georgia)
class(georgia)
str(georgia)
# select the first element 
appling <- georgia.polys[[1]]
# set the plot extent
plot(appling, asp=1, type='n', xlab="Easting", ylab="Northing")
# plot the selected features with hatching 
polygon(appling, density=14, angle=135)

dev.off() ## Turns the plotting device off (clears everything, eg. par())
plot(appling, asp=1, type='n', xlab="Easting", ylab="Northing")
# plot the selected features with hatching 
polygon(appling, density=14, angle=135)

## Plot Colors

plot(appling, asp=1, type='n', xlab="Easting", ylab="Northing")
polygon(appling, col=rgb(0,0.5,0.7))

plot(appling, asp=1, type='n', xlab="Easting", ylab="Northing")
polygon(appling, col=rgb(0,0.5,0.7,0.4))

# set the plot extent
plot(appling, asp=1, type='n', xlab="Easting", ylab="Northing")
# plot the points
px <- runif(500,126,132)*10000
py <- runif(500,103,108)*10000
points(px, py, pch=16, col='red') 
# plot the polygon with a transparency factor 
polygon(appling, col=rgb(0,0.5,0.7,0.4))

plot(px, py, pch=16, col='red')

# add text
plot(appling, asp=1, type='n', xlab="Easting", ylab="Northing")
polygon(appling, col="#B3B333")
# add text, sepcifying its placement, colour and size 
text(1287000,1053000, "Appling County",cex=1.5) 
text(1287000,1049000, "Georgia",col='darkred')

plot(c(-1.5,1.5),c(-1.5,1.5),asp=1, type='n')
# plot the green/blue rectangle
rect(-0.5,-0.5,0.5,0.5, border=NA, col=rgb(0,0.5,0.5,0.7))
# then the second one
rect(0,0,1,1, col=rgb(1,0.5,0.5,0.7))