# Manual Test Instruction for Programming Assignment 2 of the R Programming course.
#
# This TI consists of the following TC Groups:
#
# TC Group 1 - operations on an empty object
# TC Group 2 - initialization of an empty object with an invertible matrix and operatons
#              upon it
# TC Group 3 - operations on non-emty-created object
# TC Group 4 - operations on a re-initialized object
# TC Group 5 - isolation between two instances of the object
#
# This TI assumes that the cachematrix.R is located in the current working directory.
# Furthermore, this TI assumes that no robustness against non-matrix or non-square matrices
# is required, hence it is not tested.
#
# Document revision: 1
#      Date created: 2015-06-21
#     Date modified: 2015-06-21
#

source("cachematrix.R")

######################
# TC Group 1

# TC1.1 Create an empty cache matrix object and store it in cmx1 variable. Neither error
# nor any other message should be returned.

cmx1 <- makeCacheMatrix()

# TC1.2 Get the stored matrix in cmx1 variable. It should return NA

cmx1$get()

# TC1.3 Get the cached inverse from cmx1. It should return NULL

identical(cmx1$getinv(), NULL)

# TC1.4 Call cacheSolve on cmx1. It should return NA

cacheSolve(cmx1)

# TC1.5 Call again cacheSolve on cmx1. It should again return NA without printing
# the message "getting cahced data"

cacheSolve(cmx1)

# TC1.6 Get the cached inverse from cmx1 after calls to cacheSolve in TC1.4 and TC1.5.
# It should return NULL

cmx1$getinv()

######################
# TC Group 2

# TC2.1 Create an invertible matrix mx1 and store it in cmx1 object. Neither error
# nor any other message should be returned.

mx1 <- diag(ncol = 4, nrow = 4)
mx1[1,] <- c(2,0,4,0)

cmx1$set(mx1)

# TC2.2 Get the stored matrix in cmx1 variable. It should return a matrix identical to mx1

identical(mx1, cmx1$get())

# TC2.3 Get the cached inverse from cmx1. It should return NULL

identical(cmx1$getinv(), NULL)

# TC2.4 Call cacheSolve on cmx1. It should return a matrix so that matrix multiplication
# with mx1 yelds a unit matrix and the call to cacheSolve DOESN'T print the message 
# "getting cahced data"

invmx1 <- cacheSolve(cmx1)
identical(invmx1 %*% mx1, diag(ncol = ncol(mx1), nrow = nrow(mx1)))

# TC2.5 Call again cacheSolve on cmx1. It should return a matrix so that matrix multiplication
# with mx1 yelds a unit matrix and the call to cacheSolve DOES print the message 
# "getting cahced data"

invmx1 <- cacheSolve(cmx1)
identical(invmx1 %*% mx1, diag(ncol = ncol(mx1), nrow = nrow(mx1)))

# TC2.6 Get the cached inverse from cmx1 after calls to cacheSolve in TC2.4 and TC2.5.
# It should return a matrix so that matrix multiplication with mx1 yelds a unit matrix

identical(cmx1$getinv(), invmx1)

######################
# TC Group 3

# TC3.1 Create an invertible matrix mx2 different to mx1. Verify that mx2 and mx1 are not
# identical. Create a new cache matrix object on mx2. Store that object in cmx2 variable.
# Neither error nor any other message should be returned.

mx2 <- matrix(c(1,-2,3,-2), nrow = 2, ncol = 2)
identical(mx1, mx2)
cmx2 <- makeCacheMatrix(mx2)

# TC3.2 Get the stored matrix in cmx2 variable. It should return a matrix identical to mx2

identical(mx2, cmx2$get())

# TC3.3 Get the cached inverse from cmx2. It should return NULL

identical(cmx2$getinv(), NULL)

# TC3.4 Call cacheSolve on cmx2. It should return a matrix so that matrix multiplication
# with mx2 yelds a unit matrix and the call to cacheSolve DOESN'T print the message 
# "getting cahced data"

invmx2 <- cacheSolve(cmx2)
identical(round(invmx2 %*% mx2), diag(ncol = ncol(mx2), nrow = nrow(mx2)))

# TC3.5 Call again cacheSolve on cmx2. It should return a matrix so that matrix multiplication
# with mx2 yelds a unit matrix and the call to cacheSolve DOES print the message 
# "getting cahced data"

invmx2 <- cacheSolve(cmx2)
identical(round(invmx2 %*% mx2), diag(ncol = ncol(mx2), nrow = nrow(mx2)))

# TC3.6 Get the cached inverse from cmx2 after calls to cacheSolve in TC3.4 and TC3.5.
# It should return a matrix so that matrix multiplication with mx2 yelds a unit matrix

identical(round(cmx2$getinv() %*% mx2), diag(ncol = ncol(mx2), nrow = nrow(mx2)))

######################
# TC Group 4

# TC4.1 Create an invertible matrix mx11 different to mx1 and mx2. Verify that mx11 is not
# identical to mx1 and mx2. Store mx11 matrix in already existing cmx1 object. Neither'=
# error nor any other message should be returned.

mx11 <- rbind(c(2,1,0),c(-1,1,-3),c(0.5,-1,-1))
identical(mx11, mx1)
identical(mx11, mx2)

cmx1$set(mx11)

# TC4.2 Get the stored matrix in cmx1 variable. It should return a matrix identical to mx11

identical(cmx1$get(), mx11)

# TC4.3 Get the cached inverse from cmx1. It should return NULL

identical(cmx1$getinv(), NULL)

# TC4.4 Call cacheSolve on cmx1. It should return a matrix so that matrix multiplication
# with mx11 yelds a unit matrix and the call to cacheSolve DOESN'T print the message 
# "getting cahced data"

invmx11 <- cacheSolve(cmx1)
identical(round(invmx11 %*% mx11), diag(ncol = ncol(mx11), nrow = nrow(mx11)))

# TC4.5 Call again cacheSolve on cmx1. It should return a matrix so that matrix multiplication
# with mx11 yelds a unit matrix and the call to cacheSolve DOES print the message 
# "getting cahced data"

invmx11 <- cacheSolve(cmx1)
identical(round(invmx11 %*% mx11), diag(ncol = ncol(mx11), nrow = nrow(mx11)))

# TC4.6 Get the cached inverse from cmx1 after calls to cacheSolve in TC4.4 and TC4.5.
# It should return a matrix so that matrix multiplication with mx11 yelds a unit matrix

identical(round(cmx1$getinv() %*% mx11), diag(ncol = ncol(mx11), nrow = nrow(mx11)))

######################
# TC Group 5

# TC5.1 Get matrices stored in cmx1 and cmx2 objects. They should be identical to mx11
# and mx2 respectively. Call cacheSolve on cmx1 and cmx2. Both calls should return
# cached inverses - the message "getting cahced data" DOES get printed.

identical(cmx1$get(), mx11)
identical(cmx2$get(), mx2)

cacheSolve(cmx1)
cacheSolve(cmx2)

# TC5.2 Store mx1 matrix in cmx1 object. Get back matrices stored in cmx1 and cmx2. cmx2
# should still return matrix identical to mx2 whereas cmx1 matrix identical to newly stored
# mx1. Call cacheSolve on cmx1 and cmx2. Call on cmx1 should NOT print the message 
# "getting cahced data" whereas a call to cacheSolve on cmx2 DOES print "getting cahced data"

cmx1$set(mx1)

identical(cmx1$get(), mx1)
identical(cmx2$get(), mx2)

cacheSolve(cmx1)
cacheSolve(cmx2)
