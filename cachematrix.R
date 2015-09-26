## File name: cachematrix.R 
## Brief description: R script that contains two functions, makeCachematrix and 
##   cacheSolve,  that are useful for caching the value of the inverse of a 
##   non-singular square matrix.
## Author: Justine Leon A. Uro (justineuro.moin@gmail.com)
## Date: 26 September 2015
## Additional Info:  For Programming Assignment 2: Lexical Scoping 
## https://class.coursera.org/rprog-032/human_grading/view/courses/975106/assessments/3/submissions
## Course: R Programming by Roger D. Peng, PhD, Jeff Leek, PhD, Brian Caffo, PhD
##
## Description of cachematrix.R:
## cachematrix.R contains two functions, makeCachematrix and cacheSolve, that
## are useful for caching the value of the inverse of a non-singular square matrix.
## The makeCacheMatrix function below is similar to the  makeVector function in 
## Example: Caching the Mean of a Vector (see README.md of Assignment 2). 
## This function takes an invertible square matrix as input then gives as 
## output a list containing four functions, one each for the following four 
## tasks:
## 
## 1.  set the value of the matrix labelled 'set' in list)
## 2.  get the value of the matrix (labeled 'get' in list)
## 3.  set the value of the inverse matix (labelled 'setxInv' in list)
## 4.  get the value of the inversematrix (lablled 'getxInv' in list)
##

makeCacheMatrix <- function(x = matrix()) {
            xInv <- NULL
            set <- function(y) {
                    x <<- y
                    xInv <<- NULL
            }
            get <- function() x
            setxInv <- function(xInvR) xInv <<- xInvR
            getxInv <- function() xInv
            list(set = set, get = get,
                 setxInv = setxInv,
                 getxInv = getxInv)
	}

## The cacheSolve function below takes as input the list output (processed vector)
## from makeCacheMatrix.  It calculates the inverse of the invertible
## square matrix that served as input fot makeCacheMatrix in the following
## manner:
## 1. If the inverse matrix of x, xInv, has already been calculated and 
##    is in the cache (of the environment in which the caclulations are 
##    being carried out), then it outputs the message: 'getting cached data'
##    and gives as output the cached value of xInv.
## 2. If xInv is not in the cache, then it calculates with solve(x$get()), 
##    caches xInv just calculated using x$setxInv(xInv), and then gives
##    the calculated xInv as output. 

cacheSolve <- function(x, ...) {
            xInv <- x$getxInv()
            if(!is.null(xInv)) {
                    message("getting cached data")
                    return(xInv)
            }
            data <- x$get()
            xInv <- solve(data, ...)
            x$setxInv(xInv)
            xInv
}

## Example: Calculating and Caching the Inverse of matrix(c(1,2,2,1),2,2)
##
## > amat <- matrix(c(1,2,2,1),2,2)
## > amat
##      [,1] [,2]
## [1,]    1    2
## [2,]    2    1
## > mCMamat <- makeCacheMatrix(amat)
## > cacheSolve(mCMamat)
##            [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333
##
## # The second time, the cache value of xInv is given as output.
## > cacheSolve(mCMamat)
## getting cached data
##           [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333
## > 

