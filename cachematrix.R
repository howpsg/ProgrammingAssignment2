## Write a R function to cache the potentially time-consuming computations.
## Matrix inversion is gnerally a costly computation.
## Hence having a funciton that allow to cache the inverse of a matrix rather than always do the computation.
## Two functions below are used for caching the inverse of a matrix.

## This function is to create a list, and to compute the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    valinv <- NULL

## Setp 1. valset = set the value of the matrix
    valset <- function(y) {
        x <<- y
        valinv <<- NULL
    }
    
## Step 2. valget = get the value of the matrix
    valget <- function() x

## Step 3. valsetinverse = set the value of inverse of the matrix
    valsetinverse <- function(inverse) {
        valinv <<- inverse
    }

## Step 4. valgetinverse = get the value of inverse of the matrix
    valgetinverse <- function() {
        valinv
    }

    list(valset=valset, valget=valget, valsetinverse=valsetinverse, valgetinverse=valgetinverse)
}


## This function returns the inverse of the matrix.
## Assumption: the matrix is always invertible.
cacheSolve <- function(x, ...) {
    valinv <- x$valgetinverse()

## Step 1. first checks if the inverse has already been computed. 
## Step 2. if yes, gets the result from cached and skips the computation.
    if(!is.null(valinv)) {
        message("getting cached data")
        return(valinv)
    }
    
## Step 3. if no, computes the inverse, set the value in the cache via valsetinverse function.    
    valdata <- x$valget()
    valinv <- solve(valdata)
    x$valsetinverse(valinv)
    valinv
}

