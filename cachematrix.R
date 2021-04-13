## The aim is to write two functions - makeCacheMatrix, and cacheSolve
## We are working under the assumption that the provided matrix is invertible
## Also, all comments indicate processes that will be undertaken in the
## Succeeding line of code. 


## MakeCacheMatrix intends to create a matrix object capable of caching 
## Its own inverse

## cacheSolve computes the inverse of aforementioned matrix and/or
## Retrieves said inverse from the cache

## makeCacheMatrix consists of set, get, setinv, and getinv
## The Modern Applied Statistics with S (MASS) library has been used
## To allow the function to get inverses of square and non-square matrices

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
      
      # initialising the inverse as a NULL/undefined object
      
      inv <- NULL   
      
      # creating the set function to assign 'y' to 'x'
      
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      # creating the get function to get matrix 'x'
      
      get <- function() {x}
      
      # function to set inverse
      
      setinv <- function(inverse) {inv <<- inverse}
      
      # function to obtain the inverse
      
      getinv <- function() {
            inver <- ginv(x)
            inver%*%x
      } 
      
      list(set = set, get = get, setinv = setinv, getinv = getinv)
      
      
}


## cacheSolve gets cached data, checks to see whether the matrix is inverted
## if inverse has been calculated and the matrix unchanged, we return the
## inverse from the cache. If the inverse is not calculated, the function
## computes said inverse 

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      
      ## 'if' loop to check whether the inverse is NULL
      
      if (!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      
      # solve inverse value
      
      inv <- solve(data, ....)
      x$setinv(inv)
      
      ## return the inverse of the matrix x
      
      inv
}
