## In this example we introduce the <<- operator which can be used to assign a 
## value to an object in an environment that is different from the current 
## environment. Below are two functions that are used to create a special object
## that stores a matrix and cache's its inverse.

## The first function, makeCacheMatrix creates a special "matrix", which is 
## really a matrix containing a function to
##    set the value of the matrix
##    get the value of the matrix
##    set the value of inverse of the matrix
##    get the value of inverse of the matrix

## Caveat: It is assumed that matrix 'x' is a square matrix and is invertible
## meaning the determinant of |x| <> 0 (not equal to zero).

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL

        set <- function(y) {

                x <<- y       ## set the value of x in an environment outside of makeCacheMatrix()

                inv <<- NULL  ## set the value of inv in an environment outside of makeCacheMatrix()
        }

        get <- function() x

        setinverse <- function(inverse) inv <<- inverse    ## anonymous function; again inv value 
                                                           ## is set in env outside of
                                                           ## makeCacheMatrix()

        getinverse <- function() inv

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)  ## as you know the last executed statement in R program
                                       ## is the result object; In this case the returned matrix
                                       ## 'x' will have these additional attributes get, set,
                                       ## setinverse and getinverse.

}

## The following function cacheSolve calculates the inverse of the special 
## "matrix" created with the above function. However, it first checks to see if
## the inverse has already been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of the
## matrix and sets the inverse value in the cache via the setinverse function
## before it returns with inverse value. Subsequent calls to this function will
## retrieve cached values.

## Caveat: The assumption here is that the original matrix 'x' is static, otherwise
## you have to explicitly call inv <- makeCacheMatrix(x) to re-initialize inv.

cacheSolve <- function(x, ...) {

        inv <- x$getinverse()

        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)                    ## since value is set in inv, return function from here
        }

        ## Inverse matrix is not set in 'inv'
        message("First generating inverse to cache and then getting cached data")
    
        data <- x$get()

        inv <- solve(data, ...)

        x$setinverse(inv)

        inv

}

## > source("cachematrix.R")
## > x = rbind(c(1, -0.5), c(-0.5, 1))
## > 
## > inv=makeCacheMatrix(x)
## > 
## > inv$get()
##      [,1] [,2]
## [1,]  1.0 -0.5
## [2,] -0.5  1.0
## > 
## > cacheSolve(inv)
## First generating inverse to cache and then getting cached data
##           [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333
## > cacheSolve(inv)
## getting cached data
##           [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333
