## This code is for the purpose of the Peer Assessment 2 of the R Programming Course on Coursera
## In this Programming Assignment we take advantage of the scoping rules of the R language and how 
## they can be manipulated to preserve state inside of an R object.
## The code basically computes the inverse of a matrix and stores it's inverse in a cache
## If during subsequent runs the contenets of the matrix do not change then it looks up the value of the matrix in the
## cache instead of computing the inverse again and again.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {                     # argument x is of type matrix
        inverseOfx <- NULL
        set <- function(y){                                     # set the inverse of matrix in cache
                x <<- y
                inverseOfx <<- NULL                             # if new computations are done then remove the old contents
        }  
        get <- function(){x}                                    # return the inverse of the matrix
        setInverse <- function(solve){inverseOfx <<- solve}     # set the inverse of the matrix using cacheSolve function
        getInverse <- function(){inverseOfx}                    # return the inverse of the matrix
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)            # return these items

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverseOfx <- x$getInverse()                    # query the x matrix's cache
        if(!is.null(inverseOfx)){                       # if there is a cache
                message("getting cached data")          
                return(inverseOfx)                      # just return the cache, no computation needed 
        }
        data <- x$get()                                 # if there's no cache
        inverseOfx <- solve(data,...)                   # compute the inverse of matrix
        x$setInverse(inverseOfx)                        # save the result back to x's cache
        inverseOfx                                      ## return a matrix that is the inverse of 'x'
        
}
