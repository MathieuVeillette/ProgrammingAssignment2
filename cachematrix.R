## The functions below take a matrix vector as an argument, and store it as a global
## variable using the <<- operator. They then compute the inverse, and store the answer 
## in a cache as well. If the functions are run again, the answer is already in the cache
## so the calculation is skipped, saving processing time.

## This function produces a list of four functions. See body of code for explanations 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ## Takes in a matrix and assigns it to the x variable, 
        ## and empties the cache (this will make sure that no answers from previous computations 
        ## get returned.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## Returns the matrix assigned as part of the set function; this will be used
        ## by the cacheSolve function to assign the matrix to a variable, on which
        ## the inverse calculation will be applied
        get <- function() x
        
        ## Saves an argument to the cache; this will be used to save the inverse 
        ## of the matrix calculated near the end of the cacheSolve function
        saveinverse <- function(inv) m <<- inv
        
        ## Returns the inverse matrix OR NULL; either the inverse is stored in global variable "m" 
        ## or it is empty
        getinverse <- function() m
        list(set = set, get = get,
             saveinverse = saveinverse,
             getinverse = getinverse)
}


## This function takes the argument x, which is the list of functions returned by makeCacheMatrix 
## From this list of functions it extracts the matrix that was passed to the function "set"
## and returns its inverse, either by calculating it, or returning the answer 
## stored in the cache if it's there
## note you have to call all functions from scratch if you want to compute a new inverse

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        ## checks if there's an answer stored in memory - if so it returns it, 
        ## otherwise jumps to code below next comment
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## if no answer stored, does the calculation
        data <- x$get()
        m <- solve(data, ...)
        x$saveinverse(m)
        m
}


