## The function makeCacheMatrix creates a list that contains functions to 
## 1. set the value of the matrix, 2. get the matrix of the vector
## 3. set the value of the inverse, and 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setInv <- function(solved) minv <<- solved
    getInv <- function() minv
    list(set = set, get = get,
         setInv = setInv, 
         getInv = getInv)
}


## Calculates the inverse of the matrix x (computed in makeCacheMatrix) 
## and stores it in solve
## The function first checks to see if the matrix inverse has already been solved
## If so, get inverse from the cache and skip computation 
## Otherwise solve the inverse and set it in the cache

cacheSolve <- function(x, ...) {
        minv <- x$getInv()
        if(!is.null(minv)) {
            message("getting cached data")
            return(minv) # function ends
        }
        # else
        data <- x$get() #assign variable matrix x from get to data
        minv <- solve(data) # solve inverse
        x$setInv(minv) # cache solved inverse
        minv
}