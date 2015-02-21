
## makeCacheMatrix: This function creates a special "matrix object" that can cache its inverse. 


makeCacheMatrix <- function(x = matrix()) {
        # sets x equal to an empty matrix
        inv <- NULL
        # sets the inverse equal to NULL
        set <- function(y) {
                x <<- y
                # set function assigns the argument to x
                inv <<- NULL
                # Once the set function is called, the inverse is re-set to NULL
        }
        get <- function() x
        # get function returns the matrix
        setinv <- function(solve) inv <<- solve
        # setinv function overrides the previous value of the inverse 
        # and assigns the argument to inverse
        getinv <- function() inv
        # getinv functions returns the inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        # creates a list of the functions 
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
                # If the value of inverse is NOT null (was previously calculated), cacheSolve returns that value
        }
        # If the value of Inverse is NULL, then matrix x is retrived and it's inverse is calculated with the solve() function
        message("newly calculating data")
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
	# Sets Inverse to the newly calculated value
        inv # Returns the new Inverse value
}
