## A set of functions for matrix inversion
## and holding the resulting matrix in cache once inversion is done

## makeCacheMatrix() builds a set of functions and returns the functions (list) to the parent environment

makeCacheMatrix <- function(ori = matrix()) {
        inv <- NULL
        set <- function(ori.set) {
                ori <<- ori.set
                inv <<- NULL
        }
        get <- function() ori
        setInv <- function(inv.set) inv <<- inv.set
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## cacheSolve() populates and/or retrieves the inverse of a Matrix from an object of type makeCacheMatrix()

cacheSolve <- function(f, ...) {
        inv <- f$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ori <- f$get()
        inv <- solve(ori, ...)
        f$setInv(inv)
        inv
}