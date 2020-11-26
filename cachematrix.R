## A set of functions for matrix inversion
## and holding the resulting matrix in cache once inversion is done

## makeCacheMatrix() builds a set of functions and returns the functions (list) to the parent environment

makeCacheMatrix <- function(ori = matrix()) {
        inv <- NULL
        set <- function(ori.set) {
                ori <<- ori.set
                inv <<- NULL
        }
        # send original matrix to acheSolve()'s solve()
        get <- function() ori
        # cache inversion made by cacheSolve()'s solve()
        setInv <- function(inv.set) inv <<- inv.set
        # send cached inversion to cacheSolve() when prompted
        getInv <- function() inv
        # return a named list of functions
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## cacheSolve() populates and/or retrieves the inverse of a Matrix from an object of type makeCacheMatrix()

cacheSolve <- function(f, ...) {
        # return the matrix inversion from cache (if exists) to the function's output 
        inv <- f$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # OR
        # retrive original matrix from makeCacheMatrix() type object
        # send its inversion to both cacheSolve() output and makeCacheMatrix() cashed inv
        ori <- f$get()
        inv <- solve(ori, ...)
        f$setInv(inv)
        inv
}