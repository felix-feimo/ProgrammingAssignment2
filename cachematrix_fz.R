## The functions store inverse of matrices in terms of cache memory, which allows the cacheSolve function to read the cache
## data directly instead of doing another calculation, if given the same matrix.

## This function returns a list consisting of four sub-functions, describing the matrix passed in.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){ 
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setin <- function(inv) inverse <<- inv
        getin <- function() inverse
        
        list(set = set, get = get, setin = setin, getin = getin)
}


## This function calculates the inverse of a matrix. If already calculated, it instead reads the results from cache.

cacheSolve <- function(x, ...) {
        my_inv <- x$getin()
        if(!is.null(my_inv)){
                print("Reading cache data...")
                return(my_inv)
        }
        my_matrix <- x$get() 
        my_inv <- solve(my_matrix)
        x$setin(my_inv)
        my_inv
}

##Some tests :)
Hola <- makeCacheMatrix(matrix(rnorm(25,1), nrow=5, ncol=5))
#First time executing, calculates inverse as usual.
cacheSolve(Hola)
#But if pass the same matrix again, it reads data from cache instead.
cacheSolve(Hola)
