##  makeCacheMatrix and cacheSolve work together to provide a
##  cached matrix solution, where the matrix solution (inverse)
##  is calculated once, remembered and reused.
##
##  Example usage:
##
##    > x <- matrix(c(1,0,5, 2,1,6, 3,4,0), nrow=3, ncol=3)
##
##    > x
##         [,1] [,2] [,3]
##    [1,]    1    2    3
##    [2,]    0    1    4
##    [3,]    5    6    0
##
##    > cm <- makeCacheMatrix(x)
##
##    > cacheSolve(cm)
##         [,1] [,2] [,3]
##    [1,]  -24   18    5
##    [2,]   20  -15   -4
##    [3,]   -5    4    1
##
##    > cacheSolve(cm)
##    getting cached data
##         [,1] [,2] [,3]
##    [1,]  -24   18    5
##    [2,]   20  -15   -4
##    [3,]   -5    4    1
##


##  makeCacheMatrix()
##
##    Takes this argument:
##
##      mat      - the matrix
##
##    And creates a closure:
##
##      http://en.wikipedia.org/wiki/Closure_(computer_programming)
##
##    With an environment that contains:
##
##      mat      - the matrix
##      solution - the solution (inverse) of mat or NULL
##
##    And returns a list that contains these functions:
##
##      set         - sets the matrix
##      get         - gets the matrix
##      setSolution - sets the solution
##      getSolution - gets the solution

makeCacheMatrix <- function(mat = matrix()) {
    solution <- NULL
    set <- function(m) {
        mat <<- m
        solution <<- NULL
    }
    get <- function() mat
    setSolution <- function(sol) solution <<- sol
    getSolution <- function() solution
    list(set = set,
        get = get,
        setSolution = setSolution,
        getSolution = getSolution)
}

##    cacheSolve()
##
##      Takes this argument:
##
##        cMat, a cached matrix as created by makeCacheMatrix()
##
##      and returns:
##
##        the solution (matrix inverse)
##
##      If a cached value exists, that value is returned, otherwise
##      the solution is calculated, stored in the cache, and returned.

cacheSolve <- function(cMat, ...) {
    #### Return a matrix that is the inverse of 'cMat'
    solution <- cMat$getSolution()
    if(!is.null(solution)) {
        message("getting cached data")
        return(solution)
    }
    mat <- cMat$get()
    solution <- solve(mat, ...)
    cMat$setSolution(solution)
    solution
}


## Extra Credit ###############################################################

## Above I've done what I think was expected and used the given
## cahced mean example as a template. Thank you for taking the
## time to review my work. If you're too busy you don't need to
## review this alternative implementation, but I want to share a
## couple changes that I made.
##
## While working on this assignment few questions came up:
##
##    1. What if there wasn't a separate cacheSolve function?
##        Instead, what if we added "getSolution" to the list
##        of functions returned by makeCacheMatrix and had it
##        implement the cache logic?
##
##    2. What if function list was created directly
##        As it is we create the functions, assign them names,
##        and use those names in the returned list. Why not
##        create the functions directly in the list? While
##        it looks a little weird at first, I've see it done
##        this way unapologetically in JavaScript libraries.
##
## Below is my implementation of these changes (which I've
## tested and verified). I think it's simpler (it's certainly
## less code).

##  makeLazyMatrix()
##
##    Takes these arguments:
##
##      mat      - the matrix
##
##    And creates a closure:
##
##      http://en.wikipedia.org/wiki/Closure_(computer_programming)
##
##    With an environment that contains:
##
##      mat      - the matrix
##      solution - the solution (inverse) of mat or NULL
##
##    And returns a list that contains these functions:
##
##      set         - sets the matrix
##      get         - gets the matrix
##      getSolution - gets the (possibly cached) solution
##      clearCache  - clears cache
##
##  Example usage:
##
##    > x <- matrix(c(1,0,5, 2,1,6, 3,4,0), nrow=3, ncol=3)
##
##    > x
##         [,1] [,2] [,3]
##    [1,]    1    2    3
##    [2,]    0    1    4
##    [3,]    5    6    0
##
##    > lm <- makeLazyMatrix(x)
##
##    > cacheSolve(cm)
##         [,1] [,2] [,3]
##    [1,]  -24   18    5
##    [2,]   20  -15   -4
##    [3,]   -5    4    1
##
##    > lm$getSolution()
##         [,1] [,2] [,3]
##    [1,]  -24   18    5
##    [2,]   20  -15   -4
##    [3,]   -5    4    1

makeLazyMatrix <- function(mat = matrix()) {
    solution <- NULL
    list(
        set = function(m) {
            mat <<- m
            solution <<- NULL
        },

        get = function() mat,

        getSolution = function(...) {
            if(is.null(solution)) {
                solution <<- solve(mat, ...)
            }
            solution
        },

        clearCache = function() solution <<- NULL
    )
}

