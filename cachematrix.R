#### Info


##     R: help 
# The operators <- and = assign into the environment in which they are evaluated. 
# The operator <- can be used anywhere, whereas the operator = is only allowed at 
# the top level (e.g., in the complete expression typed at the command prompt) or 
# as one of the subexpressions in a braced list of expressions.

# <<- operator which can be used to assign a value to an object in 
# an environment that is different from the current environment.
# The operators <<- and ->> are normally only used in functions, and cause a 
# search to made through parent environments for an existing definition of the 
# variable being assigned. If such a variable is found (and its binding is not 
# locked) then its value is redefined, otherwise assignment takes place in the 
# global environment. 

## computer cache explained: https://www.youtube.com/watch?v=YJFOVtEQqRE
# cache: temporary storage bin for data that would be re-used 

#### Assignment 2: Lexical scoping

makeCacheMatrix <- function(x = numeric()) {
        cache <- NULL
        setMatrix <- function(y) {
                x <<- y
                cache <<- NULL
        }
        getMatrix <- function() {
                x
        }
        cacheInverse <- function(solve) {
                cache <<- solve
        }
        getInverse <- function() {
                cache
        }
        list(setMatrix = setMatrix, 
             getMatrix = getMatrix, 
             cacheInverse = cacheInverse, 
             getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(y, ...) {
        m <- y$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- y$getMatrix()
        m <- solve(data)
        y$cacheInverse(m)
        m
}
