## cachematrix.R: Cache a matrix and its inverse

## These functions support caching of a matrix and its inverse in order to
## avoid potentially-lenghty computations: A given matrix's inverse is
## calculated once and then cached for future use.
##
## How to use:
##
##   m <- matrix(c(1, 4, 5, 2, 0, 8, 3, 2, 5), 3, 3) # The original matrix
##   mcached <- makeCacheMatrix(m)                   # Cache the matrix
##   minv <- cacheSolve(mcached)                     # Find inverse and cache
##   minv2 <- cacheSolve(mcached)                    # Return cached inverse
##
## How it works:
##
## The makeCacheMatrix() and cacheSolve() functions use closures, defined
## as "a function or reference to a function together with a referencing
## environment--a table storing a reference to each of the non-local variables
## (also called free variables or upvalues) of that function" (Wikipedia).
##
## When invoked makeCacheMatrix() defines four "getter" and "setter" functions
## and then returns a list containing references to each of the four functions.
## Each of the functions is bound to the environment associated with that
## particular invocation of makeCacheMatrix(). The environment in turn contains
## two variables x and inv used for storing the cached matrix and its inverse.
## (These variables are defined in makeCacheMatrix() itself, and thus are
## free variables with respect to the four getter and setter functions.)
##
## The variable x is set to the value of the matrix originally passed to
## makeCacheMatrix(), and the variable inv is set to the inverse of the matrix
## as calculated in the cacheSolve() function.


## Create an object to hold a matrix and its (cached) inverse.
makeCacheMatrix <- function(x = matrix()) {

        # x and inv are variables in the (new) environment that is
        # instantiated each time makeCacheMatrix() is called.
        inv <- NULL

        # Define getter and setter functions referencing x and inv.

        # Set the object's matrix value and clear the cached inverse.
        set <- function(y) {
                # NOTE: We use the <<- assignment operator because we want to
                # set the variables x and inv in the environment associated
                # with this particular invocation of makeCacheMatrix(), the one
                # in which this and the other getter and setter functions are
                # being defined.
                #
                # (In other words, we want to treat x and inv as free variables
                # as far as this function is concerned. If the <- assignment
                # operator were used instead then x and inv would be created as
                # local variables in this function, and their values would not
                # be visible in the other getter and setter functions.)
                x <<- y
                inv <<- NULL
        }

        # Retrieve the matrix value of the object.
        get <- function() {
                # Since x was not previously defined in this function it is
                # considered a free variable, and hence is looked up in and its
                # value retrieved from the environment associated with the
                # invocation of makeCacheMatrix() in which this function was
                # defined.
                x
        }

        # Cache the inverse of the object's matrix value.
        set_inverse <- function(inverse) {
                # See the above note for the set() function re use of <<-.
                inv <<- inverse
        }

        # Retrieve the cached inverse of the object's matrix value.
        get_inverse <- function() {
                # See the above note for the get() function re inv as a free
                # variable.
                inv
        }

        # Return the new object as a list of getter and setter functions
        # bound to the environment associated with this particular invocation
        # of makeCacheMatrix().
        #
        # For convenience give each of the list elements a name that matches
        # that of the underlying function. If v <- makeCacheMatrix(x) we can
        # then reference (e.g.) the set() and get() functions as v$set() and
        # v$get() respectively.
        list(set = set,
             get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## Return the inverse of a CacheMatrix object's matrix value.
cacheSolve <- function(x, ...) {
        # Return the cached inverse if one is available.
        inv <- x$get_inverse()
        if(!is.null(inv)) {
                message("Getting cached inverse")
                return(inv)
        }

        # There is no inverse cached, so we compute it from scratch. Any extra
        # arguments passed to cacheSolve() are passed on to solve().
        y <- x$get()
        inv <- solve(y, ...)

        # Cache the resulting inverse and then return it.
        x$set_inverse(inv)
        inv
}
