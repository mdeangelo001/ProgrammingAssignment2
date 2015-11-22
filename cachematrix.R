## Programming Assignment 2 is used to demonstrate the capability
## of the <<- assignment operator in R. In R if a variable is
## referenced inside a function that is not a local variable
## or a parameter of the function, it is a "free variable".
## Its value is found by searching each (lexically) enclosing
## environment until the variable is found, or until the original
## ancestor "empty environment" is reached.  The standard assignment
## operator <- will search the current environment for a variable
## with the given name and if it is not found, it will create it.
## The <<- assignment operator, however, will conduct a search
## through (lexically) enclosing environments following the same
## rules as for free variables. If the variable is not found in
## any of the lexically enclosing environments, the variable will
## be created in the global environment (the first child of the
## original ancestor empty environment).

## When function makeCacheMatrix is called, a new environment is
## created to hold the local variables declared inside the
## function. Four functions are declared inside the the environment:
## set and get which set and return the matrix of insterest, and
## setinverse and getinverse which set and return the inverse
## of the matrix of interest. The variable x is created as part of
## the local environment because it is a formal parameter to the
## function. The variable inverse is used to store the inverse
## when it is calculated. The function makeCacheMatrix returns
## a list of the four functions set, get, setinverse, and getinverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    ## Set the value of x in the enclosing environment to y
    ## and clear the value of inverse in the enclosing
    ## environment.
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }

    ## Retrieve the value of x. Since it is a free variable it is
    ## found in the enclosing environment.
    get <- function() x

    ## Set the value of inverse in the enclosing environment to i.
    setinverse <- function(i) inverse <<- i

    ## Retrieve the value of inverse from the enclosing environment.
    getinverse <- function() inverse

    ## Return a list of functions. Note that the names of the members
    ## of the list are the same as the names of the functions. This is
    ## not a requirement. The functions could have been named a,b,c,d
    ## and list(set = x, get = b, setinverse = c, getinverse = d)
    ## would behave the exact same way.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function cacheSolve is called to retrieve the inverse
## of the matrix. First the function checks and sees if the inverse
## matrix has already been calculated and cached. If so, the
## cached copy is returned. If the inverse matrix has not bee cached
## then it is caclculated, cached, and returned.

cacheSolve <- function(x, ...) {
    ## Call the function that is in the getinverse member
    ## of the list x and assign the result to i.
    i <- x$getinverse()

    ## If the result is not NULL print a message and return the result.
    if(!is.null(i)) {
        message("getting cached inverse matrix")
        return(i)
    }

    ## If the result is NULL get the original matrix data by
    ## calling the function in the get member of the list x.
    data <- x$get()

    ## Calculate the inverse matrix
    i <- solve(data)

    ## Call the function in the setinverse member of the list x to
    ## save the inverse matrix for use later on.
    x$setinverse(i)

    ## Return the inverse of the matrix.
    i
}
