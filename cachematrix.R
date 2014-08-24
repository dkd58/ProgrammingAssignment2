## The R function solve() calculates the inverse of an invertible matrix
## This may be a timeconsuming process if the matrix is large; therefore
## we would like to prevent calculating the inverse more than once
## In order to accomplish this we create a new function, cacheSolve, that
## simply retrieves the inverse of the matrix if it has already been 
## calculated; only in case it has not been calculated (and stored) yet,
## cacheSolve calls solve() to get the inverse.

## To make this work, we need another function, makeCacheMatrix, that 
## takes an existing invertible matrix x and creates a list object 
## containing 4 functions. 
## These 4 functions can be called (e.g. from another function such as 
## cacheSolve below) in order to retrieve the value of the inverse of x
## (if it has already been determined before) using 'getinv' OR calculate 
## it on the spot (if it has not) and store it for later use using 'get', 
## solve() and 'setinv'. 
## The 4th function, 'set' is not used in this example; it could be used 
## to supply another matrix to the list object created by makeCacheMatrix

## So we start with makeCacheMatrix; the result of this function is the
## list object with the functions set, get, setinv and getinv
## - 'set' can be used to supply a new matrix to the object
## - 'get' simply gets the matrix itself
## - 'setinv' stores the inverse of the matrix in the object
## - 'getinv' retrieves the inverse (calculated in an earlier call)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(sinv) inv <<- sinv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Now the function cacheSolve, which works on the list object created
## by makeCacheMatrix(x) - so not on x itself(!) - is defined.
## First, it is checked whether the inverse of x has already been calculated
## and is stored in the object; if so the stored inverse is retrieved and the
## functions ends.
## If the inverse has not been calculated yet (the value of mat$getinv is NULL)
## the following functions are called:
## - mat$get to get the matrix x itself
## - solve() to calculate the inverse of x
## - mat$setinv to store the inverse for later retrieval

cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'mat'
    iv <- mat$getinv()
    if(!is.null(iv)) {
        message("getting cached data")
        return(iv)
    }
    data <- mat$get()
    iv <- solve(data, ...)
    mat$setinv(iv)
    iv
} 

## An example of using both functions follows below

## First create an invertible matrix, in this case the correlation
## matrix (which should be invertible per definition) of the longley
## dataset.

cm <- cor(longley)

## show that it can be inverted

solve(cm)

## Now create the list object that can store the inverse once it 
## is calculated

ccm <- makeCacheMatrix(cm)

## Call cacheSolve for the first time; the inverse will be calculated

cacheSolve(ccm)

## Now call cacheSolve again; the stored inverse is retrieved instead of
## calculating the inverse again

cacheSolve(ccm)


