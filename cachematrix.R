## The first function makes a list with functions that set and get a matrix and 
## its inverse in an intrinsic environment
## The second function takes the matrix from the first and attempts to calculate and set its inverse.  
## If the inverse is already set, the cached value is spit out, otherwise it tries to solve the inverse matrix.

## this first funciton here (makeCacheMatrix) creates a matrix x, and then sets the value 
## of the vector (set function) and gets the vector (get function) in the intrinsic envoronment


makeCacheMatrix <- function(x = matrix()) {
        cachedInv <- NULL 
        set <- function(userValue = matrix()) {
                x <<- userValue 
                cachedInv <<- NULL
        }
        get <- function() x
        setInverse <- function(invVal) {
                cachedInv <<- invVal 
                return(cachedInv)
        }
        getInverse  <- function() cachedInv
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## given the list variable from the cached matrix function above, the following 
## function checks to see if a cached inverse already exhists and then spits it out
## if it doesn't already exhist, then it  attempts to solve its and spit it out
## it also gives an error message or warning message if there is something wrong using tryCatch()

cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...) {     
        calculatedInverse <- x$getInverse()     
        if(!is.null(calculatedInverse) && is.matrix(calculatedInverse)) { 
                message("found some cached data...")
                return(calculatedInverse)
        }   
        matrixToSolve <- x$get()  
        calculatedInverse <- tryCatch({ 
                solve(matrixToSolve)
        }, warning=function(w) {
                message("something was strange, and the result may not be what you want")
                message(w)
        }, error=function(e) {
                message("something went wrong and the matrix couldn't be solved")
                message(e)
                message("\n")
        })
        message("set the inverse matrix to...") 
        x$setInverse(calculatedInverse)
}

