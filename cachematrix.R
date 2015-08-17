## The two functions will enable the caching of an invertible matrix's inverse.


## "makeCacheMatrix" creates the object that will hold the cached matrix information 

makeCacheMatrix <- function(x = matrix()) {
        
        ## initializes container for inverse matrix 
        invMat <- NULL
        
        ## function caches the *original* matrix
        setOrig <- function(y){
                
                x <<- y
                
                ## if the original matrix changes, the inverse matrix container is reset
                invMat <<- NULL
                
        }
        
        ##returns the value of the cached *original* matrix
        getOrig <- function() x
        
        ## function to set the value of the cached *inverse* matrix
        setInvMat <- function(newInv) invMat <<- newInv
        
        ## function to return the value of the cached *inverse* matrix
        getInvMat <- function() invMat
        
        ## builds list of functions to represent the cached "matrix"
        list(setOrig = setOrig, getOrig = getOrig, setInvMat = setInvMat, getInvMat = getInvMat)
}


## This function either retrieves and returns the stored inverse of a given "makeCacheMatrix" object 
## or solves for and returns the inverse of a "makeCacheMatrix" object

cacheSolve <- function(x, ...) {
        
        ## gets the cached inverse matrix
        invMat <- x$getInvMat()
        
        ## if the cached inverse exists, returns the cached inverse; the "return" stops the function 
        ## execution, so no "else" clause is necessary.
        if(!is.null(invMat)){
                
                return (invMat)
        }
        
        ## if the cached inverse *does not* exist, retrieves the original matrix. i.e. the matrix
        ## whose inverse is to be calculated
        matToInvert <-x$getOrig()
        
        ## inverts the matrix
        invMat <- solve(matToInvert)
        
        ## caches the inverse matrix
        x$setInvMat(invMat)
        
        ## returns the inverse matrix
        invMat
}
