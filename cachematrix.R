#### Purposeof the function pair " makeCacheMatrix / cacheSolve " is to improve the performance of calculation of Inverse of a Matrix  by means of CACHEING the calculated value.

## The function makeCacheMatrix will GET the MATRIX data and computes the INVERSE of it and stores in variable m in environment X
makeCacheMatrix <- function(X = matrix()) {
        m <- matrix(data = NA, nrow = nrow(X), ncol = ncol(X), byrow = FALSE, dimnames = NULL)
        set <- function(y) {
                X <<- y
                m <<- NULL
        }
        get <- function() X
        setInverse <- function(invMat) 
        {
            
            m <<- invMat       
        }
        getInverse <- function() {
         
           m         
       }
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The function cacheSolve is responsible for serving the Cached Data if the INVERSE is computed already for the first time. 
## If not computed already, will compute the Inverse of the Matrix data and caches the Inversed data
cacheSolve <- function(X, ...) {
        m <- X$getInverse()
        if( !(is.na(m) ) ) {
                message("Getting cached Inversed Matrix Data")
                return(m)
        }
         message("Computing for first time and caching Inversed Matrix Data")
         data <- X$get()
         m <- solve(data)
         X$setInverse(m)
         return(m)
}


## BELOW is the testing script
##
## B = matrix(c(2, 4, 3, 1), 
##          nrow=2, 
##          ncol=2) 

##mat2 <- makeCacheMatrix(B)

##dd <- cacheSolve(mat2)	
##print(dd)

##B = matrix(c(5, 5, 7, 8), 
##          nrow=2, 
##          ncol=2) 
##mat2 <- makeCacheMatrix(B)

##dd <- cacheSolve(mat2)	
##print(dd)

## END BELOW is the testing script

