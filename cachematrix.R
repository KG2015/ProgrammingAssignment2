## These functions will cache the inverse of a matrix so that this
## doesn't need to be repeatly computed. 

## The first function creates a matrix object and inverts it.

makeCacheMatrix <- function(x = matrix()) {
                        m<-NULL
                        set<-function(y) {
                                x<<-y
                                m<<-NULL
                        } ## Sets value of the matrix
                        
                        get<-function() x ## Retrieves the value of the matrix
                        setmatrix<-function(solve) m <<- solve ## Inverts the matrix
                        getmatrix<-function() m ## Retrieves the value of the inverted matrix
                        list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
                        
}


## The second function looks to see if the matrix has already been inverted.
## If this has already been computed, this is retrieved from the cache. If not, the matrix is inverted and value returned.

cacheSolve <- function(x, ...) {
                m<-x$getmatrix()
                if(!is.null(m)){
                        message("getting cached data")
                        return(m) ## 'If' function looks to see if matrix exists already; if does it returns 
                                  ##it after telling you it's getting cached data
                }
                matrix<-x$get() ## Gets data from cached matrix
                m<-solve(matrix,...) ## Calculates the inverse of the matrix if it wasn't already there
                x$setmatrix(m) ## Sets value of inverse matrix in the cache
                m ## returns the value of the inverse matrix
}
