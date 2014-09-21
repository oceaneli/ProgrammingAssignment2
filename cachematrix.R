## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        invM<-NULL
        set<-function(y){ 
                x<<-y
                invM<<-NULL
        }
        get<-function() x
        setinverse<-function(invS) invM<<- invS
        getinverse<-function() invM
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}
## This function returns the inverse of the special "matrix". The inverse is computed if not already cached.
cacheSolve <- function(x, ...) {
        
        ##if the cached inverse is not NA, then return the cached value
        invM<-x$getinverse() 
        if(!is.null(invM)){ 
                message("getting cached data") 
                return(invM)
        }
        
        ##if the cached inverse is NA, then use the solve function to calculate the inverse of 'x'
        mat<-x$get() 
        invM<-solve(mat, ...)
        x$setinverse(invM)
        
        ##Return the inverse of 'x'
        invM
}

