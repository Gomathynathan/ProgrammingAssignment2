## The functions makeCacheMAtrix and cacheSolve are used to provide the inverse of the matrix using a cache,if available.
##This will avoid calculating the inverse of the same matrix repeatedly.


##The makeCaheMatrix function returns a list of functions that provides access to the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
inverse<-NULL
setValue<-function(y){
    x<<- y
    inverse<<-NULL
}
getValue<-function() x
setInv<-function(y) inverse<<-y
getInv<-function() inverse

list(set=setValue,get=getValue,setInverse=setInv,getInverse=getInv)
}


##The cacheSolve Function calculates the inverse of a matrix if it's not found in the cache.If it's in the
##cache ,it returns it from the cache, without calculating the inverse each time.

cacheSolve <- function(x, ...) {
    z<<-x$getInverse()
    if(is.null(z)){
        val<-x$get()
        inverse<-solve(val)
        x$setInverse(inverse)
        return(inverse)
    }
    else{
        print("getting cached data")
        return(z)
    }
    
}
