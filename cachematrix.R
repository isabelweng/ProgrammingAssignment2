## Compute and store the inverse of a matrix in a speicial matrix object.

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
i<-NULL
set<-function(y){
        x<<-y
        inverse<<-NULL
}
get<-function() x
setinverse<-function(inverse) i<<-inverse
getinverse<-function() i
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


##  This function computes the inverse of the special "matrix" returned 
##  by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
i<-x$getinverse()
if (!is.null(i)){
        message("getting cached data")
        return(i)
}
else {
data<-x$get()
if (det(data)!=0){
i<-solve(data)
x$setinverse(i)
i
}
else {message("The inverse of the matrix can not be calculated")}
}
}

## Testing files
matrix<-matrix(c(1,2,3,4),nrow=2,ncol=2)
cache<-makeCacheMatrix(matrix)
Cachesol<-cacheSolve(cache)

matrix<-matrix(rep(0,4),nrow=2,ncol=2)
cache<-makeCacheMatrix(matrix)
Cachesol<-cacheSolve(cache)

