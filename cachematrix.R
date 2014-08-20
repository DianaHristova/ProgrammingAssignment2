## The functions can compute the inverse of a matrix,cache the inverse and if asked to compute again,
## call it up from the cache, rather than recomputing. 

## This function creates a 'matrix' object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
	set<-function(y){
	       	x<<-y
	       	m<<-NULL
	}
	get<-function()x
	setinverse<-function(solve)m<<-solve
	getinverse<-function()m
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function computes the inverse of the special 'matrix' object created above and sets the value of the inverse 
## in the cache, after first checking to see if it has already been calculated, in which case it pulls it from the cache. 

cacheSolve <- function(x, ...) {
        m<-x$getinverse()
	if(!is.null(m)){
	       	message("getting cached data")
	       	return(m)
	}
	data<-x$get()
	m<-solve(data,...)
	x$setinverse(m)
	m
}
