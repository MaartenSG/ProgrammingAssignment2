## The following two functions calculate and store (cache) the inverse of a matrix

## makeCacheMatrix creates an object that stores a matrix, can store it's inverse and 
## contains fuctions to set and extract the matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}	
	get <- function() {
		x
	}
	setinv <- function(inverse) {
		inv <<- inverse
	}
	getinv <- function() {
		inv
	}	
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## cacheSolve uses (and requires) a makeCacheMatrix object to get it's matrix inverse or 
## to calculate the inverse and store it in the object for later use.

cacheSolve <- function(x,...) {
	i <- x$getinv()
	
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	
	mat <- x$get()
	inverse <- solve(mat)
	x$setinv(inverse)
	inverse
}
