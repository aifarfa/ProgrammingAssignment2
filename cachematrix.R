## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	if(nrow(x) != ncol(x)){
		print('[WARNING!] this matrix is not invertible')
	}
	inverse <- NULL

	get <- function(){
		return(x)
	}
	set <- function(value){
		x <<- value
		inverse <<- NULL
	}
	getInverse <- function(){
		return(inverse)
	}
	setInverse <- function(value){
		inverse <<- value
	}
	list(set = set, get = get,
		getInverse = getInverse, 
		setInverse = setInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	result <- x$getInverse()
	if(!is.null(result)){
		print('resolve from cached')
		return(result)
	}
	data <- x$get()
	result <- solve(data) ## %*% data
	x$setInverse(result)

	## Return a matrix that is the inverse of 'x'
	return(result)
}


