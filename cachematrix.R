

## The function "makeCacheMatrix" is a list containing functions 

makeCacheMatrix <- function(x = numeric()) {
	inverse <- NULL
	##	Set the value of a matrix
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	##	Get the value of a matrix from cache
	get <- function() x
	##	Set the value of inverse of a matrix using 'solve' function
	setinverse <- function(solve) inverse <<- solve
	##	Get the value of the inverse of a matrix from cache
	getinverse <- function() inverse
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse) 

}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
      
	inverse <- x$getinverse()
	## If inverse exists in cache, get it
	if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
      }
	## If inverse is null, calculates inverse and sets it in cache
	data <- x$get()
      inverse <- solve(data, ...)
      x$setinverse(inverse)
      inverse
}
