## The function creates a list containing the 4 sub functions:
## set, get, setInv and getINv. These functions will return the inverse of a matrix. 
## If the inverse of the matrix has been previously calculated, it will be cached 
## and we can extract the cached result instead of calculate it again. 
 
## makeCacheMatrix offers a list of functions that will be used in cacheSolve. 
## The original matrix and its inverse can be saved in the list. 

makeCacheMatrix <- function(x = matrix()) {
	xinv <- NULL ## store of result
	set <- function(y) {  ##creates a function to set a matrix
		x <<- y
		xinv <<- NULL ## xinv is initially equal to null
	}

	get <- function() x ## return input matrix
	setInv <- function(inv) xinv <<- inv ## set inverse matrix
	getInv <- function() xinv ## return inverse matrix
	list(set = set, get = get, ## creates a list with the above functions
		setInv = setInv,
		getInv = getInv)
}


## cacheSolve will cache the inverse of the matrix

cacheSolve <- function(x, ...) {
	m <- x$getInv() ## get inverse matrix from x
	if(!is.null(m)) { ## if inverse is possible
		message("getting cached data")
		return(m) ## return inversion
	}
	data <- x$get() ## if not, x$get is used to get the matrix object
	m <- solve(data) ## solve
	x$setInv(m) ## set to object
	m ## return solve
}

## Test

  test <- matrix(runif(9,1,100),3,3) ## test matrix
  testCached <- makeCacheMatrix(test)
  testInv <- cacheSolve(testCached)