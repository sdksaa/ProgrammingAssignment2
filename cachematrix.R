## The function creates a list containing the 4 sub functions:
## set, get, setInv and getINv.


## Write a short comment describing this function

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


## Write a short comment describing this function

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