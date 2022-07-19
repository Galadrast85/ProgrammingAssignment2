## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        invmat <- NULL
  	#here we store the inverse matrix 
	
	#then we create the matrix
	set <- function(y) {
    		x <<- y
    		invmat <<- NULL
	}

  	# Here we get the matrix
  	get <- function() x
 
 	# Then we set the inverse given the inverse 'matrinv'
  	setinver <- function(matrinv) invmat <<- matrinv

  	# Here we get the inverse, which is invmat 
  	getinver <- function() invmat 
  	list(set = set, get = get,
 		setinver = setinver,
		getinver = getinver)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmat <-x$getinver()
  
	if(!is.null(invmat)) {
    	# invmat is not null, the inverse matrix exists
    		message("Getting Cached Data")
    		return(invmat)
  	}

  	# If invmat is null, the inverse matrix does not exist
  	# So we put x into data
  	data <- x$get()

  	#Here we invert data and put into invmat 
  	invmat <- solve(data)
  	x$setinver(invmat)

  	# Then we return invmat 
 	invmat 
}
