## The functions below create an interface that allows to cache a matrix and its inverse

## makeCacheMatrix function creates an "matrix" object that can cache the original matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
        ##reset inverse
	cachedInverse  <- NULL 

	setMatrix <- function(y) {
		##reset the inverse to enforce re-calculation when matrix changes
		if (!identical(x,y) ) 	cachedInverse <<- NULL
		##save new mtrix
		x <<- y		
	}
	getMatrix <- function() { x }
	setInverse <- function(inverse) { cachedInverse <<- inverse 	}
	getInverse <- function() { cachedInverse  }

	##save all functions in a list
	list (setMatrix = setMatrix, getMatrix = getMatrix ,setInverse=setInverse,
		getInverse=getInverse)
}


## cacheSolve function computes the matrix inverse if it hasn't been created already and returns it. 
## Otherwise it returns the cached inverse 
## The function takes makeCacheMatrix function and returns the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        myInverse <- x$getInverse()
	if (!is.null(myInverse) )  {
		message("getting cached data")
		return(myInverse)
	}

	message("calculating and caching inverse")
	myMatrix <- x$getMatrix()
	myInverse <- solve(myMatrix)
	x$setInverse(myInverse)
	myInverse
}
