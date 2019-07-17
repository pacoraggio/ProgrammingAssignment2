## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix create a CacheMatrix that store
## the matrix values and it's inverse
## example of use:
## x1 = makeCacheMatrix(matrix(c(1,2,3,0,2,-1,-1,-1,0),3,3,byrow = TRUE))

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse)
    {
		## The following code execute somthing that
		## was not explicity requested
		## check if the user is trying to set an inverse
		## with an object with dimension that are not 
		## compatible with the created cached matrix
        if(is.null(dim(inverse)))
        {
            # the inverse of a matrix must be a matrix
            message('Cannot set inverse of a matrix with dim == NULL')
            return(inv)
        }
        if((dim(inverse)[1] != dim(x)[1]) )
        {
			# checking that the inverse matrix has the same
			# dimension as the direct one
            # We are supposing to enter only squared matrix
            # that's why I check only the first dimentsion
            message('the inverse matrix must be squared and with same dimension')
            return(inv)
        }
        inv <<- inverse
    }
    getInverse <- function() inv
    list(set=set, get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}


## Write a short comment describing this function
## cacheSolve compute the inverse of a matrix
## using solve() function. If the inverse has not been calculated yet
## the function uses solve() and store the resulting matrix
## if already computed does not call solve() and return 
## the cached inverted matrix
## example of use:
## cacheSolve(x1)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
    # if the inverse has been already computed
	# it returns the cached value
	if(!is.null(inv))
    {
        message('getting the cached Inverse matrix')
        return(inv)
    }
	# if the inverse has not been computed yet 
	# calls solve() on the cached matrix
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
