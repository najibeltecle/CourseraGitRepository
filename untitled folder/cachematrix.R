## Put comments here that give an overall description of what your
## functions do
## These functions cache the inverse of a Matrix 

## Write a short comment describing this function
## This function creates a special matrix to 
## 1) set teh value of teh matrix 
## 2) get the value of the matric
## 3) set the value of the inverse
## 4) get the value of teh inverse 

makeCacheMatrix <- function(x = matrix()) {
	inverseSM <- NULL
	set <-function(y) {
		x <<- y
		inverseSM <<-NULL
	}
	get <- function() x
	setInverse <- function(invSM) inverseSM <<- invSM
	getInverse <- function () inverseSM
	list(set=set, get=get, setInverse = setInverse, getInverse=getInverse)

}


## Write a short comment describing this function
## This ufnciton calculates teh inverse
## It first checkes if teh inverse was previously calculated for the same matrix
## If so inverse is printed from cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseSM <- x$getInverse()
	if(!is.null(inverseSM)) {
		message("getting cached data")
		return(inverseSM)
	}
	data<- x$get()
	## InverseSM will ceck if the matrix is inversible before processing it 
	inverseSM <- if (dim(data)[1] == dim(data)[2] && det(data)!=0) {solve(data, ...)} else stop("input of this functions should be limited to square invertible matrices") 
	x$setInverse(inverseSM)
	inverseSM
}
