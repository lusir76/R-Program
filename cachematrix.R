## Matrix inversion is usually a costly computation.
## There may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## So I write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	flage_s <<- FALSE
	if(nrow(x) != ncol(x) || det(x) == 0 || qr(x)$rank < nrow(x)){
	    ## Estimate whether the matrix supplied is invertible.
		message("The matrix supplied isn't invertible!")
		message("Please Rerun the makeCacheMatrix function ...\n")
		flage_s <<- TRUE
	}else s <- NULL
	set <- function(y = matrix()) {
		if(nrow(y) != ncol(y) || det(y) == 0 || qr(y)$rank < nrow(y)){
		    ## Estimate whether the matrix supplied is invertible.
			message("The matrix supplied isn't invertible!")
			message("Undo the makeCacheMatrix$set function ...\n")
		}else{
			flage_s <<- FALSE
			if(nrow(x) == nrow(y) && ncol(x) == ncol(y)){
				l <- c(apply(x, 1, sum) == apply(y, 1, sum),apply(x, 2, sum) == apply(y, 2, sum))
				mark <- L(l)  ## Call the function of "L"
			}else mark <- FALSE
			if(!mark){
				x <<- y
				s <<- NULL
				message("Matrix is changed!")
			}else s <<- getinverse()
		}
	}
	get <- function() x
	setinverse <- function(inverse) s <<- inverse
	getinverse <- function() s
	getflage_s <- function() flage_s
 	list(set = set, get = get,
 	     setinverse = setinverse,
	     getinverse = getinverse,
	     getflage_s = getflage_s)	
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	flage_s <- x$getflage_s()
	if(flage_s){
	    ## the special "matrix" returned by makeCacheMatrix above is wrong.
		message("The matrix supplied in makeCacheMatrix function isn't invertible!")
		message("Please Rerun the makeCacheMatrix function.")
		return() ## Return NULL
	}else{
		s <- x$getinverse()
		if(!is.null(s)){
			message("getting cached data")
      			return(s)
		}
		data <- x$get()		
		s <- solve(data, ...)
		x$setinverse(s)
		return(s)
	}
}

## L:This function is used to estimate whether exist FALSE in a "logic" vector.

L <- function(l = vector()){
	k <- TRUE
	len <- length(l)
	for(i in c(1:len)){
		if(!isTRUE(l[i])){
			k <- FALSE
			break
		}
	}
	k
}
