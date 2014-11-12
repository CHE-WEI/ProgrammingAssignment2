## makeCacheMatrix is used to store functions 
## related to the data and the result.
    ## It define 4 sub-function including 
       ## input data matrix, 
       ## return inputted matrix, 
       ## input the result which is calculated from cacheSolve , 
       ## and return the inputted result .

## cacheSolve is used to store functions
## related to calculate the results and check the cache of result




## Created 4 subfunction. They are used in the way of cache  
makeCacheMatrix <- function(x1 = numeric()) {
        m1 <- NULL
		
        set <- function(y) {             ## Alternative way to input data
                x1 <<- y
                m1 <<- NULL              ## New data in, reset m1
        }
		
        get <- function() {
			return(x1)
		}
		
        setinverse <- function(inverse) { ## Input value for m1  
			m1 <<- inverse
		}
		
        getinverse <- function() {        ## Print out m1 
			return(m1)
		}
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## Calculate & Return a matrix that is the inverse of 'x' 
cacheSolve <- function(x2, ...) {
        m2 <- x2$getinverse()        ## Retrive m1 from makeCacheMatrix 
                                     ## (renamed as m2 here)
        
		if(!is.null(m2)) {       ## If m1 in makeCacheMatrix isn't null 
                message("getting cached data")  ## Print out the value of m1 
                return(m2)
        }
        
		data <- x2$get()         ## Import data  
		m2 <- solve(data, ...)   ## Do the calculation 
		x2$setinverse(m2)        ## Pass the value to makeCacheMatrix 
        
		return(m2)               ## Print the Result
}
