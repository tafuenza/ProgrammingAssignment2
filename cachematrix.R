## Put comments here that give an overall description of what your
## functions do


makeCacheMatrix <- function(x = matrix()) {
        ## Declare inverse of matrix NULL
        imatrix <- NULL
        set <- function(y){
                x <<- y
                imatrix <<- NULL
        }
        ## Get the matrix
        get <- function() x
        ## Solve and assign the inverse of the matrix by solve
        setimatrix <- function(solve) imatrix <<- solve
        ## Get the inverse of the matrix
        getimatrix <- function() imatrix
        ## Create a list of functions
        list(set = set, get = get,
             setimatrix = setimatrix,
             getimatrix = getimatrix)
        
}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Get the inverse of the matrix
        m <- x$getimatrix()
        ## If the inverse of the matrix exist
        if(!is.null(m)){
                message("getting cached data")
                return(m)   
        }
        # If the inverse of matrix doesnt exist, calculate them and return
        data <- x$get()
        m <- solve(data,...)
        x$setimatrix(m)
        return(m)
}

x <- rbind(c(1,2),c(2,1))
y <- makeCacheMatrix(x)
cacheSolve(y)
