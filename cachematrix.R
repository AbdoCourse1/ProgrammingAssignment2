# abdo55
# This function is able to cache potentially time-consuming computations.
# In smoe cases where the data set is too large repeat performaing the calcualtions
# consume the time, especially in operations that require repeate calculation.
# If the operation is done before and no change in the data, we can resue the 
# privious result without repeating the process of calculation.


## Cashing the inverse of a Matrix
# when peroform matrix calculations, Matrix inverse is usually take much time
# to overcome time consuming this function will cash the inverse of matrix
# to allow user using it without recalculate it each time he need.
# This will done by writting 2 functions as follow.

# This function create a modified object of matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) i <<- inverse
        
        getinverse <- function() i
        
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse
             
        )
        
}


## THis function compute the inverse of the modified matrix returned by
# the privious makeCashMatrix function.
# If the inverse of the matrix has already been calculated and the matrix has not changed,
# the cacheSolve should retrive the inverse from the cashe.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cashed data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
