## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                              #inverse matrix initiated to zero 
        set <- function (y){
                x <<- y                          #substitutes the vector x with y (the input) in the main function
                inv <<- NULL                     #restore to NULL the value of inverse matrix inv  
             
        }
        get <- function() x                      #returns x stored in the makeCacheMatrix function
        setinv <- function (solve) inv <<- solve #sets the value of input (i.e.solve) to inv
        getinv <- function () inv                #returns inverse matrix 
        list (set=set,get=get, 
              setinv = setinv, 
              getinv = getinv)

}

## This function computes the inverse of the matrix returned by makeCacheMatrix above
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        inv <-x$getinv ()
        if (!is.null(inv)){
                message ("getting cached data")
                return (inv)
        }
        data <- x$get()                         #data gets the matrix stored with makeCacheMatrix 
        inv <- solve(data,...)                  #inv calculates the inverse of the matrix                          
        x$setinv (inv)                          #x$setinv(inv) stores it in the object generated assigned with makeCacheMatrix
        inv
        
}
