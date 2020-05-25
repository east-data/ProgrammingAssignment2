## Put comments here that give an overall description of what your
## functions do

## Reset Enviro - rm(list=ls())
        
## The first function, `makeCacheMatrix` creates a special "matrix", 
## that can cache its inverse

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Create 2 invertible matrix caches for testing
aMatrix <- matrix(c(-1,1,3/2,-1), 2, 2)
bMatrix <- matrix(c(1,0,-2,3,1,-2,-5,-1,9), 3, 3)


## Cache 2 the matrices caches for testing
aMatrixCache <- makeCacheMatrix(aMatrix)
bMatrixCache <- makeCacheMatrix(bMatrix)

## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and sets the value of the inverse in the cache via the `setmean`
## function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## Run 2 Matrices
cacheSolve(aMatrixCache)
cacheSolve(bMatrixCache)


## Test Ouput Values
solve(aMatrix) == cacheSolve(aMatrixCache)
solve(bMatrix) == cacheSolve(bMatrixCache)