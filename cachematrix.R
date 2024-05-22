# matrix inversion caching

# makes cache matrix containing a list of get, set, setinverse, getinverse functions
makeCacheMatrix <- function(x = matrix(numeric())){
  # initializes i to NULL
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
    #sets local variables
  }
  get <- function() x
  #returns local variable
  setinverse <- function(inverse) {i <<- inverse}
  #sets from free variable
  getinverse <- function() {i}
  #retrieves updated i
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
  
}

# solves, and used cached data if calculated previously
cacheSolve <- function(x, ...){
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached matrix inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

# tests on a sample matrix to make sure that both 
# 1. correct inverse is showing
# 2. proper caching is done

tester <- function(){
  invertible_matrix <- matrix(c(4, 2, 1, 
                                  3, 5, 7, 
                                  2, 8, 6), nrow = 3, byrow = TRUE)
  cache_matrix <- makeCacheMatrix(invertible_matrix)
  inverse_matrix <- cacheSolve(cache_matrix)
  truth1 <- identical(inverse_matrix %*% invertible_matrix, diag(3))
  print(paste("creates correct inverse: ", (truth1)))
  print(inverse_matrix)
  inverse_matrix_cached <- cacheSolve(cache_matrix)
  print("print statement indicates proper caching")
}
