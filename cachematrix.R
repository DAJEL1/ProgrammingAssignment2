
## Caching the Inverse of a Matrix
## functions do

## This function creates a special "matrix" object and cache the inverse.

   makeCacheMatrix <- function(x = matrix()) {
  
         dtm <- det(x)
  
  if ((nrow(x)!=ncol(x))|(dtm==0)){
    
          print("The given matrix is not invertible")
    
  }  
    
 else
   
     { 
   
    inv <- NULL
    
    set <- function(y){
      
      x <<- y 
      
      inv <- NULL     }
    
    get <- function() x
    
    setinv <- function(inverse) inv <<- inverse
    
    getinv <- function() inv
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
     }
  
  }
   
   ## This function computes the inverse of the special "matrix" returned by 
   ##  makeCacheMatrix above. 
  
   ## if the inverse has already been calculated(and the matrix has not changed)
   ## ,then the cachesolve retrieve the inverse from the cache.

  cacheSolve <- function(x, ...) {
    
         inv <- x$getinv()
         
         if(!is.null(inv)){
           
            print(message("getting cached data"))
           
            return(inv)
           
         }
         
         data <- x$get()
         
          inv <- solve(data, ...) 
           
           x$setinv(inv)
           
           inv
           
   }


