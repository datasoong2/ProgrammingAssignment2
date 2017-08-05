## The following functions help us compute the inverse of a square matrix
## (if it hasn't been already computed) and store it in cache for future usage
## or return the inverse of the samesquare matrix 
## (if it has been already computed) by retriving it from cache.



#The function makeCacheMatrix creates a special "matrix", 
#which is a list containing four functions that
#1.set the value of the "matrix"
#2.get the value of the "matrix"
#3.set the value of the inverse of the "matrix"
#4.get the value of the inverse of the "matrix"
#The following code is mainly based on the code of the makeVector function


makeCacheMatrix <- function(x = matrix()) {
  # takes in as argument the matrix x     

  
  inverseOf_x <- NULL                             
  #inverseOf_x is created to store the inverse of x. 
  #Initial value of inverseOf_x is NULL

    
  set <- function(new_x) {                    
    x <<- new_x                            
    inverseOf_x <<- NULL                        
  }
  # The set function accepts as an argument the new_x value
  # which is the new matrix (that we want to set).
  # It assigns the new_x value to the x variable in the
  # parent environment of the makeCacheMatrix function.
  # It also assigns the NULL value to the inverseOf_x variable
  # in the parent environment of the makeCacheMatrix function.
  
  
  get <- function() x                     
  # The get function has no arguments. 
  # It returns the value of the x variable,
  # which, since it's not locally defined, refers
  # to the x variable of the parent environment 
  # of the makeCacheMatrix function.
  
  
  setinverse <- function(inverse) inverseOf_x <<- inverse  
  # The setinverse function receives the argument  
  # inverse, which represents the inverse of a matrix. 
  # It assigns the value inverse to the variable 
  # inverseOf_x, which, since the superassignment operator 
  # is used, refers to the inverseOf_x variable of the parent 
  # environment of the makeCacheMatrix function.
  
  
  getinverse <- function() inverseOf_x                     
  # The getinverse function has no arguments. 
  # It returns the value of the inverseOf_x variable,
  # which, since it's not locally defined, refers to 
  # the inverseOf_x variable of the parent environment 
  # of the makeCacheMatrix function.
  
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer 
  # The makeCacheMatrix function returns a list. 
  # This list contains four functions that
  # 1.set the value of the x matrix
  # 2.get the value of the x matrix
  # 3.set the value of the inverse of the x matrix
  # 4.get the value of the inverse of the x matrix
}





# This function computes the inverse of the special "matrix" returned by 
# function makeCacheMatrix. If the inverse has already  been calculated
#  (and the matrix has not changed), then cachesolve retrieves the inverse
#  from the cache. The code of this function is mainly based
#  on the code of the function cachemean.

cacheSolve <- function(x, ...) {
  # cacheSolve accepts the x argument, which is is not a matrix,
  # but a list of 4 functions, returned by the makeCacheMatrix() function.
  
  
  inverseOf_x <- x$getinverse()
  # x$getinverse(), calls the getinverse() function, within x list.   
  # The returned value (which is the inverse of the matrix argument of the  
  # makeCacheMatrix function) is assigned to the inverseOf_x variable
  # , which is also a matrix.     


  # The if code below ensures that if the inverse (inverseOf_x) of the matrix, 
  # has already been calculated (and subsequently the (!is.null(inverseOf_x))
  # condition is TRUE), then the string "retrieving data from cache" 
  # is printed on the screen. 
  if(!is.null(inverseOf_x)) {
    print("retrieving data from cache")
  }  else {
    data <- x$get()
    
    inverseOf_x <- solve(data, ...)
    x$setinverse(inverseOf_x)
  }
  # The else code above ensures that if the inverse (inverseOf_x) of the 
  # matrix hasn't been calculated (and subsequently the (!is.null(iinverseOf_x))
  # condition is FALSE), then via the return of x$get(), we get the 
  # matrix (whose inverse we want to calculate). And this matrix is assigned 
  # to the data variable (which is also a matrix). Then we get the inverse 
  # of this matrix (via solve(data, ...)) and we assign it to the inverseOf_x
  # (which is also a matrix). Then we pass inverseOf_x to the setinverse 
  # function of the x list, so that it can be stored in the cache, 
  # so that in the future we' re going to get the inverse from the cache and 
  # not repeat its computation.
  
  
  inverseOf_x
  #Finally  the inverseOf_x variable is returned
}
