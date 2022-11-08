
f <- function(x){
  cos(x)-x
}

f1 <- function(x){
  -sin(x)-1
}

p0 <- pi/4
newtonmethod <-  function(f,f1, tol = 1e-5, maxiter = 100)
{
  #step1
  i <- 1
  #step2
  while(i <= maxiter)
  {
    #step3
    p <- p0 - f(p0)/f1(p0)
    
    #step4
    if(abs(p-p0) < tol)
    {
      cat(p)
    }
    
    #step5
    i <-  i+1
    
    #step6
    p0 <- p
  }
  #adım7
  return(p)
  
}
newtonmethod(f,f1)
