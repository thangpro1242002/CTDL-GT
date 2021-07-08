n=5
giaithua=function(n){
  if (n==0){
    return(1)
  }
  else{
    return(n * giaithua(n - 1))
  }
}
giaithua(n)