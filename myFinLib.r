#Option pricing as all
OptionPrice <- function(spot, mtrty, prd, sigma, strike, r, dvd=0, call=TRUE, US=FALSE) {
  
  q <- Qcalc(r, dvd, mtrty, prd, sigma)
  u <- Ucalc(sigma, mtrty, prd)
  
  if (!is.matrix(spot))
    {pTree <- bio.tree(spot, prd, u)}
  else
    {pTree <- spot}
  
  OptionTree <- OptTree(pTree, strike, sigma, mtrty, r, prd, dvd, call=call, US=US)
  
  result <- list(q=q, PriceTree=pTree, Payoff=OptionTree, Option.Price=OptionTree[1,1])
}

#Future Pricing
FuturePrice <- function(spot, maturity, prd, sigma, r, dvd=0){
  q <- Qcalc(r, dvd, maturity, prd, sigma)
  u <- Ucalc(sigma, maturity, prd)
  
  pTree <- bio.tree(spot, prd, u)
  
  #future don't discount by interest rate
  price <- discount(pTree, q, r=1, maturity)
  
  future <- list(q=q, price.tree = price )
}

#Generate Binomial Tree lattice. Can be used for price/short rate generation. 
bio.tree.simple <- function(start, prd, u){

  b.tree <- matrix(0, prd+1, prd+1)
  b.tree[1,1] <- start
  
  for (col in 1:(prd+1)){
    b.tree[1,col] <- start * u^(col-1)
  }
  
  for (row in 2:(prd+1)){
    b.tree[row,row:(prd+1)] <- b.tree[row-1,(row-1):prd]/u
  }
  
  return(b.tree)
}

bio.tree <- function(start, prd, u){
  
  if (!is.matrix(start)){
    dim(start) <- c(1,1)
  }
  
  r.start <- dim(start)[1]
  c.start <- dim(start)[2]
  
  b.tree <- rbind(start, matrix(0, prd, c.start))
  b.tree <- cbind(b.tree, matrix(0, prd + r.start, prd))
  
  for (col in (c.start+1):(prd+c.start)){
    b.tree[1,col] <- b.tree[1,col-1] * u
  }
  
  for (row in 2:(prd+r.start)){
    b.tree[row,(c.start + 1):(c.start + prd)] <- b.tree[row-1,c.start:(c.start + prd - 1)]/u
  }
  
  return(b.tree)
}

#Calc with Strike price/Payoff Matrix

OptTree <- function(pTree, strike, sigma, mtrty, r, prd, dvd = 0, call=TRUE, US=FALSE){ 
  
  r.real <-  exp(r*mtrty/prd)
  
  q <- Qcalc(r, dvd, mtrty, prd, sigma)
  

  if (FALSE) {
    if (call)
      {payoff <- pmax(pTree - strike, 0)}
    else 
      #{payoff <- pmax(pmin(- pTree + strike, pTree), 0)}
      {payoff <- pmax(- pTree + strike, 0)}
    
    for (col in prd:1){
      for (row in prd:1){
        if (row <= col) {
          if (US == TRUE) {
            payoff[row,col] <- max(payoff[row,col], (q*payoff[row,col+1] + (1 - q)*payoff[row+1, col+1]))/r.real
                   }
          else {
            payoff[row,col] <- (q*payoff[row,col+1] + (1 - q)*payoff[row+1, col+1])/r.real
          }
        }
      }
    }
  }
    #payoff
  

#if (call) {payoff <- pmax(pTree - strike, 0)} else {payoff <- pmax(pmin(- pTree + strike, pTree), 0)}
if (call) 
  {payoff <- pmax(pTree - strike, 0)} 
else {
    payoff <- pmax(- pTree + strike, 0)
    payoff[payoff == strike] <- 0
}
  
payoff <- discount(payoff, q, r.real, mtrty, US)

payoff

}  
#discount
discount <- function(tree, q, r, maturity, early=FALSE) {
  r.real <- r
  #r.real <-  exp(r*maturity/prd)
  #discounted <- matrix(0, prd+1, prd+1)
  
  t.col <- dim(tree)[1]
  t.row <- dim(tree)[2]
  
  if(!is.matrix(r.real)) {
    r.real <- matrix(r.real, t.row, t.col)
  }
  
  for (col in (t.col-1):1){
    for (row in (t.row-1):1){
      if (row <= col) {
        if (early) {
          tree[row,col] <- max(tree[row,col], (q*tree[row,col+1] + (1 - q)*tree[row+1, col+1])/r.real[row, col])
        }
        else {
          tree[row,col] <- (q*tree[row,col+1] + (1 - q)*tree[row+1, col+1])/r.real[row,col]
        }
      }
    }
  }
  tree
}

#Calc Up factor
Ucalc <- function(sigma, mtrty, prd){
  u <- exp(sigma*sqrt(mtrty/prd))
  return(u)
}

#Calc q factor
Qcalc <- function(r, dvd, mtrty, prd, sigma){
  u <- exp(sigma*sqrt(mtrty/prd))
  r.real <-  exp((r - dvd)*mtrty/prd)
  q <- (r.real - 1/u)/(u-1/u)
  return(q)
}