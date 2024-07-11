library(mvtnorm)
data <- read.csv("assign4_train.csv")

num_rows <- nrow(data)
df <- data[sample(num_rows),]

X <- as.matrix(df[-c(1)])
XX <- as.matrix(df[-c(1)])
X <- X[, -1]
y<- as.matrix(df[1])

X_train <- as.matrix(X[1:480,])
y_train <- as.matrix(y[1:480,])
X_test<-as.matrix(X[481:500,])
y_test <- as.matrix(y[481:500,])



self.pred.y <- function(beta, x){
  x <- as.matrix(x)
  return(x%*%beta)
}

p <- dim(X)[2]

model_fitting <- function(X, y){
  num_rows <- nrow(X)
  
  optim_lam <- 0
  lam.vec <- c(10^(seq(-3, 3, by = 0.1)))
  max_error <- 1000000000
  
  
  #LOOCV
  cnt <- 0
  for(lam in lam.vec){
    curr_error <- 0
    
    
    for(i in 1:num_rows){
      
      X_iter <- X[-i, ]
      y_iter <- y[-i,]
      X_test <- X[i,]
      y_test <- y[i,]
      
      beta <- solve(t(X_iter) %*% X_iter+ diag(lam, p)) %*% t(X_iter) %*% y_iter
      preds <- X_test %*% beta
      curr_error <- curr_error + ((y_test-preds)**2)
    }
    
    if(curr_error < max_error){
      max_error = curr_error
      optim_lam <- lam
      
    }
    cnt <- cnt + 1
    print(cnt)
  }
  return(optim_lam)
}

final_lam <- model_fitting(X_train, y_train)

beta <- solve((t(X_train) %*% X_train) + diag(final_lam, p)) %*% t(X_train) %*% y_train

print(mean((self.pred.y(beta, X_test) - y_test)**2))

save(beta, file = "fit_params.Rdata")

