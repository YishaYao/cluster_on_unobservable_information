#!/usr/bin/env Rscript
#SBATCH --mem-per-cpu=1g  --time=2:00:00 --mail-type=ALL --mail-user=yisha.yao@yale.edu

#setting 1
n <- 1500;  d <- 3500;  q <- 700;  p <- 200;  del <- 0.1714
alpha <- 20; opt_lam <- 2800; lam_0 <- 4000;  lam_thred <- 1000 # tuning lam_0, lam_thred, alpha=20

library(plyr)
library(MASS)
library(Matrix)
library(plus)
library(mclust)
library(grplasso)
rand_vect <- function(N, M, sd = 1, pos.only = TRUE) {
  vec <- rnorm(N, M/N, sd)
  if (abs(sum(vec)) < 0.01) vec <- vec + 1
  vec <- round(vec / sum(vec) * M)
  deviation <- M - sum(vec)
  for (. in seq_len(abs(deviation))) {
    vec[i] <- vec[i <- sample(N, 1)] + sign(deviation)
  }
  if (pos.only) while (any(vec < 0)) {
    negs <- vec < 0
    pos  <- vec > 0
    vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
    vec[pos][i]  <- vec[pos ][i <- sample(sum(pos ), 1)] - 1
  }
  vec
}

set.seed(18)
E <- mvrnorm(n, mu=rep(0, p), Sigma=diag(p))
X <- mvrnorm(n, mu=rep(0, d), Sigma=diag(d))

group_sizes <- rand_vect(q, d)
ind_low <- rep(0, q);  ind_upp <- rep(0, q);  count <- 0
for (i in 1:q){
  ind_low[i] <- count + 1
  ind_upp[i] <- count+ group_sizes[i]
  count <- count + group_sizes[i]
}
grouping <- rep(1:q, group_sizes)
rm(count, i)
Beta <- matrix(0, d, p)
Beta_cluster_num <- rep(0, q)
Beta_labels <- matrix(0, q, p)
true_means <- matrix(0, d, p)
for (i in 1:q){
  r <- rbinom(1,1,del) 
  if (r==0){} else{
    x <- sample(2:6, 1)
    Beta_cluster_num[i] <- x  #the number of clusters in the ith row-block of B
    gaussian_mix_size <- sample(c(ceiling(p/2), ceiling(2*p/3)), 1)
    cluster_sizes <- rand_vect(x, gaussian_mix_size) #the number of elements in each cluster
    label_temp <- as.matrix(rep(0:x, c((p-gaussian_mix_size),cluster_sizes)))  
    Beta_temp <- matrix(0, group_sizes[i], (p-gaussian_mix_size))
    v <- runif(x, 0.5, 1.5)
    means <- matrix(0, group_sizes[i], x)
    for (k in 1:x){
      #means[,k] <- mvrnorm(n = 1, rep(0, group_sizes[i]), 6*diag(group_sizes[i]))
      means[,k] <- runif(group_sizes[i], -3, 3)
    }
    for (j in 1:x){
      multigaussian <- mvrnorm(n = cluster_sizes[j], means[,j], v[j]*diag(group_sizes[i]))
      Beta_temp <- cbind(Beta_temp, t(multigaussian))
    }
    index <- sample(nrow(label_temp))
    Beta_labels[i,] <- label_temp[index]
    Beta[ind_low[i]:ind_upp[i],] <- Beta_temp[,index]
    for (l in 1:p){
      if (Beta_labels[i,l] !=0){true_means[ind_low[i]:ind_upp[i],l] <- means[,Beta_labels[i,l]]}
    }
  }
}
# Beta_labels records the true classifications
Y <- X %*% Beta + E

Betas <- list()  #Betas[[t]] <- matrix(0, n_pc_d, p)
Blabels <- list()  #Blabels[[t]] <- matrix(rep(1:p, n_q), ncol=p, byrow = T)
means <- list()  #means[[t]] <- matrix(0, n_pc_d, p)
classifications <- list()  #classifications[[t]] <- matrix(0, n_q, p)
cluster_nums <- list()  #cluster_nums[[t]] <- rep(0, n_q)
best_models <- list()  #best_models[[t]] <- c()
covariances <- list()  #covariances[[t]] is also a list.
