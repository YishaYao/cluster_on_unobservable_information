#!/usr/bin/env Rscript
#SBATCH --mem-per-cpu=6g  --time=11:00:00 --mail-type=ALL --mail-user=yisha.yao@yale.edu

options(warn = 1)
load("/gpfs/gibbs/project/zhang_heping/yy634/cluster_gene_association/real_data/chr9/chr9_design.RData")
load("/gpfs/gibbs/project/zhang_heping/yy634/cluster_gene_association/real_data/phenotype.RData")
rm(n_X, n_X_group_sizes)
p <- dim(Y)[2]

alpha <- 16; opt_lam <- 4200; lam_0 <- 5500;  lam_thred <- 3400
library(plyr)
library(MASS)
library(Matrix)
library(plus)
library(mclust)
library(grplasso)


#the records of iterations
Betas <- list()  #Betas[[t]] <- matrix(0, n_pc_d, p)
Blabels <- list()  #Blabels[[t]] <- matrix(rep(1:p, n_q), ncol=p, byrow = T)
means <- list()  #means[[t]] <- matrix(0, n_pc_d, p)
classifications <- list()  #classifications[[t]] <- matrix(0, n_q, p)
cluster_nums <- list()  #cluster_nums[[t]] <- rep(0, n_q)
best_models <- list()  #best_models[[t]] <- c()
covariances <- list()  #covariances[[t]] is also a list.
#to obtain the starting point
Beta_0 <- matrix(0, n_pc_d, p)
Beta_0_labels <- matrix(rep(1:p, n_q), ncol=p, byrow = T)
for (j in 1:p){
  fit <- grplasso(n_s_PC_design, y=Y[,j], index=n_PC_grouping, lambda=lam_0, model=LinReg(), penscale=sqrt, 
                  control=grpl.control(update.hess="lambda", trace=0), center=FALSE)
  grp_est <- fit$coefficients[,1]
  if (all(grp_est==0)){}else {
    indicator <- c(1:n_q)
    for (k in 1:n_q){
      if (all(grp_est[n_pc_low[k]:n_pc_upp[k]]==0)){indicator[k] <- 0}
    }  
    indicator <- indicator[which(indicator!=0)]
    grp_list <- list()
    for (l in indicator){
      grp_list[[l]] <- c(n_pc_low[l]:n_pc_upp[l])
    } 
    grp_ind <- unlist(grp_list)
    mcp <- plus(n_s_PC_design[,grp_ind], Y[,j], method = "mc", normalize = TRUE, intercept = FALSE)
    Beta_0[grp_ind,j] <- mcp$beta[dim(mcp$beta)[1],]
  }
  for (i in 1:n_q){
    if (all(Beta_0[n_pc_low[i]:n_pc_upp[i],j]==0)){Beta_0_labels[i,j] <- 0}
  }
}
Betas[[1]] <- Beta_0
Blabels[[1]] <- Beta_0_labels 
rm(i,j,k,l, fit, mcp, grp_list, grp_ind, grp_est, indicator, Beta_0, Beta_0_labels)

Beta_temp <- Betas[[1]]
Blabels_temp <- Blabels[[1]]
means_temp <- matrix(0, n_pc_d, p)
covs_temp <- list()
best_models_temp <- c()
classifications_temp <- matrix(0, n_q, p)
cluster_nums_temp <- rep(0, n_q)
for (i in 1:n_q){
  if (length(which(Blabels_temp[i,]!=0))<5){ # 5 can be adjusted in real data analysis
    classifications_temp[i,] <- rep(0, p)
    best_models_temp <- c("nocluster")
    means_temp[n_pc_low[i]:n_pc_upp[i],] <- 0
    covs_temp[[i]] <- 0
    cluster_nums_temp[i] <- 0
  } else if (length(which(Blabels_temp[i,]!=0))==p){
    temp <- Beta_temp[(n_pc_low[i]:n_pc_upp[i]),1:p]
    mod <- Mclust(t(temp), G=1:12, control=emControl(eps=0.001))
    classifications_temp[i,] <- mod$classification
    best_models_temp[i] <- mod$modelName
    for (j in 1:p){
      if (n_PC_group_sizes[i]==1){
        means_temp[n_pc_low[i]:n_pc_upp[i],j] <- mod$parameters$mean[classifications_temp[i,j]]
      }else {
        means_temp[n_pc_low[i]:n_pc_upp[i],j] <- mod$parameters$mean[,classifications_temp[i,j]]
      }
    }
    covs_temp[[i]] <- mod$parameters$variance$sigma
    cluster_nums_temp[i] <- mod$G
  } else{
    temp <- Beta_temp[(n_pc_low[i]:n_pc_upp[i]),Blabels_temp[i,]]
    mod <- Mclust(t(temp), G=1:12, control=emControl(eps=0.001))
    index1 <- which(Blabels_temp[i,] ==0)
    part1 <- as.data.frame(cbind(index1, 0))
    colnames(part1) <- c("index", "label")
    index2 <- which(Blabels_temp[i,] !=0)
    part2 <- as.data.frame(cbind(index2, mod$classification))
    colnames(part2) <- c("index", "label")
    est <- rbind(part1, part2)
    est <- est[order(est$index),]
    classifications_temp[i,] <- est$label
    best_models_temp[i] <- mod$modelName
    for (j in 1:p){
      if (classifications_temp[i,j]==0){means_temp[n_pc_low[i]:n_pc_upp[i],j] <- 0}else {
        if (n_PC_group_sizes[i]==1){
          means_temp[n_pc_low[i]:n_pc_upp[i],j] <- mod$parameters$mean[classifications_temp[i,j]]
        }else {
          means_temp[n_pc_low[i]:n_pc_upp[i],j] <- mod$parameters$mean[,classifications_temp[i,j]]
        }
      }
    }
    covs_temp[[i]] <- mod$parameters$variance$sigma
    cluster_nums_temp[i] <- mod$G
  }
}
means[[1]] <- means_temp
covariances[[1]] <- covs_temp
classifications[[1]] <- classifications_temp
cluster_nums[[1]] <- cluster_nums_temp
best_models[[1]] <- best_models_temp
rm(i, temp, mod, index1, part1, index2, part2, est, Beta_temp, Blabels_temp) 
#  means_temp, classifications_temp, cluster_nums_temp, best_models_temp, covs_temp
