#Author: MMRAU


#########################################################
# This module generate the weight matrices              #
# and stores them as an RData file                      #
# The density for an object is then for instance        #
# reconstructed for the 1'st object                     #
# using density(train[,1], weights = weight_matrix[1,]) #
#########################################################


source('ordinal_class.R');

calc_weights <- function(cv_class, numel, train_el_class){
  train_weights <- vector(length = length(train_el_class));
    for(j in 1:length(numel)){
    train_weights[train_el_class == j] <- length(train_el_class) * cv_class[j] / numel[j];
  }
  return(train_weights);
}

#num_of_bins <- 10;

train <- read.table("<path to train data>", header = T);
train_class <- train;
cv <- read.table("<path to cv data>", header = T);


bins_class <- seq(min(train[,1]) - 0.00001, max(train[,1]) + 0.00001, by = 0.01);

#bin the response of train:

num_el_bin <- vector(length = length(bins_class) - 1);
for(i in 1:(length(bins_class) - 1)){
  print(i);
  index_tr <- train[,1] > bins_class[i] & train[,1] <= bins_class[i + 1]
  num_el_bin[i] <- sum(index_tr);
  train_class[index_tr,1] <- i;
}

train_class[,1] <- as.factor(train_class[,1]);

print('End binning')

#Define tuning parameters (pick your own)
tunematrix <- matrix(ncol = 2, nrow = 5);
#Format of the tuning matrix:
##mtry nodesize
#
#Example:
#nodesize = 1

tunematrix[1,] <- c(1, 1);
tunematrix[2,] <- c(2, 1);
tunematrix[3,] <- c(3, 1);
tunematrix[4,] <- c(4, 1);
tunematrix[5,] <- c(5, 1);


#perform tuning steps
for(t in 1:length(tunematrix[,1])){
  #distribute on the elements in the trainingset

  cv_class <- generate_bi_probs(bins_class, train, cv, tunematrix[t, 1] , tunematrix[t, 2]);

  weight_matrix <- matrix(nrow = length(cv[,1]), ncol = length(train[,1]));

  for(i in 1:length(weight_matrix[,1])){
    print('calc weight matrix');
    print(i);
    weight_matrix[i,] <- calc_weights(cv_class[i,], num_el_bin, train_class[,1]);
  }

  #eliminate positive elments due to numerical errors by setting them to 0
  weight_matrix[weight_matrix < 0]  <- 0;
  #now normalize this in order to let it sum to one

  for(i in 1:length(weight_matrix[,1])){
    weight_matrix[i,] <- weight_matrix[i,]/sum(weight_matrix[i,]);
  }
  print('Store the weight matrix');
  print(t);
  save(weight_matrix, file = paste(paste("weight_matrix_<your tag>", "_mtry",  toString(tunematrix[t,1]), "nodesize", toString(tunematrix[t,2]), sep="_"), "RData", sep = "."));

}
