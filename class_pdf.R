#library(randomForest); 

source('ordinary_class.R'); 

calc_weights <- function(cv_class, numel, train_el_class){
  train_weights <- vector(length = length(train_el_class));
  #vectorize: 
  #browser();
  for(j in 1:length(numel)){
    train_weights[train_el_class == j] <- length(train_el_class) * cv_class[j] / numel[j]; 
  }
  return(train_weights); 
}

#num_of_bins <- 10;

train <- read.table("cv_datasets/traincv.dat", header = T);

train_class <- train;
cv <- read.table("cv_datasets/cv.dat", header = T); 


bins_class <- seq(min(train[,1]) - 0.00001, max(train[,1]) + 0.00001, by = 0.01); 


#bin the response of train:

num_el_bin <- vector(length = length(bins_class) - 1);
for(i in 1:(length(bins_class) - 1)){
  print(i);
  index_tr <- train[,1] > bins_class[i] & train[,1] <= bins_class[i + 1]
  num_el_bin[i] <- sum(index_tr); 
  train_class[index_tr,1] <- i; 
}

##browser();
train_class[,1] <- as.factor(train_class[,1]);
print('End binning')
##browser();

tunematrix <- matrix(ncol = 2, nrow = 20); 
##mtry nodesize
#
#nodesize = 1

tunematrix[1,] <- c(1, 1); 
tunematrix[2,] <- c(2, 1); 
tunematrix[3,] <- c(3, 1); 
tunematrix[4,] <- c(4, 1); 
tunematrix[5,] <- c(5, 1); 

#nodesize = 2 

tunematrix[6,] <- c(1, 2); 
tunematrix[7,] <- c(2, 2); 
tunematrix[8,] <- c(3, 2); 
tunematrix[9,] <- c(4, 2); 
tunematrix[10,] <- c(5, 2); 

#nodesize = 3

tunematrix[11,] <- c(1, 3); 
tunematrix[12,] <- c(2, 3); 
tunematrix[13,] <- c(3, 3); 
tunematrix[14,] <- c(4, 3); 
tunematrix[15,] <- c(5, 3); 

#nodesize = 5

tunematrix[16,] <- c(1, 5); 
tunematrix[17,] <- c(2, 5); 
tunematrix[18,] <- c(3, 5); 
tunematrix[19,] <- c(4, 5); 
tunematrix[20,] <- c(5, 5); 

tunematrix <- matrix(ncol = 2, nrow = 10); 

#nodesize = 7 

tunematrix[1,] <- c(1, 7);
tunematrix[2,] <- c(2, 7);
tunematrix[3,] <- c(3, 7);
tunematrix[4,] <- c(4, 7);
tunematrix[5,] <- c(5, 7);

#nodesize = 9 

tunematrix[6,] <- c(1, 9);
tunematrix[7,] <- c(2, 9);
tunematrix[8,] <- c(3, 9);
tunematrix[9,] <- c(4, 9);
tunematrix[10,] <- c(5, 9);


for(t in 1:length(tunematrix[,1])){
#distribute on the elements in the trainingset


cv_class <- generate_bi_probs(bins_class, train, cv, tunematrix[t, 1] , tunematrix[t, 2]); 

weight_matrix <- matrix(nrow = length(cv[,1]), ncol = length(train[,1]));

for(i in 1:length(weight_matrix[,1])){
  print('calc weight matrix'); 
  print(i);
  #browser()
  weight_matrix[i,] <- calc_weights(cv_class[i,], num_el_bin, train_class[,1]);
}

#eliminate positive elments due to numerical errors by setting them to 0
weight_matrix[weight_matrix < 0]  <- 0; 
#now normalize this in order to let it sum to one

for(i in 1:length(weight_matrix[,1])){
  weight_matrix[i,] <- weight_matrix[i,]/sum(weight_matrix[i,]); 
}
print('output');
print(t); 
save(weight_matrix, file = paste(paste("weight_matrix_cv_CFHTLS_", "mtry",  toString(tunematrix[t,1]), "nodesize", toString(tunematrix[t,2]), sep="_"), "RData", sep = "."));

}
