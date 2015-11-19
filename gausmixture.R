eval_gausmix <- function(x, prop, mmeans, vvar){
	pvalue <- 0.0; 
	
	
	for(i in 1:length(prop)){
		pvalue <- pvalue + 
		prop[i] * dnorm(x, mean = mmeans[i], sd = sqrt(vvar[i])); 
	}

	return(pvalue);  
}


library(Rmixmod); 

load('weight_matrix_cv_CFHTLS__2_3.RData'); 

train <- read.table("train.dat", header = F); 
test <- read.table("cv.dat", header = F); 

mnlp_result <- c(); 
mnlp_maxkomp <- c(); 
for(maxkomp in 1:8){

  mnlp_vec <- c(); 
  select_clus <- c();

  for(i in 1:nrow(weight_matrix)){
    print(i);
    wweights <- weight_matrix[i, weight_matrix[i,]!=0]; 
	  wweights <- round(wweights * length(train[,1])); 
	  yy <- train[weight_matrix[i,] != 0,1]; 
  
	  gmm <- mixmodCluster(yy, criterion = "NEC",	weight = wweights, 
		  nbCluster = 1:3); 
    select_clus[i] <- gmm[9][1]; 

	  proportions <- as.numeric(gmm[13][1]);
	  means <- as.numeric(gmm[13][2]); 
	  variances <- as.numeric(gmm[13][3])

	  pvalue <- eval_gausmix(test[i,1], proportions, 
		  means, variances); 

	  mnlp_vec[i] <- -log(pvalue + 0.00000001); 

  }

  print('MNLP Value Gausmix'); 
  print(mean(mnlp_vec)); 
  mnlp_result[maxkomp] <- mean(mnlp_vec); 
  mnlp_maxkomp[maxkomp] <- maxkomp; 
}
