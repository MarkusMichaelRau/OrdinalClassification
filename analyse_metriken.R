source('./analyse_ordinal_class/get_mnlp.R'); 
source('./analyse_ordinal_class/get_stats.R'); 


cv <- read.table('cv_datasets/cv.dat', header = T);
train <- read.table('cv_datasets/traincv.dat', header = T);

kernels <<- "gaussian"; 


generate_output <- function(weight_matrix, bw_mod){

  bw_scott <- get_scott(weight_matrix, train[,1]) * bw_mod; 
		
  mnlp_scott <- get_mnlp(weight_matrix, train[,1], cv[,1],
      bw_scott, kernels); 

  print('mnlp_scott'); 
  print(mnlp_scott);

  bw_hjort <- get_hjort(weight_matrix, train[,1]) * bw_mod; 

  mnlp_hjort <- get_mnlp(weight_matrix, train[,1], cv[,1],
                         bw_hjort, kernels); 

  print('mnlp hjort'); 
  print(mnlp_hjort); 


}

print_results <- function(weight_matrix){

print('bw_mod 0.5'); 
generate_output(weight_matrix, 0.5); 
print('bw_mod 0.6'); 
generate_output(weight_matrix, 0.6); 
print('bw_mod 0.7'); 
generate_output(weight_matrix, 0.7); 
print('bw_mod 0.8'); 
generate_output(weight_matrix, 0.8); 
print('bw_mod 0.9'); 
generate_output(weight_matrix, 0.9); 
print('bw_mod 1.0'); 
generate_output(weight_matrix, 1.0); 
print('bw_mod 1.1'); 
generate_output(weight_matrix, 1.1); 
print('bw_mod 1.2'); 
generate_output(weight_matrix, 1.2); 
print('bw_mod 1.3'); 
generate_output(weight_matrix, 1.3); 
print('bw_mod 1.4'); 
generate_output(weight_matrix, 1.4); 
print('bw_mod 1.5'); 
generate_output(weight_matrix, 1.5); 
print('bw_mod 1.6'); 
generate_output(weight_matrix, 1.6); 
print('bw_mod 1.7'); 
generate_output(weight_matrix, 1.7); 
print('bw_mod 1.8'); 
generate_output(weight_matrix, 1.8); 
print('bw_mod 1.9'); 
generate_output(weight_matrix, 1.9); 
print('bw_mod 2.0'); 
generate_output(weight_matrix, 2.0); 
print('bw_mod 2.1'); 
generate_output(weight_matrix, 2.1); 
print('bw_mod 2.2'); 
generate_output(weight_matrix, 2.2); 
print('bw_mod 2.3'); 
generate_output(weight_matrix, 2.3); 
print('bw_mod 2.4'); 
generate_output(weight_matrix, 2.4); 
print('bw_mod 2.5'); 
generate_output(weight_matrix, 2.5); 
print('bw_mod 2.6'); 
generate_output(weight_matrix, 2.6); 
print('bw_mod 2.7'); 
generate_output(weight_matrix, 2.7); 
print('bw_mod 2.8'); 
generate_output(weight_matrix, 2.8); 
print('bw_mod 2.9'); 
generate_output(weight_matrix, 2.9); 
print('bw_mod 3.0'); 
generate_output(weight_matrix, 3.0); 


}


#load('weight_matrix_cv_CFHTLS__mtry_1_nodesize_1.RData');
#print('mtry 1 nodesize 1'); 
#print_results(weight_matrix); 
#
#load('weight_matrix_cv_CFHTLS__mtry_1_nodesize_2.RData');
#print('mtry 1 nodesize 2');
#print_results(weight_matrix); 
#
#load('weight_matrix_cv_CFHTLS__mtry_1_nodesize_3.RData');
#print('mtry 1 nodesize  3'); 
#print_results(weight_matrix); 
#
#load('weight_matrix_cv_CFHTLS__mtry_1_nodesize_5.RData');
#print('mtry 1 nodesize 5'); 
#print_results(weight_matrix); 
#
#load('weight_matrix_cv_CFHTLS__mtry_2_nodesize_1.RData');
#print('mtry 2 nodesize 1'); 
#print_results(weight_matrix); 
#
#load('weight_matrix_cv_CFHTLS__mtry_2_nodesize_2.RData');
#print('mtry 2 nodesize 2'); 
#print_results(weight_matrix); 
#
#load('weight_matrix_cv_CFHTLS__mtry_2_nodesize_3.RData');
#print('mtry 2 nodesize 3'); 
#print_results(weight_matrix); 
#
#
#load('weight_matrix_cv_CFHTLS__mtry_2_nodesize_5.RData');
#print('mtry 2 nodesize 5'); 
#print_results(weight_matrix); 
#
#
#load('weight_matrix_cv_CFHTLS__mtry_3_nodesize_1.RData');
#print('mtry 3 nodesize 1'); 
#print_results(weight_matrix); 
#
#
#load('weight_matrix_cv_CFHTLS__mtry_3_nodesize_2.RData');
#print('mtry 3 nodesize 2'); 
#print_results(weight_matrix); 
#
#
#load('weight_matrix_cv_CFHTLS__mtry_3_nodesize_3.RData');
#print('mtry 3 nodesize 3'); 
#print_results(weight_matrix); 
#
#
#load('weight_matrix_cv_CFHTLS__mtry_3_nodesize_5.RData');
#print('mtry 3 nodesize 5'); 
#print_results(weight_matrix); 
#
#
#load('weight_matrix_cv_CFHTLS__mtry_4_nodesize_1.RData');
#print('mtry 4 nodesize 1'); 
#print_results(weight_matrix); 
#
#load('weight_matrix_cv_CFHTLS__mtry_4_nodesize_2.RData');
#print('mtry 4 nodesize 2'); 
#print_results(weight_matrix); 
#
#
#load('weight_matrix_cv_CFHTLS__mtry_4_nodesize_3.RData');
#print('mtry 4 nodesize 3'); 
#print_results(weight_matrix); 
#
#
#load('weight_matrix_cv_CFHTLS__mtry_4_nodesize_5.RData');
#print('mtry 4 nodesize 5'); 
#print_results(weight_matrix); 
#
#
#load('weight_matrix_cv_CFHTLS__mtry_5_nodesize_1.RData');
#print('mtry 5 nodesize 1'); 
#print_results(weight_matrix); 
#
#
#load('weight_matrix_cv_CFHTLS__mtry_5_nodesize_2.RData');
#print('mtry 5 nodesize 2'); 
#print_results(weight_matrix); 
#
#
#load('weight_matrix_cv_CFHTLS__mtry_5_nodesize_3.RData');
#print('mtry 5 nodesize 3'); 
#print_results(weight_matrix); 
#
#
#load('weight_matrix_cv_CFHTLS__mtry_5_nodesize_5.RData');
#print('mtry 5 nodesize 5'); 
#print_results(weight_matrix); 

load('weight_matrix_cv_CFHTLS__mtry_1_nodesize_7.RData');
print('mtry 1 nodesize 7'); 
print_results(weight_matrix); 

load('weight_matrix_cv_CFHTLS__mtry_2_nodesize_7.RData');
print('mtry 2 nodesize 7'); 
print_results(weight_matrix); 

load('weight_matrix_cv_CFHTLS__mtry_3_nodesize_7.RData');
print('mtry 3 nodesize 7'); 
print_results(weight_matrix); 

load('weight_matrix_cv_CFHTLS__mtry_4_nodesize_7.RData');
print('mtry 4 nodesize 7'); 
print_results(weight_matrix); 


load('weight_matrix_cv_CFHTLS__mtry_5_nodesize_7.RData');
print('mtry 5 nodesize 7'); 
print_results(weight_matrix); 


load('weight_matrix_cv_CFHTLS__mtry_1_nodesize_9.RData');
print('mtry 1 nodesize 9'); 
print_results(weight_matrix); 

load('weight_matrix_cv_CFHTLS__mtry_2_nodesize_9.RData');
print('mtry 2 nodesize 9'); 
print_results(weight_matrix); 

load('weight_matrix_cv_CFHTLS__mtry_3_nodesize_9.RData');
print('mtry 3 nodesize 9'); 
print_results(weight_matrix); 

load('weight_matrix_cv_CFHTLS__mtry_4_nodesize_9.RData');
print('mtry 4 nodesize 9'); 
print_results(weight_matrix); 

load('weight_matrix_cv_CFHTLS__mtry_5_nodesize_9.RData');
print('mtry 5 nodesize 9'); 
print_results(weight_matrix); 





