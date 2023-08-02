source('functions.R')


# ----------------------- ANALYSIS FUNCTIONS ----------------------- #


analyse_subset_S = function(M, nmf_methods, n_sign, n_itr, subset_values) {
  
  for (method in nmf_methods) {
    for (subset_value in subset_values) {
      
      ES_matrices = create_ES_matrices_subset(M, method, n_sign, n_itr, subset_value)
      
      S_matrices = ES_matrices[[2]]
      cosine_matrices = create_cosine_matrices(S_matrices)
      scores = calc_scores(cosine_matrices)
      write.table(scores, file = paste0('results/signature/subset/subset_', gsub('/','',method), '_', sprintf("%03g", subset_value*100), '.csv'),
                  row.names = FALSE, col.names = paste0('values_', method, '0', subset_value*10))
      
    }
  }
}
  

analyse_permutations_ES = function(M, nmf_methods, n_sign, n_itr,
                                   clust_method, dist_method, n_clust) {
  
  for (method in nmf_methods) {
    
    ES_matrices = create_ES_matrices_permut(M, method, n_sign, n_itr)
    
    S_matrices = ES_matrices[[2]]
    cosine_matrices = create_cosine_matrices(S_matrices)
    scores = calc_scores(cosine_matrices)
    
    write.table(scores, file = paste0('results/signature/permutation/perm', n_itr, '_', gsub('/','',method), '.csv'), 
                row.names = FALSE, col.names = paste0('value_', method)) 
    
    
    E_matrices = ES_matrices[[1]]
    clust_df = create_clust_df(E_matrices, clust_method, dist_method, n_clust)
    ari_M = create_ari_matrix(clust_df)
    p_val_M = create_p_val_matrix(clust_df)
    
    write.table(ari_M, file = paste0('results/cluster/permutation/ari_perm', n_itr, '_', gsub('/','',method), '.csv'), 
                row.names = TRUE, col.names = TRUE)
    
    write.table(p_val_M, file = paste0('results/cluster/permutation/pval_perm', n_itr, '_', gsub('/','',method), '.csv'), 
                row.names = TRUE, col.names = TRUE)
  
  }
}



analyse_seed_ES = function(M, nmf_methods, n_sign, n_itr,
                                   clust_method, dist_method, n_clust) {
  
  for (method in nmf_methods) {
    
    ES_matrices = create_ES_matrices_seed(M, method, n_sign, n_itr)
    
    S_matrices = ES_matrices[[2]]
    cosine_matrices = create_cosine_matrices(S_matrices)
    scores = calc_scores(cosine_matrices)
    
    write.table(scores, file = paste0('results/signature/seed/seed', n_itr, '_', gsub('/','',method), '.csv'), 
                row.names = FALSE, col.names = paste0('value_', method))
    
    
    E_matrices = ES_matrices[[1]]
    clust_df = create_clust_df(E_matrices, clust_method, dist_method, n_clust)
    ari_M = create_ari_matrix(clust_df)
    p_val_M = create_p_val_matrix(clust_df)
    
    write.table(ari_M, file = paste0('results/cluster/seed/ari_seed', n_itr, '_', gsub('/','',method), '.csv'), 
                row.names = TRUE, col.names = TRUE)
    
    write.table(p_val_M, file = paste0('results/cluster/seed/pval_seed', n_itr, '_', gsub('/','',method), '.csv'), 
                row.names = TRUE, col.names = TRUE)
    
  }
}



analyse_noise_ES = function(M, nmf_methods, n_sign, n_itr, noise_values,
                             clust_method, dist_method, n_clust) {
  
  for (method in nmf_methods) {
    for (noise_value in noise_values) {
      
      ES_matrices = create_ES_matrices_noise(M, method, n_sign, n_itr, noise_value)
      
      S_matrices = ES_matrices[[2]]
      cosine_matrices = create_cosine_matrices(S_matrices)
      scores = calc_scores(cosine_matrices)
      write.table(scores, file = paste0('results/signature/noise/noise_', gsub('/','',method), '_', noise_value*10, '.csv'),
                  row.names = FALSE, col.names = paste0('values_', method, noise_value))
      
      
      E_matrices = ES_matrices[[1]]
      clust_df = create_clust_df(E_matrices, clust_method, dist_method, n_clust)
      ari_M = create_ari_matrix(clust_df)
      p_val_M = create_p_val_matrix(clust_df)
      
      write.table(ari_M, file = paste0('results/cluster/noise/ari_noise_', gsub('/','',method), '_', noise_value*10, '.csv'), 
                  row.names = TRUE, col.names = TRUE)
      
      write.table(p_val_M, file = paste0('results/cluster/noise/pval_noise_', gsub('/','',method), '_', noise_value*10, '.csv'), 
                  row.names = TRUE, col.names = TRUE)
    }
  }
}




