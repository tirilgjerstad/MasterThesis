

library(NMF) 

# visualizing 
library(heatmaply) 
library(plotly)
library(ggplot2)
library(ggtext)
library(cowplot) # plot side by side 
library(scales)


# saving 
library(htmlwidgets)
library(webshot)
library(reticulate)
reticulate::py_run_string("import sys")

library(combinat) # permutations 
library(mclust) # adjusted rand index 
library(amap) # Dist()
library(clusterSim) # data.Normalization
library(proxy)



# ------------------------------- READ DATA ---------------------------- #


read_M = function(path_M) {
  # Reads the M matrix 
  # Returns M as a data frame 
  
  M = read.table(path_M, header = TRUE, row.names = 1, sep = '')
  names(M) = scan(path_M, what = 'character', sep = " ", nlines = 1)
  
  return(M)
}


read_hartwig_M = function(path_M) {
  
  colnames = c('A[C>A]A', 'A[C>A]C', 'A[C>A]G', 'A[C>A]T', 'C[C>A]A', 'C[C>A]C', 'C[C>A]G', 'C[C>A]T', 
               'G[C>A]A', 'G[C>A]C', 'G[C>A]G', 'G[C>A]T', 'T[C>A]A', 'T[C>A]C', 'T[C>A]G', 'T[C>A]T', 
               'A[C>G]A', 'A[C>G]C', 'A[C>G]G', 'A[C>G]T', 'C[C>G]A', 'C[C>G]C', 'C[C>G]G', 'C[C>G]T', 
               'G[C>G]A', 'G[C>G]C', 'G[C>G]G', 'G[C>G]T', 'T[C>G]A', 'T[C>G]C', 'T[C>G]G', 'T[C>G]T', 
               'A[C>T]A', 'A[C>T]C', 'A[C>T]G', 'A[C>T]T', 'C[C>T]A', 'C[C>T]C', 'C[C>T]G', 'C[C>T]T', 
               'G[C>T]A', 'G[C>T]C', 'G[C>T]G', 'G[C>T]T', 'T[C>T]A', 'T[C>T]C', 'T[C>T]G', 'T[C>T]T', 
               'A[T>A]A', 'A[T>A]C', 'A[T>A]G', 'A[T>A]T', 'C[T>A]A', 'C[T>A]C', 'C[T>A]G', 'C[T>A]T', 
               'G[T>A]A', 'G[T>A]C', 'G[T>A]G', 'G[T>A]T', 'T[T>A]A', 'T[T>A]C', 'T[T>A]G', 'T[T>A]T', 
               'A[T>C]A', 'A[T>C]C', 'A[T>C]G', 'A[T>C]T', 'C[T>C]A', 'C[T>C]C', 'C[T>C]G', 'C[T>C]T', 
               'G[T>C]A', 'G[T>C]C', 'G[T>C]G', 'G[T>C]T', 'T[T>C]A', 'T[T>C]C', 'T[T>C]G', 'T[T>C]T', 
               'A[T>G]A' ,'A[T>G]C', 'A[T>G]G', 'A[T>G]T', 'C[T>G]A', 'C[T>G]C', 'C[T>G]G', 'C[T>G]T', 
               'G[T>G]A', 'G[T>G]C', 'G[T>G]G', 'G[T>G]T', 'T[T>G]A' ,'T[T>G]C' ,'T[T>G]G' ,'T[T>G]T')
  
  
  M = read.csv(path_M, sep = '', header = FALSE)
  
  
  names(M) = colnames
  row.names(M) = paste0('id', 1:nrow(M))
  
  return(M)
}

read_clinical = function(path_clinical) {
  # Reads the cilical data 
  # Returns a dataframe 
  clinical = read.table(path_clinical, header = TRUE, row.names = NULL, sep = '')
  
  return(clinical)
  
}


# ----------------------------- CALCULATIONS  -------------------------- #


nmf_model = function(data, n_sign, method, seed = 1234, beta = 0.0001, eta = 0.1) {
  # Create a model for NMF 
  
  # data: The M matrix 
  # n_sign: Number of signatures / the rank
  # Method: The NMF method to use 
  
  data = data[which(rowSums(data) > 0), which(colSums(data) > 0)]
  
  if (method == 'snmf/r' || method == 'snmf/l') {
    model = nmf(data, n_sign, method, seed = seed, beta = beta, eta = eta)
  } else {
    model = nmf(data, n_sign, method, seed = seed)
    }
  
  return(model)
  
}

nmf_clustering = function(E, dist_method, hclust_method, k_groups) {
  # Create a clustering based on NMF result
  
  # E: The Expusure matrix from NMF 
  # dist_method: The distanse method used while clustering 
  # hclust_method: The clustering method used while clustering 
  # k_groups: The number of clusters to return 
  
  if (dist_method == 'correlation') {
    d = as.dist(1 - cor(t(E)))
  } 
  else if (dist_method %in% c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")) {
    d = dist(E, dist_method)
  } else {return()}
  
  if (!(hclust_method %in% c('complete', 'average'))) {return()}
  
  dend = hclust(d, method = hclust_method)

  # Return the dataframe with the grouping 
  data.frame(cluster = cutree(dend, k = k_groups))
  
}

calc_best_score = function(cosine_matrix) {
  
  # Calculates the best match and its score for a given cosine matrix 
  # cosine_matrix: A n x n matrix with the cosine scores between every pair of vectors from two matrices.
  
  n = dim(cosine_matrix)[1]
  
  best_score = 0
  best_perm = NULL
  
  
  for (perm in permn(n)) {
    score = 0
    for (i in 1:n) {
      score = score + cosine_matrix[i, perm[i]]
    }
    
    if (score > best_score) {
      best_score = score
      best_perm = perm
    }
  }
  
  return(list(best_score/n, best_perm))
  
}

calc_scores = function(cosine_matrices) {
  scores = c()
  
  for (sim in cosine_matrices) {
    scores = append(scores, calc_best_score(sim)[[1]])
  }
  
  return(scores)
  
  
}


add_binomial_noise = function(M, alpha) {
  # M: input matrix
  # alpha: used to set the variance relative on the value of each element
  
  apply(M, c(1, 2), function(m) {
    if (m > 0 && alpha > 1) {
      v = m * alpha
      size = m^2 / (v-m)
      prob = m/v
      max(rnbinom(1, size = size, prob = prob), 0)
    } else {m}})
}

test_random_cos_sim = function(n = 10000) {
  cos_vals = c()
  
  for (i in 1:n) {
    vec1 = runif(96)
    vec2 = runif(96)
    
    cos_v = cosine(vec1, vec2)
    cos_vals[i] = cos_v
    
  }
  
  print(summary(cos_vals))
  
}

subsample = function(x,p) {
  xnew = x
  xc = cumsum(x)
  n = sum(x)
  k = sample(1:n, floor(n*(1-p)))
  processed = F
  for (i in 1:length(x)) {
    ok = !processed & k <= xc[i]
    xnew[i] = xnew[i] - sum(ok)
    processed = k <= xc[i]
  }
  xnew
}

create_subsample_M = function(M, subvalue, filename) {
  
  r_sum = 1
  
  while(r_sum > 0) {
    
    subsample = as.data.frame(t(apply(M, 1, function(row) {
      subsample(row, subvalue)
    })))
    
    r_sum = sum(rowSums(subsample))
  }
  
  write.table(subsample, file = filename, row.names = FALSE, col.names = FALSE)
}

create_TCGA_M_subset50 = function() {
  TCGA_sub50 = TCGA_M[sort(sample(nrow(TCGA_M), round(0.5*nrow(TCGA_M)))), ]
  TCGA_sub50 = TCGA_sub50[which(rowSums(TCGA_sub50) < 500), ]
  write.table(TCGA_sub50, file = 'files/TCGA_M_subset50.txt', row.names = TRUE, col.names = TRUE)
}



# ---------------------------- CREATE MATRICES ------------------------- #

create_ES_matrices_subset = function(M, nmf_method, n_sign, n_itr, subset_value) {
  
  E_matrices = list()
  S_matrices = list()
  n = dim(M)[1]
  
  for (i in 1:n_itr) {
    
    r_sum = 1
    while(r_sum > 0) {
      sub_M = M[sort(sample(n, round(subset_value*n))), ]
      r_sum = sum(colSums(sub_M) == 0) + sum(rowSums(sub_M) == 0)
    }
    
    model = nmf_model(sub_M, n_sign, nmf_method)
    E = basis(model)
    E_matrices = append(E_matrices, list(E))
    S = coef(model)
    S_matrices = append(S_matrices, list(S))
  }
  
  return(list(E_matrices, S_matrices))
}

create_ES_matrices_permut = function(M, nmf_method, n_sign, n_itr) {
  
  E_matrices = list()
  S_matrices = list()
  n = dim(M)[1]
  
  for (i in 1:n_itr) {
    perm_M = M[sample(n, n), ]
    model = nmf_model(perm_M, n_sign, nmf_method)
    E = basis(model)
    E_matrices = append(E_matrices, list(E))
    S = coef(model)
    S_matrices = append(S_matrices, list(S))
  }
  
  return(list(E_matrices, S_matrices))
}
  
create_ES_matrices_seed = function(M, nmf_method, n_sign, n_itr) {
  
  E_matrices = list()
  S_matrices = list()
  
  for (i in 1:n_itr) {
    model = nmf_model(M, n_sign, nmf_method, seed = i)
    E = basis(model)
    E_matrices = append(E_matrices, list(E))
    S = coef(model)
    S_matrices = append(S_matrices, list(S))
  }
  
  return(list(E_matrices, S_matrices))
  
  }

create_ES_matrices_noise = function(M, nmf_method, n_sign, n_itr, noise_value) {
  
  E_matrices = list()
  S_matrices = list()
  
  for (i in 1:n_itr) {
    
    r_sum1 = 1
    r_sum2 = 1
    while((r_sum1 > 0) | (r_sum2 > 5))  {
      print(paste('r_rum:', r_sum1 + r_sum2, 'i:', i))
      M_noise = add_binomial_noise(M, noise_value)
      
      
      r_sum1 = sum(colSums(M_noise) == 0)
      r_sum2 = sum(rowSums(M_noise) == 0)
      print(paste(r_sum1, r_sum2))
    }
    
    model = nmf_model(M_noise, n_sign, nmf_method)
    E = basis(model)
    E_matrices = append(E_matrices, list(E))
    S = coef(model)
    S_matrices = append(S_matrices, list(S))
  }
  
  return(list(E_matrices, S_matrices))
}

create_cosine_matrices = function(list_of_signature_matrices) {
  
  list_of_cosine_similarity_matrices = list()
  
  
  for (i in 1:length(list_of_signature_matrices)) {
    S1 = list_of_signature_matrices[[i]]
    for (j in 1:length(list_of_signature_matrices)) {
      if (i < j) {
        #print(paste(i, j))
        S2 = list_of_signature_matrices[[j]]
        
        if (ncol(S1) > ncol(S2)) {
          S1 = S1[,sort(sample(ncol(S1), ncol(S2)))]
        }
        if (ncol(S2) > ncol(S1)) {
          S2 = S2[,sort(sample(ncol(S2), ncol(S1)))]
        }
        
        
        cos_m = simil(S1, S2, method = 'cosine')
        list_of_cosine_similarity_matrices = append(list_of_cosine_similarity_matrices, list(cos_m))
      }
    }
  }
  
  return(list_of_cosine_similarity_matrices)
  
}


create_ari_matrix = function(clust_df) {
  
  n = ncol(clust_df)
  ari_M = matrix(nrow = n, ncol = n, dimnames = list(paste0('E', 1:n), paste0('E', 1:n)))
  
  for (i in 1:n) {
    for (j in 1:n) {
      ari_M[i,j] = adjustedRandIndex(clust_df[, i], clust_df[, j])
    }
  }
  
  return(ari_M)
}

create_p_val_matrix = function(clust_df) {
  
  n = ncol(clust_df)
  p_val_M = matrix(nrow = n, ncol = n, dimnames = list(paste0('E', 1:n), paste0('E', 1:n)))
  
  for (i in 1:n) {
    for (j in 1:n) {
      f = fisher.test(table(clust_df[,i], clust_df[,j]), workspace = 2e7)
      p_val_M[i,j] = f$p.value
    }
  }
  
  return(p_val_M)
  
}


create_clust_df = function(E_matrices, clust_method, dist_method, n_clust) {
  
  common_row_names <- Reduce(intersect, lapply(E_matrices, rownames))
  clust_df = data.frame(row.names = common_row_names)
  
  for (E in E_matrices) {
    E = E[common_row_names, ]
    clust = nmf_clustering(E, dist_method, clust_method, n_clust)
    clust_df = base::cbind(clust_df, clust$cluster)
    
  }
  
  return(clust_df)
  
}

create_median_df = function(nmf_methods, file_path) {

  all_files = rev(list.files(file_path, full.names = TRUE))
  medians = c()
  q1s = c()
  q3s = c()
  
  for (method in nmf_methods) {
    method_files = grep(gsub('/','',method), all_files, value = TRUE)
    
    for (i in seq_along(method_files)) {
      file = method_files[i]
      scores = as.matrix(read.table(file, header = TRUE))
      m = median(scores)
      q1 = quantile(scores, 0.25)
      q3 = quantile(scores, 0.75)
      medians = c(medians, m)
      q1s = c(q1s, q1)
      q3s = c(q3s, q3)
    }
  }
  
  median_df = data.frame(x = seq(1,0.75, -0.01), 
                         median = medians, 
                         q1 = q1s,
                         q3 = q3s,
                         methods = rep(paste('Method:', nmf_methods), each = i))
  
  return(median_df)
}

# ---------------------------- PLOT  FUNCTIONS ------------------------- #

plot_M = function(M) {
  
  heatmap = heatmaply(M, colors = Blues(200),
                      
                      xlab = 'Mutation Categories', ylab = 'Sample ID', main = 'Mutation Matrix M',
                      scale = 'none', 
                      margins = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
                      Rowv = NULL, Colv = NULL, dendrogram = 'none',
                      label_names = c('ID', 'Mutation Category', 'Value'),
                      fontsize_row = 8, fontsize_col = 8,
                      dynamicTicks = TRUE,
                      #file = save_as,
                      #showticklabels = c(FALSE, TRUE), k_col = k_groups,
                      
                      
  )
  heatmap
  
}

plot_E = function(E, k_groups, dist_met, hclust_met, save_as = NULL, nmf_met = 'brunet') {

  
  if (dist_met == 'correlation') {
    d = as.dist(1 - cor(t(E)))
  } 
  else if (dist_met %in% c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")) {
    d = dist(E, dist_met)
  } else {return()}
  
  
  if (!(hclust_met %in% c('complete', 'average'))) {return()}
  
  dend = hclust(d, method = hclust_met)
  E = data.Normalization(E, type = 'n10', normalization = 'row')
  

  main_title = paste('Exposure Matrix E','\nNMF-method:', nmf_met, '// dist:', dist_met, ' // hclust:', hclust_met)
  if(!(is.null(save_as))) {
    html_f = paste0(save_as, '.html')
    png_f = paste0(save_as, '.png')
    
    save_as = c(html_f, png_f)
    }
  
  heatmap = heatmaply(t(E), colors = Greens(200),scale = 'none',
            
            xlab = 'Sample ids', ylab = 'Mutation Signature', 
            main = main_title,
            margins = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
            Colv = dend, Rowv = FALSE,
            dendrogram = TRUE, show_dendrogram = TRUE,
            label_names = c('Signature', 'ID', 'Value'),
            fontsize_row = 8, fontsize_col = 8,
            branches_lwd = 0.3,
            showticklabels = c(FALSE, TRUE), k_col = k_groups,
            file = save_as)
  

  
  heatmap 
  
}

plot_S = function(S, k_groups, save_as = NULL, nmf_met = 'brunet') {
  
  S = data.Normalization(S, type = 'n10', normalization = 'row')
  
  main_title = paste('Signature Matrix S\nNMF-method:', nmf_met, '// not clustered')
  
  if(!(is.null(save_as))) {
    html_f = paste0(save_as, '.html')
    png_f = paste0(save_as, '.png')
    
    save_as = c(html_f, png_f)
  }
  
  
  heatmap = heatmaply(S, colors = Oranges(200),
            
            xlab = 'Mutation Categories', ylab = 'Mutation Signatures', main = main_title,
            scale = 'none', 
            margins = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
            Rowv = NULL, Colv = NULL, dendrogram = 'none',
            label_names = c('Sgnature', 'Mutation', 'Value'),
            fontsize_row = 8, fontsize_col = 8,
            dynamicTicks = TRUE,
            file = save_as,
            #showticklabels = c(FALSE, TRUE), k_col = k_groups,
            
            
  )
  
  heatmap
}

plot_cosine = function(cosine_matrix, nmf_met1, nmf_met2, m, save_as = NULL) {
  

  main_title = paste('Cosine similarity of', m, '\nComparing', nmf_met1, 'and', nmf_met2)
  
  if(!(is.null(save_as))) {
    html_f = paste0(save_as, '.html')
    png_f = paste0(save_as, '.png')
    
    save_as = c(html_f, png_f)
  }
  
  
  heatmap = heatmaply(cosine_matrix, colors = Blues(200),
                      
                      xlab = nmf_met2, ylab = nmf_met1, main = main_title,
                      scale = 'none', cellnote = cosine_matrix, cellnote_textposition = 'middle center',
                      margins = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
                      Rowv = NULL, Colv = NULL, dendrogram = 'none',
                      label_names = c(nmf_met1, nmf_met2, 'Similarity'),
                      fontsize_row = 8, fontsize_col = 8,
                      file = save_as
                      
  )
  
  heatmap
}

plot_mutation_count_TCGA = function() {
  
  row_sum_M = data.frame(n_mut = sort(rowSums(TCGA_M), decreasing = TRUE))
  
  p1 = ggplot(row_sum_M, aes(x = seq_along(n_mut), y = n_mut)) + 
    geom_point(size = 1, color = "red") +
    geom_hline(yintercept = median(rowSums(TCGA_M)), linetype = 'dotted', size = 1) + 
    labs(x = 'Samples', y = 'Total number of mutations') + 
    scale_y_continuous(breaks = seq(0,7000,1000)) + 
    theme(axis.title = element_text(size = 18),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14))
  
  
  
  p2 = ggplot(row_sum_M, aes(x = seq_along(n_mut), y = n_mut)) + 
    geom_point(size = 1, color = "red") +
    geom_hline(yintercept = median(rowSums(TCGA_M)), linetype = 'dotted', size = 1) + 
    labs(x = 'Samples', y = 'Total number of mutations') + 
    ylim(0,500) + 
    theme(axis.title = element_text(size = 18),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14))
  
  plot_grid(p1,p2, ncol = 2, labels = 'auto')
  ggsave('results_plot/TCGA_mut_count.png', bg = 'white', height = 4, width = 12)
  
}

plot_mutation_count_hartwig = function() {
  
  row_sum_M = data.frame(n_mut = sort(rowSums(hartwig_M), decreasing = TRUE))
  
  p1 = ggplot(row_sum_M, aes(x = seq_along(n_mut), y = n_mut)) + 
    geom_point(size = 1, color = "blue") +
    geom_hline(yintercept = median(rowSums(hartwig_M)), linetype = 'dotted', size = 1) + 
    labs(x = 'Samples', y = 'Total number of mutations') + 
    scale_y_continuous(breaks = seq(0,2000000,500000),
                       labels = function(x) format(x, big.mark = "", scientific = FALSE),
    ) + 
    theme(axis.title = element_text(size = 18),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14))
  
  
  
  p2 = ggplot(row_sum_M, aes(x = seq_along(n_mut), y = n_mut)) + 
    geom_point(size = 1, color = "blue") +
    geom_hline(yintercept = median(rowSums(hartwig_M)), linetype = 'dotted', size = 1) + 
    labs(x = 'Samples', y = 'Total number of mutations') + 
    scale_y_continuous(
      labels = function(x) format(x, big.mark = "", scientific = FALSE),
      breaks = seq(0, 500000, 100000),
      limits = c(0, 500000)
    ) + 
    theme(axis.title = element_text(size = 18),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14))
  
  plot_grid(p1,p2, ncol = 2, labels = 'auto')
  ggsave('results_plot/hartwig_mut_count.png', bg = 'white', height = 4, width = 12)
}




gghist_cluster = function(all_scores, sub_titles = '', ylimit = 1, ncol = NULL, 
                          title_text_size = 12, axis_text_size = 10, met = NULL, ari = TRUE) {
  
  p_list = list()
  n = length(all_scores)
  
  for (i in seq_along(all_scores)) {
    score = all_scores[[i]]
    
    colr = hist_color(met = met, n = n, i = i)
    
    if (i > n - sqrt(n)) {
      if (ari) {x = 'Adjusted Rand Index Scores'} else {x = 'P value Scores'}
    } else {x = NULL}
    
    if (i %% sqrt(n) == 1) {y = 'fraction'} else {y = NULL}
    
    p = ggplot(as.data.frame(score), aes(x=score)) + 
      geom_histogram(color = 'white', fill = colr, bins = 30, 
                     aes(y = after_stat(count / sum(count)))) + 
      #scale_x_continuous(breaks = seq(floor(min(score)), 1.05, 0.25)) +
      xlim(-0.12, 1.05) + 
      ylim(0 , ylimit) +
      theme(plot.title = element_text(size = title_text_size),
            axis.title = element_text(size = axis_text_size)) + 
      labs(x = x, y = y, title = sub_titles[i])
    
    
    p_list[[i]] = p
    
  }
  
  p = plot_grid(plotlist = p_list, labels = 'AUTO', ncol = ncol)
  return(p)
  
}

plot_cluster = function(matrix, main_title = NULL, filename = NULL) {
  
  
  p = heatmaply(matrix, colors = Blues(200), limits = c(-1, 1),
                 scale = "none", main = main_title, xlab = 'E matrices', ylab = 'E matrices',
                 Rowv = NULL, Colv = NULL, dendrogram = 'none',
                 #cellnote = matrix, cellnote_size = 5,
                 cellnote.format = "%.2f", cellnote_textposition = 'middle center',
                 margins = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
                 file = filename, 
                 showticklabels = TRUE
  )
  
  return(p)
  
}
  
plot_subplot = function(plot_list, nmf_methods, title) {
  
  titles = paste('NMF method:', nmf_methods)
  
  pp = subplot(plot_list, nrows = 2, shareX = TRUE, shareY = TRUE) %>%
    
    layout(title = title)
  
  
  annotations = list( 
    list( 
      x = 0.25,  
      y = 0.92,  
      text = titles[1],  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    ),  
    list( 
      x = 0.75,  
      y = 0.92,  
      text = titles[2],  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    ),  
    list( 
      x = 0.25,  
      y = 0.40,  
      text = titles[3],
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    ),
    list( 
      x = 0.75,  
      y = 0.40,  
      text = titles[4],  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    ))
  
  pp = pp %>% layout(annotations = annotations)
  return(pp)
  
}

  
  
  
plot_ari = function(matrix, title = NULL, filename = NULL) {
  
  title = paste0('Adjusted Rand Index \n', title)
  
  
  p = heatmaply(matrix, colors = YlGnBu(200), limits = c(-1, 1),
                scale = "none", main = title, xlab = "E matrices", ylab = "E matrices",
                Rowv = TRUE, Colv = TRUE, dendrogram = 'both',  branches_lwd = 0.3,
                margins = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
                fontsize_row = 8, fontsize_col = 8,
                showticklabels = FALSE, symm = TRUE)
  
  p %>% 
    layout(width = 800, height = 800) %>% 
    save_image(filename, scale = 3, resolution = 300)
  
  
  return(p)
  
}

plot_pval = function(matrix, title = NULL, filename = NULL) {
  
  title = paste0('P value\n', title)
  
  
  p = heatmaply(matrix, colors = YlOrRd(200), limits = c(0, 1),
                scale = "none", main = title, xlab = "E matrices", ylab = "E matrices",
                Rowv = TRUE, Colv = TRUE, dendrogram = 'both',  branches_lwd = 0.3,
                margins = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
                fontsize_row = 8, fontsize_col = 8,
                showticklabels = FALSE, symm = TRUE)
  
  p %>% 
    layout(width = 800, height = 800) %>% 
    save_image(filename, scale = 3, resolution = 300)
  
  
  return(p)
  
}
  
  
hist_color = function(met = NULL, n = -1, i = -1) {
  
  colors = c("purple", "red", "green3", "dodgerblue1")
  
  if (is.null(met) & n == 16) {colr = colors[ceiling(i/4)]}
  if (is.null(met) & n == 4) {colr = colors[i]}
  
  if (!is.null(met)) {
    if (met == "brunet") {colr = "purple"} 
    else if (met == "lee") {colr = "red"}
    else if (met == "snmf/l") {colr = "green3"}
    else if (met == "snmf/r") {colr = "dodgerblue1"}
  }
  
  return(colr)
}

  




