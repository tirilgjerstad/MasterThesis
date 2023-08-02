source('functions.R')
source('analysis_functions.R')
source('plots.R')

# Path to the datasets 
path_TCGA_M = '../matrix_files/TCGA_M.txt'
path_TCGA_M_subset50 = '../matrix_files/TCGA_M_subset50.txt'
path_hartwig_M = '../matrix_files/hartwig_skin_matrix.csv'
path_hartwig_M_subsample002 = '../matrix_files/subsample_hartwig_002.csv'


TCGA_M = read_M(path_TCGA_M)
TCGA_M_subset50 = read_M(path_TCGA_M_subset50)
TCGA_M_outlier = TCGA_M[which(rowSums(TCGA_M) < 500), ]

hartwig_M = read_hartwig_M(path_hartwig_M)
hartwig_M_subsample002 = read_hartwig_M(path_hartwig_M_subsample002)
hartwig_M_outlier = hartwig_M[which(rowSums(hartwig_M) < 500000), ]


nmf_methods = c('brunet', 'lee', 'snmf/l', 'snmf/r')



# Run the analysis: 
NMF_analysis = function(M) {
  
  # M is the mutation matrix to use in the analysis.
  
  # Use some standard choices
  n_sign = 5
  n_clust = 2
  
  dist_method = 'correlation'
  clust_method = 'complete'
  
  subset_values = c(0.25,0.50, seq(0.75, 1.0, 0.01))
  noise_values = c(1, 1.5, 2, 4, 8)
  n_itr = 100
  
  analyse_seed_ES(M, nmf_methods, n_sign, n_itr, clust_method, dist_method, n_clust)
  
  analyse_permutations_ES(M, nmf_methods, n_sign, n_itr, clust_method, dist_method, n_clust)
  
  analyse_subset_S(M, nmf_methods, n_sign, n_itr, subset_values)

  analyse_noise_ES(M, nmf_methods, n_sign, n_itr, noise_values, clust_method, dist_method, n_clust)
  
}

#NMF_analysis(TCGA_M)
#NMF_analysis(TCGA_M_outlier)
#NMF_analysis(TCGA_M_subset50)
#NMF_analysis(hartwig_M)
#NMF_analysis(hartwig_M_outlier)
#NMF_analysis(hartwig_M_subsample002)







# ---------------------------------------------------------------------- #



run_test_random_cos_sim = function() {
  test_random_cos_sim(100000)
}

create_example_cos_m = function() {
  ES_matrices = create_ES_matrices_seed(TCGA_M, 'brunet', 5, 10)
  S_matrices = ES_matrices[[2]]
  cosine_matrices = create_cosine_matrices(S_matrices)
  cos_M = cosine_matrices[[10]]
  
  a = calc_best_score(cos_M)
  print(a)
  
  hm = heatmaply(cos_M, colors = Blues(200),
                 xlab = 'Signature S<sub>i</sub>', ylab = 'Signatures S<sub>j</sub>',
                 scale = 'none', cellnote = cos_M, cellnote_textposition = 'middle center',
                 cellnote_size = 16,
                 colorbar_title_side = list(font = list(size = 20)),
                 margins = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
                 Rowv = NULL, Colv = NULL, dendrogram = 'none',
                 label_names = c('nmf_met1', 'nmf_met2', 'Similarity'),
                 fontsize_row = 12, fontsize_col = 12)
  hm
  
  return(hm)
  
  
  
}




# ---------------------------------------------------------------------- #


plot_mutation_counts = function() {
  plot_mutation_count_hartwig()
  plot_mutation_count_TCGA()
}

TCGA_plotting = function() {
  # Signature Subset
  plot_4x4(nmf_methods, paste('Subset:', seq(1.0, 0.25, -0.25)),
           'results_TCGA/signature/subset_all',
           'results_plot_TCGA/signature/subset_all.png')
  # Signature Noise
  plot_4x4(nmf_methods, paste('Noise:', c(0,2,4,8)),
           'results_TCGA/signature/noise_all',
           'results_plot_TCGA/signature/noise_all.png')
  # Signature Seed 
  plot_2x2(nmf_methods,
           'results_TCGA/signature/seed',
           'results_plot_TCGA/signature/seed.png')
  
  # Signature Permutation
  plot_2x2(nmf_methods,
           'results_TCGA/signature/permutation',
           'results_plot_TCGA/signature/permutation.png')
  
  # Signature Subset median values 
  plot_median_subset(nmf_methods,
                     'results_TCGA/signature/subset_75_100',
                     'results_plot_TCGA/signature/subset_75_100.png')
  
  # Cluster Seed 
  plot_cluster_PS(nmf_methods,
                  'results_TCGA/cluster/seed',
                  'results_plot_TCGA/cluster/seed_ari_pval.png')
  
  # Cluster Permutation 
  plot_cluster_PS(nmf_methods,
                  'results_TCGA/cluster/permutation',
                  'results_plot_TCGA/cluster/perm_ari_pval.png')
  
  
  # Cluster Noise 
  plot_cluster_noise(nmf_methods,
                     'results_TCGA/cluster/noise_all',
                     'results_plot_TCGA/cluster/noise_ari_pval.png')
  
}

hartwig_plotting = function() {
  # Signature Subset
  plot_4x4(nmf_methods, paste('Subset:', seq(1.0, 0.25, -0.25)),
           'results_hartwig/signature/subset_all',
           'results_plot_hartwig/signature/subset_all.png')
  # Signature Noise
  plot_4x4(nmf_methods, paste('Noise:', c(0,2,4,8)),
           'results_hartwig/signature/noise_all',
           'results_plot_hartwig/signature/noise_all.png')
  # Signature Seed 
  plot_2x2(nmf_methods,
           'results_hartwig/signature/seed',
           'results_plot_hartwig/signature/seed.png')
  
  # Signature Permutation
  plot_2x2(nmf_methods,
           'results_hartwig/signature/permutation',
           'results_plot_hartwig/signature/permutation.png')
  
  # Signature Subset median values 
  plot_median_subset(nmf_methods,
                     'results_hartwig/signature/subset_75_100',
                     'results_plot_hartwig/signature/subset_75_100.png')
  
  # Cluster Seed 
  plot_cluster_PS(nmf_methods,
                  'results_hartwig/cluster/seed',
                  'results_plot_hartwig/cluster/seed_ari_pval.png')
  
  # Cluster Permutation 
  plot_cluster_PS(nmf_methods,
                  'results_hartwig/cluster/permutation',
                  'results_plot_hartwig/cluster/perm_ari_pval.png')
  
  
  # Cluster Noise 
  plot_cluster_noise(nmf_methods,
                  'results_hartwig/cluster/noise_all',
                  'results_plot_hartwig/cluster/noise_ari_pval.png')
  
}

hartwig_sub_plotting = function() {
  # Signature Subset
  plot_4x4(nmf_methods, paste('Subset:', seq(1.0, 0.25, -0.25)),
           '../results_files/results_hartwig_subsample/signature/subset_all',
           '../results_plot/results_hartwig_subsample/signature/subset_all.png')
           
  # Signature Noise
  plot_4x4(nmf_methods, paste('Noise:', c(0,2,4,8)),
           '../results_files/results_hartwig_subsample/signature/noise_all',
           '../results_plot/results_hartwig_subsample/signature/noise_all.png')
  # Signature Seed 
  p1 = plot_2x2(nmf_methods,
           '../results_files/results_hartwig_subsample/signature/seed',
           '../results_plot/results_hartwig_subsample/signature/seed.png')

  
  # Signature Permutation
  p2 = plot_2x2(nmf_methods,
           '../results_files/results_hartwig_subsample/signature/permutation',
           '../results_plot/results_hartwig_subsample/signature/permutation.png')
  
  p = plot_grid(p1, p2, ncol = 2, labels = c('a', 'b'), label_size = 18)
  ggsave('../results_plot/results_hartwig_subsample/signature/seed_perm.png', bg = 'white', height = 6, width = 16)
  
  # Signature Subset median values 
  plot_median_subset(nmf_methods,
                     '../results_files/results_hartwig_subsample/signature/subset_75_100',
                     '../results_plot/results_hartwig_subsample/signature/subset_75_100.png')
  


  
}

hartwig_sub_plotting()


seed_plotting = function() {
  
  
  p1 = plot_2x2(nmf_methods,
                '../results_files/results_tcga/signature/seed',
                '../results_plot/results_tcga/signature/seed.png')
  
  
  
  p2 = plot_2x2(nmf_methods,
                '../results_files/results_tcga_outlier/signature/seed',
                '../results_plot/results_tcga_outlier/signature/seed.png')
  
  p3 = plot_2x2(nmf_methods,
                '../results_files/results_tcga_subset/signature/seed',
                '../results_plot/results_tcga_subset/signature/seed.png')
  
  
  p4 = plot_2x2(nmf_methods,
                '../results_files/results_hartwig/signature/seed',
                '../results_plot/results_hartwig/signature/seed.png')
  
  
  
  p5 = plot_2x2(nmf_methods,
                '../results_files/results_hartwig_outlier/signature/seed',
                '../results_plot/results_hartwig_outlier/signature/seed.png')
  
  
  p6 = plot_2x2(nmf_methods,
                '../results_files/results_hartwig_subsample/signature/seed',
                '../results_plot/results_hartwig_subsample/signature/seed.png')
  

  

  p = plot_grid(p1, p4, ncol = 2, labels = 'auto', label_size = 18)
  ggsave('seed_both.png', bg = 'white', height = 6, width = 16)
  
  p = plot_grid(p3, p6, ncol = 2, labels = 'auto', label_size = 18)
  ggsave('hss_seed.png', bg = 'white', height = 6, width = 16)
  
  p = plot_grid(p1, p2, p3,  p4, p5, p6, ncol = 3, labels = 'auto', label_size = 18)
  ggsave('../results_plot/seed_all.png', bg = 'white', height = 12, width = 24)
  
  
}

perm_plotting = function() {
  
  
  p1 = plot_2x2(nmf_methods,
                '../results_files/results_tcga/signature/permutation',
                '../results_plot/results_tcga/signature/permutation.png')
  
  
  
  p2 = plot_2x2(nmf_methods,
                '../results_files/results_tcga_outlier/signature/permutation',
                '../results_plot/results_tcga_outlier/signature/permutation.png')
  
  p3 = plot_2x2(nmf_methods,
                '../results_files/results_tcga_subset/signature/permutation',
                '../results_plot/results_tcga_subset/signature/permutation.png')
  
  
  p4 = plot_2x2(nmf_methods,
                '../results_files/results_hartwig/signature/permutation',
                '../results_plot/results_hartwig/signature/permutation.png')
  
  
  
  p5 = plot_2x2(nmf_methods,
                '../results_files/results_hartwig_outlier/signature/permutation',
                '../results_plot/results_hartwig_outlier/signature/permutation.png')
  
  
  p6 = plot_2x2(nmf_methods,
                '../results_files/results_hartwig_subsample/signature/permutation',
                '../results_plot/results_hartwig_subsample/signature/permutation.png')
  
  
  
  
  
  p = plot_grid(p1, p2, p3,  p4, p5, p6, ncol = 3, labels = 'auto', label_size = 18)
  ggsave('../results_plot/perm_all.png', bg = 'white', height = 12, width = 24)
  
  plot_grid(p1, p4, ncol = 2, labels = 'auto', label_size = 18)
  ggsave('permutation_both.png', bg = 'white', height = 6, width = 16)
  
  
}

subset_plotting = function() {

  
  p1 = plot_median_subset2(nmf_methods,
                         '../results_files/results_tcga/signature/subset_75_100',
                         '../results_plot/results_tcga/signature/subset_75_100.png')
  
  p2 = plot_median_subset2(nmf_methods,
                           '../results_files/results_tcga_outlier/signature/subset_75_100',
                           '../results_plot/results_tcga_outlier/signature/subset_75_100.png')
  
  p3 = plot_median_subset2(nmf_methods,
                          '../results_files/results_tcga_subset/signature/subset_75_100',
                          '../results_plot/results_tcga_subset/signature/subset_75_100.png')
  p4 = plot_median_subset2(nmf_methods,
                          '../results_files/results_hartwig/signature/subset_75_100',
                          '../results_plot/results_hartwig/signature/subset_75_100.png')
  p5 = plot_median_subset2(nmf_methods,
                          '../results_files/results_hartwig_outlier/signature/subset_75_100',
                          '../results_plot/results_hartwig_outlier/signature/subset_75_100.png')
  p6 = plot_median_subset2(nmf_methods,
                          '../results_files/results_hartwig_subsample/signature/subset_75_100',
                          '../results_plot/results_hartwig_subsample/signature/subset_75_100.png')
  
  plot_grid(p1, p2, p3,  p4, p5, p6, ncol = 1, labels = 'auto', label_size = 18)
  ggsave('../results_plot/subset_all.png', bg = 'white', height = 22, width = 22)

  
  plot_grid(p3, p6, ncol = 1, labels = 'auto', label_size = 18)
  ggsave('../results_plot/subset_all_htsub.png', bg = 'white', height = 8, width = 18)
  
  plot_4x4(nmf_methods, paste0('Subset: ', seq(100, 25, -25), '%'),
           '../results_files/results_TCGA/signature/subset_all',
           '../results_files/results_plot_TCGA/signature/subset_all.png')
  

  
}

noise_plotting = function(){
  

  p1 = plot_4x4(nmf_methods, paste('Noise: \u03B1 =', c(0,2,4,8)),
           '../results_files/results_tcga/signature/noise_all',
           '../results_plot/test_noise1.png')
  
  p2 = plot_4x4(nmf_methods, paste('Noise: \u03B1 =', c(0,2,4,8)),
                '../results_files/results_tcga_outlier/signature/noise_all',
                '../results_plot/test_noise2.png')
  
  p3 = plot_4x4(nmf_methods, paste('Noise: \u03B1 =', c(0,2,4,8)),
                '../results_files/results_tcga_subset/signature/noise_all',
                '../results_plot/test_noise3.png')
  
  p4 = plot_4x4(nmf_methods, paste('Noise: \u03B1 =', c(0,2,4,8)),
                '../results_files/results_hartwig/signature/noise_all',
                '../results_plot/test_noise4.png')
  
  p5 = plot_4x4(nmf_methods, paste('Noise: \u03B1 =', c(0,2,4,8)),
                '../results_files/results_hartwig_outlier/signature/noise_all',
                '../results_plot/test_noise5.png')
  
  p6 = plot_4x4(nmf_methods, paste('Noise: \u03B1 =', c(0,2,4,8)),
                '../results_files/results_hartwig_subsample/signature/noise_all',
                '../results_plot/test_noise6.png')
  
  
  
  all_files = list.files('../results_files/results_hartwig/signature/noise_all', full.names = TRUE)
  noise_files = grep('8', all_files, value = TRUE)
  #noise_files
  
  scores = list()
  
  for (file in noise_files) {
    scores = append(scores, read.table(file, header = TRUE))
  }
  
  titles = paste('Method:',nmf_methods) 
  
  p = plot_2x2_grid(scores, titles, ncol = 4)
  p
  ggsave('noise_8.png', bg = 'white', height = 4, width = 16)
  

  
  
}

more_plots = function() {



  
  all_files = list.files('../results_files/results_hartwig/signature/subset_all', full.names = TRUE)
  subset_files = grep('lee', all_files, value = TRUE)
  subset_files
  
  scores = list()
  
  for (file in subset_files) {
    scores = append(scores, read.table(file, header = TRUE))
  }
  
  col_names = paste0('Subset: ',seq(100, 25, -25), '%') 
  
  # Create a dataframe from the scores
  df <- data.frame(
    values = unlist(scores),
    grid_col = rep(col_names, each = 4950)
  )
  
  # List of medians for each group
  df_median = df %>%
    group_by(grid_col) %>%
    summarise(m = median(values))
  
  
  p = ggplot(df, aes(x = values, fill = "red")) + 
    geom_histogram(color = 'white', bins = 30, 
                   aes(y = after_stat(count / sum(count/4))),
                   boundary = 0, closed = "left") + 
    scale_fill_manual(values = c("red")) + 
    xlim(0, 1.05) + 
    ylim(0,1) +
    labs(x = 'Similarity Score', y = 'fraction') + 
    facet_grid( ~ factor(grid_col, levels = col_names)) +
    theme(legend.position = 'none',
          axis.title = element_text(size = 24),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          strip.text = element_text(size = 22)) + 
    geom_vline(xintercept = 0.75, linetype = 'dotted', size = 0.9) + 
    geom_vline(data = df_median, aes(xintercept = m, color = "red"),  linetype = 'dotted', linewidth = 0.9)  
  #scale_color_manual(values = colors)
  p
  ggsave('subset_lee.png', bg = 'white', height = 4, width = 16)
  
  source('plots.R')
  
  df_TCGA = create_median_df(nmf_methods, '../results_files/results_TCGA/signature/subset_75_100')
  p1_TCGA = plot_subset_100_75(df_TCGA)
  p2_TCGA = plot_median(df_TCGA)
  
  df_hartwig = create_median_df(nmf_methods, '../results_files/results_hartwig/signature/subset_75_100')
  p1_hartwig = plot_subset_100_75(df_hartwig)
  p2_hartwig = plot_median(df_hartwig)
  
  g1 = plot_grid(p1_TCGA, p1_hartwig, ncol = 2, labels = 'auto', label_size = 18)
  g2 = plot_grid(NULL, p2_TCGA, ncol = 1, rel_heights= c(1, 5))
  g3 = plot_grid(NULL, p2_hartwig, ncol = 1, rel_heights= c(1, 5))
  
  g4 = plot_grid(g1, g2, g3, labels = c('a', 'c', 'd'), ncol = 1, rel_heights = c(3,2,2), label_size = 18)
  g4
  ggsave('subset_median_both.png', bg = 'white', height = 12, width = 15)
  
  
  
  p1 = plot_cluster_PS(nmf_methods,
                       '../results_files/results_TCGA/cluster/seed',
                       '../results_plot/results_plot_TCGA/cluster/seed_ari_pval.png')
  
  
  p2 = plot_cluster_PS(nmf_methods,
                       '../results_files/results_hartwig/cluster/seed',
                       '../results_plot/results_plot_hartwig/cluster/seed_ari_pval.png')
  
  
  p = plot_grid(p1, p2, ncol = 1, rel_widths = c(2, 3))
  p
  ggsave('seed_cluster_both.png', bg = 'white', height = 8, width = 12)
  
  
  p1 = plot_cluster_PS(nmf_methods,
                       '../results_files/results_TCGA/cluster/permutation',
                       '../results_plot/results_plot_TCGA/cluster/seed_ari_pval.png')
  
  
  p2 = plot_cluster_PS(nmf_methods,
                       '../results_files/results_hartwig/cluster/permutation',
                       '../results_plot/results_plot_hartwig/cluster/seed_ari_pval.png')
  
  
  p = plot_grid(p1, p2, ncol = 1, rel_widths = c(2, 3))
  p
  ggsave('perm_cluster_both.png', bg = 'white', height = 8, width = 12)
  
  
  # Noise both 
  source('plots.R')
  
  p1 = plot_cluster_noise(nmf_methods,
                          '../results_files/results_TCGA/cluster/noise_all',
                          '../results_files/results_plot_TCGA/cluster/noise_ari_pval.png')
  
  
  
  p2 = plot_cluster_noise(nmf_methods,
                          '../results_files/results_hartwig/cluster/noise_all',
                          '../results_files/results_plot_hartwig/cluster/noise_ari_pval.png')
  
  
  
  plot_grid(p1, p2, ncol = 1)
  ggsave('noise_both.png', bg = 'white', height = 14, width = 16)
  
  
  
  
  
}




