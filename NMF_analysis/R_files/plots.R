


# ---------- Plot functions ---------- #


plot_subset_100_75 = function(df) {
  
  ggplot(df, aes(x = rev(x), y = median, color = methods, group = methods)) +
    geom_line(linewidth = 0.85) +
    geom_point() + 
    scale_color_manual(values = c("purple", "red", "green3", "dodgerblue1"))  +
    scale_x_continuous(
      limits = c(0.75, 1),  
      breaks = seq(1, 0.75, by = -0.05), 
      labels = rev(seq(1, 0.75, by = -0.05))  
    ) +
    scale_y_continuous(
      limits = c(0.75,1),
      breaks = seq(0.75, 1, by = 0.05),
      labels = seq(0.75, 1, by = 0.05)
    ) +
    geom_hline(yintercept = 0.75, linetype = 'dotted', linewidth = 0.9) + 
    xlab('Subset value (porportion of dataset)') +
    ylab("Median Similarity Score") +
    #theme(legend.position = c(0.865, 0.87),
    theme(legend.position = c(0.15, 0.21),
      axis.title = element_text(size = 22),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20,),
      legend.text = element_text(size = 14), 
      legend.title = element_blank()) 
  
}


plot_median = function(df) {
  
  colors = c("purple", "red", "green3", "dodgerblue1")
  
  
  p = ggplot(df, aes(x = rev(x), y = median)) +
    geom_line(aes(color = methods)) +
    geom_point(aes(color = methods)) +
    scale_color_manual(values = colors) + 
    scale_x_continuous(
      limits = c(0.75, 1),  
      breaks = seq(1, 0.75, by = -0.05), 
      labels = rev(seq(1, 0.75, by = -0.05))  
    ) +
    scale_y_continuous(
      limits = c(0.74,1),
      breaks = seq(0.70, 1, by = 0.05),
      labels = seq(0.70, 1, by = 0.05)
    ) +
    geom_ribbon(aes(ymin = q1, ymax = q3, fill = methods), alpha = 0.15) +
    scale_fill_manual(values = colors) + 
    geom_hline(yintercept = 0.75, linetype = 'dotted', linewidth = 0.75) +
    labs(x = 'Subset value (porportion of dataset)', y = 'Median Similarity Score') + 
    theme(legend.position = 'none',
          axis.title = element_text(size = 24),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          strip.text = element_text(size = 24)) + 
    facet_wrap(~ methods, ncol = 4) 
  return(p)  
  
}


plot_4x4_grid = function(scores, row_names, col_names) {

  colors = c("purple", "red", "green3", "dodgerblue1")
  
  
  # Create a dataframe from the scores
  df <- data.frame(
    values = unlist(scores),
    grid_row = rep(row_names, each = 19800),
    grid_col = rep(col_names, each = 4950)
  )
  
  # List of medians for each group
  df_median = df %>%
    group_by(grid_row, grid_col) %>%
    summarise(m = median(values))
  
  
  p = ggplot(df, aes(x = values, fill = grid_row)) + 
    geom_histogram(color = 'white', bins = 30, 
                   aes(y = after_stat(count / sum(count/16))),
                   boundary = 0, closed = "left") + 
    scale_fill_manual(values = colors) + 
    xlim(0, 1.05) + 
    ylim(0,1) +
    labs(x = 'Similarity Score', y = 'fraction') + 
    facet_grid(grid_row ~ factor(grid_col, labels = col_names)) + 
    theme(legend.position = 'none',
          axis.title = element_text(size = 18),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          strip.text = element_text(size = 16)) + 
    geom_vline(xintercept = 0.75, linetype = 'dotted', linewidth = 0.9) + 
    geom_vline(data = df_median, aes(xintercept = m, color = grid_row),  linetype = 'dotted', linewidth = 0.9) + 
    scale_color_manual(values = colors)
  
  
  return(p)
  
  
}


plot_2x2_grid = function(scores, titles, ncol = 2) {
  
  colors = c("purple", "red", "green3", "dodgerblue1")
  
  df = data.frame(
    values = unlist(scores),
    grid_titles = rep(titles, each = 4950)
  )
  
  df_median = df %>%
    group_by(grid_titles) %>%
    summarise(m = median(values))
  
  
  
  p = ggplot(df, aes(x = values, fill = grid_titles)) + 
    geom_histogram(color = 'white', bins = 30, 
                   aes(y = after_stat(count / sum(count/4))),
                   boundary = 0, closed = "left") + 
    scale_fill_manual(values = colors) + 
    xlim(0, 1.05) + 
    ylim(0,1) +
    labs(x = 'Similarity Score', y = 'fraction') + 
    facet_wrap(~ grid_titles, ncol = ncol) +
    theme(legend.position = 'none',
          axis.title = element_text(size = 24),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          strip.text = element_text(size = 22)) +
    geom_vline(xintercept = 0.75, linetype = 'dotted', linewidth = 0.9) + 
    geom_vline(data = df_median, aes(xintercept = m, color = grid_titles),  linetype = 'dotted', linewidth = 0.9) + 
    scale_color_manual(values = colors)
  
  return(p)
}


plot_pval_seed_perm = function(scores, nmf_methods) {
  colors = c("purple", "red", "green3", "dodgerblue1")
  
  df = data.frame(
    group = rep(nmf_methods, each = 4950),
    value = unlist(scores))
  
  
  ggplot(df, aes(x = seq_along(value), y = -log10(value), color = group)) +
    geom_point(size = 0.5) +
    geom_hline(yintercept = -log10(0.01), linetype = 'dotted', linewidth = 0.75) +
    xlab("Methods") +
    ylab("-log10(p-value)") +
    scale_color_manual(values = colors) +
    scale_y_continuous(
      breaks = c(0,50,100,150,200),
      limits = c(0,210)
      #labels = scientific
    ) +
    scale_x_continuous(breaks = c(4950*0.5, 4950*1.5, 4950*2.5, 4950*3.5),
                       labels = nmf_methods) + 
    theme(legend.position = 'none',
          axis.title = element_text(size = 18),
          axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 16)) 
}


plot_ari_perm_seed = function(scores, nmf_methods) {
  
  colors = c("purple", "red", "green3", "dodgerblue1")
  
  
  df = data.frame(
    group = rep(nmf_methods, each = 4950),
    value = unlist(scores)
  )
  
  
  p = ggplot(df, aes(x = group, y = value, fill = group)) + 
    geom_boxplot() + 
    scale_fill_manual(values = colors) + 
    labs(x ='Methods', y = 'Adjusted Rand Index') + 
    theme(legend.position = 'none',
          axis.title = element_text(size = 18),
          axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 16)
    )
  
  return(p)
  
  
}


plot_pval_noise = function(scores, nmf_methods) {
  # Using the rainbow() function
  colors  = rep(c("purple", "red", "green3", "dodgerblue1", 
                  "purple4", "red4", "green4", "dodgerblue4"), 
                times = 2)
  
  
  
  df = data.frame(
    method = rep(paste('Method:', nmf_methods), each = 19800),
    noise = rep(c(1.5,2,4,8), each = 4950),
    value = unlist(scores)
  )
  
  # the one line p valies 
  p = ggplot(df, aes(x = seq_along(value), y = -log10(value), color = interaction(method, factor(noise)))) + 
    geom_point(size = 0.3) +
    geom_hline(yintercept = -log10(0.01), linetype = 'dotted', linewidth = 0.75) +
    labs(x = "Noise", y = "-log10(p-value)") + 
    facet_wrap(~ method, scales = 'free_x',ncol = 4) +
    scale_color_manual(values = colors) + 
    scale_x_continuous(breaks = seq(2475, 79200, 4950),
                       labels = factor(rep(c(1.5, 2, 4, 8), times = 4))) + 
    scale_y_continuous(
      breaks = c(0,20,40,60,80),
      limits = c(0,85)
      #labels = scientific
    ) +
    theme(legend.position = 'none',
          axis.title = element_text(size = 24),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          strip.text = element_text(size = 22))
  
  
  return(p)
  
}


plot_ari_noise = function(scores, nmf_methods) {
  
  colors = c("purple", "red", "green3", "dodgerblue1")
  
  df = data.frame(
    method = rep(paste('Method:',nmf_methods), each = 19800),
    noise = rep(c(1.5,2,4,8), each = 4950),
    value = unlist(scores)
  )
  
  
  ari_noise = ggplot(df, aes(x = as.factor(noise), y = value, fill = method)) + 
    geom_boxplot(outlier.shape = 1, fatten = 1, outlier.size = 1.5) + 
    facet_wrap(~ method, ncol = 4) + 
    scale_fill_manual(values = colors) + 
    labs(x = "Noise", y = "ARI") + 
    ylim(-0.15, 1) + 
    theme(legend.position = 'none',
          axis.title = element_text(size = 24),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          strip.text = element_text(size = 22))
  
  
  return(ari_noise)
  
}



# ---------- Create and save plots ---------- #

plot_4x4 = function(nmf_methods, grid_col, file_path, file_save) {
  
  all_files = list.files(file_path, full.names = TRUE)

  scores = list()
  
  for (file in all_files) {
    vals = read.table(file, header = TRUE)
    scores = append(scores, vals)
  }
  grid_row = paste('Method:',nmf_methods)

  p = plot_4x4_grid(scores, grid_row, grid_col)
  ggsave(file_save, bg = 'white', height = 7.8, width = 12)
  return(p)
  
}

plot_2x2 = function(nmf_methods, file_path, file_save) {
  scores = list()
  
  for (file in list.files(file_path, full.names = TRUE)) {
    scores = append(scores, read.table(file, header = TRUE))
  }
  
  titles = paste('Method:',nmf_methods) 
  
  p = plot_2x2_grid(scores, titles)
  ggsave(file_save, bg = 'white', height = 7.8, width = 12)
  return(p)
  
}


plot_median_subset = function(nmf_methods, file_path, file_save) {
  df = create_median_df(nmf_methods, file_path)
  p1 = plot_subset_100_75(df)
  p2 = plot_median(df)
  g1 = plot_grid(NULL, p1, NULL, ncol = 3, rel_widths = c(1,4,1))
  g2 = plot_grid(NULL, p2, ncol = 1, rel_heights= c(1, 5))
  g3 = plot_grid(g1, g2, labels = 'auto', ncol = 1, rel_heights = c(3,2))
  ggsave(file_save, bg = 'white', height = 8, width = 12)
  return(g3)
}

plot_median_subset2 = function(nmf_methods, file_path, file_save) {
  df = create_median_df(nmf_methods, file_path)
  p = plot_median(df)
  #ggsave(file_save, bg = 'white', height = 8, width = 12)
  return(p)
}


plot_cluster_PS = function(nmf_methods, file_path, file_save) {
  
  all_files = list.files(file_path, full.names = TRUE)
  # Plot ARI values 
  scores = list()
  ari_files = grep('ari', all_files, value = TRUE)
  
  for (file in ari_files) {
    
    score_table = read.table(file, header = TRUE)
    score_list = score_table[lower.tri(score_table)]
    scores = append(scores, list(score_list))
  }
  ari = plot_ari_perm_seed(scores, nmf_methods)
  
  # PLot P values 
  pval_files = grep('pval', all_files, value = TRUE)
  scores = list()
  
  for (file in pval_files) {
    
    score_table = read.table(file, header = TRUE)
    score_list = score_table[lower.tri(score_table)]
    score_list[score_list > 0.05] = 0.05
    scores = append(scores, list(sort(score_list)))
  }
  
  pval = plot_pval_seed_perm(scores, nmf_methods)
  p = plot_grid(ari, pval, labels = c('a', 'b'), ncol = 2, rel_widths = c(2, 3), label_size = 18)
  ggsave(file_save, bg = 'white', height = 4, width = 12)
  return(p)
  
}


plot_cluster_noise = function(nmf_methods, file_path, file_save) {
  
  all_files = list.files(file_path, full.names = TRUE)
  ari_files = grep('ari', all_files, value = TRUE)
  # Plot ARI values 
  scores = list()
  for (file in ari_files) {
    
    score_table = read.table(file, header = TRUE)
    score_list = score_table[lower.tri(score_table)]
    scores = append(scores, list(score_list))
  }
  
  ari = plot_ari_noise(scores, nmf_methods)
  
  # Plot p values 
  pval_files = grep('pval', all_files, value = TRUE)
  
  scores = list()
  
  for (file in pval_files) {
    score_table = read.table(file, header = TRUE)
    score_list = score_table[lower.tri(score_table)]
    score_list[score_list > 0.05] = 0.05
    scores = append(scores, list(sort(score_list)))
  }
  
  pval = plot_pval_noise(scores, nmf_methods)
  p = plot_grid(ari, pval, ncol = 1, labels = c('a', 'b'), label_size = 18)
  ggsave(file_save, bg = 'white', height = 8, width = 16)
  return(p)
  
}
