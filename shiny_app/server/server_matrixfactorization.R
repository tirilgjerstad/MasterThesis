
# -------- OUTPUT DESCRIPTIVE ANALYSIS --------


output$dataM = DT::renderDataTable({
  
  req(data_M())
  
  DT::datatable({
    data_M()
  },
  extensions = 'Buttons', 
  options = list(lengthMenu=list(c(5, 10, 15, 20, 100, -1),c('5', '10', '15','20', '100', 'all')),
                 pageLength=10, scrollX = TRUE, searching = FALSE,
                 dom = 'Bfrtip',
                 buttons = list(
                   list(
                     extend = 'copy',
                     text = '<i class="fa-solid fa-copy fa-xl"></i>',
                     titleAttr = 'Copy'
                   ),
                   list(
                     extend = 'excel',
                     text = '<i class="fa-solid fa-file-excel fa-xl"></i>',
                     titleAttr = 'download Excel',
                     filename = 'mut_matrix'
                     
                   ),
                   list(
                     extend = 'csv',
                     text = '<i class="fa-solid fa-file-csv fa-xl"></i>',
                     titleAttr = 'download CSV',
                     filename = 'mut_matrix'

                   ))))


  
}) #end dataM


output$raw_heatmap_M = renderPlotly({
  
  req(data_M())
  
  if (input$scale_M1) {scl_M = 'row'} else {scl_M = 'none'}
  
  if (input$norm_M) {data = data.Normalization(data_M(), type = 'n10', normalization = 'row')}
  else {data = data_M()}
  
  heatmaply(data, colors = Blues(200), 
            dendrogram = 'none',
            xlab = 'Mutation Categories', ylab = 'Sample IDs', main = 'Original Mutation Matrix',
            Rowv = NULL, Colv = NULL, scale = scl_M,
            
            label_names = c('ID', 'Mutation', 'Value'),
            fontsize_row = input$fs_row_m1, fontsize_col = input$fs_col_m1,
            dynamicTicks = TRUE
  )
  
}) # end NMF plot

output$cluster_heatmap_M = renderPlotly({

  req(data_M())
  
  
  #Cluster 
  if (input$dist_met == 'correlation') {
    d_col = as.dist(1 - cor(data_M()))
    d_row = as.dist(1 - cor(t(data_M())))
    } 
  else {
    d_col = dist(data_M(), input$dist_met)
    d_row = dist(data_M(), input$dist_met)
    
    }

  dend_col = hclust(d_col, method = input$clust_met)
  dend_row = hclust(d_row, method = input$clust_met)
  
  if (input$cluster_M == 'row') {dend_col = NULL}
  if (input$cluster_M == 'column') {dend_row = NULL}
  if (input$scale_M2) {scl_M = 'row'} else {scl_M = 'none'}
  
  data = data.Normalization(data_M(), type = 'n10', normalization = 'row')
  

  
  heatmaply(data, colors = Reds(200), 
            
            xlab = '\nMutation Categories', ylab = 'Sample IDs\n', main = 'Mutation Matrix (clustered)',
            scale = scl_M, Rowv = dend_row, Colv = dend_col, 
            dendrogram = input$cluster_M, show_dendrogram = input$dendro_m,
            label_names = c('ID', 'Mutation', 'Value'),
            branches_lwd = 0.3,
            fontsize_row = input$fs_row_m2, fontsize_col = input$fs_col_m2,
            showticklabels = c(input$tick_row_m, input$tick_col_m), k_row = input$k_groups
  )
  
}) # end NMF plot 


output$dataE = DT::renderDataTable({
  req(E_hat())
  
  DT::datatable({
    E_hat()
  },
  extensions = 'Buttons',
  options = list(lengthMenu=list(c(5, 10, 15, 20, 100, -1),c('5', '10', '15','20', '100', 'all')),
                 searching = FALSE, pageLength=10, scrollX = TRUE,
                 dom = 'Bfrtip',
                 buttons = list(
                   list(
                     extend = 'copy',
                     text = '<i class="fa-solid fa-copy fa-xl"></i>',
                     titleAttr = 'Copy'
                   ),
                   list(
                     extend = 'excel',
                     text = '<i class="fa-solid fa-file-excel fa-xl"></i>',
                     titleAttr = 'download Excel',
                     filename = 'E_hat'
                     
                   ),
                   list(
                     extend = 'csv',
                     text = '<i class="fa-solid fa-file-csv fa-xl"></i>',
                     titleAttr = 'download CSV',
                     filename = 'E_hat'
                     

                   )))
  
  )
}) #end E_hat


output$raw_heatmap_E = renderPlotly({

  req(E_hat())
  
  if (input$norm_E) {data = data.Normalization(E_hat(), type = 'n10', normalization = 'row')}
  else {data = E_hat()}
  
  heatmaply(t(data), colors = Purples(200),
            dendrogram = 'none',
            xlab = 'Sample IDs', ylab = 'Mutation Signatures', main = 'Exposure Matrix E',
            Rowv = NULL, Colv = NULL, scale = 'none',
            label_names = c('Signature', 'ID', 'Value'),
            fontsize_row = input$fs_row_e1, fontsize_col = input$fs_col_e1,
            dynamicTicks = TRUE
            
  )
  
}) # end NMF plot 

output$cluster_heatmap_E = renderPlotly({

  req(E_hat())
  
  #Cluster 
  if (input$dist_met == 'correlation') {
    d_col = as.dist(1 - cor(t(E_hat())))
    d_row = as.dist(1 - cor(E_hat()))
  } 
  else {
    d_col = dist(data_M(), input$dist_met)
    d_row = dist(data_M(), input$dist_met)
    
  }
  
  dend_col = hclust(d_col, method = input$clust_met)
  dend_row = hclust(d_row, method = input$clust_met)
  
  
  if (input$cluster_E == 'row') {dend_col = NULL}
  if (input$cluster_E == 'column') {dend_row = NULL}
  
  data = data.Normalization(E_hat(), type = 'n10', normalization = 'row')
  
  
  heatmaply(t(data), colors = Greens(200),
            
            xlab = '\nSample IDs', ylab = 'Mutation Signatures\n', main = 'Exposure Matrix E (clustered)',
            scale = 'none', 
            Rowv = dend_row, Colv = dend_col, dendrogram = input$cluster_E, show_dendrogram = input$dendro_e,
            label_names = c('Signature', 'ID', 'Value'),
            fontsize_row = input$fs_row_e2, fontsize_col = input$fs_col_e2,
            branches_lwd = 0.3,
            showticklabels = c(input$tick_row_e, input$tick_col_e), k_col = input$k_groups
            
            
  )
  
}) # end NMF plot 


output$NMFplot_S = renderPlotly({

  req(S_hat())
  
  if (input$norm_S) {data = data.Normalization(S_hat(), type = 'n10', normalization = 'row')}
  else {data = S_hat()}
  
  heatmaply(data, colors = Oranges(200),
            
            xlab = '\n Mutation Categories', ylab = 'Mutation Signatures \n', main = 'Signatures Matrix',
            scale = 'none', 
            Rowv = NULL, Colv = NULL, dendrogram = 'none',
            label_names = c('Sgnature', 'Mutation', 'Value'),
            fontsize_row = input$fs_row_s1, fontsize_col = input$fs_col_s1,
            dynamicTicks = TRUE
            
  )
  
  
}) # end NMF plot 

output$NMFplot_S2 = renderPlotly({

  req(S_hat())

  #Cluster 
  if (input$dist_met == 'correlation') {
    d_col = as.dist(1 - cor(S_hat()))
    d_row = as.dist(1 - cor(t(S_hat())))
  } 
  else {
    d_col = dist(data_M(), input$dist_met)
    d_row = dist(data_M(), input$dist_met)
    
  }
  
  dend_col = hclust(d_col, method = input$clust_met)
  dend_row = hclust(d_row, method = input$clust_met)
  
  if (input$cluster_S == 'row') {dend_col = NULL}
  if (input$cluster_S == 'column') {dend_row = NULL}
  
  data = data.Normalization(S_hat(), type = 'n10', normalization = 'row')
  
  heatmaply(data, colors = BuPu(200),
            
            xlab = ' \n Mutation Categories', ylab = 'Mutation Signatures \n', main = 'Signature Matrix (clustered)',
            Rowv = dend_row, Colv = dend_col, scale = 'none',
            dendrogram = input$cluster_S, branches_lwd = 0.3,
            show_dendrogram = input$dendro_s,
            label_names = c('Signature', 'Mutation', 'Value'),
            fontsize_row = input$fs_row_s2, fontsize_col = input$fs_col_s2,
            showticklabels = c(input$tick_row_s, input$tick_col_s)
            
            
  )
  
}) # end NMF plot 


output$dataS = DT::renderDataTable({
  
  req(S_hat())
  
  DT::datatable({
    S_hat()
  },
  extensions = 'Buttons',
  options = list(lengthMenu=list(c(5, 10, 15, 20, 100, -1),c('5', '10', '15','20', '100', 'all')),
                 pageLength=10, scrollX = TRUE, searching = FALSE,
                 dom = 'Bfrtip',
                 buttons = list(
                   list(
                     extend = 'copy',
                     text = '<i class="fa-solid fa-copy fa-xl"></i>',
                     titleAttr = 'Copy'
                   ),
                   list(
                     extend = 'excel',
                     text = '<i class="fa-solid fa-file-excel fa-xl"></i>',
                     titleAttr = 'download Excel',
                     filename = 'S_hat'
                   ),
                   list(
                     extend = 'csv',
                     text = '<i class="fa-solid fa-file-csv fa-xl"></i>',
                     titleAttr = 'download CSV',
                     filename = 'S_hat'
                     

                   )))
                 
  
  )
}) #end S_hat


nmf_cluster_data = reactive({
  req(E_hat())
  E = as.data.frame(E_hat())
  
  data = cbind(rownames(E), colnames(E)[max.col(E,ties.method="first")])
  colnames(data) = c('Ids', 'Signatures')
  data
})



reactive_ggplot = reactive({
  req(nmf_cluster_data())
  
  data = nmf_cluster_data()[,2]
  
  ggplot(data.frame(data), aes(x=data)) +
    geom_bar(fill = 'lightgreen', width = 0.5) + 
    ggtitle("NMF clustering based on E matrix") + 
    labs(y = 'Counted individuals', x = 'Mutation Signatures') + 
    theme(axis.title = element_text(size = 18),
          axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 16),
          plot.title = element_text(size = 20))
})


output$E_cluster_hist = renderPlot({
  reactive_ggplot()
})

output$download_hist = downloadHandler(
  filename = function() {
    "nmf_clustering_histogram.png"
  },
  content = function(file) {
    ggsave(file, plot = reactive_ggplot(), device = "png", height = 6, width = 10)
  }
)


output$nmf_cluster_table = renderTable({
  req(nmf_cluster_data())
  
  table(nmf_cluster_data()[,2], dnn = c('Signature'))
  
})

output$nmf_cluster_ids = renderTable ({
  req(nmf_cluster_data())
  nmf_cluster_data()
})


output$heatmap_nmf_clust = renderPlotly({
  
  req(E_hat())  
  
  data = data.Normalization(E_hat(), type = 'n10', normalization = 'row')
  
  for (i in 1:nrow(data)) {
    data[i, ] = ifelse(data[i,] == max(data[i,]), max(data[i,]), 0)
  }
  
  heatmaply(t(data), colors = c('white', 'red'),
            dendrogram = 'none',
            xlab = 'Sample IDs', ylab = 'Mutation Signatures', main = 'Exposure Matrix E',
            Rowv = NULL, Colv = NULL, scale = 'none',
            label_names = c('Signature', 'ID', 'Value'),
            fontsize_row = input$fs_row_e1, fontsize_col = input$fs_col_e1,
            dynamicTicks = TRUE
            
  )
  
}) # end NMF plot 


output$heatmap_nmf_clust2 = renderPlotly({
  

  req(E_hat())
  
  
  data = data.Normalization(E_hat(), type = 'n10', normalization = 'row')
  
  for (i in 1:nrow(data)) {
    data[i, ] = ifelse(data[i,] == max(data[i,]), max(data[i,]), 0)
  }
  
  heatmaply(t(data), colors = c('white', 'blue'),
            dendrogram = 'both',
            xlab = 'Sample IDs', ylab = 'Mutation Signatures', main = 'Exposure Matrix E (clustered)',
            Rowv = TRUE, Colv = TRUE, scale = 'none', 
            label_names = c('Signature', 'ID', 'Value'),
            fontsize_row = input$fs_row_e1, fontsize_col = input$fs_col_e1,
            branches_lwd = 0.3,
            dynamicTicks = FALSE
            
  )
  
}) # end NMF plot 