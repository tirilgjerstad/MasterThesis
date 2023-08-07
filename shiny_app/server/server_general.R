


url = a("GitHub repository.", href="https://github.com/tirilgjerstad/MasterThesis/tree/main/shiny_app", target="_blank")
output$tab = renderUI({
  tagList("To access the application's source code, please visit our", url)
})



# -------------- THE DATA TABLES --------------


# The uploaded datasets (if upload)
input_M = reactive({

  req(input$mutation_data)
  data = input$mutation_data
  ext = tools::file_ext(data$datapath)
  
  req(ext %in% c("csv", "txt", "xlsx"))

  
  
  
  if (endsWith(data$datapath, '.xlsx')) {
    
    M = read.xlsx(data$datapath, colNames = TRUE, rowNames = TRUE, sep.names = input$sep_M)
  }
  
  else if (endsWith(data$datapath, '.txt')){
    M = read.table(data$datapath, header = TRUE, row.names = 1, sep = input$sep_M)
  }
  
  else if (endsWith(data$datapath, '.csv')){
    M = read.csv(data$datapath, header = TRUE, row.names = 1, sep = input$sep_M)
  }
  
  else {return(NULL)}
  
  if (input$row_col_M == 'Row') {
    M = t(M)
  }
  names(M) = scan(data$datapath, what = 'character', sep = "", nlines = 1)
  M = replace(M, is.na(M), 0)
  M = M[rowSums(M[]) > 0,]
  
  
  
  as.matrix(M) 
})

input_clinical = reactive({
 
  req(input$clinical_data)
  data = input$clinical_data
  ext = tools::file_ext(data$datapath)
  req(ext %in% c("csv", "txt", "xlsx"))
  

  cn = TRUE
  rn = FALSE 
  row = NULL
  
  
  if (input$row_col_C == 'Row') {
    cn = FALSE
    rn = TRUE
    row = 1
  }
  
  if(endsWith(data$datapath, '.txt')) {
    clinical = read.table(data$datapath, header = TRUE, row.names = row, sep = input$sep_C)
    
  } 
  else if (endsWith(data$datapath, '.xlsx')) {
    
    clinical = read.xlsx(data$datapath, colNames = cn, rowNames = rn)
  }
  
  if (rn) {clinical = t(clinical)}
  clinical
  
  
  
})

# The random generated datasets
random_M = reactive({
  as.matrix(random_generated_M)
})

random_clinical = reactive({
  random_generated_clinical
})

# Subset 
ss_clinical = reactive({
  
 req(input$subset_cols)
 req(input$subset_rows)
 req(data_clinical())
  
  data_clinical() %>% 
    dplyr::select(any_of(input$subset_cols)) %>% 
    dplyr::slice_head(n = input$subset_rows)
  
})

# The datasets to use. Either the uploaded or the random data 
data_M = reactive({
  
  data = NULL
  
  
  if ((input$choose_upload == 'upload') && (input$choose_subset_M == 'full')) {
    if (is.null(input$mutation_data)) {data = NULL}
    else {data = input_M()}
  }
  
  else if ((input$choose_upload == 'random') && (input$choose_subset_M == 'full')) {
    data = random_M()
  }
  
  else if ((input$choose_upload == 'upload') && (input$choose_subset_M == 'subset')) {
    
    req(input$subset_rows_M)
    req(input_M())
    
    data = as.matrix(as.data.frame(input_M()) %>% 
                       dplyr::slice_head(n = input$subset_rows_M))
                     
  }
  
  else if ((input$choose_upload == 'random') && (input$choose_subset_M == 'subset')) {
    
    req(input$subset_rows_M)
    
    data = as.matrix(as.data.frame(random_M()) %>% 
                       dplyr::slice_head(n = input$subset_rows_M))
                     
  }

  return(data)
})

data_clinical = reactive({
  
  if ((input$choose_upload == 'upload') && (input$choose_subset == 'full')) {
    if (is.null(input$clinical_data)) {data = NULL}
    else {data = input_clinical()}
  }
  
  else if ((input$choose_upload == 'random') && (input$choose_subset == 'full')) {
    data = random_clinical()
  }
  
  else if ((input$choose_upload == 'upload') && (input$choose_subset == 'subset') && !is.null(input$clinical_data)) {
    
    req(input$subset_cols)
    req(input$subset_rows)
    
    data = input_clinical() %>% 
      dplyr::select(any_of(input$subset_cols)) %>% 
      dplyr::slice_head(n = input$subset_rows)
    
  }

  else if ((input$choose_upload == 'random') && (input$choose_subset == 'subset')) {
    
    req(input$subset_cols)
    req(input$subset_rows)
    
    
    data = random_clinical() %>% 
      dplyr::select(any_of(input$subset_cols)) %>% 
      dplyr::slice_head(n = input$subset_rows)
    
  }
  else {data = NULL}
  
  
  return(data)
})


# ---------------- THE  MODELS ----------------

nmf_mod = reactive({
  
  req(data_M())
  
  
  if (input$method == 'snmf/r' || input$method == 'snmf/l') {
    model = nmf(data_M(), input$rank, input$method, seed = input$seed, beta = input$beta, eta = input$eta)
  } else {
    model = nmf(data_M(), input$rank, input$method, seed = input$seed)
  }
  
  model
  
  
}) # run nmf

E_hat = reactive({
  req(data_M())
  E = as.data.frame(basis(nmf_mod()))
  
  for (i in 1:ncol(E)) {
    colnames(E)[i] = paste0('S', i)
  }
  as.matrix(E)
}) # computed exposure matrix 

S_hat = reactive({
  req(data_M())
  coef(nmf_mod())
}) # computet signature matrix 

clustering = reactive({
  req(data_M())
  plottitle = paste('Clustering brest cancer data \n Method:', input$method, 
                    'Rank:', input$rank, 'K groups:', input$k_groups)
  
  pheatmap::pheatmap(t(E_hat()), scale = input$scale,
                     clustering_distance_cols = input$dist_met,
                     clustering_distance_rows = input$dist_met,
                     clustering_method = input$clust_met,
                     cutree_cols = input$k_groups,
                     main = plottitle)
  
})

merge_id_response_cluster = reactive({
  
  req(E_hat())
  req(clustering())
  req(data_clinical())  
  
  req(input$id_p)
  req(input$response)
  req(input$k_groups)
  
  E = as.data.frame(E_hat())
  id = row.names(E)
  E = cbind(id, E)
  
  subset_vector = c(input$id_p, input$response)
  subset_clinical = subset(data_clinical(), select = subset_vector)
  
  # Remove all rows with NA values 
  subset_clinical = subset_clinical[complete.cases(subset_clinical), ]
  names(subset_clinical) = c('id', 'response')
  
  grouping = data.frame(cluster = cutree(clustering()$tree_col, k = input$k_groups))
  
  id = row.names(grouping) 
  grouping = cbind(id, grouping)
  
  
  merge(subset_clinical, grouping, by = 'id')
  
  
})


# ------------- OBSERVE EVENTS ---------------

# go to upload from frontpage 
observeEvent(input$switchtab, {
  newtab <- switch(input$sidebar, "welcome" = "upload","upload" = "welcome")
  updateTabItems(session, "sidebar", newtab)
})

# Show modal if data is not present in general
observeEvent(input$sidebar, {
  if (input$choose_upload == 'upload' && 
      is.null(input$mutation_data) &&
      (input$sidebar == 'nmf1' || input$sidebar == 'nmf2')) {
    showModal(modalDialog(
      title = "Required data is not present",
      sprintf('Mutation data is needed to do NMF.
                Go to "Upload data" to upload your data or choose the synthetic dataset"'),
      easyClose = TRUE,
      footer = modalButton('Ok'),
    ))}
  
  if (input$choose_upload == 'upload' && 
      (is.null(input$clinical_data) || is.null(input$mutation_data))  &&
      (input$sidebar == 'categorical' || input$sidebar == 'continuous' || input$sidebar == 'survival')) {
    showModal(modalDialog(
      title = "Required data is not present",
      sprintf('Mutation data and clinical dataset is required to do clinical analysis.
                Go to "Upload data" to upload your data or choose the synthetic dataset"'),
      easyClose = TRUE,
      footer = modalButton('Ok'),
    ))}
  
  if (input$choose_upload == 'upload' && input$sidebar == 'subset' &&
      (is.null(input$clinical_data) || is.null(input$mutation_data))){
    showModal(modalDialog(
      title = "Check for required data",
      sprintf('Make sure you have uoloaded the dataset you want to subset'),
      easyClose = TRUE,
      footer = modalButton('Ok')
      
      
    ))}
  
  
  
})


# dont remove all columns 
observeEvent(input$subset_cols, {
  
  
  if (length(input$subset_cols) == 1) {
    showModal(modalDialog(
      title = "Require at least two columns",
      sprintf('The id column and at least one clinical vairble must be present in the dataset'),
      easyClose = TRUE,
      footer = modalButton('Ok')))
    
    if (input$choose_upload == 'upload') {
      data = input_clinical()
    }
    
    else if (input$choose_upload == 'random') {
      data = random_clinical()
    }
    
    updateSelectizeInput(session, "subset_cols", selected = names(data))
  }
})


# correct file format 
observeEvent(input$mutation_data, {
  
  req(input$mutation_data)
  data = input$mutation_data
  ext = tools::file_ext(data$datapath)
  
  if (!(ext %in% c("csv", "txt", "xlsx"))) {
    showModal(modalDialog(
      title = "Invalid file format",
      sprintf('Please upload a csv, txt, or xlsx file'),
      easyClose = TRUE,
      footer = modalButton('Ok')))
    
    
  }})

# correct file format 
observeEvent(input$clinical_data, {
  
  req(input$clinical_data)
  data = input$clinical_data
  ext = tools::file_ext(data$datapath)
  
  if (!(ext %in% c("csv", "txt", "xlsx"))) {
    showModal(modalDialog(
      title = "Invalid file format",
      sprintf('Please upload a csv, txt, or xlsx file'),
      easyClose = TRUE,
      footer = modalButton('Ok')))
    
    
  }})

# update boxes - datasets 
observeEvent(input$choose_upload, {
  
  # upload but no uploadbox -> open box 
  if (input$choose_upload == 'upload' && input$upload_box$collapsed) {
    updateBox('upload_box', action = 'toggle')
  }
  
  # random but uploadbox -> close box 
  if (input$choose_upload == 'random' && !input$upload_box$collapsed) {
    updateBox('upload_box', action = 'toggle')
  }
  
  # random but no show clinical -> open clinical 
  if (input$choose_upload == 'random' && input$clinical_box$collapsed) {
    updateBox('clinical_box', action = 'toggle')
  }
  
  # random but no show mut -> open mut 
  if (input$choose_upload == 'random' && input$mut_box$collapsed) {
    updateBox('mut_box', action = 'toggle')
  }
  
  # no data M bit open mut box -> close mut box
  if (is.null(data_M()) && !input$mut_box$collapsed) {
    updateBox('mut_box', action = 'toggle')
  }
  
  # data M but no mut box -> open mut box
  if (!is.null(data_M()) && input$mut_box$collapsed) {
    updateBox('mut_box', action = 'toggle')
  }
  
  # no data clinical bit open clinical box -> close clinical box
  if (is.null(data_clinical()) && !input$clinical_box$collapsed) {
    updateBox('clinical_box', action = 'toggle')
  }
  
  # data clincial but no clincal box -> open clinical box
  if (!is.null(data_clinical()) && input$clinical_box$collapsed) {
    updateBox('clinical_box', action = 'toggle')
  }
  
  
})

observeEvent(data_M(), {
  if (is.null(data_M()) && !input$mut_box$collapsed) {
    updateBox('mut_box', action = 'toggle')
  }
  
  if (!is.null(data_M()) && input$mut_box$collapsed) {
    updateBox('mut_box', action = 'toggle')
  }
  
  
})

observeEvent(data_clinical(), {
  if (is.null(data_clinical()) && !input$clinical_box$collapsed) {
    updateBox('clinical_box', action = 'toggle')
  } 
  
  if (!is.null(data_clinical()) && input$clinical_box$collapsed) {
    updateBox('clinical_box', action = 'toggle')
  } 
  
})





