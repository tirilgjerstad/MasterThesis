

## Shiny Server component for dashboard

function(input, output, session){
  
  
  source('server/server_general.R', local = TRUE)
  source('server/server_datasets.R', local = TRUE)
  source('server/server_matrixfactorization.R', local = TRUE)
  source('server/server_categorical.R', local = TRUE)
  source('server/server_continuous.R', local = TRUE)
  source('server/server_survival.R', local = TRUE)
  
  
  
  session$onSessionEnded(function() {
    stopApp()
  })

  
  
  
}