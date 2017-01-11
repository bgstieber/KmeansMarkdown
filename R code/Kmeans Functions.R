## kmeans aic function

kmeansAIC <- function(fit){
  
  m = ncol(fit$centers) 
  k = nrow(fit$centers)
  D = fit$tot.withinss
  return(D + 2*m*k)
}

##kmeans bic function

kmeansBIC <- function(fit){
  m = ncol(fit$centers) 
  n = length(fit$cluster)
  k = nrow(fit$centers)
  D = fit$tot.withinss
  return(D + log(n) * m * k)
}


##Define AIC and/or BIC function
kmeans_IC <- function(fit, criteria = 'AIC'){
  criteria <- toupper(criteria) #capitalize
  #stop function if neither option specified
  if(! criteria %in% c('AIC', 'BIC')){
    stop("Please specify one of either 'AIC' or 'BIC' for criteria")
  }
  #do the calculation
  m <- ncol(fit$centers) 
  n <- length(fit$cluster)
  k <- nrow(fit$centers)
  D <- fit$tot.withinss
  if (criteria == 'AIC'){
    return(D + 2 * m * k)
  }else{
    return(D + log(n) * m * k)
  }

}


kmeans2 <- function(data, center_range, iter.max, nstart, plot = TRUE){
  
  #fit kmeans for each center
  all_kmeans <- lapply(center_range, 
                       FUN = function(k) 
                         kmeans(data, center = k, iter.max = iter.max, nstart = nstart))

  #extract AIC 
  all_aic <- sapply(all_kmeans, kmeans_IC)
  #extract BIC
  all_bic <- sapply(all_kmeans, kmeans_IC, criteria = 'BIC')
  #extract tot.withinss
  all_wss <- sapply(all_kmeans, FUN = function(fit) fit$tot.withinss)
  #extract between ss
  btwn_ss <- sapply(all_kmeans, FUN = function(fit) fit$betweenss)
  #extract totall sum of squares
  tot_ss <- all_kmeans[[1]]$totss
  #put in data.frame
  clust_res <- 
    data.frame('Clusters' = center_range, 
               'AIC' = all_aic,   
               'BIC' = all_bic, 
               'WSS' = all_wss,
               'BSS' = btwn_ss,
               'TSS' = tot_ss)
  #plot or no plot?
  if(plot){
    par(mfrow = c(2,2))
    with(clust_res,{
      plot(Clusters, AIC)  #NOTE: Have not yet altered this portion to correspond to AIC/BIC choice
      plot(Clusters, BIC)
      plot(Clusters, WSS, ylab = 'Within Cluster SSE')
      plot(Clusters, BSS / TSS, ylab = 'Prop of Var. Explained')
    })
  }else{
    return(clust_res)
  }
  
}

# test<- kmeans2(readiness.shuff, c(4:6), 50, 50, plot=FALSE, Criteria = Criteria)

##kmeans_viz function

kmeans_viz <- function(fit, levels = NULL, show_N = TRUE){
  require(ggplot2)
  require(dplyr)
  #extract number of clusters
  clusts <- length(unique(fit$cluster))
  #centroids
  kmeans.table <- as.data.frame(t(fit$center), stringsAsFactors = FALSE)
  #variable names
  kmeans.table$Variable <- row.names(kmeans.table)
  #name clusters
  names(kmeans.table)[1:clusts] <- paste0('cluster', 1:clusts)
  #reshape from wide table to long (makes plotting easier)
  kmeans.table <- reshape(kmeans.table, direction = 'long',
                          idvar = 'Variable', 
                          varying = paste0('cluster', 1:clusts),
                          v.names = 'cluster')
  
  #number of observations in each cluster
  #should we show N in the graph or just print it?
  if(show_N){
    #show it in the graph
    kmeans.table$time <- paste0(kmeans.table$time,
                                ' (N = ',
                                fit$size[kmeans.table$time],
                                ')')
  }else{
    #just print it
    print(rbind('Cluster' = 1:clusts,
                'N' = fit$size))
  }
  #standardize the cluster means to make a nice plot
  kmeans.table %>%
    group_by(Variable) %>%
    mutate(cluster_stdzd = (cluster - mean(cluster)) / sd(cluster)) -> kmeans.table
  #did user specify a variable levels vector?
  if(length(levels) == length(unique(kmeans.table$Variable))){
    kmeans.table$Variable <- factor(kmeans.table$Variable, levels = levels)
  }
  
  
  #make the plot
  ggplot(kmeans.table, aes(x = Variable, y = time))+
    geom_tile(colour = 'black', aes(fill = cluster_stdzd))+
    geom_text(aes(label = round(cluster,2)))+
    coord_flip()+
    xlab('')+ylab('Cluster')+
    scale_fill_gradient(low = 'white', high = 'grey60')+
    theme_bw()+
    theme(legend.position = 'none',
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 16),
          panel.grid = element_blank(),
          axis.text = element_text(size = 14),
          axis.ticks = element_blank())
  
}




