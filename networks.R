library(igraph)
library(networkD3)

get_author_pairs <- function(authors, sep = " -- "){
  #browser()
  if(length(authors) == 1){
    return(tibble(pair = NA, weight = 0))
  }
  ret <- expand_grid(author1 = sort(authors) , author2 = sort(authors) ) %>%
    mutate(pair = sprintf("%s%s%s", author1, sep, author2)) %>% 
    filter(author1 < author2) %>% 
    mutate(weight = 1/(length(authors)-1))
  if(any(is.na(ret$weight))){
    browser()
  }
  if(nrow(ret) == 0){
    return(tibble(pair = NA, weight = 0))
  }
  ret
}

get_collab_matrix <- function(data){
  master_cleaned <- apply_full_name_patches(data, "full_name_patches.xlsx")
  ap <- master_cleaned %>% 
    select(abstract_id, full_name) %>%
    group_by(abstract_id) %>%
    mutate(nest(get_author_pairs(full_name))) %>% 
    ungroup() %>% 
    filter(!is.na(abstract_id))
  apc <- ap %>%  
    distinct(abstract_id, data) %>% 
    unnest(data) %>% 
    filter(!is.na(pair))
  apc %>% 
    bind_rows(apc %>% rename(x = author2) %>% 
                select(abstract_id, pair, author2 = author1, author1 = x, weight)) %>% 
    group_by(pair) %>% 
    mutate(count = n()/2, 
           total_weight = sum(weight, na.rm = T)/2)  %>% 
    ungroup()
}
get_collab_adj_matrix <- function(collab){
  collab_adj <- collab %>% 
    select(author1, author2, weight) %>% 
    pivot_wider(names_from = author2, values_from = weight, values_fn = sum) %>%
    column_to_rownames("author1") %>% 
    as.matrix() 
  collab_adj <- collab_adj[order(row.names(collab_adj)),order(colnames(collab_adj))] 
  collab_adj[is.na(collab_adj)] <- 0
  collab_adj
}

get_coworkers <- function(collab, author = "", degree = 1, exclude = NULL){
  #messagef("Finding coworker for %s (degree = %d)", author, degree)
  author <- str_replace(author, "\\{[a-zA-Z]+\\}", "")
  deg1 <- collab %>% 
    filter(str_detect(pair, author)) %>% 
    select(author1, author2) %>% 
    pivot_longer(c(author1, author2)) %>% 
    pull(value) %>% 
    unique()
  l <- length(deg1)
  saturated <- FALSE
  ret <- tibble(name = deg1, degree = 1)
  #messagef("Degree: %d, New: %d, total = %d, saturated = %s", 1, l, l, saturated)
  
  cur_degree <- 2
  remaining <- setdiff(deg1, exclude)
  while(cur_degree < degree & !saturated){
    #browser()
    deg2 <- 
      map(remaining, function(x){
      tmp <- get_coworkers(collab, x, degree = 1, exclude = remaining) %>% pull(name)
      setdiff(tmp, x)
    }) %>% unlist() %>% unique() 
    remaining <- setdiff(deg2, deg1)
    deg1 <- unique(c(deg1, deg2))
    l2 <- length(deg1)
    saturated <- l2 == l
    #messagef("Degree: %d, New: %d, total = %d, saturated = %s", cur_degree, l2 - l, l2, saturated)
    if(!saturated){
      ret <- bind_rows(ret, tibble(name = remaining, degree = cur_degree))
      
    }
    l <- l2
    cur_degree <- cur_degree + 1
    
  }  
  ret
}
highlight_author <- function(d3_network, collab_df, author = "Frieler, Klaus"){
  highlight <- get_coworkers(collab_df, author) %>% 
    pull(name) %>% 
    unique()
  #browser()
  d3_network$nodes$group <- 3
  if(nrow(d3_network$nodes[!(d3_network$nodes$name %in% highlight),]) > 0){
    d3_network$nodes[!(d3_network$nodes$name %in% highlight),]$group <- 1
    full_name_author <- highlight[str_detect(highlight, author)] 
    if(length(full_name_author) == 1){
      d3_network$nodes[d3_network$nodes$name == full_name_author,]$group <- 2
    }
    
  }
  d3_network  
}

highlight_community <- function(d3_network, community_data, community_idx){
  if(tolower(community_idx) == "top10"){
    d3_network$nodes$group <- NULL
    d3_network$nodes <- 
      d3_network$nodes %>% 
      left_join(community_data %>%  
                  select(name, group = community), by = "name") %>% 
      mutate(group = fct_lump_n(as.factor(group), n = 10, other_level = "11")) %>% 
      mutate(group = as.integer(group))
    return(d3_network)
  }
  full_name_author <- community_data %>% filter(community == community_idx) %>% pull(name)
  
  if(length(full_name_author) > 0 && sum(d3_network$nodes$name %in% full_name_author) > 0){
    d3_network$nodes$group <- 1
    d3_network$nodes[d3_network$nodes$name %in% full_name_author,]$group <- 2
    if(d3_network$nodes$group[1] == 2){
      d3_network$nodes$group <- 3 - d3_network$nodes$group
    }
    messagef("First group value: %s", d3_network$nodes$group[1])        
  }  
  else{
    d3_network$nodes$group <- 1
    
  }
  d3_network  
}

get_network <- function(master, 
                        sample_n = NA, 
                        author = "Frieler, Klaus", 
                        community = NA,
                        recalc = F,
                        set_globals = F,
                        format = "d3", 
                        subset = "all"){
  if(recalc){
    if(!is.na(sample_n)){
      p <- unique(master$abstract_id) %>% sample(sample_n)
      master <- master %>% filter(abstract_id %in% p)
    }
    collab <- get_collab_matrix(master)
    collab_adj <- collab %>% 
      select(author1, author2, weight) %>% 
      pivot_wider(names_from = author2, values_from = weight, values_fn = sum) %>%
      column_to_rownames("author1") %>% 
      as.matrix() 
    collab_adj <- collab_adj[order(row.names(collab_adj)),order(colnames(collab_adj))] 
    collab_adj[is.na(collab_adj)] <- 0
  }
  else{
    load("collab_data.rda")
  }
  #browser()
  adj_mat <- collab_adj
  collab_df <- collab
  if(subset == "core"){
    adj_mat <- collab_adj_core
    collab_df <- collab_core
  }
  if(subset == "rim"){
    adj_mat <- collab_adj_rim
    collab_df <- collab_rim
  }
  ig_network <- graph_from_adjacency_matrix(adj_mat, mode = "undirected", weighted = T)
  d3_network <- igraph_to_networkD3(ig_network)
  if(!is.na(community) && !is.null(community) && nchar(community) > 0 && community != "---"){
    messagef("Highlighting community '%s'", community)
    d3_network <- highlight_community(d3_network, community_data, community ) 
  }
  else{
    d3_network <- highlight_author(d3_network, collab_df, author) 
    
  }
  if(set_globals){
    assign("collab", collab, globalenv())
    assign("collab_adj", collab_adj, globalenv())
    assign("d3n", d3_network, globalenv())
    assign("ign", ig_network, globalenv())
  }
  if(format == "d3"){
    return(d3_network)
  }  
  ig_network
}

plot_D3_network <- function(d3_network, 
                           file = "network.html",
                           charge = -120, 
                           linkDistance = 20,
                           fontSize = 24,
                           opacityNoHover = .1){
  sn <- forceNetwork(
    Links = d3_network$links, 
    Nodes = d3_network$nodes, 
    Source = 'source', 
    Target = 'target', 
    NodeID = 'name', 
    Group = "group", 
    fontSize = fontSize,
    opacityNoHover = opacityNoHover,
    bounded = F,
    zoom = T,
    charge = charge,
    linkDistance = linkDistance,
    #Nodesize = "size",
    colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")
  )
  if(!is.null(file) && nchar(file) > 0){
    sn %>% saveNetwork(file = file)
  }
  else{
   return(sn) 
  }
}

get_all_communities <- function(collab, seed = "Crooke"){
  all_names <- union(collab$author1, collab$author2)
  
  counter <- 1
  init_cluster <- get_coworkers(collab, seed, degree = 200) %>% mutate(community = 1)
  repository <- setdiff(all_names, init_cluster$name)
  while(length(repository) > 0 ){
    counter <- counter  + 1 
    cluster <- get_coworkers(collab, repository[1], degree = 200) %>% mutate(community = counter)
    messagef("Found %d members for community %d (seed = %s)", nrow(cluster), counter, repository[1])
    init_cluster <- bind_rows(init_cluster, cluster)
    repository <- setdiff(repository, cluster$name)
    
  }
  init_cluster %>% 
    select(-degree) %>%
    group_by(community) %>% 
    mutate(n_comm = n()) %>% 
    ungroup() %>% 
    mutate(community = factor(community) %>% 
             fct_infreq() %>% 
             as.integer() %>% 
             as.factor())
}

save_collab_data <- function(collab, steps = 20){
  core_cluster <- get_coworkers(collab, "Crooke", 200)
  
  collab_core <- get_collab_matrix(master %>% filter(full_name %in% core_cluster$name))
  collab_adj_core <- get_collab_adj_matrix(collab_core) 

  collab_rim <- get_collab_matrix(master %>% filter(!(full_name %in% core_cluster$name)))
  collab_adj_rim <- get_collab_adj_matrix(collab_rim) 
  
  community_data <- community_analysis(collab, steps = steps)

  save(collab, collab_adj, 
       collab_core, collab_adj_core, 
       collab_rim, collab_adj_rim, 
       community_data, 
       file = "collab_data.rda")
}

community_analysis <- function(collab, steps = 20){
  base_communities <- get_all_communities(collab, seed = "Crooke")
  ms <- walktrap.community(ign, steps = steps) %>% membership()
  ms <- tibble(name = names(ms), community = as.integer(ms))  %>%   
    group_by(community) %>% 
    mutate(n_comm = n()) %>% 
    ungroup() %>% 
    mutate(community = factor(community) %>% 
             fct_infreq() %>% 
             as.integer() %>% 
             as.factor())
  
  combined <-  ms %>% left_join(base_communities %>% rename(base_com = community, n_comm_base = n_comm), by = "name")
  combined
}

get_community_entries <- function(subnetwork = "all", min_size = 0, type = "communities"){
  tmp <- community_data
  if(subnetwork == "core"){
    tmp <- community_data %>% filter(base_com == "1")
  }
  if(subnetwork == "rim"){
    tmp <- community_data %>% filter(base_com != "1")
  }
  tmp <- tmp %>% filter(n_comm >= min_size) %>% arrange(desc(n_comm))
  if(type == "communities"){
    tmp <- tmp %>% distinct(community, n_comm)  
    ret <- as.character(tmp$community)
    names(ret) <- sprintf("%s (%s)", as.character(tmp$community), tmp$n_comm)
    ret <- c("Top 10" = "top10", ret)
  }
  else{
    ret <- tmp$name 

  }
  c("---" = "---", ret) 
 }