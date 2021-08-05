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

get_coworkers <- function(collab, author = "", degree = 1, exclude = NULL){
  messagef("Finding coworker for %s (degree = %d)", author, degree)
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
    messagef("Degree: %d, New: %d, total = %d, saturated = %s", cur_degree, l2 - l, l2, saturated)
    if(!saturated){
      ret <- bind_rows(ret, tibble(name = remaining, degree = cur_degree))
      
    }
    l <- l2
    cur_degree <- cur_degree + 1
    
  }  
  ret
}

get_network <- function(master, 
                        sample_n = NA, 
                        author = "Frieler, Klaus", 
                        recalc = F,
                        set_globals = F,
                        format = "d3"){
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
  ig_network <- graph_from_adjacency_matrix(collab_adj, mode = "undirected", weighted = T)
  d3_network <- igraph_to_networkD3(ig_network)
  highlight <- get_coworkers(collab, author) %>% 
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

plot_D3_network <-function(d3_network, 
                           file = "network.html",
                           charge = -120, 
                           linkDistance = 20,
                           fontSize = 24){
  sn <- forceNetwork(
    Links = d3_network$links, 
    Nodes = d3_network$nodes, 
    Source = 'source', 
    Target = 'target', 
    NodeID = 'name', 
    Group = "group", 
    fontSize = fontSize,
    opacityNoHover = .1,
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