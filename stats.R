library(tidyverse)

get_basic_stats <- function(data  = master, display_names = T){
  stats <- data %>% 
    filter(theme_category != "Non-Paper") %>% 
    summarise(
      num_authors = n_distinct(full_name),
      num_papers = n_distinct(abstract_id),
      num_themes = n_distinct(theme_cleaned),
      num_themes_cat = n_distinct(theme_category),
      num_papers_per_theme = num_papers/num_themes,
      num_papers_per_theme_cat = num_papers/num_themes_cat,
      mean_authors_per_paper = num_authors/num_papers,
      mean_authors_per_theme = num_authors/num_themes,
      mean_authors_per_theme_cat = num_authors/num_themes_cat)
  if(display_names){
    names(stats) <- c("Authors", 
                      "Papers", 
                      "Themes (Original)", 
                      "Themes (Categorized)", 
                      "Papers per Theme (Original)",
                      "Papers per Theme (Categorized)",
                      "Avg. Number of Authors per Paper", 
                      "Avg. Number of Authors per Theme (Original)", 
                      "Avg. Number of Authors per Theme (Categorized)") 
    
  }
    stats %>% pivot_longer(cols = everything())
}

get_author_stats <- function(data = master){
  coworkers <- data %>% 
    filter(theme_category != "Non-Paper") %>% 
    group_by(full_name) %>% 
    summarise(n_coworkers = sum(n_auth, na.rm = T), 
              n_paper = n_distinct(abstract_id), 
              n_coworkers = n_coworkers - n_paper, .groups = "drop" ) 
  data %>% 
    group_by(full_name) %>% 
    summarise(n_paper = n_distinct(title), 
              n_first = sum(first_author == full_name, na.rm = T), 
              n_last = sum(last_author == full_name, na.rm = T), 
              n_middle = n_paper - n_first - n_last, 
              n_theme = n_distinct(theme_cleaned), 
              n_theme_cat = n_distinct(theme_category), 
              topic_diversity = n_theme/n_paper, 
              topic_diversity_cat = n_theme_cat/n_paper, 
              .groups = "drop") %>% 
    left_join(coworkers %>% select(-n_paper), by = "full_name") %>% 
    mutate(mean_coworkers_per_paper = n_coworkers/n_paper) 
}

get_theme_stats <- function(data = master, var = "theme_category"){
  data %>% 
    filter(theme_category != "Non-Paper") %>% 
    group_by(!!sym(var)) %>% 
    summarise(n_papers = n_distinct(title),
              n_authors = n_distinct(full_name), 
              mean_authors_per_paper = n_authors/n_papers
      , .groups = "drop")
}