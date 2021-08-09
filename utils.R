library(tidyverse)

messagef <- function(...) message(sprintf(...))

format_time <- function(time){
  suffix <- rep("00", length(time))
  suffix[floor(time) != time] <- "30"
  sprintf("%02d:%s", as.integer(floor(time)), suffix)
}
slot_codes <- c("P" = "Poster", 
                "S" = "Symposium", 
                "W" =  "Workshop", 
                "K" = "Keynote",
                "C" = "Concert", 
                "1" = "Talk", 
                "2" = "Talk", 
                "3" = "Talk", 
                "4" = "Talk")
time_zones <- c("time_utc" = "Universal Time", "time_cest" = "Central European Summer Time",
                "time_ist" = "Indian Standard Time", "time_bst" = "British Summer Time",
                "time_cdt" = "Central Daylight Time", 
                "time_aest" = "Australian Eastern Standard Time")
tz <- names(time_zones)
names(tz) <- time_zones

tz_offset <- c("utc" = 0, "cest" = 2, "bst" = 1, "ist" = 5.5, "cdt" = -5, "aest" = 10)
days <- c("Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
names(days) <- as.character(28:31)
day_select <- days[Sys.time() %>% lubridate::day() %>% as.character()] %>%  as.vector()
time_select <- sprintf("%02d:00", Sys.time() %>% lubridate::hour())

get_tz_day <- function(day, time_utc, timezone){
  if(length(day) > 1 ){
    return(map2_chr(day, time_utc, function(x,y) get_tz_day(x, y, timezone)))
  }
  #browser()
  real_time <- time_utc + tz_offset[timezone]
  real_day <- day
  if(real_time >= 24 ){
    day_map <- days %>% lead(1)
    names(day_map) <- days
    real_day <- day_map[day]    
  }
  if(real_time < 0 ){
    day_map<- days %>% lag(1)
    names(day_map) <- days
    real_day <- day_map[day]    
  }
  as.vector(real_day)
}

fix_days <- function(data){
  day_map <- days %>% lag(1)
  names(day_map) <- days
  data$day_utc <- data$day
  data[str_detect(data$time_utc, "23"),]$day_utc <- day_map[data[str_detect(data$time_utc, "23"),]$day_utc]
  data
}

get_last_author <- function(authors){
  if(length(authors) > 1){
    return(map_chr(authors, function(x) get_last_author(x)))
  }
  elems <- str_split(authors, ";")[[1]] %>% trimws()
  if(length(elems) < 3){
    return(NA)
  }
  elems[length(elems)]
  
}

setup_workspace <- function(fname = "ICMPC-ESCOM-2021-Programme.csv"){
  
  master <- readr::read_csv(fname)
  names(master) <- c("day", "time_utc", "strand", "room", "order", "theme", "authors", "title", "abstract_id", 
                     "country")
  assign("master_raw", master, globalenv())
  master <- fix_days(master) %>%  
    mutate(time_utc =  str_replace_all(time_utc, "\\([a-zA-Z]+\\)", "") %>% trimws() %>% as.integer()) %>% 
    mutate(day_cdt = get_tz_day(day_utc, as.integer(time_utc), "cdt"),
           day_ist = get_tz_day(day_utc, as.integer(time_utc), "ist"),
           day_aest = get_tz_day(day_utc, as.integer(time_utc), "aest"),
           day_cest = get_tz_day(day_utc, as.integer(time_utc), "cest"),
           day_bst = get_tz_day(day_utc, as.integer(time_utc), "bst"))
  
  assign("master_time", master, globalenv())
  master <- master %>% 
    mutate(authors = str_replace(authors, "Cohen, A., J., Russo, F. A., Ilari, B., Gudmundsdottir, H.R., Beynon, C., Ludke, K., M., Heydon, R., Fancourt, D.", 
                                 "Cohen, A. J.; Russo, F. A.; Ilari, B.; Gudmundsdottir, H.R.; Beynon, C.; Ludke, K. M.; Heydon, R.; Fancourt, D.")) %>% 
    mutate(authors = str_replace(authors, "\\([0-9,]+\\)", "")) %>% 
    mutate(authors = str_replace(authors, "Fink, Lauren K.", "XXXXX")) %>% 
    mutate(authors = str_replace(authors, "Fink, Lauren", "Fink, Lauren K.")) %>% 
    mutate(authors = str_replace(authors, "XXXXX", "Fink, Lauren K.")) %>% 
    mutate(authors = str_replace(authors, "Krause, Amanda E\\.", "XXXXX")) %>% 
    mutate(authors = str_replace(authors, "Krause, Amanda E", "Krause, Amanda E.")) %>% 
    mutate(authors = str_replace(authors, "XXXXX", "Krause, Amanda E.")) %>% 
    mutate(authors = str_replace(authors,"\\\r\\\n", "")) %>% 
    mutate(first_author = str_split(authors, ";") %>% map_chr(., ~{.x[[1]][1] %>% trimws()})) %>% 
    mutate(last_author = get_last_author(authors)) %>% 
    mutate(full_name = str_split(authors, ";")) %>%
    unnest(full_name) %>% 
    mutate(last_name = str_split(full_name, ",") %>% map_chr(., ~{.x[[1]][1]})) %>% 
    mutate(full_name = trimws(full_name)) %>% 
    mutate(last_name = trimws(last_name)) 
    master <- master %>%
    mutate(hub = case_when(time_utc >= 8 & time_utc <= 18 ~ "08:00-18:00", 
                                  time_utc >= 23 | time_utc <= 2 ~ "23:00-02:00", 
                                  TRUE ~ "05:00-07:00")) %>% 
    mutate(time_cest = (time_utc  + 2) %% 24, 
           time_bst = (time_utc + 1)  %% 24, 
           time_ist = (time_utc  + 5.5)  %% 24, 
           time_cdt = (time_utc - 5)  %% 24,
           time_aest = (time_utc + 10) %% 24) %>% 
    mutate_at(vars(starts_with("time")), format_time) %>% 
    mutate(slot_type = substr(room, 1, 1), poster_slot = substr(room, 2, 2)) %>% 
    mutate(slot_type = substr(room, 1, 1), poster_slot = substr(room, 2, 2)) %>% 
    mutate(slot_type_long = slot_codes[slot_type]) %>% 
    mutate(theme_cleaned = str_replace(theme, "\\([A-Z]{1}\\)", "") %>% 
             str_replace("\\\r\\\n", " ") %>% 
             str_replace("\\[[a-zA-Z ]+\\]", "") %>% trimws())
  master <- master %>% 
    group_by(abstract_id) %>% 
    mutate(n_auth = n()) %>% 
    ungroup()
  master <- add_theme_cats(master) %>% apply_full_name_patches(patch_file = "full_name_patches.xlsx")
  load("collab_data.rda", envir = globalenv())
  assign("master", master, globalenv())
}

apply_full_name_patches <- function(data, patch_file){
  patches <- readxl::read_xlsx(patch_file) %>% filter(!is.na(real_name))
  for(i in 1:nrow(patches)){
    data[data$full_name == patches[i,]$name,]$full_name <-  patches[i,]$real_name
  }
  data
}

add_theme_cats <- function(data, theme_file = "themes.xlsx"){
  theme_cats <- readxl::read_xlsx(theme_file) 

  ret <- data %>% left_join(theme_cats, by = "theme_cleaned" )
  assertthat::assert_that(nrow(ret) == nrow(data))
  ret
}

filter_author <- function(data = master, author = ""){
  data %>% filter(str_detect(full_name, author))
}

recalc_all <- function(){
  setup_workspace()
  get_network(master, recalc = T, set_globals = T)
  save_collab_data(collab)
}