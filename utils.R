messagef <- function(...) messagef(sprintf(...))
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
                "time_ist" = "Indian Standard Time", "time_gmt" = "Greenwich Mean Time",
                "time_cdt" = "Central Daylight Time", 
                "time_aest" = "Australian Eastern Standard Time")
tz <- names(time_zones)
names(tz) <- time_zones
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
fix_days <- function(data){
  day_map <- unique(master$day) %>% lag(1)
  names(day_map) <- unique(data$day)
  data$real_day <- data$day
  data[str_detect(data$time_utc, "23"),]$real_day <- day_map[data[str_detect(data$time_utc, "23"),]$real_day]
  data
}
setup_workspace <- function(fname = "ICMPC-ESCOM-2021-Programme.csv"){
  
  master <- readr::read_csv(fname)
  names(master) <- c("day", "time_utc", "strand", "room", "order", "theme", "authors", "title", "abstract_id", 
                     "country")
  assign("master_raw", master, globalenv())
  master <- fix_days(master)
  master <- master %>% 
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
    mutate(last_name = trimws(last_name)) %>% 
    mutate(time_utc =  str_replace_all(time_utc, "\\([a-zA-Z]+\\)", "") %>% trimws() %>% as.numeric()) 
    master <- master %>%
    mutate(hub = case_when(time_utc >= 8 & time_utc <= 18 ~ "08:00-18:00", 
                                  time_utc >= 23 | time_utc <= 2 ~ "23:00-02:00", 
                                  TRUE ~ "05:00-07:00")) %>% 
    mutate(time_cest = (time_utc  + 2) %% 24, 
           time_gmt = (time_utc + 1)  %% 24, 
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
  
  assign("master", master, globalenv())
}
