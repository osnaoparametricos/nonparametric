
# package -----------------------------------------------------------------
require(rvest)
require(stringr)
require(rebus)
require(dplyr)
require(purrr)


# get number of _last_page  ------------------------------------------------
#' @title get the number of last page
#' @function get.last.page 
#' @param link - complete link in the city
#' @return - max number of pagination

get.last.page <- function(link){
  
  pages_data <-
    link %>% 
    html_nodes('a')  %>%  # puxa todas as linhas de nó (a) que é HTML
    html_attr("href") %>%  # extrai os atributo href
    grep("page", ., value = TRUE) %>% 
    str_match_all("[0-9]+") %>% unlist %>% as.numeric() %>% 
    sort(decreasing  = TRUE) %>% 
    first(1)
  
  return(pages_data)
  
}

# extract links  -------------------------------------------------------------------
#' @title extract all links 
#' @function get.all.links
#' @param page - pages of events 
#' @return - extract all links, to go on event card

get.all.link <- function(page){
  
  evento <- 
    page %>%
    read_html() %>% 
    html_nodes("section aside a") %>% 
    html_attr("href") %>% 
    unique()
  
  return(evento)
}


# extract.data ------------------------------------------------------------

#' @title extract all links 
#' @function get.all.links
#' @param link - link card event 
#' @return - the informations in format data.frame

extract.attr.union <- function(link){
  
  link_html <- read_html(link)   

  en_html <- 
    link_html %>% 
    html_nodes("meta[content='en']")
  
  title <-
    link_html %>% 
    html_nodes("h1[class='listing-hero-title']") %>% 
    html_text("h1") %>% # Take between the nodes 
    character.to.na()
  
  date <- 
    link_html %>%
    html_nodes("time[class='listing-hero-date']") %>% 
    html_attr("datetime") %>% 
    character.to.na()
  
  
  price <-
    link_html %>%
    html_nodes("div[class='js-display-price']") %>% 
    html_text("div") %>% # Take between the nodes
    str_replace_all(., space(), "") %>% 
    gsub("R\\$", "", .) %>% 
    character.to.na()
  
  lat <-
    link_html %>%
    html_nodes("meta[property ='event:location:latitude']") %>%
    html_attr("content")  # Take between the nodes
  
  long <- 
    link_html %>%
    html_nodes("meta[property ='event:location:longitude']") %>%
    html_attr("content") 
  
  if(purrr::is_empty(en_html)){
    
    description <-
      link_html %>%
      html_nodes("div[data-automation='listing-event-description']") %>%
      html_text("div") %>%  # Take between the nodes
      str_replace_all(., space(), " ") %>% 
      stringr::str_squish() %>%
      character.to.na()
    
  } else{
    
    description <-
      link_html %>%
      html_nodes("div[class='has-user-generated-content']") %>% 
      html_text("div") %>% 
      str_replace_all(., space(), " ") %>% 
      stringr::str_squish() %>%
      character.to.na()
  } 
  
  nd_df <- tibble("date" = date, 
                  "title" = title, 
                  "price" = price,
                  "description" = description, 
                  "longitude"= long, 
                  "latitude" = lat, 
                  "url" = link)
  
  return(nd_df)
  
}

# create_url_to_city ------------------------------------------------------

format.city <- function(city){
  
  prefixo <- "https://www.eventbrite.com.br/d/brazil--" 
  sufixo <- "/all-events/"
 
  city  %<>%  
    str_to_lower() %>% 
    str_squish() %>% 
    stringi::stri_trans_general("Latin-ASCII")
  
  # mount link
  link_city <- 
    paste0(prefixo, city, sufixo)

  return(link_city)
  }

# function.to.apply --------------------------------------------------------
#' @title extract data from eventbrite 
#' @description 
#' 
#' @function get.eventbrite
#' @param city - city  
#' @return - the informations all events in the city

get.eventbrite <- function(city){
  
  url <- format.city(city)
  all_event <- url
  
  loop_page_event <- paste0(all_event, "?page=")
  
  # get_all_links
  html_all_event <- read_html(all_event)
  list_of_page <- str_c(loop_page_event, 1:get.last.page(html_all_event))
  
  future::plan(future::multiprocess)
  all_event_url <- furrr::future_map(list_of_page, ~get.all.link(.x)) %>% unlist()
  
  # Extract_all_events

  # future::plan(future::multiprocess)
  eventbrite <- try(furrr::future_map_dfr(all_event_url, ~extract.attr.union(.x)))
  
  return(eventbrite)
}


