### Function to assemble list of URLs to scrape ################################

upgo_scrape_kijiji_urls <- function(url) {
  
  ### Process url and initialize url list ######################################
  
  url_1 <-
    url %>% 
    str_extract('.*(?=/)') %>% 
    paste0("/")
    
  url_2 <-
    url %>% 
    str_replace(url_1, "")
  
  urls <- list()
  
  
  ### Open browser #############################################################
  
  remDr <- rD[["client"]]
  remDr$setImplicitWaitTimeout(5000)
  
  
  ### Figure out how many pages to scrape ######################################
  
  remDr$navigate(url)
  
  pages <- remDr$findElement("class", "showing")
  
  pages <-
    pages$getElementAttribute("outerHTML")[[1]] %>%
    str_extract('(?<= of ).*(?=( Ads)|( results))') %>%
    parse_number() %>%
    {ceiling(./40)} %>%
    min(100)
  
  
  ### Loop to retrieve long-term listing links #################################
  
  for (i in seq_len(pages)) {
    
    remDr$navigate(paste0(url_1, "page-", i, "/", url_2))
    
    elements <- remDr$findElements("class", "title")
    
    urls[[i]] <-
      map_chr(elements, ~{
        .x$getElementAttribute("outerHTML")[[1]]
      }) %>%
      str_extract('(?<=href=..).*(?=" class)') %>%
      unique()
    
    Sys.sleep(timeout)
    
  }
  
  
  ### Repeat the loop backwards if necessary ###################################
  
  for (i in seq_len(pages)) {
    
    remDr$navigate(paste0(url_1, "page-", i, "/", url_2, "&sort=dateAsc"))
    
    elements <- remDr$findElements("class", "title")
    
    urls[[pages + i]] <-
      map_chr(elements, ~{
        .x$getElementAttribute("outerHTML")[[1]]
      }) %>%
      str_extract('(?<=href=..).*(?=" class)') %>%
      unique()
    
    Sys.sleep(timeout)
    
  }
  
  
  ### Compile links ############################################################
  
  urls_complete <- urls %>% unlist() %>% unique()
  
  return(urls_complete)
}
