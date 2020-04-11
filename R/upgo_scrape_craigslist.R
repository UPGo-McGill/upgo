

upgo_scrape_craigslist <- function(city, old_results = NULL, timeout = 0.2,
                                   quiet = FALSE) {
  
  ### Find number of listings ##################################################
  
  listings_page <- 
    read_html(paste0(
      "https://",
      city, 
      ".craigslist.org/d/apts-housing-for-rent/search/apa?lang=en&cc=us"))
  
  listings_to_scrape <- 
    listings_page %>% 
    html_node(".totalcount") %>% 
    html_text()
  
  pages <- ceiling(as.numeric(listings_to_scrape) / 120)
  
  
  ### Scrape listing URLs ######################################################
  
  # Initialize url list
  url_list <- list()
  
  for (i in seq_len(pages)) {
    
    listings <- 
      read_html(paste0(
        "https://",
        city,
        ".craigslist.org/search/apa?s=",
        120 * (i - 1),
        "&lang=en&cc=us"
      ))
    
    url_list[[i]] <- 
      suppressWarnings(
        listings %>% 
          html_nodes(".result-row") %>% 
          str_extract('(?<=href=").*(?=" c)')
      )
    
    if (!quiet) message("Page ", i, " of ", pages, " scraped.")
    
  }
  
  url_list <- unique(unlist(url_list))
  
  
  ### Remove duplicate listings if old_results is provided
  
  if (!missing(old_results)) {
    
    url_list <-
      url_list[!str_replace(url_list, 
                            "\\?lang=en&amp;cc=us", "") %in% old_results$url]
    
  }
  
  .temp_urls <<- url_list 
  
  ### Scrape individual pages ##################################################
  
  listings <- list()
  
  for (i in seq_along(url_list)) {
    
    listings[[i]] <- 
      tryCatch({
        url_list[[i]] %>% 
          read_html(options = "HUGE")
      }, error = function(e) NULL)
    
    if (!quiet) message("Listing ", i, " of ", length(url_list), " scraped.")
    
    .temp_listings <<- listings
    Sys.sleep(timeout)
    
  }
  
  
  ### Clean up #################################################################
  
  listings <- listings[!map_lgl(listings, is.null)]
  
  
  ### Parse HTML ###############################################################
  
  results <-
    listings %>% 
    map_dfr(~{
      tibble(
        id = tryCatch({
          .x %>% 
            html_node(xpath = '/html/body/section[@class="page-container"]') %>% 
            html_node(xpath = 'section[@class="body"]') %>% 
            html_node(xpath = 'section[@class="userbody"]') %>% 
            html_node(xpath = 'div[@class="postinginfos"]') %>% 
            html_node("p") %>% 
            html_text() %>% 
            str_extract('(?<=id: ).*')
        }, error = function(e) NA_character_),
        url =
          .x %>% 
          html_node(xpath = '/html/head/link[@rel="canonical"]/@href') %>% 
          html_text(),
        title = 
          .x %>% 
          html_node(xpath = '/html/head/title') %>% 
          html_text(),
        date = 
          .x %>% 
          html_node(xpath = '//*[@id="display-date"]/time/@datetime') %>% 
          html_text(),
        price =
          tryCatch({
            .x %>% 
            html_node(xpath = '/html/body') %>% 
              html_node(xpath = 'section[@class = "page-container"]') %>% 
              html_node(xpath = 'section[@class = "body"]') %>% 
              html_node(xpath = 'h2[@class = "postingtitle"]') %>% 
              html_node(xpath = 'span/span[@class = "price"]') %>% 
              html_text() %>% 
              readr::parse_number()
          }, error = function(e) NA_real_),
        location =
          .x %>% 
          html_node(xpath = '/html/head') %>% 
          html_node(xpath = 'meta[@name = "geo.position"]/@content') %>% 
          html_text(),
        details =
          tryCatch({
            .x %>% 
              html_node(xpath = '/html/body/section[@class="page-container"]') %>% 
              html_node(xpath = 'section[@class="body"]') %>% 
              html_node(xpath = 'section[@class="userbody"]') %>% 
              html_node(xpath = 'div[@class = "mapAndAttrs"]') %>% 
              html_nodes(xpath = 'p') %>% 
              html_text() %>% 
              str_replace_all("\n", "") %>% 
              str_replace_all("  ", " ") %>% 
              str_replace_all("  ", " ") %>% 
              str_replace_all("  ", " ") %>% 
              str_replace_all("  ", " ") %>% 
              str_extract('(?<= ).*(?= )') %>% 
              paste(collapse = "; ")
            }, error = function(e) NA_character_),
        text =
          tryCatch({
            .x %>% 
              html_node(xpath = '/html/body/section[@class="page-container"]'
                        ) %>% 
              html_node(xpath = 'section[@class= "body"]') %>% 
              html_node(xpath = 'section[@class= "userbody"]') %>% 
              html_node(xpath = 'section[@id = "postingbody"]') %>% 
              html_text() %>% 
              str_replace(
                "\n            QR Code Link to This Post\n            \n        \n", ""
                ) %>% 
              str_replace_all("•\t", "") %>% 
              str_replace_all("\n", " ")
          }, error = function(e) NA_character_),
        photos = 
          list(.x %>% 
          html_nodes(xpath = '//*/img') %>% 
          html_node(xpath = '@src') %>% 
          html_text() %>% 
          # `[`(2:length(.)) %>% 
          str_replace("50x50c", "600x450") %>% 
          unique())
        )
    })
    
  results
  
}

