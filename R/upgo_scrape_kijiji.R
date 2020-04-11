upgo_scrape_kijiji <- function(urls, folder_name) {

  remDr <- rD[["client"]]
  remDr$setImplicitWaitTimeout(2000)


  ## For loop to save full page contents #########################################

  results <-
    tibble(id = NA_real_,
           url = NA_character_,
           title = NA_character_,
           date = as.Date(NA),
           price = NA_real_,
           location = NA_character_,
           owner_name = NA_character_,
           owner_link = NA_character_,
           owner_listings = NA_integer_,
           owner_age = NA_character_,
           details = NA_character_,
           text = NA_character_,
           photos = list())

  on.exit(.temp_output <<- results)

  for (i in seq_along(urls)) {

    message(crayon::magenta(crayon::bold((glue::glue(
      "Scraping listing {i} of {length(urls)}. {Sys.time()}"
    )))))

    # Go to page
    remDr$navigate(paste0("http://www.kijiji.ca/", urls[[i]],
                          "?siteLocale=en_CA"))

    Sys.sleep(2)

    # Fill in id in results table
    results[i,]$id <- urls[[i]] %>% str_extract('(?<=/)[:digit:]*$')

    # Fill in URL in results table
    results[i,]$url <- urls[[i]]

    # Exit early if listing has been removed
    if (
      str_detect(unlist(remDr$getCurrentUrl()), "adRemoved") |
      suppressMessages(tryCatch({
        expired <- remDr$findElement("id", "PageExpiredVIP"); TRUE
      }, error = function(e) FALSE)) |
      suppressMessages((tryCatch({
        missing <- remDr$findElement(
          "xpath", "//*[text()='Hmm... Apparently this page no longer exists.']");
        TRUE
      }, error = function(e) FALSE)))
    ) {next}

    # Retrieve title
    title <- remDr$findElement("class", "title-2323565163")
    title <- title$getElementAttribute("outerHTML")[[1]] %>%
      str_extract('(?<=>).*(?=</h1>)')
    results[i,]$title <- title

    # Retrieve posting date
    date <- remDr$findElement("class", "datePosted-383942873")
    date <- date$getElementAttribute("outerHTML")[[1]] %>%
      str_extract('(?<=datetime..).*(?=T)') %>%
      as.Date()
    results[i,]$date <- date

    # Retrieve price
    price <- remDr$findElement("class", "currentPrice-2842943473")
    price <- price$getElementAttribute("outerHTML")[[1]] %>%
      str_extract('(?<=content..).*(?=.>\\$)') %>%
      as.numeric()
    results[i,]$price <- price

    # Retrieve location
    location <- remDr$findElement("class", "address-3617944557")
    location <- location$getElementAttribute("outerHTML")[[1]] %>%
      str_extract('(?<=557..).*(?=</span)')
    results[i,]$location <- location

    # Retrieve owner name and link
    if (suppressMessages(tryCatch({
      owner <- remDr$findElement("class", "link-3552666815")
      TRUE
    }, error = function(e) FALSE))) {
      owner <- owner$getElementAttribute("outerHTML")[[1]]
      owner_name <- owner %>% str_extract('(?<=..>).*(?=</a>)')
      owner_link <- owner %>% str_extract('(?<=href...).*(?=">)')
      results[i,]$owner_name <- owner_name
      results[i,]$owner_link <- owner_link

      # Retrieve owner listings
      owner_listings <- remDr$findElements("class", "line-2791721720")
      owner_listings <-
        (map_chr(owner_listings, ~{
          .x$getElementAttribute("outerHTML")[[1]]
        }))[[2]] %>%
        str_extract('(?<=<span>).*(?=</span>)') %>%
        as.integer()
      results[i,]$owner_listings <- owner_listings

      # Retrieve owner age
      owner_age <- remDr$findElements("class", "text-910784403")
      if (length(owner_age) > 0) {
        owner_age <-
          (map_chr(owner_age, ~{
            .x$getElementAttribute("outerHTML")[[1]]
          }))[[3]] %>%
          str_extract('(?<=..>).*(?=</div>)')
      } else owner_age <- NA_character_

      results[i,]$owner_age <- owner_age

    }

    # Retrieve details
    details <- suppressMessages(
      tryCatch({
        remDr$findElement("class", "itemAttributeCards-2416600896")},
        error = function(e) {
          remDr$findElement("class", "attributeListWrapper-2108313769")
        }))
    details <- details$getElementAttribute("outerHTML")[[1]]
    results[i,]$details <- details

    # Retrieve text
    text <- remDr$findElement("class", "descriptionContainer-3544745383")
    text <- text$getElementAttribute("outerHTML")[[1]] %>%
      str_replace_all("\\n", " ") %>%
      str_extract('(?<=<p>).*(?=</p>)')
    results[i,]$text <- text

    # Retrieve photo URLs
    photos <- remDr$findElements("class", "heroImageForPrint-1152389693")
    photos <- photos %>% map_chr(~{
      .x$getElementAttribute("outerHTML")[[1]] %>%
        str_extract('(?<=src..).*(?=" class)')
    })

    results[i,]$photos <- list(photos)

    # Save photos

    dir.create(paste0(folder_name, "/", results[i,]$id))

    if (length(photos) > 0) {
      download.file(photos, paste0(
        folder_name,
        "/",
        results[i,]$id,
        "/image_",
        seq_along(photos),
        ".jpg"
      ))
    }

    # Save temporary output


  }

  # Return output

  on.exit()

  results

}
