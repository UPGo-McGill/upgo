#' Scrape media articles from Nexis Uni
#'
#' \code{upgo_scrape_ln} scrapes Nexis Uni for all articles which mention a
#' given search term.
#'
#' TKTK
#'
#' @param query TKTK
#' @param start_date TKTK
#' @param end_date TKTK
#' @param language TKTK
#' @param skip A positive integer. How many batches of 50 articles have already
#' been downloaded, and so should be skipped? E.g. a value of 33 means that 33
#' files (of 50 articles each) have been downloaded, for a total of 1,650
#' articles, and the function will resume with article 1,651.
#' @param file_prefix TKTK
#' @param timeout TKTK
#' @param quiet A logical scalar Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return TKTK
#' @export

upgo_scrape_nu <- function(query, start_date = NULL, end_date = NULL,
                           language = "English", skip = 0,
                           file_prefix = substr(query, 1, 6),
                           timeout = 20,
                           quiet = FALSE) {

  start_time <- Sys.time()


  ### Check packages and arguments #############################################

  helper_require("RSelenium")

  stopifnot(is.character(query), is.character(language), is.logical(quiet))

  if (!missing(start_date)) {
    start_date <- as.Date(start_date, origin = "1970-01-01")
    start_date <- paste(substr(start_date, 6, 7),
                        substr(start_date, 9, 10),
                        substr(start_date, 1, 4),
                        sep = "/")
  }

  if (!missing(end_date)) {
    end_date <- as.Date(end_date, origin = "1970-01-01")
    end_date <- paste(substr(end_date, 6, 7),
                      substr(end_date, 9, 10),
                      substr(end_date, 1, 4),
                        sep = "/")
  }


  ### Helper functions #########################################################

  get_current_page <- function() {

    current_page <- 0

    while (current_page == 0) {
      try({
        webElem <- remDr$findElement(using = "class name", value = "current")
        current_page <- as.numeric(unlist(webElem$getElementText()))
      }, silent = TRUE)
    }

    current_page

  }

  go_to_next_page <- function() {

    webElem <- remDr$findElement(using = "css selector",
                                 value = "a[data-action='nextpage']")

    suppressMessages(try(webElem$clickElement(), silent = TRUE))
    Sys.sleep(timeout / 10)
    suppressMessages(try(webElem$clickElement(), silent = TRUE))
    Sys.sleep(timeout / 10)
    suppressMessages(try(webElem$clickElement(), silent = TRUE))

  }

  go_to_previous_page <- function() {

    webElem <- remDr$findElement(using = "css selector",
                                 value = "a[data-action='prevpage']")

    suppressMessages(try(webElem$clickElement(), silent = TRUE))
    Sys.sleep(timeout / 10)
    suppressMessages(try(webElem$clickElement(), silent = TRUE))
    Sys.sleep(timeout / 10)
    suppressMessages(try(webElem$clickElement(), silent = TRUE))

  }

  clear_progress <- function() {

    # Find correct starting page
    current_page <- get_current_page()
    target_page <- current_page - current_page %% 10 + 1
    if (target_page > current_page) target_page <- target_page - 10

    # Go to starting page
    while (current_page > target_page) {
      go_to_previous_page()
      current_page <- get_current_page()
    }

    Sys.sleep(timeout / 10)

    # Deselect all articles
    view_tray <- remDr$findElement(using = "xpath",
                                   value = "//*[@data-action = 'viewtray']")

    # Exit early if no articles are selected
    if (!unlist(view_tray$isElementDisplayed())) return()

    while (unlist(view_tray$getElementAttribute("aria-expanded")) == "false") {
      view_tray$clickElement()
    }

    clear_all <- remDr$findElement(using = "xpath",
                                   value = "//*[@data-action = 'cancel']")

    clear_all$clickElement()

    Sys.sleep(timeout / 10)

    confirm <- remDr$findElement(using = "xpath",
                                 value = "//*[@data-action = 'confirm']")

    confirm$clickElement()

  }


  ### Initialize server and open browser #######################################

  if (!quiet) message(crayon::magenta("Beginning scrape... "), appendLF = FALSE)

  remDr <- .upgo_env$rD[["client"]]

  remDr$open(silent = TRUE)
  remDr$setImplicitWaitTimeout(1000)

  # Take screenshot on function failure for diagnostic purposes
  on.exit(remDr$screenshot(file = "test.png"))


  ### Load page ################################################################

  ## Check IP and load NU directly if already on McGill network ----------------

  if (stringr::str_starts(check_ip(), "132")) {
    remDr$navigate("http://nexisuni.com")
  } else remDr$navigate(
    "https://proxy.library.mcgill.ca/login?url=http://nexisuni.com")

  Sys.sleep(timeout)


  ### Authenticate if necessary  ###############################################

  # Check URL to determine if proxy login is necessary
  url <- remDr$getCurrentUrl()

  if (!substr(url, 9, 31) %in% c("advance-lexis-com.proxy",
                                 "advance.lexis.com/searc")) {


    ## Get user name -----------------------------------------------------------

    if (Sys.getenv("MCGILL_USER") != "") {

      user <- Sys.getenv("MCGILL_USER")

    } else {

      if (requireNamespace("rstudioapi", quietly = TRUE)) {

        user <-
          rstudioapi::askForPassword("Please enter your McGill email address.")

      } else stop("McGill credentials are missing. They can be supplied with ",
                  "the MCGILL_USER and MCGILL_PASSWORD .Renviron entries, ",
                  "or interactively by running this function from within ",
                  "RStudio.")

    }


    ## Get password ------------------------------------------------------------

    if (Sys.getenv("MCGILL_PASSWORD") != "") {

      password <- Sys.getenv("MCGILL_PASSWORD")

    } else {

      if (requireNamespace("rstudioapi", quietly = TRUE)) {

        password <-
          rstudioapi::askForPassword("Please enter your McGill password.")

      } else stop("McGill credentials are missing. They can be supplied with ",
                  "the MCGILL_USER and MCGILL_PASSWORD .Renviron entries, ",
                  "or interactively by running this function from within ",
                  "RStudio.")

    }

    if (!quiet) message(crayon::magenta("Authenticating... "), appendLF = FALSE)


    ## Find username field and enter value -------------------------------------

    user_field <- remDr$findElement(using = "name", value = "j_username")

    user_field$clickElement()

    user_field$sendKeysToElement(list(user))


    ## Find password field and enter value -------------------------------------

    pass_field <- remDr$findElement(using = "name", value = "j_password")

    pass_field$clickElement()

    pass_field$sendKeysToElement(list(password))


    ## Find log in button and click --------------------------------------------

    log_button <- remDr$findElement(
      using = "xpath", value = "/html/body/div/div[1]/form/div[3]/input")

    log_button$clickElement()


    ## Confirm success ---------------------------------------------------------

    Sys.sleep(timeout)

    url <- remDr$getCurrentUrl()

    if (substr(url, 9, 31) != "advance-lexis-com.proxy") stop(
      "Authentication failed. Please try again.")

  }

  # Clean up
  suppressWarnings(rm(url, user, password, user_field, pass_field, log_button))


  ### Perform search ###########################################################

  if (!quiet) message(crayon::magenta(
    "Entering search term... "), appendLF = FALSE)

  search_flag <- TRUE

  while (search_flag) {

    try({

      search_terms <- remDr$findElement(using = "id", value = "searchTerms")

      search_terms$clickElement()

      search_terms$sendKeysToElement(list(query))

      search_button <- remDr$findElement(using = "id", value = "mainSearch")

      search_button$clickElement()

      Sys.sleep(timeout)

      search_flag <- FALSE

    }, silent = TRUE)

  }

  # Clean up
  suppressWarnings(rm(search_flag, search_terms, search_button))


  ### Set 50 articles per page #################################################

  current_url <- remDr$getCurrentUrl()
  current_url <- unlist(current_url)

  if (stringr::str_starts(current_url, "https://advance.lexis")) {

    new_url <- paste0(
      "https://advance.lexis.com/settings/",
      stringr::str_extract(current_url, '(?<=com/search/).*')
    )

  } else new_url <- paste0(
    "https://advance-lexis-com.proxy3.library.mcgill.ca/settings/",
    stringr::str_extract(current_url, '(?<=mcgill.ca/search/).*')
    )

  remDr$navigate(new_url)

  result_button <- remDr$findElement(using = "id",
                                     value = "ResultsDisplay")

  {result_button$clickElement()
  result_button$sendKeysToActiveElement(list("5"))
  result_button$clickElement()}

  save_button <- remDr$findElement(using = "id",
                                   value = "saveChangesButton")

  save_button$clickElement()

  # Clean up
  suppressWarnings(rm(current_url, new_url, result_button, save_button))


  ### Perform search again #####################################################

  search_flag <- TRUE

  while (search_flag) {

    try({

      search_terms <- remDr$findElement(using = "id", value = "searchTerms")

      search_terms$clickElement()

      search_terms$sendKeysToElement(list(query))

      search_button <- remDr$findElement(using = "id", value = "mainSearch")

      search_button$clickElement()

      Sys.sleep(timeout)

      search_flag <- FALSE

    }, silent = TRUE)

  }

  # Clean up
  suppressWarnings(rm(search_flag, search_terms, search_button))


  ### Set language #############################################################

  if (!quiet) message(crayon::magenta("Setting language... "), appendLF = FALSE)

  language_flag <- TRUE

  while (language_flag) {

    suppressMessages(try({


      ## Find language button and click it if it isn't expanded ----------------

      lang_button <- remDr$findElement(using = "id",
                                           value = "podfiltersbuttonlanguage")

      if (unlist(lang_button$getElementAttribute("aria-expanded")) == "false") {
        lang_button$clickElement()
      }


      ## Click on "English" (and eventually other languages) -------------------

      lang_selection <- remDr$findElement(
        using = "xpath",
        value = "//*[@data-id = 'language']/li[1]/label/span[1]")

      lang_selection$clickElement()

      Sys.sleep(timeout)


      ## Check if the selection produced results -------------------------------

      article_header <-
        remDr$findElement(using = "class name", value = "resultsHeader")

      article_header <- unlist(article_header$getElementText())

      if (article_header == "News (0)") {

        clear_filters <- remDr$findElements(using = "class name",
                                            value = "clear-filters")

        clear_filters <- clear_filters[[
          which(str_detect(purrr::map_chr(
            clear_filters, ~.x$getElementAttribute("outerHTML")[[1]]),
            "data-action"))]]

        clear_filters$clickElement()

        Sys.sleep(timeout / 2)

      }


      ## Check if filter is applied and articles are displayed -----------------

      article_header <-
        remDr$findElement(using = "class name", value = "resultsHeader")

      article_header <- unlist(article_header$getElementText())

      filter_on <- suppressMessages(tryCatch({
        x <- remDr$findElement(using = "class name", value = "filter-text")
        unlist(x$getElementText())
        },
        error = function(e) "ERROR"))

      if (filter_on == "English" &&
          article_header != "News (0)") language_flag <- FALSE else {

            # Reapply clearing filters
            if (article_header == "News (0)") {

              clear_filters <- remDr$findElements(using = "class name",
                                                  value = "clear-filters")

              clear_filters <- clear_filters[[
                which(str_detect(purrr::map_chr(
                  clear_filters, ~.x$getElementAttribute("outerHTML")[[1]]),
                  "data-action"))]]

              clear_filters$clickElement()

              Sys.sleep(timeout / 2)

            }

          }

    }, silent = TRUE))

  }

  # Clean up
  suppressWarnings(
    rm(language_flag, lang_button, lang_selection, article_header,
       clear_filters, filter_on, x))


  ### Set date range ###########################################################

  date_flag <- if (!missing(start_date) || !missing(end_date)) TRUE else FALSE

  while (date_flag) {

      if (!quiet) message(crayon::magenta("Setting date range."))

    try({

      ## Test if the date entry fields are visible, and open area if not -------

      date_button <- remDr$findElement(
        using = "xpath",
        value = paste0("(//*[@alt = 'Timeline filter'])|",
                       "(//*[@id = 'podfiltersbuttondatestr-news'])"))

      if (unlist(date_button$getElementAttribute("data-action")) == "expand") {
        date_button$clickElement()
      }


      ## Enter start date ------------------------------------------------------

      if (!missing(start_date)) {

        min_date <- remDr$findElement(using = "class name", value = "min-val")

        min_date$sendKeysToElement(
          list("\u0008", "\u0008", "\u0008", "\u0008", "\u0008",
               "\u0008", "\u0008", "\u0008", "\u0008", "\u0008"))

        min_date$sendKeysToElement(list(start_date))

      }


      ## Enter end date --------------------------------------------------------

      if (!missing(end_date)) {

        max_date <- remDr$findElement(using = "class name", value = "max-val")

        max_date$clickElement()

        max_date$sendKeysToElement(
          list("\u0008", "\u0008", "\u0008", "\u0008", "\u0008",
               "\u0008", "\u0008", "\u0008", "\u0008", "\u0008"))

        max_date$sendKeysToElement(list(end_date))

      }

      Sys.sleep(timeout/10)

      ## Save parameters -------------------------------------------------------

      ok_button <- remDr$findElement(using = "class name", value = "save")

      ok_button$clickElement()

      Sys.sleep(timeout)

      date_flag <- FALSE

    }, silent = TRUE)

  }

  # Clean up
  suppressWarnings(rm(date_flag, date_button, min_date, max_date, ok_button))


  ### Sort oldest to newest for stable order ###################################

  sort_flag <- TRUE

  while (sort_flag) {

    suppressMessages(try({

      sort_menu <- remDr$findElement(using = "id", value = "sortbymenulabel")
      sort_menu$clickElement()

      old_opt <- remDr$findElement(using = "xpath",
                                   value = '//*[@id="dropdownmenu"]/button[5]')
      old_opt$clickElement()

      Sys.sleep(timeout / 2)

      sort_flag <- FALSE

    }, silent = TRUE))

  }

  # Clean up
  suppressWarnings(rm(sort_flag, sort_menu, old_opt))


  ### Apply offset if necessary ################################################

  skip_flag <- TRUE

  while (skip_flag) {

    try({

      # Get target page
      target_page <- floor(skip) + 1

      # Get current page
      current_page <- get_current_page()

      if (current_page < target_page) {

        # Set up progress bar
        if (!quiet) handler_upgo("Advancing page")

        # Advance until target_page == current_page
        progressr::with_progress({

          .upgo_env$pb <- progressr::progressor(
            steps = target_page - current_page)

          while (current_page < target_page) {

            last_page <- current_page

            suppressMessages(try({

              page_links <-
                remDr$findElements(using = "class name", value = "action")

              page_links_text <-
                purrr::map_chr(page_links, ~{
                  .x$getElementAttribute("outerHTML")[[1]]
                })

              page_links_numbers <-
                which(stringr::str_detect(page_links_text, "data-value"))

              page_links <- page_links[page_links_numbers]

              page_links_numbers <-
                page_links_text[page_links_numbers] %>%
                str_extract('(?<=>).*(?=<)') %>%
                as.numeric()

              page_button <-
                page_links[[which.max(page_links_numbers *
                                        (page_links_numbers <= target_page))]]
              page_button$clickElement()
              Sys.sleep(timeout / 5)

              current_page <- get_current_page()

              .upgo_env$pb(amount = current_page - last_page)

            }, silent = TRUE))

          }

        })

        Sys.sleep(timeout / 2)

      }

      skip_flag <- FALSE

    }, silent = TRUE)

  }

  # Clean up
  suppressWarnings(rm(skip_flag, target_page, page_links, page_links_text,
                      page_links_numbers, page_button))


  ### Establish last page ######################################################

  last_page_flag <- TRUE

  while (last_page_flag) {

    try({

      page_links <- remDr$findElements(using = "class name", value = "action")

      page_links_text <-
        purrr::map_chr(page_links, ~{
          .x$getElementAttribute("outerHTML")[[1]]
        })

      last_page <-
        page_links_text[
          which(stringr::str_detect(page_links_text, "data-value"))] %>%
        str_extract('(?<=>).*(?=<)') %>%
        as.numeric() %>%
        max()

      last_page_flag <- FALSE

    }, silent = TRUE)

  }

  # Clean up
  suppressWarnings(rm(last_page_flag, page_links, page_links_text))



  ### New version of loop ######################################################

  if (!quiet) handler_upgo("Downloading article batch")

  progressr::with_progress({

    .upgo_env$pb <- progressr::progressor(steps = last_page - current_page + 1)

    # Run loop until it reaches the last page
    while (current_page < last_page) {


      ## Select articles -------------------------------------------------------

      select_flag <- TRUE
      tries <- 0

      while (select_flag && tries < 10) {

        tries <- tries + 1

        try({

          current_page <- get_current_page()

          checkbox <- remDr$findElement(
            using = "xpath", value = "//*[@data-action = 'selectall']")

          while (!unlist(checkbox$isElementSelected())) {
            checkbox$clickElement()}

          Sys.sleep(stats::runif(1, timeout - 0.5, timeout + 0.5))

          total_selected <- remDr$findElement(using = "class name",
                                              value = "select")

          total_selected <- as.numeric(unlist(total_selected$getElementText()))

          # Need to reset if current_page and total_selected aren't synced
          if (total_selected != 50 && current_page != last_page) {

            clear_progress()

            } else select_flag <- FALSE

          }, silent = TRUE)

        }

      ## Check for stray downloads ---------------------------------------------

      delivery_flag <- suppressMessages(tryCatch({
        delivery_popin <- remDr$findElement(using = "id",
                                            value = "delivery-popin")
        TRUE
        }, error = function(e) FALSE))

      tries <- 0

      while (delivery_flag && tries < 10) {

        tries <- tries + 1

        try({

          delivery_popin <- remDr$findElement(using = "id",
                                              value = "delivery-popin")
          delivery_popin$clickElement()

          delivery_buttons <- remDr$findElements(
            using = "xpath",
            value = '//*[@class = "download-manual big-blue-button"]'
          )

          delivery_buttons[[1]]$clickElement()

          delivery_flag <- suppressMessages(tryCatch({
            delivery_popin <- remDr$findElement(using = "id",
                                                value = "delivery-popin")
            TRUE
          }, error = function(e) FALSE))

        }, silent = TRUE)

        delivery_flag <- suppressMessages(tryCatch({
          delivery_popin <- remDr$findElement(using = "id",
                                              value = "delivery-popin")
          TRUE
        }, error = function(e) FALSE))

      }

      # Clean up
      suppressWarnings(rm(delivery_flag, delivery_popin, delivery_buttons))


      ### Download articles ####################################################

      download_flag <- TRUE
      tries <- 0

      while (download_flag && tries < 10) {

        tries <- tries + 1

        try({

          ## Click download button ---------------------------------------------

          download_button <- remDr$findElement(
            using = "xpath",
            value = paste0("/html/body/main/div/main/div[2]/div/div[2]/div[2]/",
                           "form/div[1]/div/ul[1]/li[4]/ul/li[3]/button/span[1]"
            ))

          download_button$clickElement()

          Sys.sleep(stats::runif(1, timeout - 2, timeout + 2))


          ## Click docx button -------------------------------------------------

          docx_button <- remDr$findElement(using = "id", value = "Docx")

          docx_button$clickElement()

          Sys.sleep(timeout / 5)


          ## Enter file name ---------------------------------------------------

          file_name <- remDr$findElement(using = "id", "FileName")

          file_name$clearElement()

          file_name$sendKeysToElement(list(glue::glue(
            "{file_prefix}_{current_page}")))

          Sys.sleep(timeout / 5)


          ## Download file -----------------------------------------------------

          download <- remDr$findElement(
            using = "xpath", value = "//*[@data-action = 'download']")

          download$clickElement()

          Sys.sleep(stats::runif(1, timeout - 0.5, timeout + 0.5))

          download_flag <- FALSE

        }, silent = TRUE)


        ## Cancel download and try again if it failed --------------------------

        if (download_flag) {

          try({

            cancel <- remDr$findElements(using = "xpath",
                                         value = "//*[@data-action = 'cancel']")

            which_cancel <-
              purrr::map(cancel, ~unlist(.x$getElementAttribute("aria-label")))

            cancel <- cancel[[which(which_cancel == "Close")]]

            cancel$clickElement()

          }, silent = TRUE)

        }

      }

      go_to_next_page()

      .upgo_env$pb(amount = 1)

    }

  })


  ### Clean up and exit ########################################################

  ## Check for stray downloads one more time -----------------------------------

  delivery_flag <- suppressMessages(tryCatch({
    delivery_popin <- remDr$findElement(using = "id",
                                        value = "delivery-popin")
    TRUE
  }, error = function(e) FALSE))

  tries <- 0

  while (delivery_flag && tries < 10) {

    tries <- tries + 1

    delivery_popin <- remDr$findElement(using = "id",
                                        value = "delivery-popin")
    delivery_popin$clickElement()

    delivery_buttons <- remDr$findElements(
      using = "xpath",
      value = '//*[@class = "download-manual big-blue-button"]'
    )

    delivery_buttons[[1]]$clickElement()

    suppressMessages(tryCatch({
      delivery_popin <- remDr$findElement(using = "id",
                                          value = "delivery-popin")
    }, error = function(e) delivery_flag <- FALSE))

  }

  # Clean up
  suppressWarnings(rm(delivery_flag, delivery_popin, delivery_buttons))

  on.exit()

  total_time <- Sys.time() - start_time

  if (!quiet) message(crayon::magenta(paste0(
    "Scraping complete. ", substr(total_time, 1, 5), " ",
    attr(total_time, 'units'), "."
    )))

}
