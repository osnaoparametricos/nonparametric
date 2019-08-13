#' Collect news from www1.folha.uol.com.br.
#'
#' This function get the news from the www1.folha.uol.com.br.
#' 
#' @param collect today, yesterday, thisweek, all or a url from the
#'     website. (Default: yesterday)
#' @param categories the comons categories on the website that are:
#' opiniao, poder, mercado, mundo, cotidiano, equilibrioesaude, esporte,
#' ilustrada, all (Default: all)
#' 
#' @return A tibble that contains the title, date, text and url.
#' 
#' @example
#'
#' fsp <- folhasp(collect = "today")
#'
#' fsp <- folhasp(collect = "today", categories = "poder")
#'
#' fsp <- folhasp(collect = "thisweek", categories = c("poder", "mundo"))
#'
#' # It may take a while...
#' fsp <- folhasp(collect = "all", categories = "cotidiano")

folhasp <- function(collect, categories = "all") {
    categories <- unique(categories)
    categories.2 <- c("all", "opiniao", "poder", "mercado", "mundo", "cotidiano",
                      "equilibrioesaude", "esporte", "ilustrada")
    
    # Check args ---------------------------------
    Check <- ArgumentCheck::newArgCheck()
    
    if (!(collect %in% c("today", "yesterday", "thisweek", "all") | 
          grepl("^https|^http", collect))) {
        ArgumentCheck::addError(
                           msg = paste0("collect must be one of the possible arguments:\n\t", 
                                        "'today', 'yesterday', 'thisweek', 'all' or a url\n"),
                           argcheck = Check
                       )
    }

    ## Categories
    if (!all(categories %in% categories.2) | is.null(categories) | categories == "") {
        ArgumentCheck::addError(
                           msg = paste0("Categories must be one or more of the following:  ",
                                        paste0(categories.2, collapse = ", "), "\n\n"),
                           argcheck = Check
                       )
    }
    
    ArgumentCheck::finishArgCheck(Check)
    # --------------------------------------------
    

    if (collect %in% c("today", "yesterday", "thisweek", "all")) {
        x <- c(
            "https://www1.folha.uol.com.br/opiniao/#100",
            "https://www1.folha.uol.com.br/poder/#100",
            "https://www1.folha.uol.com.br/mercado/#100",
            "https://www1.folha.uol.com.br/mundo/#100",
            "https://www1.folha.uol.com.br/cotidiano/#100",
            "https://www1.folha.uol.com.br/equilibrioesaude/#100",
            "https://www1.folha.uol.com.br/esporte/#100",
            "https://www1.folha.uol.com.br/ilustrada/#100"
        )

        if (sum("all" %in% categories) != 1) {
            x <- x[which(categories %in% stringr::str_extract(x, "(?<=/)[A-z]+(?=/)"))]
        }
        
        cat("\nCollecting URL's from www1.folha.uol.com.br.\n\n")
        
        all.links <- pbmcapply::pbmclapply(x, function(i) {
            url <- i
            while (TRUE) {
                Sys.sleep(0.3)
                suppressMessages({
                    h <- try(XML::htmlParse(httr::GET(url, httr::config(ssl_verifypeer = FALSE))), silent = TRUE)
                })
                if (!("try-error" %in% class(h))) break
            }
            
            xpath <- paste0("//a[@class='c-main-headline__url']|//div[@class='c-headline__content']/a")

            links <- XML::xpathSApply(h, xpath, XML::xmlGetAttr, "href")
            links <- if (is.list(links)) do.call(c, links) else links
            links <- links[stringr::str_detect(links, "^https")]
            links
        })
        
        all.links <- unique(do.call(c, all.links))
        
        cat("\nGetting the news from each URL.\n\n")
    } else {
        cat("\nGetting the news from URL.\n\n")
        all.links <- collect
    }

    content <- pbmcapply::pbmclapply(all.links, function(l) {
        while (TRUE) {
            Sys.sleep(0.3)
            suppressMessages({
                h <- httr::GET(l, httr::config(ssl_verifypeer = FALSE))
                h <- try(XML::htmlParse(h, encoding = "UTF-8"), silent = TRUE)
            })
            if (!("try-error" %in% class(h))) break
        }

        xpath <- c(
            # Title
            paste0("//h1[@class='c-content-head__title' or @class='news__title' or ",
                   "@class='c-content-head__title c-content-head__title--italic']|",
                   "//h2[@class='c-content-head__title' or @class='news__title' or ",
                   "@class='c-content-head__title c-content-head__title--italic']|",
                   "//div[@class='c-content-head__wrap']//h1"), 
            # Date
            paste0("//div[@class='c-more-options__header' or ",
                   "@class='c-more-options__published-date']/time|",
                   "//time[@class='c-content-head__time']"),
            # Text
            paste0("//div[@class='c-news__body' or @class='c-news__content']//p|",
                   "//div[@class='c-news__body' or @class='c-news__content']//li|",
                   "//div[@class='j-paywall news__content js-news-content js-disable",
                   "-copy js-tweet-selection']//p|",
                   "//div[@class='j-paywall news__content js-news-content js-disable",
                   "-copy js-tweet-selection']//li")
            
        )

        content <- lapply(xpath, function(l) {
            out <- trimws(XML::xpathSApply(h, l, XML::xmlValue))
            if (length(out) == 0) return(NA) else out
        })
        
        # fix date to format yyyy-mm-dd
        content[[2]] <- lubridate::dmy(stringr::str_extract(content[[2]], "^[0-9]{1,2}.[a-z]{3}.[0-9]{4}"))
        # fix text
        content[[3]] <- paste0(content[[3]], collapse = "\n")
        # add url
        content <- append(content, l)
        names(content) <- c("title", "date", "text", "url")
        content <- dplyr::as_tibble(content)
        content
    })

    content <- dplyr::bind_rows(content)
    content$text[content$text == "NA"] <- NA

    # Remove news where the main text wasn't collected
    # generally news about photos,  tv etc...
    content <- tidyr::drop_na(content, title, date)

    # Remove emojis
    content$text <- gsub("[^[:cntrl:][:alnum:][:blank:]!\",.><()&/+-~{}^#:=@|]", "", content$text)

    if (collect %in% c("today", "yesterday", "thisweek")) {
        f <- switch(collect,
                    today = Sys.Date(),
                    yesterday = Sys.Date() - 1,
                    thisweek = Sys.Date() - 7
                    )
        content <- dplyr::filter(content, date >= !!f)
    }

    return(content)
}

