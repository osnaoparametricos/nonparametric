#' Collect news from g1.globo.com.
#'
#' This function get the news from the www.g1.globo.com.
#' 
#' @param collect  today, yesterday, thisweek, all or a url from the website. (Default: yesterday)
#' 
#' @return A tibble that contains the title, author, date,  meta.title, text, tags and url.
#' 
#' @example
#'
#' # approx 21 sec
#' g1 <- g1globo(collect = "to day")
#' 
#' # approx 5 min
#' g1 <- g1globo(collect = "yesterday")
#'
#' # approx 1 min and 40 sec
#' g1 <- g1globo(collect = "thisweek")
#'
#' # approx 1 hour and 30 min
#' g1 <- g1globo(collect = "all")


g1globo <- function(collect = "yesterday") {
    # Check args ---------------------------------
    Check <- ArgumentCheck::newArgCheck()

    if (!(collect %in% c("today", "yesterday", "thisweek", "all") | 
        grepl("^https://g1.globo.com.+ghtml$|^http://g1.globo.com.+ghtml$", collect))) {
        ArgumentCheck::addError(
                           msg = paste0("collect must be one of the possible arguments:\n\t", 
                                        "'today', 'yesterday', 'thisweek', 'all' or a url\n"),
                           argcheck = Check
                       )
    }

    ArgumentCheck::finishArgCheck(Check)
    # --------------------------------------------

    x <- switch(collect,
                today = 1:5,
                yesterday = 1:10,
                thisweek = 1:30,
                all = 1:2000,
                collect
                )

    if (is.numeric(x)) {
        cat("\nCollecting URL's from www.g1globo.com.\n\n")

        all.links <- pbmcapply::pbmclapply(x, function(i) {
            url <- sprintf("https://g1.globo.com/index/feed/pagina-%s.ghtml", i)
            while (TRUE) {
                Sys.sleep(0.3)
                suppressMessages({
                    h <- try(XML::htmlParse(httr::GET(url, httr::config(ssl_verifypeer = FALSE))), silent = TRUE)
                })
                if (!("try-error" %in% class(h))) break
            }
            
            xpath <- paste0("//div[@class='feed-post-body-title gui-color-primary ",
                            "gui-color-hover ']//a"
                            )

            links <- XML::xpathSApply(h, xpath, XML::xmlGetAttr, "href")
            links <- if (is.list(links)) do.call(c, links) else links
            links <- links[stringr::str_detect(links, "^https")]
            links <- links[stringr::str_detect(links, ".ghtml$|.html$")]
            links
        })

        all.links <- unique(do.call(c, all.links))
        
        cat("\nGetting the news from each URL.\n\n")
    } else {
        cat("\nGetting the news from URL.\n\n")
        all.links <- x
    }
    
    content <- pbmcapply::pbmclapply(all.links, function(l) {
        while (TRUE) {
            Sys.sleep(0.3)
            suppressMessages({
                h <- try(XML::htmlParse(httr::GET(l, httr::config(ssl_verifypeer = FALSE))), silent = TRUE)
            })
            if (!("try-error" %in% class(h))) break
        }

        xpath <- c(
            # Title
            "//div[@class='title']",
            # Author
            "//p[@class='content-publication-data__from']",
            # Date
            "//p/time",
            # Meta.title
            "//div[@class='medium-centered subtitle']",
            # Text
            "//div[@class='mc-column content-text active-extra-styles ']",
            # Tags
            "//li[@class='entities__list-item']"
        )

        content <- lapply(xpath, function(l) {
            out <- trimws(XML::xpathSApply(h, l, XML::xmlValue))
            if (length(out) == 0) return(NA) else out
        })
        
        # fix date to format yyyy-mm-dd
        content[[3]] <- lubridate::dmy(stringr::str_extract(content[[3]], "[0-9]{2}/[0-9]{2}/[0-9]{4}"))
        # add url
        content <- append(content, l)
        content[[5]] <- paste0(content[[5]], collapse = " \n ")
        content[[6]] <- paste0(content[[6]], collapse = " - ")
        names(content) <- c("titulo", "autor", "data", "metatitulo", "texto", "tags", "url")
        content <- dplyr::as_tibble(content)
        content
    })
    
    content <- dplyr::bind_rows(content)
    content$texto[content$texto == "NA"] <- NA
    content$tags[content$tags == "NA"] <- NA

    # Remove news where the main text wasn't collected
    # generally futebol news.
    content <- tidyr::drop_na(content, titulo)

    # Remove emojis
    content$texto <- gsub("[^[:cntrl:][:alnum:][:blank:]!\",.><()&/+-~{}^#:=@|]", "", content$texto)

    if (collect %in% c("today", "yesterday", "thisweek")) {
        f <- switch(collect,
                    today = Sys.Date(),
                    yesterday = Sys.Date() - 1,
                    thisweek = Sys.Date() - 7
                    )
        content <- dplyr::filter(content, data >= !!f)
    }

    return(content)
}

