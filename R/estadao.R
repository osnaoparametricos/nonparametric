#' Collect news from https://www.estadao.com.br/.
#'
#' This function get the news from the https://www.estadao.com.br/.
#' 
#' @param collect today, yesterday, thisweek, all or a url from the
#'     website. (Default: yesterday)
#' @param categories the comons categories on the website that are: cultura,
#' brasil, politica, esportes, tv, economia, emais,  educacao, internacional,
#' sao-paulo, opiniao, saude, sustentabilidade, ciencia, paladar, radio, viagem,
#' esportefera, others, all. (Default: all)
#' 
#' @return A tibble that contains the title, date, text and url.
#' 
#' @example
#'
#' est <- estadao(collect = "today")
#'
#' est <- estadao(collect = "today", categories = "politica")
#'
#' est <- estadao(collect = "thisweek", categories = c("politica", "brasil"))
#'
#' # It may take a while...
#' est <- estadao(collect = "all", categories = "ciencia")

estadao <- function(collect, categories = "all") {
    categories <- trimws(categories)

    categories.2 <- c("cultura", "brasil", "politica", "esportes", "tv", 
                      "economia", "emais", "educacao", "internacional",
                      "sao-paulo", "opiniao", "saude", "sustentabilidade",
                      "ciencia", "paladar", "radio", "viagem", "esportefera", 
                      "others", "all"
                      )
    
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

    x <- switch(collect,
                today = 1:5,
                yesterday = 1:10,
                thisweek = 1:30,
                all = 1:10000,
                collect
                )

    if (is.numeric(x)) {
        cat("\nCollecting URL's from www.estadao.com.br.\n\n")

        all.links <- pbmcapply::pbmclapply(x, function(i) {
            url <- sprintf("https://www.estadao.com.br/ultimas,%s", i)
            while (TRUE) {
                Sys.sleep(0.3)
                suppressMessages({
                    h <- try(XML::htmlParse(httr::GET(url, httr::config(ssl_verifypeer = FALSE))), silent = TRUE)
                })
                if (!("try-error" %in% class(h))) break
            }
            
            xpath <- paste0("//a[@class='link-title']")

            links <- XML::xpathSApply(h, xpath, XML::xmlGetAttr, "href")
            links <- if (is.list(links)) do.call(c, links) else links
            links <- links[stringr::str_detect(links, "^https")]
            links
        })
        
        all.links <- unique(do.call(c, all.links))
        ff <- stringr::str_extract(all.links, "(?<=//)[A-z-]+(?=.)")
        ff[!(ff %in% categories.2)] <- "others"
        
        if (length(categories) == 1) {
            if (categories == "all") {
                all.links <- all.links
            } else {
                all.links <- all.links[ff == categories]
            }
        } else {
            all.links[ff %in% categories]
        }
        
        cat("\nGetting the news from each URL.\n\n")
    } else {
        cat("\nGetting the news from URL.\n\n")
        all.links <- x
    }
    
    content <- pbmcapply::pbmclapply(all.links, function(l) {
        while (TRUE) {
            Sys.sleep(0.3)
            h <- httr::GET(l, httr::config(ssl_verifypeer = FALSE))
            h <- try(XML::htmlParse(h, encoding = "utf-8"), silent = TRUE)
            if (!("try-error" %in% class(h))) break
        }

        xpath <- c(
            # Title
            "//h1[@class='n--noticia__title' or @class='n--noticia__title ' or @class='post-title']",
            # Author
            paste0("//div[@class='n--noticia__state']/p/span|//div[@class='n--noticia__state-title']",
                   "|//p[@class='credits']|//div[@class='credits']"),
            # Date
            paste0("//div[@class='n--noticia__state']/p[2]|//div[@class='n--noticia__state-desc']/p|",
                   "//p[@class='post-date']|//div[@class='post-date']"),
            # Text
            paste0("//div[@class='n--noticia__content content' or @class='content-text content post-content'",
                   " or @class='content n--noticia__content']//p|",
                   "//div[@class='n--noticia__content content' or @class='content-text content post-content'",
                   " or @class='content n--noticia__content']//li|",
                   "//article[@class='content-text content']//p|//article[@class='content-text content']//li")
        )

        content <- lapply(xpath, function(l) {
            out <- trimws(XML::xpathSApply(h, l, XML::xmlValue))
            if (length(out) == 0) return(NA) else out
        })

        # fix date to format yyyy-mm-dd
        if (is.na(content[[3]])) {
            content[[3]] <- content[[3]]
        } else if (stringr::str_detect(content[[3]], "[0-9]{2} de .+ de [0-9]{4}")) {
            content[[3]] <- lubridate::dmy(stringr::str_extract(content[[3]], "[0-9]{2} de .+ de [0-9]{4}"))
        } else if (stringr::str_detect(content[[3]], "[0-9]{2}/[0-9]{2}/[0-9]{4}")) {
            content[[3]] <- lubridate::dmy(stringr::str_extract(content[[3]], "[0-9]{2}/[0-9]{2}/[0-9]{4}"))
        } else {
            content[[3]] <- content[[3]]
        }
        
        content[[4]] <- paste0(content[[4]], collapse = "\n")
        # add url
        content <- append(content, l)
        names(content) <- c("title", "author", "date", "text", "url")
        content <- dplyr::as_tibble(content)
        content
    })

    content <- dplyr::bind_rows(content)
    content$text[content$text == "NA"] <- NA

    # Remove news where the main text wasn't collected
    # generally news about photos,  tv etc...
    content <- tidyr::drop_na(content, title)

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
