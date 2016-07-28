
# general information ------------------------------------------------

# data frame with items
# matrix with items
# recoding of items
# making scales of items
# making scales with measurement models
# data frame

# this function extracts all scales from a codebook and turns them into a scale
# the codebook is a data frame with several colums that indicate
# - item
# - short-item
# - scales
# - items to recode


# make scales into a readable list -----------------------------------


#' Transform a scale into a readable list
#'
#' This function turns a scale in the form of \code{A = 1, 5, 10R, 15r} into a list which of \enumerate{
#'  \item the scale used
#'  \item the name of the scale
#'  \item the items to use for the scale (by position in data frame or questionnaire)
#'  \item the items that need to be recoded.
#' }
#'
#'Items that need to be recoded are indicated using an \code{R} or \code{r} behind the number of the item.
#'
#' @param scale A description of a scale in the form of \code{scale <- c(name = 1, 2, 3, 4r, 5)}. The scale is defined using \code{=}, the items by their position in the data frame or questionnaire (this should correspond!).
#'
#' @return
#' Returns a list that list that contains four entries: The input used (\code{original.scale}), the derived name of the scale (\code{name}), the items that constitute the scale \code{items}, the items that need to be recoded (\code{recode})
#' @export
#'
#' @examples
#' scales = c("E = 1r, 6, 11R, 16",
#'            "A = 2r, 7, 12r, 17r",
#'            "C = 3, 8r, 13, 18",
#'            "N = , 9r, 14, 19",
#'            "O = 4, 10RR, 15, 20 21")
#' scale2list(scales[1])

scale2list <- function(scale){
  if (!is.character(scale)) {
    stop("Input needs to be a character vector. See help for input", call. = TRUE)
  }
  if ( i <- grep("=", scale) != 1) {
    stop ("in definiton of scales, there needs to be at least one `=` per scale")
  }

  # extract name of scale (later to be used everywhere, e. g. in lavaan)
  name.space <- unlist(strsplit(scale, "=", fixed = TRUE))[1]
  name.scale <- gsub(x = name.space, pattern = " ", replacement = "")
  if (nchar(name.scale) < 1) {
    stop ("one scale does not have a name, please check scale input format, e. g. `extra = 1, 4, 8, ...` ")
  }
  if(unlist(strsplit(name.scale, "[^0-9]+")) != "") {stop ( "no numerics allowed in name of scale: `", name.scale, "`. Please rename scale")}

  # extract items for scales
  items.raw <- unlist(strsplit(scale, "="))
  stopifnot(length(items.raw) == 2)
  items.raw <- items.raw[2]
  items <- unique(na.omit(as.numeric(unlist(strsplit(items.raw, "[^0-9]+")))))
  if (length(items) == 0) {
    stop ("one scale does not have items, please check scale input format, e. g. `extra = 1, 4, 8, ...` ")
  }

  # get items that need recoding
  # this works, but does only the replacement ## maybe run this with stringr
  gsub("\\d+[rR]", "---", items.raw)
  pos <- gregexpr(pattern = "\\d+[rR]", text = items.raw)
  start <- pos[[1]]
  stop <- start + attr(pos[[1]], "match.length")-2

  if (!requireNamespace("purrr", quietly = FALSE)) {
    to.recode <- unlist(purrr::map2(.x = start,
                                    .y = stop,
                                    .f = ~ substr(items.raw,start = .x, stop = .y)))
  } else {
    to.recode <- mapply(function(x, y) substr(items.raw, start = x,
                                              stop = y), x = start, y = stop)
  }
  to.recode <- as.numeric(to.recode)

  result <- list(original.scale = paste("The scale used was", scale),
                 name = name.scale,
                 items = items,
                 recode = to.recode)
  result
}

# take scales and make recoded data set ------------------------------

# takes a scale and recodes the items and makes a new data set,
# with the recoded items

scales <-  c("E = 1r, 6, 11R, 16",
             "A = 2r, 7, 12r, 17r",
             "C = 3, 8r, 13, 18",
             "N = 4, 9r, 14, 19",
             "O = 5, 10, 15, 20 21")

data <- bigfive[grep(x = names(bigfive), pattern = "BFI_")]

codeScales <- function(data, scales, recode = TRUE, valid.values = NULL){


  ### beg of section that is identical ____________________________________

  stopifnot(is.data.frame(data))

  if (!requireNamespace("purrr", quietly = FALSE)) {
    itemkeys <- purrr::map(scales, scale2list)
  } else {
    itemkeys <- lapply(scales, scale2list)
  }

  # check data frame TESTTHAT
  if (!requireNamespace("purrr", quietly = FALSE)) {
    all.names <- purrr::map(itemkeys,  ~.x$name)
    all.items <- purrr::map(itemkeys,  ~.x$items)
    all.recodes <- purrr::map(itemkeys,  ~.x$recode)
  } else {
    all.names <- lapply(itemkeys, function(x) x$name)
    all.items <- lapply(itemkeys, function(x) x$items)
    all.recodes <- lapply(itemkeys, function(x) x$recode)
  }

  number.of.items <- length(unlist(all.items))
  if (number.of.items == 0) {stop ("Number of items indicated in scales is 0. Please check input") }

  if (ncol(data) != number.of.items) {
    stop("The data frame you inserted contains ", if(ncol(data) < number.of.items) {"less"} else {"more"}, " colmns (", ncol(data), ") than the number of items (", number.of.items, ") you indicated. Please correct either of them. Number of columns must be equal to number of items.")
  }

  # make duplicate check in shortkeys
  if (any(table(unlist(all.items)) > 1)) {
    message("You have used the following item more than once: ",
            names(table(unlist(all.items))[table(unlist(all.items)) > 1]),
            ". Please check if correct.")
  }
  if(!setequal(seq_along(unlist(all.items)), sort(unlist(all.items)))) {
    message("You have inserted ", max(unlist(all.items)), " items, but you have not used every item from 1 to ", max(unlist(all.items)), ". Please check if correct")
  }
  ### end of section that is identical ____________________________________

  if (recode) {
    all.recodes <- na.omit(unlist(all.recodes))
    data <- recodeQuick(data, recode = all.recodes, valid.values = valid.values)
  } else {
    data <- data
  }

  ## computing the final scores

  scored.scales <- purrr::map(all.items, ~ rowMeans(data[.x], na.rm = TRUE))
  names(scored.scales) <- all.names
  scored.scales <- as.data.frame(scored.scales)

  ## make it optional to get measurement models?
  return(data)
}





# recode items in data set -------------------------------------------

# input is a vector of item positions or names of items to recode
# get as input not only the position but also the names, so either option.

data <- data.frame(a = c(1, 2, -99), b = c(3, 4, NA))
recodeQuick(data, recode = "a", valid.values = c(1:4))
## throws error becuase there is neither TRUE FALSE, but NA

data <- bigfive[grep(x = names(bigfive), pattern = "BFI_")]
data <- data[1:10, 1:10]
data[1, 6] <- -99
recode <- c(6, 10)
recodeQuick(data, recode = c(6, 10), valid.values = c(1:4))

recodeQuick <- function(data, recode, valid.values = NULL){

  stopifnot(is.data.frame(data))


  # check that values are only integers:
  data.check <- as.data.frame(purrr::map(data, as.integer))

  if (any(data.check != data, na.rm = TRUE)) {
    message("Some values are not integers. Please check your input if neccessary")
  }
  rm(data.check)

  # drop NA from recode
  recode <- recode[!is.na(recode)]

  if (!(is.numeric(recode) || is.character(recode))) {
    stop("Input to `recode` must either be numeric or character (i. e. the
         position or  the name of the items `data`")
  }

  # check if positions are all part of the data frame
  if (is.numeric(recode)) {
    recode.names <- names(data)[recode]
    if (any(is.na(recode.names))) {
      stop("you have provided the position of items to be recoded that are
           not included in the data frame. Please check input.")
    }
  }

  # check if names are all part of the data frame
  if (is.character(recode)) {
    recode.names <- recode
    if (any(!recode.names %in% names(data))) {
      comprsn <- recode.names %in% names(data)
      stop("Your have provided names of items to be recoded that \n do not
           exist in your data frame: \n", recode.names[!comprsn],
           "\n Please check input.")
    }
  }

  # make data frame that is checked for recode:
  data.recode <- data[recode.names]


  # check values in data frame, are all recodes present, some missings?
  if (is.null(valid.values)) {
    table.values <- table(unlist(data.recode))
    include.values <- as.numeric(names(table.values))
  } else {
    include.values <- valid.values
  }

  recode.hits.data <- purrr::map(include.values, ~ data.recode == . )

  for (i in seq_along(include.values)) {
    data.recode[recode.hits.data[[i]]] <- rev(include.values)[i]
  }

  # merge two data frames again
  data[recode.names] <- data.recode[recode.names]

  message("The following values were recoded: \n ",
          paste(paste(include.values,"to", rev(include.values), collapse = ", ")))
  data
}


# function: make scores from data set --------------------------------

## get measurement models

# input: data and keys
# output: measurement models as text

scales <-  c("E = 1r, 6, 11R, 16",
             "A = 2r, 7, 12r, 17r",
             "C = 3, 8r, 13, 18",
             "N = 4, 9r, 14, 19",
             "O = 5, 10, 15, 20 21")
data <- bigfive[grep(x = names(bigfive), pattern = "BFI_")]
library(quest)


mModels <- function(data, scales, run.models, ...){

  ### this section identical to section in other functions ________________

  stopifnot(is.data.frame(data))

  if (!requireNamespace("purrr", quietly = FALSE)) {
    itemkeys <- purrr::map(scales, scale2list)
  } else {
    itemkeys <- lapply(scales, scale2list)
  }

  # check data frame TESTTHAT
  if (!requireNamespace("purrr", quietly = FALSE)) {
    all.names <- purrr::map(itemkeys,  ~.x$name)
    all.items <- purrr::map(itemkeys,  ~.x$items)
  } else {
    all.names <- lapply(itemkeys, function(x) x$name)
    all.items <- lapply(itemkeys, function(x) x$items)
  }

  number.of.items <- length(unlist(all.items))
  if (number.of.items == 0) {stop ("Number of items indicated in scales is 0. Please check input") }

  if (ncol(data) != number.of.items) {
    stop("The data frame you inserted contains ", if(ncol(data) < number.of.items) {"less"} else {"more"}, " colmns (", ncol(data), ") than the number of items (", number.of.items, ") you indicated. Please correct either of them. Number of columns must be equal to number of items.")
  }

  # make duplicate check in shortkeys
  if (any(table(unlist(all.items)) > 1)) {
    message("You have used the following item more than once: ",
            names(table(unlist(all.items))[table(unlist(all.items)) > 1]),
            ". Please check if correct.")
  }
  if(!setequal(seq_along(unlist(all.items)), sort(unlist(all.items)))) {
    message("You have inserted ", max(unlist(all.items)), " items, but you have not used every item from 1 to ", max(unlist(all.items)), ". Please check if correct")
  }

  ### end of section that is identical ____________________________________


## make measurement models:

  item.names.text <- purrr::map(all.items, ~ names(data)[.])
  manifests.string <- purrr::map(item.names.text, ~ stringr::str_c(., collapse = " + "))
  ## get the models:
  m.models <- purrr::map2(.x = all.names, .y = manifests.string, ~ stringr::str_c(.x, .y, sep = " =~ "))

  if (run.models) {
    model.fits <- purrr::map(m.models, ~ cfa(model = . , data = data, ...))
    return(model.fits)
  } else {
    return(m.models)
  }

}

# function: make measurement models ----------------------------------

# get recoded data frame with eventual scales

## input data
bfi.data <- data[grep(x = names(data), pattern = "BFI_", value = TRUE)]
data <- bfi.data

## this needs to be addded into a function:
recodes <- na.omit(unlist(purrr::map(seq_along(itemkeys), ~ itemkeys[[.]]$recode)))
bfi.recoded <- recodeQuick(bfi.data, recode=recodes)

## now make skales, total scale scores:


items <- purrr::map(seq_along(itemkeys), ~ itemkeys[[.]]$items)
scores <- purrr::map(seq_along(items), ~ rowMeans(bfi.recoded[items[[.]]], na.rm = TRUE)) ## add here the possibility to remove items when neccessary.

# scores of items
names(scores) <- purrr::map(seq_along(itemkeys), ~ itemkeys[[.]]$name)

final.data <- cbind(bfi.recoded, data.frame(scores))





# helper: keys of important questionnaires ---------------------------

#' Short Keys for Questionnaires
#'
#' Insert the commonly used short name of a questionnaire to generate the
#'
#' Currently available questionnaires are:
#' \itemize {
#' \item \href{www.google.com}{BFI-K} QUELLE LINK
#' \item NEO-PI-R
#' }
#'
#'
#' @param keytable The short name of the questionnaire that should be analyzed. See details for all available questionnaires.
#'
#' @return Returns a vector with short keys readable by the package \code{quest} for further analyses.
#'
#'
#'
#' @export
#'
#' @examples
#' shortKeys("BFI-K")
shortKeys <- function(keytable = "BFI-K") {
  stopifnot(is.character(keytable))
  if (keytable == "BFI-K") {
    scales <-  c("E = 1r, 6, 11R, 16",
                 "A = 2r, 7, 12r, 17r",
                 "C = 3, 8r, 13, 18",
                 "N = 4, 9r, 14, 19",
                 "O = 5, 10, 15, 20 21")
  } else if (keytable == "NEO-PI-R") {

  } else {
    stop("Short name of questionnaire you indicated not found.")
  }
  message("You have selected a pre-defined shortkey for your questionnaire. Please check that the order of itmes in keytable corresponds to order of items in your data frame!")
  scales
}



