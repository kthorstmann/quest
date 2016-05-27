
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

# should return a list of items as the table form, an object "quest"
# exCodebook <- function(){
#
# }



# helper: make one scale to a list -----------------------------------


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


# helper: make a scale (list) to a recoded data set ------------------

# takes a scale and recodes the items and makes a new data set,
# with the recoded items

scales <-  c("E = 1r, 6, 11R, 16",
             "A = 2r, 7, 12r, 17r",
             "C = 3, 8r, 13, 18",
             "N = 4, 9r, 14, 19",
             "O = 5, 10, 15, 20 21")

# data <- bigfive[grep(x = names(bigfive), pattern = "BFI_")]

codeScales <- function(data, scales, recode = TRUE){
  stopifnot(is.data.frame(data))

  if (!requireNamespace("purrr", quietly = FALSE)) {
    itemkeys <- purrr::map(scales, scale2list)
  } else {
    itemkeys <- lapply(scales, scale2list)
  }

# extract information
  # scale.name <- itemkeys[[1]]["name"]
  # scale.items <- itemkeys[[1]]["items"]
  # scale.recode <- itemkeys[[1]]["recode"]

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

## add recode function, is currently developed, see below.


## reutrn only the new data frame

data[unlist(scale.items)]
# data frame


}



# recode items -------------------------------------------------------

# input is a vector of item positions or names of items to recode
# library(quest)
# THESTTHAT value.check is between 100 and 0

# data <- data.frame(a = c(1, 2, -99), b = c(3, 4, NA))
# data <- bigfive[grep(x = names(bigfive), pattern = "BFI_")]
# data <- data[1:10, 1:10]
# recode <- c(6, 10)


# recodeQuick(data, recode, NULL)




## FEHLER: Funktion rekodiert hier die falschen Werte in den falschen Spalten

## change this to the valid values that exist, and only run the check if there is a null.

# give valid values as input?

# or name all values that should be excluded from recoding?

# valid.values = NULL
# data[2, 3] <- -99
# recodeQuick(data, recode = c(1, 2), 1:4)

recodeQuick <- function(data, recode, valid.values = NULL){

  stopifnot(is.data.frame(data))

  # drop NA
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

  recode.hits.data <- purrr::map(include.values, ~ data == .)

  for (i in seq_along(include.values)) {
    data[recode.hits.data[[i]]] <- rev(include.values)[i]
  }

  # if (FALSE) { ## this works, maybe faster, but is maybe dangerous?
  #   rec <- function(x, y) {
  #     data[x] <<- y ## careful, double arrow
  #     data
  #   }
  #   purrr::map2(.x = recode.hits.data, .y = rev(include.values), ~ rec(.x, .y))
  # }


  message("The following values were recoded: \n ",
          paste(paste(include.values,"to", rev(include.values), collapse = ", ")))
  data

  }




# function: make measurement models ----------------------------------







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



