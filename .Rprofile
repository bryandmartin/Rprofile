# options(stringsAsFactors = FALSE)
options(max.print = 100)
options(scipen = 10)
options(menu.graphics = FALSE)
options(prompt = "> ")
options(continue = "... ")
options(useFancyQuotes = FALSE)
options(width = 80)
options(repos = c(CRAN = "https://cloud.r-project.org"))


options(warnPartialMatchAttr = TRUE,
        warnPartialMatchDollar = TRUE,
        warn = 1,
        warning.length = 8170)

# Autocomplete package names
utils::rc.settings(ipck = TRUE)

.First <- function(){
  if (interactive()) {
    library(utils)
    timestamp(,prefix = paste("##------ [",getwd(),"] ", sep = ""))
  }
}

.Last <- function(){
  if (interactive()) {
    hist_file <- Sys.getenv("R_HISTFILE")
    if (hist_file == "") hist_file <- "~/.RHistory"
    savehistory(hist_file)
  }
}


sshhh <- function(a.package){
  suppressWarnings(suppressPackageStartupMessages(
    library(a.package, character.only = TRUE)))
}

# auto.loads <- c("dplyr", "ggplot2", "stringr")
# 
# if (interactive()) {
#   invisible(sapply(auto.loads, sshhh))
# }
# rm(auto.loads)
# rm(sshhh)



.env <- new.env()

.env$q <- function(save = "no", ...) {
  quit(save = save, ...)
}
.env$cleannames <- function(dat){
  thenames <- names(dat)
  nospace <- stringr::str_replace_all(thenames, "\\s+", "_")
  nobad <- stringr::str_replace_all(nospace, "[^_A-Za-z1-9]", "_")
  return(nobad)
}

.env$setcleannames <- function(dat){
  setnames(dat, cleannames(dat))
}

.env$unrowname <- function(x) {
  rownames(x) <- NULL
  x
}

.env$unfactor <- function(df){
  id <- sapply(df, is.factor)
  df[id] <- lapply(df[id], as.character)
  df
}

.env$corner <- function(x, n = 4) {
  if (class(x) == "matrix" | class(x) == "data.frame") x[1:4, 1:4]
}

.env$macopen <- function(...) if (Sys.info()[1] == "Darwin") system("open .")


.env$.ls.objects <- function(pos = 1, pattern, order.by,
                             decreasing = FALSE, head = FALSE, n = 5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    utils::capture.output(format(utils::object.size(x), units = "auto")) })
  obj.size <- napply(names, utils::object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing = decreasing), ]
  if (head)
    out <- utils::head(out, n)
  out
}

.env$lsos <- function(..., n = 10) {
  .ls.objects(..., order.by = "Size", decreasing = TRUE, head = TRUE, n = n)
}

attach(.env, warn.conflicts = FALSE)

message("*** Successfully loaded .Rprofile ***")
