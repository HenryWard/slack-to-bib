#!/usr/bin/env Rscript

suppressWarnings(library(methods))
suppressWarnings(library(optparse))
suppressWarnings(library(knitcitations))
suppressWarnings(library(httr))
suppressWarnings(library(jsonlite))

######
# MAIN FUNCTIONS
######

# Gets messages in a specified date range from slack channel
get_slack_messages <- function(token, channel_id, latest = "now",
                               oldest = 0) {
  response <- POST(url="https://slack.com/api/channels.history",
                   body=list(token=token,
                             channel=channel_id,
                             count=1000,
                             oldest=oldest))
  message_table <- fromJSON(content(response, as="text"))$messages
  return(message_table)
}

# Adds bib entry to bibtex file, creating it if nonexistent
write_bib <- function(bib_file, append = TRUE) {
  if (!file.exists(bib_file)) {
    write.bibtex(file = bib_file)
  } else {
    write.bibtex(file = bib_file, append = append)
  }
}

# Filters messages into a list of valid DOIs
messages_to_doi <- function(messages) {
  messages$text <- tolower(messages$text)
  dois <- gsub("[\r\n]", " ", messages$text)
  dois <- unlist(strsplit(dois, " "))
  
  # Gets dois separated by whitespace after a colon
  whitespace_dois <- which(dois=="doi:")
  whitespace_dois <- dois[whitespace_dois+1]
  
  # Gets remaining dois
  dois <- dois[grepl("doi", dois)]
  dois <- dois[grepl("\\d", dois)]
  
  # Filters and returns dois
  dois <- c(dois, whitespace_dois)
  dois <- filter_doi(dois)
  print(dois)
  return(dois)
}

# Cleans dois
filter_doi <- function(doi) {
  doi <- gsub("<", "", doi)
  doi <- gsub(">", "", doi)
  doi <- gsub(",", "", doi)
  doi <- gsub("\"", "", doi)
  doi <- gsub("\'", "", doi)
  doi <- gsub("doi:", "", doi)
  doi <- doi[doi != ""]
  return(doi)
}

# Turns a doi string into a bibtex citation
doi_to_bib <- function(doi) {
  
  # Captures links to doi.org and ignores other links
  if (startsWith(doi, "https") ) {
    doi_temp <- substr(doi, 9, nchar(doi))
    if (startsWith(doi_temp, "doi.org")) {
      doi <- substr(doi, 17, nchar(doi))
    } else { 
      return(FALSE)
    }
  } else if (startsWith(doi, "http")) {
    doi_temp <- substr(doi, 9, nchar(doi))
    if (startsWith(doi_temp, "doi.org")) {
      doi <- substr(doi, 16, nchar(doi))
    } else { 
      return(FALSE)
    }
  }
  
  # Gets citation and stores in session memory
  citep(doi)
}

# Manually appends a doi to a bib file
append_doi <- function(doi, bib_file) {
  doi <- filter_doi(doi)
  if (substr(doi, 1, 5) == "https") {
    doi <- substr(doi, 17, nchar(doi))
  } else if (substr(doi, 1, 4) == "http") {
    doi <- substr(doi, 17, nchar(doi))
  }
  citep(doi)
  write_bib(bib_file, append = TRUE)
}

# Main function
slack_to_bib <- function(slack_token_file, channel_id, bib_file,
                         last_timestamp_file = NA, oldest = "all") {
  
  # Converts oldest to given range from now in seconds since epoch
  if (oldest == "week") {
    oldest = as.integer(as.POSIXct(Sys.time() - 604800)) 
  } else if (oldest == "day") {
    oldest = as.integer(as.POSIXct(Sys.time() - 86400)) 
  } else if (oldest == "all") {
    oldest = 0 
  }
  
  # Loads timestamp from file if it exists, and writes timestamp to file
  last_timestamp <- NA
  if (!is.na(last_timestamp_file)) {
    f <- file(last_timestamp_file)
    if (file.exists(last_timestamp_file)) {
      last_timestamp <- readLines(f)[[1]]
    }
    writeLines(as.character(Sys.time(), f))
    close(f)
  }
  
  # Overwrites oldest with timestamp if specified
  if (!is.na(last_timestamp)) {
    oldest <- as.POSIXct(last_timestamp)
  }
  
  # Gets slack messages
  token <- readLines(slack_token_file)[[1]]
  messages <- get_slack_messages(token, channel_id, oldest = oldest)
  
  # Gets all DOIs in messages
  dois <- messages_to_doi(messages)
  
  # Resets knitcitations citation list and sets format to pandoc
  cleanbib()
  options("citation_format" = "pandoc")
  
  # Writes dois to file
  for (doi in dois) {
    doi_to_bib(doi)
  }
  write_bib(bib_file)
}

######
# COMMAND LINE INTERFACE
######

# Gets and parses args
args <- commandArgs()
options = list(
  make_option(c("-t", "--token_file"), type="character", default=NULL, 
              help="path to file containing slack API token"),
  make_option(c("-c", "--channel_id"), type="integer", default=NULL, 
              help="target slack channel id"),
  make_option(c("-b", "--bib_file"), type="character", default="citations.bib", 
              help="path to bibtex file to output or update [default= %default]"),
  make_option(c("-l", "--last_timestamp_file"), type="character", default=NA, 
              help="path to file containing the last timestamp [default= %default]"),
  make_option(c("-o", "--oldest"), type="character", default="week", 
              help="range of days to check slack history for [default= %default]")
)

# Extracts variables from args
options <- parse_args(OptionParser(option_list = options))
token_file <- options$token_file
channel_id <- options$channel_id
bib_file <- options$bib_file
last_timestamp_file <- options$last_timestamp_file
oldest <- options$oldest

# Calls main function
slack_to_bib(token_file, channel_id, bib_file,
             last_timestamp_file, oldest)

