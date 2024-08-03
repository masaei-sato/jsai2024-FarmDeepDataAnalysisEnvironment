## ai.R
## 
## Author: Masaei Sato <masaei@affrc.go.jp>
## Date: 2024-2-19
## Last update: 2024-2-19
##
## Overview: This script responds to user text using the OpenAI API.
##    The openai_api_request function sends a request to the OpenAI
##    API and returns the response.
##
## Requirements:
## - R (Confirmed to work with R version 4.3.2)
## - R packages: httr, jsonlite
## - openai's API key
##
## Note:
## - The API key must be set as an environment variable OPENAI_API_KEY.
## 
## Usage:
## Rscript ai.R [options] user_content
## 
## Options:
##   -h, --help              show this help message and exit
##   -m MODEL, --model=MODEL
##   -u USER_CONTENT, --user_content=USER_CONTENT
## 
## Example usage:
##   Rscript ./ai.R "What is the function to create a box plot in R?"
##
## Reference: The openai_api_request function of the
##     requestOpenaiGPT.R script in the agritechviz package are used.
##
## Note: The environment variable OPENAI_API_KEY must be set first.
##      For example, in the .Renviron file, add the following line:
##      OPENAI_API_KEY="your_api_key" where your_api_key is your
##      OpenAI API key.  The method of obtaining your_api_key is
##      described at https://beta.openai.com/docs/.

## * library
library(httr)
library(jsonlite)

## * function
openai_api_request <- function(api_key = Sys.getenv("OPENAI_API_KEY"),
                               model = "gpt-4o-mini",
                               system_content = "You are my R coding assistant, helping with debugging, analysis, visualization, and best practices.", #You are a helpful assistant.
                               user_content,
                               temperature = 0.7,
                               max_tokens = 100,
                               top_p = 1,
                               n = 1) {
  # Construct Authorization header
  authorization_header <- paste0("Bearer ", api_key)
  # Define the URL and headers
  url <- "https://api.openai.com/v1/chat/completions" #api endpoint
  headers <- add_headers("Content-Type" = "application/json", "Authorization" = authorization_header) #header

  # Define the request body
  body <- list(
    model = model,
    messages = list(
      list(
        role = "system",
        content = system_content
      ),
      list(
        role = "user",
        content = user_content
      )
    ),
    temperature = temperature,
    max_tokens = max_tokens,
    top_p = top_p,
    n = n
  )

  # Convert the R list to JSON
  json_body <- toJSON(body, auto_unbox = TRUE)

  # Make the API request
  response <- httr::POST(url, body = json_body, config = headers, content_type("application/json"))

  # Return the parsed response
  return(content(response, "parsed"))
}

## * usage
## Enter user text
## user_text <- readline("Enter your message: ")

## Change input to the following since it is run in Rscript.
## Accept user text from command line arguments
user_text <- commandArgs(trailingOnly = TRUE)

## Get response using OpenAI API
response <- openai_api_request(
  user_content = user_text,
  model = "gpt-4o-mini", #gpt-3.5-turbo, gpt-3.5-turbo-1106
  temperature = 1,
  max_tokens = 2000
)

## Output response
## Output a text-only response. However, a newline is inserted at the end.
cat(response$choices[[1]]$message$content, "\n")
