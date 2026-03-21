# function to push to airtable

push_to_airtable <- function(df, token, base_id, table_name, batch_size = 10, sleep = 0.25) {

  stopifnot(is.data.frame(df))
  if (nchar(base_id) == 0) stop("BASE_ID is empty — check your config.")

  url <- paste0("https://api.airtable.com/v0/", base_id, "/", URLencode(table_name, reserved = TRUE))

  headers <- httr::add_headers(
    Authorization = paste("Bearer", token),
    `Content-Type` = "application/json"
  )

  # Delete all existing records in the table
  message("Clearing existing records...")
  offset <- NULL
  repeat {
    query     <- if (!is.null(offset)) list(offset = offset) else list()
    list_resp <- httr::GET(url, headers, query = query)
    if (httr::status_code(list_resp) >= 400)
      stop("Failed to fetch existing records (HTTP ", httr::status_code(list_resp), ")")
    parsed   <- jsonlite::fromJSON(httr::content(list_resp, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
    rec_ids  <- vapply(parsed$records, `[[`, character(1), "id")
    if (length(rec_ids) > 0) {
      del_chunks <- split(rec_ids, ceiling(seq_along(rec_ids) / 10))
      for (ids in del_chunks) {
        query_str <- paste(paste0("records[]=", ids), collapse = "&")
        del_resp  <- httr::DELETE(paste0(url, "?", query_str), headers)
        if (httr::status_code(del_resp) >= 400)
          stop("Failed to delete records (HTTP ", httr::status_code(del_resp), ")")
        Sys.sleep(sleep)
      }
    }
    offset <- parsed$offset
    if (is.null(offset)) break
  }
  message("Table cleared.")

  # Convert df to a clean list, dropping NA fields per row
  to_records <- function(chunk) {
    lapply(seq_len(nrow(chunk)), function(j) {
      row <- as.list(chunk[j, , drop = FALSE])
      row <- lapply(row, function(x) {
        if (is.factor(x)) x <- as.character(x)
        if (inherits(x, "POSIXt")) x <- format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
        x
      })
      row <- row[!vapply(row, function(x) length(x) == 1 && is.atomic(x) && is.na(x), logical(1))]
      list(fields = row)
    })
  }

  chunks <- split(df, ceiling(seq_len(nrow(df)) / batch_size))

  for (i in seq_along(chunks)) {
    body <- jsonlite::toJSON(
      list(typecast = TRUE, records = to_records(chunks[[i]])),
      auto_unbox = TRUE
    )

    resp <- httr::POST(url, headers, body = body)

    if (httr::status_code(resp) == 429) {
      retry_after <- httr::headers(resp)[["retry-after"]]
      Sys.sleep(if (!is.null(retry_after)) as.numeric(retry_after) else 1)
      resp <- httr::POST(url, headers, body = body)
    }

    if (httr::status_code(resp) >= 200 && httr::status_code(resp) < 300) {
      cat(sprintf("✓ Batch %d/%d uploaded (%d records)\n", i, length(chunks), nrow(chunks[[i]])))
    } else {
      msg <- httr::content(resp, as = "text", encoding = "UTF-8")
      stop(sprintf("✗ Batch %d failed (HTTP %d): %s", i, httr::status_code(resp), msg))
    }

    Sys.sleep(sleep)
  }

  invisible(TRUE)
}
