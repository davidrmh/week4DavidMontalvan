file_name <- make_filename(2015)
expect_match(file_name,
             "accident_\\d{2,}\\.csv\\.bz2")
