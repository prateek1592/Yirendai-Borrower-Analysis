#### Get Chinese script compatibility for the R Code to prevent special character replacement
# oldloc <- Sys.getlocale("LC_CTYPE")
# Sys.setlocale("LC_CTYPE", locale="Chinese")
## Note to self: Set to "English_United States.1252" when done!

library(rvest)

# Create a list of URLs to be accessed
url1 <- "https://www.yirendai.com/loan/view/"
seq_num <- 26519182 - 0:30000   # 30k IDs checked in one run
url2 <- "?page=1&tabflag=0"

out_list <- list()
count <- 0

# Extracting loan related details for each synthesized URL
for (n in seq_num){
  url <- paste(url1, n, url2, sep="")
  temp <- tryCatch({read_html(url, encoding='UTF-8') %>% html_nodes("table")},
                   error=function(cond){return (NULL)})
  if (is.null(temp)) next;
  indx <- grep("borrowerInfo",temp)
  indx <- indx[-length(indx)]
  
  for (j in seq_along(indx)){
    if (j==1){
      mat <- html_table(temp[indx[j]])[[1]]
    }
    else {
      mat2 <- html_table(temp[indx[j]])[[1]]
      mat <- rbind(mat,mat2)
    }
  }
  out_list[[as.character(n)]] <- mat
  print (paste("Done for", n))
  count <- count + 1
  Sys.sleep(0.05)
}

# Mild post-processing and output of data
print(paste("Successful for", count, "out of", length(seq_num), "IDs"))
out <- dplyr::bind_rows(out_list, .id="ID")
names(out) <- c("ID", "Particular", "Value")
xlsx::write.xlsx(out, 'Temp_Data.xlsx', row.names=F)