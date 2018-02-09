library(rvest)
library(dplyr)
url <- "http://multimedia.scmp.com/2016/cities/"

## Scraping URL for information on Chinese cities
writeLines(sprintf("var page = require('webpage').create();
                     page.open('%s', function () {
                   console.log(page.content); //page source
                   phantom.exit();
                   });", url), con="scrape1.js")
system("cmd.exe", input = "C:\\Users\\prateek\\Downloads\\phantomjs-2.1.1-windows\\bin\\phantomjs scrape1.js > file.html")

## Obtaining data on tier, income, GDP for cities/provinces
tiers <- 1:4
df <- setNames(lapply(tiers, function(num){
  dat <- read_html("file.html") %>% 
    html_nodes(paste(".AVT",num,sep="")) %>% 
    html_text()
  dat <- gsub("\\,","", dat, perl=T)
  cities <- unlist(lapply(dat, function(x) {gsub("GDP.*","",x)}))
  pop <- stringr::str_extract(dat, "(?<=population\\s)([0-9>\\.]+)")
  gdp <- stringr::str_extract(dat, "(?<=US\\$)([0-9>\\.]+)")
  out <- data.frame(Working_city=cities, GDP=gdp, Population=pop, stringsAsFactors = F)
}),paste("Tier",tiers))

## General Data Clean-Up
out <- bind_rows(df, .id="Tier")
out$Population <- as.numeric(gsub("\\.","",out$Population))
out$GDP <- as.numeric(gsub(">","",out$GDP))
out$GDP_per_capita <- out$GDP * 6 * 1e9 / out$Population
out <- rbind(out, transform(out, Working_city = paste(Working_city, "City")))
write.csv(out, "CityMap.csv", row.names=F)

# df <- setNames(lapply(tiers, function(num){
#   dat <- read_html("file.html") %>% html_nodes(paste(".AVT",num,sep="")) %>% html_text()
#   out <- setNames(data.frame(unlist(lapply(dat, function(x) {gsub("GDP.*","",x)})), stringsAsFactors = F),"Working_city")
# }),paste("Tier",tiers))
# out <- dplyr::bind_rows(df, .id="Tier")
# out <- rbind(out, transform(out, City = paste(City, "Working_city")))