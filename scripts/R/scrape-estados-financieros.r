library(rvest)
library(dplyr)

page <- read_html( "https://www.mifuturo.cl/ministerio-de-educacion-entrega-estados-financieros-de-las-instituciones-de-educacion-superior/")


urls <- page |> html_elements("a") |> html_attr("href")

# select urls that match 'eeff2018'
urls <- urls[grepl("eeff2018", urls)]
urls <- urls[1:10]

# download the files
# into the following directory C:\Users\djaar\Downloads\eeff\eeff2018
for ( url in urls ) {
    download.file( url, destfile = paste0("C:/Users/djaar/Downloads/eeff/eeff2018/", basename(url)) )
}




library(httr)

dir.create("C:/Users/djaar/Downloads/eeff/eeff2018", recursive = TRUE, showWarnings = FALSE)

for (url in urls) {
  dest <- paste0("C:/Users/djaar/Downloads/eeff/eeff2018/", basename(url))
  response <- GET(url, 
                  add_headers(`User-Agent` = "Mozilla/5.0"),
                  write_disk(dest, overwrite = TRUE),
                  config(followlocation = TRUE))
  cat("Downloaded:", url, "| Status:", status_code(response), "\n")
}







# filter to just the files you want, then download

# Links per year

# 2019
https://www.mifuturo.cl/ministerio-de-educacion-entrega-estados-financieros-de-las-instituciones-de-educacion-superior/

# 2018
https://www.mifuturo.cl/ministerio-de-educacion-entrega-estados-financieros-de-las-instituciones-de-educacion-superior-2018/

# 2017
https://www.mifuturo.cl/estadosfinancieros2016publicados-en-2017/

# 2016
https://www.mifuturo.cl/2016estados-financieros-auditados-2016/

# 2015
https://www.mifuturo.cl/estados-financieros-de-las-instituciones-de-ed-superior/

# 2014
https://www.mifuturo.cl/estados-financieros-auditados-de-las-instituciones-de-ed-superior/

# 2013
https://www.mifuturo.cl/estados-financieros-auditados-de-las-instituciones-de-educacion-superior/
