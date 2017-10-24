
###
# Takes mongo original keys, translate  and update in a mongo collection 
#
mr = db.runCommand({
  "mapreduce" : "offer",
  "map" : function() {
    for (var key in this) { emit(key, null); }
  },
  "reduce" : function(key, stuff) { return null; }, 
  "out": "offer" + "_keys"
})

###
require(rvest)

transPage <- read_html("https://translate.google.com/#es/en/habitaciones")

transPage %>% html_node("#result_box") %>% html_node('span')

#result_box > span
%>% html_attr('texttext', 'class') 

%>% html_node('') 

transPage %>%  html_node(xpath='//*[@id="result_box"]/span')

###
#########################################################
################################
remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4444
                      , browserName = "chrome"
)
remDr$open(silent=F) 
remDr$getStatus()
unlist(lapply(uniqKeys, function(x)x$`_id`))
x <- uniqKeys[20]


       
       
translateGW <-        function(x){
  x1 <- as.character(unlist(x))
  textQuery <- URLencode(as.character(x1))
url <- paste("https://translate.google.com/#es/en/",textQuery,collapse ='',sep='')
remDr$navigate(url)
Sys.sleep(2)
webElems <- remDr$findElement(using = 'xpath', '//*[@id="result_box"]/span')
webElems$getElementText()

}

lapply(uniqKeys, translateGW)
