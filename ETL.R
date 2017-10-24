#Extract data from WEB pages
# www.fincaraiz.com.co
# www.metrocuadrado.com

# install.packages(c('tidyr'))
# install.packages(c('rvest','dplyr'))
# install.packages(c('purrr'))
# install.packages("stringr")
# install.packages("rdom")
#install.packages("RMongo")

install.packages(c('ggmap'))
# 
# list.of.packages <- c("tm")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# 
 lapply(as.list(c('tidyr','rvest','dplyr','RSelenium','jsonlite'
                  ,'rmongodb','httr','geojsonio','ggmap','ggplot2','leaflet'))
        , library,character.only = TRUE)

# metroCuadrado <- read_html(c("http://www.metrocuadrado.com/apartamentos/venta-bogota"))
# housingHref <- metroCuadrado %>%
#   html_nodes("div.propertyInfo.item") %>% html_nodes("a")
# 
# itemPage <- read_html("http://www.metrocuadrado.com/venta/apartamento-en-bogota-chico-chico-con-3-habitaciones-4-ba%C3%B1os-2-garajes-estrato-6-area-156-mts-$1.500.000.000-id-2162-1725978")
# 
# itemPage %>% 
# 
# %>% #html_node("a")%>%
#   html_text()
# stopifnot(Sys.which("phantomjs") != "")
# devtools::install_github("cpsievert/rdom")
################################
remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4444
                      , browserName = "chrome"
)
remDr$open(silent=F) 
remDr$getStatus()

remDr$close()
# remDr$navigate("http://www.google.com/ncr")
# webElem <- remDr$findElement(using = "css", "[name = 'q']")
# webElem$sendKeysToElement(list("R Cran", "\uE007"))
# webElems <- remDr$findElements(using = 'css selector', "h3.r")
# resHeaders <- unlist(lapply(webElems, function(x){x$getElementText()}))
# webElem <- webElems[[which(resHeaders == "The Comprehensive R Archive Network")]]
# webElem$clickElement()
# 
# write(remDr$getPageSource()[[1]],file='proof.html')


#########################################################
remDr$navigate("http://www.metrocuadrado.com/casas/venta/bogota/")
webElems <- remDr$findElements(using = 'css selector', "div.propertyInfo.item")
contResults1 <- remDr$findElement(using = 'id', "rb_txtTopNumeroResultados")
contResults <- as.numeric(gsub("\\.","",unlist(contResults1$getElementText()) ,perl = T))
fromTo <- 1:ceiling(contResults/32)
itemLinksCon <- NA

for (i in fromTo){
    currentPage <- read_html(remDr$getPageSource()[[1]])
    itemLinks <- currentPage %>%
      html_nodes("div.propertyInfo.item") %>% # get the CSS nodes
      html_node("a") %>% 
      html_attr('href')
    itemLinksCon <- c(itemLinksCon,itemLinks) 
    # followingRes <- remDr$findElement(using = 'id', "paginador_siguiente")
    remDr$executeScript("siguientePagina('')")
    Sys.sleep(1)
    # webElem <- followingRes
    # webElem$clickElement()
    # webElem$getCurrentUrl() 
    #contResults2 <- remDr$findElement(using = 'id', "rb_formOrdenar_desde")
    
    #print(contResults2$getElementText())
    
}


itemLinksCon[which(itemLinksCon == itemLinksCon.1[[2500]] )]

itemLinksCon.1 <- unique(itemLinksCon) 

prrof1 <- itemLinksCon
identical(prrof1, itemLinksCon)
library(rmongodb)
# connect to MongoDB
mongo <-  mongo.create()

mongo.is.connected(mongo)
mongo.get.databases(mongo)
# mongo.get.database.collections(mongo, db='realstate')
# mongo.get.database.collections(mongo, "realstate")


ns <- "realstate.offer"
for ( i in 2211:length(itemLinksCon.1)){
  remDr$navigate(itemLinksCon.1[i])
        itemPage <-  read_html(remDr$getPageSource()[[1]])
          features <-    itemPage %>% 
                html_nodes('div.infoinmueble') %>% 
                html_nodes('h3') %>%
                html_text() %>% #t()%>%
                gsub("[[:punct:]]","", .)
        
        values <-  itemPage %>% 
          html_nodes('div.infoinmueble') %>% 
          html_nodes('h4')%>%
          html_text() %>% #t()%>% 
          gsub("[[:punct:]]","", .)
         
        itemFeatures_df <-  data.frame(t(values),stringsAsFactors = F)
        colnames(itemFeatures_df) <- features
        
        itemCoords <- itemPage %>% 
                html_nodes('div.container4') %>% 
                html_nodes('input') %>%
                html_attr("value") %>%
                data.frame() %>% t()
        if(all(dim(itemCoords) == c(1,2))){
          colnames(itemCoords) <- c('latitud','longitud')
        
        }else {
          itemCoords <- t(c(NA,NA))
          colnames(itemCoords) <- c('latitud','longitud')
        }
        
      
        itemFeatures_df$coordenadas <- itemCoords
        itemFeatures_df$url <- itemLinksCon.1[i]
        
        #itemFeatures_json <- toJSON(itemFeatures_df,raw = c('mongo'),pretty = T)
        
        
        
        
        # df <- data.frame(A=c("a","a","b","b"), B=c("X","X","Y","Z"), C=c(1,2,3,4), 
        #                  stringsAsFactors = F) 
        # lst <- split(df, rownames(df))
        
        # bson_lst <- lapply(lst, mongo.bson.from.list)
        # mongo.insert.batch(mongo = mongo, ns = "db.collection", lst = bson_lst)
        
       
        if (mongo.is.connected(mongo)) {
        
          # buf <- mongo.bson.buffer.create()
          # mongo.bson.buffer.append(buf, "name", "Joe")
          # mongo.bson.buffer.append(buf, "age", 22L)
          # b <- mongo.bson.from.buffer(buf)
          # mongo.insert(mongo, ns, b)
          # 
          # do the same thing in shorthand:
          mongo.insert(mongo, ns, as.list(itemFeatures_df))
          
          # Find the first 100 records
          #    in collection people of database test where age == 18
        } else { stop() }
        
        
}
i

  # Step though the matching records and display them
  while (mongo.cursor.next(cursor))
    print(mongo.cursor.value(cursor))
  mongo.cursor.destroy(cursor)
  buf <- mongo.bson.buffer.create()
  mongo.bson.buffer.append(buf, "Tipo Inmueble", "Casa")
  query1<- mongo.bson.from.buffer(buf)
  
  cursor2 <- mongo.find.all(mongo, "realstate.offer", query = query1
                            
                           ,fields = list(coordenadas = 1L, "Valor Venta" = 1L , "Tipo Inmueble" = 1L))
  

  uniqKeys <- mongo.find.all(mongo,"realstate.offer_keys") 
  unlist(uniqKeys )
   
   unlist(lapply(uniqKeys, function(x)x$`_id`))
    
    housingCoords_df <- as.data.frame(do.call('rbind',lapply(cursor,function(x)  as.numeric(unlist(x$coordenadas)))))
     colnames(housingCoords_df) <- c('lat', 'lng') 
 (m <- leaflet() %>% addTiles()) 
 
 m2 <- m %>%
     setView(-74.08175, 4.6097, 12) %>% # map location
     addMarkers(housingCoords_df$lng, housingCoords_df$lat, clusterOptions =  markerClusterOptions()) %>% # add a marker
     #addPopups(-1.6, 53.3, popup = "Hello Sheffield!") %>% # popup
     # add som circles:
     addCircles(color = "black", runif(90, -2, -1), runif(90, 53, 54), runif(90, 10, 500))
 
 m2

 
 tartu_map_g_str <- get_map(location = c(lon = -74.08175, lat = 4.6597), zoom = 10)
 
 ggmap(tartu_map_g_str, extent = "device") + 
  geom_point(aes(x = lng, y = lat), colour = "black",alpha = I(0.1), size = 0.5, data = housingCoords_df) + 
   geom_density2d(data =  housingCoords_df, aes(x = lng, y = lat), size = 0.01, alpha = I(0.1)) +
   stat_density2d(data = housingCoords_df, aes(x = lng, y = lat, fill = ..level.., alpha = ..level..), 
                  bins = 5000, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
   scale_alpha(range = c(0, 1), guide = FALSE)

 
 library(ggplot2)
 library(ggmap)
 
 
  # shorthand: find all records where age=32, sorted by name,
  # and only return the name & address fields:
  cursor <- mongo.find(mongo, "test.people", 
                       `Tipo Negocio`= 1L)

  
}
mongo.find.all(mongo,'realstate.housing')#,list=("Tipo Negocio"=1L))

json <- '{"Tipo Negocio": 1L, "coordenadas": 1L}'
 bson <- mongo.bson.from.JSON(json)
 cursor <- mongo.find(mongo, ns, bson)
 while(mongo.cursor.next(cursor)) {
     value <- mongo.cursor.value(cursor)
     list1 <- mongo.bson.to.list(value)
     str(list1)
   }

 
 
 buf <- mongo.bson.buffer.create()
 mongo.bson.buffer.start.object(buf, "realstate.housing")
 mongo.bson.buffer.append(buf, "$in", c("Tipo Negocio", "Valor Venta"))
 mongo.bson.buffer.finish.object(buf)
 criteria <- mongo.bson.from.buffer(buf)
 mongo.find(mongo, ns, criteria)

 
 
 
 if(i == 1){
  allItems_df <- itemFeatures_df
} else {
  
  allItems_df <- rbind(allItems_df, itemFeatures_df)
}




isn




%>%
  as.tbl() 


%>% select(features) %>%  mutate(is_pnum = grepl("[[:digit:]]", . )) %>%
  as.data.frame()
  
grep("[:alpha:]", '6000000')

itemPage %>% 
  html_nodes('div.container4') %>% 
  html_nodes('input') %>%
  html_attr("value") %>% #t()%>%
  as.data.frame()

%>% toJSON(dataframe=c("rows"), raw = 'mongo')


  colnamData <- c("housingType","sale","neighborhood","price","adminFees","stratus","area","bedrooms","bathrooms", "parking","antiquity")
  
  