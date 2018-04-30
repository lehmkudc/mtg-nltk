rm( list=ls() )
library(rjson)
options(stringsAsFactors = FALSE)

url <- 'https://api.scryfall.com/cards/search?q=ft:/\\w/'
page <- fromJSON( file = url )

end <- F
k <- 0
card_data_list <- list()
flavor <- list()

ext_card <- function( card ){
   
   name <- iconv( card$name, to = "ASCII//TRANSLIT")
   name <- gsub( '"', '', name[1])
   name <- gsub( ' //.+', '', name[1])
   if (nchar(name[1]) > 100){
      name <- 'Our Market Research'
   }
   
   colors <- paste( card$color_identity , collapse = '')
   is_w <- grepl( 'W', colors )
   is_u <- grepl( 'U', colors )
   is_b <- grepl( 'B', colors )
   is_r <- grepl( 'R', colors )
   is_g <- grepl( 'G', colors )
   
   is_a <- grepl( 'Artifact', card$type_line )
   is_l <- grepl( 'Land', card$type_line )
   
   return( c( name, is_w, is_u, is_b, is_r, is_g, is_a, is_l ) )
   
   
}

while ( end == F ){ # Per Page
   N_cards <- length( page$data )
      
   for (i in 1:N_cards){ # For Each Card
      card <- page$data[[i]]
         
      if ( length(card$card_faces) == 0 ){
         k <- k + 1
         card_data_list[[k]] <- ext_card( card )
         flavor[[k]] <- card$flavor_text
         
      } else {
         
         f1 <- card$card_faces[[1]]
         if ( length(f1$flavor_text) > 0 ){
            k <- k + 1
            card_data_list[[k]] <- ext_card( f1 )
            flavor[[k]] <- f1$flavor_text
         }
         
         f2 <- card$card_faces[[2]]
         if ( length(f2$flavor_text) > 0 ){
            k <- k + 1
            card_data_list[[k]] <- ext_card( f2 )
            flavor[[k]] <- f2$flavor_text
         }
         
      }
   }
   
   if ( page$has_more == F ){
      end <- T
   } else {
      page <- fromJSON(file= as.character(page$next_page) )
   }
}

card_data <- data.frame( matrix( unlist(card_data_list), nrow=k, byrow =T) )
colnames(card_data) <- c( 'Name','is_w','is_u','is_b','is_r','is_g','is_a','is_l')
write.csv(x = card_data,file = 'mtg-nltk/card_data.csv',row.names = T)

flavor_data <- data.frame( matrix( unlist( flavor ), nrow = k, byrow=T ) )
colnames( flavor_data ) <- c( 'Flavor' )
write.csv(x = flavor_data, file = 'mtg-nltk/flavor_data.csv', row.names=T )

