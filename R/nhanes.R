get_catalog_nhanes <-
	function( data_name = "nhanes" , output_dir , ... ){

		data_page <- "https://wwwn.cdc.gov/nchs/nhanes/search/DataPage.aspx"

		data_html <- xml2::read_html( data_page )

		this_table <- rvest::html_table( data_html )[[1]]

		names( this_table ) <- c( 'years' ,'doc_name' , 'file_name' , 'date_published' )


		all_links <- rvest::html_nodes( data_html , "a" )

		link_text <- rvest::html_text( all_links )

		link_refs <- rvest::html_attr( all_links , "href" )

		this_table$full_url <- link_refs[ match( this_table$file_name , link_text ) ]

		this_table$doc_url <- link_refs[ match( this_table$doc_name , link_text ) ]

		this_table[ c( 'full_url' , 'doc_url' ) ] <- sapply( this_table[ c( 'full_url' , 'doc_url' ) ] , function( w ) ifelse( is.na( w ) , NA , paste0( "https://wwwn.cdc.gov" , w ) ) )

		catalog <- this_table[ this_table$file_name != 'RDC Only' & this_table$date_published != 'Withdrawn' & !grepl("(spx)|(x_g)|(x_h)$", this_table$full_url) , ]

		# one all years doc hardcode
		this_ayd <- data.frame(
  years = c("2005-2006", "2003-2004", "2001-2002", "1999-2000"),
  doc_name = c("DXX_D Doc", "DXX_C Doc", "DXX_B Doc", "DXX Doc"),
  file_name = c("DXX_D Data [XPT - 28.2 MB]", "DXX_C Data [XPT - 28.9 MB]", "DXX_B Data [XPT - 31.1 MB]", "DXX Data [XPT - 23.5 MB]"),
  date_published = c("Updated December 2016",  "Updated March 2010", "Updated March 2010", "Updated March 2010" ),
  full_url = c("https://wwwn.cdc.gov/nchs/data/nhanes/dxa/dxx_d.xpt",  "https://wwwn.cdc.gov/nchs/data/nhanes/dxa/dxx_c.xpt", "https://wwwn.cdc.gov/nchs/data/nhanes/dxa/dxx_b.xpt",  "https://wwwn.cdc.gov/nchs/data/nhanes/dxa/dxx.xpt"),
  doc_url = c("https://wwwn.cdc.gov/nchs/data/nhanes/dxa/dxx_d.htm",  "https://wwwn.cdc.gov/nchs/data/nhanes/dxa/dxx_c.pdf", "https://wwwn.cdc.gov/nchs/data/nhanes/dxa/dxx_b.pdf",  "https://wwwn.cdc.gov/nchs/data/nhanes/dxa/dxx.pdf")
)
		# vitamin D file hardcode 
# https://wwwn.cdc.gov/nchs/nhanes/2001-2002/VID_B.XPT
		vid_table <- data.frame(
  years = c("2001-2002", "2003-2004", "2005-2006", "2007-2008", "2009-2010", "2011-2012", "2013-2014", "2015-2016", "2017-2018"),
  doc_name = c("VID_B Doc", "VID_C Doc", "VID_D Doc", "VID_E Doc", "VID_F Doc", "VID_G Doc", "VID_H Doc", "VID_I Doc", "VID_J Doc"),
  file_name = c("VID_B Data [XPT - 136.2 KB]", "VID_C Data [XPT - 144.5 KB]", "VID_D Data [XPT - 148.5 KB]", "VID_E Data [XPT - 656.4 KB]", "VID_F Data [XPT - 693.5 KB]", "VID_G Data [XPT - 631.7 KB]", "VID_H Data [XPT - 664.5 KB]", "VID_I Data [XPT - 646.4 KB]", "VID_J Data [XPT - 590.2 KB]"),
  date_published = c("Updated October 2015", "Updated October 2015", "Updated October 2015", "October 2015", "October 2015", "October 2017", "May 2018", "August 2021", "April 2022"),
  full_url = c("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/VID_B.XPT", "https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/VID_C.XPT", 
               "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/VID_D.XPT", "https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/VID_E.XPT", 
               "https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/VID_F.XPT", "https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/VID_G.XPT", 
               "https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/VID_H.XPT", "https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/VID_I.XPT", 
               "https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/VID_J.XPT"),
  doc_url = c("https://wwwn.cdc.gov../vitamind/analyticalnote.aspx?b=2001&e=2002&d=VID_B&x=htm", "https://wwwn.cdc.gov../vitamind/analyticalnote.aspx?b=2003&e=2004&d=VID_C&x=htm",  "https://wwwn.cdc.gov../vitamind/analyticalnote.aspx?b=2005&e=2006&d=VID_D&x=htm",  "https://wwwn.cdc.gov../vitamind/analyticalnote.aspx?b=2007&e=2008&d=VID_E&x=htm",  "https://wwwn.cdc.gov../vitamind/analyticalnote.aspx?b=2009&e=2010&d=VID_F&x=htm",  "https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/VID_G.htm", "https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/VID_H.htm",  "https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/VID_I.htm", "https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/VID_J.htm")
)

		ayd <- merge(this_ayd, vid_table)

		catalog <- rbind( catalog , ayd )

		catalog$output_filename <- paste0( output_dir , "/" , catalog$years , "/" , tolower( gsub( "\\.xpt" , ".rds" , basename( catalog$full_url ) , ignore.case = TRUE ) ) )

		catalog <- catalog[ order( catalog[ , 'years' ] ) , ]

		catalog <- data.frame(catalog)

		catalog

	}


lodown_nhanes <-
	function( data_name = "nhanes" , catalog , ... ){

		on.exit( print( catalog ) )

		tf <- tempfile()

		for ( i in seq_len( nrow( catalog ) ) ){

			# download the file
			cachaca( catalog[ i , "full_url" ] , tf , mode = 'wb' )

			if( grepl( "\\.zip$" , catalog[ i , "full_url" ] , ignore.case = TRUE ) ){

				unzipped_files <- unzip( tf , exdir = tempdir() )

				suppressWarnings( file.remove( tf ) )

				tf <- unzipped_files

			}

			xport_attempt <- try( x <- foreign::read.xport( tf ) , silent = TRUE )

			if( class( xport_attempt ) == 'try-error' ) x <- data.frame( haven::read_sas( tf ) )

			# convert all column names to lowercase
			names( x ) <- tolower( names( x ) )

			saveRDS( x , file = catalog[ i , 'output_filename' ] , compress = FALSE )

			catalog[ i , 'case_count' ] <- nrow( x )

			# delete the temporary files
			suppressWarnings( file.remove( tf ) )

			cat( paste0( data_name , " catalog entry " , i , " of " , nrow( catalog ) , " stored at '" , catalog[ i , 'output_filename' ] , "'\r\n\n" ) )

		}

		on.exit()

		catalog

	}

