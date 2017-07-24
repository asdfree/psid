if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )
my_email_address <- Sys.getenv( "my_email_address" )
my_password <- Sys.getenv( "my_password" )
library(lodown)
lodown( "psid" , output_dir = file.path( getwd() ) , 
	your_email = my_email_address , 
	your_password = my_password )
