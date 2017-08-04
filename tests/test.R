if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )
my_email_address <- Sys.getenv( "my_email_address" )
my_password <- Sys.getenv( "my_password" )
library(lodown)
# examine all available PSID microdata files
psid_cat <-
	get_catalog( "psid" ,
		output_dir = file.path( getwd() ) , 
		your_email = my_email_address , 
		your_password = my_password )

# download the microdata to your local computer
stopifnot( nrow( psid_cat ) > 0 )



options( survey.lonely.psu = "adjust" )

library(survey)

# identify the cross-year individual filename
cross_year_individual_rds <- 
	grep( 
		"cross-year individual" ,
		list.files( 
			file.path( getwd() ) , 
			recursive = TRUE , 
			full.names = TRUE 
		) ,
		value = TRUE
	)

individual_df <- readRDS( cross_year_individual_rds )

ind_variables_to_keep <-
	c( 
		'one' ,			# column with all ones
		'er30001' , 	# 1968 interview number
		'er30002' , 	# 1968 person number
		'er31997' ,		# primary sampling unit variable
		'er31996' ,		# stratification variable
		'er33802' ,		# sequence number, 2005
		'er34302' , 	# sequence number, 2015
		'er32000' ,		# sex
		'er34305' ,		# age in 2015
		'er33813' ,		# employment status in 2005
		'er34317' ,		# employment status in 2015
		'er33848' ,		# 2005 longitudinal weight
		'er34413'		# 2015 longitudinal weight
	)

	

individual_df <- individual_df[ ind_variables_to_keep ] ; gc()

family_2005_df <- 
	readRDS( file.path( getwd() , "family files/2005.rds" ) )

fam_2005_variables_to_keep <- 
	c( 
		'er25002' ,	# 2005 interview number
		'er28037' 	# 2005 total family income
		
	)

individual_df <- individual_df[ ind_variables_to_keep ] ; gc()

family_2015_df <- 
	readRDS( file.path( getwd() , "family files/2015.rds" ) )

fam_2015_variables_to_keep <-
	c( 
		'er60002' ,	# 2015 interview number
		'er65349' 	# 2015 total family income
	)

individual_df <- individual_df[ fam_2015_variables_to_keep ] ; gc()

ind_fam_2005 <- 
	merge( 
		individual_df , 
		family_2005_df , 
		by.x = 'er33802' , 
		by.y = 'er25002' 
	)

ind_fam_2015 <- 
	merge( 
		individual_df , 
		family_2015_df , 
		by.x = 'er34302' , 
		by.y = 'er60002' 
	)

psid_df <- merge( ind_fam_2005 , ind_fam_2015 , all = TRUE )

psid_design <- 
	svydesign( 
		~ er31997 , 
		strata = ~ er31996 , 
		data = psid_df , 
		weights = ~ er33848 , 
		nest = TRUE 
	)
psid_design <- 
	update( 
		psid_design , 
		
		employment_2005 =
			factor( er33813 , levels = 1:8 ,
				labels = c( 'working now' , 'only temporarily laid off' ,
				'looking for work, unemployed' , 'retired' , 'permanently disabled' ,
				'housewife; keeping house' , 'student' , 'other' )
			) ,
			
		employed_in_2015 = 
			factor( er34317 , levels = 1:8 ,
				labels = c( 'working now' , 'only temporarily laid off' ,
				'looking for work, unemployed' , 'retired' , 'permanently disabled' ,
				'housewife; keeping house' , 'student' , 'other' )
			) ,
			
		female = as.numeric( er32000 == 2 )

	)
sum( weights( psid_design , "sampling" ) != 0 )

svyby( ~ one , ~ employment_2005 , psid_design , unwtd.count )
svytotal( ~ one , psid_design )

svyby( ~ one , ~ employment_2005 , psid_design , svytotal )
svymean( ~ er28037 , psid_design , na.rm = TRUE )

svyby( ~ er28037 , ~ employment_2005 , psid_design , svymean , na.rm = TRUE )
svymean( ~ employed_in_2015 , psid_design , na.rm = TRUE )

svyby( ~ employed_in_2015 , ~ employment_2005 , psid_design , svymean , na.rm = TRUE )
svytotal( ~ er28037 , psid_design , na.rm = TRUE )

svyby( ~ er28037 , ~ employment_2005 , psid_design , svytotal , na.rm = TRUE )
svytotal( ~ employed_in_2015 , psid_design , na.rm = TRUE )

svyby( ~ employed_in_2015 , ~ employment_2005 , psid_design , svytotal , na.rm = TRUE )
svyquantile( ~ er28037 , psid_design , 0.5 , na.rm = TRUE )

svyby( 
	~ er28037 , 
	~ employment_2005 , 
	psid_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ er28037 , 
	denominator = ~ er65349 , 
	psid_design ,
	na.rm = TRUE
)
sub_psid_design <- subset( psid_design , er34305 >= 65 )
svymean( ~ er28037 , sub_psid_design , na.rm = TRUE )
this_result <- svymean( ~ er28037 , psid_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ er28037 , 
		~ employment_2005 , 
		psid_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( psid_design )
svyvar( ~ er28037 , psid_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ er28037 , psid_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ er28037 , psid_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ female , psid_design ,
	method = "likelihood" )
svyttest( er28037 ~ female , psid_design )
svychisq( 
	~ female + employed_in_2015 , 
	psid_design 
)
glm_result <- 
	svyglm( 
		er28037 ~ female + employed_in_2015 , 
		psid_design 
	)

summary( glm_result )
library(srvyr)
psid_srvyr_design <- as_survey( psid_design )
psid_srvyr_design %>%
	summarize( mean = survey_mean( er28037 , na.rm = TRUE ) )

psid_srvyr_design %>%
	group_by( employment_2005 ) %>%
	summarize( mean = survey_mean( er28037 , na.rm = TRUE ) )

