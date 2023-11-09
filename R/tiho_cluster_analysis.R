###############################################################
#
#			K-MEANS CLUSTERING MODEL
#
# descripttion: This function performs unsupervised clustering
#		    using the K-means model. The optimal number of
#		    clusters is selected automatically according to
#		    two alternative methods decided by the user:
#
#		    1) Silhouette score (default method)
#		    2) Knee method (as in DeepSqueak, Coffey et al 2019, 
#			   Neuropsychopharmacology, 44(5): 859-868))
#
# author:	Daniel Romero Mujalli
# email:	danielrm84@gmail.com
#
#
# last update: 23_05_2023
###############################################################


###############################################################
#' KmeansElbow							  	  
#'
#' DESCRIPTION
#' This function performs unsupervised clustering
#'		    using the K-means model. The optimal number of
#'		    clusters is selected automatically according to
#'		    two alternative methods decided by the user:
#'
#'		    1) Silhouette score (default method)
#'		    2) Knee method (as in DeepSqueak)
#' ARGUMENTS
#' @param x: numeric matrix of data, or an object that can be
#' coerced to such a matrix (such as a numeric vector or a data
#' frame with all numeric columns).
#'
#' @param rep: maximum number of iterations (default 10; 
#' iter.max in kmeans()).
#' After selecting the initial k clusters, the model performs
#' an iterative process of computing and selecting the mean 
#' of each cluster as the new centroid for the next interation.
#' This parameter controls how many times such iteration 
#' should be performed.
#'	 
#' @param max.clusters: maximum number of clusters to evaluate.
#' Default 20.
#'
#' @param nstarts: how many random sets should be chosen?
#' Default 10. 
#' For example, when finding k clusters, the model initially 
#' randomly selects k data points from x to conform the inital
#' clusters. This parameter specifies how many times the model 
#' should perform this random selection. It is recommended to 
#' use nstarts > 1. This is different to the parameter rep (see above).
#'	    
#'
#' @param scaling.method: feature scaling method. In case the data
#' has not been scaled, whether to use "standardization" or 
#' "z-score". By default, the function assumes that the data has 
#' already been prepared for the Kmeans model (i.e, "na").
#'
#' @param decision.method: whether "silhouette" or "knee" (default)
#' method. Knee method is based on the implementation by Coffey et al 
#' (2019).
#'
#' @param plot.pca: whether to perform a pca plot colored according 
#' to the k clusters (default FALSE)
#'
#' @param elbow.to.pdf: boolean, save elbow figure to pdf
#'
#' @param filename: name of the pdf where to save the elbow figure.
#' Default "elbow"
#'
#' @param outdir: output directory where to store the elbow figure.
#' Default "." (i.e., current directory) 
#'
#' OUTPUT
#' @return kmeans object (see kmeans function from stats package)
#'
#' COMMENTS ON THE K-MEANS ALGORITHM
#' The algorithm of Hartigan and Wong (1979) is used by default
#' Kmeans is sensitive to outlayers and to initialization of the
#' centroids
#' Kmeans is scale dependent and isnt suitable for data of varying
#' shapes and densities
#' Different solutions can converge with random initialization
#' metrics:
#' inertia: sum of squared errors for each cluster (within cluster sse)
#' Silhouette coefficient: measure of how far apart clusters are
#' high silhouette score means that clusters are well separated
#'
#' @examples
#' Assuming that x is a data frame with numeric variables only
#' km <- KmeansElbow(x) # run the model
#' str(km) # check data structure
#' x <- cbind(km$cluster, x) # add clustering results (IDs) to x
#' head(x)
#'
#' @export
KmeansElbow <- function(x 
				,rep = 10
				,max.clusters = 20
				,nstarts = 10
				,scaling.method = "na"
				,decision.method = "knee"
				,plot.pca = FALSE
                ,elbow.to.pdf = FALSE
                ,filename = "elbow"
                ,outdir = "."
                )
{
#------------------		check arguments		----------------#
	# the proposed nr of clusters cannot be greater than
	# the number of potential clusters (data points)
	
	if (is.vector(x) == FALSE && 
	    max.clusters > dim(x)[1])
	{ 
		max.clusters = dim(x)[1]
		cat("max.clusters exceeds available data points
		     and was adjusted to ", max.clusters)
  
	}
	#DEBUG
	#print(max.clusters)

	# if requested, scale the data accordingly
	if(scaling.method == "standardization" |
	   scaling.method == "z-score")
	{
		x = feature.scaling(x, scaling.method)
	}

	# check the decision method
	if(substr(decision.method, start = 1, stop = 3) == "sil")
	{ decision.method = "silhouette" }
	else if (decision.method == "knee")
	{ decision.method = "knee" }
	else 
	{ 
		stop (cat("invalid decision method: ", decision.method))
	}
	
	
#------------------	   end checking arguments	----------------#

#------------------  	     adjust plot area 		----------------#

	# force to have only one plot per window
	# the elbow plot is updated iteratively on the same plot window	
	par(mfrow = c(1,1))

#------------------  	   end of adjust plot area 	----------------#

#------------------  	   Main function procedure 	----------------#


	# empty vectors to store data
	# sse within / total sse
	sse <- vector()
	
	# silhouette score vector
	if(decision.method == "silhouette")
	{ silh.score <- vector() }

	
	# iterative loop
	for (i in 1:max.clusters)
	{
		cluster <- stats::kmeans(x
						,centers = i
					      ,iter.max = rep 
				     		,algorithm = "Hartigan-Wong"    
				     		,nstart = nstarts
						)
		
		# plot elbow method
		
		
		window = 1
		# ensure that display of output is current
		flush.console()
		
		# plot the total sum of squares
		sse[i] <- cluster$tot.withinss / cluster$totss
		#DEBUG
		#print(sse)
		plot(sse, las = 1, type = "l", lwd = 3
		    ,col = "deepskyblue"
		    #, xlim = c(0, i + window)
		    ,ylim = c(0, 1)
		    ,ylab = "SSE within / total SSE"
		    ,xlab = "Nr of clusters"
		    ,main = "Elbow method"
		    )
		
		if(decision.method == "silhouette")
		{
			# compute silhouette score for each number of cluster i
			# based on https://medium.com/codesmart/
			# r-series-k-means-clustering-silhouette-794774b46586
			
			silh.object <- cluster::silhouette(cluster$cluster
								    ,dist(x)
								    )
			if(i > 1)# details in documentation
			{ silh.score[i] <- mean(silh.object[, 3]) }
			else { silh.score[i] <- NA } 
		
			#plot silhouette score
			lines(silh.score, type = "o", lwd = 2, col = "grey")

			# EXPERIMENTAL
			# Silh. score can be biased to first order cluster 
			# separation. Subclusters are typically missed. Thus, 
			# one could try the greater the number of clusters, 
			# the more the ballast

		}
		

		# suspend executions of r expression for given
		# time interval
		Sys.sleep(.09)
		
	} # end of for loop
	
	# set the optimal number of clusters according to the
	# selected decision method
	# silhouette score (range -1 (wrong cluster) to 1, far away from
	# neighboring clusters
	# knee method selects the elbow (knee) of the plot as the
	# interception of two lines at the selected knee that minimize 
	# the sse
	if( decision.method == "silhouette" )
	{
		opt <- which(silh.score == max(silh.score, na.rm = TRUE))
		#print(silh.score[opt])
	} else {
			if(length(sse) > 2)
			{
				opt = knee(sse)
			} else { 
				   warning("knee decision method 
				   requires length > 2")
				 }
		 }
		
	# optimum clusters in plot
	lines(sse[opt] ~ opt, 
	     type = "p", pch = 16, col = "red", cex = 1.3)
	text(x = length(sse)/2, y = 0.7,
	     labels = bquote("selected nr of clusters = " ~ .(opt))
	    )
	
	# prepare and return the kmeans object 
	km <- stats::kmeans(x, centers = opt, iter.max = rep
			,algorithm = "Hartigan-Wong"    
		  	,nstart = nstarts
			)
 
	if(plot.pca == TRUE){ plot.pca(x, km$cluster) }

	# save elbow figure to pdf
    if(elbow.to.pdf)
    {
        if(!grepl(pattern = ".pdf", x = filename))
        { filename <- paste0(filename,".pdf")}
        # open file
        pdf(file = file.path(outdir,filename)
    	   ,width  = 7
           ,height = 5
           )
        #create plot
        plot(sse, las = 1, type = "l", lwd = 3
		    ,col = "deepskyblue"
		    #, xlim = c(0, i + window)
		    ,ylim = c(0, 1)
		    ,ylab = "SSE within / total SSE"
		    ,xlab = "Nr of clusters"
		    ,main = "Elbow method"
		    )
		# optimum clusters in plot
		lines(sse[opt] ~ opt, 
	     type = "p", pch = 16, col = "red", cex = 1.3)
		text(x = length(sse)/2, y = 0.7,
	     labels = bquote("selected nr of clusters = " ~ .(opt))
	    	)
        # close plotting device
        dev.off()
	}

	# return kmeans object
	return (km)			
}
#------------------  End of Main function procedure 	----------------#


####		       END OF K-MEANS MODEL			#########
###############################################################


###############################################################
#' get_pca_components
#'
#' DESCRIPTION
#' this function calculates and returns the pca components from
#' the input data frame df. This is helpful to visualize the outcome
#' of the clustering trask performed by the KmeansElbow function
#'
#' PARAMETERS
#' @param df a data frame with n rows (data points) and p columns
#' (numeric variables)
#'
#' OUTPUT
#' @return a data frame with pca coordinates
#'
#' @export
get_pca_components <- function(df)
{
	df.pca <- FactoMineR::PCA(df
					 ,scale.unit = FALSE
					 ,ncp = 5
					 ,graph = FALSE
					 )


	#get list of pca info:
	list.pca <- factoextra::get_pca_ind(df.pca)
	# check coordinates of the componentes (at all dimensions)
	head(list.pca$coord)

	# store the number of dimensions (i.e., nr of principal components)
	ndim <- length(list.pca$coord[1,])

	# store component's coordinates as data frame
	return (as.data.frame(list.pca$coord[,1:ndim]))

}

####		    		END OF PCA PLOT			#########
###############################################################


###############################################################
####		     PCA PLOT, DATA DISTRIBUTION		#########

plot.pca <- function(df, clusters)
{
	df.pca <- FactoMineR::PCA(df
					 ,scale.unit = FALSE
					 ,ncp = 5
					 ,graph = FALSE
					 )


	#get list of pca info:
	list.pca <- factoextra::get_pca_ind(df.pca)
	# check coordinates of the componentes (at all dimensions)
	head(list.pca$coord)

	# store the number of dimensions (i.e., nr of principal components)
	ndim <- length(list.pca$coord[1,])

	# store component's coordinates as data frame
	df.pca.plot <- as.data.frame(list.pca$coord[,1:ndim])
	#head(df.pca.plot)


	# plot
	# only plot first two components (2D plot)
	# Usually together they explain most of the variance in the data
	# check summary(df.pca) for more information

	# get the coordinates
	x <- df.pca.plot$Dim.1 # princpl. component 1
	y <- df.pca.plot$Dim.2 # princpl. component 2

	# plotting 2d or 3d depending on % of explained variance
	
	if(length(names(df.pca.plot)) > 2 && # more than two components
	   df.pca$eig[2,3] < 90) # less than 90% variation	
	{ # 3d plot
	  z <- df.pca.plot$Dim.3 # princpl. component 3
	  rgl::plot3d(x, y, z, type = "s", size = 1, lit = TRUE, box = FALSE
		  , col = clusters 
		  )
	}
	else if (length(names(df.pca.plot)) == 2){ # two components
		x11()
		plot(y ~ x, pch = 16, las = 1
    	     	     , xlab = "PC1"
     	     	     , ylab = "PC2"
     	     	     , col = clusters
    	     	     )
	     } else {
				print("plotting aborted: nr of components < 2")
			}

}

####		    		END OF PCA PLOT			#########
###############################################################


###############################################################
####		     	    KNEE DECISION METHOD		#########
#
# Developed according to the Knee method described in
# Coffey et al (2019)
# The knee is defined as the intersection point of two lines
# that minimizes the SSE (sum of squared errors)
knee <- function(y	# a vector of length > 2
		    )
{
	# if lenght(y) == 3, there is only one solution
	if(length(y) == 3) { return (2) }
	else
	{
		x <- seq(from = 1, to = length(y), by = 1)
		#DEBUG
		#plot(y)
	
		v <- vector()
		# explore potential knee points
		for(i in 2:(length(y) - 1))
		{
			# adjust linear models to fit a line before and after
			# of the selected knee point
			lm1 <- lm(y[1:i] ~ x[1:i]) # before
			lm2 <- lm(y[i:length(y)] ~ x[i:length(x)]) # after
		
			# get the residuals from each model
			a <- as.vector(unlist(lm1$residuals, use.names = FALSE))
			b <- as.vector(unlist(lm2$residuals, use.names = FALSE))
			
			#DEBUG
			#print(a)
			#print(b)
		
			# SSE of knee point i
			v[i] = sum(a^2) + sum(b^2)
		
			#DEBUG
			#abline(lm1)
			#abline(lm2) 
		
		}
		#print(v)
		# return the knee point evaluated on y
		return(which(v == min(v, na.rm = TRUE)))
	}

}

####		     	 END OF KNEE DECISION METHOD		#########
###############################################################



###############################################################
####		     		FEATURE SCALING			#########

# this function performs feature scaling of a data frame x
# according to, either
# standardization: usually recommended when data does not follow
#		     a gaussian distribution. Apropriate for
#		     algorithms that do not assume any distribution
#		     (e.g., K-nearest neighbor and artificial neural
#			networks)
#	formula: X' = (X - Xmax) / (Xmax - Xmin)
#	scaling range: (0,1)
#
# z-score: 		 can be useful when data follows a gaussian
#			 distribution. However, this is not
#			 necessarily true
#	formula: X' = (X - mu) / sigma; where mu and sigma are
#						the mean and std of X
#	scaling range: does not have bounding
#
# based on: https://www.analyticsvidhya.com/blog/2020/04/
#		feature-scaling-machine-learning-normalization-
#		standardization/
#		
#		and
#		
#		Quinn & Keough 2002: Experimental design and data
# 		analysis for Biologists


feature.scaling <- function(x			 # data frame
				   ,scaling.method # selected method
				   )
{
	# DEBUG
	print(scaling.method)
	
	# loop control
	
	if ( !is.null(dim(x)) ) { top <- dim(x)[2] }
	else { top <- 1 }
	
	for ( i in 1:top)
	{
		if(scaling.method == "standardization")
		{
			x[,i] <- ( x[,i] - min(x[,i]) ) /     # numerator 
				   ( max(x[,i]) - min(x[,i]) ) # denominator

		} else if (scaling.method == "z-score")
		{
			x[,i] <- ( x[,i] - mean(x[,i]) ) / sd(x[,i])
		}
	}
	return(x)
}

####		        END OF FEATURE SCALING		#########
###############################################################

