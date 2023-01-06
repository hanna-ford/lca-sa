#run just the LCPs
#run from a single point to all points
#to run to and from for each point 

ptm <- proc.time()

mc.bmc <- leastcostpath::create_FETE_lcps(
  cost_surface = slope_altitude_cs, 
  locations = sample.final.sp[1:10, ], 
  cost_distance = FALSE,
  parallel = TRUE,
  ncores = 10
  )

proc.time() - ptm

#sanity check - plot the data so far
plot(raster(slope_altitude_cs), col = grey.colors(100))
#plot(sample, add = TRUE)
plot(sample.final.sp, add=TRUE, col="red")
plot(barrier.sp, col = "blue", add=TRUE)
plot(mc.bmc, col = "red", add=TRUE)


# 
# # A second option - trying to decide which way to go with this
# # my second option is creating duplicates - would need to keep reviewing, the "built-in" solution
# # is also slightly faster (by 20 secs on a test run of 2 pts near each other)
# library(leastcostpath)
# library(foreach)
# library(doParallel)
# 
# # this function is called within the foreach
# # View(leastcostpath::create_lcp)
# 
# generate_lcps <- function (x, locations, cost_distance, ncores)
# {
#   
#   myCluster <- makeCluster(ncores)
#   registerDoParallel(myCluster)
#   
#   #expand (Jack prefers flatten) the grid to get the to and from locations
#   network <- expand.grid(1:nrow(locations), 1:nrow(locations))
#   #removing where the to and from locations are the same point
#   network <- network[network[, 1] != network[, 2], ]
#   
#   #setting up to create lcps
#   lcp_network <- foreach::foreach(i = 1:nrow(network), .packages = c("leastcostpath"), 
#                                   .errorhandling = "remove", .combine = "rbind") %dopar% 
#     {
#       lcp <- (create_lcp_network(cost_surface = x, locations = locations, nb_matrix = network, cost_distance = cost_distance, parallel = FALSE))
#       #lcp$originID <- network[i, 1]
#       #lcp$destinationID <- network[i, 2]
#       return(lcp)
#     }
#   
#   stopCluster(myCluster)
#   
#   return(lcp_network)
# }
# 
# ptm <- proc.time()
# 
# lcps2 <- generate_lcps(x = slope_altitude_cs, locations = sample.final.sp, cost_distance = FALSE, ncores = 30)
# 
# proc.time() -ptm