####################################################
library(downscaleR.BN)                             #
####################################################
####################################################
####################################################
####################################################

###################################################
library(loadeR)                                   #
loginUDG('Mitor', '450cacahuetes')                #
###################################################

### datasets
#y <- loadStationData('/home/w/Dropbox/R/BNdownscale/data/VALUE/AEMET', var = "precip")
#di <-dataInventory('http://meteo.unican.es/tds5/dodsC/interim/interim075.ncml')
# x <- loadGridData('http://meteo.unican.es/tds5/dodsC/interim/interim075.ncml', var = 'tp',
#                    lonLim = c(-7,-1), latLim = 43.5, time = "DD" ,aggr.d = "mean")
# # Traslado de grid
# # x <- interpGrid(x, new.coordinates = list(x = x$xyCoords$x, y = x$xyCoords$y + 0.25), method = "nearest")
# # x$xyCoords$y <- x$xyCoords$y + 0.25
#
# #######
# #######
# #x <- getTemporalIntersection(obs = y, prd = x, which.return = "prd")
# #y <- getTemporalIntersection(obs = y, prd = x, which.return = "obs")
#
# x$Data <- x$Data*1000
# x$Data[x$Data >= 1] <- 1
# x$Data[x$Data < 1] <- 0
# save(x , file = "data/interim075_43.75_-7_-1_DDmean.RData")

##
##    EQUALS
##

 load("data/interim075_43_43.75_-7_-1_DDmean.RData") # loads x (complete grid)
# load("data/AEMET_precip.RData") # loads y
# ##
# grid2 <- prepare_predictors(x = x,y = y)

load("data/AEMET_precip.RData") # loads y

load("data/grid.RData") # loads grid
data <- prepare_predictors.forBN(grid = grid, rm.na = TRUE, rm.na.mode = "observations")

descbn <- buildDescriptiveCBN(y,
                              structure.learning.algorithm = "hc",
                              structure.learning.args.list = list(distance = 0.5)
                              )
plotCBN(descbn, dev = TRUE, nodes = 1)

dbn <- buildCBN(data)

dbncomplex <- buildCBN(data,
                       structure.learning.algorithm = "mmhc",
                       #structure.learning.args.list = list(distance = 2),
                       param.learning.method = "bayes",
                       forbid.GG = FALSE, forbid.DD = FALSE,
                       structure.learning.steps = c("local", "global"),
                       fix.intermediate = TRUE,
                       structure.learning.algorithm2 = "tabu",
                       #structure.learning.args.list2 = list( distance = 2 ),
                       return.intermediate = TRUE,
                       compile.junction = FALSE,
                       parallelize = TRUE, n.cores = NULL, cluster.type = "FORK"
                       )

plotCBN(dbn, dev = TRUE)
plotCBN(dbncomplex$intermediateDBN1, dev = TRUE, nodes = 1)
plotCBN(dbncomplex, dev = TRUE)
# exp.name <- "phiCG.pdf"
# dev.print(pdf, file = paste0("exampleplots/", exp.name), width = 30, height = 15)

tx <- getTemporalIntersection(obs = filterNA(y), prd = filterNA(x), which.return = "prd")
ty <- getTemporalIntersection(obs = filterNA(y), prd = filterNA(x), which.return = "obs")
test <- prepare_newdata(newdata = tx, predictor = grid)

###
### Prediction
###
pycomplex <- downscaleBN(dbncomplex, x = test, output = "probabilities", prediction.type = "exact",
                         parallelize = TRUE, n.cores = 3 , cluster.type = "FORK")
py <- downscaleBN(dbn, x = test, output = "probabilities", prediction.type = "exact",
                  parallelize = TRUE, n.cores = 3 , cluster.type = "FORK")

###
### Simulation
###
sy <- downscaleBN(dbn, x = test,
                        prediction.type = "simulation",
                        parallelize = TRUE, n.cores = 3 , cluster.type = "FORK")

dbnaucS <- aucStation(prediction = py$member_1, realData = ty$Data, plot.curves = TRUE)
pye <- convertEvent( py$member_1, threshold.vector = 0.5 )

cTableRates(cTable(predicted = pye, real = ty$Data))
cTable(predicted = pye, real = ty$Data)
distanceBias(real = ty, prediction = pye)

#aucvs(realData = ty$Data, downscaled = py$member_1, downscaled2 = sy$member_1,
#         is.event2 = TRUE)

