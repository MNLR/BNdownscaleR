####################################################
library(downscaleR.BN)                             #
####################################################
####################################################
####################################################
####################################################

#####################################################
# library(loadeR)                                   #
# loginUDG('Mitor', '450cacahuetes')                #
#####################################################

### datasets
#y <- loadStationData('/home/w/Dropbox/R/BNdownscale/data/VALUE/AEMET', var = "precip")
#di <-dataInventory('http://meteo.unican.es/tds5/dodsC/interim/interim075.ncml')
#x <- loadGridData('http://meteo.unican.es/tds5/dodsC/interim/interim075.ncml', var = 'tp',
#                   lonLim = c(-7,-1), latLim = c(43,43.75), time = "DD" ,aggr.d = "mean")
# Traslado de grid
# x <- interpGrid(x, new.coordinates = list(x = x$xyCoords$x, y = x$xyCoords$y + 0.25), method = "nearest")

#######
#######
#x <- getTemporalIntersection(obs = y, prd = x, which.return = "prd")
#y <- getTemporalIntersection(obs = y, prd = x, which.return = "obs")

##
##    EQUALS
##
load("data/interim075_43_43.75_-7_-1_DDmean.RData") # loads x
load("data/AEMET_precip.RData") # loads y
##

grid <- prepare_predictors(x = x,y = y)
data <- prepare_predictors.forBN(grid = grid, rm.na = TRUE, rm.na.mode = "observations")
#Ddata <- prepareDataDynamicBN(data, 3)

descbn <- buildDescriptiveBN(y,
                             structure.learning.algorithm = "hc",
                             structure.learning.args.list = list(distance = 1)
                             )
plotDBN(descbn, dev = TRUE, nodes = 1)

dbn <- build.downscalingBN(data)

dbncomplex <- build.downscalingBN(data,
                                  structure.learning.algorithm = "mmhc",
                                  structure.learning.args.list = list(distance = 2),
                                  param.learning.method = "bayes",
                                  forbid.GG = TRUE, forbid.DD = FALSE,
                                  dynamic = TRUE, epochs = 2, remove.past.G = TRUE,
                                  keep.dynamic.distance = TRUE,
                                  structure.learning.steps = c("local", "global", "past"),
                                  fix.intermediate = TRUE,
                                  structure.learning.algorithm2 = "tabu",
                                  structure.learning.args.list2 = list( distance = 2 ),
                                  structure.learning.algorithm3 = "hc",
                                  structure.learning.args.list3 = list( distance = 0.1 ),
                                  return.intermediate = TRUE,
                                  forbid.backwards = FALSE, forbid.past.dynamic.GD = TRUE,
                                  forbid.dynamic.GG = TRUE, forbid.past.DD = TRUE,
                                  compile.junction = FALSE,
                                  parallelize = TRUE, n.cores = NULL, cluster.type = "FORK"
                                  )

plotDBN(dbn, dev = TRUE)
plotDBN(dbncomplex$intermediateDBN1, dev = TRUE, nodes = 1)
plotDBN(dbncomplex)
# exp.name <- "phiCG.pdf"
# dev.print(pdf, file = paste0("exampleplots/", exp.name), width = 15, height = 15/2)

tx <- getTemporalIntersection(obs = filterNA(y), prd = filterNA(x), which.return = "prd")
ty <- getTemporalIntersection(obs = filterNA(y), prd = filterNA(x), which.return = "obs")
test <- prepare_newdata(newdata = tx,
                        predictor = grid)

###
### Prediction
###
py <- downscaleBN(dbn, x = test, output = "probabilities", prediction.type = "exact",
                  parallelize = FALSE, n.cores = 3 , cluster.type = "FORK")

###
### Simulation
###
sy <- downscaleBN(dbn, x = test,
                        prediction.type = "simulation",
                        parallelize = TRUE, n.cores = 3 , cluster.type = "FORK")


dbnaucS <- aucStation(prediction = py$member_1, realData = ty$Data, plot.curves = TRUE)
pye <- convertEvent( py$member_1, threshold.vector = 0.5)

cTableRates(cTable(predicted = pye, real = ty$Data))
cTable(predicted = pye, real = ty$Data)
distanceBias(real = ty, prediction = pye )

#aucvs(realData = ty$Data, downscaled = py$member_1, downscaled2 = sy$member_1,
#         is.event2 = TRUE)

