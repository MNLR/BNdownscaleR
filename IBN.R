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
# y <- loadStationData('/home/w/Dropbox/R/BNdownscale/data/VALUE/AEMET', var = "precip")
# di <-dataInventory('http://meteo.unican.es/tds5/dodsC/interim/interim075.ncml')
 x <- loadGridData('http://meteo.unican.es/tds5/dodsC/interim/interim075.ncml', var = 'tp',
                   lonLim = c(-7,-1), latLim = c(43,43.75), time = "DD", aggr.d = "sum")
# # # Traslado de grid
# x$xyCoords$y <- x$xyCoords$y + 0.25
# #
# #######
# #######
# #x <- getTemporalIntersection(obs = y, prd = x, which.return = "prd")
# #y <- getTemporalIntersection(obs = y, prd = x, which.return = "obs")
#
# x$Data <- x$Data*1000
# x$Data[x$Data >= 1] <- 1
# x$Data[x$Data < 1] <- 0
# save(x , file = "data/interim075.RData")

##
##    EQUALS
##

# load("data/interim075.RData") # loads x (complete)
load("data/AEMET_precip.RData") # loads y
# ##

# grid2 <- prepare_predictors(x = x,y = y)
# load("data/AEMET_precip.RData") # loads y
# grid <- grid2
# grid$x.global <- grid$x.global[ ,c(2,4,6,8,10,12,14,16,18) ]

# grid <- grid2
load("data/grid.RData") # loads grid, contains trimmed grid
data <- prepare_predictors.forBN(grid = grid, rm.na = TRUE, rm.na.mode = "observations")

descbn <- buildDescriptiveCBN(y,
                              structure.learning.algorithm = "hc"
                              )
plotCBN(descbn, dev = TRUE)
ry <- rbn(descbn$BN.fit, n = 4800)

dbn <- buildCBN(data)
dbn.mle <- buildCBN(data, param.learning.method = "mle")

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
# exp.name <- "wgIB.G.pdf"
# dev.print(pdf, file = paste0("exampleplots/", exp.name), width = 30, height = 15)

# tx <- getTemporalIntersection(obs = filterNA(y), prd = filterNA(x), which.return = "prd")
 ty <- getTemporalIntersection(obs = filterNA(y), prd = filterNA(x), which.return = "obs")
 # test2 <- prepare_newdata(newdata = tx, predictor = grid2)
# test2 needs removal of G nodes (remove odd)
# eq.:
# save(test, file = "data/testIB,RData")
load("data/testIB.RData")

###
### Prediction
###

py <- downscaleBN(dbn, x = test, output = "probabilities", prediction.type = "exact",
                  parallelize = TRUE, n.cores = 2 , cluster.type = "FORK")

###
### Simulation
###
sy <- downscaleBN(dbn, x = test,
                        prediction.type = "simulation",
                        parallelize = TRUE, n.cores = 2 , cluster.type = "FORK")

dbnaucS <- aucStation(downscaled = py$member_1, realData = ty$Data, plot.curves = TRUE)
c.thresholds <- findClimatologyThreshold(py$member_1, event.marginalS = dbn$marginals)
pye <- convertEvent( py$member_1, threshold.vector = c.thresholds )

downscaleR.BN:::cTableRates(cTable(predicted = pye, real = ty$Data))
cTable(predicted = pye, real = ty$Data)
distanceBias(real = ty, prediction = pye)

library(verification)
pyba <- purgeBinAbsence(probabilities = py$member_1)

dev.new()
aux <- verify(ty$Data[,1], pyba[,1], frcst.type = "prob", obs.type = "binary",
              thresholds = seq(0,1, by = 0.05))
reliability.plot(aux, titl = "Reliability plot, IB")
