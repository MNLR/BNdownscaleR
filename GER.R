####################################################
library(downscaleR.BN)                             #
####################################################
####################################################
####################################################
####################################################

# library(loadeR)
# loginUDG('Mitor', '450cacahuetes')
# dataInventory("http://meteo.unican.es/tds5/dodsC/interim/daily/interim20_daily.ncml")
# xx <- loadGridData(dataset = "http://meteo.unican.es/tds5/dodsC/interim/daily/interim20_daily.ncml",
#                    var = "TP", lonLim = c(4,16), latLim = c(46,58), years = 1979:2008
#                    )
# yy <- loadStationData(dataset = "data/VALUE_ECA_53_Germany_spatial_v1.zip",
#                      var = "precip", years = 1979:2008
#                      )
# xx$Data <- xx$Data*1000
# xx$Data[xx$Data >= 1] <- 1
# xx$Data[xx$Data < 1] <- 0
# yy$Data[yy$Data >= 1] <- 1
# yy$Data[yy$Data < 1] <- 0

##
##    EQUALS
##
load("data/xx.RData")
load("data/yy.RData")
##

gridGER <- prepare_predictors(x = xx, y = yy)
dataGER <- prepare_predictors.forBN(grid = gridGER)

descGER <- buildDescriptiveCBN(yy, structure.learning.algorithm = "hc")
plotCBN(descGER, dev = TRUE, nodes = 1)

dbnGER <- buildCBN(dataGER,
                  structure.learning.algorithm = "hc", #structure.learning.args.list = list(distance = 2),
                  param.learning.method = "bayes"
                  )

dbnGERalt <- buildCBN(dataGER,
                      structure.learning.algorithm = "mmhc", #structure.learning.args.list = list(distance = 2),
                      param.learning.method = "bayes", structure.learning.steps = 2,
                      forbid.GG = FALSE,
                      structure.learning.algorithm2 = "hc"
                      )

plotCBN(dbnGER, dev = TRUE)
# exp.name <- "wgGER.pdf"
# dev.print(pdf, file = paste0("exampleplots/", exp.name), width = 15, height = 15)

txx <- getTemporalIntersection(obs = filterNA(yy), prd = filterNA(xx), which.return = "prd")
tyy <- getTemporalIntersection(obs = filterNA(yy), prd = filterNA(xx), which.return = "obs")
testGER <- prepare_newdata(newdata = txx,
                        predictor = gridGER)

###
### Prediction
###
pyy <- downscaleBN(dbnGER, x = testGER,
                  output = "probabilities", prediction.type = "exact",
                  parallelize = TRUE, n.cores = 2 , cluster.type = "FORK")
###
### Simulation:
###
syy <- downscaleBN(dbnGER, x = testGER,
                   output = "event", prediction.type = "simulation",
                   parallelize = TRUE, n.cores = 3 , cluster.type = "FORK")

###
### Generation
###

gyy <- rbn(descGER$BN.fit, n = 10000)

aucGER <- aucStation(downscaled = pyy$member_1, realData = tyy$Data)
c(max(aucGER), min(aucGER), mean(aucGER))

cthreshold <- findClimatologyThreshold(pyy$member_1, dbnGER$marginals[2 , ])
pyye <- convertEvent( pyy$member_1, threshold.vector = cthreshold)
table(pyye)
table(tyy$Data)
cTableRates(cTable(predicted = pyye, real = tyy$Data ))

phiGGER <- measureMatrix(dataGER$data[ , dataGER$x.names])
dGGER <- measureMatrix(list(Data = dataGER$data[ , dataGER$x.names],
                            xyCoords = t(dataGER$positions[ , dataGER$x.names] )),
                       measure = "distance"
                      )
dev.new()
distanceBias(real = tyy, prediction = pyye)
points(dGGER, phiGGER, col = "green")

dev.new()
distanceBias(real = tyy,
             prediction = convertEvent( pyy$member_1,
                                        threshold.vector = rep(0.5, length(dataGER$y.names))
                                        )
             )
points(dGGER, phiGGER, col = "green")

#aucvs(realData = tyy$Data, downscaled = pyy$member_1, downscaled2 = syy$member_1,
#         is.event2 = TRUE)

distanceBias(real = tyy,
             prediction = pyy$member_1, third = syy$member_1, fourth = gyy
             )
downscaleR.BN:::cTableRates(cTable(predicted = syy, real = tyy$Data ))

distanceBias(real = tyy,
             prediction = pyye, third = syy$member_1, fourth = gyy,
             legend_ = c("Real", "Predicted", "Simulated", "Generated") , cex = 0.75
            )

#ppyy <- tyy
#ppyy$Data <- purgeBinAbsence( pyy$member_1 )
#reliabilityCategories(hindcast = ppyy  , obs = tyy)


library(verification)
pyyba <- purgeBinAbsence(probabilities = pyy$member_1)

dev.new()
aux <- verify(tyy$Data[,1], pyyba[,1], frcst.type = "prob", obs.type = "binary",
              thresholds = seq(0,1, by = 0.05))
reliability.plot(aux, titl = "Reliability plot, GER")
# exp.name <- "rpGER.pdf"
# dev.print(pdf, file = paste0("exampleplots/", exp.name), width = 15, height = 15)

