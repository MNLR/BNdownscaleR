####################################################
library(downscaleR.BN)                             #
####################################################
####################################################
####################################################
####################################################

library(loadeR)
loginUDG('Mitor', '450cacahuetes')
dataInventory("http://meteo.unican.es/tds5/dodsC/interim/daily/interim20_daily.ncml")
# xx <- loadGridData(dataset = "http://meteo.unican.es/tds5/dodsC/interim/daily/interim20_daily.ncml",
#                    var = "TP", lonLim = c(4,16), latLim = c(46,58), years = 1979:2008
#                    )
 dataInventory(dataset = "data/VALUE_ECA_53_Germany_spatial_v1.zip")
 eca53Ger <- loadStationData(dataset = "data/VALUE_ECA_53_Germany_spatial_v1.zip",
                             var = "precip")
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

gridGER <- prepareData(x = xx, y = yy)
dataGER <- prepare_predictors.forBN(grid = gridGER)

descGER <- buildDescriptiveCBN(yy, structure.learning.algorithm = "tabu",
                               structure.learning.args.list = list(tabu = 10^4)
                               )
plotCBN(descGER, dev = TRUE)
# exp.name <- "descGER_0_1_10_I_tabu4.pdf"
# dev.print(pdf, file = paste0("exampleplots/", exp.name), width = 15, height = 15)


dbnGER <- buildCBN(dataGER, structure.learning.args.list = list(tabu = 10^3))

dbnGERalt <- buildCBN(dataGER,
                      structure.learning.algorithm = "gs",
                      structure.learning.args.list = list(alpha = 0.001),
                      param.learning.method = "bayes", structure.learning.steps = 1,
                      forbid.GG = FALSE,
                      structure.learning.algorithm2 = "hc",
                      parallelize = TRUE, n.cores = 2
                      )

plotCBN(dbnGER, dev = TRUE)
# exp.name <- "dbn.pdf"
# dev.print(pdf, file = paste0("exampleplots/", exp.name), width = 15, height = 15)

txx <- getTemporalIntersection(obs = filterNA(yy), prd = filterNA(xx), which.return = "prd")
tyy <- getTemporalIntersection(obs = filterNA(yy), prd = filterNA(xx), which.return = "obs")
testGER <- prepareNewData(newdata = txx,data.structure = gridGER)

###
### Prediction
###
pyy <- predictBN(dbnGER, x = testGER,
                  output = "probabilities", prediction.type = "exact",
                  parallelize = TRUE, n.cores = 2 , cluster.type = "FORK")
###
### Simulation:
###
syy <- predictBN(dbnGER, x = testGER,
                   output = "event", prediction.type = "simulation",
                   parallelize = TRUE, n.cores = 2 , cluster.type = "FORK")

###
### Generation
###

gyy <- rbn(descGER$BN.fit, n = 10000)

aucGER <- aucStation(downscaled = pyy$member_1, realData = tyy$Data)
aucGER

cthreshold <- findClimatologyThreshold(pyy$member_1, dbnGER$marginals)
pyye <- convertEvent( pyy$member_1, threshold.vector = 0.5)
table(pyye)
table(tyy$Data)
downscaleR.BN:::cTableRates(cTable(predicted = pyye, real = tyy$Data ))

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


library(verification)
pyyba <- purgeBinAbsence(probabilities = pyy$member_1)

dev.new()
aux <- verify(tyy$Data[,1], pyyba[,1], frcst.type = "prob", obs.type = "binary",
              thresholds = seq(0,1, by = 0.1))
reliability.plot(aux, titl = "Reliability plot, GER")
# exp.name <- "wgG.pdf"
# dev.print(pdf, file = paste0("exampleplots/", exp.name), width = 30, height = 15)


#
##
### Weather gens
##
#

wg <- buildDynamicCBN(y = gridGER$y, structure.learning.algorithm = "tabu",
                      structure.learning.args.list = list(tabu = 10^2)
                      )
dev.new()
plotCBN(wg, dev = TRUE)

wgG <- buildDynamicCBN(y = yy, x = xx, structure.learning.algorithm = "tabu",
                       structure.learning.args.list = list(tabu = 10^2)
                       )
dev.new()
plotCBN(wgG, dev = TRUE)

pywg <- predictBN(cbn = wg, x = NULL, y = yy)
pywgG <- predictBN(cbn = wgG, x = xx, y = yy)


n_ <- 10000
initial <- as.matrix(wg$training.data[1,1:wg$NY])
print(initial)
gen <- generateWeatherBN(wg = wg, n = n_, initial = initial,
                         initial.date = rownames(wg$training.data)[1], inference.type = "exact"
)

wgG <- buildDynamicCBN(y = data, structure.learning.algorithm = "tabu",
                       structure.learning.args.list = list(tabu = 10^4), compile.junction = TRUE
)

genG <- generateWeatherBN(wg = wgG, x = grid$x.global,
                          initial.date = rownames(wgG$training.data)[1], inference.type = "exact")

wg.2step <- buildDynamicCBN(y = gridGER$y, structure.learning.algorithm = "tabu",
                      structure.learning.args.list = list(tabu = 10^2, k = 1),
                      structure.learning.steps = 2,
                      structure.learning.args.list2 = list(k = 0.5),
                      fix.intermediate = TRUE
                      )
plotCBN(wg.2step, dev = TRUE)


