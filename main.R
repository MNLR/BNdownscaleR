#####################################################
library(loadeR)                                     #
library(devtools)
library(downscaleR.BN)
#####################################################
loginUDG('Mitor', '450cacahuetes')                  #
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
grid <- prepare_predictors(x = x,y = y)
data <- prepare_predictors.forBN(grid = grid, rm.na = TRUE, rm.na.mode = "observations")
#Ddata <- prepareDataDynamicBN(data, 3)

descbn <- buildDescriptiveBN(y, structure.learning.algorithm = "hc", structure.learning.args.list = list(distance = 1))
plotDBN(descbn, dev = TRUE, nodes = 1)

dbn <- build.downscalingBN(data,
                           structure.learning.algorithm = "mmhc",
                           #structure.learning.args.list = list(distance = 2),
                           param.learning.method = "bayes",
                           forbid.GG = TRUE, forbid.DD = FALSE,
                           dynamic = TRUE, epochs = 2, remove.past.G = TRUE,
                           keep.dynamic.distance = TRUE,
                           structure.learning.steps = c("local", "global", "past"),
                           fix.intermediate = TRUE,
                           structure.learning.algorithm2 = "tabu",
                           structure.learning.args.list2 = list( distance = 1 ),
                           structure.learning.algorithm3 = "hc",
                           structure.learning.args.list3 = list( distance = 0.1 ),
                           return.intermediate = TRUE,
                           forbid.backwards = FALSE, forbid.past.dynamic.GD = TRUE,
                           forbid.dynamic.GG = TRUE, forbid.past.DD = TRUE,
                           output.marginals = FALSE,
                           compile.junction = FALSE,
                           parallelize = TRUE, n.cores = NULL, cluster.type = "FORK"
                           )

plotDBN(dbn, dev = TRUE)
plotDBN(dbn$intermediateDBN1, dev = TRUE, nodes = 1)

years <- c(1991, 1992)
tx <- subsetGrid(x, years = years)

tx <- getTemporalIntersection(obs = y, prd = x, which.return = "prd")
ty <- getTemporalIntersection(obs = y, prd = x, which.return = "obs")

tindex <- complete.cases(ty$Data)
ty$Data <- ty$Data[tindex, ]
grid$x.global <- grid$x.global[tindex, ]
tx$Data <- tx$Data[tindex, , ] #time lat lon


test <- prepare_newdata(newdata = x,
                predictor = grid)

py <- downscale.BN(dbn, x = test,
                        prediction.type = "probabilities", #event = "1", threshold.vector = NULL,
                        parallelize = TRUE, n.cores = NULL , cluster.type = "FORK")

comparablesy <- py$member_1[tindex,,]
comparablesy <- subsetGrid(y, years = years)

c.table(predicted = y$member_1, real = y$Data)
auc.DBN(downscaled = py$member_1, realData = comparablesy$Data, plot.curves = TRUE)



#######################
#######################
#######################   CV
#######################
#######################

ix <- getTemporalIntersection(obs = y, prd = x, which.return = "prd")
iy <- getTemporalIntersection(obs = y, prd = x, which.return = "obs")

dS <- dataSplit(ix, iy, f = 3/4, type = "random")

descbn <- buildDescriptiveBN(y, structure.learning.algorithm = "hc", structure.learning.args.list = list(distance = 0.75),
                             compile.junction = FALSE, output.marginals = TRUE)
