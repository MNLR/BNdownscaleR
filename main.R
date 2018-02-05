#####################################################
library(loadeR)                                     #
library(downscaleR)                                 #
library(transformeR)                                #
library(bnlearn)                                    #
library(gRain)                                      #
library(igraph)                                     #
library(shape)                                      #
library(flexclust)                                  #
library(parallel)                                   #
library(sfsmisc)                                    #
loginUDG('Mitor', '450cacahuetes')                  #
#####################################################

### datasets
y <- loadStationData('/home/w/Dropbox/R/BNdownscale/data/VALUE/AEMET', var = "precip")
# x <- loadGridData('/home/w/Dropbox/R/BNdownscale/data/VALUE/Interim', var='precip')
#di <-dataInventory('http://meteo.unican.es/tds5/dodsC/interim/interim075.ncml')
x <- loadGridData('http://meteo.unican.es/tds5/dodsC/interim/interim075.ncml', var = 'tp',
                   lonLim = c(-7,-1), latLim = c(43,43.75))

x <- loadGridData('http://meteo.unican.es/tds5/dodsC/interim/daily/interim20_daily.ncml', var = 'TP',
                  lonLim = c(-7,-1), latLim = c(40,44))

x$Data = x$Data*1000
y$Data[y$Data >= 1] <- 1
y$Data[y$Data < 1] <- 0

x$Data[x$Data >= 1] <- 1
x$Data[x$Data < 1] <- 0

###########
###########

grid <- prepare_predictors(x = x,y = y)
data <- prepare_predictors.forBN(grid = grid, rm.na = TRUE, rm.na.mode = "observations")
dbn <- build.downscalingBN(data,
                           forbid.global.arcs = FALSE, forbid.local.arcs = FALSE,
                           bnlearning.algorithm = "gs",
                           bnlearning.args.list = list(),
                           param.learning.method = "mle",
                           output.marginals = FALSE,
                           compile.junction = FALSE,
                           parallelize = TRUE, n.cores= NULL, cluster.type = "PSOCK",
                           two.step = TRUE,
                           return.first = FALSE,
                           bnlearning.algorithm2 = "hc",
                           bnlearning.args.list2 = list()
                           )

tx <- subsetGrid(x, years = c(1991))
test <- prepare_newdata(newdata = tx,
                predictor = grid)

ty <- downscale.BN(dbn, x = test,
                      prediction.type = "probabilities", event = "1", threshold.vector = NULL,
                      parallelize = TRUE, n.cores = NULL , cluster.type = "FORK")


