#####################################################
library(loadeR)                                     #
#####################################################
loginUDG('Mitor', '450cacahuetes')                  #
#####################################################

### datasets
y <- loadStationData('/home/w/Dropbox/R/BNdownscale/data/VALUE/AEMET', var = "precip")
# x <- loadGridData('/home/w/Dropbox/R/BNdownscale/data/VALUE/Interim', var='precip')
#di <-dataInventory('http://meteo.unican.es/tds5/dodsC/interim/interim075.ncml')
x <- loadGridData('http://meteo.unican.es/tds5/dodsC/interim/interim075.ncml', var = 'tp',
                   lonLim = c(-7,-1), latLim = c(43,43.75), time = "DD" ,aggr.d = "mean")

#x <- loadGridData('http://meteo.unican.es/tds5/dodsC/interim/daily/interim20_daily.ncml', var = 'TP',
#                  lonLim = c(-7,-1), latLim = c(40,44))

x$Data = x$Data*1000
y$Data[y$Data >= 1] <- 1
y$Data[y$Data < 1] <- 0

x$Data[x$Data >= 1] <- 1
x$Data[x$Data < 1] <- 0

# Traslado de grid

x <- interpGrid(x, new.coordinates = list(x = x$xyCoords$x, y = x$xyCoords$y + 0.25), method = "nearest")

###########
###########
#x <- getTemporalIntersection(obs = y, prd = x, which.return = "prd")
#y <- getTemporalIntersection(obs = y, prd = x, which.return = "obs")
grid <- prepare_predictors(x = x,y = y)
data <- prepare_predictors.forBN(grid = grid, rm.na = TRUE, rm.na.mode = "observations")

dbn <- build.downscalingBN(data,
                           forbid.global.arcs = TRUE, forbid.local.arcs = FALSE,
                           bnlearning.algorithm = "gs",
                           #bnlearning.args.list = list(distance = 0.5),
                           param.learning.method = "bayes",
                           output.marginals = TRUE,
                           compile.junction = TRUE,
                           parallelize = TRUE, n.cores= NULL, cluster.type = "PSOCK",
                           two.step = TRUE,
                           return.first = FALSE,
                           bnlearning.algorithm2 = "hc.local",
                           bnlearning.args.list2 = list(distance = 0.5)
                           )

plot.DBN(dbn, dev = TRUE)

years <- c(1991, 1992)
tx <- subsetGrid(x, years = years)

tx <- getTemporalIntersection(obs = y, prd = x, which.return = "prd")
ty <- getTemporalIntersection(obs = y, prd = x, which.return = "obs")

tindex <- complete.cases(ty$Data)
ty$Data <- ty$Data[tindex, ]
grid$x.global <- grid$x.global[tindex, ]
tx$Data <- tx$Data[tindex, , ] #time lat lon


test <- prepare_newdata(newdata = tx,
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


