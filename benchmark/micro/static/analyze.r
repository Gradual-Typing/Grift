library(stringr, lib.loc="~/.Rlibs")
library(boot, lib.loc="~/.Rlibs")

benchmarks <- list.files("./tmp/out")

df <- data.frame()

for (benchmark in benchmarks){
    path <- paste("./tmp/out", benchmark, sep="/")

    raw       <- readLines(path)
    times <- raw[grep("time", raw)]
    times <- str_match(times, "(\\d+\\.\\d+)")[,2]
    times <- as.numeric(times)

    basename <- paste(benchmark, ".png", sep="")
    png(filename=paste("./tmp/lag/", basename, sep=""))
    dev.off()

    png(filename=paste("./tmp/acf/", basename, sep=""))
    acf(times)
    dev.off()

    avg <- mean(times)
    
    samplemean1 <- function(X, s) mean(X[s])

    meanboot1 <- boot(times, samplemean1, R=10000)

    png(filename=paste("./tmp/boot/", basename, sep=""))
    plot(meanboot1)
    dev.off()

    meanbootci1 <- boot.ci(meanboot1, type="basic")
    high <- meanbootci1$basic[4]
    low  <- meanbootci1$basic[5]

    df <- rbind(df, data.frame(benchmark, avg, low, high)) 

}



