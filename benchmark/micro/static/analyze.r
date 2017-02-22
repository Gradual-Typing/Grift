library(stringr, lib.loc="~/.Rlibs")
library(boot, lib.loc="~/.Rlibs")
library(ggplot2, lib.loc="~/.Rlibs")

makeRandomString <- function(n=1, length=12)
{
  randomString <- c(1:n)                  # initialize vector
  for (i in 1:n)
  {
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                                    length, replace=TRUE),
                             collapse="")
  }
  return(randomString)
}

bootstrap_ci_mean_dist <- function(X, R=5000, n=length(X), ci=.95){
  avg <-mean(X)
  sqrtn <- sqrt(n)
  sample_mean <- function(X, s) {
    spts <- X[s]
    (mean(spts) - avg) /
      (sd(spts) / sqrtn)
  }    
  
  
  mean_dist <- vector("numeric", R)  

  for (i in 1:R){
    mean_dist[i] <- sample_mean(sample(length(X), n, replace = TRUE))
  }
  
  stdev <- sd(X)
  mean_dist <- sort(decreasing = TRUE, mean_dist)
  
  high_percentile <- (1 + ci) / 2
  low_percentile  <- (1 - ci) / 2
  
  ci_index_high <- round(high_percentile * R)
  ci_index_low  <- round(low_percentile * R)
  
  high <- avg - (mean_dist[ci_index_high] * stdev / sqrtn)
  low  <- avg - (mean_dist[ci_index_low] * stdev / sqrtn)
  
 
  list(mean=avg, confidence=ci, ci.low=low, ci.high=high,
       dist=mean_dist)

}

bootstrap_ci_mean_diff <- function(L, R, N=5000, ci=.95){
  avg.l <- mean(L)
  avg.r <- mean(R)
  avg.l_r <- avg.l - avg.r
  len.l <- length(L)
  len.r <- length(R)
  
  sample_diff <- function(indices.l, indices.r) {
    samples.l <- L[indices.l]
    samples.r <- R[indices.r]
    # ((mean(samples.l) - mean(samples.r)) - avg.l_r) /
    #   sqrt((sd(samples.l) / len.l) + (sd(samples.r) / len.r))
    mean(samples.l) - mean(samples.r)
  }

  diff_dist <- vector("numeric", N)

  for (i in 1:N){
    s1 <- sample(len.l, len.l, replace = TRUE)
    s2 <- sample(len.r, len.r, replace = TRUE)
    diff_dist[i] <- sample_diff(s1, s2)
  }
  
  #v <- sqrt((sd(L) / len.l) + (sd(R) / len.r))

  diff_dist <- sort(decreasing = TRUE, diff_dist)
  
  high_percentile <- (1 + ci) / 2
  low_percentile  <- (1 - ci) / 2

  ci_index_high <- round(high_percentile * N)
  ci_index_low  <- round(low_percentile * N)

  #high <- avg.l_r - (diff_dist[ci_index_high] * v)
  high <- diff_dist[ci_index_high]
  #low  <- avg.l_r - (diff_dist[ci_index_low] * v)
  low  <- diff_dist[ci_index_low]

  c(avg.l_r, ci, low, high)
  
}

benchmark_mean_diff <- function(d){
  x0 <- d$benchmark.x0
  x1 <- d$benchmark.x1
  df <- data.frame()
  for (i in 1:length(x0)) {
    D1 <- eval(as.name(x1[i]))
    D0 <- eval(as.name(x0[i]))
    diff <- bootstrap_ci_mean_diff(D1, D0)
    benchmark.x0 <- x0[i]
    benchmark.x1 <- x1[i]
    t.x1_x0 <- diff[1]
    l.x1_x0 <- diff[3]
    h.x1_x0 <- diff[4]
    
    ext <- data.frame(benchmark.x0,
                      benchmark.x1,
                      t.x1_x0, l.x1_x0, h.x1_x0,
                      stringsAsFactors = FALSE)
    df <- rbind(df, ext)
  }
  df
}

bootstrap_ci_mean_diff2 <- 
  function(LL, LR, RL, RR, N=5000, ci=.95){
    avg.ll <- mean(LL)
    avg.lr <- mean(LR)
    avg.rl <- mean(RL)
    avg.rr <- mean(RR)
    avg.l_r <- (avg.ll - avg.lr) - (avg.rl - avg.rr)
    n <- length(LL)
    
    sample_diff <- function(ll, lr, rl, rr){
      (mean(LL[ll]) - mean(LR[lr])) -
        (mean(RL[rl]) - mean(RR[rr]))
    }

  diff_dist <- vector("numeric", N)
  
  for (i in 1:N){
    sll <- sample(n, n, replace = TRUE)
    slr <- sample(n, n, replace = TRUE)
    srl <- sample(n, n, replace = TRUE)
    srr <- sample(n, n, replace = TRUE)
    diff_dist[i] <- sample_diff(sll, slr, srl, srr)
  }
  
  #v <- sqrt((sd(L) / len.l) + (sd(R) / len.r))
  
  diff_dist <- sort(decreasing = TRUE, diff_dist)
  
  high_percentile <- (1 + ci) / 2
  low_percentile  <- (1 - ci) / 2
  
  ci_index_high <- round(high_percentile * N)
  ci_index_low  <- round(low_percentile * N)
  
  #high <- avg.l_r - (diff_dist[ci_index_high] * v)
  high <- diff_dist[ci_index_high]
  #low  <- avg.l_r - (diff_dist[ci_index_low] * v)
  low  <- diff_dist[ci_index_low]
  
  c(avg.l_r, ci, low, high)
  
}

benchmark_mean_diff2 <- function(d){
  x0.h0 <- d$benchmark.x0.h0
  x0.h1 <- d$benchmark.x0.h1
  x1.h0 <- d$benchmark.x1.h0
  x1.h1 <- d$benchmark.x1.h1
  df <- data.frame()
  for (i in 1:length(x0.h0)) {
    D00 <- eval(as.name(x0.h0[i]))
    D01 <- eval(as.name(x0.h1[i]))
    D10 <- eval(as.name(x1.h0[i]))
    D11 <- eval(as.name(x1.h1[i]))
    # This bs is (x0.h0 - x1.h0) - (x0.h1 - x1.h1)
    # The cost of performorm a op minus in 
    # the baseline by hand cost
    diff <- bootstrap_ci_mean_diff2(D10, D00, D11, D01)
    benchmark.x0.h0 <- x0.h0[i]
    t.h0_h1 <- diff[1]
    l.h0_h1 <- diff[3]
    h.h0_h1 <- diff[4]
    
    ext <- data.frame(benchmark.x0.h0,
                      t.h0_h1, l.h0_h1, h.h0_h1,
                      stringsAsFactors = FALSE)
    df <- rbind(df, ext)
  }
  merge(d, df, by=c("benchmark.x0.h0"))
}


strip_data <- function(file_path){
  raw   <- readLines(file_path)
  times <- raw[grep("time", raw)]
  times <- str_match(times, "(\\d+\\.\\d+)")[,2]
  as.numeric(times)
}

bootstrap_benchmark_data <- 
  function(data_dir, diagnostics=FALSE, tmp_dir="./tmp") {
  
  if (diagnostics) {
    acf_dir <- paste(tmp_dir, "acf", sep="/")
    dir.create(acf_dir, recursive=TRUE, showWarnings = FALSE)
  }
    
  df <- data.frame()
  for (benchmark in list.files(data_dir)) {
    path <- paste(data_dir, benchmark, sep="/")
    
    times <- strip_data(path)    
    
    assign(benchmark, times, pos=1)
    
    if (diagnostics) {
      acf(times)
    }
    
    #bs <- bootstrap_ci_mean_dist(times)
    #avg  <- bs[["mean"]]
    #high <- bs[["ci.high"]]
    #low  <- bs[["ci.low"]]
    
    bs <- boot(times, function(X, s) mean(X[s]), R=5000)
    ci <- boot.ci(bs, type="basic", conf=0.95)
    avg  <- ci$t0
    high <- ci$basic[5]
    low  <- ci$basic[4]
    
    df <- rbind(df, data.frame(benchmark, avg, low, high,
                               stringsAsFactors = FALSE)) 
    
  }
  df
}

parse_benchmarks_static <- function(data) {
  data$feature <- ifelse(str_detect(data$benchmark, "fn-app"),
                         "Fun App", "Ref R/W")
  data$cast.type <- ifelse(str_detect(data$benchmark, "Coercions"),
                           "Coercion", "Type-Based")
  data$ref.type <- ifelse(str_detect(data$benchmark, "Monotonic"),
                          "Monotonic", "Guarded")
  data$op.used <- str_detect(data$benchmark, "x1")
  data$hand.coded <- str_detect(data$benchmark, "byHand")
  data[c("benchmark",
         "feature",
         "cast.type",
         "ref.type",
         "op.used",
         "hand.coded",
         "avg",
         "low",
         "high")]
}



t <- bootstrap_benchmark_data("./tmp/out")
d <- parse_benchmarks_static(t)
cns <- colnames(d)
cns <- cns[cns != "feature"]
f <- d[d$feature == "Fun App",][cns]
r <- d[d$feature == "Ref R/W",][cns]

r <- merge(r[ r$op.used,], r[!r$op.used,], 
            by=c("ref.type", "hand.coded", "cast.type"),
            suffixes=c(".x1", ".x0"))

f <- merge(f[ f$op.used,], f[!f$op.used,], 
           by=c("ref.type", "hand.coded", "cast.type"),
           suffixes=c(".x1", ".x0"))


# for each result report x0 - x1
r2 <- benchmark_mean_diff(r)
f2 <- benchmark_mean_diff(f)

f3 <- merge(f, f2, 
            by=c("benchmark.x0", "benchmark.x1"))

r3 <- merge(r, r2, 
            by=c("benchmark.x0", "benchmark.x1"))

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#scale second times to nano seconds
s = 10^9 / 100000

pf3 <- ggplot(f3, aes(x = paste(cast.type,
                                ref.type,
                                hand.coded),
                      y = t.x1_x0 * s,
                      group = paste(cast.type, ref.type),
                      fill = paste(cast.type, ref.type,
                                   ifelse(hand.coded,
                                          "by Hand",
                                          "Schml")))) +
  geom_bar(stat="identity", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=l.x1_x0 * s, ymax=h.x1_x0 * s),
                width=.6,
                position=position_dodge()) +
  xlab("Configuration") +
  ylab("Time (nanoseconds)") +
  ggtitle("Time to Perform a Statically Typed Function Application
while Varying Cast and Reference Representation
with \"by Hand\" baseline") +
  #scale_y_continuous(breaks=0:20*4) +
  #theme_bw()
  scale_fill_manual(name="Configurations",
                    values=cbPalette)
print(pf3)

pr3 <- ggplot(r3, aes(x = paste(cast.type,
                                ref.type,
                                hand.coded),
                      y = t.x1_x0 * s,
                      group = paste(cast.type, ref.type),
                      fill = paste(cast.type, ref.type,
                                   ifelse(hand.coded,
                                          "by Hand",
                                          "Schml")))) +
  geom_bar(stat="identity", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=l.x1_x0 * s, ymax=h.x1_x0 * s),
                width=.6,
                position=position_dodge()) +
  xlab("Configuration") +
  ylab("Time (nanoseconds)") +
  ggtitle("Time to Perform a Static Reference Read and Write
while Varying Cast and Reference Representation
with \"by Hand\" Baseline") +
  #scale_y_continuous(breaks=0:20*4) +
  #theme_bw()
  scale_fill_manual(name="Configurations", values=cbPalette)
print(pr3)


# mtcars$gear = factor(mtcars$gear)
# mtcars$carb = factor(mtcars$carb)
# 
# ggplot(mtcars, aes(x=hp, y=disp, shape=gear, colour=carb)) +
#   geom_point() + 
#   geom_line(aes(group=interaction(carb,gear)))

# for each configuration report (x0 - x1).by_Hand - (x0 - x1).not_by_Hand
f4 <- merge(f3[ f3$hand.coded,], f3[!f3$hand.coded,],
            by=c("ref.type", "cast.type"),
            suffixes=c(".h1", ".h0"))
f5 <- benchmark_mean_diff2(f4)

png("tmp/static_function_overhead.png")
pf5 <- ggplot(f5, aes(x = paste(cast.type, ref.type),
                      y = t.h0_h1 * s,
                      fill = paste(cast.type, ref.type))) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=l.h0_h1 * s, ymax=h.h0_h1 * s),
                width=.5,
                position=position_dodge()) +
  xlab("Configuration") +
  ylab("Time (nanoseconds)") +
  ggtitle("Time Overhead of Performing a Static Function Application
          while Varying Cast and Reference Representation") +
  #scale_y_continuous(breaks=0:20*4) +
  #theme_bw()
  scale_fill_manual(name="Configurations", values=cbPalette) +
  theme(axis.text.x  = element_text(angle=35, hjust=.75, vjust=.75, size=10),
        legend.position="none")
print(pf5)
dev.off()


r4 <- merge(r3[ r3$hand.coded,], r3[!r3$hand.coded,],
            by=c("ref.type", "cast.type"),
            suffixes=c(".h1", ".h0"))
r5 <- benchmark_mean_diff2(r4) 

png("tmp/static_reference_overhead.png")
pr5 <- ggplot(r5, aes(x = paste(cast.type, ref.type),
                      y = t.h0_h1 * s,
                      fill = paste(cast.type, ref.type))) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=l.h0_h1 * s, ymax=h.h0_h1 * s),
                width=.5,
                position=position_dodge()) +
  xlab("Configuration") +
  ylab("Time (nanoseconds)") +
  ggtitle("Time Overhead of Performing a Static Reference Read and Write
          while Varying Cast and Reference Representation") +
  #scale_y_continuous(breaks=0:20*4) +
  #theme_bw()
  scale_fill_manual(name="Configurations", values=cbPalette) +
  theme(axis.text.x  = element_text(angle=35, hjust=.75, vjust=.75, size=10),
        legend.position="none")
print(pr5)
dev.off()

# export data frames for plotting with gnuplot
export.data.frame.csv <-
  function(df, f) {
    df <- df[c("cast.type", "ref.type", "t.h0_h1", "l.h0_h1", "h.h0_h1")]
    df$configuration <- paste(df$cast.type, df$ref.type, sep=" ")
    df$cast.type <- NULL
    df$ref.type  <- NULL
    df["mean overhead (sec)"] <- df$t.h0_h1
    df$t.h0_h1 <- NULL
    df["2.5 percentile"] <- df$l.h0_h1
    df["95.7 percentile"] <- df$h.h0_h1
    df$l.h0_h1 <- NULL
    df$h.h0_h1 <- NULL
    print(df)
    write.csv(df, file=f, col.names = FALSE, quote = FALSE)
}

export.data.frame.csv(f5, "static-function-overhead.csv")
export.data.frame.csv(r5, "static-reference-overhead.csv")





