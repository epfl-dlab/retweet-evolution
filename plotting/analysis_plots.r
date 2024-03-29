SAVE_PLOTS <- TRUE

DATA_DIR <- sprintf('%s/github/retweet-evolution/plotting/data', Sys.getenv('HOME'))
PLOT_DIR <- sprintf('%s/github/retweet-evolution/plotting/plots', Sys.getenv('HOME'))

COL <- c("#000000", "#E69F00", "#56B4E9", "#D55E00", "#009E73", "#0072B2", "#CC79A7", "#F0E442")
COL_LIGHT <- sprintf('%s44', COL)


# Fig. 1a: Evolution of retweet counts

dfs <- NULL

for (i in 1:4) {
  wmin <- i*100
  wmax <- (i+1)*100
  dfs[[i]] <- read.table(sprintf('%s/fig1a/%d_%d.csv', DATA_DIR, wmin, wmax), header=TRUE)
}

xlim <- range(do.call(rbind, dfs)[,'x'])
ylim <- c(0,0.85) # range(do.call(rbind, dfs)[,'y'])
lwd <- 2

if (SAVE_PLOTS) pdf(sprintf('%s/fig1a.pdf', PLOT_DIR), width=1.7, height=1.4, pointsize=6,
                      family='Helvetica', useDingbats=FALSE)
par(mar=c(3.5, 3.5, 1.0, 0.5))
for (i in 4:1) {
  df <- dfs[[i]]
  wmin <- i*100
  wmax <- (i+1)*100
  if (i == 4) {
    plot(df$x, df$y, type='l', xlim=xlim, ylim=ylim, col=COL[i], bty='n', lwd=lwd, axes=FALSE,
         xlab='', ylab='')
  } else {
    lines(df$x, df$y, col=COL[i], lwd=lwd)
  }
}
axis(1); mtext('Week on Twitter', side=1, line=2.5)
axis(2); mtext('Median mean retweets', side=2, line=2.5)
legend('bottomright', legend=sprintf('%d-%d weeks', seq(100,400,100), seq(200,500,100)), lty=1, lwd=lwd,
       bty='n', col=COL, inset=c(0,0))
if (SAVE_PLOTS) dev.off()


# Fig. 1b: Evolution of per-follower retweet rates

dfs <- NULL

for (i in 1:4) {
  wmin <- i*100
  wmax <- (i+1)*100
  df <- read.table(sprintf('%s/fig1b/%d_%d.csv', DATA_DIR, wmin, wmax), header=TRUE)
  df$y <- 1000 * df$y
  dfs[[i]] <- df
}

xlim <- range(do.call(rbind, dfs)[,'x'])
ylim <- range(do.call(rbind, dfs)[,'y'])
lwd <- 2

if (SAVE_PLOTS) pdf(sprintf('%s/fig1b.pdf', PLOT_DIR), width=1.7, height=1.4, pointsize=6,
                      family='Helvetica', useDingbats=FALSE)
par(mar=c(3.5, 3.5, 1.0, 0.5))
for (i in 4:1) {
  df <- dfs[[i]]
  if (i == 4) {
    plot(df$x, df$y, type='l', xlim=xlim, ylim=ylim, col=COL[i], bty='n', lwd=lwd, axes=FALSE,
         xlab='', ylab='')
  } else {
    lines(df$x, df$y, col=COL[i], lwd=lwd)
  }
}
axis(1); mtext('Week on Twitter', side=1, line=2.5)
axis(2); mtext('Retweets per 1000 followers', side=2, line=2.5)
legend('topright', legend=sprintf('%d-%d weeks', seq(100,400,100), seq(200,500,100)), lty=1, lwd=lwd,
       bty='n', col=COL, inset=c(0,-0.07))
if (SAVE_PLOTS) dev.off()


# Fig. 4a: Assumption I

dfs <- NULL

for (i in 1:4) {
  wmin <- i*100
  wmax <- (i+1)*100
  df <- read.table(sprintf('%s/fig4a/%d_%d.csv', DATA_DIR, wmin, wmax), header=TRUE)
  dfs[[i]] <- df
}

xlim <- range(do.call(rbind, dfs)[,'x'])
ylim <- c(0,3000) # range(do.call(rbind, dfs)[,'y'])
lwd <- 2

if (SAVE_PLOTS) pdf(sprintf('%s/fig4a.pdf', PLOT_DIR), width=1.7, height=1.4, pointsize=6,
                      family='Helvetica', useDingbats=FALSE)
par(mar=c(3.5, 3.5, 1.0, 0.5))
for (i in 1:4) {
  df <- dfs[[i]]
  wmin <- i*100
  wmax <- (i+1)*100
  if (i == 1) {
    plot(df$x, df$y, type='l', xlim=xlim, ylim=ylim, col=COL[i], bty='n', lwd=lwd, axes=FALSE,
         xlab='', ylab='')
  } else {
    lines(df$x, df$y, col=COL[i], lwd=lwd)
  }
}
axis(1); mtext('Week on Twitter', side=1, line=2.5)
axis(2); mtext('Median number of followers', side=2, line=2.5)
legend('bottomright', legend=sprintf('%d-%d weeks', seq(100,400,100), seq(200,500,100)), lty=1, lwd=lwd,
       bty='n', col=COL, inset=c(0,0))
if (SAVE_PLOTS) dev.off()


# Fig. 4b: Assumption II (power-law decay)

dfs_raw <- NULL
dfs_fit <- NULL

for (i in 1:4) {
  df_raw <- read.table(sprintf('%s/fig4b_raw/%d.txt', DATA_DIR, i), header=TRUE)
  df_fit <- read.table(sprintf('%s/fig4b_fit/%d.txt', DATA_DIR, i), header=TRUE)
  dfs_raw[[i]] <- df_raw
  dfs_fit[[i]] <- df_fit
}

xlim <- c(0,400) #range(do.call(rbind, dfs_raw)[,'x'])
ylim <- c(0.8,4) # range(do.call(rbind, dfs_raw)[,'y'])
lwd <- 2
pch <- 16
cex <- 0.7

if (SAVE_PLOTS) pdf(sprintf('%s/fig4b.pdf', PLOT_DIR), width=1.7, height=1.4, pointsize=6,
                      family='Helvetica', useDingbats=FALSE)
par(mar=c(3.5, 3.5, 1.0, 0.5))
for (i in 1:4) {
  df_raw <- dfs_raw[[i]]
  df_fit <- dfs_fit[[i]]
  if (i == 1) {
    plot(df_raw$x, df_raw$y, xlim=xlim, ylim=ylim, col=COL_LIGHT[i+4], bty='n', axes=FALSE,
         xlab='', ylab='', pch=pch, cex=cex, log='')
  } else {
    points(df_raw$x, df_raw$y, col=COL_LIGHT[i+4], pch=pch, cex=cex)
  }
  lines(df_fit$x, df_fit$y, col=COL[i+4], lwd=lwd)
}
axis(1); mtext('Week on Twitter', side=1, line=2.5)
axis(2); mtext('Retweet count', side=2, line=2.5)
legend('topright', legend=sprintf('Followers of week %d', seq(0,240,80)), lty=1, lwd=lwd,
       bty='n', col=COL[5:8], inset=c(0,-0.07))
if (SAVE_PLOTS) dev.off()


# Fig. 5: Power-law parameters

df <- read.table(sprintf('%s/fig5.txt', DATA_DIR), header=TRUE)
colnames(df) <- c('x', 'alpha', 'c')

xlim <- c(0,250)
col_alpha <- COL[5]
col_c <- COL[7]

if (SAVE_PLOTS) pdf(sprintf('%s/fig5.pdf', PLOT_DIR), width=2.7, height=1.4, pointsize=6, family='Helvetica', useDingbats=FALSE)
par(mar=c(3.5, 3.5, 1.0, 3.5))
# alpha
plot(df$x, df$c, type='l', xlim=xlim, ylim=c(0,2), col=col_c, bty='n', lwd=2, axes=FALSE, xlab='', ylab='')
axis(1)
mtext('Week on Twitter', side=1, line=2.5)
axis(4, col=col_c, col.axis=col_c)
mtext(expression(italic(c)), side=4, line=2.5, col=col_c)
# c
par(new=TRUE)
plot(df$x, df$alpha, type='l', xlim=xlim, ylim=c(0,1), col=col_alpha, bty='n', lwd=2, axes=FALSE, xlab='', ylab='')
axis(2, col=col_alpha, col.axis=col_alpha)
mtext(expression(alpha), side=2, line=2.5, col=col_alpha)
if (SAVE_PLOTS) dev.off()
