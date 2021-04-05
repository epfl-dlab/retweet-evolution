# Continuous time

alpha <- 1.1
t0 <- 1
tmax <- 10
t <- seq(0, tmax, tmax/100)
c <- (alpha-1) / (t0^-(alpha-1) + 1/(alpha-2)*((1+t0)^-(alpha-2) - t0^-(alpha-2)))

par(mfrow=c(2,2))

plot(t, c*(t+t0)^-alpha, col='white')
for (delta in seq(0, tmax, tmax/30)) {
  lines(t+delta, c*(t+t0)^-alpha, type='l', col=rgb(0,0,0,0.2), lwd=2)
}

y <- c/(alpha-1) * (t0^-(alpha-1) - (t+t0)^-(alpha-1))
plot(t, y, type='l')

# Upper bound of tweets per week:
c/(alpha-1) * t0^-(alpha-1)

z <- y/t
plot(t, z, type='l', ylim=c(0,max(z, na.rm=TRUE)))

i <- 1
S <- 0:4
for (s in S) {
  z <- y/(t+s)
  if (i == 1) plot(t, z, type='l', col=i, ylim=c(0,max(y/(t+S[2]))))
  else lines(t, z, col=i)
  i <- i + 1
}

# Sanity check to see that the volume of retweets in the first week is indeed 1:
# z <- c/(alpha-1) * (t0^-(alpha-1)*t + 1/(alpha-2)*((t+t0)^-(alpha-2) - t0^-(alpha-2)))
# plot(t, z, type='l', xlim=c(0,1), ylim=c(0,1), panel.first=abline(h=1,v=1))

###########################################################################

# Discrete time

PLOTDIR <- sprintf('%s/Desktop/qscore', Sys.getenv('HOME'))

# Source: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
COL_LIGHTBLUE <- "#56B4E9"
COL_YELLOW <- "#F0E442"
COL_DARKBLUE <- "#0072B2"
COL_RED <- "#D55E00"
COL_MAGENTA <- "#CC79A7"
COL_GRAY <- "#999999"
COL_ORANGE <- "#E69F00"
COL_GREEN <- "#009E73"
col <- c(COL_DARKBLUE, COL_LIGHTBLUE, COL_GREEN, COL_YELLOW, COL_ORANGE, COL_RED)

# alpha <- 1.1
alpha <- 0.8
c <- 1
t <- 1:20

f <- function(x) {
  c*x^-alpha
}

pdf(sprintf('%s/retweet_rates_f.pdf', PLOTDIR), width=1.7, height=1.4, pointsize=6, family='Helvetica', useDingbats=FALSE)
par(mar=c(3.2, 3.4, 0.8, 0.8))
plot(t, f(t), col='white', axes=FALSE, xlab='', ylab='', xlim=c(0,max(t)))
axis(1)
mtext('Time t', side=1, line=2)
axis(2)
mtext("Followers' retweet rates f(t)", side=2, line=2)
for (delta in 0:max(t)) {
  y <- f(t)
  y[y==0] <- NA
  lines(t+delta, y, type='l', col=rgb(0,0,0,0.2), lwd=2)
}
abline(v=3, col=COL_MAGENTA); points(rep(3,3), f(1:3), col=COL_MAGENTA, pch=20, cex=2)
abline(v=7, col=COL_GREEN); points(rep(7,7), f(1:7), col=COL_GREEN, pch=20, cex=2)
dev.off()

pdf(sprintf('%s/retweet_counts_F.pdf', PLOTDIR), width=1.7, height=1.4, pointsize=6, family='Helvetica', useDingbats=FALSE)
par(mar=c(3.2, 3.4, 0.8, 0.8))
y <- cumsum(f(t))
plot(t, y, type='l', axes=FALSE, xlab='', ylab='', xlim=c(0,max(t)), lwd=2)
points(3, y[3], col=COL_MAGENTA, pch=20, cex=2)
points(7, y[7], col=COL_GREEN, pch=20, cex=2)
axis(1)
mtext('Time t', side=1, line=2)
axis(2)
mtext('Retweet count F(t)', side=2, line=2)
dev.off()

pdf(sprintf('%s/norm_retweet_counts_G.pdf', PLOTDIR), width=1.7, height=1.4, pointsize=6, family='Helvetica', useDingbats=FALSE)
par(mar=c(3.2, 3.4, 0.8, 0.8))
z <- y/t
plot(t, z, type='l', axes=FALSE, xlab='', ylab='', xlim=c(0,max(t)), ylim=c(0,max(z)), lwd=2, col=col[1])
axis(1)
mtext('Time t', side=1, line=2)
axis(2)
mtext(expression(paste('Norm. retweet count ', G[s](t))), side=2, line=2)
legend('topright', bty='n', legend='s = 0', lty=1, lwd=2, col=col[1])
dev.off()

pdf(sprintf('%s/norm_retweet_counts_Gs.pdf', PLOTDIR), width=1.7, height=1.4, pointsize=6, family='Helvetica', useDingbats=FALSE)
par(mar=c(3.2, 3.4, 0.8, 0.8))
i <- 1
S <- c(0,4,6,8,10,12)
for (s in S) {
  z <- y/(t+s)
  if (i == 1) plot(t, z, type='l', col=col[i], axes=FALSE, xlab='', ylab='', xlim=c(0,max(t)), ylim=c(0,max(y/(t+S[2]))), lwd=2)
  else lines(t, z, col=col[i], lwd=2)
  i <- i + 1
}
axis(1)
mtext('Time t', side=1, line=2)
axis(2)
mtext(expression(paste('Norm. retweet count ', G[s](t))), side=2, line=2)
legend('bottomleft', bty='n', legend=paste('s =', S[1:3]), lty=1, lwd=2, col=col[1:3])
legend('bottomright', bty='n', legend=paste('s =', S[4:6]), lty=1, lwd=2, col=col[4:6])
dev.off()

###########################################################################

# In the paper, we assumed that the s initial followers don't retweet. Instead, we may
# also assume that they do retweet, at the same rate as those "regular" followers that
# started to follow the focal user in the first time step. The resulting curves are very
# similar to those that emerge when the s initial followers are silent, with the difference
# that the curves are prefixed with a sharp, brief dip at the very beginning.
# Note that the vector if time steps were adjusted here. This is just to zoom into the
#interesting regime; the same could be achieved by keeping t fixed and adjusting c.

t <- seq(0.01,5,0.01)
par(mar=c(3.2, 3.4, 0.8, 0.8))
i <- 1
S <- c(0,4,6,8,10,12)
for (s in S) {
  # This is the relevant change.
  yy <- cumsum(f(t)) + s*f(t)
  z <- yy/(t+s)
  if (i == 1) plot(t, z, type='l', col=col[i], axes=FALSE, xlab='', ylab='', xlim=c(0,max(t)), ylim=c(0,100), lwd=2)
  else lines(t, z, col=col[i], lwd=2)
  i <- i + 1
}
axis(1)
mtext('Time t', side=1, line=2)
axis(2)
mtext(expression(paste('Norm. retweet count ', G[s](t))), side=2, line=2)
legend('bottomleft', bty='n', legend=paste('s =', S[1:3]), lty=1, lwd=2, col=col[1:3])
legend('bottomright', bty='n', legend=paste('s =', S[4:6]), lty=1, lwd=2, col=col[4:6])
