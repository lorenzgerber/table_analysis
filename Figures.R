# This document describes the data processing and analysis for the mandatory assignment 'OU3'.
# First some names and definitions are set:
    
# setting the workdirectory
setwd('~/github/table_analysis/')

# names of the data types tested
datatype<-c('array', 'dlist', 'mtf')

# number of operations in each test case
datapoints<-c(250, 500, 1000, 2500, 5000, 7500, 10000)

# reading the raw data filenames into variables
array<-list.files(pattern = '^a.')
dlist<-list.files(pattern = '^d.')
mtf<-list.files(pattern = '^m.')

# creating result matrix
results<-matrix(105, 7, 15)

# value insertion sequence into results matrix
col_ind<-seq(1,15,3)

# row and column names for result tabme
colnames(results)<-rep(c('array', 'dlist', 'mtf'),5)
rownames(results)<-c(250, 500, 1000, 2500, 5000, 7500, 10000)

# names of conducted experiments
experiments<-c('Insertion', 'RandomExistingLookup', 
               'RandomNonExisitingLookup', 'SkewedLookup', 'Remove')

# Then the mean and sd for each datatype, datapoint and test is calculated:
# looping through datatypes
for (dt_i in 1:3 ){
    # looping through datapoints
    for ( exp_i in 1:7 ){
        # dynamically select file to process
        current<-eval(parse(text=paste(datatype[dt_i], '[',exp_i,']')))
        # load current file
        current_data<-read.table(file=current, sep='\t', dec='.')
        # calculate statistics and write into result table
        results[exp_i, col_ind+dt_i-1]<-apply(current_data,2, mean)
    }
}

means<-results

# Calculating Standard Deviations and Relative Standard Deviation (RSD)
# looping through datatypes
for (dt_i in 1:3 ){
    # looping through datapoints
    for ( exp_i in 1:7 ){
        # dynamically select file to process
        current<-eval(parse(text=paste(datatype[dt_i], '[',exp_i,']')))
        # load current file
        current_data<-read.table(file=current, sep='\t', dec='.')
        # calculate statistics and write into result table
        results[exp_i, col_ind+dt_i-1]<-apply(current_data,2, sd)
    }
}

sds<-results

# calculating relative standard deviation
rsds<-100/means*sds


setEPS()


# Plotting the 'insertion' experiment. Plot (a) shows the speed in microseconds and plot (b) the RSDs. 
postscript(file="fig1.eps", horizontal=F, height=5, width=10, onefile = F, paper = "special")
par(mfrow=c(1,2))
plot(datapoints, means[,2], col='black', type='b', ylim=c(0,100), ylab=substitute(paste(mu,'s')), cex.axis=0.6)
points(datapoints, means[,3], col='red', type='b', ylim=c(0,100))
points(datapoints, means[,1], col='blue', type='b', ylim=c(0,100))
legend(x=5000, y=100, datatype[c(2,3,1)], cex=0.8, col=c('black','red', 'blue'),
       pch=1, lty=1)
text(x=9800, y=100, labels = '(a)')

plot(datapoints, rsds[,2], col='black', type='b', ylim=c(0,250), ylab='% RSD', cex.axis=0.6)
points(datapoints, rsds[,3], col='red', type='b', ylim=c(0,250))
points(datapoints, rsds[,1], col='blue', type='b', ylim=c(0,250))
legend(x=5000, y=250, datatype[c(2,3,1)], cex=0.8, col=c('black','red', 'blue'),
       pch=1, lty=1)
text(x=9800, y=250, labels = '(b)')
dev.off()

# Plotting the 'RandomExistingLookup' experiment. Plot (a) shows the speed in microseconds and plot (b) the RSDs. 
postscript(file="fig2.eps", horizontal=F, height=5, width=10, onefile = F, paper = "special")
par(mfrow=c(1,2))
plot(datapoints, means[,5], col='black', type='b', ylim=c(0,2000), ylab=substitute(paste(mu,'s')), cex.axis=0.6)
points(datapoints, means[,6], col='red', type='b', ylim=c(0,2000))
points(datapoints, means[,4], col='blue', type='b', ylim=c(0,2000))
legend(x=0, y=2000, datatype[c(2,3,1)], cex=0.8, col=c('black','red', 'blue'),
       pch=1, lty=1)
text(x=9800, y=2000, labels = '(a)')

plot(datapoints, rsds[,5], col='black', type='b', ylim=c(0,100), ylab='% RSD', cex.axis=0.6)
points(datapoints, rsds[,6], col='red', type='b', ylim=c(0,100))
points(datapoints, rsds[,4], col='blue', type='b', ylim=c(0,100))
legend(x=5000, y=100, datatype[c(2,3,1)], cex=0.8, col=c('black','red', 'blue'),
       pch=1, lty=1)
text(x=9800, y=100, labels = '(b)')
dev.off()

# Plotting the 'RandomNonExistingLookup' experiment. Plot (a) shows the speed in microseconds and plot (b) the RSDs. 
postscript(file="fig3.eps", horizontal=F, height=5, width=10, onefile = F, paper = "special")
par(mfrow=c(1,2))
plot(datapoints, means[,8], col='black', type='b', ylim=c(0,4000), ylab=substitute(paste(mu,'s')), cex.axis=0.6)
points(datapoints, means[,9], col='red', type='b', ylim=c(0,4000))
points(datapoints, means[,7], col='blue', type='b', ylim=c(0,4000))
legend(x=0, y=4000, datatype[c(2,3,1)], cex=0.8, col=c('black','red', 'blue'),
       pch=1, lty=1)
text(x=9800, y=4000, labels = '(a)')

plot(datapoints, rsds[,8], col='black', type='b', ylim=c(0,50), ylab='% RSD', cex.axis=0.6)
points(datapoints, rsds[,9], col='red', type='b', ylim=c(0,50))
points(datapoints, rsds[,7], col='blue', type='b', ylim=c(0,50))
legend(x=5000, y=50, datatype[c(2,3,1)], cex=0.8, col=c('black','red', 'blue'),
       pch=1, lty=1)
text(x=9800, y=50, labels = '(b)')
dev.off()



# Plotting the 'SkewedLookup' experiment. Plot (a) shows the speed in microseconds and plot (b) the RSDs. 
postscript(file="fig4.eps", horizontal=F, height=5, width=10, onefile = F, paper = "special")
par(mfrow=c(1,2))
plot(datapoints, means[,11], col='black', type='b', ylim=c(0,2000), ylab=substitute(paste(mu,'s')), cex.axis=0.6)
points(datapoints, means[,12], col='red', type='b', ylim=c(0,2000))
points(datapoints, means[,10], col='blue', type='b', ylim=c(0,2000))
legend(x=0, y=2000, datatype[c(2,3,1)], cex=0.8, col=c('black','red', 'blue'),
       pch=1, lty=1)
text(x=9800, y=2000, labels = '(a)')

plot(datapoints, rsds[,11], col='black', type='b', ylim=c(0,50), ylab='% RSD', cex.axis=0.6)
points(datapoints, rsds[,12], col='red', type='b', ylim=c(0,50))
points(datapoints, rsds[,10], col='blue', type='b', ylim=c(0,50))
legend(x=5000, y=50, datatype[c(2,3,1)], cex=0.8, col=c('black','red', 'blue'),
       pch=1, lty=1)
text(x=9800, y=50, labels = '(b)')
dev.off()



# Plotting the 'remove' experiment. Plot (a) shows the speed in microseconds and plot (b) the RSDs. 
postscript(file="fig5.eps", horizontal=F, height=5, width=10, onefile = F, paper = "special")
par(mfrow=c(1,2))
plot(datapoints, means[,14], col='black', type='b', ylim=c(0,1000), ylab=substitute(paste(mu,'s')), cex.axis=0.6)
points(datapoints, means[,15], col='red', type='b', ylim=c(0,1000))
points(datapoints, means[,13], col='blue', type='b', ylim=c(0,1000))
legend(x=0, y=1000, datatype[c(2,3,1)], cex=0.8, col=c('black','red', 'blue'),
       pch=1, lty=1)
text(x=9800, y=1000, labels = '(a)')

plot(datapoints, rsds[,14], col='black', type='b', ylim=c(0,50), ylab='% RSD', cex.axis=0.6)
points(datapoints, rsds[,15], col='red', type='b', ylim=c(0,50))
points(datapoints, rsds[,13], col='blue', type='b', ylim=c(0,50))
legend(x=5000, y=50, datatype[c(2,3,1)], cex=0.8, col=c('black','red', 'blue'),
       pch=1, lty=1)
text(x=9800, y=50, labels = '(b)')
dev.off()


# Plotting the several experiments of dlist datatype. Plot (a) shows the speed in microseconds and plot (b) the RSDs. 
postscript(file="fig6.eps", horizontal=F, height=5, width=10, onefile = F, paper = "special")
par(mfrow=c(1,2))
plot(datapoints, means[,5], col='black', type='b', ylim=c(0,3000), ylab=substitute(paste(mu,'s')), cex.axis=0.6)
points(datapoints, means[,8], col='red', type='b', ylim=c(0,3000))
points(datapoints, means[,11], col='blue', type='b', ylim=c(0,3000))
legend(x=0, y=3000, experiments[c(2,3,4)], cex=0.5, col=c('black','red', 'blue'),
       pch=1, lty=1)
text(x=9800, y=3000, labels = '(a)')

plot(datapoints, rsds[,5], col='black', type='b', ylim=c(0,50), ylab='% RSD', cex.axis=0.6)
points(datapoints, rsds[,8], col='red', type='b', ylim=c(0,50))
points(datapoints, rsds[,11], col='blue', type='b', ylim=c(0,50))
legend(x=1000, y=50, experiments[c(2,3,4)], cex=0.5, col=c('black','red', 'blue'),
       pch=1, lty=1)
text(x=9800, y=50, labels = '(b)')
dev.off()



# Plotting the several experiments of mtf datatype. Plot (a) shows the speed in microseconds and plot (b) the RSDs. 
postscript(file="fig7.eps", horizontal=F, height=5, width=10, onefile = F, paper = "special")
par(mfrow=c(1,2))
plot(datapoints, means[,6], col='black', type='b', ylim=c(0,3000), ylab=substitute(paste(mu,'s')), cex.axis=0.6)
points(datapoints, means[,9], col='red', type='b', ylim=c(0,3000))
points(datapoints, means[,12], col='blue', type='b', ylim=c(0,3000))
legend(x=0, y=3000, experiments[c(2,3,4)], cex=0.5, col=c('black','red', 'blue'),
       pch=1, lty=1)
text(x=9800, y=3000, labels = '(a)')

plot(datapoints, rsds[,6], col='black', type='b', ylim=c(0,50), ylab='% RSD', cex.axis=0.6)
points(datapoints, rsds[,9], col='red', type='b', ylim=c(0,50))
points(datapoints, rsds[,12], col='blue', type='b', ylim=c(0,50))
legend(x=1000, y=50, experiments[c(2,3,4)], cex=0.5, col=c('black','red', 'blue'),
       pch=1, lty=1)
text(x=9800, y=50, labels = '(b)')
dev.off()

# Plotting the several experiments of array datatype. Plot (a) shows the speed in microseconds and plot (b) the RSDs. 
postscript(file="fig8.eps", horizontal=F, height=5, width=10, onefile = F, paper = "special")
par(mfrow=c(1,2))
plot(datapoints, means[,4], col='black', type='b', ylim=c(0,6000), ylab=substitute(paste(mu,'s')), cex.axis=0.6)
points(datapoints, means[,7], col='red', type='b', ylim=c(0,6000))
points(datapoints, means[,10], col='blue', type='b', ylim=c(0,6000))
legend(x=0, y=6000, experiments[c(2,3,4)], cex=0.5, col=c('black','red', 'blue'),
       pch=1, lty=1)
text(x=9800, y=6000, labels = '(a)')

plot(datapoints, rsds[,4], col='black', type='b', ylim=c(0,50), ylab='% RSD', cex.axis=0.6)
points(datapoints, rsds[,7], col='red', type='b', ylim=c(0,50))
points(datapoints, rsds[,10], col='blue', type='b', ylim=c(0,50))
legend(x=1000, y=50, experiments[c(2,3,4)], cex=0.5, col=c('black','red', 'blue'),
       pch=1, lty=1)
text(x=9800, y=50, labels = '(b)')
dev.off()

