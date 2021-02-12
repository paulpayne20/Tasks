
R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
Copyright (C) 2020 The R Foundation for Statistical Computing
Platform: x86_64-apple-darwin17.0 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[R.app GUI 1.73 (7892) x86_64-apple-darwin17.0]

[History restored from /Users/paul_payne20/.Rapp.history]

2021-02-11 22:33:58.234 R[3530:128820] Warning: Expected min height of view: (<NSPopoverTouchBarItemButton: 0x7fa2a9ece570>) to be less than or equal to 30 but got a height of 32.000000. This error will be logged once per view in violation.
2021-02-11 22:34:09.652 R[3530:128820] Warning: Expected min height of view: (<NSPopoverTouchBarItemButton: 0x7fa2a9efdcd0>) to be less than or equal to 30 but got a height of 32.000000. This error will be logged once per view in violation.
2021-02-11 22:34:09.653 R[3530:128820] Warning: Expected min height of view: (<NSButton: 0x7fa2affa5c10>) to be less than or equal to 30 but got a height of 32.000000. This error will be logged once per view in violation.
2021-02-11 22:34:09.655 R[3530:128820] Warning: Expected min height of view: (<NSPopoverTouchBarItemButton: 0x7fa2affa6020>) to be less than or equal to 30 but got a height of 32.000000. This error will be logged once per view in violation.
2021-02-11 22:34:09.657 R[3530:128820] Warning: Expected min height of view: (<NSPopoverTouchBarItemButton: 0x7fa2affa6c40>) to be less than or equal to 30 but got a height of 32.000000. This error will be logged once per view in violation.
2021-02-11 22:34:12.395 R[3530:128820] +[NSSavePanel _warmUp] attempted warmup
> trueMean1 <- 5
> trueSD1 <- 5
> population1 <- rnorm(1e6, trueMean1, trueSD1)
> trueMean2 <- 4
> trueSD2 <- 5
> population2 <- rnorm(1e6, trueMean2, trueSD2)
> Size <- 50
> Sample1 <- sample(population1, Size)
> Sample2 <- sample(population2, Size)
> bocplot(Sample1, Sample2)
Error in bocplot(Sample1, Sample2) : could not find function "bocplot"
> boxplot(Sample1, Sample2)
> #Yes, according the graph that was made the samples were different and the populations were different too.
> source("http://jonsmitchell.com/code/simFxn04.R")
> MatGrandma <- makeFounder("grandma_mom")
> MatGrandpa <- makeFounder("grandpa_mom")
> PatGrandma <- makeFounder("grandma_da")
> PatGrandpa <- makeFounder("grandpa_da")
> Alan <- makeBaby(PatGrandma, PatGrandpa)
> Brenda <- makeBaby(PatGrandma, PatGrandpa)
> Focus <- makeBaby(Brenda, Alan)
> ToMom <- length( grep("mom", Focus ) ) / length( Focus )
> ToMomMom <- length( grep( "grandma_mom", Focus ) ) / length( Focus )
> ToMomDad <- length( grep("grandpa_mom", Focus ) ) / length( Focus )
> Sibling_01 <- makeBaby(Brenda, Alan)
> ToSib <- length( intersect( Focus, Sibling_01 ) ) / length( Focus )
> ManySiblings <- replicate( 1e3, length( intersect ( Focus, makeBaby(Brenda, ALan) ) ) / length( Focus ) )
Error in nrow(individual) : object 'ALan' not found
> ManySiblings <- replicate( 1e3, length( intersect( Focus, makeBaby(Brenda, Alan) ) ) / length( Focus ) )
> quantile(ManySiblings)
      0%      25%      50%      75%     100% 
0.293100 0.501775 0.586975 0.671475 0.784800 
> mean(ManySiblings)
[1] 0.5837604
> plot(density(ManySiblings), main="", xlab=" proportion shared genes")
> HWE <- fucntion(p) {
Error: unexpected '{' in "HWE <- fucntion(p) {"
> HWE<- fucntion(p) {
Error: unexpected '{' in "HWE<- fucntion(p) {"
> HWE<- fucntion(p) {
Error: unexpected '{' in "HWE<- fucntion(p) {"
> 	aa <- p^2
Error: object 'p' not found
> 	ab <- 2 * p * (1 - p)
Error: object 'p' not found
> 	bb <- (1 - p)^2
Error: object 'p' not found
> 	return(c(aa=aa, ab=ab, bb=bb))
Error: object 'aa' not found
> }
Error: unexpected '}' in "}"
> HWE(0.5)
Error in HWE(0.5) : could not find function "HWE"
> HWE<- fucntion(p) {
Error: unexpected '{' in "HWE<- fucntion(p) {"
> 	aa <- p^2
Error: object 'p' not found
> 	ab <- 2 * p * (1 - p)
Error: object 'p' not found
> 	bb <- (1 - p)^2
Error: object 'p' not found
> 	return(c(aa=aa, ab=ab, bb=bb))
Error: object 'aa' not found
> 
> HWE<- fucntion(p) {
Error: unexpected '{' in "HWE<- fucntion(p) {"
> HWE <- Function(p) {
Error: unexpected '{' in "HWE <- Function(p) {"
> 	aa <- p^2
Error: object 'p' not found
> 	ab <- 2*p*(1-p)
Error: object 'p' not found
> 	bb <- (1-p)^2
Error: object 'p' not found
> 	return(c(aa=aa, ab=ab, bb=bb))
Error: object 'aa' not found
> }
Error: unexpected '}' in "}"
> HWE(0.5)
Error in HWE(0.5) : could not find function "HWE"
> # aa=0.25 ab=0.50 bb=0.25
> HWE <- Function(p) {
Error: unexpected '{' in "HWE <- Function(p) {"
> 	aa <- p^2
Error: object 'p' not found
> 	ab <- 2*p*(1-p)
Error: object 'p' not found
> 	bb <- (1-p)^2
Error: object 'p' not found
> 	return(c(aa=aa, ab=ab, bb=bb))
Error: object 'aa' not found
> }
Error: unexpected '}' in "}"
> HWE(0.5)
Error in HWE(0.5) : could not find function "HWE"
> HWE <- Function(p) {
Error: unexpected '{' in "HWE <- Function(p) {"
> HWE <- Function(p) {
Error: unexpected '{' in "HWE <- Function(p) {"
> HWE <- Function(p) {
Error: unexpected '{' in "HWE <- Function(p) {"
> plot(1, 1, type="n", xlim=c(0, 1), ylim=c(0, 1), xlab="freq. allele a", ylab="geno. freq")
> p <- seq(from = 0, to = 1, by = 0.01)
> GenoFreq <- t(sapply(p, HWE))
Error in match.fun(FUN) : object 'HWE' not found
> lines(p, GenoFreq[,"aa"], lwd=2, col="red")
Error in xy.coords(x, y) : object 'GenoFreq' not found
> lines(p, GenoFreq[,"ab"], lwd=2, col="purple")
Error in xy.coords(x, y) : object 'GenoFreq' not found
> lines(p, GenoFreq[,"bb"], lwd=2, col="bule")
Error in xy.coords(x, y) : object 'GenoFreq' not found
> legend("top", legend=c("aa", "ab", "bb"), col=c("red", "purple", "blue"), lty=1, lwd=2, bty="n")
> Pop <- simPop(500)
> points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500, pch=21, bg="red")
> Pop <- simPop(50)
> points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/50, pch=22, bg="red")
> #The frequency has increased due to our population being smaller. B is smaller in a smaller population size. 
> install.packages('learnPopGen')
--- Please select a CRAN mirror for use in this session ---
also installing the dependencies ‘Rcpp’, ‘tmvnsim’, ‘quadprog’, ‘igraph’, ‘fastmatch’, ‘ape’, ‘maps’, ‘clusterGeneration’, ‘coda’, ‘combinat’, ‘expm’, ‘mnormt’, ‘numDeriv’, ‘phangorn’, ‘plotrix’, ‘scatterplot3d’, ‘gtools’, ‘phytools’

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/Rcpp_1.0.6.tgz'
Content type 'application/x-gzip' length 3203922 bytes (3.1 MB)
==================================================
downloaded 3.1 MB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/tmvnsim_1.0-2.tgz'
Content type 'application/x-gzip' length 23111 bytes (22 KB)
==================================================
downloaded 22 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/quadprog_1.5-8.tgz'
Content type 'application/x-gzip' length 38458 bytes (37 KB)
==================================================
downloaded 37 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/igraph_1.2.6.tgz'
Content type 'application/x-gzip' length 8842303 bytes (8.4 MB)
==================================================
downloaded 8.4 MB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/fastmatch_1.1-0.tgz'
Content type 'application/x-gzip' length 49780 bytes (48 KB)
==================================================
downloaded 48 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/ape_5.4-1.tgz'
Content type 'application/x-gzip' length 2650273 bytes (2.5 MB)
==================================================
downloaded 2.5 MB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/maps_3.3.0.tgz'
Content type 'application/x-gzip' length 3687860 bytes (3.5 MB)
==================================================
downloaded 3.5 MB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/clusterGeneration_1.3.7.tgz'
Content type 'application/x-gzip' length 265848 bytes (259 KB)
==================================================
downloaded 259 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/coda_0.19-4.tgz'
Content type 'application/x-gzip' length 321930 bytes (314 KB)
==================================================
downloaded 314 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/combinat_0.0-8.tgz'
Content type 'application/x-gzip' length 40594 bytes (39 KB)
==================================================
downloaded 39 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/expm_0.999-6.tgz'
Content type 'application/x-gzip' length 238015 bytes (232 KB)
==================================================
downloaded 232 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/mnormt_2.0.2.tgz'
Content type 'application/x-gzip' length 189336 bytes (184 KB)
==================================================
downloaded 184 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/numDeriv_2016.8-1.1.tgz'
Content type 'application/x-gzip' length 113169 bytes (110 KB)
==================================================
downloaded 110 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/phangorn_2.5.5.tgz'
Content type 'application/x-gzip' length 2774993 bytes (2.6 MB)
==================================================
downloaded 2.6 MB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/plotrix_3.8-1.tgz'
Content type 'application/x-gzip' length 1139222 bytes (1.1 MB)
==================================================
downloaded 1.1 MB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/scatterplot3d_0.3-41.tgz'
Content type 'application/x-gzip' length 333688 bytes (325 KB)
==================================================
downloaded 325 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/gtools_3.8.2.tgz'
Content type 'application/x-gzip' length 324588 bytes (316 KB)
==================================================
downloaded 316 KB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/phytools_0.7-70.tgz'
Content type 'application/x-gzip' length 2137373 bytes (2.0 MB)
==================================================
downloaded 2.0 MB

trying URL 'https://mirror.las.iastate.edu/CRAN/bin/macosx/contrib/4.0/learnPopGen_1.0.4.tgz'
Content type 'application/x-gzip' length 175740 bytes (171 KB)
==================================================
downloaded 171 KB


The downloaded binary packages are in
	/var/folders/jr/s38lkn0s6yd5_jrrk8v9d2sh0000gn/T//RtmpcTeX44/downloaded_packages
> library('learnPopGen')
> #Ne is how many individuals are in each population
> #nrep is how many populations are simulating at once
> #pause is how fast the lines grow
> x <- genetic.drift(Ne=200, nrep=5, pause=0.01)
> Popsizes <- 5:50
> Samples <- rep(PopSizes, 5)
Error: object 'PopSizes' not found
> Samples <- rep(PopSizes, 5)
Error: object 'PopSizes' not found
> tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))
Error in lapply(X = X, FUN = FUN, ...) : object 'Samples' not found
> Line <- lm(tExt ~Samples)
Error in eval(predvars, data, env) : object 'tExt' not found
> summary(Line)
Error in summary(Line) : object 'Line' not found
> Line$coef
Error: object 'Line' not found
> plot(Samples, tExt)
Error in plot(Samples, tExt) : object 'Samples' not found
> abline(Line)
Error in abline(Line) : object 'Line' not found
> 