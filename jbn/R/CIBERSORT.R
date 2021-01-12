
#' CIBERSORT R script v1.03
#' Note: Signature matrix construction is not currently available; use java version for full functionality.
#' Author: Aaron M. Newman, Stanford University (amnewman@stanford.edu)
#' License: http://cibersort.stanford.edu/CIBERSORT_License.txt
#' Core algorithm
#' X cell-specific gene expression
#' y mixed expression per sample
CoreAlg <- function(X, y){

    #try different values of nu
    svn_itor <- 3

    res <- function(i){
        if(i==1){nus <- 0.25}
        if(i==2){nus <- 0.5}
        if(i==3){nus <- 0.75}
        model<-svm(X,y,type="nu-regression",kernel="linear",nu=nus,scale=F)
        model
    }

    if(Sys.info()['sysname'] == 'Windows') out <- mclapply(1:svn_itor, res, mc.cores=1) else
        out <- mclapply(1:svn_itor, res, mc.cores=svn_itor)

    nusvm <- rep(0,svn_itor)
    corrv <- rep(0,svn_itor)

    #do cibersort
    t <- 1
    while(t <= svn_itor) {
        weights = t(out[[t]]$coefs) %*% out[[t]]$SV
        weights[which(weights<0)]<-0
        w<-weights/sum(weights)
        u <- sweep(X,MARGIN=2,w,'*')
        k <- apply(u, 1, sum)
        nusvm[t] <- sqrt((mean((k - y)^2)))
        corrv[t] <- cor(k, y)
        t <- t + 1
    }

    #pick best model
    rmses <- nusvm
    mn <- which.min(rmses)
    model <- out[[mn]]

    #get and normalize coefficients
    q <- t(model$coefs) %*% model$SV
    q[which(q<0)]<-0
    w <- (q/sum(q))

    mix_rmse <- rmses[mn]
    mix_r <- corrv[mn]

    newList <- list("w" = w, "mix_rmse" = mix_rmse, "mix_r" = mix_r)

}

#' do permutations
#' perm Number of permutations
#' X cell-specific gene expression
#' y mixed expression per sample
doPerm <- function(perm, X, Y){
    Ylist <- as.list(data.matrix(Y))
    dist <- matrix()
    tpb <- txtProgressBar(min = 0,max = perm,initial = 0,width = 30,style = 3)
    for (itor in 1:perm){
        setTxtProgressBar(pb = tpb,value = itor)

        #random mixture
        yr <- as.numeric(Ylist[sample(length(Ylist),dim(X)[1])])

        #standardize mixture
        yr <- (yr - mean(yr)) / sd(yr)

        #run CIBERSORT core algorithm
        result <- CoreAlg(X, yr)

        mix_r <- result$mix_r

        #store correlation
        if(itor == 1) {dist <- mix_r}
        else {dist <- rbind(dist, mix_r)}
    }
    newList <- list("dist" = dist)
    newList
}

#' Main functions
#' @param exprs heterogenous mixed expression
#' @param perm Number of permutations, default is 100
#' @param QN Perform quantile normalization or not (TRUE/FALSE)
#' @export
CIBERSORT <- function(exprs, perm=100, QN=TRUE){

    X <- data.matrix(sig_matrix)
    Y <- data.matrix(exprs)

    #order
    X <- X[order(rownames(X)),]
    Y <- Y[order(rownames(Y)),]

    P <- perm #number of permutations

    #anti-log if max < 50 in mixture file
    if(max(Y) < 50) {Y <- 2^Y}

    #quantile normalization of mixture file
    if(QN == TRUE){
        message('quantile normalization of mixture file')
        tmpc <- colnames(Y)
        tmpr <- rownames(Y)
        Y <- normalize.quantiles(Y)
        colnames(Y) <- tmpc
        rownames(Y) <- tmpr
    }

    #intersect genes
    message('intersect genes')
    Xgns <- row.names(X)
    Ygns <- row.names(Y)
    YintX <- Ygns %in% Xgns
    Y <- Y[YintX,]
    XintY <- Xgns %in% row.names(Y)
    X <- X[XintY,]

    #standardize sig matrix
    message('standardize sig matrix')
    X <- (X - mean(X)) / sd(as.vector(X))

    #empirical null distribution of correlation coefficients
    message('permutation')
    if(P > 0) {nulldist <- sort(doPerm(P, X, Y)$dist)}

    #print(nulldist)

    header <- c('Mixture',colnames(X),"P-value","Correlation","RMSE")
    #print(header)

    output <- matrix()
    mixtures <- dim(Y)[2]
    pval <- 9999

    #iterate through mixtures
    cat('\n')
    message('iteration')
    tpb <- txtProgressBar(min = 0,max = mixtures,initial = 0,width = 30,style = 3)
    for (itor in 1:mixtures){
        setTxtProgressBar(pb = tpb,value = itor)
        y <- Y[,itor]

        #standardize mixture
        y <- (y - mean(y)) / sd(y)

        #run SVR core algorithm
        result <- CoreAlg(X, y)

        #get results
        w <- result$w
        mix_r <- result$mix_r
        mix_rmse <- result$mix_rmse

        #calculate p-value
        if(P > 0) {pval <- 1 - (which.min(abs(nulldist - mix_r)) / length(nulldist))}

        #print output
        out <- c(colnames(Y)[itor],w,pval,mix_r,mix_rmse)
        if(itor == 1) {output <- out}
        else {output <- rbind(output, out)}

    }
    #return matrix object containing all results
    obj <- rbind(header,output)
    obj <- obj[,-1]
    obj <- obj[-1,]
    obj <- matrix(as.numeric(unlist(obj)),nrow=nrow(obj))
    rownames(obj) <- colnames(Y)
    colnames(obj) <- c(colnames(X),"P-value","Correlation","RMSE")
    obj
}
