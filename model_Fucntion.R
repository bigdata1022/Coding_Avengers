#SPLS 모형 시작 
spls_vm_model <- function(ref_data, new_data, y_names, para_names){
  cv <- cv.spls(as.matrix(ref_data[,para_names]), ref_data[,y_names], eta=seq(0.1,0.9,0.1), K=c(3:3,length(para_names)-2), plot.it=F)
  spls_vm_model_result <- spls(as.matrix(ref_data[,para_names]), ref_data[,y_names], eta=cv$eta.opt, K=cv$K.opt)
  spls_result <- predict(spls_vm_model_result, as.matrix(new_data[,para_names]), type="fit")
  return(spls_result)
}
#SPLS 모형 끝

#MLR작 모형 시작
mlr_vm_model <- function(ref_data, new_data, y_names, para_names){
  mlr_vm_model_result <- step(lm(as.formula(paste(y_names, paste(para_names, collapse=" + "), sep=" ~ ")), data=ref_data), trace=0)
  mlr_result <- predict(mlr_vm_model_result, new_data[,para_names])#예측값
  return(mlr_result)
}
#MLR 모형 끝

#PCR 모형 시작
pcr_vm_model <- function(ref_data, new_data, y_name, para_names){
  #y_nmae <- y_names
  pcr_tuning_result <- data.frame(ncomp=NA) #모형 파라미터 저장 변수 지정
  #pcr_response <- ref_data[,y_names] #y값 저장(Reference data의 CTQ)
  #pcr_term <- as.mastrix(subset(ref_data, select=para_names))
  pcr_tmp <- pcr(as.formula(paaste(y_name, paste(para_names, collapse=" + "), sep=" ~ ")), ncomp=lengt(para_names), data=ref_data)
  pcr_tuning_result$ncomp <- which.min(RMSEP(crossval(pcr_tmp, segments=10, segment.type="consecutive"))[[1]][,1,][1,])-1
  #10-fold Cross-Validation으로 componet갯수 설정
  if(pcr_tuning_result$ncmop>0){
    #모형 설정 : 최적 component 갯수 적용
    pcr_vm_model_result <- pcr(as.formula(paste(y_name, paste(para_names, collapse=" + "), sep=" ~ ")), data=ref_data, ncomp=pcr_tuning_result$ncomp) #PCR 모형 적용
    pcr_result <- predict(pcr_vm_model_result, as.matrix(new_data[,para_names]), ncomp=pcr_tuning_result$ncomp) #예측값
  } else{pcr_result <- mean(ref_data[,y_name])}
  return(pcr_result)
}
#PCR 모형 끝

#PLS 모형 시작
pls_vm_model <- function(ref_data, new_data, y_name, para_names){
  #y_name <- y_names
  pls_tuning_result <- data.frame(ncomp=NA) #모형 파라미터 저장 변수 지정
  #pls_response <- ref_data[,y_names] #y값 저장(Reference data의 CTQ)
  #pls_term <- as.matrix(subset(ref_data, select=para_names)) #x값 저장(Reference data의 Para. 통계량)
  #최적 모형 파라미터 설정 : component 갯수 설정
  pls_tmp <- plsr(as.formula(paste(y_name, paste(para_names, collapse = " + "), sep = " ~ ")), ncomp=length(para_names), method="simpls", data=ref_data) #전체 component에 대한 PCR모형 적용
  pls_tuning_result$ncomp <- which.min(RMSEP(crossval(pls_tmp, segments=10, segment.type="consecutive"))[[1]][,1,][1,])-1 #10-fold Cross-Validation으로 component 갯수 설정
  #pls_result <- 0
  if(pls_tuning_result$ncomp>0){
    #모형 설정 : 최적 component 갯수 적용
    pls_vm_model_result <- plsr(as.formula(paste(y_name, paste(para_names, collapse = " + "), sep = " ~ ")), data=ref_data, ncomp=pls_tuning_result$ncomp, method="simpls") #PLS 모형 저장
    pls_result <- predict(pls_vm_model_result, as.matrix(new_data[,para_names]), ncomp=pls_tuning_result$ncomp) #예측값
  } else {pls_result <- mean(ref_data[,y_name])}
  return(pls_result)
}

#Ridge 모형 시작
ridge_vm_model <- function(ref_data, new_data, y_names, para_names){
  rid_nfold <- 10
  rid_tuning_result <- data.frame(alpha=NA, lambda.min=NA) #setting값 설정
  rid_tuning_result$alpha <- 0
  rid_tuning_result$lambda.min <- cv.glmnet(y=ref_data[,y_names], x=as.matrix(ref_data[,para_names]), alpha=rid_tuning_result$alpha, nfolds=rid_nfold)$labmda.min
  #최적의 모형 설정
  rid_vm_model_result <- glmnet(y-ref_data[,y_names], x=as.matrix(ref_data[,para_names]), alpha=rid_tuning_result$alpha, lambda=rid_tuning_result$lambda.min) #모형 적용
  rid_result <- predict(rid_vm_model_result, newx=as.matrix(new_data[,para_names])) #예측값
  return(rid_result)
}
#Ridge 모형 끝

#LASSO 모형 시작
lasso_vm_model <- function(ref_data, new_data, y_names, para_names){
  las_nfold1 <- 10
  las_tuning_result <- data.frame(alpha=NA, lambda.min=NA) #setting 값 설정
  las_tuning_result$alpha <- 1
  las_tuning_reuslt$lambda.min <- cv.glmnet(y=ref_data[,y_names], x=as.matrix(ref_data[,para_names]), alpha=las_tuning_result$alpha, nfolds=las_nfold1)$lambda.min #10-fold Cross-Validation으로 설정
  #최적의 모형 설정
  las_vm_model_result <- glmnet(y=ref_data[,y_names], x=as.matrix(ref_data[,para_names]), alpha=las_tuning_result$alpha, lambda=las_tuning_result$lambda.min) #모형 적용
  las_result <- predict(las_vm_model_result, newx=as.matrix(new_data[,para_names])) #예측값
  return(las_result)
}
#LASSO 모형 끝

#Elastic Net 모형 시작
elastic_vm_model <- function(ref_data, new_data, y_names, para_names){
  #Elastic Net 최적 모형 파라미터 설정 : 0 <= alpha <= 1
  #ela_min_error를 최소로 하는 lambda.min 값을 탐색
  ela_nfold <- 10
  ela_tuning_result <- data.frame(alpha=NA, lambda.min = NA) #setting값 설정
  for(j in 0:10)
  {if (j == 0){
    ela_tmp <- cv.glmnet(y=ref_data[,y_names], x=as.matrix(ref_data[, para_names]), alpha = j/10, nfold=ela_nfold)
    ela_tuning_result$lambda.min <- ela_tmp$lambda.min
    ela_min_error <- min(ela_tmp$cvm)
    ela_tuning_result$alpha <- j/10}
    else {ela_tmp <- cv.glmnet(y=ref_data[,y_names], x=as.matrix(ref_data[,para_names]), alpha=j/10, nfolds=ela_nfold)
    if(min(ela_tmp$cvm) < ela_min_error){
      ela_tuning_result$lambda.min <- ela_tmp$lambda.min
      ela_min_error <- min(ela_tmp$cvm)
      ela_tuning_result$alpha <- j/10}}}
  #최적의 모형 설정
  ela_vm_model_result <- glmnet(y=ref_data[,y_names], x=as.matrix(ref_data[,para_names]), alpha=ela_tuning_result$alpha, lambda=ela_tuning_result$lambda.min)#모형 적용
  ela_result <- predict(ela_vm_model_result, newx=as.matrix(new_data[,para_names])) #예측값
  return(ela_result)
}
#Elastic Net 모형 끝

#NN 모형 시작
nn_vm_model <- function(ref_data, new_data, y_names, para_names){
  nn_nfold <- 10
  nn_tuning_result <- data.frame(decay_val = NA)
  nn_tmp <- step(lm(as.formula(paste(y_names, paste(para_names, collapse = " + "), sep = " ~ ")), data=ref_data), trace=0) #단계별 변수 선택
  nn_para_names_nnet <- row.names(summary(nn_tmp)[[4]])[-1] #선택된 변수의 index
  nn_random_index <- 1:dim(ref_data)[1] #전체 데이터 수
  nn_cv_data_num <- as.integer(dim(ref_data)[1]/nn_nfold) #CV 수
  nn_pred_val <- nn_real_val <- ref_data[,y_names] #실제값
  #최적 모형 파라미터 설정 : decay(parameter for weight decay) 설정
  #RSS값이 가장 적은 decay값을 선택
  for(k in seq(0,1,length.out=11))
  {for(j in 1:nn_nfold){
    if(j != nn_nfold){nn_cv_test_index <- nn_random_index[((j-1)*nn_cv_data_num + 1):(j*nn_cv_data_num)]}
    else {nn_cv_test_index <- nn_random_index[((j-1)*nn_cv_data_num + 1):dim(ref_data)[1]]}
    nn_vm_model_result <- nnet(as.formula(paste(y_names, paste(nn_para_names_nnet, collapse = " + "), sep = " ~ ")), data=ref_data[-nn_cv_test_index,], decay=as.double(k), size=5, maxit = 100000, trace = F.linout=T, abstol = 1.0e-12, reltol=1.0e-24)
    nn_pred_val[nn_cv_test_index] <- predict(nn_vm_model_result, as.matrix(ref_data[nn_cv_test_index, nn_para_names_nnet])) #예측값
  }
    nn_RSS <- mean((nn_pred_val - nn_real_val)^2)
    if(k==0){nn_MIN_RSS <- nn_RSS
    nn_tuning_result$decay_val <- k}
    else if(nn_RSS < nn_MIN_RSS){
      nn_MIN_RSS <- nn_RSS
      nn_tuning_result$decay_val <- k}}
  #모형 설정 : decay(parameter for weight decay) 적용
  nn_vm_model_result <- nnet(as.formula(paste(y_names, paste(nn_para_names_nnet, collapse = " + "), sep = " ~ ")), #neuralnetwork 모형 적용
                             data=ref_data, decay=as.double(nn_tuning_result$decay_val), size=5, maxit = 100000, trace=F, linout=T, abstol=1.0e-12, reltol=1.0e-24)
  nn_result <- predict(nn_vm_model_result, as.matrix(new_data[,nn_para_names_nnet])) #예측값
  return(nn_result)
}
#NN 모형 끝

#SVM 모형 시작
svm_vm_model <- function(ref_data, new_data, y_names, para_names){
  svm_nfold <- 10
  svm_tuning_result <- data.frame(C=NA, epsilon=NA) #모형 파라미터 저장 변수 지정
  svm_random_index <- 1:dim(ref_data)[1] #전체 데이터 수
  svm_cv_data_num <- as.integer(dim(ref_data)[1]/svm_nfold) #CV 수
  svm_pred_val <- svm_real_val <- ref_data[,y_names] #실제값
  #최적 모형 파라미터 설정 : c, epsilon 설정
  #RSS값이 가장 적은 c, epsilon 값을 선택
  for(C_val in (1:5)*5)
  {
    for(epsilon_val in (1:5)/20){
      svm_Cross_ERR <- cross(ksvm(as.formula(paste(y_names, paste(para_names, collapse = " + "), sep = " ~ ")), #CV
                                  data=ref_data, type='eps-svr', kernel='vanilladot', C=C_val,epsilon=epsilon_val, cross=10))
      if(C_val == 5 & epsilon_val == 1/20)
      {
        svm_MIN_ERR <- svm_Cross_ERR
        svm_tuning_result$C <- C_val
        svm_tuning_result$epsilon <- epsilon_val
      }
      else if(svm_Cross_ERR < svm_MIN_ERR){
        svm_MIN_ERR <- svm_Cross_ERR
        svm_tuning_result$C <- C_val
        svm_tuning_result$epsilon <- epsilon_val}}}
  #모형 설정 : C, epsilon 적용
  svm_vm_model_result <- ksvm(as.formula(paste(y_names, paste(para_names, collapse = " + "), sep = " ~ ")), #SVM모형 적용
                              data=ref_data, type='eps-svr', kernel='vanilladot', C=svm_tuning_result$C, epsilon=svm_tuning_result$epsilon)
  svm_result <- predict(svm_vm_model_result, as.matrix(new_data[,para_names])) #예측값
  return(svm_result)
}
#SVM 모형 끝

#Kernel PCR 모형 시작
kernel_vm_model <- function(ref_data, new_data, y_names, para_names){
  ker_nfold <- 10 #Cross_Validation을 위한 fold 갯수 설정
  ker_tuning_result <- data.frame(sigma=NA, feature=NA) #모형 파라미터 저장 변수 지정
  ker_sigma <- c(seq(0.00001, 0.001, by=0.00001))# 모형 파라미터 튜닝값 지정
  ker_rmsep <- function(ker_pred_val=NULL, ker_real_val=NULL) #모형 파리미터 평가 함수(RMSEP)
  {n <- length(ker_pred_val)
  RMSEP <- sqrt((sum(ker_pred_val-ker_real_val)^2)/n)
  return(RMSEP)}
  ker_response <- ker_real_val <- ref_data[,y_names]
  ker_tun1 <- ker_tun2 <- 0
  #ker_data_tun <- 0
  #최적 모형 파라미터 설정 : Gaussian 커널함수 sigma 값, component 갯수 설정
  for(j in 1:length(ker_sigma))
  {
    ker_tmp <- kpca(~., data=ref_data[,para_names], kernel="rbfdot", kpar=list(sigma=ker_sigma[j]), feature=round(nrow(ref_data)/3), th=0.00001) #Kernel PCA계산
    ker_term_tun <- rotated(ker_tmp) #Kernel PcA에 의해 projection된 Training data의 x값
    ker_data_tun <- data.frame(ker_response, ker_term_tun) #Regression을 위한 데이터셋 설정
    ker_vm_model_tun_result <- lm(as.formula(paste("ker_response", paste(names(ker_data_tun)[-1], collapse = " + "), sep = " ~ ")), data=ker_data_tun) #Kernel PCR 모형 저장
    #ker_pred_val <- CVlm(df=ker_data_tun, ker_vm_model_tun_result$call[[2]], m=ker_nfold, printif=F)$cvpred #10-fold Cross-Validation에서의 예측값 계산
    ker_pred_val <- CVlm_jy(data=ker_data_tun, as.formula(paste("Ker_response", paste(names(ker_data_tun)[-1], collapse = " + "), sep = " ~ ")), m=ker_nfold, printit=F)$cvpred
    #RMSEP 계산
    if(j == 1){ker_tun1 <- ker_rmsep(ker_pred_val, ker_real_val)
    ker_tuning_result$sigma <- ker_sigma[j] #최적 sigma값 저장
    ker_tuning_result$feature <- dim(ker_term_tun)[2]#최적 component 갯수 저장
    } else {ker_tun2 <- ker_rmsep(ker_pred_val, ker_real_val)
    if(ker_tun1 > ker_tun2){
      ker_tun1 <- ker_tun2
      ker_tuning_result$sigma <- ker_sigma[j] #최적 sigma 값 저장
      ker_tuning_result$feature <- dim(ker_term_tun)[2] #최적 component 갯수 저장
    }}
  }
  #모형 설정 : 최적 Gaussian 커널함수 sigma 값, component 갯수 적용
  ker_vm_model_kpca <- kpca(~., data=ref_data[,para_names], kernel="rbfdot", kpar=list(sigma=ker_tuning_result$sigma), feature=ker_tuning_result$feature) #Kernel PCA 계산
  ker_term_result <- rotated(ker_vm_model_kpca) #Kernel PcA에 의해 projection된 ref_data의 x값
  ker_data_result <- data.frame(ker_response, ker_term_result) #Regression을 위한 데이터셋 설정
  ker_vm_model_result <- lm(as.formula(paste("ker_response", paste(names(ker_data_result)[-1], collapse = " + "), sep = " ~ ")), data=ker_data_result) #Kernel PCA모형 저장
  new_data1 <- rbind(ref_data[,para_names], new_data[,para_names])
  ker_vm_model_kpca_pred <- kpca(~., data=new_data1[,para_names], kernel="rbfdot", kpar=list(sigma=ker_tuning_result$sigma), feature=ker_tuning_result$feature) #Kernel PCA 모형
  ker_term_pred <- rotated(ker_vm_model_kpca_pred) #Kernel PCA에 projection된 vm_data의 x값
  colnames(ker_term_pred) <- names(data.frame(ker_term_pred))#vm_data의 컬럼명 설정
  ker_result <- predict(ker_vm_model_result, as.data.frame(ker_term_pred))[nrow(new_data1)] #예측값
  return(ker_result)
} 
#Kernel PCR 모형 끝

#Bagging 모형 시작
bagging_vm_model <- function(ref_data, new_data, y_names, para_names){
  #모형 설정
  bag_vm_model_result <- bagging(as.formula(paste(y_names, paste(para_names, collapse = " + "), sep = " ~ ")), #Bagging 모형 저장 
                                 data=ref_data, control=rpart.control(minsplit=2, cp=.0), nbagg=100)
  bag_result <- predict(bag_vm_model_result, as.matrix(new_data[,para_names])) #예측값
  return(bag_result)
}
#Bagging 모형 끝

#CVlm_jy 시작
CVlm_jy <- function(data=DAAG::houseprices, form.lm=formula(sale.price ~ area), m=3, dots=FALSE, seed=29, plotit=c("Observed", "Residual"), main="Small symbols show cross-validatiaon predicted values", legend.pos="topleft", printit=TRUE){
  gphtype <- ""
  if(is.logical(plotit)){
    if(plotit)
      gphtype <- "Observed"
  }
  else if (is.character(plotit)){
    if(!(plotit[1] %in% c("Observed", "Residual", "")))
      stop(paste("Illegal argument plotit =", plotit[1]))
    gphtype <- plotit[1]
    if(plotit[1] %in% c("Observed","Residual"))
      plotit <- TRUE
  }
  else stop("Argument plotit must be logical or character")
  if(class(form.lm)=="formula")
    form <- form.lm
  else if(class(form.lm) %in% c("call", "lm"))
    form <- formula(form.lm)
  else stop("form.lm must be formula or call or lm object")
  formtxt <- deparse(form)
  mf <- model.frame(form, data=data)
  ynam <- attr(mf, "names")[attr(attr(mf, "terms"), "response")]
  data.lm <- lm(mf)
  tm <- terms(mf)
  xcolumns <- labels(tm)
  n <- nrow(data)
  data[,tnam] <- model.response(mf)
  data[, "Predicted"] <- predict(data.lm)
  data[, "cvpred"] <- numeric(n)
  yval <- mf[,ynam]
  if(gphtype == "Residual")
    yval <- yval - data[, "Predicted"]
  if(!is.null(seed))
    set.seed(seed)
  n <- dim(data)[1]
  rand <- sample(n)%%m +1
  foldnum <- sort(unique(rand))
  for(i in foldnum){
    rows.in <- rand != i
    rows.out <- rand == i
    subs.lm <- lm(form, data=data[rows.in, ])
    data[rows.out, "cvpred"] <- predict(subs.lm, newdata=data[rows.out, ])
  }
  if(length(xcolumns) == 1){
    stline <- TRUE
    xnam <- xcolumns
  }
  else {stline <- FALSE
  xnam <- "Predicted"}
  if (printit){options(digits=3)
    print(anova(data.lm))
    cat("\n")}
  # if(plotit){
  #   oldpar <- par(mar=par()$mar - c(1,0,2,0))
  #   on.exit(par(pldpar))
  #   coltypes <- palette()[c(2,3,6,1,4:5, 7)]
  #   if(m>7)
  #     coltypes <- c(coltypes, rainbow(m-7))
  #   ltypes <- 1:m
  #   ptypes <- 2:(m+1)
  #   par(lwd = 2)
  #   if(stline)
  #     xlab <- xnam
  #   else{
  #     xlab <- "Predicted(fit to all data)"
  #     cat("\n")
  #     warning(paste("\n\nAs there is >1 explanatory bariable, corss-validation\n", 
  #                   "predicted values for a fold are not a linear function\n",
  #                   "of corresponding overall predicted values.Lines that\n",
  #                   "are shown for the different folds are approximate\n"))
  #   }
  # ylab <- ynam
  # if(gphtype == "Residual")
  #   ylab <- paste(ynam, " (offset from predicted using all data)")
  # plot(as.formula(paste("yval ~", xnam)), data=data, ylab=ylab, type="p", pch=ptypes[rand], col=coltypes[rand], cex=1.25, xlab=xlab)
  # title(main=main, cex=1.05)
  # if(dots){
  #   with(data, points(as.formula(paste("yval ~", xnam)),
  #                     data=data, type="p", pch=16, col=coltypes[rand],
  #                     cex=1))
  # }
  # }
  if(printit | plotit){
    sumss <- 0
    sumdf <- 0
    for(i in foldnum){
      rows.in <- rand != i
      rows.out <- rand == i
      n.out <- sum(rows.out)
      resid <- data[rows.out, ynam]-data[rows.out, "cvpred"]
      ss <- sum(resid^2)
      sumss <- sumss + ss
      if(printit){
        fold_data <- t(cbind(data[rows.out, c(xnam, "cvpred", ynam)], resid))
        rownames(fold_data)=c(xnam, "cvpred", ynam, "CV residual")
        cat("\nfold", i, "\n")
        cat("Observations in test set:", n.out, "\n")
        print(fold_data, collb=rep("", n.out))
        cat("\nSum of squares =", round(ss, 2), " Mean square =", rouns(ss/n.out, 2), " n=", n.out, "\n")
      }
      if(plotit){
        # xval <- data[row.out, xnam]
        # nminmax <- c(which.min(xval), which.max(xval))
        # cvpred <- data[rows.out, "cvpred"]
        # if(gphtype == "Residual")
        #   cvpred <- cvpred - data[rows.out, "Predicted"]
        # # points(xval, cvpred, col=coltypes[i], pch=ptypes[i], cex=0.75, lwd=1)
        # n1 <- which.min(xval)
        # n2 <- which.max(xval)
        # fold.lm <- lm(cvpred ~ xval)
        # fold.b <- coef(fold.lm)
        # lines(xval[c(n1, n2)], fold.b[1] + fold.b[2] * xval[c(n1, n2)], col=coltypes[i], lty=ltypes[i])
        # topleft <- par()$usr[c(1,4)]
        # par(lwd=1, col=1)
        # legend(x=legend.pos, legend=paste("Fold", 1:m), pch=ptypes, lty=ltypes, col=coltypes, cex=0.75)
      }
    }
  }
  sumdf <- sum(!is.na(data[, "Predicted"]))
  if(printit){
    cat("\nOverall","(Sum over all", n.out, "folds)", "\n")
    print(c(ms = sumss/sumdf))
  }
  attr(data, "ms") <- sumss/sumdf
  attr(data, "df") <- sumdf
  invisible(data)
}
#CVlm_jy 끝

#gom_model 시작
gom_model <- function(ref_data, times){
  ifelse(nrow(ref_data)%%3==0, ref_data <- ref_data, ref_data <- ref_data[nrow(ref_data)%%3+1:(nrow(ref_data)-nrow(ref_data)%%3),])
  ref_data <- as.data.frame(ref_data)
  ref_data$cnt <- (0:(nrow(ref_data)-1))
  n <- nrow(ref_data)
  ref_data$cume <- cumsum(ref_data[,1])
  ref_data$ln <- log10(ref_data$cume)
  tmp <- ref_data[(ref_data$cnt+1) %% (n/3)==0, 2]
  c1 <- sum(ref_data$ln[1:(n/length(tmp))])
  c2 <- sum(ref_data$ln[(n/length(tmp)+1):(n/length(tmp)+(n/3))])
  c3 <- sum(ref_data$ln[(n/length(tmp)+(n/3)+1):(n/length(tmp)+((n/3)*2))])
  b <- ((c3-c2)/(c2-c1))^(1/(n/3))
  a <- 10^((c2-c1)*((b-1/(b^(n/3)-1)^2)))
  k <- 10^((1/(n/3))*((((c1*c3)-(c2^2))/(c1+c3-2*c2))))
  result_data <- NULL
  result_data$cnt <- c(0:times)
  result_data <- as.data.frame(result_data)
  result_data$est <- k*a^(b^result_data$cnt)
  return(result_data)
}
#gom_model 끝

#gom_model_para 시작
gom_model_para <- function(ref_data){
  ifelse(nrow(ref_data)%%3 ==0, ref_data <- ref_data, ref_data <- ref_data[nrow(ref_data)%%3+1:(nrow(ref_data)-nrow(ref_data)%%3),])
  ref_data <- as.data.frame(ref_data)
  ref_data$cnt <- (0:(nrow(ref_data)-1))
  n <- nrow(ref_data)
  ref_data$cume <-cumsum(ref_data[,1])
  ref_data$ln <- log10(ref_data$cume)
  tmp <- ref_data[(ref_data$cnt+1)%%(n/3)==0.2]
  c1 <- sum(ref_data$ln[0:tmp[1]+1])
  c2 <- sum(ref_data$ln[tmp[1]+2:((n/3)+1)])
  c3 <- sum(ref_data$ln[tmp[2]+2:((n/3)+1)])
  b <- ((c3-c2)/(c2-c1))^(1/(n/3))
  a <- 10^((c2-c1)*((b-1)/((b^(n/3)-1)^2)))
  k <- 10^((1/(n/3))*((((c1*c3)-(c2^2))/(c1+c3-2*2))))
  result_data <- cbind(b,a,k)
  return(result_data)
}
#gom_model_para 끝

#gom_model_para_brand 시작
gom_model_para_brand <- function(ref_data){
  ifelse(nrow(ref_dat)%%3==0, ref_data <- ref_data, ref_data <- ref_data[nrow(ref_data)%%3+1:(nrow(ref_data)-nrow(ref_data)%%3),])
  ref_data <- as.data.frame(ref_data)
  ref_data$cnt <- (0:(nrow(ref_data)-1))
  n <- nrow(ref_data)
  ref_data$cume <- cumsum(ref_data[,3])
  ref_data$ln <- log10(ref_data$cume)
  tmp <- ref_data[(ref_data$cnt+1) %%(n/3) ==0, 4]
  c1 <- sum(ref_data$ln[0:tmp[1]+1])
  c2 <- sum(ref_data$ln[tmp[1]+2:((n/3)+1)])
  c3 <- sum(ref_data$ln[tmp[2]+2:((n/3)+1)])
  b <- ((c3-c2)/(c2-c1))^(1/(n/3))
  a <- 10^((c2-c1)*((b-1)/((b^(n/3)-1)^2)))
  k <- 10^((1/(n/3))*((((c1*c3)-(c2^2))/(c1+c3-2*c2))))
  result_data <- cbind(ref_data[,1],b,a,k)
  return(result_data)
}
#gom_model_para_brand 끝
