# qcc package ( statistical quality control and QC charts)
# qcc(Quality control charts)
# 품질관리에 많이 사용되는 패키지

# 파레토차트 함수 
# 파레토 법칙(20:80)

if(!any(grep("qcc",installed.packages()))) install.packages("qcc", repos="")
library(qcc)

# 일정기간 고객의 총 구매액수를 한산한 데이터로 핵심 고객들의 분포 파악시 유용

customer_amount <- c(2000,1500,1200, 700, 150, 120, 60, 30, 20, 15)
names(customer_amount) <- c(1,2,3,4,5,6,7,8,9,10)

# 그래프로 표현하기 위해서 기존 데이터를 정렬하여 전처리 해야함
pareto.chart(customer_amount, ylab = "USER_SALES", family = "AppleGothic", 
             col = rainbow(length(customer_amount)))



# shewhart chart
# X 막대형 차트
data("pistonrings")
View(pistonrings)
help("pistonrings")
# with() 데이터 프레임 또는 리스트 내 필드를 필드 이름만으로
# 접근할 수 있게 해주는 함수 attach 함수와 비슷
diameter <- with(pistonrings, qcc.groups(diameter,sample))

#head(diameter)
#View(diameter)

q1 <- qcc(diameter[1:25,], type = "xbar", newdata = diameter[26:40,])
# chart.all option 
# qcc함수로 나누어진 데이터표시방법
# FALSE -> new data로 선언된 부분만 시각화
# TRUE -> 전체 데이터 시각화
plot(q1, chart.all = FALSE)
# TRUE -> stats 값 표현 옵션 qcc에 계산된
plot(q1, add.stats = FALSE)

X11()

# 신뢰구간의 추가 
q2 <- qcc(diameter[1:25,], type = "xbar", newdata = diameter[26:40,],
          confidence.level = 0.99)
# 신뢰 상한 하한 및 표본크기제한및 표본위반데이터 수


# 경고 제한 추가(편차)
q3 <- qcc(diameter[1:25,], type = "xbar", newdata = diameter[26:40,], plot=FALSE)
          (warn.limits = limits.xbar(q3$center, q3$std.dev, q3$sizes,2))

plot(q3, restore.par = FALSE)
abline(h = warn.limits, lty = 3, col = "chocolate")



# R Chart
q4 <- qcc(diameter[1:25,], type = "R")
summary(q4)

q5 <- qcc(diameter[1:25,] , type = "R" , newdata = diameter[26:40,])
summary(q5)


# S chart
q6 <- qcc(diameter[1:25,], type = "S")
summary(q6)


q7 <- qcc(diameter[1:25,], type = "S", newdate = diameter[26:40,])
summary(q7)



# 가변 제어 한계
out <- c(9,10,30,35,45,64,65,74,75,86,99,100)
diameter2 <- with(pistonrings, qcc.groups(diameter[-out], sample[-out]))
summary(qcc(diameter2[1:25,],type = 'xbar'))

summary(qcc(diameter2[1:25,],type = 'R'))



# p and np chart

data("orangejuice")
qq1 <- with(orangejuice, qcc(D[trial], sizes = size[trial], type = 'p'))
summary(qq1)

qq2 <- with(orangejuice, qcc(D[trial], size  = size[trial], type ="np"))
summary(qq2)



# 통제 불능 지점 제거
help("orangejuice")
# 15번째에서 새로운 배치 상황, 23번째에서 작업자 변경( 통제불능 지점)
inc <- setdiff(which(orangejuice$trial),c(15,23))
qq3 <- with(orangejuice, 
            qcc(D[inc], sizes = size[inc], type = "p",
                newdata = D[!trial], newsizes = size[!trial]))



# orangejuice2
help("orangejuice2")
# orangejuice의 데이터에 장비 재정비한수 데이터가 들어가 있는 데이터셋
data("orangejuice2")
qq4 <- with(orangejuice2, qcc(D[trial], sizes = size[trial], type = "p", 
                              newdata = D[!trial], newsizes = size[!trial]))
View(orangejuice2)
summary(qq4)


# C and U Charts

# 100개의 인쇄 회로기판 26개 샘플에서 관찰된 부적합수
# 샘플 6 과 20은 관리한계 벗어난 데이터
# 샘플6을 새로운 검사관 
# 샘플 20은 비정상적으로 많은 기계 결함
# 마지막 20개의 샘플은 검사장치(각 100개의 보드로 구성)에 수집된 추가 샘플
data("circuit")
help("circuit")
View(circuit)

qq5 <- with(circuit, qcc(x[trial], sizes = size[trial], type = "c"))
summary(qq5)

# 제어 불능 지접 제거
inc2 <- setdiff(which(circuit$trial),c(6,20))
qq6 <- with(circuit, qcc(x[inc2], sizes = size[inc2] , type = "c", labels = inc2,
                         newdata = x[!trial], newsizes = size[!trial], newlabels = which(!trial)))
summary(qq6)


qq6 <- with(circuit, qcc(x[inc2], sizes = size[inc2], type = "u", labels = inc2,
                         newdata = x[!trial], newsizes = size[!trial], newlabels = which(!trial)))
summary(qq6)


# 개인용 컴퓨터 제조업체의 최종 조립 라인 단위당 부적합 수
# 20개의 샘플에서 각가 5개의 데이터 수집
data("pcmanufact")
help("pcmanufact")
View(pcmanufact)


qq7 <- with(pcmanufact, qcc(x, sizes = size, type = "u"))
summary(qq7)


# 연속적인 1회성 데이터
# viscositty daya(Montgomery, pag.242)
x <- c(33.75, 33.05, 34, 33.81, 33.46, 34.02, 33.68, 33.27, 33.49, 33.20,
      33.62, 33.00, 33.54, 33.12, 33.84)
qqq1 <- qcc(x, type="xbar.one")
summary(qqq1)

qqq2 <- qcc(x, type = 'xbar.one', std.dev = "SD")
summary(qqq2)



# 표준화된 P Chart
# 새로운 컨트롤 차트, 포준화된 p 차트(type = "p.std")를 정의
# 패키지 확장

# 그룹통계및 센터 계산
stats.p.std <- function(data,sizes){
  data = as.vector(data)
  sizes = as.vector(sizes)
  pbar = sum(data)/sum(sizes)
  z = (data/sizes - pbar)/sqrt(pbar*(1-pbar)/sizes)
  list(statistics = z, center = 0)
}

# 그룹내 표준 편차 계산
sd.p.std <- function(data,sizes, ...){ return(1)}

# 정규근사를 기반으로 제어 한계 계산
limits.p.std <- function(center, std.dev, sizes, conf){
  if(conf >=1){
    lcl <- -conf
    ucl <- conf
  }else{
    if(conf > 0 & conf < 1){
      nsigmas <- qnorm(1 - (1-conf)/2)
      lcl <- -nsigmas
      ucl <- nsigmas
    }else stop("invalid 'conf' argument.")
  }
  limits <- matrix(c(lcl, ucl), ncol = 2)
  rownames(limits) <- rep("", length = nrow(limits)) 
  colnames(limits) <- c("LCL","UCL")
  return(limits)
}

# 시뮬레이션 데이터 사용
# set unequal sample sizes
n <- c(rep(50,5), rep(100,5), rep(25,5))

# generate randomly the number of successes
x <- rbinom(length(n), n , 0.2)

# plot the control chart with variable limits
summary(qcc(x,type = 'p', size = n))
summary(qcc(x,type = 'p.std', size = n))


# 작동 특성 곡선
# 작동 특성 곡선은 프로세스의 변화를 감지하지 못할 확률에 대한 정보를 제공
# oc.curves()는 입력된 qcc객체 유형에 따라 호출

data("pistonrings")
diameter <- with(pistonrings, qcc.groups(diameter,sample))
q <- qcc(diameter, type = 'xbar', nsigmas = 3, plot=F)
beta <- oc.curves.xbar(q)
print(round(beta, digits =4))
