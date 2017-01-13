##将元素类型为list的list转换成矩阵，子list长度可以不相同
LLtoM <- function(pos){
	RC <- sapply(lapply(pos,length),identity) #各个list的长度
	NC <- max(RC) #各个list的长度的最大值，用作M的列数
	NR <- length(pos) #M的行数
	M <- matrix(ncol=NC)
	plyr::l_ply(pos,function(s){
		s <- as.array(s)
		if (length(s)<NC) s[NC] <- NA
		if (is.na(M[1,1])) {
			M <<- matrix(s,ncol=NC)
		}else M <<- rbind(M,s)
	})
	return(M)
}

##根据gregexpr的返回结果，提取对应的值
##如果返回多个结果，则提取多个值
getRegVal <- function(s,pos){
	#s是字符串序列
	#pos是gregexpr返回结果，是元素类型为list的list
	pos_start <- LLtoM(pos)
	pos_stop <- pos_start + LLtoM(lapply(pos,attr,which="match.length"))-1
	#构造字符串类型的代码的序列或矩阵，用parse转换成表达式的序列或矩阵，然后用sapply等批量执行
	tmp <- matrix(	paste(	"substr(x='",
							s,
							"',start=",
							pos_start,
							",stop=",
							pos_stop,
							")",
							sep=""),
					ncol=ncol(pos_start))
	re <- apply(tmp,1,function(ss){
		sapply(ss,FUN=function(sss){
			eval(parse(text=sss))
		})
	})
	return(re)
}

##设置进度条
proBar <- function(){
	require(tcltk)
	pb <- tkProgressBar("进度","已完成",0,100)
	n <- 100
	for (i in 1:n) {
		Sys.sleep(0.05)
		info <- sprintf("已完成%6.1f%%",i*100/n)
		setTkProgressBar(pb,i*100/n,sprintf("进度%s",info),info)
	}
	close(pb)
}

##检验字符串序列t中是否存在某种text模式s
checkText <- function(s,t){
	grepl(s,paste(t,sep="",collapse=TRUE))
}

##关闭sqlQuery函数的数据转换功能，比如将"003021"转换为3021
RODBC::sqlQuery(conn,sql1,as.is=TRUE)

##有时，用gsub替换掉空格后，汉字编码会从GB2312变成UTF-8，可以用useBytes=TRUE关闭
re <- gsub(" ","","金 融 街",useBytes=TRUE)
##将UTF-8的字符串转换成正常编码的方法
##可能有用的函数有Encoding、utf8ToInt、charToRaw、nchar(re，type="bytes")
re <- strtrim(re,nchar(re)*2)

##构造字符串类型的代码的序列或矩阵，用parse转换成表达式的序列或矩阵，然后用sapply等批量执行
p <- sapply(parse(text=paste(	"gregexpr('[0-9A-Z]+?:",
								re,
								"','",
								tmp,
								"')",
								sep="")),
			eval)
			
