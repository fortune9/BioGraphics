#!/usr/bin/env Rscript

# functions
load_pkg<-function(pkg)
{
	if(!require(pkg,character.only=T,warn.conflicts=F)) {
		install.packages(pkg, dependencies=T,repos="https://cloud.r-project.org");
		suppressMessages(library(pkg,character.only=T));
	}
}

self<-function()
{
	args<-commandArgs(F)
	path<-sub("^--file=",'',grep("^--file=", args, value=T))
}

msg<-function(...)
{
	exe<-basename(self())
	cat(paste("[",exe,"]",sep=""),...,"\n",sep=" ")
}

# read data from a file
read_data<-function(f,columns=1,sep="\t",commChar="#")
{
	d<-read.table(f,head=F,sep=sep,stringsAsFactors=F,comment.char=commChar)
	if(length(columns)==1) # only 1 column
	{
		d<-d[,1]
	}
	return(d)
}

# get the filename extension
get_ext<-function(f)
{
	m<-regexec(".*\\.([^.]+)$",f)
	return(regmatches(f,m)[[1]][2])
}

pause<-function(...)
{
	if(interactive())
	{
		invisible(readline(...))
	}else
	{
		cat(...,"\n")
		invisible(readLines(file("stdin"),n=1))
	}
}

# program information
load_pkg("optparse")

desc<-"
This program is to create a heatmap using R. In addition to
the general options, one can also pass parameters to the R function
directly using the option --rfunc-opts (see below).

*****************************
used R function: gplots::heatmap.2
*****************************

<Input file>
The input file should contain a matrix with or without column and
row names. The field separator can be comma or tab.

Default option values are in [].
"

epiInfo<-"
Example uses:

# in default, generate a full plot with key, dendrograms, and heatmap
%prog -o test.pdf test.csv

# in default, generate a full plot with key, dendrograms, and heatmap
# and view it on screen
%prog test.csv

# generate a simple heatmap
%prog --simple test.csv

# specify a different color scheme
%prog test.csv --color=\"yellow,blue\"

# add side bar for columns and suppress row labels
%prog --column-color=column_colors.tsv --row-lab=none test.csv
# where column_colors.tsv contains the colors for each columnm, like
# black
# black
# yellow
# yellow
# ...

# use user-specified column names
%prog --row-lab=none --column-lab=sample_names.tsv -o test.pdf test.csv
# where sample_names.tsv presents one name per line and
# in the order of the samples in test.csv

# provide parameters to the R function
%prog -o test.pdf --rfunc-opts \"key=F,main='test_fun',margins=c(3,3)\" test.csv

Author: Zhenguo Zhang
Contact: zhangz.sci@gmail.com
";

option_list<-list(
	# general figure and output options
	make_option(c("-o","--outfile"),action="store",
				dest="outFile",type="character",
				help="Output filename. Default is screen"),
	make_option(c("--out-format"),action="store",default=NULL,
				dest="outFormat",type="character",
				help="Output file format [determined from outfile extension]"),
	make_option(c("-W","--width"),action="store",
				dest="figWid",type="double",default=7,
				help="The width of the figure in inches [%default]"),
	make_option(c("-H","--height"),action="store",
				dest="figHei",type="double",default=7,
				help="The height of the figure in inches [%default]"),
	make_option(c("--res"),action="store",
				dest="figRes",type="integer",default=300,
				help="The figure resolution for bitmap files [%default]"),
	make_option(c("--color"),action="store",default="green,white,red",
				dest="colorVect",type="character",
				help="a list of colors spanning a spectrum over which input data will be mapped, from small to big values [%default]"),
	make_option(c("--color-res"),action="store",default=100,
				dest="colorRes",type="integer",
				help="the number of colors in the spectrum. The higher, the more noticeable among different data values [%default]"),
	make_option(c("--no-rownames"),action="store_true",default=FALSE,
				dest="noRownames",type="logical",
				help="in default, the 1st column of input file is assumed to be rownames, unless this option is provided [%default]"),
	# plot tuning options
	make_option(c("--no-key"),action="store_true",default=FALSE,
				dest="noKey",type="logical",
				help="don't plot color key [%default]"),
	make_option(c("--column-color"),action="store",default=NA,
				dest="colColor",type="character",
				help="a file providing the colors for the column side bar, one column per line"),
	make_option(c("--row-color"),action="store",default=NA,
				dest="rowColor",type="character",
				help="a file providing the colors for the row side bar, one row per line"),
	make_option(c("--column-lab"),action="store",default=NULL,
				dest="colLab",type="character",
				help="the column labels. Provide 'none' to suppress labels, or a file containing the labels, one column per line [column names of the input matrix]"),
	make_option(c("--row-lab"),action="store",default=NULL,
				dest="rowLab",type="character",
				help="the row labels. Provide 'none' to suppress labels, or a file containing the labels, one row per line [row names of the input matrix]"),
	make_option(c("--key-x-lab"),action="store",default="value",
				dest="keyXlab",type="character",
				help="the x label for key. [%default]"),
	make_option(c("--rfunc-opts"),action="store",default=NULL,
				dest="funcOpts",type="character",
				help="Use this to specify options directly the used R function, and it overrides all other options"),
	make_option(c("--simple"),action="store_true",default=FALSE,
				dest="simple",type="logical",
				help="if provided, the heatmap has no dendrogram, nor density, nor trace lines, and NO clustering for rows and columns [%default]")
				  );

optParser<-OptionParser(option_list=option_list,
						add_help_option=F,
						description=desc,
						epilog=epiInfo)
args<-commandArgs(T);
if(length(args) < 1 || any(args %in% c("-h","--help"))) 
	{ print_help(optParser); q("no"); }

opt<-parse_args2(optParser, args)

#args<-commandArgs(T);
posArgs<-opt$args
opt<-opt$options

#print(opt$funcOpts); q("no")

if(length(posArgs) != 1)
{
	stop("Only 1 input file is expected, but '",
		 posArgs, "' received")
}

msg("Reading data")
load_pkg("data.table")
load_pkg("gplots")
load_pkg("RColorBrewer")

# prepare the data
dat<-fread(posArgs[1])
if(!opt$noRownames)
{
	tmp1<-dat[[1]]; # rownames
	dat<-as.matrix(dat[,-1])
	rownames(dat)<-tmp1
}
#class(dat[[1]]); q("no")

# let's read column/row colors
colColors<-opt$colColor
if(!is.na(colColors))
{
	colColors<-read_data(colColors,commChar="")
}
#cat(colColors); q("no");
rowColors<-opt$rowColor
if(!is.na(rowColors))
{
	rowColors<-read_data(rowColors,commChar="")
}
# let's read row and column labels
labMargins<-c(4,3) # for labels
colLabels<-opt$colLab
if(!is.null(colLabels))
{
	if(colLabels == "none"){
		colLabels<-F
		labMargins[1]<-0.5
	}else
	{
		colLabels<-read_data(colLabels);
	}
}else # use default
{
	colLabels<-colnames(dat)
}
#cat(colLabels); q("no")

rowLabels<-opt$rowLab
if(!is.null(rowLabels))
{
	if(rowLabels == "none"){
		rowLabels<-F
		labMargins[2]<-0.5
	}else
	{
		rowLabels<-read_data(rowLabels);
	}
}else
{
	rowLabels<-rownames(dat)
}

withKey<-!opt$noKey
keyXlab<-opt$keyXlab

msg("Generating plot")
## determine the output device
outFile<-opt$outFile
wid<-opt$figWid
hei<-opt$figHei
figRes<-opt$figRes
#print(class(outFile))
#cat(wid, hei, figRes)
#q("no")
if(!is.null(outFile)) # outfile is provided
{
	outFormat<-opt$outFormat
	if(is.null(outFormat)) { 
		outFormat<-get_ext(outFile) 
		if(is.na(outFormat))
		{
			stop("No file extension detected from from '", outFormat, outFile,"'")
		}else
		{
			msg("Outfile extension is set to ", outFormat)
		}
	}
	if(outFormat == "pdf")
	{
		pdf(outFile, width=wid, height=hei)
	}else if(outFormat %in% c("bmp","jpeg","png","tiff"))
	{
		get(outFormat)(outFile,width=wid,height=hei,units="in",res=figRes)
	}else
	{
		stop("Unsupported output format: '", outFormat,"'")
	}
}else # no outfile, use x11() as default
{
	x11()
}

# now manipulate the color
colorVect<-strsplit(opt$colorVect,",")[[1]];
colorN<-opt$colorRes;
colorPal<-colorRampPalette(colorVect)(colorN)
# add the user provided paramters if available
#print(vn); q("no");
if(!is.null(opt$funcOpts))
{
	cmd<-paste("heatmap.2(dat,col=colorPal,",opt$funcOpts,")")
	msg("running command", cmd)
	eval(parse(text=cmd))
}else
{
	lo<-rbind(c(5,4,2),c(6,1,3))
	lwid<-c(1,8,1)
	lhei<-c(2,8)
	keypar<-list(mar=c(2,0,2,0),mgp=c(2,1,0))
	if(opt$simple) # no ordering/clustering
	{
		heatmap.2(dat, col=colorPal,Rowv=F,Colv=F,dendrogram="none", 
				  density.info="none", trace="none", 
				  cex.lab=0.8, cex.axis=0.7, labRow=rowLabels, labCol=colLabels,
				  #cellnote=dat,
				  margins=c(2,0),
				  lmat=lo,lhei=lhei,lwid=lwid)
	}else
	{
		sideBarColors<-"";
		if(!is.na(colColors[1])) {sideBarColors<-"ColSideColors=colColors"}
		if(!is.na(rowColors[1])) {sideBarColors<-paste(sideBarColors,
											"RowSideColors=rowColors", sep=",")}
		cmd<-paste('heatmap.2(dat, col=colorPal, key=withKey, key.xlab=keyXlab,
				  density.info="none", trace="none", srtCol=45,
				  cex.lab=0.8, cex.axis=0.7, labRow=rowLabels, labCol=colLabels,
				  margins=labMargins,key.title="DNA"', sideBarColors, sep=",")
		cmd<-sub(",$","",cmd); # remove the last comma if applicable
		cmd<-paste(cmd,")")
		msg("running command", cmd)
		eval(parse(text=cmd))
	}
}

if(is.null(outFile))
{
	pause("Press ENTER key to continue ")
}else
{
	msg("Outfile '",outFile,"' is generated")
}

invisible(dev.off())

