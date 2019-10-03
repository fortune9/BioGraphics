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
This program is to create a scatter plot using R. In addition to
the general options, one can also pass parameters to the R function
directly using the option --rfunc-opts (see below).

*****************************
used R function: plot
*****************************

<Input file>
The input file should contain both x and y values for the plot.
The field separator can be comma or tab.

Default option values are in [].
"

epiInfo<-"
Example uses:

# make the default plot
%prog -o test.pdf input.tsv

# specify the variable by column numbers or names
%prog -o test.pdf input.tsv -x 7 -y 8
%prog -o test.pdf input.tsv -x xName -y yName

# add R function paramters
%prog -o test.pdf input.tsv --rfunc-opts \"col='red', main='test main'\"

# suppress correlation statistics computation
%prog -o test.pdf input.tsv --no-stat

# change correlation method to spearman
%prog -o test.pdf input.tsv --cor-method=spearman

Author: Zhenguo Zhang
Contact: zhangz.sci@gmail.com
";

option_list<-list(
	make_option(c("-x","--x-var"),action="store",default=1,
				dest="xCol",type="character",
				help="The column name or number for the x variable [%default]"),
	make_option(c("-y","--y-var"),action="store",default=2,
				dest="yCol",type="character",
				help="The column name or number for the y variable [%default]"),
	make_option(c("-W","--width"),action="store",
				dest="figWid",type="double",default=7,
				help="The width of the figure in inches [%default]"),
	make_option(c("-H","--height"),action="store",
				dest="figHei",type="double",default=7,
				help="The height of the figure in inches [%default]"),
	make_option(c("--res"),action="store",
				dest="figRes",type="integer",default=300,
				help="The figure resolution for bitmap files [%default]"),
	make_option(c("-o","--outfile"),action="store",
				dest="outFile",type="character",
				help="Output filename. Default is screen"),
	make_option(c("--out-format"),action="store",default=NULL,
				dest="outFormat",type="character",
				help="Output file format [determined from outfile extension]"),
	make_option(c("--rfunc-opts"),action="store",default=NULL,
				dest="funcOpts",type="character",
				help="The options directly passed to the used R function"),
	make_option(c("--no-stat"),action="store_true",default=FALSE,
				dest="noStat",type="logical",
				help="in default, a correlation test is done on the data. This option supresses this [%default]"),
	make_option(c("--stat-pos"),action="store",default="top",
				dest="statPos",type="character",
				help="The location to put correlation statistics. Choose from top, bottom, left, right, and combination of them such as topleft and bottomright [%default]"),
	make_option(c("--cor-method"),action="store",default="pearson",
				dest="corMethod",type="character",
				help="The statistical method used for correlation test: pearson or spearman [%default]")
				  );

optParser<-OptionParser(usage="usage: %prog [options] <input-file>",
						option_list=option_list,
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
#load_pkg("gplots")
#load_pkg("RColorBrewer")

# prepare the data
dat<-fread(posArgs[1])
xVar<-opt$xCol;
yVar<-opt$yCol;
if(xVar %in% names(dat))
{
	x<-dat[[xVar]];
}else
{
	xInd<-as.integer(xVar);
	if(xInd > length(dat)) {stop("The x variable column number ", xInd, " is out of range of input data") }
	x<-dat[[xInd]];
}
if(yVar %in% names(dat))
{
	y<-dat[[yVar]];
}else
{
	yInd<-as.integer(yVar);
	if(yInd > length(dat)) {stop("The y variable column number ", yInd, " is out of range of input data") }
	y<-dat[[yInd]];
}
#class(dat[[1]]); q("no")
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

# add the user provided paramters if available
#print(vn); q("no");
if(!is.null(opt$funcOpts))
{
	cmd<-paste("plot(x,y,",opt$funcOpts,")")
	msg("running command", cmd)
	eval(parse(text=cmd))
}else
{
		plot(x,y,cex.main=0.8, cex.lab=0.8, cex.axis=0.7)
}

# compute and add correlation statistics
if(!opt$noStat)
{
	cor.test(x, y, method=opt$corMethod)->corT
	r<-sprintf("%.2g", corT$est)
	p<-sprintf("%.2g", corT$p.val)
	statText<-c(as.expression(bquote(italic(R)==.(r))),as.expression(bquote(italic(P)==.(p))))
	legend(opt$statPos, legend=statText, bty="n",pch=NA,cex=0.8)
}

if(is.null(outFile))
{
	pause("Press ENTER key to continue ")
}else
{
	msg("Outfile '",outFile,"' is generated")
}

invisible(dev.off())

