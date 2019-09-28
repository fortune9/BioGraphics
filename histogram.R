#!/usr/bin/env Rscript

# functions
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
suppressMessages(library(optparse))

desc<-"
This program is to create a historgram using R. In addition to
the general options, one can also pass parameters to the R function
directly using the option --rfunc-opts (see below).

*****************************
used R function: hist()
*****************************

Default option values are in [].
"

epiInfo<-"
Example uses:

# default run to generate a pdf file
%prog -o test.pdf test.tsv

# specify data column
%prog -o test.pdf --col 2 test.tsv

# provide parameters to the R function
%prog -o test.pdf --rfunc-opts \"xlab='number',prob=T,xlim=c(-10,10)\" test.tsv

Author: Zhenguo Zhang
Contact: zhangz.sci@gmail.com
";

option_list<-list(
	make_option(c("--col"),action="store",default=1,
				dest="vCol",type="integer",
				help="The column at which the data are held in input file, given by column number [%default]"),
	make_option(c("-o","--outfile"),action="store",
				dest="outFile",type="character",
				help="Output filename"),
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
	make_option(c("--rfunc-opts"),action="store",default=NULL,
				dest="funcOpts",type="character",
				help="The options directly passed to the used R function, 'breaks=1:10,freq=F'")
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
suppressMessages(library(data.table))
dat<-fread(posArgs[1])
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
vCol<-opt$vCol
if(vCol > length(dat)) { stop(paste("The value for the option --col",vCol,"is out of data range:", length(dat))) }
v<-dat[[vCol]]; vn<-names(dat)[vCol];
#print(vn); q("no");
if(!is.null(opt$funcOpts))
{
	cmd<-paste("hist(v,",opt$funcOpts,")")
	msg("running command", cmd)
	eval(parse(text=cmd))
}else
{
	hist(v, xlab=vn)
}

if(is.null(outFile))
{
	pause("Press ENTER key to continue ")
}else
{
	msg("Outfile '",outFile,"' is generated")
}

invisible(dev.off())

