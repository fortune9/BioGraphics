#!/usr/bin/env Rscript

# functions
load_pkg<-function(pkg)
{
   if(!require(pkg,character.only=T,warn.conflicts=F)) {
      install.packages(pkg, dependencies=T,repos="https://cloud.r-proj
ect.org");
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

# get value from the dataset given variable name
get_value<-function(vName, type)
{
	if(is.null(vName) || is.na(vName)) { return(NULL) } 
	if(vName %in% names(dat))
	{
        v<-dat[[vName]];
	}else
	{
        vInd<-as.integer(vName);
		if(is.na(vInd)) { stop("Unrecognized column '", vName,"'") }
        if(vInd > length(dat)) {stop("The ",vName," variable column number ", vInd, " is out of range of input data") }
        v<-dat[[vInd]];
		vName<-colnames(dat)[vInd]
	}
	v<-list(v=type(v))
	names(v)[1]<-vName;
	return(v); # return a list
}
# program information
load_pkg("optparse")

desc<-"
This program is to create a barplot using R. In addition to
the general options, one can also pass parameters to the R function
directly using the option --rfunc-opts (see below).

*****************************
used R function: ggplot2::geom_bar()
*****************************

The <input-file> should have data values by columns which are separated
by comma or tab, like:

x,y,g
1,10,r
1,20,g
2,20,r
2,50,g

Default option values are in [].
"

epiInfo<-"
Example uses:

# run a plot without group and generate a pdf file
%prog -o test.pdf test.tsv

# with a group variable at column 3
%prog -o test.pdf -g 3 test.tsv

# add a title
%prog -o test.pdf -g 3 --title 'My plot' test.tsv

# specify the barplot specifications by your own.
# note the data are read into a data.frame 'dat' and
# you need provide the full 'geom_bar()' statement
%prog -o test.pdf --title 'My plot' --rfunc-opts 'geom_bar(mapping=aes(fill=g),data=dat,position=position_dodge2(preserve=\"single\"),stat=\"identity\")' test.tsv

# you can also suppress legend title for example
%prog -o test.pdf --title 'My plot' --rfunc-opts 'geom_bar(mapping=aes(fill=g),data=dat,position=position_dodge2(preserve=\"single\"),stat=\"identity\")+theme(legend.title=element_blank())' test.tsv

Author: Zhenguo Zhang
Contact: zhangz.sci@gmail.com
";

option_list<-list(
	make_option(c("-y", "--y-var"),action="store",default=2,
				dest="yCol",type="character",
				help="The column name or number in the input file for the y values which determines bar heights [%default]"),
	make_option(c("-x", "--x-var"),action="store",default=1,
				dest="xCol",type="character",
				help="The column name or number in the input file for the x values which mark bar positions [%default]"),
	make_option(c("--keep-x-order"),action="store_true",default=FALSE,
				dest="keepXOrder",type="logical",
				help="a switch option, if provided, the bars will be ordered as input for x variable, otherwise sort alphabetically"),
	make_option(c("-g", "--group"),action="store",default=NULL,
				dest="gCol",type="character",
				help="The column name or number in the input file for a group variable, which will lead to separate bars [%default]"),
	make_option(c("--min"),action="store",default=NULL,
				dest="minCol",type="character",
				help="The column name or number in the input file for the lower end of error bar [%default]"),
	make_option(c("--max"),action="store",default=NULL,
				dest="maxCol",type="character",
				help="The column name or number in the input file for the higher end of error bar [%default]"),
	make_option(c("-o","--outfile"),action="store",
				dest="outFile",type="character",
				help="Output filename"),
	make_option(c("--out-format"),action="store",default=NULL,
				dest="outFormat",type="character",
				help="Output file format, available for pdf, png, jpeg, tiff [determined from outfile extension]"),
	make_option(c("-W","--width"),action="store",
				dest="figWid",type="double",default=7,
				help="The width of the figure in inches [%default]"),
	make_option(c("-H","--height"),action="store",
				dest="figHei",type="double",default=7,
				help="The height of the figure in inches [%default]"),
	make_option(c("--res"),action="store",
				dest="figRes",type="integer",default=300,
				help="The figure resolution for bitmap files in dpi [%default]"),
	make_option(c("--title"),action="store",
				dest="figTitle",type="character",default=NULL,
				help="The figure title [%default]"),
	make_option(c("--legend-pos"),action="store",
				dest="legendPos",type="character",default="right",
				help="legend position, allowed values are top, bottom, left, right, and none [%default]"),
	make_option(c("--horizontal"),action="store_true",
				dest="horiz",type="logical",default=FALSE,
				help="if provided, the bars will be laid horizontally"),
	make_option(c("--rfunc-opts"),action="store",default=NULL,
				dest="funcOpts",type="character",
				help="The options directly passed to the used R function. Here it is appended to constructed ggplot object")
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
		 posArgs, "' are received")
}

msg("Reading data")

load_pkg("data.table")
load_pkg("ggplot2")

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

# get the data, returned as list
x<-get_value(opt$xCol, type=as.character)
y<-get_value(opt$yCol, type=as.double)
g<-get_value(opt$gCol, type=as.character)
low<-get_value(opt$minCol, type=as.double);
high<-get_value(opt$maxCol, type=as.double);

xVal<-x[[1]];
yVal<-y[[1]];
if(opt$keepXOrder)
{
	xVal<-factor(xVal,levels=unique(xVal))
}
if(is.null(g))
{
	#p<-ggplot(data.frame(), aes(x[[1]],y[[1]]))
	p<-ggplot(data.frame(), aes(xVal,yVal))
}else # group exists
{
	p<-ggplot(data.frame(), aes(xVal,yVal,fill=g[[1]])) + scale_fill_discrete(names(g)[1])
}

## set default theme parameters
# add xlab, ylab, and title
p<-p+xlab(names(x)[1])+ylab(names(y)[1])
if(!is.null(opt$figTitle))
{
	p<-p+ggtitle(opt$figTitle)+theme(plot.title=element_text(hjust=0.5))
}

#p+geom_bar(position=position_dodge2(preserve="single"),stat="identity")+theme(panel.background=NULL, panel.grid.major.x=element_line(linetype="dotted",size=0.5,color="black"))+coord_flip()


# set legend position
p<-p + theme_bw()+theme(legend.position=opt$legendPos)
# flip bars if requested
if(opt$horiz)
{
	p<-p+coord_flip(expand=F)
}

# add the user provided paramters if available, which can override
# previous settings.
if(!is.null(opt$funcOpts))
{
	cmd<-paste("p + ",opt$funcOpts)
	msg("running command", cmd)
	p<-eval(parse(text=cmd))
}else
{
	p<-p+geom_bar(position=position_dodge2(preserve="single"),stat="identity")
	#hist(v, xlab=vn,cex.main=0.9,cex.lab=0.8,cex.axis=0.7)
}

# add error bar if available
if(!is.null(low) && !is.null(high))
{
	ylimits<-aes(ymin=low, ymax=high)
	p<-geom_errorbar(ylimits, position=position_dodge2(preserve="single",padding=0.7))
}


# display the plot
p

if(is.null(outFile))
{
	pause("Press ENTER key to continue ")
}else
{
	msg("Outfile '",outFile,"' is generated")
}

invisible(dev.off())

