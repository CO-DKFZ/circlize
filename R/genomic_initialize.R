
# == title
# Initialize the circular layout with an ideogram
#
# == param
# -cytoband  A path of the cytoband file or a data frame that already contains cytoband data. By default it is cytoband for hg19.
#            Pass to `read.cytoband`.
# -species Abbreviations of species. e.g. hg19 for human, mm10 for mouse. If this
#          value is specified, the function will download cytoBand.txt.gz from
#          UCSC website automatically. If there is no cytoband for user's species,
#          it will keep on trying to download chromInfo file. Pass to `read.cytoband` or `read.chromInfo`.
# -chromosome.index subset of chromosomes, also used to reorder chromosomes.
# -sort.chr Whether chromosome names should be sorted (first sort by numbers then by letters).
#           If ``chromosome.index`` is set, this argumetn is enforced to ``FALSE``
# -draw.chr.prefix Whether draw the "chr" prefix for chromosomes.
# -major.by     Increment of major ticks. Pass to `circos.genomicInitialize`.
# -plotType     Which tracks should be drawn. ``ideogram`` for ideogram rectangle, ``axis`` for genomic axis and ``labels`` for chromosome names.
#               If there is no ideogram for specified species, ``ideogram`` will be enforced to be excluded.
#               If it is set to ``NULL``, the function just initialize the plot but draw nothing.
# -track.height Height of the track which contains "axis" and "labels".
# -ideogram.height Height of the ideogram track
# -...    Pass to `circos.genomicInitialize`.
#
# == details
# The function will initialize the circular plot in which each sector corresponds to a chromosome. You can control the order of 
# chromosomes by ``chromosome.index`` or by ``sort.chr``, or by setting a special format of ``cytoband`` (please refer to `read.cytoband` 
# to find out how to control a proper ``cytoband``).
#
# The function finally pass data to `circos.genomicInitialize` to initialize the circular plot.
#
# The style of ideogram is almost fixed, but you can customize it with your self-sefined code. Refer to vignette for demonstration.
#
# == seealso
# https://jokergoo.github.io/circlize_book/book/initialize-genomic-plot.html#initialize-cytoband
#
# == example
# \donttest{
# circos.initializeWithIdeogram()
#
# cytoband.file = system.file(package = "circlize",
#     "extdata", "cytoBand.txt")
# circos.initializeWithIdeogram(cytoband.file)
#
# cytoband.df = read.table(cytoband.file, colClasses = c("character", "numeric",
#     "numeric", "character", "character"), sep = "\t")
# circos.initializeWithIdeogram(cytoband.df)
#
# circos.initializeWithIdeogram(species = "hg18")
#
# circos.initializeWithIdeogram(species = "mm10")
#
# circos.initializeWithIdeogram(chromosome.index = c("chr1", "chr2"))
#
# cytoband = read.table(cytoband.file, colClasses = c("character", "numeric",
#     "numeric", "character", "character"), sep = "\t")
# circos.initializeWithIdeogram(cytoband, sort.chr = FALSE)
#
# cytoband[[1]] = factor(cytoband[[1]], levels = paste0("chr", c(22:1, "X", "Y")))
# circos.initializeWithIdeogram(cytoband, sort.chr = FALSE)
#
# cytoband = read.table(cytoband.file, colClasses = c("character", "numeric",
#     "numeric", "character", "character"), sep = "\t")
# circos.initializeWithIdeogram(cytoband, sort.chr = TRUE)
#
# circos.initializeWithIdeogram(plotType = c("axis", "labels"))
#
# circos.initializeWithIdeogram(plotType = NULL)
#
# circos.par("start.degree" = 90)
# circos.initializeWithIdeogram()
# circos.clear()
#
# circos.par("gap.degree" = rep(c(2, 4), 12))
# circos.initializeWithIdeogram()
# circos.clear()
# }
circos.initializeWithIdeogram = function(
	cytoband = system.file(package = "circlize", "extdata", "cytoBand.txt"), 
	species = NULL, 
	sort.chr = TRUE,
	draw.chr.prefix = FALSE,
	chromosome.index = usable_chromosomes(species), 
	major.by = NULL,
	plotType = c("ideogram", "axis", "labels"), 
	track.height = NULL, 
	ideogram.height = convert_height(2, "mm"), 
	...) {
	
	if("sector.names" %in% names(list(...))) {
		stop_wrap("Found argumnet `sector.names` is used. Please use the argument `chromosome.index` instead.")
	}

	# proper order will be returned depending on cytoband and sort.chr
	e = try(cytoband <- read.cytoband(cytoband, species = species, sort.chr = sort.chr, chromosome.index = chromosome.index), silent = TRUE)
	if(inherits(e, "try-error") && !is.null(species)) {  # if species is defined
		e2 = try(cytoband <- read.chromInfo(species = species, sort.chr = sort.chr, chromosome.index = chromosome.index), silent = TRUE)
		if(inherits(e2, "try-error")) {
			message(e)
			message(e2)
			stop_wrap("Cannot download either cytoband or chromInfo file from UCSC.")
		} else {
			message_wrap("Downloading cytoBand file from UCSC failed. Use chromInfo file instead. Note ideogram track will be removed from the plot.")
			plotType = setdiff(plotType, "ideogram")

			# because in chromInfo file, there are also many short scaffold
			if(is.null(chromosome.index)) {
	            chromInfo = read.chromInfo(species = species)
	            chr_len = sort(chromInfo$chr.len, decreasing = TRUE)

	            # sometimes there are small scaffold
	            i = which(chr_len[seq_len(length(chr_len)-1)] / chr_len[seq_len(length(chr_len)-1)+1] > 5)[1]
	            if(length(i)) {
	                chromosome = chromInfo$chromosome[chromInfo$chromosome %in% names(chr_len[chr_len >= chr_len[i]])]
	            } else {
	                chromosome = chromInfo$chromosome
	            }
	            cytoband = read.chromInfo(species = species, chromosome.index = chromosome, sort.chr = sort.chr)
	        } 
		}
	} else if(inherits(e, "try-error")) {
		stop(e)
	}
	df = cytoband$df
	chromosome = cytoband$chromosome
	if(is.null(chromosome)) {
		if(is.factor(cytoband[, 1])) {
			chromosome = levels(cytoband$df[, 1])
		} else {
			chromosome = unique(cytoband$df[, 1])
		}
	}

	if(is.null(chromosome.index)) {
		chromosome.index = chromosome
	}

	# here df[[1]] is quite important, should be re-factered
	df[[1]] = factor(as.vector(df[[1]]), levels = chromosome.index)
	
	# sn for sector names, but not for sector index
	sn = unique(as.vector(df[[1]]))

	if(!draw.chr.prefix) {
		# we do not need 'chr' prefix if it exits, it holds too much space.
		sn = gsub("chr", "", sn)
	}
	
	o.cell.padding = circos.par("cell.padding")
	circos.par(cell.padding = c(o.cell.padding[1], 0, o.cell.padding[3], 0))
	
	circos.genomicInitialize(df, sector.names = sn, major.by = major.by, plotType = plotType, track.height = track.height, ...)

	if(any(plotType %in% "ideogram")) {
		circos.genomicIdeogram(df, track.height = ideogram.height)
	}
}


# == title
# Initialize circular plot with any genomic data
#
# == param
# -data         A data frame in bed format.
# -sector.names Labels for each sectors which will be drawn along each sector. It will not modify values of sector index.
# -major.by     Increment of major ticks. It is calculated automatically if the value is not set (about every 10 degrees there is a major tick).
# -plotType     If it is not ``NULL``, there will create a new track containing axis and names for sectors.
#               This argument controls which part should be drawn, ``axis`` for genomic axis and ``labels`` for chromosome names
# -tickLabelsStartFromZero Whether axis tick labels start from 0? This will only affect the axis labels while not affect x-values in cells.
# -axis.labels.cex The font size for the axis tick labels.
# -labels.cex   The font size for the labels.
# -track.height If ``PlotType`` is not ``NULL``, height of the annotation track.
# -...          Pass to `circos.initialize`
#
# == details
# The function will initialize circular plot from genomic data. If ``plotType`` is set with value in ``axis`` or ``labels``, there will
# create a new track.
#
# The order of sectors related to data structure of ``data``. If the first column in ``data`` is a factor, the order of sectors
# is ``levels(data[[1]])``; If the first column is just a simple vector, the order of sectors is ``unique(data[[1]]``.
#
# For more details on initializing genomic plot, please refer to the vignettes.
#
# == seealso
# https://jokergoo.github.io/circlize_book/book/initialize-genomic-plot.html#initialize-with-general-genomic-category
#
# == example
# df = read.cytoband()$df
# circos.genomicInitialize(df)
#
# df = data.frame(name = c("TP53", "TP63", "TP73"),
#                 start = c(7565097, 189349205, 3569084),
#                 end = c(7590856, 189615068, 3652765),
#                 stringsAsFactors = FALSE)
# circos.genomicInitialize(df)
# circos.clear()
#
# circos.genomicInitialize(df, tickLabelsStartFromZero = FALSE)
# circos.clear()
#
# circos.genomicInitialize(df, major.by = 5000)
# circos.clear()
#
# circos.genomicInitialize(df, plotType = "labels")
# circos.clear()
#
# circos.genomicInitialize(df, sector.names = c("tp53", "tp63", "tp73"))
# circos.clear()
#
# circos.genomicInitialize(df, sector.names = c("tp53x", "tp63x", "tp73"))
# circos.clear()
#
# df[[1]] = factor(df[[1]], levels = c("TP73", "TP63", "TP53"))
# circos.genomicInitialize(df)
# circos.clear()
#
circos.genomicInitialize = function(
	data, 
	sector.names = NULL, 
	major.by = NULL,
	plotType = c("axis", "labels"), 
	tickLabelsStartFromZero = TRUE,
	axis.labels.cex = 0.4*par("cex"), 
	labels.cex = 0.8*par("cex"), 
	track.height = NULL, 
	...) {

	data = validate_data_frame(data)
	validate_region(data, check_chr = FALSE)
	
	if(is.factor(data[[1]])) {
		fa = levels(data[[1]])
	} else {
		fa = unique(data[[1]])
	}
	
	if(!is.null(sector.names)) {
		if(length(sector.names) != length(fa)) {
			stop_wrap("length of `sector.names` and length of sectors differ.")
		}
	} else {
		sector.names = fa
	}
	
	names(sector.names) = fa
	
	# calculate xlim
	x1 = tapply(data[[2]], data[[1]], min)[fa]
	x2 = tapply(data[[3]], data[[1]], max)[fa]
	
	op = circos.par("cell.padding")
	ow = circos.par("points.overflow.warning")
	circos.par(cell.padding = c(0, 0, 0, 0), points.overflow.warning = FALSE)
	circos.initialize(factor(fa, levels = fa), xlim = cbind(x1, x2), ...)

	if(circos.par$ring) {
		op = c(op[1], 0, op[3], 0)
    	ow = FALSE
	}

	if(is.null(track.height)) {
		if(all(c("axis", "labels") %in% plotType)) {
			track.height = convert_unit_in_canvas_coordinate(1.5, "mm") + strheight("0", cex = axis.labels.cex) + 
				convert_unit_in_canvas_coordinate(0.5, "mm") + strheight("chr", cex = labels.cex)
		} else if("labels" %in% plotType) {
			track.height = strheight("chr", cex = labels.cex)
		} else if("axis" %in% plotType) {
			track.height = convert_unit_in_canvas_coordinate(1.5, "mm") + strheight("0", cex = axis.labels.cex)
		} else {
			track.height = convert_height(3, "mm")
		}
	}

	# axis and chromosome names
	if(any(plotType %in% c("axis", "labels"))) {
		circos.genomicTrackPlotRegion(data, ylim = c(0, 1), bg.border = NA, track.height = track.height,
			panel.fun = function(region, value, ...) {
				sector.index = get.cell.meta.data("sector.index")
				xlim = get.cell.meta.data("xlim")
				
				if(all(c("axis", "labels") %in% plotType)) {
					circos.genomicAxis(h = "bottom", major.by = major.by, tickLabelsStartFromZero = tickLabelsStartFromZero, labels.cex = axis.labels.cex)
					circos.text(mean(xlim), convert_y(1.5, "mm") + convert_y(strheight("chr", cex = axis.labels.cex), "canvas") + convert_y(0.5, "mm"), 
						labels = sector.names[sector.index], cex = labels.cex, adj = c(0.5, 0), niceFacing = TRUE)
				} else if("labels" %in% plotType) {
					circos.text(mean(xlim), 0, labels = sector.names[sector.index], cex = labels.cex, adj = c(0.5, 0), niceFacing = TRUE)
				} else if("axis" %in% plotType) {
					circos.genomicAxis(h = "bottom", major.by = major.by, tickLabelsStartFromZero = tickLabelsStartFromZero,labels.cex = axis.labels.cex)
				}
 			}
		)
	}
	
	circos.par("cell.padding" = op, "points.overflow.warning" = ow)
	return(invisible(NULL))
}


# == title
# Initialize a layout for circular genome
#
# == param
# -name Name of the genome (or the "chromosome name").
# -genome_size Size of the genome
# -plotType Pass to `circos.genomicInitialize`.
# -... All goes to `circos.genomicInitialize`.
#
circos.initializeCircularGenome = function(name, genome_size, plotType = "axis", ...) {
	circos.genomicInitialize(data.frame(name, 0, genome_size), ..., ring = TRUE, plotType = plotType)
} 
