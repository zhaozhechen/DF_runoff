# Author: Zhaozhe Chen
# Update Date: 2026.7.27

# This code includes shared functions for figures and HTML reports

# Original Set2 palette used in the previous workflow
DF_colors <- RColorBrewer::brewer.pal(7,"Set2")

# Semantic colors used throughout the exploratory figures
DF_response_colors <- c(
  "Precipitation"=DF_colors[1],
  "Runoff"=DF_colors[7]
)

DF_frozen_colors <- c(
  "Frozen"=DF_colors[3],
  "Non-Frozen"=DF_colors[2]
)

DF_infiltration_colors <- c(
  "Slow-infiltration"=DF_colors[4],
  "Moderate-infiltration"=DF_colors[2],
  "High-infiltration"=DF_colors[1]
)

DF_tile_colors <- c(
  "No"=DF_colors[1],
  "Yes"=DF_colors[2]
)

# Theme for maps
DF_map_theme <- ggplot2::theme(
  panel.background=ggplot2::element_blank(),
  legend.key=ggplot2::element_blank(),
  legend.text=ggplot2::element_text(size=14),
  plot.title=ggplot2::element_text(size=17,face="bold"),
  plot.subtitle=ggplot2::element_text(size=14),
  axis.text=ggplot2::element_blank(),
  axis.title=ggplot2::element_blank(),
  axis.ticks=ggplot2::element_blank(),
  legend.position="right",
  legend.title=ggplot2::element_text(size=14),
  plot.margin=ggplot2::margin(8,8,8,8)
)

# Theme for non-map figures
DF_plot_theme <- ggplot2::theme(
  panel.border=ggplot2::element_rect(colour="black",fill=NA),
  axis.text=ggplot2::element_text(size=14,colour="black"),
  axis.title=ggplot2::element_text(size=15),
  axis.line=ggplot2::element_line(color="black"),
  panel.background=ggplot2::element_blank(),
  legend.key=ggplot2::element_blank(),
  legend.text=ggplot2::element_text(size=13),
  plot.title=ggplot2::element_text(size=16,face="bold"),
  plot.subtitle=ggplot2::element_text(size=13),
  legend.title=ggplot2::element_text(size=14),
  legend.background=ggplot2::element_blank(),
  strip.background=ggplot2::element_rect(fill="grey95",colour="black"),
  strip.text=ggplot2::element_text(size=14,face="bold")
)

# Save the same figure as a high-resolution PNG and a vector PDF
save_figure_pair <- function(plot,file_stem,width,height,dpi=400){
  ggplot2::ggsave(
    filename=paste0(file_stem,".png"),
    plot=plot,
    width=width,
    height=height,
    units="in",
    dpi=dpi,
    bg="white"
  )
  
  # Cairo can fail when a Windows project path contains non-ASCII characters
  # Write to a local temporary file first, then copy to the final path
  temporary_pdf <- tempfile(fileext=".pdf")
  on.exit(unlink(temporary_pdf),add=TRUE)
  
  ggplot2::ggsave(
    filename=temporary_pdf,
    plot=plot,
    width=width,
    height=height,
    units="in",
    device=grDevices::cairo_pdf,
    bg="white"
  )
  
  copied <- file.copy(
    temporary_pdf,
    paste0(file_stem,".pdf"),
    overwrite=TRUE
  )
  
  if(!copied){
    stop("Could not copy PDF figure to: ",paste0(file_stem,".pdf"))
  }
}

# Return the mean and a percentile bootstrap 95% confidence interval
bootstrap_mean_ci <- function(x,n_boot=2000){
  x <- x[is.finite(x)]
  
  if(length(x) == 0){
    return(c(mean=NA_real_,lower=NA_real_,upper=NA_real_,n=0))
  }
  
  if(length(x) == 1 || length(unique(x)) == 1){
    return(c(mean=mean(x),lower=mean(x),upper=mean(x),n=length(x)))
  }
  
  boot_mean <- replicate(
    n_boot,
    mean(sample(x,size=length(x),replace=TRUE))
  )
  
  c(
    mean=mean(x),
    lower=unname(stats::quantile(boot_mean,0.025)),
    upper=unname(stats::quantile(boot_mean,0.975)),
    n=length(x)
  )
}

# Escape text before adding it to an HTML report
html_escape <- function(x){
  x <- as.character(x)
  x <- gsub("&","&amp;",x,fixed=TRUE)
  x <- gsub("<","&lt;",x,fixed=TRUE)
  x <- gsub(">","&gt;",x,fixed=TRUE)
  x <- gsub("\"","&quot;",x,fixed=TRUE)
  x
}

# Convert a data frame to a compact HTML table
data_frame_to_html <- function(df,digits=2){
  if(nrow(df) == 0){
    return("<p><em>No records.</em></p>")
  }
  
  display_df <- df
  numeric_columns <- vapply(display_df,is.numeric,logical(1))
  display_df[numeric_columns] <- lapply(
    display_df[numeric_columns],
    function(x) format(round(x,digits),trim=TRUE,nsmall=0)
  )
  
  header <- paste0(
    "<tr>",
    paste0("<th>",html_escape(names(display_df)),"</th>",collapse=""),
    "</tr>"
  )
  
  rows <- apply(
    display_df,
    1,
    function(x){
      paste0(
        "<tr>",
        paste0("<td>",html_escape(x),"</td>",collapse=""),
        "</tr>"
      )
    }
  )
  
  paste0(
    "<div class=\"table-wrap\"><table>",
    "<thead>",header,"</thead>",
    "<tbody>",paste(rows,collapse=""),"</tbody>",
    "</table></div>"
  )
}

# Embed a local image as a data URI so the HTML report is self-contained
embedded_figure_html <- function(image_path,caption,alt=caption){
  if(!file.exists(image_path)){
    return(
      paste0(
        "<p class=\"warning\">Figure not found: ",
        html_escape(image_path),
        "</p>"
      )
    )
  }
  
  image_uri <- base64enc::dataURI(file=image_path,mime="image/png")
  
  paste0(
    "<figure><img src=\"",image_uri,"\" alt=\"",html_escape(alt),"\">",
    "<figcaption>",html_escape(caption),"</figcaption></figure>"
  )
}

# Write a self-contained HTML report
write_html_report <- function(title,body_html,output_path,subtitle=NULL){
  subtitle_html <- if(is.null(subtitle)){
    ""
  }else{
    paste0("<p class=\"subtitle\">",html_escape(subtitle),"</p>")
  }
  
  html <- paste0(
    "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<title>",html_escape(title),"</title>",
    "<style>",
    "body{font-family:Arial,Helvetica,sans-serif;color:#222;line-height:1.55;font-size:17px;",
    "max-width:1180px;margin:36px auto;padding:0 24px;background:#fff}",
    "h1{border-bottom:3px solid #66c2a5;padding-bottom:10px}",
    "h2{margin-top:34px;border-bottom:1px solid #ccc;padding-bottom:5px}",
    "h3{margin-top:26px}.subtitle{color:#555;margin-top:-8px}",
    ".callout{background:#f3f8f6;border-left:5px solid #66c2a5;",
    "padding:12px 16px;margin:18px 0}.warning{color:#9c3d10}",
    ".table-wrap{overflow-x:auto;margin:14px 0 24px}",
    "table{border-collapse:collapse;width:100%;font-size:0.92rem}",
    "th,td{border:1px solid #bbb;padding:7px 9px;text-align:left}",
    "th{background:#edf6f3}tr:nth-child(even){background:#fafafa}",
    "figure{margin:28px 0}img{max-width:100%;height:auto;border:1px solid #ddd}",
    "figcaption{font-size:0.9rem;color:#555;margin-top:7px}",
    "code{background:#f2f2f2;padding:1px 4px}",
    "</style></head><body>",
    "<h1>",html_escape(title),"</h1>",
    subtitle_html,
    paste(body_html,collapse="\n"),
    "</body></html>"
  )
  
  writeLines(html,output_path,useBytes=TRUE)
}
