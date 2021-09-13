# source: https://stackoverflow.com/questions/60663617/manually-edit-child-row-in-shiny-datatable#answer-60664739

library(DT)

df1 <- iris[1:3,]
df2 <- cars[1:4,]

# function to make the required dataframe
NestedData <- function(dat, children){
  stopifnot(length(children) == nrow(dat))
  g <- function(d){
    if(is.data.frame(d)){
      purrr::transpose(d)
    }else{
      purrr::transpose(NestedData(d[[1]], children = d$children))
    }
  }
  subdats <- lapply(children, g)
  oplus <- sapply(subdats, function(x) if(length(x)) "&oplus;" else "")
  cbind(" " = oplus, dat, "_details" = I(subdats), stringsAsFactors = FALSE)
}

# make the required dataframe
# one must have: length(children) == nrow(dat)
Dat <- NestedData(
  dat = df1, 
  children = list(df2, df2, df2)
)

## whether to show row names (set TRUE or FALSE)
rowNames <- FALSE
colIdx <- as.integer(rowNames)

## make the callback
parentRows <- which(Dat[,1] != "")
callback = JS(
  "function onUpdate(updatedCell, updatedRow, oldValue) {};",
  "table.MakeCellsEditable({",
  "  onUpdate: onUpdate,",
  "  confirmationButton: true",
  "});",
  sprintf("var parentRows = [%s];", toString(parentRows-1)),
  sprintf("var j0 = %d;", colIdx),
  "var nrows = table.rows().count();",
  "for(var i=0; i < nrows; ++i){",
  "  if(parentRows.indexOf(i) > -1){",
  "    table.cell(i,j0).nodes().to$().css({cursor: 'pointer'});",
  "  }else{",
  "    table.cell(i,j0).nodes().to$().removeClass('details-control');",
  "  }",
  "}",
  "",
  "// make the table header of the nested table",
  "var format = function(d, childId){",
  "  if(d != null){",
  "    var html = ", 
  "      '<table class=\"display compact hover\" ' + ",
  "      'style=\"padding-left: 30px;\" id=\"' + childId + '\"><thead><tr>';",
  "    for(var key in d[d.length-1][0]){",
  "      html += '<th>' + key + '</th>';",
  "    }",
  "    html += '</tr></thead></table>'",
  "    return html;",
  "  } else {",
  "    return '';",
  "  }",
  "};",
  "",
  "// row callback to style the rows of the child tables",
  "var rowCallback = function(row, dat, displayNum, index){",
  "  if($(row).hasClass('odd')){",
  "    $(row).css('background-color', 'papayawhip');",
  "    $(row).hover(function(){",
  "      $(this).css('background-color', '#E6FF99');",
  "    }, function() {",
  "      $(this).css('background-color', 'papayawhip');",
  "    });",
  "  } else {",
  "    $(row).css('background-color', 'lemonchiffon');",
  "    $(row).hover(function(){",
  "      $(this).css('background-color', '#DDFF75');",
  "    }, function() {",
  "      $(this).css('background-color', 'lemonchiffon');",
  "    });",
  "  }",
  "};",
  "",
  "// header callback to style the header of the child tables",
  "var headerCallback = function(thead, data, start, end, display){",
  "  $('th', thead).css({",
  "    'border-top': '3px solid indigo',", 
  "    'color': 'indigo',",
  "    'background-color': '#fadadd'",
  "  });",
  "};",
  "",
  "// make the datatable",
  "var format_datatable = function(d, childId){",
  "  var dataset = [];",
  "  var n = d.length - 1;",
  "  for(var i = 0; i < d[n].length; i++){",
  "    var datarow = $.map(d[n][i], function (value, index) {",
  "      return [value];",
  "    });",
  "    dataset.push(datarow);",
  "  }",
  "  var id = 'table#' + childId;",
  "  if (Object.keys(d[n][0]).indexOf('_details') === -1) {",
  "    var subtable = $(id).DataTable({",
  "                 'data': dataset,",
  "                 'autoWidth': true,",
  "                 'deferRender': true,",
  "                 'info': false,",
  "                 'lengthChange': false,",
  "                 'ordering': d[n].length > 1,",
  "                 'order': [],",
  "                 'paging': false,",
  "                 'scrollX': false,",
  "                 'scrollY': false,",
  "                 'searching': false,",
  "                 'sortClasses': false,",
  "                 'rowCallback': rowCallback,",
  "                 'headerCallback': headerCallback,",
  "                 'columnDefs': [{targets: '_all', className: 'dt-center'}]",
  "               });",
  "  } else {",
  "    var subtable = $(id).DataTable({",
  "            'data': dataset,",
  "            'autoWidth': true,",
  "            'deferRender': true,",
  "            'info': false,",
  "            'lengthChange': false,",
  "            'ordering': d[n].length > 1,",
  "            'order': [],",
  "            'paging': false,",
  "            'scrollX': false,",
  "            'scrollY': false,",
  "            'searching': false,",
  "            'sortClasses': false,",
  "            'rowCallback': rowCallback,",
  "            'headerCallback': headerCallback,",
  "            'columnDefs': [", 
  "              {targets: -1, visible: false},", 
  "              {targets: 0, orderable: false, className: 'details-control'},", 
  "              {targets: '_all', className: 'dt-center'}",
  "             ]",
  "          }).column(0).nodes().to$().css({cursor: 'pointer'});",
  "  }",
  "  subtable.MakeCellsEditable({",
  "    onUpdate: onUpdate,",
  "    confirmationButton: true",
  "  });",
  "};",
  "",
  "// display the child table on click",
  "table.on('click', 'td.details-control', function(){",
  "  var tbl = $(this).closest('table'),",
  "      tblId = tbl.attr('id'),",
  "      td = $(this),",
  "      row = $(tbl).DataTable().row(td.closest('tr')),",
  "      rowIdx = row.index();",
  "  if(row.child.isShown()){",
  "    row.child.hide();",
  "    td.html('&oplus;');",
  "  } else {",
  "    var childId = tblId + '-child-' + rowIdx;",
  "    row.child(format(row.data(), childId)).show();",
  "    td.html('&CircleMinus;');",
  "    format_datatable(row.data(), childId);",
  "  }",
  "});")

## the datatable
dtable <- datatable(
  Dat, callback = callback, rownames = rowNames, escape = -colIdx-1,
  options = list(
    columnDefs = list(
      list(visible = FALSE, targets = ncol(Dat)-1+colIdx),
      list(orderable = FALSE, className = 'details-control', targets = colIdx),
      list(className = "dt-center", targets = "_all")
    )
  )
)
path <- "/share/github/CellEdit/js" # folder containing the file dataTables.cellEdit.js
dep <- htmltools::htmlDependency(
  "CellEdit", "1.0.19", 
  path, script = "dataTables.cellEdit.js")
dtable$dependencies <- c(dtable$dependencies, list(dep))
dtable
