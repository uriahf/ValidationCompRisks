// !preview r2d3 data=data.frame(cat = rep(c('(0,0.1]', '(0.1,0.2]', '(0.2,0.3]', '(0.3,0.4]', '(0.4,0.5]', '(0.5,0.6]', '(0.6,0.7]', '(0.7,0.8]', '(0.8,0.9]', '(0.9,1]'), 2),values =  c(runif(20)), name = rep(c('real_positives', 'real_negatives'), each = 10)) |> tidyr::pivot_wider(names_from = "name", values_from = "values"), container = 'div', options = list(highlightMetrics = list("TP" = TRUE, "TN" = TRUE, "FP" = TRUE, "FN" = TRUE))
//
  // r2d3: https://rstudio.github.io/r2d3
//
  
  // TODO: add zoom
// DONE: fix tooltip - bold for censored
// DONE: add tooltip text for censored observations
// TODO: add stroke for all bins

//console.log("options.initial_zoom")
//console.log(options.initial_zoom)


//console.log("options.stratified_by")
//console.log(options.stratified_by)

console.log('data')
console.log(JSON.stringify(data))

console.log('data length')
console.log(data.length)

data.forEach((d) => {
  
  console.log('d')
  console.log(JSON.stringify(d))
  
  console.log('lower bound')
  console.log(d.lower_bound)
  
  d.lower_bound = parseFloat(d.lower_bound).toFixed(4);
  
  console.log('lower bound after parsing')
  console.log(d.lower_bound)
  
});

//const rpCheckbox = document.getElementById("reals_checkbox_"+
                                           //  options.confusionMatrixInputID + "_0");


//const rnCheckbox = document.getElementById("reals_checkbox_"+
                                         //    options.confusionMatrixInputID + "_1");

var confusionMatrixDiv = document.getElementById("confusion_matrix_for_display_probability_threshold_model");

var radioButton = document.getElementById("header_checkbox_confusion_matrix_for_display_probability_threshold_model_predicted_positives")





//console.log('get element')
//console.log(
  //  document.getElementById//("header_checkbox_confusion_matrix_for_display_probability_threshold_model_predicted_positives")
  //)




// TODO: SELECT BY name

//const inputToSelect = 'input[name="condition_checkbox-' +
  //options.confusionMatrixInputID + '"]'

const inputToSelect = 'input[name="condition_checkbox-confusionMatrixInput"]'

d3.selectAll(inputToSelect)
.on("change", function(event) {
  
  
  var radioButtonValue = event.currentTarget.value;
  
  if ( radioButtonValue === 'ao' ) {
    
    //console.log('all observations selected')
    
    var FNfillColor = "#FAC8CD";
    var FPfillColor = "#FAC8CD";
    var TNfillColor = "#009e73";
    var TPfillColor = "#009e73";
    
  } else if ( radioButtonValue === 'pn' ) {
    
    
    var FNfillColor = "#FAC8CD";
    var FPfillColor = "#FFF7F8";
    var TNfillColor = "#009e73";
    var TPfillColor = "#F4FFF0";
    
    
  } else if ( radioButtonValue === 'pp' ) {
    
    
    var FNfillColor = "#FFF7F8";
    var FPfillColor = "#FAC8CD";
    var TNfillColor = "#F4FFF0";
    var TPfillColor = "#009e73";
    
    
  } else if ( radioButtonValue === 'rp' ) {
    
    
    var FNfillColor = "#FAC8CD";
    var FPfillColor = "#FFF7F8";
    var TNfillColor = "#F4FFF0";
    var TPfillColor = "#009e73";
    
    
  } else if ( radioButtonValue === 'rn' ) {
    
    
    var FNfillColor = "#FFF7F8";
    var FPfillColor = "#FAC8CD";
    var TNfillColor = "#009e73";
    var TPfillColor = "#F4FFF0";
    
    
  }
  
  svg.selectAll(".false-negatives").attr('fill', FNfillColor);
  svg.selectAll(".true-negatives").attr('fill', TNfillColor);
  svg.selectAll(".false-positives").attr('fill', FPfillColor);
  svg.selectAll(".true-positives").attr('fill', TPfillColor);
  
});



function handleCheckboxChange() {
  var FNfillColor = (NCheckbox.checked || pnCheckbox.checked || rpCheckbox.checked) ? "#FAC8CD" : "#FFF7F8";
  svg.selectAll(".false-negatives").attr('fill', FNfillColor);
  
  var FPfillColor = (NCheckbox.checked || ppCheckbox.checked || rpCheckbox.checked) ? "#FAC8CD" : "#FFF7F8";
  svg.selectAll(".false-positives").attr('fill', FPfillColor);
  
  
  var TNfillColor = (NCheckbox.checked || pnCheckbox.checked ) ? "#19A119" : "#F4FFF0";
  svg.selectAll(".true-negatives").attr('fill', TNfillColor);
  
  var TPfillColor = (NCheckbox.checked || ppCheckbox.checked ) ? "#009e73" : "#F4FFF0";
  svg.selectAll(".true-positives").attr('fill', TPfillColor);
  
  
  
}


const timeHorizonSlider = document.getElementById("time_horizon_slider");

timeHorizonSlider.addEventListener("input", function() {
  
  //console.log('Time Horizon Chosen')
  //console.log(timeHorizonSlider.value)
  
});

const slidervaluetolisten = document.getElementById("cutoff_slider");

const stratifiedBy = document.getElementsByName('stratified_by');
  let selectedValue;
  for (const radioButton of stratifiedBy) {
    if (radioButton.checked) {
        selectedValue = radioButton.value;
        break;
    }
}
  //console.log('stratifiedBy')
  //console.log(stratifiedBy)
  
  //console.log('selectedValue')
  //console.log(selectedValue)

slidervaluetolisten.addEventListener("input", function() {
  

  const stratifiedBy = document.getElementsByName('stratified_by');
  let selectedValue;
  for (const radioButton of stratifiedBy) {
    if (radioButton.checked) {
        selectedValue = radioButton.value;
        break;
    }
}
  //console.log('stratifiedBy')
  //console.log(stratifiedBy)
  
  //console.log('selectedValue')
  //console.log(selectedValue)
  
  //console.log('The selected value is:', selectedValue);
  
  if ( selectedValue === "probability_threshold" ) {
    
    updatePlot(
      Number(slidervaluetolisten.value))
    
  } else if ( selectedValue === "ppcr" ) {
    
    
    updatePlot(
      1 - parseFloat(Number(slidervaluetolisten.value) + Number(options.by / 2)).toFixed(4))
    
  }      
  
});


const marginTop = 25;
const marginRight = 20;
const marginBottom = 35;
const marginLeft = 60;


const x2 = d3.scaleLinear()
.domain([0, 1])
.range([0, width])  


//console.log('options.outerDiv')
//console.log(options.outerDiv)


const tooltip = d3.select("#histPredictedDiv")
.append("div")
.attr("class", "tooltip")
.style("opacity", 1)
.style("position", "absolute")
.style("border", "solid")
.style("border-width", "1px")
.style("border-radius", "5px")
.style("padding", "10px")
//.style("opacity", 0);


//const tooltip = d3.select("#" + options.outerDiv)
//  .append("div")
//  .attr("class", "tooltip")
//  .style("opacity", 0)
//  .style("position", "absolute")
//  .style("background-color", "white")
//  .style("border", "solid")
//  .style("border-width", "1px")
//  .style("border-radius", "5px")
//  .style("padding", "10px")
//  .style("z-index", "10");





const keys = ['real_positives', 'real_negatives', 'real_competing', 'real_censored']

stackLayout = d3.stack().keys(keys)(data)

const y = d3
.scaleLinear()
.domain([0, d3.max(stackLayout, (d) => d3.max(d, (d) => d[1]))])
.nice()
.range([height, 0]);

const color = d3
.scaleOrdinal()
.domain(keys)
.range(d3.schemeCategory10);

var mouseleave = function() {
  tooltip.style("opacity", 0);
}

// TODO: fix mouseover for bold text

function mouseover(ev, d, tooltipcolor, predicted_as) {
  if (!d || !d.data) {
    console.error("Invalid data passed to mouseover:", d);
    return;
  }
  
  
  //console.log('Object.keys d')
  //console.log(Object.keys(d))
  
  //console.log('d[0]')
  //console.log(d['0'])
  
  //console.log('d[1]')
  //console.log(d['1'])
  
  // TODO: find generic way to identify the type of rect selected

  const styles = {
    realCompeting: "<span style='background-color:#DAB1E3;'>Real-Competing</span>: ",
    realCensored: "<span style='background-color:#E3F09B;'>Real-Censored</span>: ",
    realNegatives: {
      negatives: "<span style='background-color:lightgreen;'>TN</span>: ",
      positives: "<span style='background-color:pink;'>FP</span>: "
    },
    realPositives: {
      negatives: "<span style='background-color:pink;'>FN</span>: ",
      positives: "<span style='background-color:lightgreen;'>TP</span>: "
    }
  };

  let prefix_real_competing = styles.realCompeting;
  let prefix_real_censored = styles.realCensored;
  let prefix_real_negatives = styles.realNegatives[predicted_as] || '';
  let prefix_real_positives = styles.realPositives[predicted_as] || '';

  let real_censored_text = d.data.text_real_censored?.toString() || 'N/A';
  let real_competing_text = d.data.text_real_competing?.toString() || 'N/A';
  let real_positives_text = d.data.text_real_positives?.toString() || 'N/A';
  let real_negatives_text = d.data.text_real_negatives?.toString() || 'N/A';

  const boldText = (text) => "<b>" + text + "</b>";

  if (tooltipcolor === "#009e73") {
    real_positives_text = boldText(real_positives_text);
    if (predicted_as === "negatives") {
      real_negatives_text = boldText(real_negatives_text);
      prefix_real_negatives = boldText(prefix_real_negatives);
    } else {
      prefix_real_positives = boldText(prefix_real_positives);
    }
  } else if (tooltipcolor === "#FAC8CD") {
    if (predicted_as === "negatives") {
      real_positives_text = boldText(real_positives_text);
      prefix_real_positives = boldText(prefix_real_positives);
    } else {
      real_negatives_text = boldText(real_negatives_text);
      prefix_real_negatives = boldText(prefix_real_negatives);
    }
  } else if (tooltipcolor === "#9DB4C0") {
    real_competing_text = boldText(real_competing_text);
    prefix_real_competing = boldText(prefix_real_competing);
  } else if (tooltipcolor === "#E3F09B") {
    real_censored_text = boldText(real_censored_text);
    prefix_real_censored = boldText(prefix_real_censored);
  }

  tooltip
    .style("opacity", 1)
    .style("background-color", "#FFF");

  let tooltiptext = `<b>${d.data.text_range || 'N/A'}<br></b><br><br>${prefix_real_censored}${real_censored_text}<br>${prefix_real_competing}${real_competing_text}<br>${prefix_real_negatives}${real_negatives_text}<br>${prefix_real_positives}${real_positives_text}`;
  
  //console.log(ev.pageX)

  tooltip.html(tooltiptext)
    .style("opacity", 1)
    //.style("left", (ev.pageX + 10) + "px")
    //.style("top", (ev.pageY - 10) + "px");
}

var barHeight = Math.ceil(height / data.length);

const svg = div.append("svg")
.attr("width", width + marginLeft + marginRight)
.attr("height", height + marginTop + marginBottom)
//.style("border", "1px dotted #000");

const g = svg
.append("g")
.attr("transform", `translate(${marginLeft}, ${marginTop})`);


// Define the zoomed function
//function zoomed(event) {
//  var transform = event.transform;
//  var newX = transform.rescaleX(x2);
//  svg.selectAll(".area-path")
//  .attr("transform", transform);
//  svg.selectAll(".x-axis")
//  .call(d3.axisBottom(newX));
//}

//function zoomed(event) {
//  var transform = event.transform;
//  var newX = transform.rescaleX(x2);
//    svg.selectAll(".bucket-real-negatives rect, .bucket-real-positives rect, .bucket-real-competing rect, .bucket-real-censored rect")
//    .attr("x", function(d) { return newX(d.data.lower_bound); })
//    .attr("width", newX(options.by) - newX(0));
//  svg.select(".x-axis").call(d3.axisBottom(newX));
//}

function zoomed(event) {
  var transform = event.transform;
  var newX = transform.rescaleX(x2);
  var newY = y; 

  svg.selectAll(".bucket-real-negatives rect, .bucket-real-positives rect, .bucket-real-competing rect, .bucket-real-censored rect")
    .attr("x", function(d) {
      return newX(d.data.lower_bound);
    })
    .attr("width", newX(options.by) - newX(0))
    .attr("y", function(d) {
      return newY(d[1]);
    })
    .attr("height", function(d) {
      return newY(d[0]) - newY(d[1]);
    });

  svg.select(".x-axis").call(d3.axisBottom(newX));
  svg.select(".y-axis").call(d3.axisLeft(newY));

  updateHoverEffects(newX);
}


var zoom = d3.zoom()
  .scaleExtent([1, 10])
  .translateExtent([[0, 0], [width, height]])
  .extent([[0, 0], [width, height]])
  .on("zoom", zoomed);

function zoomend(event) {
  // Re-enable hover effects after zooming
  svg.selectAll("rect")
    .on("mousemove", function(ev, d) {
      // ... (existing mousemove code)
    });
}

//const zoom = d3.zoom()
//  .scaleExtent([1, 10])
//  .translateExtent([[-100, -100], [width + 90, height + 100]])
//  .extent([[0, 0], [width, height]])
//  .on("zoom", zoomed)
//  .on("end", zoomend);

svg.on("dblclick", function() {
  svg.transition().duration(750).call(
    zoom.transform,
    d3.zoomIdentity
  );
});

//g.append("rect")
//.attr("width", width)
//.attr("height", height)
//.style("fill", "none")
//.style("pointer-events", "all")
//.call(zoom);

g.append("g")
.selectAll(".bucket-real-negatives")
.data(stackLayout)
.join("g")
.attr("class", "bucket-real-negatives")
.filter(function(d, i) {
  //console.log('prints d')
  //console.log(d)
  //console.log('prints i')
  //console.log(i)
  
  return i == 1
})
.selectAll("rect")
.data((d) => d)
.join("rect")
.attr("x", function(d, i){
  
  //console.log('d.data object keys')
  //console.log(Object.keys(d.data))
  
  return x2(d.data.lower_bound)})
.attr("y", (d) => y(d[1]))
.attr("height", (d) => y(d[0]) - y(d[1]))
.attr("width", x2(options.by))
.attr("class", function(d,i) {
  if (x2(d.data.lower_bound) < x2(slidervaluetolisten) ) {
    return "false-positives"
  } else {
    return "true-negatives"
  }})
.attr('fill', function(d,i) {
  if (x2(d.data.lower_bound) < x2(1) ) {
    return "#009e73"
  } else {
    return "#FAC8CD"
  }})
.on("mousemove", function(ev, d) {
  d3.select(this)
  .style("stroke", "black")
  .style("stroke-width", "2")
  .on("mouseout", function() {
    
    d3.select(this)
    .style("stroke-width", "0")
    
    mouseleave()
    
  })
  
  if (x2(d.data.lower_bound) < x2(Number(slidervaluetolisten.value)) ) {
    return mouseover(ev, d, "#009e73", "negatives")
  } else {
    return mouseover(ev, d, "#FAC8CD", "positives")
  }
})

g.append("g")
.selectAll(".bucket-real-positives")
.data(stackLayout)
.join("g")
.attr("class", "bucket-real-positives")
.filter(function(d, i) {
  return i == 0
})
.selectAll("rect")
.data((d) => d)
.join("rect")
.attr("x", function(d, i){
  return x2(d.data.lower_bound)})
.attr("y", (d) => y(d[1]))
.attr("height", (d) => y(d[0]) - y(d[1]))
.attr("width", x2(options.by))
.attr("class", function(d,i) {
  if (x2(d.data.lower_bound) < x2(slidervaluetolisten) ) {
    return "false-positives"
  } else {
    return "true-negatives"
  }})
.attr('fill', function(d,i) {
  if (x2(d.data.lower_bound) < x2(1) ) {
    return "#FAC8CD"
  } else {
    return "#009e73"
  }
})
//.on("mouseover", (ev, d) => mouseover(ev, d))
.on("mousemove", function(ev, d) {
  d3.select(this)
  .style("stroke", "black")
  .style("stroke-width", "2")
  .on("mouseout", function() {
    
    d3.select(this)
    .style("stroke-width", "0")
    
    mouseleave()
    
  })
  
  if (x2(d.data.lower_bound) < x2(Number(slidervaluetolisten.value)) ) {
    return mouseover(ev, d, "#FAC8CD", "negatives")
  } else {
    return mouseover(ev, d, "#009e73", "positives")
  }
})

g.append("g")
.selectAll(".bucket-real-competing")
.data(stackLayout)
.join("g")
.attr("class", "bucket-real-competing")
.filter(function(d, i) {
  return i == 2
})
.selectAll("rect")
.data((d) => d)
.join("rect")
.attr("x", function(d, i){
  return x2(d.data.lower_bound)})
.attr("y", (d) => y(d[1]))
.attr("height", (d) => y(d[0]) - y(d[1]))
.attr("width", x2(options.by))
.attr("class", function(d,i) {
  if (x2(d.data.lower_bound) < x2(slidervaluetolisten) ) {
    return "competing-positives"
  } else {
    return "competing-negatives"
  }})
.attr('fill', function(d,i) {
  if (x2(d.data.lower_bound) < x2(1) ) {
    return "#B19BB6"
  } else {
    return "#9DB4C0"
  }})
.on("mousemove", function(ev, d) {
  d3.select(this)
  .style("stroke", "black")
  .style("stroke-width", "2")
  .on("mouseout", function() {
    
    d3.select(this)
    .style("stroke-width", "0")
    
    mouseleave()
    
  })
  
  if (x2(d.data.lower_bound) < x2(Number(slidervaluetolisten.value)) ) {
    return mouseover(ev, d, "#9DB4C0", "negatives")
  } else {
    return mouseover(ev, d, "#9DB4C0", "positives")
  }
})


g.append("g")
.selectAll(".bucket-real-censored")
.data(stackLayout)
.join("g")
.attr("class", "bucket-real-censored")
.filter(function(d, i) {
  return i == 3
})
.selectAll("rect")
.data((d) => d)
.join("rect")
.attr("x", function(d, i){
  return x2(d.data.lower_bound)})
.attr("y", (d) => y(d[1]))
.attr("height", (d) => y(d[0]) - y(d[1]))
.attr("width", x2(options.by))
.attr("class", function(d,i) {
  if (x2(d.data.lower_bound) < x2(slidervaluetolisten) ) {
    return "censored-positives"
  } else {
    return "censored-negatives"
  }})
.attr('fill', function(d,i) {
  if (x2(d.data.lower_bound) < x2(1) ) {
    return "#E3F09B"
  } else {
    return "#9DB4C0"
  }})
.on("mousemove", function(ev, d) {
  d3.select(this)
  .style("stroke", "black")
  .style("stroke-width", "2")
  .on("mouseout", function() {
    
    d3.select(this)
    .style("stroke-width", "0")
    
    mouseleave()
    
  })
  
  if (x2(d.data.lower_bound) < x2(Number(slidervaluetolisten.value)) ) {
    return mouseover(ev, d, "#E3F09B", "negatives")
  } else {
    return mouseover(ev, d, "#E3F09B", "positives")
  }
})



g.append("line")
.attr("class", "verticalline")
.attr("x1", function() {
  
  if ( stratifiedBy === "probability_threshold" ) {
    
    return x2(1)
    
  } else if ( stratifiedBy === "ppcr" ) {
    
    return x2(1 +  + Number(options.by) / 2)
    
  }
})  //<<== change your code here
.attr("y1", 0)
.attr("x2", function() {
  
  if ( stratifiedBy === "probability_threshold" ) {
    
    return x2(1)
    
  } else if ( stratifiedBy === "ppcr" ) {
    
    return x2(1 +  + Number(options.by) / 2)
    
  }
})  //<<== and here
.attr("y2", height )
.style("stroke-dasharray", ("3, 3"))  // <== This line here!!
  .style("stroke-width", 2)
.style("stroke", "black")
.style("fill", "none");  


function updatePlot(sliderelse) {
  
    const newX = x2.copy().domain(x2.range().map(d => d3.zoomTransform(svg.node()).invert(d)));
  
  
  g.selectAll(".verticalline")
  .attr("x1", x2(sliderelse))  //<<== change your code here
  .attr("x2", x2(sliderelse))  //<<== and here
  
  svg.selectAll(".bucket-real-negatives rect")
  .attr('fill', function(d,i) {
    
    
    if (x2(d.data.lower_bound) < x2(sliderelse) ) {
      return "#009e73"
    } else {
      return "#FAC8CD"
    }})
  .attr("class", function(d,i) {
    if (x2(d.data.lower_bound) < x2(sliderelse) ) {
      return "true-negatives"
    } else {
      return "false-positives"
    }})
  
  svg.selectAll(".bucket-real-positives rect")
  .attr('fill', function(d,i) {
    if (x2(d.data.lower_bound) < x2(sliderelse) ) {
      return "#FAC8CD"
    } else {
      return "#009e73"
    }
  })
  .attr("class", function(d,i) {
    if (x2(d.data.lower_bound) < x2(sliderelse) ) {
      return "false-negatives"
    } else {
      return "true-positives"
    }})
    
    updateHoverEffects(newX);
  
}



// Define the brush
//const brush = d3.brushX()
//.extent([[0, 0], [width, height]])
//.on("end", brushed);

// Define the brushed function
//function brushed(event) {
//  if (!event.selection) return;
//  const [x0, x1] = event.selection;
//  svg.transition().duration(750).call(
//    zoom.transform,
//    d3.zoomIdentity
//    .scale(width / (x1 - x0))
//    .translate(-x0, 0)
//  );
//  svg.select(".brush").call(brush.move, null); // Clear the brush selection
//}


function brushed(event) {
  if (!event.selection) return;
  const [x0, x1] = event.selection;
  svg.transition().duration(750).call(
    zoom.transform,
    d3.zoomIdentity
      .scale(width / (x1 - x0))
      .translate(-x0, 0)
  );
  svg.select(".brush").call(brush.move, null); // Clear the brush selection
}

// Append the brush to the SVG
//svg.append("g")
//.attr("class", "brush")
//.call(brush);

g.append("g")
  .attr("transform", `translate(0,${height})`)
  .attr("class", "x-axis")
  .call(d3.axisBottom(x2));

//g.append("g").call(d3.axisLeft(y));

g.append("g")
  .attr("class", "y-axis")
  .call(d3.axisLeft(y));


g.call(zoom);

// TODO-Zoom: apply .zoom() to vertical line as well
// TODO-Zoom: allow hover with zoom
// TODO-Zoom: add zoomend that resets to initial zoom state triggered by double-click
// TODO-Zoom: Initial state is defined the same way as it was defined in plotly histogram
// TODO-Zoom: disallow scrolling as an event


function updateHoverEffects(newX) {
  
  svg.selectAll(".bucket-real-negatives rect")
  //svg.selectAll("rect")
    .on("mousemove", function(ev, d) {
      d3.select(this)
        .style("stroke", "black")
        .style("stroke-width", "2")
        .on("mouseout", function() {
          d3.select(this)
            .style("stroke-width", "0");
          mouseleave();
        });
      if (newX(d.data.lower_bound) < newX(Number(slidervaluetolisten.value))) {
        return mouseover(ev, d, "#009e73", "negatives");
      } else {
        return mouseover(ev, d, "#FAC8CD", "positives");
      }
    });
}



return svg.node();