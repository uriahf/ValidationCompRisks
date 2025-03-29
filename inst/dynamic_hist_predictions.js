//
// r2d3: https://rstudio.github.io/r2d3
//

//console.log('data')
//console.log(JSON.stringify(data))

const CONFIG = {
  colors: {
    positives: "#009e73",
    negatives: "#FAC8CD",
    competing: "#9DB4C0",
    censored: "#E3F09B",
    truePositives: "#009e73",
    trueNegatives: "#009e73",
    falsePositives: "#FAC8CD",
    falseNegatives: "#FAC8CD"
  },
  transitions: { 
    duration: 750
  },
  tooltipStyles: {
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
  }
};

const updateRectangles = (svg, selector, real_type, slideValue, x2) => { // TODO: fix this
  const getRectangleColor = (d, slideValue, x2, real_type) => {
    const isBeforeSlider = x2(d.data.lower_bound) < x2(slideValue);
    if (real_type == 'real_negatives') {
    return isBeforeSlider ? CONFIG.colors.trueNegatives: CONFIG.colors.falsePositives;
    } else if (real_type == 'real_positives') {
    return isBeforeSlider ? CONFIG.colors.falseNegatives: CONFIG.colors.truePositives;
    } else if (real_type == 'real_censored') {
      return CONFIG.colors.censored
    } else if (real_type == 'real_competing') {
      return CONFIG.colors.competing
    }
  };

  const getRectangleClass = (d, slideValue, x2, real_type) => {
  const isBeforeSlider = x2(d.data.lower_bound) < x2(slideValue);
  
  if (real_type === 'real-negatives') {
    return isBeforeSlider ? "true-negatives" : "false-positives";
  } else if (real_type === 'real-positives') {
    return isBeforeSlider ? "false-negatives" : "true-positives";
  }
};
  
  svg.selectAll(selector)
    .attr('fill', d => getRectangleColor(d, slideValue, x2, real_type))
    .attr("class", d => getRectangleClass(d, slideValue, x2));
};

const createVerticalLine = (svg, width, height, stratifiedByValue, options, sliderValue) => {
  const x2 = d3.scaleLinear()
    .domain([0, 1])
    .range([0, width]);


  const getLinePosition = (stratifiedByValue, options) => {
    if (stratifiedByValue === "probability_threshold") {
      return x2(sliderValue);
    } else if (stratifiedByValue === "ppcr") {
      return x2(sliderValue + Number(options.by) / 2);
    }
    return x2(sliderValue); // default position
  };

  svg.append("line")
    .attr("class", "verticalline")
    .attr("x1", () => getLinePosition(stratifiedByValue, options))
    .attr("y1", 0)
    .attr("x2", () => getLinePosition(stratifiedByValue, options))
    .attr("y2", height)
    .style("stroke-dasharray", "3, 3")
    .style("stroke-width", 2)
    .style("stroke", "black")
    .style("fill", "none");
};

const getStratifiedByValue = () => {
  const radioButtons = document.getElementsByName('stratified_by');
  return Array.from(radioButtons).find(radio => radio.checked)?.value;
};

const getReferenceGroupValue = () => {
  const radioButtons = document.getElementsByName('reference_group_radiobutton_');
  return Array.from(radioButtons).find(radio => radio.checked)?.value;
};


const initialStratifiedBy = getStratifiedByValue();
const initialReferenceGroupValue = getReferenceGroupValue()


const preprocessData = data => 
  data.map(d => ({
    ...d,
    lower_bound: parseFloat(d.lower_bound).toFixed(4)
  }));
  
const createStackLayout = data => {
  const keys = ['real_positives', 'real_negatives', 'real_competing', 'real_censored'];
  return d3.stack().keys(keys)(data);
};

const updateVisualization = (svg, width, height, options) => (selectedIndex, stratifiedByValue, ReferenceGroupValue, cutOffValue) => {
  
  console.log('ReferenceGroupValue')
  console.log(ReferenceGroupValue)
  
  const selectedData = preprocessData(data[ReferenceGroupValue][stratifiedByValue][selectedIndex]);
  const stackLayout = createStackLayout(selectedData);
  const scales = createScales(width, height, stackLayout);
  const tooltip = createTooltip();

  const groups = [
    { key: 'truePositives', className: 'true-positives' },
    { key: 'real_negatives', className: 'true-negatives' },
    { key: 'real_negatives', className: 'false-positives' },
    { key: 'falseNegatives', className: 'false-negatives' },
    { key: 'real_negatives', className: 'bucket-real-negatives' },
    { key: 'real_positives', className: 'bucket-real-positives' },
    { key: 'real_competing', className: 'bucket-real-competing' },
    { key: 'real_censored', className: 'bucket-real-censored' }
  ];

  groups.forEach(group => 
    updateGroup(svg, group, stackLayout, scales, options, tooltip, cutOffValue)
  );
  
  //createVerticalLine(svg, width, height, stratifiedByValue, options);
  
};

const setupEventListeners = (svg, width, height, options) => {
  const update = updateVisualization(svg, width, height, options);
  
  const timeHorizonSlider = document.getElementById("time_horizon_slider");
  const cutoffSlider = document.getElementById("cutoff_slider");

  timeHorizonSlider.addEventListener("input", () => 
    update(timeHorizonSlider.value, getStratifiedByValue(), getReferenceGroupValue(), cutoffSlider.value)
  );

  cutoffSlider.addEventListener("input", () => {
    const selectedValue = getStratifiedByValue();
    const newValue = selectedValue === "probability_threshold" 
      ? Number(cutoffSlider.value)
      : 1 - parseFloat(Number(cutoffSlider.value) + Number(options.by / 2)).toFixed(4);
    
    updatePlot(svg, width, newValue, getReferenceGroupValue());
  });

  document.getElementsByName('stratified_by').forEach(radio => {
    radio.addEventListener('click', () => 
      update(timeHorizonSlider.value, radio.value, getReferenceGroupValue(), cutoffSlider.value)
    );
  });
  
  document.getElementsByName('reference_group_radiobutton_').forEach(radio => {
    radio.addEventListener('click', () => 
      update(timeHorizonSlider.value, getStratifiedByValue(), getReferenceGroupValue(), cutoffSlider.value)
    );
  });
  
};

//setupEventListeners(svg, width, height, options);

const createScales = (width, height, stackLayout) => ({
  x: d3.scaleLinear()
    .domain([0, 1])
    .range([0, width]),
  y: d3.scaleLinear()
    .domain([0, d3.max(stackLayout, d => d3.max(d, d => d[1]))])
    .nice()
    .range([height, 0])
});

const createTooltip = () => 
  d3.select("body")
    .append("div")
    .attr("class", "tooltip")
    .style("opacity", 1)
    .style("position", "absolute")
    .style("border", "solid")
    .style("border-width", "1px")
    .style("border-radius", "5px")
    .style("padding", "10px");

const x2 = d3.scaleLinear()
.domain([0, 1])
.range([0, width]) 







const cutoffSliderValue = document.getElementById("cutoff_slider");

const createMouseOverHandler = (tooltip, tooltipcolor, predicted_as) => (event, d) => {
  if (!d?.data) return;

  const createTooltipText = (d, tooltipcolor, predicted_as) => {
    const boldText = text => `<b>${text}</b>`;
    const getPrefix = (type, predicted_as) => CONFIG.tooltipStyles[type][predicted_as] || '';

    const data = {
      censored: d.data.text_real_censored?.toString() || 'N/A',
      competing: d.data.text_real_competing?.toString() || 'N/A',
      positives: d.data.text_real_positives?.toString() || 'N/A',
      negatives: d.data.text_real_negatives?.toString() || 'N/A'
    };

    // Format text based on tooltipcolor and predicted_as
    // ... (rest of tooltip text formatting logic)
    
    return `<b>${d.data.text_range || 'N/A'}<br></b><br><br>
            ${getPrefix('realCensored')}${data.censored}<br>
            ${getPrefix('realCompeting')}${data.competing}<br>
            ${getPrefix('realNegatives', predicted_as)}${data.negatives}<br>
            ${getPrefix('realPositives', predicted_as)}${data.positives}`;
  };

  tooltip
    .style("opacity", 1)
    .style("background-color", "#FFF")
    .html(createTooltipText(d, tooltipcolor, predicted_as));
};

const handleMouseMove = (tooltip) => (event) => {
  tooltip
    .style("left", (event.pageX + 10) + "px")
    .style("top", (event.pageY - 28) + "px");
};

const handleMouseOut = (tooltip) => () => {
  tooltip.style("opacity", 0);
};

cutoffSliderValue.addEventListener("input", function() {
  

  const selectedValue = getStratifiedByValue()
  
  if ( selectedValue === "probability_threshold" ) {
    
    updatePlot(
      svg, width, Number(cutoffSliderValue.value))
    
  } else if ( selectedValue === "ppcr" ) {
    
    
    updatePlot(
      svg, width, 1 - parseFloat(Number(cutoffSliderValue.value) + Number(options.by / 2)).toFixed(4))
    
  }      
  
});

const updatePlot = (svg, width, slideValue) => {
  
  const x2 = d3.scaleLinear()
    .domain([0, 1])
    .range([0, width]);
    
  svg.selectAll(".verticalline")
  .attr("x1", x2(slideValue))  //<<== change your code here
  .attr("x2", x2(slideValue))  //<<== and here
  
  updateRectangles(svg, ".bucket-real-negatives rect", "real_negatives", slideValue, x2);
  updateRectangles(svg, ".bucket-real-positives rect", "real_positives", slideValue, x2);
    
}




const updateGroup = (svg, groupConfig, stackLayout, scales, options, tooltip, cutOffValue) => {
  
    const x2 = d3.scaleLinear()
    .domain([0, 1])
    .range([0, width]);
  
  const getRectangleColor = (d, slideValue, x2, real_type) => {
    const isBeforeSlider = x2(d.data.lower_bound) < x2(slideValue);
    if (real_type == 'real_negatives') {
    return isBeforeSlider ? CONFIG.colors.trueNegatives: CONFIG.colors.falsePositives;
    } else if (real_type == 'real_positives') {
    return isBeforeSlider ? CONFIG.colors.falseNegatives: CONFIG.colors.truePositives;
    } else if (real_type == 'real_censored') {
      return CONFIG.colors.censored
    } else if (real_type == 'real_competing') {
      return CONFIG.colors.competing
    }
  };

  
  console.log('groupConfig.key')
  console.log(groupConfig.key)
  
  const group = svg.selectAll(`.${groupConfig.className}`)
    .data(stackLayout.filter(d => d.key === groupConfig.key));

  console.log('groupConfig.key.split(_)[1]')
  console.log(groupConfig.key.split('_')[1])


  const groupEnter = group.enter()
    .append("g")
    .attr("class", groupConfig.className);

  const rects = group.merge(groupEnter)
    .selectAll("rect")
    .data(d => d);

  const rectsEnter = rects.enter()
    .append("rect");
    
    //const real_type = "real-negatives"

  rects.merge(rectsEnter)
    .transition()
    //.duration(CONFIG.transition.duration)
    .attr("x", d => scales.x(d.data.lower_bound))
    .attr("y", d => scales.y(d[1]))
    .attr("height", d => scales.y(d[0]) - scales.y(d[1]))
    .attr("width", scales.x(options.by))
    //.attr("fill", d => CONFIG.colors[groupConfig.key.split('_')[1]]);
    .attr("fill", d => getRectangleColor(d, cutOffValue, x2, groupConfig.key)) //TODO: use this

  // Add event handlers
  rects.merge(rectsEnter)
    .on("mouseover", createMouseOverHandler(tooltip, CONFIG.colors[groupConfig.key.split('_')[1]], "positives"))
    .on("mousemove", handleMouseMove(tooltip))
    .on("mouseout", handleMouseOut(tooltip));

  rects.exit().remove();
};




//createVerticalLine(svg, width, height, initialStratifiedBy, options);


//updateVisualization(svg, width, height, options);

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


const initVisualization = (svg, width, height, options) => {
  const tooltip = createTooltip();
  const initialStratifiedBy = getStratifiedByValue();
  const initialReferenceGroup = getReferenceGroupValue()
  
  console.log('initialReferenceGroup on initviz')
  console.log(initialReferenceGroup)
  
  const cutoffSlider = document.getElementById("cutoff_slider");
  
  const newValue = initialStratifiedBy === "probability_threshold" 
      ? Number(cutoffSlider.value)
      : 1 - parseFloat(Number(cutoffSlider.value) + Number(options.by / 2)).toFixed(4);
  
  setupEventListeners(svg, width, height, options);
  
  // Create the vertical line first
  createVerticalLine(svg, width, height, initialStratifiedBy, options, newValue);
  
  updateVisualization(svg, width, height, options)(3, initialStratifiedBy, initialReferenceGroup, cutoffSlider.value);
  
};

initVisualization(svg, width, height, options);
