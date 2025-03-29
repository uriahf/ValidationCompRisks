// !preview r2d3 data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20)
//
// r2d3: https://rstudio.github.io/r2d3
//

const performance_data_ready_for_curve_treat_none = data.treat_none.performance_data_ready_for_curve;
const performance_data_ready_for_curve_treat_all = data.treat_all.performance_data_ready_for_curve;

const baselineStrategyRadioButton = document.getElementById("baseline_treatment_strategy_radiobutton_probability_threshold_train");
const radioButtonStrategyID = document.getElementById("baseline_treatment_strategy_radiobutton_probability_threshold_trainTreat None_");
const buttons = d3.selectAll('input[name="baseline_treatment_strategy_radiobutton_probability_threshold_train"]');

d3.selectAll(radioButtonStrategyID).on("change", function(event) {
  // Event listener for radio button change
});

function getYAxisXPosition() {
  return x(Math.max(x.domain()[0], Math.min(0, x.domain()[1])));
}

function getXAxisYPosition() {
  return y(Math.max(y.domain()[0], Math.min(0, y.domain()[1])));
}

const marginTop = 25;
const marginRight = 20;
const marginBottom = 35;
const marginLeft = 30;

const groupedDataTreatNone = d3.group(data.treat_none.reference_data, d => d.reference_group);
const groupedDataTreatAll = d3.group(data.treat_all.reference_data, d => d.reference_group);



const slidervaluetolisten = document.getElementById(options.listenTO);

slidervaluetolisten.addEventListener("input", function() {
  updatePlot(Number(slidervaluetolisten.value), currentX, currentY, currentData);
  updateInteractiveMarker(Number(slidervaluetolisten.value), currentX, currentY, currentData);
});


const svg = div.append("svg")
  .attr("width", width + marginLeft + marginRight)
  .attr("height", height + marginTop + marginBottom)
  .append("g")
  .attr("transform", `translate(${marginLeft},${marginTop})`);

var x = d3.scaleLinear()
  .domain(options.initial_zoom ? options.initial_zoom.treat_none.xaxis : [0, 1])
  .range([0, width]);

var y = d3.scaleLinear()
  .domain(options.initial_zoom ? options.initial_zoom.treat_none.yaxis : [0, 1])
  .range([height, 0]);

var x_treat_all = d3.scaleLinear()
  .domain(options.initial_zoom ? options.initial_zoom.treat_all.xaxis : [0, 1])
  .range([0, width]);

var y_treat_all = d3.scaleLinear()
  .domain(options.initial_zoom ? options.initial_zoom.treat_all.yaxis : [0, 1])
  .range([height, 0]);


const yAxis = svg.append("g")
  .attr("class", "y-axis")
  .attr("transform", `translate(${getYAxisXPosition()},0)`)
  .call(d3.axisLeft(y));

const xAxis = svg.append("g")
  .attr("class", "x-axis")
  .attr("transform", `translate(0,${getXAxisYPosition()})`)
  .call(d3.axisBottom(x));

function updateAxes() {
  svg.select(".x-axis").call(d3.axisBottom(x));
  yAxis.attr("transform", `translate(${getYAxisXPosition()},0)`).call(d3.axisLeft(y));
}

let currentX = x;
let currentY = y;
let currentData = performance_data_ready_for_curve_treat_none;
let currentGroupedData = groupedDataTreatNone;

updateAxes();

svg.append("text")
  .attr("class", "x-axis-label")
  .attr("text-anchor", "middle")
  .attr("x", width / 2)
  .attr("y", height + marginBottom + 10)
  .text(options.axes_labels.xaxis);

svg.append("path")
  .attr("class", "decision-curve-path")
  .datum(performance_data_ready_for_curve_treat_none)
  .attr("fill", "none")
  .attr("stroke", "black")
  .attr("stroke-width", 2)
  .attr("d", d3.line()
    .x(function(d) { return x(d.x); })
    .y(function(d) { return y(d.y); })
  );

groupedDataTreatNone.forEach((groupData, groupName) => {
  svg.append("path")
    .attr("class", "decision-curve-path-reference")
    .attr("id", `reference-${groupName}`)
    .datum(groupData)
    .attr("fill", "none")
    .attr("stroke", "gray")
    .attr("stroke-width", 2)
    .attr("stroke-dasharray", "5,5")
    .attr("d", d3.line()
      .x(d => x(d.x))
      .y(d => y(d.y))
    );
});

svg.append('g')
  .attr("class", "dots-container")
  .selectAll("dot")
  .data(performance_data_ready_for_curve_treat_none)
  .enter()
  .append("circle")
    .attr("class", "decision-curve-dots")
    .attr("cx", function (d) { return x(d.x); })
    .attr("cy", function (d) { return y(d.y); })
    .attr("r", 3)
    .style("fill", "black");
    
function updateChart() {
  // Update axes
  svg.select(".x-axis")
    .attr("transform", `translate(0,${currentY(0)})`)
    .call(d3.axisBottom(currentX));
  
  svg.select(".y-axis")
    .attr("transform", `translate(${currentX(0)},0)`)
    .call(d3.axisLeft(currentY));

  // Update path
  svg.selectAll(".decision-curve-path")
    .datum(currentData)
    .attr("d", d3.line()
      .x(function(d) { return currentX(d.x); })
      .y(function(d) { return currentY(d.y); })
    );
      
  // Update dots
  svg.select(".dots-container")
    .selectAll(".decision-curve-dots")
    .data(currentData)
    .join(
      enter => enter.append("circle")
        .attr("class", "decision-curve-dots")
        .attr("r", 3)
        .style("fill", "black"),
      update => update,
      exit => exit.remove()
    )
    .attr("cx", d => currentX(d.x))
    .attr("cy", d => currentY(d.y));

  // Update reference curves
  currentGroupedData.forEach((groupData, groupName) => {
    svg.select(`#reference-${groupName}`)
      .datum(groupData)
      .attr("d", d3.line()
        .x(d => currentX(d.x))
        .y(d => currentY(d.y))
      );
  });
  
  // Update interactive marker
  updateInteractiveMarker(Number(slidervaluetolisten.value), currentX, currentY, currentData);
}

buttons.on('change', function() {
  if (this.value === 'treat_none') {
    currentData = performance_data_ready_for_curve_treat_none;
    currentX = x;
    currentY = y;
    currentGroupedData = groupedDataTreatNone;
  } else {
    currentData = performance_data_ready_for_curve_treat_all;
    currentX = x_treat_all;
    currentY = y_treat_all;
    currentGroupedData = groupedDataTreatAll;
  }
  
  updateChart();
});

svg.append("circle")
  .attr("class", "interactiveMarker")
  .attr("r", 6)
  .style("fill", "#f6e3be")
  .attr('stroke', 'black')
  .attr("stroke-width", 3.5);      
    
function updatePlot(sliderValue, currentX, currentY, currentData) {
  let matchingPoint = currentData.find(d => d.stratified_by === sliderValue);

  if (matchingPoint) {
    svg.selectAll(".interactiveMarker")
      .attr("cx", currentX(matchingPoint.x))
      .attr("cy", currentY(matchingPoint.y));
  }
}

// New function to update the interactive marker
function updateInteractiveMarker(sliderValue, currentX, currentY, currentData) {
  let matchingPoint = currentData.find(d => d.stratified_by === sliderValue);

  if (matchingPoint) {
    svg.selectAll(".interactiveMarker")
      .attr("cx", currentX(matchingPoint.x))
      .attr("cy", currentY(matchingPoint.y));
  }
}

updateChart();
