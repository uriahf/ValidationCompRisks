// Constants and Configuration
const CONFIG = {
  margin: {
    top: 25,
    right: 20,
    bottom: 35,
    left: 60
  },
  colors: {
    truePositive: "#009e73",
    trueNegative: "#009e73",
    falsePositive: "#FAC8CD",
    falseNegative: "#FAC8CD",
    competing: "#9DB4C0",
    censored: "#E3F09B"
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

class RetentionVisualization {
  constructor(container, data, options) {
    this.container = container;
    this.data = data;
    this.options = options;
    this.width = options.width || 800;
    this.height = options.height || 600;
    
    this.initializeScales();
    this.createSVG();
    this.setupTooltip();
    this.setupZoom();
    this.createAxes();
    this.createVisualization();
  }

  initializeScales() {
    this.x = d3.scaleLinear()
      .domain([0, 1])
      .range([0, this.width]);

    this.stackLayout = d3.stack()
      .keys(['real_positives', 'real_negatives', 'real_competing', 'real_censored'])(this.data);

    this.y = d3.scaleLinear()
      .domain([0, d3.max(this.stackLayout, d => d3.max(d, d => d[1]))])
      .nice()
      .range([this.height, 0]);
  }

  createSVG() {
    this.svg = this.container.append("svg")
      .attr("width", this.width + CONFIG.margin.left + CONFIG.margin.right)
      .attr("height", this.height + CONFIG.margin.top + CONFIG.margin.bottom);

    this.g = this.svg.append("g")
      .attr("transform", `translate(${CONFIG.margin.left}, ${CONFIG.margin.top})`);
  }

  setupTooltip() {
    this.tooltip = d3.select("#" + this.options.outerDiv)
      .append("div")
      .attr("class", "tooltip")
      .style("opacity", 1)
      .style("position", "absolute")
      .style("border", "solid")
      .style("border-width", "1px")
      .style("border-radius", "5px")
      .style("padding", "10px");
  }

  setupZoom() {
    this.zoom = d3.zoom()
      .scaleExtent([1, 10])
      .translateExtent([[0, 0], [this.width, this.height]])
      .extent([[0, 0], [this.width, this.height]])
      .on("zoom", (event) => this.handleZoom(event));

    this.svg.on("dblclick", () => {
      this.svg.transition().duration(750).call(
        this.zoom.transform,
        d3.zoomIdentity
      );
    });

    this.g.call(this.zoom);
  }

  handleZoom(event) {
    const transform = event.transform;
    const newX = transform.rescaleX(this.x);
    
    this.updateRects(newX);
    this.updateAxes(newX);
    this.updateHoverEffects(newX);
  }

  updateRects(newX) {
    this.svg.selectAll("rect")
      .attr("x", d => newX(d.data.lower_bound))
      .attr("width", newX(this.options.by) - newX(0))
      .attr("y", d => this.y(d[1]))
      .attr("height", d => this.y(d[0]) - this.y(d[1]));
  }

  createAxes() {
    this.g.append("g")
      .attr("transform", `translate(0,${this.height})`)
      .attr("class", "x-axis")
      .call(d3.axisBottom(this.x));

    this.g.append("g")
      .attr("class", "y-axis")
      .call(d3.axisLeft(this.y));
  }

  createVisualization() {
    this.createBuckets();
    this.createVerticalLine();
  }

  createBuckets() {
    const bucketTypes = [
      { name: 'real-negatives', index: 1 },
      { name: 'real-positives', index: 0 },
      { name: 'real-competing', index: 2 },
      { name: 'real-censored', index: 3 }
    ];

    bucketTypes.forEach(bucket => {
      this.createBucket(bucket);
    });
  }

  createBucket({ name, index }) {
    this.g.append("g")
      .selectAll(`.bucket-${name}`)
      .data(this.stackLayout)
      .join("g")
      .attr("class", `bucket-${name}`)
      .filter((d, i) => i === index)
      .selectAll("rect")
      .data(d => d)
      .join("rect")
      .attr("x", d => this.x(d.data.lower_bound))
      .attr("y", d => this.y(d[1]))
      .attr("height", d => this.y(d[0]) - this.y(d[1]))
      .attr("width", this.x(this.options.by))
      .attr("class", (d) => this.getBucketClass(d, name))
      .attr("fill", (d) => this.getBucketFill(d, name))
      .on("mousemove", (ev, d) => this.handleMouseMove(ev, d, name))
      .on("mouseout", () => this.handleMouseOut());
  }

  getBucketClass(d, bucketType) {
    const threshold = Number(this.options.sliderValue);
    const isBeforeThreshold = this.x(d.data.lower_bound) < this.x(threshold);
    
    const classMap = {
      'real-negatives': isBeforeThreshold ? 'false-positives' : 'true-negatives',
      'real-positives': isBeforeThreshold ? 'false-negatives' : 'true-positives',
      'real-competing': isBeforeThreshold ? 'competing-positives' : 'competing-negatives',
      'real-censored': isBeforeThreshold ? 'censored-positives' : 'censored-negatives'
    };
    
    return classMap[bucketType];
  }

  updateVisualization(sliderValue) {
    this.updateVerticalLine(sliderValue);
    this.updateBucketColors(sliderValue);
  }

  updateVerticalLine(value) {
    const xPos = this.x(value);
    this.g.select(".verticalline")
      .attr("x1", xPos)
      .attr("x2", xPos);
  }
}

// Usage
const viz = new RetentionVisualization(div, data, options);