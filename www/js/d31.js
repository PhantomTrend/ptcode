
myDateFormat = d3.time.format("%Y-%m-%d").parse;

var margin = {top: 20, right: 20, bottom: 30, left: 40},
    width = 960 - margin.left - margin.right,
    height = 500 - margin.top - margin.bottom;

// setup x
var xValue = function(d) { return d.PollEndDate;}, // data -> value
    xScale = d3.time.scale()
                .range([0, width])
                .domain([myDateFormat("2013-01-01"),
                         myDateFormat("2015-03-31")]), // value -> display
    xMap = function(d) { return xScale(xValue(d));}, // data -> display
    xAxis = d3.svg.axis().scale(xScale).orient("bottom");
xScale.domain()


// setup y
var yValue = function(d) { return d.Vote;}, // data -> value
    yScale = d3.scale.linear()
                .range([height, 0])
                .domain([0,60]), // value -> display
    yMap = function(d) { return yScale(yValue(d));}, // data -> display
    yAxis = d3.svg.axis().scale(yScale).orient("left");


// add the graph canvas to the body of the webpage
var svg = d3.select("body").append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

// Fill colour
var cValue = function(d) { return d.Party;},
    color = d3.scale.category10();

d3.csv('data/TinyModelData.csv',
// Accessor
    function(d){
        if(isNaN(+d.Vote)){
            return null;
        }
        else{
        return {
                PollEndDate: myDateFormat(d.PollEndDate),
                Pollster: d.Pollster,
                Party: d.Party,
                Vote: +d.Vote,
                Electorate: d.Electorate
            }
        };
    },
// Callback
    function(error, rows){
        // x-axis
      svg.append("g")
          .attr("class", "x axis")
          .attr("transform", "translate(0," + height + ")")
          .call(xAxis);

      // y-axis
      svg.append("g")
          .attr("class", "y axis")
          .call(yAxis);

      // draw dots
      svg.selectAll(".dot")
          .data(rows)
        .enter().append("circle")
          .attr("class", "dot")
          .attr("r", 3.5)
          .attr("cx", xMap)
          .attr("cy", yMap)
          .style("fill", function(d) { return color(cValue(d));});
    }
);
