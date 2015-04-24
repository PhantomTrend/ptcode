//(function() {

var graphData = [
    [new Date("2013-01-01"), [50, 1]]
];

var colourPalette = {
    "ALP": "#ec2929",
    "LNP": "#0037ff",
    "GRN": "#36ce32",
    "PUP": "#bb00ff",
    "OTH": "#d1e349"
};

var knownPollsters = ["Galaxy", "Ipsos", "Morgan", "EssentialOnline", "Newspoll",
    "Nielsen", "MorganSMS", "ReachTEL", "MorganMulti", "Essential",
    "Election", "NewspollQuarterly"];

var graphLabels = ["time", "ALP"];
var graphColours = [colourPalette.ALP];
var graphSeriesOptions = {"ALP": {"connectSeparatedPoints":true},
                            "LNP": {"connectSeparatedPoints":true},
                            "PUP": {"connectSeparatedPoints":true},
                            "GRN": {"connectSeparatedPoints":true},
                            "OTH": {"connectSeparatedPoints":true}    };
var graph;


var graphOptions = {
    electorate: "AUS",
    partypov: "ALP",
    yaxis: "auto",
    type: "twopp"
};


function splicePollsterDataIntoPlotData(newDataSeries) {
    var graphDataRow = 0;
    var nIntermediateCols = graphData[0].length - 1;
    for (var i = 0; i < newDataSeries.length; i++) {
        while (graphDataRow < graphData.length && graphData[graphDataRow][0] < newDataSeries[i][0]) {
            graphData[graphDataRow].push(null);
            graphDataRow++;
        }
        if (graphDataRow < graphData.length && graphData[graphDataRow][0] === newDataSeries[i][0]) {
            graphData[graphDataRow].push([newDataSeries[i][1],null]);
            graphDataRow++;
        } else {
            var newRow = [newDataSeries[i][0]];
            for (var p = 0; p < nIntermediateCols; p++) {
                newRow.push(null);
            }
            newRow.push([newDataSeries[i][1],null]);
            graphData.splice(graphDataRow, 0, newRow);
            graphDataRow++;
        }
    }
    while (graphDataRow < graphData.length){
        graphData[graphDataRow].push(null);
        graphDataRow++;
    }
}



function getTitle() {
    var electorateName = graphOptions.electorate;
    if (graphOptions.electorate === "AUS") {
        electorateName = "All of Australia";
    }
    var graphTypeString = "";
    if (graphOptions.type === "twopp") {
        graphTypeString = graphOptions.partypov + " \u2013 share of two-party preferred";
    } else {
        graphTypeString = "Primary vote shares";
    }
    return graphTypeString + " \u2013 " + electorateName;
}

function createGraph() {
    graph = new Dygraph(
        document.getElementById("trend-chart"),
        graphData, {
            dateWindow: [new Date("2013-01-01"), new Date("2015-06-01")],
            labels: graphLabels,
            errorBars: true,
            title: getTitle(),
            ylabel: '(per cent)',
            legend: 'hideOverlayOnMouseOut',
            labelsDivStyles: {
                'textAlign': 'right'
            },
            showRangeSelector: true,
            colors: graphColours,
            series: graphSeriesOptions
        }
    );
}

function refreshGraph() {
    graph.updateOptions({
        file: graphData,
        title: getTitle(),
        colors: graphColours,
        labels: graphLabels
    });
}

function getDataForElectorate(electorate, callback) {
    if (graphOptions.type === "twopp") {
        getTwoppDataForElectorate(electorate, callback);
    } else {
        getPrimaryDataForElectorate(electorate, callback);
    }
}

function getPollDataForElectorate(electorate, callback, party, pollster) {
    $.ajax({
        url: "http://" + PT_HOST + ":3001/polls?electorate=" + electorate + "&party=" + party + "&pollster=" + pollster,
        error: function(jqXHR, textStatus, errorThrown) {
            console.log(errorThrown);
        },
        success: function(data) {
            if(data.length < 2) {
                return;
            }
            thisPollData = data.map(function(v) {
                return [new Date(v[0]), v[2]];
            });
            splicePollsterDataIntoPlotData(thisPollData);
            graphColours.push("black");
            graphLabels.push(pollster + " " + (party === "ALP2pp" ? "2PP" : party));
            callback();
        }
    });
}

function getTwoppDataForElectorate(electorate, callback) {
    $.ajax({
        url: "http://" + PT_HOST + ":3001/twopp?electorate=" + electorate,
        error: function(jqXHR, textStatus, errorThrown) {
            console.log(errorThrown);
        },
        success: function(data) {
            graphData = data.map(function(v) {
                return [new Date(v[0]), v[1]];
            });
            callback();
        }
    });
}

function getPrimaryDataForElectorate(electorate, callback) {
    graphData = [];
    graphColours = [];
    graphLabels = ["time"];
    partyNames = ["ALP", "LNP", "GRN", "PUP", "OTH"];
    partyNames.map(function(party) {
        $.ajax({
            url: "http://" + PT_HOST + ":3001/primary?electorate=" + electorate + "&party=" + party,
            error: function(jqXHR, textStatus, errorThrown) {
                console.log(errorThrown);
            },
            success: function(data) {
                if (graphData.length === 0) {
                    graphData = data.map(function(v) {
                        return [new Date(v[0]), v[1]];
                    });
                } else {
                    graphData.forEach(function(e, i) {
                        e.push(data[i][1]);
                    });
                }
                graphColours.push(colourPalette[party]);
                graphLabels.push(party);
                callback();
                if (party === graphOptions.partypov) {
                    var seriesOption = {};
                    seriesOption[party] = {
                        showInRangeSelector: true
                    };
                    graph.updateOptions({
                        series: seriesOption
                    });
                }
            }
        });
    });
}

function changeGraphToElectorate(electorate) {
    getDataForElectorate(electorate, refreshGraph);
}

function changeGraphOptions(key, value) {
    graphOptions[key] = value;
    // TODO: save cookie
}

$("#trend-electorate-select").change(function() {
    changeGraphOptions("electorate", $(this).val());
    changeGraphToElectorate($(this).val());
});

$("#trend-yaxis-range").change(function() {
    if ($(this).val() === "auto") {
        graph.updateOptions({
            valueRange: [null, null]
        });
    } else if ($(this).val() === "fixed") {
        if (graphOptions.type === "twopp") {
            graph.updateOptions({
                valueRange: [35, 65]
            });
        } else {
            graph.updateOptions({
                valueRange: [0, 50]
            });
        }
    }
    changeGraphOptions("yaxis", $(this).val());
});


function invertTwoppPoints() {
    graphData = graphData.map(function(v) {
        return ([v[0], v[1] ? [100 - v[1][0], v[1][1]] : null ]
                .concat(v.slice(2).map(function(z){ return (z ? [(100 - z[0]),null] : null); })));
    });
}

$("#twopp-party").change(function() {
    changeGraphOptions("partypov", $(this).val());
    invertTwoppPoints();
    if ($(this).val() === "LNP") {
        graphColours[0] = colourPalette.LNP;
        graphLabels[1] = "LNP";
    } else {
        graphColours[0] = colourPalette.ALP;
        graphLabels[1] = "ALP";
    }
    refreshGraph();
});

$("#trend-graph-type").change(function() {
    changeGraphOptions("type", $(this).val());
    if ($(this).val() === "twopp") {
        graphLabels = ["time", graphOptions.partypov];
        $("#twopp-party").prop('disabled', false);
    } else {
        $("#twopp-party").prop('disabled', true);
    }
    changeGraphToElectorate(graphOptions.electorate);
});


$(document).ready(function() {
    // TODO: read cookie
    getDataForElectorate('AUS', function() {
        createGraph();
        knownPollsters.forEach(function(pollster){
            getPollDataForElectorate('AUS', refreshGraph, "ALP2pp", pollster);
        });
    });
});

//})();
