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

    var graphLabels = ["time","ALP"];
    var graphColours = [colourPalette.ALP];
    var graph;
    var graphOptions = {
        electorate: "AUS",
        partypov: "ALP",
        yaxis: "auto",
        type: "twopp"
    };


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
                errorBars: true,
                labels: ["time", "ALP"],
                title: getTitle(),
                ylabel: '(per cent)',
                legend: 'hideOverlayOnMouseOut',
                labelsDivStyles: {
                    'textAlign': 'right'
                },
                showRangeSelector: true,
                colors: [colourPalette.ALP]
            }
        );
    }

    function getDataForElectorate(electorate, callback) {
        if (graphOptions.type === "twopp") {
            getTwoppDataForElectorate(electorate, callback);
        } else {
            getPrimaryDataForElectorate(electorate, callback);
        }
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
                        graphData.forEach(function(e,i){
                            e.push(data[i][1]);
                        });
                    }
                    graphColours.push(colourPalette[party]);
                    graphLabels.push(party);
                    callback();
                    if(party === graphOptions.partypov) {
                        var seriesOption = {};
                        seriesOption[party] = {showInRangeSelector: true};
                        graph.updateOptions({
                            series: seriesOption
                        });
                    }
                }
            });
        });
    }

    function changeGraphToElectorate(electorate) {
        getDataForElectorate(electorate, function() {
            graph.updateOptions({
                file: graphData,
                title: getTitle(),
                colors: graphColours,
                labels: graphLabels
            });
        });
    }

    $(document).ready(function() {
        // TODO: read cookie
        getDataForElectorate('AUS', createGraph);
    });

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

    $("#twopp-party").change(function() {
        changeGraphOptions("partypov", $(this).val());
        if ($(this).val() === "LNP") {
            graph.updateOptions({
                file: graphData.map(function(v) {
                    return [v[0],
                        [100 - v[1][0], v[1][1]]
                    ];
                }),
                colors: [colourPalette.LNP],
                labels: ["time", "LNP"],
                title: getTitle()
            });
        } else {
            graph.updateOptions({
                file: graphData,
                colors: [colourPalette.ALP],
                title: getTitle(),
                labels: ["time", "ALP"]
            });
        }

    });

    $("#trend-graph-type").change(function() {
        changeGraphOptions("type",$(this).val());
        if($(this).val() === "twopp") {
            graphLabels = ["time", graphOptions.partypov];
            $("#twopp-party").prop('disabled', false);
        } else {
            $("#twopp-party").prop('disabled', true);
        }
        changeGraphToElectorate(graphOptions.electorate);
    });

//})();
