(function(){

var graphData = [[new Date("2013-01-01"),[50,1]]];
var graph;
var graphOptions = {
    electorate: "AUS",
    partypov: "ALP",
    yaxis: "auto",
    type: "twopp"
};

var alpColour = '#ec2929';
var lnpColour = '#0037ff';

function createGraph() {
    graph = new Dygraph(
        document.getElementById("trend-chart"),
        graphData,
        {
          dateWindow: [new Date("2013-01-01"), new Date("2015-06-01")],
          errorBars: true,
          labels: ["time","ALP"],
          title: '',
          ylabel: '(per cent)',
          legend: 'hideOverlayOnMouseOut',
          labelsDivStyles: { 'textAlign': 'right' },
          showRangeSelector: true,
          color: alpColour
        }
    );
}

function getDataForElectorate(electorate, callback) {
    $.ajax({
        url: "http://localhost:3001/twopp?electorate=" + electorate,
        error: function (jqXHR, textStatus, errorThrown ) {
                console.log(errorThrown);
        },
        success: function (data) {
            graphData = data.map(function(v){
                return [new Date(v[0]), v[1]];
            });
            callback();
        }
    });
}

function changeGraphToElectorate(electorate) {
    getDataForElectorate(electorate, function() {
        graph.updateOptions({file: graphData});
    });
}

$( document ).ready(function() {
    // TODO: read cookie
    getDataForElectorate('AUS', createGraph);
});

function changeGraphOptions(key, value){
    graphOptions[key] = value;
    // TODO: save cookie
}

$("#trend-electorate-select").change(function(){
    changeGraphToElectorate($(this).val());
    changeGraphOptions("electorate", $(this).val());
});

$("#trend-yaxis-range").change(function(){
    if($(this).val() === "auto") {
        graph.updateOptions({valueRange: [null,null]});
    } else if($(this).val() === "fixed") {
        if(graphOptions.type === "twopp") {
            graph.updateOptions({valueRange: [35,65]});
        }else{
            graph.updateOptions({valueRange: [0,50]});
        }
    }
    changeGraphOptions("yaxis", $(this).val());
});

$("#twopp-party").change(function(){
    if($(this).val() === "LNP") {
        graph.updateOptions({file: graphData.map(function(v){
                                    return [v[0], [100-v[1][0],v[1][1]]];
                                }),
                            color: lnpColour,
                            labels: ["time", "LNP"]
        });
    } else {
        graph.updateOptions({file: graphData,
                             color: alpColour,
                             labels: ["time", "ALP"] });
    }
    changeGraphOptions("partypov", $(this).val());
});

})();
