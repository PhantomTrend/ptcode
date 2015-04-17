(function(){

var graphData = [[new Date("2013-01-01"),[50,1]]];
var graph;

function createGraph() {
    graph = new Dygraph(
        document.getElementById("trend-chart"),
        graphData,
        {
          dateWindow: [new Date("2013-01-01"), new Date("2015-06-01")],
          errorBars: true,
          labels: ["time","ALP"],
          title: '',
          ylabel: '(%)',
          legend: 'always',
          labelsDivStyles: { 'textAlign': 'right' },
          showRangeSelector: true,
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
    getDataForElectorate('AUS', createGraph);
});

$("#electorate-select").change(function(){
    changeGraphToElectorate($(this).val());
});

})();
