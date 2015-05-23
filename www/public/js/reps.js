var RepsResultText = React.createClass({displayName: "RepsResultText",
    render: function(){
        var countSeats = (function(p){
            return(Object.keys(this.props.resultList)
                .filter(function(x) {
                    return houseResults[x].winner == p;
                })
                .length);
        }).bind(this);
        var nAlpSeats = countSeats("ALP");
        var nLnpSeats = countSeats("LNP");
        var nGrnSeats = countSeats("GRN");
        var nPupSeats = countSeats("PUP");
        var nOthSeats = countSeats("OTH");
        return(
            React.createElement("div", {class: "repsforecasttext"}, 
                React.createElement("h4", null, "Current Forecast Summary"), 
                "The model's best guess sees the LNP government winning ", React.createElement("span", {class: "lnprepsnumber"}, nLnpSeats), "," + ' ' +
                "the ALP ", React.createElement("span", {class: "alprepsnumber"}, nAlpSeats), "," + ' ' +
                "the Greens ", React.createElement("span", {class: "grnrepsnumber"}, nGrnSeats), "," + ' ' +
                "Palmer United with ", React.createElement("span", {class: "puprepsnumber"}, nPupSeats), " and" + ' ' +
                "Independents/KAP/Others winning ", React.createElement("span", {class: "othrepsnumber"}, nOthSeats), "."
            )
        );
    }
});

var RepsSeatList = React.createClass({displayName: "RepsSeatList",
    render: function() {
        var seatNodes = [];
        for(var seatName in this.props.data){
            seatNodes.push( React.createElement(RepsSeat, {data: this.props.data[seatName]}) );
        }
        return (
            React.createElement("div", {className: "repsSeatList"}, 
                seatNodes
            )
        );
    }
});

var RepsSeat = React.createClass({displayName: "RepsSeat",
    render: function() {
        var d = this.props.data;
        return(
            React.createElement("div", {class: "repsSeatTile"}, 
                React.createElement("div", {class: "seatName"}, d.name), 
                React.createElement("div", {class: "seatDescription"}, d.description)
            )
        );
    }
});

var RepsSection = React.createClass({displayName: "RepsSection",
    render: function(){
        return(
            React.createElement("div", null, 
            React.createElement(RepsResultText, {resultList: this.props.data}), 
            React.createElement(RepsSeatList, {data: this.props.data})
            )
        );
    }
});

React.render(React.createElement(RepsSection, {data: houseResults}), document.getElementById('repsApp'));
