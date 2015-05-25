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
                React.createElement("h4", null, "Summary"), 
                "The model's best guess sees the LNP government winning ", React.createElement("span", {className: "repsSummaryNumber"}, nLnpSeats), "," + ' ' +
                "the ALP ", React.createElement("span", {className: "repsSummaryNumber"}, nAlpSeats), "," + ' ' +
                "the Greens ", React.createElement("span", {className: "repsSummaryNumber"}, nGrnSeats), "," + ' ' +
                "Palmer United with ", React.createElement("span", {className: "repsSummaryNumber"}, nPupSeats), " and" + ' ' +
                "Independents winning ", React.createElement("span", {className: "repsSummaryNumber"}, nOthSeats), "."
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

var RepsSeatHeaderRow = React.createClass({displayName: "RepsSeatHeaderRow",
    render: function() {
        return(
            React.createElement("div", {className: "seatHeaderRow"}, 
                React.createElement("div", null, this.props.data.name), 
                React.createElement("div", null, this.props.data.description)
            )
        );
    }
});

var RepsSeatIncumbentRow = React.createClass({displayName: "RepsSeatIncumbentRow",
    render: function() {
        return(
            React.createElement("div", {className: "seatIncumbentRow"}, 
                React.createElement("div", {className: "seatState"}, this.props.data.state), 
                React.createElement("div", {className: "seatIncumbentName"}, this.props.data.member), 
                React.createElement("div", {className: "seatIncumbentParty"}, this.props.data.incumbentparty)
            )
        );
    }
});

function roundVote(v){
    return Math.round(v*10)/10;
}

var RepsSeatTwoppRow = React.createClass({displayName: "RepsSeatTwoppRow",
    render: function() {
        return(
            React.createElement("div", {className: "seatTwoPPRow"}, 
                React.createElement("div", {className: "seat2pp LNP"}, roundVote(this.props.data.lnp2pp)), 
                React.createElement("div", {className: "seat2pp ALP"}, roundVote(this.props.data.alp2pp))
            )
        );
    }
});

var RepsSeatPrimaryRow = React.createClass({displayName: "RepsSeatPrimaryRow",
    render: function() {
        return(
            React.createElement("div", {className: "seatPrimaryRow"}, 
                React.createElement("div", {className: "seatPrimary LNP"}, roundVote(this.props.data.lnpprimary)), 
                React.createElement("div", {className: "seatPrimary ALP"}, roundVote(this.props.data.alpprimary)), 
                React.createElement("div", {className: "seatPrimary GRN"}, roundVote(this.props.data.grnprimary)), 
                React.createElement("div", {className: "seatPrimary PUP"}, roundVote(this.props.data.pupprimary)), 
                React.createElement("div", {className: "seatPrimary OTH"}, roundVote(this.props.data.othprimary))
            )
        );
    }
});

var RepsSeatExtraInfoRow = React.createClass({displayName: "RepsSeatExtraInfoRow",
    render: function() {
        var wikiLink = "http://en.wikipedia.org/wiki/Division_of_" + this.props.data.name;
        return(
            React.createElement("div", {className: "seatExtraInfo"}, 
                React.createElement("div", {className: "seatWikiLink"}, 
                    React.createElement("a", {href: wikiLink}, "Wiki")
                )
            )
        );
    }
});

function getTileCssClass(d){
    var seatType = "Retain";
    if(d.winner !== d.incumbentparty){
        seatType = "Win";
    }
    return(d.winner.toLowerCase() + seatType);
}

var RepsSeat = React.createClass({displayName: "RepsSeat",
    render: function() {
        var thisSeatClass = "seatTile " + getTileCssClass(this.props.data);
        return(
            React.createElement("div", {className: thisSeatClass}, 
                React.createElement(RepsSeatHeaderRow, {data: this.props.data}), 
                React.createElement(RepsSeatIncumbentRow, {data: this.props.data}), 
                React.createElement(RepsSeatTwoppRow, {data: this.props.data}), 
                React.createElement(RepsSeatPrimaryRow, {data: this.props.data}), 
                React.createElement(RepsSeatExtraInfoRow, {data: this.props.data})
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
