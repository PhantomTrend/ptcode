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
                "The model's best guess sees the LNP government winning ", React.createElement("span", {className: "repsSummaryNumber"}, nLnpSeats), "," + ' ' +
                "the ALP ", React.createElement("span", {className: "repsSummaryNumber"}, nAlpSeats), "," + ' ' +
                "the Greens ", React.createElement("span", {className: "repsSummaryNumber"}, nGrnSeats), "," + ' ' +
                "Palmer United with ", React.createElement("span", {className: "repsSummaryNumber"}, nPupSeats), " and" + ' ' +
                "Independents winning ", React.createElement("span", {className: "repsSummaryNumber"}, nOthSeats), ".", 
                React.createElement("br", null), React.createElement("b", null, "Caveats:"), " The model is likely to get about 10-12 individual seats wrong, hopefully in a way that stays close to the overall total." + ' ' +
                  "And it's based on state-level polls, so individual seats could behave very differently if they have unsusually high third-party or independent votes."
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
                seatNodes, 
                React.createElement("div", {className: "repsSeatListEnd"})
            )
        );
    }
});

var RepsSeatHeaderRow = React.createClass({displayName: "RepsSeatHeaderRow",
    render: function() {
        return(
            React.createElement("div", {className: "seatHeaderRow"}, 
                React.createElement("div", {className: "seatName"}, this.props.data.name), 
                React.createElement("div", {className: "seatDescription"}, this.props.data.description)
            )
        );
    }
});

var RepsSeatIncumbentRow = React.createClass({displayName: "RepsSeatIncumbentRow",
    render: function() {
        return(
            React.createElement("div", {className: "seatIncumbentRow"}, 
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
        var aecLink = "http://www.aec.gov.au/" + this.props.data.name.toLowerCase();
        var tallyRoomLink = "http://www.tallyroom.com.au/archive/aus2016/" + this.props.data.name.toLowerCase() + "2016";
        var abcLink = "http://www.abc.net.au/news/federal-election-2016/guide/" + this.props.data.name.toLowerCase().substring(0,4);
        return(
            React.createElement("div", {className: "seatExtraInfo"}, 
                React.createElement("div", {className: "seatWikiLink"}, 
                    React.createElement("a", {href: wikiLink}, "Wikipedia")
                ), 
                React.createElement("div", {className: "seatAecLink"}, 
                    React.createElement("a", {href: aecLink}, "AEC")
                ), 
                React.createElement("div", {className: "seatTRLink"}, 
                    React.createElement("a", {href: tallyRoomLink}, "Tally Room")
                ), 
                React.createElement("div", {className: "seatAbcLink"}, 
                    React.createElement("a", {href: abcLink}, "ABC")
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
    getInitialState: function() {
        return {showDetails: false};
        },
    handleClick: function(event) {
        this.setState({showDetails: !this.state.showDetails});
        },
    render: function() {
        var thisSeatClass = "seatTile " + getTileCssClass(this.props.data);
        if(this.state.showDetails){
            return(
                React.createElement("div", {className: thisSeatClass}, 
                    React.createElement("span", {className: "fa fa-toggle-down", onClick: this.handleClick}), 
                    React.createElement(RepsSeatHeaderRow, {data: this.props.data}), 
                    React.createElement(RepsSeatIncumbentRow, {data: this.props.data}), 
                    React.createElement(RepsSeatTwoppRow, {data: this.props.data}), 
                    React.createElement(RepsSeatPrimaryRow, {data: this.props.data}), 
                    React.createElement(RepsSeatExtraInfoRow, {data: this.props.data})
                )
            );
        }else{
            return(
                React.createElement("div", {className: thisSeatClass}, 
                    React.createElement("span", {className: "fa fa-toggle-right", onClick: this.handleClick}), 
                    React.createElement(RepsSeatHeaderRow, {data: this.props.data})
                )
            );
        }
    }
});

var RepsSeatTypeFilter = React.createClass({displayName: "RepsSeatTypeFilter",
    handleFilterChange: function(event) {
        this.props.updateFilter(event.target.value);
    },
    render: function() {
        return  React.createElement("select", {className: "repsFilterBox", onChange: this.handleFilterChange}, 
                    React.createElement("option", {value: "changingHands"}, "Changing hands"), 
                    React.createElement("option", {value: "inPlay"}, "In play"), 
                    React.createElement("option", {value: "all"}, "All seats")
                );
    }
});

var RepsStatesFilter = React.createClass({displayName: "RepsStatesFilter",
    handleFilterChange: function(event) {
        this.props.updateFilter(event.target.value);
    },
    render: function() {
        return  React.createElement("select", {className: "repsFilterBox", onChange: this.handleFilterChange}, 
                    React.createElement("option", {value: "allStates"}, "All States"), 
                    React.createElement("option", {value: "NSW"}, "NSW"), 
                    React.createElement("option", {value: "Vic"}, "VIC"), 
                    React.createElement("option", {value: "Qld"}, "QLD"), 
                    React.createElement("option", {value: "SA"}, "SA"), 
                    React.createElement("option", {value: "WA"}, "WA"), 
                    React.createElement("option", {value: "Tas"}, "TAS"), 
                    React.createElement("option", {value: "NT"}, "NT"), 
                    React.createElement("option", {value: "ACT"}, "ACT")
                );
    }
});

var RepsSection = React.createClass({displayName: "RepsSection",
    getInitialState: function() {
        return({
            seatTypes: "all",
            statesToShow: "allStates"
        });
    },
    handleSeatTypeUpdate: function(filterValue) {
        this.setState({
            seatTypes: filterValue
        });
    },
    handleStatesUpdate: function(filterValue) {
        this.setState({
            statesToShow: filterValue
        });
    },
    render: function(){
        var seatsForList = {};
        for(var seatName in this.props.data){
            var seat = this.props.data[seatName];
            var includeSeat = true;
            if(this.state.seatTypes === "changingHands" && (seat.winner === seat.incumbentparty)){
                includeSeat = false;
            }else if(this.state.seatTypes === "inPlay" &&
                    (seat.description.indexOf("Easy") !== -1 ||
                     seat.description.indexOf("Assumed") !== -1)){
                includeSeat = false;
            }else if(this.state.statesToShow !== "allStates" && (seat.state !== this.state.statesToShow)){
                includeSeat = false;
            }
            if(includeSeat){
                seatsForList[seatName] = seat;
            }
        }
        return(
            React.createElement("div", null, 
                React.createElement("h4", null, "Summary"), 
                React.createElement(RepsResultText, {resultList: this.props.data}), 
                React.createElement("h4", null, "Individual Seats"), 
                React.createElement(RepsSeatTypeFilter, {updateFilter: this.handleSeatTypeUpdate}), 
                React.createElement(RepsStatesFilter, {updateFilter: this.handleStatesUpdate}), 
                React.createElement(RepsSeatList, {data: seatsForList})
            )
        );
    }
});

React.render(React.createElement(RepsSection, {data: houseResults}), document.getElementById('repsApp'));
