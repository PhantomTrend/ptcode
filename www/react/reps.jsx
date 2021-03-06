var RepsResultText = React.createClass({
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
            <div class="repsforecasttext">
                The model's best guess sees the LNP government winning <span className="repsSummaryNumber">{nLnpSeats}</span>,
                the ALP <span className="repsSummaryNumber">{nAlpSeats}</span>,
                the Greens <span className="repsSummaryNumber">{nGrnSeats}</span>,
                Palmer United with <span className="repsSummaryNumber">{nPupSeats}</span> and
                Independents winning <span className="repsSummaryNumber">{nOthSeats}</span>.
                <br/><b>Caveats:</b> The model is likely to get about 10 individual seats wrong, hopefully in a way that makes the errors cancel each other out a little so the overall total is ok.
                  And it's based on state-level polls, so individual seats could behave very differently if they have unsusually high third-party or independent votes.
                  <br/>Also, the seat winners aren't a linear function of the 2PP numbers.
                    In very marginal seats, sometimes the most likely winner has an average 2PP of just below 50, because the model sees many scenarios in which they win narrowly and a few scenarios where they lose decisively.
            </div>
        );
    }
});

var RepsSeatList = React.createClass({
    render: function() {
        var seatNodes = [];
        for(var seatName in this.props.data){
            seatNodes.push( <RepsSeat data={this.props.data[seatName]} /> );
        }
        return (
            <div className="repsSeatList">
                {seatNodes}
                <div className="repsSeatListEnd"></div>
            </div>
        );
    }
});

var RepsSeatHeaderRow = React.createClass({
    render: function() {
        return(
            <div className="seatHeaderRow">
                <div className="seatName">{this.props.data.name}</div>
                <div className="seatDescription">{this.props.data.description}</div>
            </div>
        );
    }
});

var RepsSeatIncumbentRow = React.createClass({
    render: function() {
        return(
            <div className="seatIncumbentRow">
                <div className="seatIncumbentName">{this.props.data.member}</div>
                <div className="seatIncumbentParty">{this.props.data.incumbentparty}</div>
            </div>
        );
    }
});

function roundVote(v){
    return Math.round(v*10)/10;
}

var RepsSeatTwoppRow = React.createClass({
    render: function() {
        return(
            <div className="seatTwoPPRow">
                <div className="seat2pp LNP">{roundVote(this.props.data.lnp2pp)}</div>
                <div className="seat2pp ALP">{roundVote(this.props.data.alp2pp)}</div>
            </div>
        );
    }
});

var RepsSeatPrimaryRow = React.createClass({
    render: function() {
        return(
            <div className="seatPrimaryRow">
                <div className="seatPrimary LNP">{roundVote(this.props.data.lnpprimary)}</div>
                <div className="seatPrimary ALP">{roundVote(this.props.data.alpprimary)}</div>
                <div className="seatPrimary GRN">{roundVote(this.props.data.grnprimary)}</div>
                <div className="seatPrimary PUP">{roundVote(this.props.data.pupprimary)}</div>
                <div className="seatPrimary OTH">{roundVote(this.props.data.othprimary)}</div>
            </div>
        );
    }
});

var RepsSeatExtraInfoRow = React.createClass({
    render: function() {
        var wikiLink = "http://en.wikipedia.org/wiki/Division_of_" + this.props.data.name;
        var aecLink = "http://www.aec.gov.au/" + this.props.data.name.toLowerCase();
        var tallyRoomLink = "http://www.tallyroom.com.au/archive/aus2016/" + this.props.data.name.toLowerCase() + "2016";
        var abcLink = "http://www.abc.net.au/news/federal-election-2016/guide/" + this.props.data.name.toLowerCase().substring(0,4);
        return(
            <div className="seatExtraInfo">
                <div className="seatWikiLink">
                    <a href={wikiLink}>Wikipedia</a>
                </div>
                <div className="seatAecLink">
                    <a href={aecLink}>AEC</a>
                </div>
                <div className="seatTRLink">
                    <a href={tallyRoomLink}>Tally Room</a>
                </div>
                <div className="seatAbcLink">
                    <a href={abcLink}>ABC</a>
                </div>
            </div>
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

var RepsSeat = React.createClass({
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
                <div className={thisSeatClass}>
                    <span className="fa fa-toggle-down" onClick={this.handleClick}></span>
                    <RepsSeatHeaderRow data={this.props.data} />
                    <RepsSeatIncumbentRow data={this.props.data} />
                    <RepsSeatTwoppRow data={this.props.data} />
                    <RepsSeatPrimaryRow data={this.props.data} />
                    <RepsSeatExtraInfoRow data={this.props.data} />
                </div>
            );
        }else{
            return(
                <div className={thisSeatClass}>
                    <span className="fa fa-toggle-right" onClick={this.handleClick}></span>
                    <RepsSeatHeaderRow data={this.props.data} />
                </div>
            );
        }
    }
});

var RepsSeatTypeFilter = React.createClass({
    handleFilterChange: function(event) {
        this.props.updateFilter(event.target.value);
    },
    render: function() {
        return  <select className="repsFilterBox" onChange={this.handleFilterChange}>
                    <option value="changingHands">Changing hands</option>
                    <option value="inPlay">In play</option>
                    <option value="all">All seats</option>
                </select>;
    }
});

var RepsStatesFilter = React.createClass({
    handleFilterChange: function(event) {
        this.props.updateFilter(event.target.value);
    },
    render: function() {
        return  <select className="repsFilterBox" onChange={this.handleFilterChange}>
                    <option value="allStates">All States</option>
                    <option value="NSW">NSW</option>
                    <option value="Vic">VIC</option>
                    <option value="Qld">QLD</option>
                    <option value="SA">SA</option>
                    <option value="WA">WA</option>
                    <option value="Tas">TAS</option>
                    <option value="NT">NT</option>
                    <option value="ACT">ACT</option>
                </select>;
    }
});

var RepsSection = React.createClass({
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
            <div>
                <h4>Summary</h4>
                <RepsResultText resultList={this.props.data} />
                <h4>Individual Seats</h4>
                <RepsSeatTypeFilter updateFilter={this.handleSeatTypeUpdate} />
                <RepsStatesFilter updateFilter={this.handleStatesUpdate} />
                <RepsSeatList data={seatsForList} />
            </div>
        );
    }
});

React.render(<RepsSection data={houseResults} />, document.getElementById('repsApp'));
