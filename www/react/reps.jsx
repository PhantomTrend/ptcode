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
                <h4>Summary</h4>
                The model's best guess sees the LNP government winning <span className="repsSummaryNumber">{nLnpSeats}</span>,
                the ALP <span className="repsSummaryNumber">{nAlpSeats}</span>,
                the Greens <span className="repsSummaryNumber">{nGrnSeats}</span>,
                Palmer United with <span className="repsSummaryNumber">{nPupSeats}</span> and
                Independents winning <span className="repsSummaryNumber">{nOthSeats}</span>.
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
                <h4>Individual Seats</h4>
                {seatNodes}
            </div>
        );
    }
});

var RepsSeatHeaderRow = React.createClass({
    render: function() {
        return(
            <div className="seatHeaderRow">
                <div>{this.props.data.name}</div>
                <div>{this.props.data.description}</div>
            </div>
        );
    }
});

var RepsSeatIncumbentRow = React.createClass({
    render: function() {
        return(
            <div className="seatIncumbentRow">
                <div className="seatState">{this.props.data.state}</div>
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
        return(
            <div className="seatExtraInfo">
                <div className="seatWikiLink">
                    <a href={wikiLink}>Wiki</a>
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
                <div className={thisSeatClass} onClick={this.handleClick}>
                    <span className="fa fa-toggle-down"></span>
                    <RepsSeatHeaderRow data={this.props.data} />
                    <RepsSeatIncumbentRow data={this.props.data} />
                    <RepsSeatTwoppRow data={this.props.data} />
                    <RepsSeatPrimaryRow data={this.props.data} />
                    <RepsSeatExtraInfoRow data={this.props.data} />
                </div>
            );
        }else{
            return(
                <div className={thisSeatClass} onClick={this.handleClick}>
                    <span className="fa fa-toggle-right"></span>
                    <RepsSeatHeaderRow data={this.props.data} />
                </div>
            );
        }
    }
});

var RepsSection = React.createClass({
    render: function(){
        return(
            <div>
            <RepsResultText resultList={this.props.data} />
            <RepsSeatList data={this.props.data} />
            </div>
        );
    }
});

React.render(<RepsSection data={houseResults} />, document.getElementById('repsApp'));
