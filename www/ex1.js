var fs = require('fs');
var serverConfig = JSON.parse(fs.readFileSync("server_config.json"));

var pg = require('pg');

var express = require('express');
var app = express();
var url = require('url');

var FileStreamRotator = require('file-stream-rotator');

var compression = require('compression');
app.use(compression());

var helmet = require('helmet');
app.use(helmet());


var morgan = require('morgan');
var logDirectory = __dirname + '/log';
// ensure log directory exists
if (!fs.existsSync(logDirectory)) {
    fs.mkdirSync(logDirectory);
}
// create a rotating write stream
var accessLogStream = FileStreamRotator.getStream({
    filename: logDirectory + '/access.log',
    frequency: 'daily',
    verbose: false
});
// setup the logger
app.use(morgan('combined', {
    stream: accessLogStream
}));


var doT = require('dot-express');
app.set('view engine', 'dot');
app.engine('html', doT.__express);

app.use('/css', express.static(__dirname + '/public/css'));
app.use('/img', express.static(__dirname + '/public/img'));
app.use('/js', express.static(__dirname + '/public/js'));
app.get('/', function(req, res) {
    getHouseResultsJson(function(houseResults){
        var templateData = {
            "PT_HOST": serverConfig.host_address + ":" + serverConfig.host_port,
            "houseResults": houseResults
        };
        res.render('index.html', templateData);
    });
});

app.get('/twopp', function(req, res) {
    var url_parts = url.parse(req.url, true);
    var query = url_parts.query;
    var electorate = query.electorate;
    var acceptableElectorates = ['AUS', 'NSW', 'VIC', 'TAS', 'WA', 'SA', 'QLD', 'NT', 'ACT'];
    if (acceptableElectorates.indexOf(electorate) == -1) {
        res.writeHead(404, {
            'content-type': 'text/plain'
        });
        res.end("Unknown electorate");
        return;
    }
    getTwoPPJson(electorate, res);
});

app.get('/primary', function(req, res) {
    var url_parts = url.parse(req.url, true);
    var query = url_parts.query;
    var electorate = query.electorate;
    var acceptableElectorates = ['AUS', 'NSW', 'VIC', 'TAS', 'WA', 'SA', 'QLD', 'NT', 'ACT'];
    if (acceptableElectorates.indexOf(electorate) == -1) {
        res.writeHead(404, {
            'content-type': 'text/plain'
        });
        res.end("Unknown electorate");
        return;
    }
    getPrimaryJson(electorate, res);
});

app.get('/polls', function(req, res) {
    var url_parts = url.parse(req.url, true);
    var query = url_parts.query;
    var electorate = query.electorate;
    var acceptableElectorates = ['AUS', 'NSW', 'VIC', 'TAS', 'WA', 'SA', 'QLD', 'NT', 'ACT'];
    if (acceptableElectorates.indexOf(electorate) == -1) {
        res.writeHead(404, {
            'content-type': 'text/plain'
        });
        res.end("Unknown electorate");
        return;
    }
    getPollData(electorate, res);
});

app.get('/robots.txt', function(req, res) {
    res.type('text/plain');
    res.send("User-agent: *\nDisallow: http://www.phantomtrend.com/twopp\nDisallow:http://www.phantomtrend.com/primary\nDisallow:http://www.phantomtrend.com/polls\n/");
});

var server = app.listen(serverConfig.host_port, function() {
    var host = serverConfig.host_address;
    var port = serverConfig.host_port;
    console.log('PhantomTrend app listening at http://%s:%s', host, port);
});


function getTwoPPJson(electorate, res) {
    getDbQuery("SELECT pollenddate, alp2pp AS avg, onesd AS stddev " +
        "FROM twopp WHERE electorate = $1" +
        "ORDER BY pollenddate;", [electorate],
        function(v) {
            return ([new Date(v.pollenddate), [v.avg, v.stddev]]);
        },
        res);
}

function getPrimaryJson(electorate, res) {
    getDbQuery("SELECT pollenddate, party, vote AS avg, onesd AS stddev " +
        "FROM primarytrend WHERE electorate = $1 " +
        "ORDER BY pollenddate;", [electorate],
        function(v) {
            return ([new Date(v.pollenddate), v.party.trim(), [v.avg, v.stddev]]);
        },
        res);
}

function getPollData(electorate, res) {
    getDbQuery("SELECT * FROM polldata WHERE electorate = $1 " +
        "AND pollenddate >= '2000-01-01' " +
        "ORDER BY pollster, party, pollenddate;", [electorate],
        function(v) {
            return ([new Date(v.pollenddate), v.pollster, v.vote, v.url, v.party.trim()]);
        },
        res);
}

function getHouseResultsJson(callback) {
    pg.connect(serverConfig.pg_connection_string, function(err, client, done) {
        var handleError = function(err) {
            if (!err) return false;
            done(client); // remove client from pool if it exists
            console.log(err);
            return true;
        };
        if (handleError(err)) {
            return {};
        }
        client.query("SELECT * FROM seatresults;",
            function(err, result) {
                if (handleError(err)) {
                    return {};
                }
                done();
                callback(result.rows);
            });
    });
}

function getDbQuery(query, arg, formatter, res) {
    pg.connect(serverConfig.pg_connection_string, function(err, client, done) {
        var handleError = function(err) {
            if (!err) return false;
            done(client); // remove client from pool if it exists
            console.log(err);
            return true;
        };
        if (handleError(err)) {
            res.writeHead(500, {
                'content-type': 'text/plain'
            });
            res.end("Server error");
            return;
        }
        client.query(query, arg,
            function(err, result) {
                if (handleError(err)) {
                    res.writeHead(500, {
                        'content-type': 'text/plain'
                    });
                    res.end("Server error");
                    return;
                }
                done();
                var output = result.rows.map(formatter);
                res.send(output);
            });
    });
}
