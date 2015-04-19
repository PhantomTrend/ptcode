var pg = require('pg');
var conString = "postgres://ptuser@localhost:5432/ptdata";

var express = require('express');
var app = express();
var url = require('url');

var compression = require('compression');
app.use(compression());

var helmet = require('helmet');
app.use(helmet());


app.use(express.static(__dirname + '/public', {index: "index.html"}));


app.get('/twopp', function(req, res) {
    var url_parts = url.parse(req.url, true);
    var query = url_parts.query;
    var electorate = query.electorate;
    var acceptableElectorates = ['AUS','NSW','VIC','TAS','WA','SA','QLD','NT','ACT'];
    if(acceptableElectorates.indexOf(electorate) == -1){
        res.writeHead(404, {'content-type': 'text/plain'});
        res.end("Unknown electorate");
    }
    getTwoPPJson(electorate, res);
});


var server = app.listen(3001, function() {

    var host = server.address().address;
    var port = server.address().port;
    console.log('Example app listening at http://%s:%s', host, port);
});


getTwoPPJson = function(electorate, res) {
    pg.connect(conString, function(err, client, done) {
        var handleError = function(err) {
            if (!err) return false;
            done(client); // remove client from pool if it exists
            console.log(err);
            return true;
        };
        client.query("SELECT pollenddate, avg(alp2pp), stddev(alp2pp)" +
            "FROM TwoPP WHERE electorate = $1" +
            "GROUP BY pollenddate ORDER BY pollenddate;",
            [electorate],
            function(err, result) {
                if (handleError(err)) {
                    res.writeHead(500, {'content-type': 'text/plain'});
                    res.end("Server error");
                    return;
                }
                done();
                var output = result.rows.map(function(v) {
                    return ([new Date(v.pollenddate), [v.avg, v.stddev]]);
                });
                res.send(output);
            });
    });
};
