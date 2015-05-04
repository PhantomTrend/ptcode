
DROP TABLE IF EXISTS twopp;
CREATE TABLE twopp (
    pollenddate date,
    electorate character(4),
    alp2pp   real,
    onesd   real
    );
\copy twopp from '/tmp/TwoPartyPreferred.csv' with (DELIMITER ',', HEADER, FORMAT CSV);

DROP TABLE IF EXISTS primarytrend;
CREATE TABLE primarytrend (
    party character(3),
    electorate character(4),
    pollenddate date,
    vote real,
    onesd real
    );
\copy primarytrend from '/tmp/PrimaryVotes.csv' with (DELIMITER ',', HEADER, FORMAT CSV, NULL 'NA');

DROP TABLE IF EXISTS polldata;
CREATE TABLE polldata (
    pollenddate date,
    pollster text,
    party character(6),
    vote real,
    electorate character(4),
    url text
);
\copy polldata from '/tmp/PollsForDb.csv' with (DELIMITER ',', HEADER, FORMAT CSV, NULL 'NA');



DROP TABLE IF EXISTS seatresults;
CREATE TABLE seatresults (
    electorate text,
    alpwinpct real,
    lnpwinpct real,
    grnwinpct real,
    pupwinpct real,
    othwinpct real,
    alpprimary real,
    lnpprimary real,
    grnprimary real,
    pupprimary real,
    othprimary real,
    alp2pp real,
    lnp2pp real,
    member text,
    incumbentparty text,
    state text,
    winner text,
    description text
);
\copy seatresults from '/tmp/SeatResults.csv' with (DELIMITER ',', HEADER, FORMAT CSV, NULL 'NA');


GRANT SELECT ON twopp TO ptuser;
GRANT SELECT ON primarytrend TO ptuser;
GRANT SELECT ON polldata TO ptuser;
GRANT SELECT ON seatresults TO ptuser;


