
DROP TABLE IF EXISTS twopp;
CREATE TABLE twopp (
    pollenddate date,
    electorate character(4),
    alp2pp   real,
    repetition integer
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
