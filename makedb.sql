
DROP TABLE IF EXISTS twopp;
CREATE TABLE twopp (
    pollenddate date,
    electorate character(4),
    alp2pp   real,
    repetition integer
    );

\copy twopp from 'ElectionResults/TwoPartyPreferred.csv' with (DELIMITER ',', HEADER, FORMAT CSV);

DROP TABLE IF EXISTS primaryinput;
CREATE TABLE primaryinput (
    rownumber integer,
    pollster character(23),
    party character(15),
    vote real,
    electorate character(20),
    year character(2),
    week character(2),
    pollenddate date,
    lag integer,
    observationcolumn character(2)
    );

\copy primaryinput from 'PlotData/ModelOutput.csv'with (DELIMITER ',', HEADER, FORMAT CSV, NULL 'NA');




