"using strict";

function assert(condition, message) {
    if (!condition) {
        message = message || "Assertion failed";
        throw new Error(message);
    }
}

// Returns true if all data members are null
function rowIsNull(x) {
    return x.slice(1).every(function(i) {
        return i === null;
    });
}

function columnIsNull(d, colIndex) {
    return d.every(function(r) {
        return r[colIndex] === null;
    });
}

function DatedDataFrame(data, colnames) {
    assert(typeof data === "object" && data instanceof Array, "Data must be an array");
    if (data.length === 0) {
        this.data = [];
        if (colnames instanceof Array) {
            this.colnames = colnames;
        }else if(typeof colnames === "string"){
            this.colnames = [colnames];
        }else{
            throw new Error("Invalid colnames.");
        }
        return;
    }
    assert(data.every(function(r) {
        return r[0] instanceof Date;
    }), "First column must be dates");
    assert(data[0].length >= 2, "Data must have more than one column");
    assert(data.every(function(r, i) {
        return i === 0 || r[0] > data[i - 1][0];
    }), "Dates must be increasing");
    if (colnames instanceof Array) {
        assert(colnames.length === data[0].length - 1, "Length of colnames must match number of columns");
        assert(colnames.every(function(c) {
            return typeof c === "string";
        }), "Colnames must be strings");
        for (var c = 0; c < colnames.length; c++) {
            assert(!columnIsNull(data, c), "Data column '" + colnames[c] + "' is empty.");
        }
    } else if (typeof colnames === "string") {
        assert(data[0].length === 2, "Only one label provided for multiple columns.");
        assert(!columnIsNull(data,1), "Data column '" + colnames + "' is empty");
        colnames = [colnames];
    } else {
        throw new Error("Unknown colnames type.");
    }
    this.data = data;
    this.colnames = colnames;
}

DatedDataFrame.prototype.push = function(newRow) {
    assert(newRow instanceof Array, "New row must be an array.");
    assert(newRow.length > 0, "New row must not be empty.");
    assert(newRow.length === this.colnames.length + 1, "Column width must match.");
    assert(newRow[0] instanceof Date, "New row must have a date index.");
    if (this.data.length > 0) {
        assert(this.data[this.data.length - 1][0] <= newRow[0], "New row must be subsequent to previous.");
        if(this.data[this.data.length-1][0].getTime() === newRow[0].getTime()){
            console.warn("Ignoring duplicate entry for " + this.colnames[0] + " " + newRow[0]);
            return;
        }
    }
    this.data.push(newRow);
};

DatedDataFrame.prototype.column = function(colname) {
    var colIndex = this.colnames.indexOf(colname);
    assert(colIndex !== -1, "Column '" + colname + "' not found.");
    var output = [];
    for (var r = 0; r < this.data.length; r++) {
        if (this.data[r][colIndex + 1] !== null) {
            output.push([this.data[r][0], this.data[r][colIndex + 1]]);
        }
    }
    return new DatedDataFrame(output, colname);
};

DatedDataFrame.prototype.columns = function(colnames) {
    var _this = this;
    var colIndexes = colnames.map(function(x) {
        return _this.colnames.indexOf(x);
    });
    colnames.forEach(function(x) {
        assert(_this.colnames.indexOf(x) !== -1, "Column '" + x + "' not found.");
    });
    var output = [];
    for (var r = 0; r < this.data.length; r++) {
        var thisRow = [this.data[r][0]];
        for (var c = 0; c < colIndexes.length; c++) {
            thisRow.push(this.data[r][colIndexes[c] + 1]);
        }
        if (!rowIsNull(thisRow)) {
            output.push(thisRow);
        }
    }
    return new DatedDataFrame(output, colnames);
};

DatedDataFrame.prototype.drop = function(colname) {
    var colIndex = this.colnames.indexOf(colname);
    assert(colIndex !== -1, "Column '" + colname + "' not found.");
    assert(this.data[0].length > 2, "Cannot drop column '" + colname + "' as it is the last member.");
    var newData = [];
    var atLeastOneNonemptyRowRemains = false;
    for (var r = 0; r < this.data.length; r++) {
        var rowWithoutDroppedCol = this.data[r].splice(colIndex + 1, 1);
        if (!rowIsNull(rowWithoutDroppedCol)) {
            newData.push(rowWithoutDroppedCol);
            atLeastOneNonemptyRowRemains = true;
        }
    }
    assert(atLeastOneNonemptyRowRemains, "Cannot drop column'" + colname + "' as it is the last non-empty column.");
    this.data = newData;
    this.colnames = this.colnames.splice(colIndex, 1);
};

DatedDataFrame.prototype.merge = function(newDf) {
    assert(newDf instanceof DatedDataFrame, "Can only merge DatedDataFrame objects");
    var mergedData = [];
    var thisRow = 0;
    var newRow = 0;
    var nExistingSeries = this.colnames.length;
    var nNewSeries = newDf.colnames.length;
    var emptyExistingData = [];
    for (var i = 0; i < nExistingSeries; i++) {
        emptyExistingData.push(null);
    }
    var emptyNewData = [];
    for (i = 0; i < nNewSeries; i++) {
        emptyNewData.push(null);
    }
    while (thisRow < this.data.length && newRow < newDf.data.length) {
        if (this.data[thisRow][0].getTime() === newDf.data[newRow][0].getTime()) {
            mergedData.push(this.data[thisRow].concat(newDf.data[newRow].splice(1)));
            thisRow++;
            newRow++;
        } else if (this.data[thisRow][0] < newDf.data[newRow][0]) {
            mergedData.push(this.data[thisRow].concat(emptyNewData));
            thisRow++;
        } else {
            mergedData.push(
                [newDf.data[newRow][0]]
                .concat(emptyExistingData)
                .concat(newDf.data[newRow].splice(1))
            );
            newRow++;
        }
    }
    while (thisRow < this.data.length) {
        mergedData.push(this.data[thisRow].concat(emptyNewData));
        thisRow++;
    }
    while (newRow < newDf.data.length) {
        mergedData.push(
            [newDf.data[newRow][0]]
            .concat(emptyExistingData)
            .concat(newDf.data[newRow].splice(1))
        );
        newRow++;
    }
    return new DatedDataFrame(mergedData, this.colnames.concat(newDf.colnames));
};
