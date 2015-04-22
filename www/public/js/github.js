$(function() {

    var timeSince = function(date) {
        // This function by Sky Sanders at http://stackoverflow.com/a/3177838
        var seconds = Math.floor((new Date() - date) / 1000);
        var interval = Math.floor(seconds / 31536000);
        if (interval > 1) {
            return interval + " years";
        }
        interval = Math.floor(seconds / 2592000);
        if (interval > 1) {
            return interval + " months";
        }
        interval = Math.floor(seconds / 86400);
        if (interval > 1) {
            return interval + " days";
        }
        interval = Math.floor(seconds / 3600);
        if (interval > 1) {
            return interval + " hours";
        }
        interval = Math.floor(seconds / 60);
        if (interval > 1) {
            return interval + " minutes";
        }
        return Math.floor(seconds) + " seconds";
    };

    var repoAddress = "https://api.github.com/repos/PhantomTrend/ptcode/";
    var startDate = "2015-01-01T09:00:00Z";
    $.ajax({
        url: repoAddress + "commits?since=" + startDate,
        success: function(data) {
            for (var i = 0; i < 5; i++) {
                var url = data[i].html_url;
                var message = data[i].commit.message;
                var datetime = timeSince(new Date(data[i].commit.author.date));
                var pId = "#github-commit-"+(i+1);
                $(pId).html("<a href=\"" + url + "\">" + datetime + " ago</a> " + message);
            }
        }
    });
});
