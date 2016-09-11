var log = console.log.bind(console)

exports.display = function(gs) {
    return function() {
    	if (typeof document != 'undefined')
    		displayo(gs)
    }
}

exports.timeout = function(milli) {
    return function(f) {
        return function() {
            setTimeout(f, milli)
        }
    }
}


