var log = console.log.bind(console)

exports.display = function(gs) {
	if (typeof document != 'undefined')
		displayo(gs)
}

exports.timeout = function(milli) {
    return function(f) {
        return function() {
            setTimeout(f, milli)
        }
    }
}

exports.click = function(f) {
	if (typeof document == 'undefined') return function() { setTimeout(f, 1000) }

	return function() {
		window.onmouseup = f
	}
}