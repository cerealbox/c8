var log = console.log.bind(console)

exports.display = function(gs) {
	return function() {
		if (typeof document != 'undefined')
			displayo(gs)
	}
}

// ================================================================================
var uiEvents = []
var uiCallback
if (typeof window != 'undefined') {
	window.onmouseup = function(e) {
		uiEvents.push(new PS.Client.Click())
	    if (uiCallback) {
		    while (uiEvents.length)
		        uiCallback()	
	    }
	}
}

exports.getMouseEvent = function() {
	return uiEvents.shift()
}

exports.waitForMouseEvent = function(f) {

    return function() {
        uiCallback = f
        while (uiEvents.length)
            uiCallback()
    }
}
