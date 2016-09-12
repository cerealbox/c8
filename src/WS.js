var log = console.log.bind(console)
var WebSocketServer = require('ws').Server


exports.listenStatic = function(ip) {
    return function(port) {
        return function() {

            if (typeof document == 'undefined') {
                var fs = require('fs')
                var http = require('http')
                var path = require('path')

                http.createServer(function (req, res) {
                    var filePath = '.' + req.url
                    if (filePath == './')
                        filePath = './index.html'
                    var ext = String(path.extname(filePath)).toLowerCase()

                    var mimeTypes = {
                        '.html': 'text/html',
                        '.js': 'text/javascript',
                        '.css': 'text/css',
                        '.json': 'application/json',
                        '.png': 'image/png'
                    }
                    
                    try {
                        var data = fs.readFileSync(filePath)

                        res.writeHead(200, {'Content-Type': mimeTypes[ext] || 'text/plain'})
                        res.end(data)
                    } catch(e) {
                        res.writeHead(404)
                        res.end("404 not found.")
                    }

                }).listen(port, ip)

                log("static file webserver running at http://" + ip + ":" + port + "/")
            }

        }
    }
}
// -------------------------------------------------------------

// -------------------------------------------------------------

//======================================================================
var socket
var events = []
var callback

exports.sendToAll = function(msg) {
    return function() {
        socket.clients.forEach(function(client) { 
            var client_ip = client._socket.remoteAddress
            var client_port = client._socket.remotePort
            client.send(JSON.stringify(msg))
        })
    }
}

exports.send = function(msg) {
    return function() {
        socket.send(JSON.stringify(msg))
    }
}

exports.connect = function(ip) {
    return function(port) {
        return function() {
            log("[connecting to websocket]")
            socket = new WebSocket("ws://" + ip + ":" + port + "/");
            socket.onopen = function(e) { 
                log("[websocket connected]")
            }
            socket.onclose = function(e) { log("[websocket closed]") }
            socket.onmessage = function(e) { 
                events.push(new PS.WS.ServerMessage(JSON.parse(e.data)))
                if (callback) {
                    while (events.length)
                        callback()
                }
            }
        }
    }
}

exports.listen = function(ip) {
    return function(port) {
        return function() {
            socket = new WebSocketServer({ ip: ip, port: port })
            log("websocket server listening on " + ip + ":" + port)

            setInterval(function() {
                events.push(new PS.WS.Tick())
                if (callback) {
                    while (events.length)
                        callback()
                }                
            }, 1000)
            
            socket.on('connection', function connection(newclient) {
                var ip = newclient.upgradeReq.connection.remoteAddress
                var port = newclient.upgradeReq.connection.remotePort

                events.push(new PS.WS.Connection(new PS.WS.Client(ip, port)))
                if (callback) {
                    while (events.length)
                        callback()
                }
                
                newclient.on('message', function(msg) {
                    events.push(new PS.WS.ClientMessage(new PS.WS.Client(ip, port), msg))
                    if (callback) {
                        while (events.length)
                            callback()
                    }
                })
             
                newclient.on('close', function() {
                    events.push(new PS.WS.Close(new PS.WS.Client(ip, port)))
                    if (callback) {
                        while (events.length)
                            callback()
                    }
                })

            })
        }
    }
}

exports.waitForEvent = function(f) {
    return function() {
        if (!socket)
            log("websocket server not started.  call 'listen'.")

        callback = f
        while (events.length)
            callback()
    }
}


exports.getEvent = function() {
    return events.shift()
}
