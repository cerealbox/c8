var log = console.log.bind(console)

function list2array(lst) {
	function toCard(c) {
		if (c.constructor.name == "CardView") {
			return c.value0.value0.constructor.name + " of " + c.value1.constructor.name
		} else if (c.constructor.name == "Card") {
			return c.value0.constructor.name + " of " + c.value1.constructor.name
		} else {
			return c
		}
	}

	if (lst.value0 == null) return []	
	return [toCard(lst.value0)].concat(list2array(lst.value1)) 
}

//@TODO: only displays 2 players.
function displayo(gs) {
	var players = list2array(gs.players)
	if (players.length != 2) {
		log("@TODO: code missing to display more than two players.")
		return
	}

	var discard = list2array(gs.discard)
	var deck = list2array(gs.deck)
	var player0, player1, player0score, player1score
	var spectators = list2array(gs.spectators)

	if (gs.players.value0.id == 0) {
		player0 = list2array(gs.players.value0.hand)
		player1 = list2array(gs.players.value1.value0.hand)	
		player0score = gs.players.value0.score
		player1score = gs.players.value1.value0.score
		player0name = gs.players.value0.name
		player1name = gs.players.value1.value0.name
	} else {
		player1 = list2array(gs.players.value0.hand)
		player0 = list2array(gs.players.value1.value0.hand)	
		player1score = gs.players.value0.score
		player0score = gs.players.value1.value0.score
		player1name = gs.players.value0.name
		player0name = gs.players.value1.value0.name		
	}

	discard.reverse()
	discard.forEach(function(card, i) { document.getElementById(card).className = "discard_" + i })

	deck.reverse()
	deck.forEach(function(card, i) { document.getElementById(card).className = "deck_" + i })

	player0.reverse()
	player0.forEach(function(card, i) { document.getElementById(card).className = "player0_" + i })

	player1.reverse()
	player1.forEach(function(card, i) { document.getElementById(card).className = "player1_" + i })

	if (gs.perCardScore > 1)
		document.getElementById("multiplyer").innerHTML = "X" + gs.perCardScore
	else
		document.getElementById("multiplyer").innerHTML = ""
	document.getElementById("player0score").innerHTML = player0score
	document.getElementById("player1score").innerHTML = player1score
	document.getElementById("player0name").innerHTML = player0name
	document.getElementById("player1name").innerHTML = player1name
	var observers = spectators.reduce(function(acc, cur) { return acc + " " + cur.ipport + " -> " + cur.name + " (" + cur.score + ") " + (cur.queue ? "*QUEUED*" : "") + "<br />" }, "spectators: <br />")
	document.getElementById("spectators").innerHTML = observers
}

//==================================================================================

/*
document.write("<style>")

//player styles:
for (var p = 0; p < 2; p++) {
	for (var i = 0; i < 52; i++) {
		document.write(`
			.player${p}_${i} {
				position: absolute;
				left: ${30 + i * 20}px;
				top: ${p * 400}px;
				z-index: ${52 + i};
				transition: top 1s, left 1s;
			}
		`)
	}
	
	//scores:
	document.write(`
		#player${p}score {
			position: absolute;
			left: 0px;
			top: ${p * 400}px;
			font-weight: bold;
			font-size: 24px;
		}
	`)
	//names:
	document.write(`
		#player${p}name {
			position: absolute;
			left: 0px;
			top: ${p * 400 + 131}px;
			font-weight: bold;
			font-size: 24px;
		}
	`)
}

//deck styles:
for (var i = 0; i < 52; i++) {
	document.write(`
		.deck_${i} {
			position: absolute;
			left: ${100 + i}px;
			top: 200px;
			content: url("cards/backa.png");
			z-index: ${i};
			transition: top 1s, left 1s;
		}
	`)
}

document.write(`
	.deck {
		position: absolute;
		left: 100px;
		top: 200px;
		content: url("cards/backa.png");
		z-index: 0;
	}	
	#multiplyer {
		position: absolute;
		left: 70px;
		top: 250px;
		font-weight: bold;
		font-size: 24px;
	}
	#spectators {
		position: absolute;
		left: 700px;
		top: 400px;
		font-weight: bold;
		font-size: 18px;
	}
`)

//discard styles:
for (var i = 0; i < 52; i++) {
	document.write(`
		.discard_${i} {
			position: absolute;
			left: ${235 + i * 20}px;
			top: 200px;
			z-index: ${104 + i};
			transition: top 1s, left 1s;
		}
	`)
}

document.write("</style>")

//==================================================================================


//generate all cards:
String.prototype.capitalize = function() {
    return this.charAt(0).toUpperCase() + this.slice(1);
}

var suits = ["", "Diamonds", "Hearts", "Spades", "Clubs"]
for (var suit = 1; suit <= 4; suit++) {
	["ace", "two", "three", "four", "five", "six", 
	"seven", "eight", "nine", "ten", "jack", "queen", "king"].forEach(function(face) {
		document.write(`
			<img 
				id="${face.capitalize()} of ${suits[suit]}" 
				src="cards/${face}${suit}.png" 
				class="deck"
			/>
		`)
	})
}

//generate other stats:
document.write(`
	<div id="player0name"></div>
	<div id="player1name"></div>
	<div id="player0score"></div>
	<div id="player1score"></div>
	<div id="multiplyer"></div>
	<div id="spectators"></div>
`)

*/