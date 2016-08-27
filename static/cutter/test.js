window.addEventListener( 'load', function(event){

    // 30 of takahashi meijin
    for (var i = 0; i < 30; i++) {
	takahashi();
    }
    
    function takahashi() {

	var ws = new WebSocket('ws://' + window.location.host + '/reporter');
	ws.onopen = function() {
	    setInterval(function(){
		ws.send('');
	    },62); // 16 taps per second
	};
	
	ws.onclose = function(event) {
	};
	
	ws.onmessage = function(event) {
	    console.log('onmessage: ' + event.data);
	};
	
    }
    
}, false );
