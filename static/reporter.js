window.addEventListener( 'load', function(event){

    var ws = new WebSocket('ws://' + window.location.host + '/reporter');
	
    ws.onopen = function() {
    };
	
    ws.onclose = function(event) {
    };
	
    ws.onmessage = function(event) {
	console.log('onmessage: ' + event.data);
    };

    document.getElementById('send-button').addEventListener('click', function(event){
	ws.send('');
    });
    
}, false );
