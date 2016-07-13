window.addEventListener( 'load', function(event){

    var ws = new WebSocket('ws://' + window.location.host + '/reporter');
	
    document.getElementById('title').style.background = '#FF0000';
    ws.onopen = function() {
	document.getElementById('title').style.background = '#00FF00';
    };
	
    ws.onclose = function(event) {
	document.getElementById('title').style.background = '#FF0000';
    };
	
    ws.onmessage = function(event) {
	console.log('onmessage: ' + event.data);
	var json = JSON.parse(event.data);
	if(json.type === 'side') {
	    if(json.content === 'left'){
		document.getElementById('send-button').value = 'Left';
	    } else if(json.content === 'right') {
		document.getElementById('send-button').value = 'Right';
	    }
	}
    };

    if ('ontouchend' in window) {
	document.getElementById('send-button').addEventListener('touchend', function(event){
	    ws.send('');
	});
    } else {
	document.getElementById('send-button').addEventListener('click', function(event){
	    ws.send('');
	});
    }    
    
}, false );
