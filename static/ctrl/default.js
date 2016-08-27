window.addEventListener( 'load', function(event){

    var commands = new Array('s','f','b','fl','fr','bl','br','tl','tr');
    var ws = new WebSocket('ws://' + window.location.host + '/default');
    
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
	if(json.type === 'command') {
	    if (json.name === 'al') {
		document.getElementById('al').value = json.level;
	    } else if(json.name === 'ar') {
		document.getElementById('ar').value = json.level;
	    } else {
	    for(var i = 0; i < commands.length; i++) {
		var cmd = commands[i];
		var elem = document.getElementById(cmd);
		if(cmd === json.name){
		    elem.style.background = '#FF9090';
		}else{
		    elem.style.background = '';
		}
	    }
	    }
	}
    };

    function clickListenerGen(cmd){
	var listener = function(event) {
	    var data = { type: 'command', name: cmd};
	    var json = JSON.stringify(data);
	    ws.send(json);
	}
	return listener;
    }
    
    for(var i = 0; i < commands.length; i++) {
	var cmd = commands[i];
	document.getElementById(cmd).addEventListener('click', clickListenerGen(cmd));	
    }
    

    document.getElementById('al').addEventListener('change', function(event){
	var lev  = parseInt(document.getElementById('al').value);
	var data = { type: 'command', name: 'al', level: lev};
	var json = JSON.stringify(data);
	ws.send(json);
    });
    
    document.getElementById('ar').addEventListener('change', function(event){
	var lev  = parseInt(document.getElementById('ar').value);
	var data = { type: 'command', name: 'ar', level: lev};
	var json = JSON.stringify(data);
	ws.send(json);
    });	
    
}, false );


