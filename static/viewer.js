window.addEventListener( 'load', function(event){

    var thresholds = new Array(5,10,20,40,80,100,120,140);
    var ws = new WebSocket('ws://' + window.location.host + '/viewer');
    var current_threshold = 10;
	
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
	if(json.type === 'freq') {
	    var lc = json.content.left;
	    var rc = json.content.right;
	    document.getElementById('left-counts').innerHTML  = lc + ' taps';
	    document.getElementById('right-counts').innerHTML = rc + ' taps';
	    if( current_threshold <= lc ){
		document.getElementById('left-hist').style.height = '100%';
		document.getElementById('left-hist').style.background = '#00FF00';
	    }else{
		var h = Math.floor((lc / current_threshold) * 100);
		console.log(h);
		document.getElementById('left-hist').style.height = h + '%';
		document.getElementById('left-hist').style.background = '#FF0000';
	    }

	    if( current_threshold <= rc ){
		document.getElementById('right-hist').style.height = '100%';
		document.getElementById('right-hist').style.background = '#00FF00';
	    }else{
		var h = Math.floor((rc / current_threshold) * 100);
		console.log(h);
		document.getElementById('right-hist').style.height = h + '%';
		document.getElementById('right-hist').style.background = '#FF0000';
	    }
	    
	} else if(json.type === 'members') {
	    document.getElementById('left-members').innerHTML  = json.content.left + ' members';
	    document.getElementById('right-members').innerHTML = json.content.right + ' members';
	} else if(json.type === 'threshold') {
	    current_threshold = json.content;
	    for(var i = 0; i < thresholds.length; i++) {
		var thr = thresholds[i];
		var elem = document.getElementById('threshold-' + thr);
		if(thr === json.content){
		    elem.style.background = '#FF9090';
		}else{
		    elem.style.background = '';
		}
	    }
	}
    };


    for(var i = 0; i < thresholds.length; i++) {
	var handlerGen = function(thr){
	    var handler = function(event) {
		ws.send(thr);
	    }
	    return handler;
	}
	var thr = thresholds[i];
	document.getElementById('threshold-' + thr).addEventListener('click', handlerGen(thr));	
    }
    
}, false );
