window.addEventListener( 'load', function(event){
    
    var reporter_url = 'http://' + location.host + '/';
    var reporter_qr_url = 'https://chart.googleapis.com/chart' + '?chs=300x300&cht=qr&chl=' + reporter_url;

    document.getElementById('rep-url').innerHTML = reporter_url;
    document.getElementById('rep-qr').setAttribute('src',reporter_qr_url);
    
}, false );
