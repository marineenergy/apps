var url ='https://marinecadastre.gov/data/';
  var page = new WebPage(); var fs = require('fs');
  // open page, wait 5000 miliseconds, write html
  page.open(url, function (status) {
    just_wait();
  });
  function just_wait() {
    setTimeout(function() {
      fs.write('scraped.html', page.content, 'w');
      phantom.exit();
    }, 5000);
  }
  
