(function(){
  window.addEventListener('load', function(){
    var url = purl(window.location)
    var subbrand  = document.querySelector('nav .navbar-subbrand');
    var searchBox = document.querySelector('nav .search-post input');
    var column    = document.querySelector('.posts-column .posts');
    var query     = decodeURI(url.param('q')).toLowerCase();

    subbrand.innerHTML = 'Search: ' + query;
    searchBox.value    = query;

    mini.ajax.get("/search.json", function(r){
      var d = JSON.parse(r);

      for(var i = 0; i < d.pages.length; i++){
        var ts = d.pages[i].tags.split(',');
        for(var j = 0; j < ts.length; j++){
          var t = ts[j].replace(/^\s+|\s+$/g, '').toLowerCase();
          if(t in d.dict){
            d.dict[t].push(i);
          } else {
            d.dict[t] = [i];
          }
        }
      }

      var is = d.dict[query];
      if(!is) return false;
      var added = [];

      for(var i = 0; i < is.length; i++){
        if (added.indexOf(is[i]) >= 0) break;
        added.push(is[i]);
        var result = d.pages[is[i]];

        var post = document.createElement('div');
        post.className = "post search-result";

        var header = document.createElement('div');
        header.className = "page-header";

        var title = document.createElement("h1");
        var link  = document.createElement('a');
        link.href = result.url;
        link.innerHTML = result.title;

        var date  = document.createElement('div');
        date.className = 'post-date';
        date.innerHTML = 'Posted on ' + result.date;

        var body  = document.createElement('div');
        body.className = 'post-body';
        body.innerHTML = result.description + ' ...';

        column.appendChild(post);
          post.appendChild(header);
            header.appendChild(title);
              title.appendChild(link);
            header.appendChild(date);
          post.appendChild(body);

      }

    }); // mini.ajax.get

  }); // window.addeventlistener
})();
