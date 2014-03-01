(function(){

  window.addEventListener('load', function(){
    var url = purl(window.location)
    var subbrand  = document.querySelector('nav .navbar-subbrand');
    var searchBox = document.querySelector('nav .search-post input');
    var column    = document.querySelector('.posts-column .posts');
    var queries   = decodeURI(url.param('q'));
    var threshold = url.param('t') || 0.8;

    subbrand.innerHTML = 'Search: ' + queries;
    searchBox.value    = queries;
    queries = queries.split(/\s+/);

    mini.ajax.get("/search.json", function(r){
      var data  = JSON.parse(r);
      var dict  = data.dict;
      var posts = data.posts;

      var results = []
        for(var i = 0; i < posts.length; i++){
          results.push([]);
        }

      for(var qid = 0; qid < queries.length; qid++){
        var query = queries[qid];
        if(query.length <= 0) continue;

        var scores = [];
        for(var i = 0; i < posts.length; i++){
          scores.push([0, '']);
        }

        for(var key in dict){
          var l = new Levenshtein(key.toLowerCase(), query.toLowerCase());
          var score = key.length > query.length? key.length: query.length;
          score = (score - l.distance) / score;

          for(var i = 0; i < dict[key].length; i++){
            var pi = dict[key][i];
            if(score > scores[pi][0]) scores[pi] = [score, key];
          }
        }

        for(var i = 0; i < posts.length; i++){
          results[i].push(scores[i]);
        }

      }

      for(var pid = 0; pid < posts.length; pid++){
        var score = 1;
        for(var i = 0; i < results[pid].length; i++){
          if(results[pid][i][0] < score) score = results[pid][i][0];
        }
        if(score < threshold) continue;

        var result = posts[pid];

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

        var tags  = document.createElement('div');
        tags.innerHTML = result.tags;

        var body  = document.createElement('div');
        body.className = 'post-body';
        body.innerHTML = ' ' + result.description + ' ...';

        column.appendChild(post);
          post.appendChild(header);
            header.appendChild(title);
              title.appendChild(link);
            header.appendChild(date);
            header.appendChild(tags);
          post.appendChild(body);

      }
    }); // mini.ajax.get

  }); // window.addeventlistener
})();
