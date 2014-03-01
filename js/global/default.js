(function(){
  function search_in_site(){
    var field = document.querySelector('nav .search-post input');
    var query = encodeURIComponent(field.value);
    window.location.href = '/search.html?q=' + query;
    return false;
  }

  window.addEventListener('load', function(){
    var search = document.querySelector('nav .search-post');
    search.onsubmit = function(){return search_in_site();};
  }, false);
})();
