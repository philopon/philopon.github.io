(function(){function n(){var n=document.querySelector("nav .search-post input");var e=encodeURIComponent(n.value);window.location.href="/search.html?q="+e;return false}window.addEventListener("load",function(){var e=document.querySelector("nav .search-post");e.onsubmit=function(){return n()}},false)})();