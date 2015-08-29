
var separate_with = ':';
var encodable = get_map('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'); 
var base10 = get_map('0123456789')

var mocklocation1 = 'https://books.google.com/books?id=WVofz29Hx9UC&printsec=frontcover&dq=joyce&hl=en&sa=X&ei=1NE7VY6hOM-4oQScnICoAw&ved=0CB4Q6AEwAA#v=onepage&q=joyce&f=false';

// test location from Worldcat
var mocklocation2 = 'http://www.worldcat.org/title/ulysses-the-corrected-text/oclc/13278856&referer=brief_results'

// test location without referer
var mocklocation3 = 'http://www.worldcat.org/title/ulysses-the-corrected-text/oclc/13278856'

// test location for Project Gutenberg books
var mocklocation4 = 'http://www.gutenberg.org/files/4300/4300-h/4300-h.htm'

var testlocation = window.location.href;

console.log(window.location.href); 

// Are we on a Google Books page? 
var gb = /books\.google\.com.*(id=)(.*?)\&/;

// Are we on a Worldcat page? 
var wc = /worldcat\.org.*\/oclc\/(\d+)($|\&)/;

// Are we on Project Gutenberg? 
var pg = /gutenberg\.org.*\/files\/(\d+)($|\/)/;

// Are we on Goodreads? 
var gr = /goodreads\.com.*\/book\/show\/(.*)/;

if (gb.test(testlocation) == true) {
    bookcode = 'B' + RegExp.$2;
} else if (wc.test(testlocation) == true) {    
    bookcode = 'o' + encodeNums([parseInt(RegExp.$1)]);
} else if (pg.test(testlocation) == true) {
    bookcode = 'g' + encodeNums([parseInt(RegExp.$1)]);
} else if (gr.test(testlocation) == true) {
    bookcode = getGoodreadsIsbn(testlocation);       
    bookcode = 'i' + encodeNums([parseInt(bookcode)]); 
} else { 
    alert( 'Sorry, I can\'t find any bibliographic information in this URL.' ); 	
    die(); 
} 

// Now make the annotag
if ('' !== bookcode) {
    console.log('now taking you to: ');
    twitterURL = 'https://twitter.com/intent/tweet?hashtags=' + bookcode;
    console.log(twitterURL);
    window.location = twitterURL; 
}


/// Now encoding stuff!



function get_map(s) {
    d = {}
    for (var i=0; i<s.length; i++) {
        d[s.charAt(i)] = i}
    d.length = s.length
    d._s = s
    return d
}

// stolen from http://stackoverflow.com/a/1268377/584121  
function baseconvert(number, fromdigits, todigits) {
    var number = String(number)

    if (number.charAt(0) == '-') {
        number = number.slice(1, number.length)
        neg=1}
    else {
        neg=0}

    // make an integer out of the number
    var x = 0
    for (var i=0; i<number.length; i++) {
        var digit = number.charAt(i)
        x = x*fromdigits.length + fromdigits[digit]
    }

    // create the result in base 'todigits.length'
    res = ""
    while (x>0) {
        remainder = x % todigits.length
        res = todigits._s.charAt(remainder) + res
        x = parseInt(x/todigits.length)
    }

    if (neg) res = "-"+res
    return res
}

function encodeNums(L) {
    var r = []
    for (var i=0; i<L.length; i++) {
         r.push(baseconvert(L[i], base10, encodable))
    }
    return r.join(separate_with)
}

function getGoodreadsIsbn() {
   var html = document.getElementsByTagName('html')[0];
   var elements = html.getElementsByTagName('span'), i = 0, isbn;

   for(i; i < elements.length; i++) {
       prop = elements[i].getAttribute('itemprop');       

       if(prop == "isbn") {
           isbn = elements[i].innerHTML;
           isbn = isbn.substring(0, 12);
           isbn = isbn.substring(3);
           break;
       }
   }
   
    return isbn;
}

