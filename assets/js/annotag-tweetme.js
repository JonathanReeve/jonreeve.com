var mocklocation1 = 'https://books.google.com/books?id=WVofz29Hx9UC&printsec=frontcover&dq=joyce&hl=en&sa=X&ei=1NE7VY6hOM-4oQScnICoAw&ved=0CB4Q6AEwAA#v=onepage&q=joyce&f=false';

// test location from Worldcat
var mocklocation2 = 'http://www.worldcat.org/title/ulysses-the-corrected-text/oclc/13278856&referer=brief_results'

// test location without referer
var mocklocation3 = 'http://www.worldcat.org/title/ulysses-the-corrected-text/oclc/13278856'

// test location for Project Gutenberg books
var mocklocation4 = 'http://www.gutenberg.org/files/4300/4300-h/4300-h.htm'

var testlocation = mocklocation4;

console.log(window.location.href); 


// Are we on a Google Books page? 
var gb = /books\.google\.com.*(id=)(.*?)\&/;

// Are we on a Worldcat page? 
var wc = /worldcat\.org.*\/oclc\/(\d+)($|\&)/;

// Are we on Project Gutenberg? 
var pg = /gutenberg\.org.*\/files\/(\d+)($|\/)/;

if (gb.test(testlocation) == true) {
    bookcode = 'B' + RegExp.$2;
} else if (wc.test(testlocation) == true) {
    bookcode = 'o' + encodeNums([parseInt(RegExp.$1)]);
} else if (pg.test(testlocation) == true) {
    bookcode = 'g' + encodeNums([parseInt(RegExp.$1)]);
}

// Now make the annotag
var annotag = '';
if ('' !== bookcode) {
    console.log('now taking you to: ');
    twitterURL = 'https://twitter.com/intent/tweet?hashtags=' + bookcode;
    console.log(twitterURL);
}
