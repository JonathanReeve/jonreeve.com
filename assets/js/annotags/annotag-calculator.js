
/* Calculator for creating annotags! */ 

function get_map(s) {
    d = {}
    for (var i=0; i<s.length; i++) {
        d[s.charAt(i)] = i}
    d.length = s.length
    d._s = s
    return d
}

var separate_with = ':';
var encodable = get_map('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'); 
var base10 = get_map('0123456789')

// UNCOMMENT ME for length/speed testing in a wider base!
// You may wish to experiment with the ranges for a happy medium between bandwidth and DB space :-P
/*var encodable = ''
for (var i=1; i<128; i++) {
    encodable += String.fromCharCode(i)
}
encodable = get_map(encodable)*/


// stolen from http://stackoverflow.com/a/1268377/584121  
function zeroPad(num, numZeros) {
	var n = Math.abs(num);
	var zeros = Math.max(0, numZeros - Math.floor(n).toString().length );
	var zeroString = Math.pow(10,zeros).toString().substr(1);
	if( num < 0 ) {
		zeroString = '-' + zeroString;
	}

	return zeroString+n;
} 
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

function decodeNums(s) {
	var r = []; 
    var s = s.split(separate_with)
    for (var i=0; i<s.length; i++) {
         r.push(parseInt(baseconvert(s[i], encodable, base10)))
    }
    return r
}

/* @param str
 * @return bool
 */ 
function isValidISBN(isbnstr) { 
	if ( ! ( ( isbnstr.length = 10 ) || ( isbnstr.length = 13 ) ) ) return false; 
	if ( 10 == isbnstr.length ) { 
		return _isValidISBN10(isbnstr); 
	} else { 
		return _isValidISBN13(isbnstr); 
	} 
} 

/* @param str
 * @return bool
 */ 
function _isValidISBN10(isbnstr) { 
	var checkdigit = isbnstr.substring(9) 
	return ( checkdigit == makeCheckDigit(isbnstr) ) ? true : false; 
} 

/* @param str
 * @return bool
 */ 
function _isValidISBN13(isbnstr) { 
	var checkdigit = isbnstr.substring(12) 
	return ( checkdigit == makeCheckDigit13(isbnstr) ) ? true : false; 
} 

$('#calculator .input').on('input change', function () {
    var code_type = $('#code_type').val();
    var raw_code = $('#raw_code').val();
    var page = $('#page').val();
    var paragraph = $('#paragraph').val();
    var line = $('#line').val();
    var location = page + paragraph + line;

    if (code_type == 'I') {

	    raw_code = raw_code.replace(/\-/g, ''); 
	    if (isValidISBN(raw_code)) { 
		    $('#raw_code').removeClass('isbn_error').addClass('isbn_ok'); 
	    } else { 
		    $('#raw_code').removeClass('isbn_ok').addClass('isbn_error'); 
	    } 	    

	    if ( 10 == raw_code.length ) { 
		    var bookcode = raw_code.substring(0, 9); // remove check digit
	    } else if ( 13 == raw_code.length ) { 
		    var bookcode = raw_code.substring(0, 12); // remove check digit
		    if ( '978' == raw_code.substring(0, 3) ) { 
			    // remove '978' as unnecessary. Use ISBN-10 form. 
			    bookcode = bookcode.substring(3); 
		    } 
	    } else { 
		    var bookcode = raw_code; 
	    } 

	    bookcode = encodeNums([parseInt(bookcode)]); 
	    out = 'i' + bookcode;
    } else {
        bookcode = raw_code;
	out = code_type + bookcode;
    }
    if (location) {
        out += ":";
    }
    if (page) {
        out += "p" + page;
    }
    if (paragraph) {
        out += "P" + paragraph;
    }
    if (line) {
        out += "l" + line;
    }
    $('#output').val("#" + out);
});
 
/* @param str
 * @return str
 */ 
function makeCheckDigit(isbn9){ 
	var sum = 0; 
	for (i=0; i<9; i++) { 
	   sum = sum + (10-i) * parseInt(isbn9.charAt(i)); 
	} 
	var mycheckdigit = 11 - ( sum % 11 ); 
	var mycheckdigitstr = mycheckdigit.toString();  
	if ( '10' == mycheckdigitstr ) { 
		mycheckdigitstr = 'X'; 
	} 
	return mycheckdigitstr; 
} 

/* @param str
 * @return str
 */ 
function makeCheckDigit13(isbn13){ 
	var sum = 0;  
	for (i=0; i<12; i++) { 
		if (!(i%2)) { // even
			sum = sum + parseInt(isbn13.charAt(i)); // multiply by one	
		} else { // odd
			sum = sum + ( 3 * parseInt(isbn13.charAt(i)) ); // multiply by three
		} 
	} 
	var mycheckdigit = 10 - ( sum % 10 ); 	
	var mycheckdigitstr = mycheckdigit.toString();  
	return mycheckdigitstr
} 

$('#to_be_decoded').on('change input', function() { 
	var to_be_decoded = $('#to_be_decoded').val(); 
	var out = decodeNums(to_be_decoded); 
	var outs = zeroPad(out, 9); // sometimes ISBNs have leading zeros 
	
	if ( outs.length < 11 ) { 
		var isbn10_out = outs + makeCheckDigit(outs); 
		$('#decoder_out').val(isbn10_out); 
		var isbn13_out = '978' + outs; 
	} else { // it's probably a ISBN-13 
		$('#decoder_out').val('n/a'); 
		var isbn13_out = outs; 
	} 

	isbn13_out = isbn13_out + makeCheckDigit13(isbn13_out); 
	$('#decoder_out13').val(isbn13_out); 
}); 
