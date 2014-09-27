
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
var encodable = get_map('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!$%^&*()_+={}[]\\|;<>.?/~'); // - is reserved for negatives obviously :-P
var base10 = get_map('0123456789')

// UNCOMMENT ME for length/speed testing in a wider base!
// You may wish to experiment with the ranges for a happy medium between bandwidth and DB space :-P
/*var encodable = ''
for (var i=1; i<128; i++) {
    encodable += String.fromCharCode(i)
}
encodable = get_map(encodable)*/

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
    var r = []
    var s = s.split(separate_with)
    for (var i=0; i<s.length; i++) {
         r.push(parseInt(baseconvert(s[i], encodable, base10)))
    }
    return r
}

$('.input').on('input change', function () {
    var code_type = $('#code_type').val();
    var raw_code = $('#raw_code').val();
    var page = $('#page').val();
    var paragraph = $('#paragraph').val();
    var line = $('#line').val();
    var location = page + paragraph + line;

    if (code_type == 'I') {
        // I originally wanted to use base64 encoding here, 
        // but many of the libraries I used would actually add
        // length to the bookcode! So here's unicode encoding. It's great 
       // at getting the length down, but has a few problems. Suggestions welcome!
        var bookcode = encodeNums([parseInt(raw_code)]); 
    } else {
        bookcode = raw_code;
    }
    out = code_type + bookcode;
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
 
