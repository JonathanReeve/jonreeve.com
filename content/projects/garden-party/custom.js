$(document).ready(function(){
    var counter = 0;
    $('.tag').click(function(){
	    // get the specific tag name, i.e. extract "colors" from `.tag.colors` 
	    var tagName = $(this)[0].classList[1]; 
	    // create a jQuery object from the extracted tag name
	    var scrollTo = $('.' + tagName); 
	    // placeholder so we can access the currently clicked object from within the `each` loop
	    var thisTag = this; 
	    // loop through each object with the tag name. Find ours, then find the next one. 
	    scrollTo.each(function(i, e){
		    if (e == thisTag) {
			    counter = i + 1;
		    }
	    });
	    // If it's the last object, go back to the top. 
	    if (counter >= scrollTo.length) {
		    counter = 0;
	    }
	    // Select only the object we found using the above process. 
	    scrollTo = scrollTo[counter]; 
	    // Scroll to the selected object. 
	    $('html, body').animate({ scrollTop: $(scrollTo).parent().offset().top });
    }); 
}); 
