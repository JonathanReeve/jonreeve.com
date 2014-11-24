---
layout: page
title: 'Annotag Calculator' 
permalink: /projects/annotags/ 
customjs: 
 - annotag-calculator.js
---

Make your tweetable book location annotags with this calculator. [Read the introductory blog post here](/projects/annotags/about.html) for an explanation of annotags. If you choose "ISBN" as your bookcode format below, this calculator will turn your unwieldy 10- or 13-digit ISBN into a shortened, more easily typable form, more suitable for squeezing into a tweet. For example, the ISBN `0743482786` will become the five-character code `FB7Xw`. 

<section id="calculator"> 
	<form>
		<select class="input" id="code_type">
			<option value="I">ISBN</option>
			<option value="G">Project Gutenberg Book ID</option>
			<option value="B">Google Books ID</option>
		</select>
		<input id="raw_code" class="input" type="text" name="code" />
		<input id="page" class="input" type="text" name="location_type" placeholder="Page number:"/>
		<input id="paragraph" class="input" type="text" name="paragraph" placeholder="Paragraph number:" />
		<input id="line" class="input" type="text" name="line" placeholder="Line number:" />
	</form>
	<hr/>
	<label for="output">Generated annotag:</label> 
	<input type="text" id="output" placeholder="generated annotag here" name="output"/> 
</section> 

#Bookcode Decoder

Enter the bookcode below to generate the decoded ISBN. Please note that the bookcode is the part that comes after the `i`, such that for an annotag like `#iFB7Xw:p12`, the bookcode is `FB7Xw`. 

<section id="decoder"> 
	<form> 
		<input id="to_be_decoded" class="input" type="text" name="to_be_decoded" placeholder="Enter book code here. Don't include the pound sign or code type digit."/> 
	</form> 
	<hr/>
	<label for="decoder_out">Decoded ISBN-10:</label>
	<input type="text" id="decoder_out" placeholder="decoded ISBN-10" name="decoder_out"/> 
	<label for="decoder_out">Decoded ISBN-13:</label>
	<input type="text" id="decoder_out13" placeholder="decoded ISBN-13" name="decoder_out13"/> 
</section> 


##Technical Details 

This calculator validates your ISBN by doing some modular arithmatic and looking at the [checkdigit](http://en.wikipedia.org/wiki/International_Standard_Book_Number#ISBN-10_check_digit_calculation). It then removes the checkdigit, as well as any leading zeros, and encodes the remaining digits with a base-62 encoding scheme, mapping the numbers onto numbers and letters. ISBN-13s that begin in 978 are automatically converted to ISBN-10s. You can read the code for this page and all the other pages on this site [here on github](https://github.com/JonathanReeve/JonathanReeve.github.io). 

##Updates

 * 2014-10-12: Version 1.1. Now supports ISBN-13s, and decodes bookcodes into both ISBN-10s and ISBN-13s. 

<script src="{{ site.url }}/assets/js/annotag-calculator.js"></script>
