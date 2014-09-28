---
layout: page
title: 'Annotag Calculator' 
permalink: /projects/annotags/ 
---

Make your tweetable book location annotags with this calculator. [Read the introductory blog post here](/projects/annotags/about.html) for an explanation of annotags. If you choose "ISBN" as your bookcode format below, this calculator will turn your unwieldy 10-digit ISBN into a shortened, more easily typable form, more suitable for squeezing into a tweet. For example, the ISBN `0743482786` will become the five-character code `EtVPZ`. 

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

Enter the bookcode below to generate the decoded ISBN. Please note that the bookcode is the part that comes after the `i`, such that for an annotag like `#iEtVPZ:p12`, the bookcode is `EtVPZ`. 

<section id="decoder"> 
	<form> 
		<input id="to_be_decoded" class="input" type="text" name="to_be_decoded" /> 
	</form> 
	<hr/>
	<label for="decoder_out">Decoded ISBN:</label>
	<input type="text" id="decoder_out" placeholder="decoded ISBN" name="decoder_out"/> 
</section> 


##Technical Details 

This calculator validates your ISBN-10 by doing some modular arithmatic and looking at the [checkdigit](http://en.wikipedia.org/wiki/International_Standard_Book_Number#ISBN-10_check_digit_calculation). It then removes the checkdigit, as well as any leading zeros, and encodes the remaining digits with a base-63 encoding scheme, mapping the numbers onto numbers, letters, and underscores. It only handles ISBN-10s at the moment, but ISBN-13s are in the works. You can read the code for this page and all the other pages on this site [here on github](https://github.com/JonathanReeve/JonathanReeve.github.io). 

<script src="{{ site.url }}/assets/js/annotag-calculator.js"></script>
