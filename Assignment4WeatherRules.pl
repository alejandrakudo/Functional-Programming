% CISC 260: Assignment 4 
% Alejandra Kudo, 10136014, 13aak15


/*match: match(Month,Day,Word) should be true if there is a fact 
*for the given month and day that contains the given Word*/

match(Month, Day, Word) :-
	weather(Month, Day, Word, _ , _); 
	weather(Month, Day,_, Word, _); 
	weather(Month, Day,_, _, Word).


/*wet: wet(Month,Day) should be true if there is a fact telling us that 
*there was either rain or snow for that day*/
wet(Month, Day) :- 
	match(Month, Day, rain);
	match(Month, Day, snow).



/* desert: desert(Month,Day) should be true if the weather on the given
*day was both hot and dry */
desert(Month, Day) :- 
	match(Month, Day, hot), 
	match(Month, Day, dry).


% tropical: tropical(Month,Day) should be true if the day was hot and had some rain
tropical(Month, Day) :- 
	match(Month, Day, hot), 
	match(Month, Day, rain).



% impossible: impossible(Month, Day) should be true if the day was hot or warm but had snow
impossible(Month, Day) :- 
	(match(Month, Day, hot); 
	match(Month, Day, warm)), 
	match(Month, Day, snow).


/*summer: summer(Month) should be true if the month is jun, jul, or aug*/
summer(jun). 
summer(jul).
summer(aug).


/*winter: winter(Month) should be true if the month is jan, feb or mar */
winter(jan).
winter(feb). 
winter(mar).



/*unseasonable: unseasonable(Month, Day) should be true if there was 
unseasonable weather on that day. Meaning a warm or hot day in the winter
or cool or cold day in the summer*/

unseasonable(Month, Day) :- 
	(match(Month, Day, hot);
	match(Month, Day, warm)), 
	winter(Month).

unseasonable(Month, Day) :-
	(match(Month, Day, cool);
	match(Month, Day, cold)), 
	summer(Month).


/* mixed: mixed(Month) should be true if there was at least one hot day 
and at least one cold day in the month */
mixed(Month) :- 
	match(Month, Day, hot), 
	match(Month, Day, cold).



