pairFlip([],[]).
pairFlip([X],[X]).
pairFlip([X,Y|Tail], [Y,X|NewTail]) :-
    pairFlip(Tail,NewTail).

sublistSum([],0,[]).
sublistSum([X|Xs],Sum,[X|Sublist]) :-
    Leftover is Sum - X,
    sublistSum(Xs, Leftover, Sublist).
sublistSum([_|Xs],Sum,Sublist) :-
    sublistSum(Xs,Sum,Sublist).

/*
legalCourse(CourseList) :-
	legal1(CourseList),
	legal2(CourseList).
	
% legal1(CourseList) means CourseList does not have a
% projects with the same name twice (i.e. john working with john).
% (helper for legalCourse)
legal1([]).
legal1([Name1+Name2+_|Tail]) :-
	Name1 \= Name2,
	legal1(Tail).
	
% legal2(CourseList) means CourseList does not have any
% projects with the same pair of students, in either order
% (helper for legalCourse)
legal2([]). 
legal2([_]).
legal2([Project|More]) :-
	noMatch(Project,More),
	legal2(More).
	
% noMatch (Project,List) means there is no project in List done
% by both the same students as the pair in Project
noMatch(_,[]).
noMatch(ProjectA,[ProjectB|More]) :-
	diffPair(ProjectA,ProjectB),
	noMatch(ProjectA,More).

% diffPair(Project1,Project2) is true if Project1 and Project2
% were NOT done by the same pair of students.  One of the student
% in Project 1 may be in Project2, but not both.
diffPair(A+_+_,Project) :-
	notInProject(A,Project).
diffPair(_+B+_,Project) :-
	notInProject(B,Project).
    */