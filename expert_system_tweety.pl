% Code for an Expert System Shell:

% See Bratko pages 353-356.
%
% The code below is for the expert system shell. This is the shell that you need to use
% to write your own expert system; that is, your task will be to write the rules
% which use the shell. Get the shell running in Prolog and test using Bratko’s
% examples.
%
% Interaction with user and why and how explanation

% Operators for easy to read rules.
:- use_module(library(dom)).

:- dynamic(fact/1).
:- dynamic(already_asked/1).

prove(true) :- !.
prove((B, Bs)) :- !,
    prove(B),
    prove(Bs).
prove(H) :-
    clause(H, B),
    prove(B).
prove(H) :-
    askable(H),
    writeln(H),
    read(Answer),
	Answer == yes.


good_pet(X) :- bird(X), small(X).
good_pet(X) :- cuddly(X), yellow(X).

bird(X) :- has_feathers(X), tweets(X).

yellow(tweety).

askable(tweets(_)).
askable(small(_)).
askable(cuddly(_)).
askable(has_feathers(_)).

% Rules based on natural language expressions

% is_true(P, Proof): Proof is a proof that P is true

is_true(P, Proof)  :-
	explore(P, Proof, []), !.

%  explore(P, Proof, Trace):
%     Proof is an explanation for P, Trace is a chain of rules between P's ancestor goals

explore(P, P, _)  :-
	fact(P).						%  P is a fact

explore((P1, P2), (Proof1, Proof2), Trace)  :-  !,
	explore(P1, Proof1, Trace),
	explore(P2, Proof2, Trace).

explore((P1; P2), Proof, Trace)  :-  !,
	(
		explore(P1, Proof, Trace)
		;
		explore(P2, Proof, Trace)
	).

explore(P, P :- CondProof, Trace)  :-
	clause(P, Cond),					%  A rule relevant to P 
	explore(Cond, CondProof, [ P :- Cond | Trace]).

explore(P, Proof, Trace)  :-
	askable(P),						% P may be asked of user
	\+ fact(P),						% P not already known factS
	\+ already_asked(P),					% P not yet asked of user
	ask_user(P, Proof, Trace).

add_message(Out, Html) :-
	get_by_id(Out, Question),
	create(p, PHtml),
	set_html(PHtml, Html),
	append_child(Question, PHtml).

test(P, Proof, Trace) :-
	FP := waitForClick(),
	await(FP, Text),
	_ := console.log(Text),
	process_answer(Text, P, Proof, Trace).

ask_user(P, Proof, Trace)  :-
	get_by_id(submit, Button),
	add_message(expert_message, P),
	FP := waitForClick(),
	await(FP, Text),
	_ := console.log(Text),
	process_answer(Text, P, Proof, Trace).
	% test(P, Proof, Trace).
	% bind(Button, click, _, test(Text, P, Proof, Trace)).

process_answer(yes, P, P  :- was_told, _)  :-	% User told P is true
	_ := console.log("yes"),
	asserta(fact(P)),
	asserta(already_asked(P)).

process_answer(no, P, _, _)  :-
	_ := console.log("no"),
	asserta(already_asked(P)),				% Make sure not to ask again about P
	fail.						            % User told P is not true

process_answer(why, P, Proof, Trace)  :-		% User requested why-explanation
	_ := console.log("why"),
	display_rule_chain(Trace, 0), nl,
	ask_user(P, Proof, Trace).				% Ask about P again

% process_answer(_, P, _, _) :-
% 	_ := console.log("FOO").

display_rule_chain([], _).

display_rule_chain([P :- C | Rules], Indent)  :-
	string_concat('To explore wether ', P, Tmp),
	string_concat(Tmp, ' is true, using rules: ', Tmp2),
	string_concat(Tmp2, P, Msg),	
	add_message(expert_message, Msg),
	% wc_html(p(['To explore whether ', P, ' is true, using rules:', C])),
	NextIndent is Indent + 2,
	display_rule_chain(Rules, NextIndent).

:- dynamic already_asked/1.
% :- initialization(main).
