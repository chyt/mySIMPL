/**----------------------------------------------------------------------------//
 * CS 345H - Assignment 3
 *   Name:   Lyee Chong
 *   UT EID: lsc568
 *   CS ID:  lchong
 **----------------------------------------------------------------------------*/

/**----------------------------------------------------------------------------//
 * parse the token list, via Prolog's phrase
 **----------------------------------------------------------------------------*/ 
 parse(TokenList,AST) :- %works
	logln('starting parse...'),
	phrase(prog(AST), TokenList),
	logln('parse complete!'),
	logln('').
 
/**----------------------------------------------------------------------------//
 * prog, assignment, base, identifier, factor, retStatement
 **----------------------------------------------------------------------------*/
% prog(ret)
prog(prog(RS)) --> % works
	retStatement(RS), 
	['.'].

% prog(func, prog)
prog(prog(FUNC,PR)) --> % works
	functionDecl(FUNC), [';'],
	prog(PR).

% prog(stat, prog)
prog(prog(STAT,PR)) --> % works
	statement(STAT), [';'],
	prog(PR).

% func
functionDecl(funcDecl(FUNCID,PARAMID,PR)) --> % works
	[function],
	funcId(FUNCID),
	['('], paramId(PARAMID), [')'],
	['{'], prog(PR), ['}'].	
	
% stat(X)
statement(STAT) --> 
	assignment(STAT); % works
	conditional(STAT);
	loop(STAT).	
	
assignment(assign(ID,BASE)) --> % works
	identifier(ID), [':='], 
	base(BASE).
	
conditional(conditional(COND,STATSEQ1,STATSEQ2)) -->  % works
	[if],
	['('], condition(COND), [')'],
	[then],
	statementSeq(STATSEQ1),
	[else],
	statementSeq(STATSEQ2),
	[endif].
	
loop(loop(COND,STATSEQ)) --> % works
	[while],
	['('], condition(COND), [')'],
	[do],
	statementSeq(STATSEQ),
	[done].
	
statementSeq(statementSeq(STAT)) --> % works
	statement(STAT),
	['.'].

statementSeq(statementSeq(STAT1, STAT2)) --> % works
	statement(STAT1),
	[';'],
	statementSeq(STAT2).
	
condition(condition(BASE1,COMP,BASE2)) --> % works
	base(BASE1),
	comparator(COMP),
	base(BASE2).
	
base(K) --> % works
	constant(K);
	identifier(K);
	['('], expr(K), [')'];
	funcCall(K).
	
funcCall(funcCall(FUNCID,BASE)) --> % works
	funcId(FUNCID),
	['('], base(BASE), [')'].
	
funcId(FUNCID) --> % works
	[FUNCID],
	{ not(number(FUNCID)) },
	{ not(invalId(FUNCID)) }.
	
paramId(paramId(X)) --> % works
	[X],
	{ not(number(X)) },
	{ not(invalId(X)) }.
	
constant(const(X)) --> % works
	[X],
	{ number(X) }.
	
identifier(id(X)) --> % works
	[X],
	{ not(number(X)) },
	{ not(invalId(X)) }.
	
factor(factor(BASE)) --> % works
	base(BASE).
	
retStatement(ret(BASE)) --> % works
	[return],
	base(BASE).
	
comparator(comp(SYM)) --> % works
	{ comparatorSym(SYM) },
	[SYM].
	
/**----------------------------------------------------------------------------//
 * invalid identifiers
 **----------------------------------------------------------------------------*/
	
invalId(+).
invalId(-).
invalId(*).
invalId(/).
invalId(:=).
invalId(return).
invalId(X) :- comparatorSym(X). % it's invalid if it's a comparator symbol

/**----------------------------------------------------------------------------//
 * valid comparator symbols
 **----------------------------------------------------------------------------*/

comparatorSym('==').
comparatorSym('<').
comparatorSym('>').
comparatorSym('>=').
comparatorSym('<=').
comparatorSym('!=').

/**----------------------------------------------------------------------------//
 * add and substract stuff. Works AFIK
 **----------------------------------------------------------------------------*/
expr(E) --> term(T), [+], left_assoc(E,T,plus).
expr(E) --> term(T), [-], left_assoc(E,T,minus).
expr(E) --> term(E).
left_assoc(expression(plus,T,T1),T,plus) --> term(T1).
left_assoc(expression(minus,T,T1),T,minus) --> term(T1).
left_assoc(expression(plus,T,T1),T,minus) --> term(T1).
left_assoc(expression(minus,T,T1),T,plus) --> term(T1).
left_assoc(E,T,minus) --> term(T1), [-], left_assoc(E,expression(minus,T,T1),minus).
left_assoc(E,T,plus) --> term(T1), [-], left_assoc(E,expression(plus,T,T1) ,minus).
left_assoc(E,T,minus) --> term(T1), [+], left_assoc(E,expression(minus,T,T1),plus).
left_assoc(E,T,plus) --> term(T1), [+], left_assoc(E,expression(plus,T,T1) ,plus).

/**----------------------------------------------------------------------------//
 * multiply and divide stuff. Works AFIK
 **----------------------------------------------------------------------------*/
term(E) --> factor(T), [*], left_assoc(E,T,mult).
term(E) --> factor(T), [/], left_assoc(E,T,divide).
term(E) --> factor(E).
left_assoc(term(mult,T,T1),T,mult) --> factor(T1).
left_assoc(term(divide,T,T1),T,divide) --> factor(T1).
left_assoc(term(mult,T,T1),T,divide) --> factor(T1).
left_assoc(term(divide,T,T1),T,mult) --> factor(T1).
left_assoc(E,T,divide) --> factor(T1), [/], left_assoc(E,term(divide,T,T1),divide).
left_assoc(E,T,mult) --> factor(T1), [/], left_assoc(E,term(mult,T,T1) ,divide).
left_assoc(E,T,divide) --> factor(T1), [*], left_assoc(E,term(divide,T,T1),mult).
left_assoc(E,T,mult) --> factor(T1), [*], left_assoc(E,term(mult,T,T1) ,mult).

/**----------------------------------------------------------------------------//
 * sanity checking statements
 **----------------------------------------------------------------------------*/
 
sanity_check_ASL(ASL_to_check, Rule_name) :-
	(
		is_assoc(ASL_to_check) ->
			(
				verboseASLSanityChecks ->
					log('passed ASL sanity check for '), logln(Rule_name)
				;
					true
			)
		;
			log('failed ASL sanity check!!!'), logln(Rule_name),
			true
	).
	
/**----------------------------------------------------------------------------//
 * verbose eval
 **----------------------------------------------------------------------------*/

% sets whether logging output is shown or not
verbose :- true.
sleepsOn :- true.
verboseASLSanityChecks :- false.

/**----------------------------------------------------------------------------//
 * dofail is used for testing. Switch to fail when done.
 **----------------------------------------------------------------------------*/

dofail :- fail.

pause(X) :- (sleepsOn -> sleep(X); true).

/**----------------------------------------------------------------------------//
 * logging functions
 **----------------------------------------------------------------------------*/

log(X) :-
	verbose -> write(X); true.
logln(X) :-
	verbose -> writeln(X); true.
logcall(X) :-
	log(X), logln(' called with params:'), pause(1).
logcallwithoutparams(X) :-
	log(X), logln(' called (no suitable params for printing).').
logparam(X) :-
	log('\t'), logln(X).
logfail(X) :-
	log('### FAILURE ### ----------- '), logln(X), dofail.
logwarn(X) :-
	log('# WARNING # --------------- '), logln(X).
lognotice(X) :-
	log('NOTICE -------------------- '), logln(X).
	
/**----------------------------------------------------------------------------//
 * evaluate stuff
 **----------------------------------------------------------------------------*/
 
% EVALUATE
% AST is the stuff parse spits out
% Number is the output value of the program
evaluate(AST, Number) :-
  logcall(evaluatetoplevel),
  logparam(AST),
	empty_assoc(ASL_vars_in), % used for values
	evaluateWithASL(AST, ASL_vars_in, ASL_vars_out, Number),
	true.
	
evaluateWithASL(AST, ASL_vars_in, ASL_vars_out, Number) :-
  logcall(evaluatewithASL),
  logparam(AST),
  logparam(ASL_vars_in),
	empty_assoc(ASL_funcs_in),
	sanity_check_ASL(ASL_vars_in, toplevelevaluate),
	sanity_check_ASL(ASL_funcs_in, toplevelevaluate),
	evalprog(AST, ASL_vars_in, ASL_vars_out, ASL_funcs_in, ASL_funcs_out),
	consolidateVar('return', ASL_vars_out, Number), % the return value is stored in KEY=return
	lognotice(functionfinishedwithReturnValue),
	logparam(Number),
	true.
	
% if we have something like x = y, and y = z and z = 5
% this method will figure out that x is 5
consolidateVar(Key, ASL_vars_in, Value) :- % ends the recursion
	number(Key),
	logcall(consolidatevar_end),
	logparam(Key),
  logparam(ASL_vars_in),
	sanity_check_ASL(ASL_vars_in, consolidatevar_end),
	Value = Key.
consolidateVar(Key, ASL_vars_in, Value) :- % continues the recursion
	logcallwithoutparams(consolidatevar_recurse),
	logparam(Key),
  logparam(ASL_vars_in),
	sanity_check_ASL(ASL_vars_in, consolidatevar_recurse),
	(
	  number(Key) ->
	      Value = Key
	    ;
	      not(number(Key)),
      	get_assoc(Key, ASL_vars_in, NextKey),
      	(
		    number(NextKey) ->
      			% we're done
            Value = NextKey
     			;
    	  		% mmm... are we expecting more recursion?
    				consolidateVar(NextKey, ASL_vars_in, Value),
    				logwarn('recursing further down the rabbit hole in consolidatevar..'),
    				true, fail
	      )
	),
	true.

% only should be called through topConsolidateVars
middleConsolidateVar(Key, ASL_vars_in, Value) :-
	logcall(middleconsolidatevar_evallll),
	logparam(Key),
	logparam(ASL_vars_in),
	logparam(Value),
	eval(Key, ASL_vars_in, ASL_vars_out, ASL_funcs_in, RetValue),
	(
		number(RetValue) ->
			Value = RetValue
		;
		consolidateVar(RetValue, ASL_vars_in, Value)
	),
	true.
middleConsolidateVar(id(Key), ASL_vars_in, Value) :-
	logcall(middleconsolidatevar_id),
	logparam(Key),
	logparam(ASL_vars_in),
	logparam(Value),
	(
		number(RetValue) ->
			Value = RetValue
		;
		consolidateVar(RetValue, ASL_vars_in, Value)
	),
	true.
middleConsolidateVar(const(Key), ASL_vars_in, Value) :-
	logcall(middleconsolidatevar_const),
	logparam(Key),
	logparam(ASL_vars_in),
	logparam(Value),
	(
		number(RetValue) ->
			Value = RetValue
		;
		consolidateVar(RetValue, ASL_vars_in, Value)
	),
	true.
	
topConsolidateVar(Key, ASL_vars_in, Value) :-
	logcall(topconsolidatevar),
	logparam(Key),
	logparam(ASL_vars_in),
	logparam(Value),
	(
		number(Key) ->
			Value = Key
		;
		middleConsolidateVar(Key, ASL_vars_in, Value)
	),
	true.
	
	
eval(Input, ASL_vars_in, ASL_vars_out, ASL_funcs_in, ASL_funcs_out) :-
	evalprog(Input, ASL_vars_in, ASL_vars_out, ASL_funcs_in, ASL_funcs_out).
	
% PROG RET
% prog(ret(A))
evalprog(prog(ret(Input)), ASL_vars_in, ASL_vars_out, ASL_funcs_in, ASL_funcs_out) :-
	logcall(progret),
	logparam(Input),
	pause(2),
	sanity_check_ASL(ASL_vars_in, progret),
	sanity_check_ASL(ASL_funcs_in, progret),
	evalreturn(ret(Input), ASL_vars_in, ASL_vars_out, ASL_funcs_in), % only needs funcs_in since no funcs will be declared in the return statement
	true.

% PROG STAT PROG
evalprog(prog(STAT, PROG), ASL_vars_in, ASL_vars_out, ASL_funcs_in, ASL_funcs_out) :-
	logcall(progstatprog),
	logparam(STAT),
	logparam(PROG),
	pause(2),
	sanity_check_ASL(ASL_vars_in, progstatprog_vars),
	sanity_check_ASL(ASL_funcs_in, progstatprog_funcs),	
	evalstat(STAT, ASL_vars_in, ASL_vars_param, ASL_funcs_in, ASL_funcs_temp),
	evalprog(PROG, ASL_vars_param, ASL_vars_out, ASL_funcs_temp, ASL_funcs_out), % now that the statement is done, eval the rest of the program (prog)
	true.
% ASSIGN
evalstat(assign(id(Id), Base), ASL_vars_in, ASL_vars_out, ASL_funcs_in, ASL_funcs_out) :-
	logcall(assign),
	logparam(Id),
	logparam(Base),
	eval(Base, ASL_vars_in, ASL_vars_out, ASL_funcs_in, RetValue), % base needs to be able to know what the variables are and functions, but not declare or assign them
	consolidateVar(RetValue, ASL_vars_in, ConsolidatedRetValue),
	put_assoc(Id, ASL_vars_in, ConsolidatedRetValue, ASL_vars_out), % do the assigning before evaluating the rest of the program
	log('assigned value of: '), log(ConsolidatedRetValue), log(' to '), logln(Id),
	ASL_funcs_out = ASL_funcs_in,
	true.

% STATSEQ(STAT)
evalstat(statementSeq(STAT), ASL_vars_in, ASL_vars_out, ASL_funcs_in, ASL_funcs_out) :-
	logcall(statementSeq),
	logparam(STAT),
	sanity_check_ASL(ASL_vars_in, statseq),
	sanity_check_ASL(ASL_funcs_in, statseq),	
	evalstat(STAT, ASL_vars_in, ASL_vars_out, ASL_funcs_in, ASL_funcs_temp),
	ASL_funcs_out = ASL_funcs_temp,
	true.
% STATSEQ(STAT, STATSEQ)
evalstat(statementSeq(STAT, STATSEQ), ASL_vars_in, ASL_vars_out, ASL_funcs_in, ASL_funcs_out) :-
	logcall(statementSeq),
	logparam(STAT),
	logparam(STATSEQ),
	sanity_check_ASL(ASL_vars_in, statseq),
	sanity_check_ASL(ASL_funcs_in, statseq),
	evalstat(STAT, ASL_vars_in, ASL_vars_param, ASL_funcs_in, ASL_funcs_temp),
	evalstat(STATSEQ, ASL_vars_param, ASL_vars_out, ASL_funcs_temp, ASL_funcs_out),
	true.
	
% RET
% should only be called in progret
evalreturn(ret(Base), ASL_vars_in, ASL_vars_out, ASL_funcs_in) :-
	logcall(ret),
	logparam(Base),
	sanity_check_ASL(ASL_vars_in, ret), % sanity check
	sanity_check_ASL(ASL_funcs_in, ret), % sanity check
	eval(Base, ASL_vars_in, ASL_vars_temp, ASL_funcs_in, RetValue), % base needs to be able to know what the variables are, but not assign nor declare funcs 
	put_assoc('return', ASL_vars_temp, RetValue, ASL_vars_out), % assign return=RetVal and set the new ASL to ASL_vars_out
	log('(return) assigning value of: '), log(RetValue), log(' to '), logln('return'),
	pause(1),	
	true.
	
% ID
% the ASL is used to try consolidating what the var actually is
% eg: x=y, y=5. Given x, it should find that it's value is 5
eval(id(Input), ASL_vars_in, ASL_vars_out, ASL_funcs_in, ThisRet) :-
	logcall(id),
	logparam(Input),
	sanity_check_ASL(ASL_vars_in, id_vars),
	sanity_check_ASL(ASL_funcs_in, id_funcs),
	consolidateVar(Input, ASL_vars_in, ThisRet),
	log('id '), logln(ThisRet).	

% FUNCDECL
% right now the code will override a previously defined function with the same name
evalstat(funcDecl(FUNCID, paramId(PARAMID), FUNCTIONLOGIC), ASL_vars_in, ASL_vars_out, ASL_funcs_in, ASL_funcs_out) :-
  logcall(funcdecl),
  logparam(FUNCID),
  logparam(PARAMID),
  logparam(FUNCTIONLOGIC),
  (funcDeclLogic(funcDecl(FUNCID, PARAMID, FUNCTIONLOGIC), ASL_vars_in, ASL_vars_out, ASL_funcs_in, ASL_funcs_out) -> true; logwarn('func decl logic failed')),
  true.
funcDeclLogic(funcDecl(FUNCID, PARAMID, FUNCTIONLOGIC), ASL_vars_in, ASL_vars_out, ASL_funcs_in, ASL_funcs_out) :-
  sanity_check_ASL(ASL_vars_in, funcdecl_vars),
  sanity_check_ASL(ASL_funcs_in, funcdecl_funcs),
  ValueList = [PARAMID, FUNCTIONLOGIC], % stores the paramID and the functionLogic as a list for the value in the assoc list
  (put_assoc(FUNCID, ASL_funcs_in, ValueList, ASL_funcs_temp) -> true; logwarn('failed to put the func decl in the ASL_func')),
  ASL_funcs_out = ASL_funcs_temp,  
  true.
  
% FUNCCALL
% TODO
eval(funcCall(FUNCID, BASE), ASL_vars_in, ASL_vars_out, ASL_funcs_in, ThisRet) :-
	logcall(funccall),
	logparam(FUNCID),
	logparam(BASE),
	(funcCallLogic(funcCall(FUNCID, BASE), ASL_vars_in, ASL_funcs_in, ThisRet) -> true; logwarn('failure'), dofail),
	true.
funcCallLogic(funcCall(FUNCID, BASE), ASL_vars_in, ASL_funcs_in, ThisRet) :-
  logcall(funcCallLogic),
  logparam(FUNCID),
  logparam(BASE),
  sanity_check_ASL(ASL_vars_in, funccall_vars),
  sanity_check_ASL(ASL_funcs_in, funccall_funcs),
  logwarn(sendmeaway),
  lognotice(ASL_vars_in),
  logwarn(withthewordsofalovesong),
  duplicate_term(ASL_vars_in, ASL_master),
	% look up the function's code/logic as well as parameters it has
	get_assoc(FUNCID, ASL_funcs_in, FunctionParamAndLogic),
	% split up the parameter for the function from the logic
	[FuncParam|_] = FunctionParamAndLogic,
	% and now grab the logic part from it
	last(FunctionParamAndLogic, FunctionLogic),	
	% copy the env here (the ASL_vars_in)
	ASL_vars_in_copy = ASL_vars_in,
	% BASE is the parameter passed to the function, so set the parameter equal to it
	% first find out what BASE is
	empty_assoc(Never_to_be_used_ASL),
	eval(BASE, ASL_vars_in, Never_to_be_used_ASL, ASL_funcs_in, BaseValue),
	put_assoc(FuncParam, ASL_vars_in_copy, BaseValue, ASL_vars_param),
	% run eval for the program with the new ASL_vars
	evaluateWithASL(FunctionLogic, ASL_vars_param, ASL_vars_to_update, ReturnValueOfFunction),
	sanity_check_ASL(ASL_vars_to_update, asl_vars_to_update_func_call_logic),
	lognotice(ASL_funcs_in),
	logwarn(heloooooooooooooooooooooooworooooooooooooooo),
	lognotice(ASL_vars_to_update),
	% now update the vars in global TODO TODO TODO
	% for each elem in the copyASL, write to the global ASL if:
		% the elem does not match the name of a param passed to func
		% it also existed in the global ASL to begin with
	%updateASLFromFuncCall(ASL_master, ASL_vars_to_update, FuncParam, ASL_vars_out),
	ThisRet = ReturnValueOfFunction,
	ASL_vars_out = ASL_vars_to_update, % Remove later, of course!!
	%get_assoc(x, ASL_vars_out, M), % temp for testing
	%logfail(M),
	true.	
	
% TODO
% updates the global ASL from the function's ASL	
updateASLFromFuncCall(ASL_global, ASL_from_func, FuncParam, ASL_out) :-
  logcall(updateASLfromfunccall),
  logparam(FuncParam),
  sanity_check_ASL(ASL_global, updateASLfromfunccall_global),
  sanity_check_ASL(ASL_from_func, updateASLfromfunccall_func),
	lognotice(aslglobal),
 	lognotice(ASL_global),
	assoc_to_keys(ASL_global, GlobalKeys), % TODO
	lognotice(hellooooo),
	lognotice(GlobalKeys),
	lognotice(hellooooo),
	([Head|Tail] = GlobalKeys -> lognotice(trueee);lognotice(faalsee)), % failing here!
	logwarn(Head),
	(updateASLFromFuncCallHelper(ASL_global, GlobalKeys, ASL_from_func, FuncParam, ASL_out)->lognotice(successoncallfunchelper);lognotice(failoncallfunchelper)),
	pause(1),
	true.
	
% recursive method	
updateASLFromFuncCallHelper(ASL_global_in, [Head|Tail], ASL_from_func, FuncParam, ASL_global_out) :-
  logcall(updateASLfromfunccallhelper),
  logparam(Head),
	logln(Head),
	(
		isEmpty(Tail), logwarn(helloworld), logwarn(Tail), logwarn(Head), logwarn(FuncParam) ->
			true, %done
			ASL_global_out = ASL_global_in
		;
			(
				Head == FuncParam -> % if the head (key) is equal to the func_param, skip it
					true,
					ASL_global_param = ASL_global_in
				;
				get_assoc(Head, ASL_from_func, ValueFromFuncForKey),
				put_assoc(Head, ASL_global_in, ValueFromFuncForKey, ASL_global_param)
			),			
			updateASLFromFuncCallHelper(ASL_global_param, Tail, ASL_from_func, func_param, ASL_global_out) % do the recursion again
	),
	true,
	pause(1).
	
isEmpty([]) :-
	true.	
	
% needs ASL_vars_out since it can lead to an assign statement
evalstat(conditional(COND,STATSEQ1,STATSEQ2), ASL_vars_in, ASL_vars_out, ASL_funcs_in, ASL_funcs_out) :-
	logln('conditional called with params:'),
	logparam(COND),
	logparam(STATSEQ1),
	logparam(STATSEQ2),
	(
		conditionalLogic(COND, STATSEQ1, STATSEQ2, ASL_vars_in, ASL_vars_out, ASL_funcs_in, ASL_funcs_out) ->
			logln('conditonal evaluated successfully')
		;
			logfail('conditonal failed to evaluate successfully'), dofail
	),
	ASL_funcs_out = ASL_funcs_in,
	true.
conditionalLogic(COND, STATSEQ1, STATSEQ2, ASL_vars_in, ASL_vars_out, ASL_funcs_in, ASL_funcs_out) :-
	(
		logln(COND),
		validCondition(COND, ASL_vars_in, ASL_funcs_in) ->
			logln('valid condition test succeed')
		;
			logfail('valid condition test failed'), dofail
	),
	sanity_check_ASL(ASL_vars_in, conditional),
	sanity_check_ASL(ASL_funcs_in, conditional),
	lognotice(butwillitblendugggh),
	%!, % do a cut here maybe?
	(
		%this will either fail or succ
		% if
		lognotice(butwillitblend),
		checkCondition(COND, ASL_vars_in) -> 
			% then
			log('check condition of '), log(COND), logln(' resulted in true'),
			evalstat(STATSEQ1, ASL_vars_in, ASL_vars_out, ASL_funcs_in, ASL_funcs_out) 
		;
			% else
			log('check condition of '), log(COND), logln(' resulted in false'),
			evalstat(STATSEQ2, ASL_vars_in, ASL_vars_out, ASL_funcs_in, ASL_funcs_out) 
	).
	
checkCondition(condition(Val1,Operator,Val2), ASL_vars_in)  :-
	% TODO: consolidate var1 and var2 here, remember they can be function calls!
	logcall(checkcondition),
	logparam(Val1),
	logparam(Operator),
	logparam(Val2),
	topConsolidateVar(Val1, ASL_vars_in, Val1Key),
	topConsolidateVar(Val2, ASL_vars_in, Val2Key),
	isComparator(Operator, Ret),
	checkConditionHelper(Val1Key,Ret,Val2Key),
	lognotice(superawesomeeee),
	true.

% checks that it is a valid condition (doesn't evaluate the condition)
% checks that both vals are instantiated
validCondition(condition(Val1,Operator,Val2), ASL_vars_in, ASL_funcs_in) :- % !! works
	logcall(validcondition),
	logparam(Val1),
	logparam(Operator),
	logparam(Val2),
	isComparator(Operator, DontCare),
	% TODO: consolidate var1 and var2, since they can be function calls
	/* DON'T CARE ANYMORE
	topConsolidateVar(Val1, ASL_vars_in, Val1Key),
	topConsolidateVar(Val2, ASL_vars_in, Val2Key),
	number(Val1Key),
	number(Val2Key).
	*/
	true.
	
isComparator(comp(Operator), ThisRet) :-
	comparatorSym(Operator),
	ThisRet = Operator.
	
% does the final checking with base values (eg 2 == 3)
% the resolution of x >= 4 to 9 >= 4 is done in the non-helper method
checkConditionHelper(Val1,'==',Val2) :-	
	Val1 == Val2.
checkConditionHelper(Val1,'<',Val2) :-
	Val2 > Val1.
checkConditionHelper(Val1,'>',Val2) :-
	Val1 > Val2.	
checkConditionHelper(Val1,'<=',Val2) :-
	Val1 =< Val2.	
checkConditionHelper(Val1,'>=',Val2) :-
	Val2 =< Val1.	
checkConditionHelper(Val1,'!=',Val2) :-
	not(Val1 == Val2).
	
% LOOP
evalstat(loop(COND, STATSEQ), ASL_vars_in, ASL_vars_out, ASL_funcs_in, ASL_funcs_out) :-
	logcall(loop),
	logparam(COND),
	logparam(STATSEQ),
	validCondition(COND, ASL_vars_in, ASL_funcs_in),
	sanity_check_ASL(ASL_vars_in, loop_vars),
	sanity_check_ASL(ASL_funcs_in, loop_funcs),
	(
		%this will either fail or succ
		% if
		ASL_vars_copy = ASL_vars_in,
		%!, % do a cut here maybe?
		checkCondition(COND, ASL_vars_in) -> 
			% then
			log('(loop) check condition of '), log(COND), logln(' resulted in true'),
			evalstat(STATSEQ, ASL_vars_copy, ASL_vars_param, ASL_funcs_in, ASL_funcs_out),
			(evalstat(loop(COND, STATSEQ), ASL_vars_param, ASL_vars_out, ASL_funcs_in, ASL_funcs_out) -> true; logfail('loop failed somehow!')),
			true
		;
			% else
			log('(loop) check condition of '), log(COND), logln(' resulted in false (end loop)'),
			ASL_vars_out = ASL_vars_in
	),
	ASL_funcs_out = ASL_funcs_in,
	true.
	

% CONST
% the ASL is not needed here, but needed for overloading the function
eval(const(Input), ASL_vars_in, ASL_vars_out, ASL_funcs_in, ThisRet) :-
	sanity_check_ASL(ASL_vars_in, const),
	ThisRet = Input.
	
% FACTOR
eval(factor(Base), ASL_vars_in, ASL_vars_out, ASL_funcs_in, ThisRet) :-
	sanity_check_ASL(ASL_vars_in, factor),
	eval(Base, ASL_vars_in, vars_out, ASL_funcs_in, ThisRet),
	log('factor '), logln(ThisRet).

% TERM
eval(term(mult, A, B), ASL_vars_in, ASL_vars_out, ASL_funcs_in, ThisRet) :- 
	logcall(term_mult),
	logparam(A),
	logparam(B),
	eval(A, ASL_vars_in, ASL_vars_out, ASL_funcs_in, R1), eval(B, ASL_vars_in, ASL_vars_out, ASL_funcs_in, R2), 
	doMult(R1, R2, ASL_vars_in, ThisRet),
	log('term mult :: '), log(ThisRet), log(' :A: '), log(B), log(' :B: '), logln(B).
doMult(A, B, ASL_vars_in, ThisRet) :-
	sanity_check_ASL(ASL_vars_in, domult),
	number(A),
	number(B),
	ThisRet is A * B.
doMult(A, B, ASL_vars_in, ThisRet) :-
	sanity_check_ASL(ASL_vars_in, domult),
	number(A),
	not(number(B)),
	topConsolidateVar(B, _, BVal),
	ThisRet is A * BVal.
doMult(A, B, ASL_vars_in, ThisRet) :-
	sanity_check_ASL(ASL_vars_in, domult),
	not(number(A)),
	number(B),
	topConsolidateVar(A, _, AVal),
	ThisRet is AVal * B.

eval(term(divide, A, B), ASL_vars_in, ASL_vars_out, ASL_funcs_in, ThisRet) :- 
	logcall(term_divide),
	logparam(A),
	logparam(B),
	eval(A, ASL_vars_in, ASL_vars_out, ASL_funcs_in, R1), eval(B, ASL_vars_in, ASL_vars_out, ASL_funcs_in, R2),
	doDivide(R1, R2, ASL_vars_in, ThisRet),
	log('term divide :: '), log(ThisRet), log(' :A: '), log(B), log(' :B: '), logln(B).
doDivide(A, B, ASL_vars_in, ThisRet) :-
	sanity_check_ASL(ASL_vars_in, dodivide),
	number(A),
	number(B),
	ThisRet is A / B.
doDivide(A, B, ASL_vars_in, ThisRet) :-
	sanity_check_ASL(ASL_vars_in, dodivide),
	number(A),
	not(number(B)),
	topConsolidateVar(B, _, BVal),
	ThisRet is A / BVal.
doDivide(A, B, ASL_vars_in, ThisRet) :-
	sanity_check_ASL(ASL_vars_in, dodivide),
	not(number(A)),
	number(B),
	topConsolidateVar(A, _, AVal),
	ThisRet is AVal / B.
	
% expression(Input)
eval(expression(plus, A, B), ASL_vars_in, ASL_vars_out, ASL_funcs_in, ThisRet) :-
	logcall(expression_plus),
	logparam(A),
	logparam(B),
	eval(A, ASL_vars_in, ASL_vars_out, ASL_funcs_in, R1), eval(B, ASL_vars_in, ASL_vars_out, ASL_funcs_in, R2),
	doPlus(R1, R2, ASL_vars_in, ThisRet),
	log('expression plus :: '), log(ThisRet), log(' :A: '), log(B), log(' :B: '), logln(B).
doPlus(A, B, ASL_vars_in, ThisRet) :-
	sanity_check_ASL(ASL_vars_in, doplus),
	number(A),
	number(B),
	ThisRet is A + B.
doPlus(A, B, ASL_vars_in, ThisRet) :-
	sanity_check_ASL(ASL_vars_in, doplus),
	number(A),
	not(number(B)),
	topConsolidateVar(B, _, BVal),
	ThisRet is A + BVal.
doPlus(A, B, ASL_vars_in, ThisRet) :-
	sanity_check_ASL(ASL_vars_in, doplus),
	not(number(A)),
	number(B),
	topConsolidateVar(A, _, AVal),
	ThisRet is AVal + B.
	
eval(expression(minus, A, B), ASL_vars_in, ASL_vars_out, ASL_funcs_in, ThisRet) :-
	logcall(expression_minus),
	logparam(A),
	logparam(B),
	eval(A, ASL_vars_in, ASL_vars_out, ASL_funcs_in, R1), eval(B, ASL_vars_in, ASL_vars_out, ASL_funcs_in, R2),
	doMinus(R1, R2, ASL_vars_in, ThisRet),
	log('expression minus :: '), log(ThisRet), log(' :A: '), log(B), log(' :B: '), logln(B).
doMinus(A, B, ASL_vars_in, ThisRet) :-
	sanity_check_ASL(ASL_vars_in, dominus),
	number(A),
	number(B),
	ThisRet is A - B.
doMinus(A, B, ASL_vars_in, ThisRet) :-
	sanity_check_ASL(ASL_vars_in, dominus),
	number(A),
	not(number(B)),
	topConsolidateVar(B, _, BVal),
	ThisRet is A - BVal.
doMinus(A, B, ASL_vars_in, ThisRet) :-
	sanity_check_ASL(ASL_vars_in, dominus),
	not(number(A)),
	number(B),
	topConsolidateVar(A, _, AVal),
	ThisRet is AVal - B.
	
/**----------------------------------------------------------------------------//
 * assign 3 test cases
 **----------------------------------------------------------------------------*/

% works: (99)
% parse([x, ':=', 1, ';', 'if', '(', 1, '<', 0, ')', 'then', x, ':=', 10, '.', 'else', x, ':=', 20, '.', 'endif', ';', return, 99, '.'], AST), evaluate(AST, Ret). 
 
% works: (20)
% parse([x, ':=', 1, ';', 'if', '(', x, '<', 0, ')', 'then', x, ':=', 10, '.', 'else', x, ':=', 20, '.', 'endif', ';', return, x, '.'], AST), evaluate(AST, Ret). 

% works: (10)
% parse([x, ':=', 2, ';', 'if', '(', x, '>', -8, ')', 'then', x, ':=', 10, '.', 'else', x, ':=', 20, '.', 'endif', ';', return, x, '.'], AST), evaluate(AST, Ret). 

% works: (30)
% parse([x, ':=', 91, ';', 'if', '(', x, '==', 91, ')', 'then', x, ':=', 30, '.', 'else', x, ':=', 20, '.', 'endif', ';', return, x, '.'], AST), evaluate(AST, Ret). 

% works: (20)
% parse([x, ':=', 4, ';', 'if', '(', x, '==', 91, ')', 'then', x, ':=', 30, '.', 'else', x, ':=', 20, '.', 'endif', ';', return, x, '.'], AST), evaluate(AST, Ret). 

% works: (0)
% parse([x, ':=', 4, ';', 'while', '(', x, '>', 0, ')', 'do', x, ':=', '(', x, '-', 1, ')', '.', 'done', ';', 'return', x, '.'], AST), evaluate(AST, Ret).

% works: (55)
% parse([x, ':=', 10, ';', y, ':=', 0, ';', 'while', '(', x, '>', 0, ')', 'do', y, ':=', '(', y, '+', x, ')', ';', x, ':=', '(', x, '-', 1, ')', '.', 'done', ';', 'return', y, '.'], AST), evaluate(AST, Ret).

% works: (8)
% parse([function, f, '(', x, ')', '{', return, 2, '.', '}', ';', return, 8, '.'], AST), evaluate(AST, N).

% works: (2)
% parse([function, f, '(', x, ')', '{', return, 2, '.', '}', ';', return, f, '(', 5, ')', '.'], AST), evaluate(AST, Ret).
% AST = prog(funcDecl(f, paramId(x), prog(ret(const(2)))), prog(ret(funcCall(f, const(5))))) 

% works: (5)
% parse([function, f, '(', x, ')', '{', return, x, '.', '}', ';', return, f, '(', 5, ')', '.'], AST), evaluate(AST, Ret).

% works: (11)
% parse([function, f, '(', x, ')', '{', return, x, '.', '}', ';', return, f, '(', '(', 10, +, 1, ')', ')', '.'], AST), evaluate(AST, Ret). 

/**----------------------------------------------------------------------------//
 * assign 2 test cases
 **----------------------------------------------------------------------------*/

% works:
% parse(['return',5,'.'], AST), evaluate(AST, Num).

% works:
% parse(['return','(',4,'*',9,')','.'], AST), evaluate(AST, N)

% works:
% parse(['return','(',12,'+',1,')','.'], AST), evaluate(AST, N).

% works:
% parse(['x',':=',5,';','return',1,'.'], AST), evaluate(AST, N).

% works:
% parse(['x',':=',5,';','return','x','.'], AST), evaluate(AST, N).

% works:
% parse(['x',':=',5,';','return','(','x','+',1,')','.'], AST), evaluate(AST, N).

% works:
% parse(['x',':=','(',5,'*',2,')',';','return','(','x','+',1,')','.'], AST), evaluate(AST, N).

% works:
% log('Should be 4'),
% parse(['return', 4, '.'], ParseTree), evaluate(ParseTree, Number).

% works:
% log('Should fail'),
% parse(['return', hi, '.'], ParseTree), evaluate(ParseTree, Number).

% works:
% log('Should be 34'),
% parse(['return', '(', 4, '+', 5, '*', '(', 3, '*', 2, ')', ')', '.'], ParseTree), evaluate(ParseTree, Number).

% works:
% log('Should be -2.6'),
% parse([hi, ':=', '(', 12, '/', 5, ')', ';', 'return', '(', hi, '-', 5, ')', '.'], ParseTree), evaluate(ParseTree, Number).
 
% works:
% log('Should be 0'),
% parse(['(',':=',5,';','return',0,'.'], ParseTree), evaluate(ParseTree, Number).

% works:
% parse([hi, ':=', 2, ';', otherhi, ':=', 1, ';', 'return', hi, '.'], ParseTree), evaluate(ParseTree, Number).

% works:
% parse([hi, ':=', 2, ';', hi, ':=', 1, ';', 'return', hi, '.'], ParseTree), evaluate(ParseTree, Number).

% works:
% parse([hi, ':=', 2, ';', hi, ':=', '(', hi, '+', 2, ')', ';', 'return', hi, '.'], ParseTree), evaluate(ParseTree, Number).

% works:
% parse([hi, ':=', '(', 4, '*', 3, ')', ';', hi, ':=', '(', hi, '+', 2, ')', ';', 'return', '(', hi, '*', hi, ')', '.'], ParseTree), evaluate(ParseTree, Number).

% works:
% parse(['x', ':=', '(', 5, '-', 2, '-', 323, ')', ';', 'return', 'x', '.'], AST), evaluate(AST, X).

/**----------------------------------------------------------------------------//
 * example parse stuff
 **----------------------------------------------------------------------------*/

% parse(['x',':=','(',5,'*',2,')',';','return','(','x','+',1,')','.'], AST).
% AST = prog(assign(id(x), term(mult, factor(const(5)), factor(const(2)))), prog(ret(expression(plus, factor(id(x)), factor(const(1)))))) 

/**----------------------------------------------------------------------------//
 * debugging ideas
 **----------------------------------------------------------------------------*/

/*
	check parens match in the rule declaration:
	eg: 
		blah(prog(Thing1,Thing2), AST, Ret)
		instead of
		blah(prog(Thing1,Thing2,AST,Ret)

	check parens match at the end where expected
	eg: this is missing a paren
		blah(prog(Thing1,Thing2, AST, Ret)

	use phrase to test out things
*/
 
/**----------------------------------------------------------------------------//
 * other random snippets, or things which don't work in general
 **----------------------------------------------------------------------------*/

print(0, _) :- !.
print(_, []).
print(N, [H|T]) :- log(H), nl, N1 is N - 1, print(N1, T).

/*** conditional example ***
	ifexample(Input) :-
		(	
			Input ->
			logln('this will print if Input is true')
		;	
			logln('this will print if Input false')
		).
*/

/*
eval(sum(val(A),K), T) :- eval(K, R), T is A + R, log('super'), logln(T).
eval(sum(K,val(A)), T) :- eval(K, R), T is A + R, log('super'), logln(T).
eval(sum(val(A),val(B)), R) :- R is A + B, log('victory!'), logln(R).
*/

% assoc example
% empty_assoc(T), put_assoc(food, T, happiness, T2), get_assoc(food, T2, X).

% another assoc example
% empty_assoc(A_1),
% put_assoc(1,A_1,1,A_2),
% put_assoc(2,A_2,2,A_3),
% put_assoc(3,A_3,3,A_4),
% put_assoc(4,A_4,4,A_5),
% put_assoc(5,A_5,5,A_6),

/*

evaluate(X, V) :- number(X), V is X.
evaluate(mult(X, Y), V) :- evaluate(X, X2), evaluate(Y, Y2), V is X2 * Y2.
evaluate(addi(X, Y), V) :- evaluate(X, X2), evaluate(Y, Y2), V is X2 + Y2.
evaluate(subt(X, Y), V) :- evaluate(X, X2), evaluate(Y, Y2), V is X2 - Y2.
evaluate(divi(X, Y), V) :- evaluate(X, X2), evaluate(Y, Y2), V is X2 / Y2.

sentence(s(NP,VP)) --> noun_phrase(NP), verb_phrase(VP).
noun_phrase(np(D,N)) --> det(D), noun(N).
verb_phrase(vp(V,NP)) --> verb(V), noun_phrase(NP).
det(d(the)) --> [the].
det(d(a)) --> [a].
noun(n(bat)) --> [bat].
noun(n(cat)) --> [cat].
verb(v(eats)) --> [eats].

parse(+TokenList, -AST).
evaluate(+AST, -Number).

sentence(s(NP,VP)) --> noun_phrase(NP), verb_phrase(VP).
noun_phrase(np(a)) --> [return].
verb_phrase(vp(V,NP)) --> verb(V), noun_phrase(NP).
det(d(the)) --> [the].
det(d(a)) --> [a].
noun(n(bat)) --> [beet].
noun(n(cat)) --> [cat].
verb(v(eats)) --> [eats].

*/
