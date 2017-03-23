%------------------------------------------------------------------------------
% File     : <For TPTP use only>
% Domain   : <The domain of the problem, from the TPTP domains>
% Problem  : <A one line description of the problem>
% Version  : <If this is a different form of an existing problem, why it is 
%             different>
% English  : <A full description of the problem>

% Refs     : <Relevant references>
% Source   : <The Ref where the formulae originate from>
% Names    : <The name(s) of this problem in the literature>

% Status   : <A value from the SZS ontology>
% Rating   : <Don't worry about this one - we'll do it automatically>
% Syntax   : <Don't worry about this one - we'll do it automatically>
% SPC      : <Don't worry about this one - we'll do it automatically>

% Comments : http://www.cs.miami.edu/~tptp/TPTP/SyntaxBNF.html
% http://www.cs.miami.edu/~tptp/cgi-bin/SystemB4TPTP
%------------------------------------------------------------------------------



fof(only_one_truth_teller, axiom, (
    ( truth_teller(henri) & ~ truth_teller(pierre) ) 
    | (truth_teller(pierre) & ~ truth_teller(henri) ) )).
%
fof(answer_yes_conditions, axiom, (
    ! [X, Q] : (
        ( ( answer_yes(X,Q) ) <=> 
        ( ( truth_teller(X) & true(Q) ) | ( ~ truth_teller(X) & ~ true(Q) )  ))))).

fof(go_left_or_not, axiom, (
    ( (true(gauche) )
        <=> ( go_left ) ) )).

fof(dit_oui_condition, axiom, (
    ! [X, Q] :
        ( ( true(dit_oui(X,Q)) ) <=>
        ( answer_yes(X,Q) ) ) ) ).

fof(dit_non_condition, axiom, (
    ! [X, Q] :
        ( ( true(dit_non(X,Q)) ) <=>
        ( ~ answer_yes(X,Q) ) ) ) ).

fof(find_puzzle_solution, conjecture, (
    ? [T] :
        ( answer_yes(henri, T)
        <=> go_left) )).

%------------------------------------------------------------------------------