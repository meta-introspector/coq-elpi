From elpi Require Import elpi.

Elpi Db tc.db lp:{{
  pred tc o:term, o:term.

  % T cannot be a free variable
  tc T _ :- var T, !, coq.say "fail on flexible function", fail.

  tc (app [global (const C) | TL]) _ :- var {std.last TL}, !, coq.say "Last param of " C "can't be flexible", fail.
  tc (prod _ _ X) _ :- pi x\ tc (X x) _.
}}.


Elpi Tactic TC_check.
Elpi Accumulate Db tc.db.
Elpi Accumulate lp:{{
  msolve L N :-
    std.rev L LR, coq.ltac.all (coq.ltac.open solve) LR N.

  :if "debug"
  solve A _ :- coq.say "Solving", fail.
  solve (goal _ _ Ty Sol _ as G) GL :- var Sol,
    (if (tc Ty X)  (refine X G GL ; coq.say "illtyped solution:" {coq.term->string X}) (GL = [seal G])).
}}.
Elpi Typecheck.

