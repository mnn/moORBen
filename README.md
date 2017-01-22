moORBen
======

This project is an esoteric language based around falling orbs and a tape of stacks. Each orb has a tape index which points to a stack, an active stack for this orb. A program syntax resembles physical object viewed from side.

Warning: It is not yet finished. Some basic programs are working, but several things are missing (custom pocket dimensions [aka procedures], more built in pocket dimensions, a cloning of a whole stack, basic math operations, orbs synchronization [a sensor and a holder]). Following examples are fully working.

Hello world
----------
```
`  Hello world 
` -------------

` <- line comment
o               ` orb starting position
@"Hello world!" ` push string to the active stack
%pS             ` enter pocket dimension called "pS" which is a built-in "function" for printing out a string
^               ` spike - pops orb which terminates execution
```

Other programs
-------------
* [Truth machine](res/TruthMachine.mrb)
* [Basic math](res/BasicMath.mrb)
