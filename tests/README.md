## Franca "Unit" Tests

These are trivial tests of specific features and minimal reproductions of 
specific bugs. There's a script (compiler/test.fr) that collects all the 
functions with the `#test` annotation in this directory and runs them (this 
is part of run_tests.fr). 

Often individual tests will have comments describing what went wrong with 
specific implementation details that are long outdated so don't trust them, 
but I figure if they break again there's a decent chance the problem is 
something similar. Some of the test cases are constructed to exercise specific 
code paths in outdated versions of the compiler. But there should be high 
correlation between programs that stress interesting corners of one language 
implementation with another. 

Testing indirectly this way, instead of writing tests for internal compiler 
functions directly, means I don't need to spend time rewriting tests when 
implementation details change. The whole point of tests is to make sure I 
didn't break something. So if I have to rewrite the tests everytime I 
refactor the program, they're kinda useless. 

See examples directory for more serious programs with better immergant 
behaviour for discovering new bugs. 
