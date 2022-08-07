# Ivan-Bratko-Prolog-Examples
### PROLOG PROGRAMMING FOR ARTIFICIAL INTELLIGENCE (by lvan Bratko, E.Kardelj University.  J.Stefan Institute Yugoslavia [1])

![image](https://user-images.githubusercontent.com/66141447/183287486-677c2acd-1d79-4140-8365-5c2f14a7e72c.png)

![image](https://user-images.githubusercontent.com/66141447/183287671-50c7453d-51b9-4472-a5b7-f371ae65c9a9.png)


### Chapter 1
- Prolog programming consists of relations, clauses(facts, rules and questions) and queries.
- A relation can be specified by facts, simply stating the n-tuples of object, or by stating rules about the relation.
- A procedure is a set of clauses about the same relation.
- To satisfy a query, involves logical inference, exploring among alternatives and backtracking. All this is done automatically by the Prolog system and is, in principle, hidden from the user.
- Also about whether goal is satisfiable or not.

### Chapter 2
- Simple objects in Prolog are atoms, variables and numbers.
- Structures are constructed by means of functors. Each functor is defined by its name and arity.
- The lexical scope of variables is one clause. Thusthe same variable name in two clauses means two different variables.
- Structures can be seen as trees
- The matching operation takes two terms and tries to make them identical
- A comma between goals means the conjunction of goals. A semicolon means disjunction of goals.
- The procedural meaning does depend on the order of goals and clauses. Thus the order can affect the efiiciency of the program. A order may even lead to infinite recursive calls.

### Chapter 3
- The list is a frequently used structure consists of a head and a tail which is a list as well. 
- Common operations on lists are: list membership, concatenation, adding an item, deleting an item, sublist
- The operator notation allows the programmer to tailor the syntax of programs toward particular needs. Using operators the readability of
programs can be greatly improved.
- Arithmetic is done by built-in procedures. Evaluation of an arithmetic expressionis forced by the procedure is and by the comparison predicates <, =<, etc

### Chapter 4
-  A database can be naturally represented as a set of Prolog facts.
- Prolog's mechanisms of querying and matching can be flexibly used for retrievirg structured information from a database.
- Abstract mathematicalconstructs,such as automata, can often be readily translated into executable Prolog definitions.

### Chapter 5
- Cut facility prevents backtracking. 
- It is used both to improve the efficiency of programs and to enhance the expressive power of the language, by not exploring the alternatives.
- Cut makes it possible to through rules of the form: *if Condition then Conclusion1 otherwise Conclusion2*
- Goals true always succeeds,fail always fails.
- Inserting a cut may destroy the correspondence between the declarative and procedural meaning of a program. Use with care. 

---

## Running queries (in SWI - Prolog or command line):

    $ swipl -s chapter1-5.pl -g "solution3(12,S)"
    $ swipl -s chapter1-5.pl -g "permutation([a,b,c,d], P)"

[1] Ivan Bratko. Prolog Programming for Artificial Intelligence, 1986

    @book{Bratko:1986:PPA:6981,
        author = {Bratko, Ivan},
        title = {Prolog Programming for Artificial Intelligence},
        year = {1986},
        isbn = {0-201-14224-4},
        publisher = {Addison-Wesley Longman Publishing Co., Inc.},
        address = {Boston, MA, USA},
    }
    
