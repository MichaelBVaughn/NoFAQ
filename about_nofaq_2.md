# How NoFAQ Works: A High-level Overview
NoFAQ learns simple textual substitution programs which match and repair command line interactions. The domain-specific language we use to express NoFAQ rules is called FixIt. Despite providing a simple set of textual substitution operations, FixIt is still able to encode repair rules for a rich class of command line mistakes.

## The Structure of FixIt Rules
At the highest level, every FixIt rule has the structure 

```
if matching-predicate.evaluate(text, variable-set) then
   repair-function.apply(variable-set)
```
That is, if a piece of input text matches a certain set of criteria, we apply repair logic to the text. The matching predicate consists of a list of sub-predicates; to evaluate the a matching predicate we split the text into a list of whitespace separated tokens, and then evaluate a single sub-prediacate for each token. Specifically, we evaluate the first sub-predicate on the first token, the second on the second, and so on for the length of the input text. Matching-predicate only returns a successful match if every sub-predicate evaluates to true. Additionally, if the length of the list in matching-predicate does not equal the number of tokens in text, we return false. 

A matching predicate can be one of three things, a simple string equality check (e.g. ```input == "ls"```), a prefix and/or suffix check (like ```input.ends-with(".py")```), or a check that allows any value (that is ```return true```). We refer to the latter two as "variable-matching". 

In addition to determining whether a given rule should apply, matching-predicate provides another critical piece of functionality: assigning tokens to variables. Variable-matching sub predicates can, in addition to evaluating a token, associate that token with a variable, which can be used later in repair-function. 

Repair-function accepts the variable-set captured in matching-predicate and uses them to construct an output string. Much like matching-predicate, repair-function consists of a list of sub-functions. Each sub-function produces a single token, and their outputs are joined sequentially by a space. A sub-function can simply output a constant string, or it can take a substring of a variable's value and append and/or prepend a single suffix or prefix.

### An Example FixIt Rule
An actual FixIt rule actually has two match predicates. We refer to a single command line interaction, consisting of a command string and its corresponding output as an "invocation". We treat an invocation as an ordered pair of strings, one for the command, and one for its output.
Thus, an actual FixIt rule looks like this:
```
match[ const("java"), var-match(var-id=1, begins-with="", ends-with=".java")] 
and match[ const("Could"), const("not"), const("not"), const("find"), const("or"), const("load"), const("main"), const("class"), var-match(var-id=2, begins-with="", ends-with".java")]
do eval-fix[f-const("java"), sub-lr(start-pos=0, end-pos=-5, prefix="", suffix="", src-var=1)]
```
The const forms are simple, ```const(str)``` returns true if the input token equals str, and ```f-const(str)``` outputs str.

The sub-predicate ```var-match(starts-with,ends-with,var-id)``` is our general purpose variable-matching predicate. It returns true only if a token starts with the value of begins-with, and ends with ends-with. Our variables are simply given integer names, so if the match is successful, the resulting value is assigned to the integer specified in var-id.

The sub-function ```sub-lr(start-pos, end-pos, prefix, suffix, src-var)``` first uses start-pos and end-pos takes the substring of the value assigned to sub-lr. Given this substring, ```sub-lr``` prepends the prefix, and appends the suffix.

Given this, our rule will match ```java MyProgram.java``` and output ```java MyProgram```. However, it will not match ```javac MyProgram.java```, ```java MyProgram.cs```, ```java``` or ```java MyProgram.java OtherProgram.java```. We see that the example rule captures the case where someone has presumably built a java program, but incorrectly invokes the JVM.

### How does NoFAQ Learn FixIt Rules?
NoFAQ learns repair rules by examining triples consisting of a incorrect command, the command's output, and a command that performs the desired interaction. We call these triples "repair examples."  Given several repair examples, NoFAQ can examine them for differences and commonalities in such a way that they can be transformed into FixIt rules.

Recall that we refer to the pair containing a command and its output as an "invocation". Given two repair examples, we first extract their invocations, and attempt to derive a match predicate from them, starting with the commands' match predicate. To learn a command match predicate, we compare the two commands (ensuring they are the same length). If they are the same length, we look for any point where they share tokens. If tokens are shared, we derive a ```const```. If they are not equal, we attempt to find a common prefix and suffix, and create a ```var-match``` expression, along with a new variable id.

Consider the case where we have the commands ```java MyProgram.java``` and ```java Game.java```. The first two tokens match, so we obtain ```const("java")```. For the second token, we see they do not match, and we also see that they share the ```.java``` as a suffix. Thus, we obtain ```var-match(var-id=1, begins-with="", ends-with=".java")```.

For the error messages, we simply perform this same process.

Learning the fix function is a bit more complex. As before, we do a one-by-one comparison of tokens, and whenever they are equal, we learn a constant. To learn ```sub-lr``` functions, we need to look at the values captured by the variables in both examples. Say we have two examples **x1** and **x2**, containing the corrected commands **f1** and **f2**. Also, assume that **f1** and **f2** have, at some point, tokens **t1** and **t2** which do not match. 

Having learned a match predicate for **x1** and **x2**, we look at each variable, one by one. First we look at the variable with ID 1. Call it **var[1]**. Let **s1** be **var[1]**'s value in **x1** and **s2** be **var[2]**'s value in **x2**. For each substring in **s1**, we find the set of all sets of substrings and constant prefixes and suffixes such that we can construct **t1**. For each such set of substring/prefix/suffix set, apply them to **x2**, and keep those that also yield **t2** from **s2**. We repeat this for every variable captured by the match predicates. If we don't find any such substring/prefix/suffix sets that work for both **x1** and **x2**, we cannot learn a fixit rule for **x1** and **x2**.

Consider this extension of our previous example (where we ignore error messages)

- Command 1: ```java MyProgram.java```
- Fix 1: ```java MyProgram```

- Command 2: ```java Game.java```
- Fix 2: ```java Game```

As we saw before, the var-match expressions will capture ```MyProgram.java``` and ```Game.java``` and assign them to **var[1]**. We also see that MyProgram and Game are not equal to each other. Examining their respective variables, we see that the ```sub-lr(0,-5,"","",1)``` correctly cuts off the ```.java``` portion of each value of **var[1]**, yielding the desired output.

Given this algorithm, note that there may be many rules that are consitent with **x1** and **x2** (in fact, there may be exponentially many!). However, we maintain a simple "symbolic" rule that implicitly encodes all possible FixIt programs consistent with **x1** and **x2** while using only polynomial space. 








