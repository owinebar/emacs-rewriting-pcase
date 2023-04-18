# emacs-rewriting-pcase
Use pcase to rewrite elisp source.

This elisp library provides a function for rewriting the s-expressions in a buffer according to a predicate.
The rewriter performs a simple recursive macro-expansion using the predicate, but instead of prodcuing another sexpr, 
it replaces the source text corresponding to the S-expression with text for the replacement value.  
The purpose of this rewriting is to maintain the relative position of surrounding code elements, including comments.

The current version only performs this function well when the S-expression is being replaced in-toto.  This is adequate
for use in my unboxed package manager, but eventually I'd like to have a proper rewriting pcase that lets the user
move arbitrary subexpressions around and have the relevant comments and formatting be preserved.


Plan:

1. Get function that properly identifies how each piece of text maps to s-exp and comments correct
2. Convert from function arguments tracking parameters for current sexp to recursive data structure
   reflecting complete parse, including systematic associate of comments to syntactic units.
3. Derive a version of pcase that operates on the parsed sexp data structure from step 2, instead of 
   on ordinary values.  Must support the same patterns, presumably with a few additional operators
   for working with the enriched information.
   
   
