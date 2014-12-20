# Cleanup roadmap
- The scanner consumes raw text and produces a stream of tokens
  (LPAREN, RPAREN, ID, etc.)
- The reader consumes the token stream and produces a tree of datums
  (LIST, DOTTED_LIST, VECTOR, etc.) It doesn't make sense for PAIR to be
  a datum type, even though it is a Scheme value. (Likewise,
  it doesn't make sense for DOTTED_LIST to be a Scheme value.)
- The parser consumes the datum tree and produces an expr+def tree: both
  primitive exprs (VAR, LITERAL, CONSTANT, PROCEDURE_CALL, LAMBDA, IF, SET!)
  and derived (= macros).
- The macro expander consumes the expr+def tree and expands all derived exprs
  until nothing is left but primitive exprs and defs.
- The primitive expr+def tree can be executed directly by the interpreter,
  or it can be further processed into an IR for opt and compilation to some
  target language.
 
