This is a couple classes to support REPL and command-line apps in
browsers. Behind each terminal instance is a pipeline of interpreter functions
that can be pushed and popped.

blockterm.js seems to work in IE<9, but mockterm.js is broken in IE<9 because
textArea.selectionEnd (how it determines where the cursor is) seems to be
unimplemented or incorrect in those browsers. There is a Microsoft-only
TextRange API that I could make use of instead. (Someone want to help?)