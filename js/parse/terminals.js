/* Copyright 2011-2014 Brendan Linn

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>. */

goog.provide('r5js.parse.Terminals');


/** @typedef {string} */
r5js.parse.Terminal;


/** @enum {string} */
r5js.parse.Terminals = {
  BACKTICK: '`',
  BEGIN: 'begin',
  COMMA: ',',
  COMMA_AT: ',@',
  DEFINE: 'define',
  DEFINE_SYNTAX: 'define-syntax',
  DOT: '.',
  ELLIPSIS: '...',
  IF: 'if',
  LAMBDA: 'lambda',
  LET_SYNTAX: 'let-syntax',
  LETREC_SYNTAX: 'letrec-syntax',
  LPAREN: '(',
  LPAREN_DOT: '.(' /* TODO bl remove, not a real terminal! */,
  LPAREN_VECTOR: '#(',
  QUASIQUOTE: 'quasiquote',
  QUOTE: 'quote',
  RPAREN: ')',
  SET: 'set!',
  SYNTAX_RULES: 'syntax-rules',
  TICK: "'",
  UNQUOTE: 'unquote',
  UNQUOTE_SPLICING: 'unquote-splicing'
};
