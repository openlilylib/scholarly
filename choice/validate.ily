%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
% This file is part of ScholarLY,                                             %
%                      =========                                              %
% a toolkit library for scholarly work with GNU LilyPond and LaTeX,           %
% belonging to openLilyLib (https://github.com/openlilylib/openlilylib        %
%              -----------                                                    %
%                                                                             %
% ScholarLY is free software: you can redistribute it and/or modify           %
% it under the terms of the GNU General Public License as published by        %
% the Free Software Foundation, either version 3 of the License, or           %
% (at your option) any later version.                                         %
%                                                                             %
% ScholarLY is distributed in the hope that it will be useful,                %
% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
% GNU Lesser General Public License for more details.                         %
%                                                                             %
% You should have received a copy of the GNU General Public License           %
% along with ScholarLY.  If not, see <http://www.gnu.org/licenses/>.          %
%                                                                             %
% ScholarLY is maintained by Urs Liska, ul@openlilylib.org                    %
% Copyright Urs Liska, 2018                                                   %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\version "2.19.80"

% Validating functions for the scholarly.choice module

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Validating a \choice expression
% This mainly includes checking for the proper included spans
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%
% Predicates

% A music expression that consists exclusively of
% span-music? expressions (defined in stylesheets.span)
% which have an 'anchor which in turn has a 'span-annotation
#(define (choice-music? obj)
   (and (ly:music? obj)
        (memq 'sequential-music (ly:music-property obj 'types))
        (every span-music? (ly:music-property obj 'elements))))

% Predicate for a list of span-class/span-music pairs
#(define (span-expressions? obj)
   (and
    (list? obj)
    (every
     (lambda (expr)
       (and (pair? expr)
            (symbol? (car expr))
            (span-music? (cdr expr))))
     obj)))

% validator-list? predicate:
% - is a list with symbol/procedure pairs
#(define (validator-list? obj)
   (and
    (list? obj)
    (every
     (lambda (o) (and (pair? o) (symbol? (car o)) (procedure? (cdr o))))
     obj)))

%%%%%%%%%%%%%%%%%%
% Helper functions

% extract span-class/span-music pairs from a choice-music? expression
#(define extract-span-pairs
   (define-scheme-function (music)(choice-music?)
     (map
      (lambda (expr)
        (let*
         ((anchor (ly:music-property expr 'anchor))
          (span-annotation (ly:music-property anchor 'span-annotation))
          (span-class (assq-ref span-annotation 'span-class)))
         (cons span-class expr)))
      (ly:music-property music 'elements))))

% Macro to create a function to validate a choice expression.
#(define-macro (define-choice-validator docstring . code)
   ; all wrapping code is (semi)quoted
   `(define-scheme-function
     (choice-type music) (symbol? choice-music?)
     ,(if (string? docstring)
          docstring
          "define-span-chooser was here")
     (let*
      ((spans (extract-span-pairs music))
       (classes (map car spans))
       ;; validation functions
       (count-class
        ;; count occurences of a class
        (lambda (class) (length (filter (lambda (c) (eq? c class)) classes))))
       (single
        ;; test if class is present exacly once
        (lambda (class) (= (count-class class) 1)))
       (optional
        ;; text if class is present zero or one times
        (lambda (class) (<= (count-class class) 1)))
       (warning-message "")
       (valid
        ,@(if (string? docstring) code (cons docstring code)))
       )
      (if (not valid)
          (oll:warn "Invalid choice of type '~a'. Found spans ~a.\n~a
Expect follow-up errors!" choice-type classes warning-message))
      spans)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Validators for built-in choice types

#(define validate-variants
   (define-choice-validator
    (let
     ((is-valid
       ;; One lemma and at least one reading
       (if
        (and
         (every (lambda (c) (memq c '(lemma reading))) classes)
         (single 'lemma)
         (memq 'reading classes)) #t #f)))
     (if (not is-valid)
         (set! warning-message "Expects one 'lemma and an arbitrary number of 'reading spans."))
     is-valid)))

#(define validate-normalization
   (define-choice-validator
    (let
     ((is-valid
       ;; One original and one regularization
       (if
        (and (= (length classes) 2)
             (memq 'original classes)
             (memq 'regularization classes)) #t #f)))
     (if (not is-valid)
         (set! warning-message "Expects one 'original and one 'regularization span."))
     is-valid)))

#(define validate-substitution
   (define-choice-validator
    (let ((is-valid
           ;; one deletion and one addition or restoration
           (if
            (and
             (= (length classes) 2)
             (single 'deletion)
             (or (single 'addition) (single 'restoration))) #t #f)))
      (if (not is-valid)
          (set! warning-message "Expects one 'deletion and one ['addition|'restoration] span)."))
      is-valid)))

#(define validate-emendation
   (define-choice-validator
    (let ((is-valid
           (if
            ;; one correction and one [gap|sic|unclear]
            (and
             (= (length classes) 2)
             (single 'correction)
             (every (lambda (c) (memq c '(gap sic unclear correction))) classes))
            #t #f)))
      (if (not is-valid)
          (set! warning-message "Expects one 'correction and one ['gap|'sic|'unclear] span."))
      is-valid)))

% validate a \choice expression against rules regarding the
% span-class attributes of the included spans.
% Returns a list with class/music pairs.
% If no validator is found for the choice type a warning is issued
% but the spans returned anyway.
#(define (validate-choice choice-type music)
   (let*
    ((validator
      (getChildOptionWithFallback
       '(scholarly choice choice-type-validators) choice-type #f))
     (result (if validator (validator choice-type music) 'unknown)))
    (if (eq? result 'unknown)
        (begin
         (oll:warn "No validator function available for choice type '~a'.Crossing fingers ..." choice-type)
         (extract-span-pairs music))
        result)))
