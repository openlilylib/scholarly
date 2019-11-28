\version "2.19.80"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Choosing the element from a \choice expression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Macro to create a span chooser function
% Body must be a single expression evaluating to one
% span-class/span-music pair.
#(define-macro (define-span-chooser docstring . code)
   `(define-scheme-function
     (choice-type props expressions)(symbol? alist? span-expressions?)
     ,(if (string? docstring)
          docstring
          "define-span-chooser was here")
     (let*
      ((preference
        (or (assq-ref props 'preference)
            (getChildOptionWithFallback
             '(scholarly choice preferences)
             choice-type 'none)))
       (get-annotation
        (lambda (expr)
          (ly:music-property
           (ly:music-property (cdr expr) 'anchor)
           'span-annotation))))
      (let ((chosen-music ,@(if (string? docstring) code (cons docstring code))))
        chosen-music))))


% Chooser function for \choice variants
% If preference is 'lemma (default) return the lemma span,
% otherwise look for a variant whose 'source attribute matches the preference.
% (attention: this may usually involve strings rather than symbols)
% If none is found fall back to the first encoded variant.
#(define choose-variants
   (define-span-chooser
    (if (eq? preference 'lemma)
        ;; return lemma
        (assq 'lemma expressions)
        (or
         ;; Find a variant that has a 'source attribute matching to <preference>
         (any
          (lambda (expr)
            (let*
             ((source (assq-ref (get-annotation expr) 'source)))
             (if (equal? preference source) expr #f)))
          expressions)
         (begin
          (oll:warn "Requested reading from source '~a', which is not present in \\choice.
Fall back to first encoded variant" preference)
          (first expressions))))))

#(define choose-normalization
   (define-span-chooser
    (assq preference expressions)))

#(define choose-substitution
   (define-span-chooser
    (cond
     ((eq? preference 'new)
      (or (assq 'addition expressions) (assq 'restoration expressions)))
     ((eq? preference 'old)
      (assq 'deletion expressions))
     (else
      (oll:warn "Unknown preference for 'substitution'. First subexpression selected")
      (first expressions)))))

#(define choose-emendation
   (define-span-chooser
    (let
     ((is-first
       (cond
        ((eq? preference 'new)
         (memq (car (first expressions)) '(conjecture correction)))
        ((eq? preference 'old)
         (memq (car (second expressions)) '(conjecture correction)))
        (else
         (oll:warn "Unknown preference for 'emendation'. First subexpression selected")
         #t)
        )))
     (if is-first (first expressions) (second expressions)))))

% Choose the element from the set of music expressions
% that matches the given preferences
% Dispatches to a chooser function
#(define (choose-element choice-type props music)
   (let*
    ((expressions (validate-choice choice-type music))
     (chooser
      (getChildOptionWithFallback
       '(scholarly choice choosers) choice-type #f)))
    (if chooser
        (cdr (chooser choice-type props expressions))
        (let ((class (caar expressions)))
         (oll:warn "No chooser function provided for \\choice '~a'.
Returning the expression defined first (~a)." choice-type class)
         (cdr (first expressions))))))
