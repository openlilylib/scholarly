% preamble

% this module is implicitly loaded by `annotate`.
% 


\include "config.ily"



%% generic editorial function.

% this is primarily used internally by the annotate module,
% but is also available as a user command, particularly with custom editorial
% types in mind. e.g. { \editorialFunction #'mytype Slur a( b) }

editorialFunction =
#(define-music-function (type item mus)
   (symbol? symbol-list? ly:music?)
   (let ((edit (getChildOptionWithFallback
		`(scholarly editorial functions ,type)
		(car item)
		#f)))
     (if (and edit
	      (getOption `(scholarly editorial functions apply)))
         (if (ly:music-function? edit)
             (edit mus)
             #{ \once #edit #mus #})
         (begin
           (oll:warn "Editorial command ~a not set for ~a." edition (car item))
           mus))))




%% available user commands:

editorialAddition =
#(define-music-function (item mus)
   (symbol-list? ly:music?)
   (editorialFunction 'addition item mus))

editorialDeletion =
#(define-music-function (item mus)
   (symbol-list? ly:music?)
   (editorialFunction 'deletion item mus))

editorialEmendation =
#(define-music-function (item mus)
   (symbol-list? ly:music?)
   (editorialFunction 'emendation item mus))
