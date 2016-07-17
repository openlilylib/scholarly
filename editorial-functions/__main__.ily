% preamble

% this module is implicitly loaded by `annotate`.
% 


\include "config.ily"



%% generic function.

% this is primarily used internally by the annotate module,
% but is also available as a user command, particularly with custom editorial
% types in mind.

editorialFunction =
#(define-music-function (type item mus)
   (symbol? symbol-list? ly:music?)
   (let ((edit (getChildOption `(scholarly editorial functions ,type) (car item))))
     (if edit
         (if (ly:music-function? edit)
             (edit mus)
             #{ \once #edit #mus #})
         (begin
           (display "warning message")
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
   (editorialFunciton 'emendation item mus))
