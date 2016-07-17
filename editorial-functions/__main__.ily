% preamble

% this module is implicitly loaded by `annotate`.
% 


\include "config.ily"


% generic function. this is primarily used internally by the annotate module,
% but is also available as a user command, particularly with custom editorial
% types in mind.


%{ .. patch into annotate ..
(let ((edition (assq-ref props 'apply)))
  (if edition
      (editorialFunction edition item mus)
      mus))
%}

editorialFunction =
#(define-music-function (type item mus)
   (symbol? symbol-list? ly:music?)
   (let ((edit (getChildOption `(scholarly editorial functions ,type) item)))
     (if edit
         (if (ly:music-function? edit)
             (edit mus)
             #{ #edit #mus #})
         (begin
           (display "warning message")
           mus))))



% specific functions

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
