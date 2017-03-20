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
              		`(scholarly editorial-functions ,type)
              		  (last item)
              		#f))
         (apply-edits (getOption `(scholarly editorial-functions apply)))
         (ignored-type (memq type (getOption
                                      `(scholarly editorial-functions ignored-types)))))
     (if edit
         (if (and apply-edits
                  (not ignored-type))
             (if (ly:music-function? edit)
                 (edit mus)
                 #{ \once #edit #mus #})
             mus)
         (begin
           (oll:warn "Editorial command ~a not set for ~a." edit (car item))
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
