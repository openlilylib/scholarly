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



%% editorialShorthand `type` `item`
% - hooks the \edit macro to `editorialFunction type item`
%
% Ideally, this could be limited to only an { expression } argument following
% the editorialShorthand invocation. Since the expression inside expands *before*
% the invocation's arguments are applied to the context, that seems not possible.

#(define edit-sect-state (make-hash-table))

editorialShorthand =
#(define-scheme-function (type item)
  (symbol? symbol-list?)
    (hash-set! edit-sect-state 'type type)
    (hash-set! edit-sect-state 'item item)
    #{#})

edit =
#(define-music-function (mus)
  (ly:music?)
  (let ((type (hash-ref edit-sect-state 'type))
        (item (hash-ref edit-sect-state 'item)))
    (editorialFunction type item mus)))


%% editorialSection type item { music }
% - applies edition for type/item pairing to music

editorialSection =
#(define-music-function (parser location properties type item sect)
  ((ly:context-mod?) symbol? symbol-list? ly:music?)
  (let* ((props (if properties (context-mod->props properties) #f))
         (edit (getChildOptionWithFallback
                  `(scholarly editorial-functions ,type)
                    (car item)
                  #f))
         (apply-edits (getOption `(scholarly editorial-functions apply)))
         ; TODO uncomment and commit the following once `editorial-opts` branch is merged
         ;(ignored-type (memq type (getOption `(scholarly editorial-function ignored-types))))
         (music (music-map
                  (lambda (m)
                    (if edit
                        ; TODO change to: (and apply-edits (not ignored-type)):
                        (if apply-edits
                            (if (ly:music-function? edit)
                                  (edit m)
                                  #{ \once #edit #m #})
                            m)
                        (begin
                          ; Send a message for each instance. Do we want to warn
                          ; only once for the entire section though?
                          (oll:warn "Editorial command ~a not set for ~a." edit (car item))
                          m)))
                  sect))
          ; function prop: applied to the group, such as large parens etc.
          (fun (if props (assq-ref props 'function) #f)))
      (if fun
          (fun music)
          music)))
