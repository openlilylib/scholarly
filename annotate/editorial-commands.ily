% editorial commands for scholarLY
%

#(define scholarly-edition-bools '())

#(define scholarly.editions (make-hash-table))

% merely a convenience procedure for now:
#(define (setEdition family group affect)
          (hash-set! scholarly.editions '(family group) affect)
          ;; `group` must also match the grob name if affects grob type
          ;; rhythm, for example, will require some additional steps later.
          )

% temporary examples; sent to scholarly.editions
#(setEdition 'addition 'Slur slurDashed)
#(setEdition 'deletion 'Slur slurDotted)
#(setEdition 'emendation 'Slur slurDashed)

% to be factored into 'annotate'
#(define (conditional-edit grp itm)
    (cond (and ((assq-ref scholarly-edition-bools 'applylocaledit) #t)
               (not (eq? (hash-ref scholarly.editions '((assq-ref temp-props 'apply) grp)) #f)))
          (let* ((faml (assq-ref temp-props 'apply))
                 (func (hash-ref scholarly.editions '(faml grp))))
                #{ #func #itm #})
          (else (ly:input-message (*location*) "Instructions for this edition have not been set. Applying nothing..."))))
