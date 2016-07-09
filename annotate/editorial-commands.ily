% editorial commands for scholarLY
%

#(define scholarly-edition-bools (make-hash-table))

#(define scholarly.editions (make-hash-table))
#(define scholarly.editions.add (make-hash-table))

% merely a convenience procedure for now:
#(define (setEdition family group affect)
          (hash-set! scholarly.editions (list family group) affect)
          ;; `group` must also match the grob name if affects grob type
          ;; rhythm, for example, will require some additional steps later.
          )

% temporary examples; sent to scholarly.editions
#(setEdition 'addition 'Slur #{ \slurDashed #})
#(setEdition 'deletion 'Slur slurDotted)
#(setEdition 'emendation 'Slur slurDashed)
#(hash-set! scholarly.editions.add 'Slur slurDashed)
