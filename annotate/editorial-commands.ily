% editorial commands for scholarLY
%

#(define scholarly-edition-bools (make-hash-table))
#(define scholarly.editions (make-hash-table))

% slurDashed, and other slur commands work normally since they don't depend on music arguments out of context
#(hash-set! scholarly.editions (list 'addition 'Slur) slurDashed)
#(hash-set! scholarly.editions (list 'addition 'NoteHead) parenthesize)
#(hash-set! scholarly.editions (list 'deletion 'Slur) slurDotted)


#(define scholarly-editorial-temp-func (make-hash-table))
% define arbitrary initial value so scholarlytempfunction will pass until used
#(hash-set! scholarly-editorial-temp-func 'temp-applic 'addition)
#(hash-set! scholarly-editorial-temp-func 'temp-grp 'NoteHead)

% temporary function used to apply editorial commands to music
#(define scholarlytempfunc
   (let ((commandtype (hash-ref scholarly-editorial-temp-func 'temp-applic))
         (musicitem (hash-ref scholarly-editorial-temp-func 'temp-grp)))
        (hash-ref scholarly.editions (list commandtype musicitem))))
