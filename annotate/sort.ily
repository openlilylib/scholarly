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
% Copyright Urs Liska, 2015                                                   %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%
% comparison operators for sorting annotations by different properties

#(define (get-rhythmic-location ann)
   "Retrieve 'rhythmic-location' from 'grob-location'"
   (assq-ref
    (assq-ref ann 'grob-location)
    'rhythmic-location))

% compare by rhythmic location
#(define (cmp-rhythmic-location ann-a ann-b)
   (let*
    ((loc-a (get-rhythmic-location ann-a))
     (ma (car loc-a))
     (pa (cdr loc-a))
     (loc-b (get-rhythmic-location ann-b))
     (mb (car loc-b))
     (pb (cdr loc-b)))
    (cond
     ((< ma mb) '<)
     ((> ma mb) '>)
     ((ly:moment<? pa pb) '<)
     ((ly:moment<? pb pa) '>)
     (else '=))))

% compare by author
#(define (cmp-author ann-a ann-b)
   (let ((a (assq-ref ann-a 'author))
         (b (assq-ref ann-b 'author)))
     (cond
      ((string<? a b) '<)
      ((string>? a b) '>)
      (else '=))))

% compare by annotation type
#(define (cmp-ann-type ann-a ann-b)
   (let ((a (assq-ref ann-a 'ann-type))
         (b (assq-ref ann-b 'ann-type)))
     (cond
      ((symbol<? a b) '<)
      ((symbol<? b a) '>)
      (else '=))))

% compare by score id
#(define (cmp-score ann-a ann-b)
   (let ((a (assq-ref ann-a 'score-id))
         (b (assq-ref ann-b 'score-id)))
     (cond
      ((symbol<? a b) '<)
      ((symbol<? b a) '>)
      (else '=))))

% compare by score id
#(define (cmp-context ann-a ann-b)
   (let ((a (assq-ref ann-a 'context-id))
         (b (assq-ref ann-b 'context-id)))
     (cond
      ((string<? a b) '<)
      ((string<? b a) '>)
      (else '=))))

% Lookup list from which the comparison procedures are retrieved
#(define annotation-comparison-predicates
   `((rhythmic-location . ,cmp-rhythmic-location)
     (author . ,cmp-author)
     (type . ,cmp-ann-type)
     (score . ,cmp-score)
     (context . ,cmp-context)))


% Return a sorted list of annotations
% Possibly sorted by multiple criteria
#(define (sort-annotations annotations)
   (let*
    ((operators (getOption '(scholarly annotate sort-by)))
     ;; list with comparison operators, to be used recursively.
     ;; ops is the base list to reset the used-ops list with.
     ;; Suppress invalid criteria, issuing a warning.
     (ops
      (filter-map
       (lambda (s)
         (let ((op (assq-ref annotation-comparison-predicates s)))
           (if (not op)
               (begin (oll:warn "Invalid sort criterium: ~a. Skipping!" s) #f)
               op)))
       operators))
     (use-ops (list-copy ops)))
    (if (null? ops)
        annotations
        (letrec
         ;; Define recursive comparison operator comparing all requested
         ;; criteria in turn, breaking the recursion as soon as a solution
         ;; has been reached.
         ((ann<?
           (let ((reset-ops (lambda () (set! use-ops (list-copy ops)))))
             (lambda (ann-a ann-b)
               (if (null? use-ops)
                   (begin (reset-ops) #f)
                   (let ((cmp-result ((car use-ops) ann-a ann-b)))
                     (case cmp-result
                       ((<) (reset-ops) #t)
                       ((>) (reset-ops) #f)
                       (else
                        (set! use-ops (cdr use-ops))
                        (ann<? ann-a ann-b)))))))))
         (stable-sort annotations ann<?)))))
