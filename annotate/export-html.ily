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
% Copyright Urs Liska, 2015-17                                                %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Export annotations to html file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (nest-indent inpt num)
   (string-append (make-string num #\space) inpt))

% convenience functions
#(define (classify-html-tag class)
   (format "class=\"~a\"" class))
#(define (idify-html-tag id)
   (format "id=\"~a\"" id))
#(define (delimit-html-tags tags)
   (format "<~a>" (string-join tags " ")))

% open div with unique tags
#(define (div-open type ann-or-string nest-level)
   ;; if class = string, don't check for an id. otherwise it
   ;; is an ann props list, so check for an id and apply if necessary
   (let* ((trailing-line (if (= nest-level 0) "\n" ""))
          (div-type (symbol->string
                     (getChildOption `(scholarly annotate export html divs) type)))
          (class (classify-html-tag (if (string? ann-or-string) ann-or-string "annotation")))
          (id
           (let ((html-id (and (not (string? ann-or-string))
                               (assq-ref ann-or-string 'html-id))))
             (if html-id (idify-html-tag html-id) "")))
          (div-tags (delimit-html-tags (list div-type class id)))
          (div-begin (format "~a~a" (nest-indent div-tags nest-level) trailing-line)))
     (append-to-output-stringlist div-begin)))

% close any div
#(define (div-close type nest-level)
   (let ((div-type (symbol->string (getChildOption
                                    `(scholarly annotate export html divs)
                                    type)))
         (trailing-line (if (= nest-level 1) "\n" "")))
     (append-to-output-stringlist
      (nest-indent (format "</~a>~a" div-type trailing-line) nest-level))))

% get all the props we want exported from the option
#(define (html-process-props ann)
   (let ((props (getOption `(scholarly annotate export html props))))
     (for-each
      (lambda (prop)
        (let* ((key (assq-ref (getOption
                               `(scholarly annotate export html prop-labels))
                      prop))
               (val (cond ((equal? prop 'grob-location)
                           (format-location ann))
                      ((equal? prop 'type)
                       (getChildOption
                        `(scholarly annotate export html labels)
                        (assq-ref ann 'type)))
                      (else (assq-ref ann prop)))))
          (begin
           (if (symbol? val)
               (set! val (symbol->string val)))
           (div-open 'each-ann-props (symbol->string prop) 3)
           (append-to-output-stringlist
            (nest-indent (if key (string-append key val) val) 4))
           (div-close 'each-ann-props 3))))
      props)))



\register-export-routine html
#(lambda ()

   (let ((full-doc (getOption `(scholarly annotate export html full-document))))

     ;; If option is True, add the header and body
     (if full-doc
         (append-to-output-stringlist (format
                                       "<!DOCTYPE html>
<html>

<head>
  <meta charset=\"utf-8\"/>
  <link rel=\"stylesheet\" type=\"text/css\" href=\"~a\">
</head>

<body>
" (getOption `(scholarly annotate export html css-name)))))

     ;; wrap everything in the annotations div. this is sort of redundant, but
     ;; could be useful if projects have multiple bookparts with annotation lists.
     (div-open 'full-ann-list "annotations" 0)

     (for-each
      (lambda (ann)

        ;; wrap each annotation in the common annotation class
        ;; add div ID tag if available
        (div-open 'each-ann-outer ann 1)

        ;; type as a class - maybe we want different types to have some different styles
        ;; this also lets us make each *ann* a list itself if we want
        (div-open 'each-ann-inner (symbol->string (assq-ref ann 'type)) 2)

        ;; add the rest of the props to output
        (html-process-props ann) ;; nest-indents x 3

        (div-close 'each-ann-inner 2)

        (div-close 'each-ann-outer 1))

      annotations)

     ;; close "annotations" div
     (div-close 'full-ann-list 0)

     (if full-doc
         (begin
          (append-to-output-stringlist "
</body>
</html>")))

     ;; write to output file
     (write-output-file 'html)))
