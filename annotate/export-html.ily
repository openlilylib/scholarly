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

#(use-modules (ice-9 rdelim))
#(use-modules (ice-9 regex))


% Default CSS Settings:
% ~~~~~~~~~~~~~~~~~~~~
% If the class/id name matches a builtin (such as annotations, annotation, etc.)
% then it is applied to that div (even if the user has distinguished a new class/id
% name for that particular div). This allows default css to still be toggled
% without the user needing to manually rename their stuff to match these.
% OTHERWISE, if it isn't a builtin, scholarLY turns that name (such as
% `my-id-for-something` below) into a string, and applies it to the matching
% class/id at the time of processing.

#(define default-css-settings
  `((class . ((full-ann-list . ("background: gray"
                                "margin: 0.5em"
                                "line-height: 1.2"
                               (ul . "list-style-type: none")))
              (annotation . ((ul . "background: lightgray")
                             (ul_li . "margin: 0.25em")))
              (todo . ("background: red"))
              (question . ("background: green"))))
    (id . ((my-unique-annotations-list . ("foo: bar"))))))

% nicely formatted css for header or exported
#(define (formatted-css css-type)
    (let* ((css-settings (if (eq? css-type 'default)
                             default-css-settings
                             (getOption `(scholarly annotate export html
                                           generate-css-settings))))
           (pretty-css ""))
        (for-each
          ;; family = a group named .class, #id, or anything else
          (lambda (family)
            (for-each
              ;; member = each .class, #id, or anything else
              (lambda (member)
                (set! pretty-css (string-append pretty-css
                  (format (cond ((eq? (car family) 'class)
                                  "\n.~a {~a \n}\n")
                                ((eq? (car family) 'id)
                                  "\n#~a {~a \n}\n")
                                (else "\n~a {~a \n}\n"))
                    (if (eq? (car member) 'full-ann-list)
                        (let ((anns-tag-redefined
                                (getChildOption
                                  `(scholarly annotate export html annotations-div-tags)
                                      (car family))))
                          (if anns-tag-redefined
                              anns-tag-redefined
                              (car member)))
                        (car member))
                    ;; sub-mem-styles = a list of everything in each sub-member:
                    ;;                  sub-member { sub-mem-styles }
                    (let ((sub-mem-styles ""))
                      (for-each
                        ;; sub-member = list of strings and/or pairs
                        (lambda (sub-member)
                          (set! sub-mem-styles
                            (if (string? sub-member)
                                ;; string gets printed literally
                                (format "~a\n  ~a;" sub-mem-styles sub-member)
                                ;; pairs are formatted like members and squeezed
                                ;; between the first and last delimiters
                                (format "~a\n} ~a {~a"
                                  sub-mem-styles
                                  (let ((multi-segs (string-match "(-|_)"
                                                      (symbol->string (car sub-member)))))
                                      (if (not multi-segs)
                                          (car sub-member)
                                          ;; turn "ul_li" or "ul-li" into "ul li"
                                          (regexp-substitute #f
                                            multi-segs
                                            'pre " " 'post)))
                                  (if (string? (cdr sub-member))
                                      (format "\n  ~a;" (cdr sub-member))
                                      ;; it is a list of strings
                                      (let ((sub-lst ""))
                                        (for-each
                                          (lambda (mem)
                                            (set! sub-lst
                                              (format "~a\n  ~a;"
                                                sub-lst mem)))
                                        (cdr sub-member))
                                      sub-lst))))))
                        (cdr member))
                      sub-mem-styles)))))
              (cdr family)))
          css-settings)
        pretty-css))


#(define (indent inpt num)
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
          (class (classify-html-tag (if (string? ann-or-string)
                                        (if (equal? ann-or-string "annotations")
                                            (let ((anns-class-renamed (assq-ref (getOption
                                                    `(scholarly annotate export html annotations-div-tags))
                                                      'class)))
                                                (if anns-class-renamed
                                                    anns-class-renamed
                                                    ann-or-string))
                                              ann-or-string)
                                          "annotation")))
          (id
            (if (equal? ann-or-string "annotations")
                (let ((anns-id-renamed (assq-ref (getOption
                        `(scholarly annotate export html annotations-div-tags))
                          'id)))
                  (if anns-id-renamed
                      (idify-html-tag anns-id-renamed) ""))
                (let ((html-id (and (not (string? ann-or-string))
                                    (assq-ref ann-or-string 'html-id))))
                  (if html-id (idify-html-tag html-id) ""))))
          (div-tags (delimit-html-tags (list div-type class id)))
          (div-begin (format "~a~a" (indent div-tags nest-level) trailing-line)))
     (append-to-output-stringlist div-begin)))

% close any div
#(define (div-close type nest-level)
   (let ((div-type (symbol->string (getChildOption
                                    `(scholarly annotate export html divs)
                                    type)))
         (trailing-line (if (= nest-level 1) "\n" "")))
     (append-to-output-stringlist
      (indent (format "</~a>~a" div-type trailing-line) nest-level))))

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
            (indent (if key (string-append key val) val) 4))
           (div-close 'each-ann-props 3))))
      props)))



\register-export-routine html
#(lambda ()

   (let* ((full-doc (getOption `(scholarly annotate export html full-document)))
          (css-method (getOption `(scholarly annotate export html with-css)))
          (css-type (getOption `(scholarly annotate export html use-css)))
          (css-name (cond ((eq? css-type 'generate)
                                   (getOption `(scholarly annotate export html generate-css-name)))
                          ((eq? css-type 'external)
                                   (getOption `(scholarly annotate export html external-css-name)))
                          (else "default-stylesheet.css"))))

     ;; If option is True, add the header and body
     (if full-doc
        (begin
         (append-to-output-stringlist (format
"<!DOCTYPE html>
<html>

<head>
  <meta charset=\"utf-8\"/>
  ~a
</head>

<body>
"
              ;; insert link to css, or directly embed css in header.
              (cond ((eq? css-method 'linked)
                    (format "<link rel=\"stylesheet\" type=\"text/css\" href=\"~a\">"
                      css-name))
                  ((eq? css-method 'header)
                    (format "\n<style>\n~a\n</style>\n" (formatted-css css-type))))))
          ;; if we want to generate and link to a seperate .css file
          (if (and (eq? css-method 'linked)
                   (or (eq? css-type 'generate)
                       (eq? css-type 'default)))
              (with-output-to-file
                css-name ; css-name
                  (lambda () (write-line (formatted-css css-type)))))))



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
        (html-process-props ann) ;; indents x 3

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
