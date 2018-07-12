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

%{
  Basic functionality to export/output annotations
%}

#(use-modules (ice-9 regex))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% General export helper routines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Provide a global string-list that output functions append their lines to.
% This is finally used to write to log files.
#(cond ((not (defined? 'annotate-export-stringlist))
        (define annotate-export-stringlist '())))

% Variable holding the string list to export
\registerOption scholarly.annotate.export.strings #'()

% Append a single string or a stringlist
% to the stringlist that is used for export
#(define (append-to-output-stringlist msg)
   (setOption '(scholarly annotate export strings)
     (if (list? msg)
         (append (reverse msg) (getOption '(scholarly annotate export strings)))
         (cons msg (getOption '(scholarly annotate export strings))))))

% Same as append-to-output-stringlist but
% operate on a given 'messages' argument.
#(define (append-to-messages messages msg)
   ;; append a single string or a string list
   (append messages
     (if (list? msg)
         msg
         (list msg))))

% As writing to a logfile uses (display)
% this is a nice shorthand (also necessary to be compatible with (write-lines)
#(define (display-line line)
   (display line)
   (newline))

% Take a string list and write it to an output using 'cmd'
% cmd could be 'display' or 'ly:message',
% basically any procedure that takes one string argument
#(define (write-lines msgs cmd)
   (for-each
    (lambda (m)
      ;; filtered properties are represented by empty strings
      ;; so we filter them out here.
      (if (not (string= m ""))
          (cmd m)))
    msgs))

% create a basename string to be used when building output file names
\registerOption scholarly.annotate.internal.basename
#(ly:parser-output-name (*parser*))

% Take the stringlist scholarly.annotate.export.strings
% and write it out to a file
#(define (write-output-file ext)
   (let*
    ((logfile
      (format "~a.annotations.~a"
        (getOption '(scholarly annotate internal basename)) ext)))
    (ly:message "writing '~a' ..." logfile)
    (with-output-to-file logfile
      (lambda ()
        (write-lines
         (reverse
          (getOption '(scholarly annotate export strings))) display-line)))
    (setOption '(scholarly annotate export strings) '())))

% Format the rhythmic location of an annotation to a string
% used when printing to the console or exporting to plain text
#(define (format-location ann)
   "Return a string representation of the annotations rhythmic location"
   (let* ((props (assq-ref ann 'grob-location))
          (measure-no (assq-ref props 'measure-no))
          )
     (if (= 0 measure-no)
         ;; workaround for a problem that sometimes the paperColumn gets
         ;; lost along the way. In that case the location is manually
         ;; set to measure zero - which is impossible.
         (format "Sorry, rhythmic position could not be determined.\nInput location at ~a"
           (assq-ref ann 'location))
         (format "Measure ~a, beat ~a"
           (assq-ref props 'measure-no)
           (beat-string props)))))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Print annotations to console
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Print annotations to the console
%#(define (do-print-annotations)
#(register-export-routine 'console
   (lambda (annotations)
     (for-each
      (lambda (ann)
        (begin
         (ly:input-message (assq-ref ann 'location) "\nAnnotation:")
         (ly:message (format "    ~a" (format-location ann)))
         (write-lines
          (format-property-messages ann
            (get-skipped-attributes #f))
          ly:message)
         (ly:message "")))
      annotations)))
