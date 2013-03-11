#!/usr/bin/env stklos-script

;; #! /usr/bin/guile  \
;; -q -e main -s
;; !#

;; (use-modules (ice-9 format))
;; (use-modules (ice-9 ftw))
;; (use-modules (ice-9 rdelim))

(define-syntax tag
  (syntax-rules (/ class: id:)
    ((_ type) (format #t "<~a> ~&" type))
    ((_ type /) (format #t "</~a> ~&" type))
    ((_ type class: exp) (format #t "<~a class=\"~a\"> ~&" type exp))
    ((_ type id: exp) (format #t "<~a id=\"~a\"> ~&" type exp))
    ((_ type field)
     (format #t "<~a>~a</~a> ~&" type field type))
    ((_ type field class: exp)
     (format #t "<~a class=\"~a\">~a</~a> ~&" type exp field type))
    ((_ type field id: exp)
     (format #t "<~a id=\"~a\">~a</~a> ~&" type exp field type))
    ((_ type field class: exp1 id: exp2)
     (format #t "<~a class=\"~a\" id=\"~a\">~a</~a> ~&" type exp1 exp2 field type))))

(define (index)
  (format #t "<!DOCTYPE html> ~&")
  (format #t "<meta http-equiv=\"Content-type\" content=\"text/html;charset=UTF-8\" /> ~&")
  (format #t "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/main.css\" /> ~&")
  (tag 'html)
  (tag 'head)
  (tag 'title "End Cycle")
  (tag 'head /)
  (tag 'body)
  (tag 'div id: 'main)
  (tag 'div id: 'header)
  (tag 'h1 "End Cycle" id: 'title)
  (tag 'div "A blog" id: 'description)
  (tag 'div /)
  (tag 'div class: 'content)
  (let loop ((postdir "./schemeblog/posts") (i 0))
    (if (not (= i (length (directory-files postdir))))
	(begin
	  (tag 'div class: 'post)
	  (tag 'div (date) class: 'timestamp)
	  (format #t "~a" (port->string (open-input-file (string-append postdir "/" (list-ref (reverse (directory-files postdir)) i)))))
	  (tag 'div /)
	  (loop postdir (+ i 1)))))
  (tag 'div /)    
  (tag 'div "CC BY-NC-SA -- rmk2" id: 'footer)
  (tag 'div /)
  (tag 'body /)
  (tag 'html /))

(define (archive)
  (let loop ((postdir "./schemeblog/posts") (i 0))
    (if (not (= i (length (directory-files postdir))))
	(begin
	  (format #t "~a ~&" (list-ref (reverse (directory-files postdir)) i))
	  (loop postdir (+ i 1))))))

(define (main arg)
  (index))
