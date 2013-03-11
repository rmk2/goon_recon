#!/usr/bin/env stklos-script

;; #! /usr/bin/guile  \
;; -q -e main -s
;; !#

;; (use-modules (ice-9 format))
;; (use-modules (ice-9 ftw))
;; (use-modules (ice-9 rdelim))

(define-syntax tag
  (syntax-rules (/ class id)
    ((_ type) (format #t "<~a> ~&" type))
    ((_ type /) (format #t "</~a> ~&" type))
    ((_ type exp class) (format #t "<~a class=\"~a\"> ~&" type exp))
    ((_ type exp id) (format #t "<~a id=\"~a\"> ~&" type exp))
    ((_ type field)
     (format #t "<~a>~a</~a> ~&" type field type))
    ((_ type field exp class)
     (format #t "<~a class=\"~a\">~a</~a> ~&" type exp field type))
    ((_ type field exp id)
     (format #t "<~a id=\"~a\">~a</~a> ~&" type exp field type))
    ((_ type field exp1 exp2 class id)
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
  (tag 'div 'main id)
  (tag 'div 'header id)
  (tag 'h1 "End Cycle" 'title id)
  (tag 'div "A blog" 'description id)
  (tag 'div /)
  (tag 'div 'content class)
  (let loop ((postdir "./schemeblog/posts") (i 0))
    (if (not (= i (length (directory-files postdir))))
	(begin
	  (tag 'div 'post class)
	  (tag 'div (date) 'timestamp class)
	  (format #t "~a" (port->string (open-input-file (string-append postdir "/" (list-ref (reverse (directory-files postdir)) i)))))
	  (tag 'div /)
	  (loop postdir (+ i 1)))))
  (tag 'div /)    
  (tag 'div "CC BY-NC-SA -- rmk2" 'footer id)
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
