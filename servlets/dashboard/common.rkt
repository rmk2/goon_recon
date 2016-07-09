#lang racket

(require eve)

(provide (all-defined-out))

;; Define maximum admissable distance (in km) for moons/towers

(define max_distance (make-parameter 10000))

;; Check if a given query string is a valid solarSystemName

;; (define (system? query) (findf (lambda (str) (equal? (string-downcase query) (string-downcase str)))
;; 			       (map vector->values
;; 				    (query-rows sqlc "SELECT DISTINCT solarSystemName FROM mapSolarSystems"))))

(define (system? query)
  (if (false? (query-maybe-row sqlc "SELECT solarSystemName FROM mapSolarSystems WHERE solarSystemName = ?" query))
      #f
      query))

;; Moon database

(define regions
  '("A-R00001" "A-R00002" "A-R00003" "Aridia" "B-R00004" "B-R00005" "B-R00006" "B-R00007" "B-R00008" "Black Rise" "Branch" "C-R00009" "C-R00010" "C-R00011" "C-R00012" "C-R00013" "C-R00014" "C-R00015" "Cache" "Catch" "Cloud Ring" "Cobalt Edge" "Curse" "D-R00016" "D-R00017" "D-R00018" "D-R00019" "D-R00020" "D-R00021" "D-R00022" "D-R00023" "Deklein" "Delve" "Derelik" "Detorid" "Devoid" "Domain" "E-R00024" "E-R00025" "E-R00026" "E-R00027" "E-R00028" "E-R00029" "Esoteria" "Essence" "Etherium Reach" "Everyshore" "F-R00030" "Fade" "Feythabolis" "Fountain" "G-R00031" "Geminate" "Genesis" "Great Wildlands" "H-R00032" "Heimatar" "Immensea" "Impass" "Insmother" "K-R00033" "Kador" "Khanid" "Kor-Azor" "Lonetrek" "Malpais" "Metropolis" "Molden Heath" "Oasa" "Omist" "Outer Passage" "Outer Ring" "Paragon Soul" "Period Basis" "Perrigen Falls" "Placid" "Providence" "Pure Blind" "Querious" "Scalding Pass" "Sinq Laison" "Solitude" "Stain" "Syndicate" "Tash-Murkon" "Tenal" "Tenerifis" "The Bleak Lands" "The Citadel" "The Forge" "The Kalevala Expanse" "The Spire" "Tribute" "Vale of the Silent" "Venal" "Verge Vendor" "Wicked Creek"))

(define (region? query) (findf (lambda (str) (equal? (string-downcase query) (string-downcase str))) regions))

(define (sql-get-scanned-regions table)
  (map vector->values (query-rows sqlc (string-append "SELECT DISTINCT regionName FROM " table " ORDER BY regionName"))))

(define (query-regions lst) (cond [(empty? lst) null]
				  [(and (not (empty? lst)) (string-empty? (car lst))) null]
				  [else (filter-map region? (string-split (car lst) ","))]))

(define (user-filter-regions lst #:filter-function filter-function #:function function)
  (cond
   [(not (empty? (query-regions lst)))
    (append-map (lambda (region) (filter-function region))
		(query-regions lst))]
   [else function]))

(define (get-regions req)
  (match
    (bindings-assq
     #"region"
     (request-bindings/raw req))
    [(? binding:form? b)
     (list
      (bytes->string/utf-8
       (binding:form-value b)))]
    [_ null]))
