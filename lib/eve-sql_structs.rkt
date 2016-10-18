#lang racket

(provide (all-defined-out))

;; SQL lookup to appropriate struct

(define (sql-parse->struct query #:struct struct)
  (call-with-values
      (lambda () (vector->values query))
    struct))

;; Struct definitions

;; parse-type
(struct invTypes (id group name) #:transparent)

;; parse-group
(struct invGroups (id name) #:transparent)

;; parse-solarsystem
(struct mapSolarSystems (region constellation id name) #:transparent)

;; parse-constellation
(struct mapConstellations (region id name) #:transparent)

;; parse-region
(struct mapRegions (id name) #:transparent)

;; parse-corporation
(struct customCorporations (id ticker name) #:transparent)

;; sql-corporation-update-input
(struct sql-corporation (id ticker name datetime) #:transparent)

;; parse-alliance
(struct customAlliances (id name ticker) #:transparent)

;; parse-map
(struct mapDenormalize (id type group system constellation region name) #:transparent)

;; sql-moon-update-scan & sql-moon-update-empty
(struct sql-moon (region constellation system planet moon alliance corporation datetime type online scanid) #:transparent)

;; sql-goo->struct & sql-goo-update-scan
(struct sql-goo (region constellation system planet moon datetime type amount) #:transparent)

;; sql-citadel-update-scan
(struct sql-citadel (region constellation system location alliance corporation datetime type scanid))

;; sql-killmail (supers + towers)
(struct sql-killmail (shiptype characterid charactername corporationid corporationname allianceid alliancename location system region datetime killid victimtype [eventtype #:auto #:mutable]) #:auto-value "" #:prefab)

;; dscan
(struct dscan (name type distance) #:prefab #:extra-constructor-name parse-dscan)

;; sql-character-maybe-ids & sql-character-update-ids (local scan)
(struct sql-character (id name corporationid corporation allianceid alliance datetime) #:prefab)

;; type-association-list
(struct typeAssociation (typeid typename groupid groupname category) #:prefab)

;; JWT token for (recon) usernames
(struct recon-jwt (issuer audiences subject username))

;; Basic auth user, input+salt
(struct scrypt-hash (user input salt) #:prefab)

;; Basic auth user, email+salt, password+salt
(struct scrypt-full (user email email-salt password password-salt) #:prefab)
