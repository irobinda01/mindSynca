
;; title: impact_reporting
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

;; Impact Reporting Contract

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-charity-not-found (err u101))

;; Define data maps
(define-map charities
  { charity-id: uint }
  { name: (string-ascii 100), ipfs-hash: (string-ascii 46) }
)

(define-map reports
  { charity-id: uint, report-id: uint }
  { title: (string-ascii 100), ipfs-hash: (string-ascii 46), timestamp: uint }
)

;; Define data variables
(define-data-var report-count uint u0)

;; Public functions

;; Register a new charity
(define-public (register-charity (charity-id uint) (name (string-ascii 100)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (map-insert charities { charity-id: charity-id } { name: name, ipfs-hash: "" })
    (ok true)
  )
)

;; Update charity's IPFS hash
(define-public (update-charity-ipfs (charity-id uint) (ipfs-hash (string-ascii 46)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (match (map-get? charities { charity-id: charity-id })
      charity (ok (map-set charities { charity-id: charity-id }
                   (merge charity { ipfs-hash: ipfs-hash })))
      err-charity-not-found
    )
  )
)

;; Add a new impact report
(define-public (add-report (charity-id uint) (title (string-ascii 100)) (ipfs-hash (string-ascii 46)))
  (let
    (
      (report-id (var-get report-count))
    )
    (asserts! (is-some (map-get? charities { charity-id: charity-id })) err-charity-not-found)
    (map-insert reports
      { charity-id: charity-id, report-id: report-id }
      { title: title, ipfs-hash: ipfs-hash, timestamp: block-height }
    )
    (var-set report-count (+ report-id u1))
    (ok report-id)
  )
)

;; Read-only functions

;; Get charity details
(define-read-only (get-charity (charity-id uint))
  (map-get? charities { charity-id: charity-id })
)

;; Get report details
(define-read-only (get-report (charity-id uint) (report-id uint))
  (map-get? reports { charity-id: charity-id, report-id: report-id })
)

;; Get total number of reports
(define-read-only (get-report-count)
  (var-get report-count)
)