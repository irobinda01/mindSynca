;; Multi-Send with Analytics Dashboard Integration
;; A smart contract for batch STX transfers with comprehensive event tracking

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-INVALID-RECIPIENT (err u103))
(define-constant ERR-BATCH-TOO-LARGE (err u104))
(define-constant ERR-PAUSED (err u105))

;; Maximum batch size to prevent gas limit issues
(define-constant MAX-BATCH-SIZE u50)

;; Data Variables
(define-data-var contract-paused bool false)
(define-data-var total-batches-processed uint u0)
(define-data-var total-transfers-processed uint u0)
(define-data-var total-volume-transferred uint u0)

;; Data Maps for analytics
(define-map user-stats 
    principal 
    {
        batches-sent: uint,
        total-sent: uint,
        total-transfers: uint,
        last-batch-block: uint
    }
)

(define-map daily-stats 
    uint ;; block height / blocks-per-day to get daily buckets
    {
        batches: uint,
        transfers: uint,
        volume: uint,
        unique-senders: uint
    }
)

;; Recipient structure for batch transfers
(define-public (multi-send (recipients (list 50 {recipient: principal, amount: uint})))
    (begin
        ;; Validate contract is not paused
        (asserts! (not (var-get contract-paused)) ERR-PAUSED)
        
        ;; Validate batch size
        (asserts! (<= (len recipients) MAX-BATCH-SIZE) ERR-BATCH-TOO-LARGE)
        
        ;; Validate all recipients and amounts
        (asserts! (validate-recipients recipients) ERR-INVALID-RECIPIENT)
        
        ;; Calculate total amount needed
        (let ((total-amount (calculate-total-amount recipients)))
            ;; Validate sender has sufficient balance
            (asserts! (>= (stx-get-balance tx-sender) total-amount) ERR-INSUFFICIENT-BALANCE)
            
            ;; Process the batch transfer
            (match (process-batch-transfer recipients)
                success (begin
                    ;; Update analytics
                    (update-batch-analytics recipients total-amount)
                    
                    ;; Emit comprehensive analytics event
                    (print {
                        event: "batch-transfer-completed",
                        sender: tx-sender,
                        batch-id: (+ (var-get total-batches-processed) u1),
                        timestamp: block-height,
                        recipients-count: (len recipients),
                        total-amount: total-amount,
                        gas-used: (get-gas-estimate (len recipients)),
                        batch-stats: (get-batch-statistics recipients)
                    })
                    
                    (ok success)
                )
                error (err error)
            )
        )
    )
)

;; Internal function to process batch transfers
(define-private (process-batch-transfer (recipients (list 50 {recipient: principal, amount: uint})))
    (let ((results (map process-single-transfer recipients)))
        (if (> (len (filter is-err-response results)) u0)
            (err u999) ;; At least one transfer failed
            (ok {
                successful-transfers: (len recipients),
                total-amount: (calculate-total-amount recipients)
            })
        )
    )
)

;; Process individual transfer with detailed event emission
(define-private (process-single-transfer (transfer-data {recipient: principal, amount: uint}))
    (let ((recipient (get recipient transfer-data))
          (amount (get amount transfer-data)))
        (match (stx-transfer? amount tx-sender recipient)
            success (begin
                ;; Emit individual transfer event for detailed tracking
                (print {
                    event: "individual-transfer",
                    sender: tx-sender,
                    recipient: recipient,
                    amount: amount,
                    block-height: block-height,
                    tx-sender-balance-after: (stx-get-balance tx-sender)
                })
                (ok true)
            )
            error (begin
                ;; Emit transfer failure event
                (print {
                    event: "transfer-failed",
                    sender: tx-sender,
                    recipient: recipient,
                    amount: amount,
                    error: error,
                    block-height: block-height
                })
                (err error)
            )
        )
    )
)

;; Update comprehensive analytics after successful batch
(define-private (update-batch-analytics (recipients (list 50 {recipient: principal, amount: uint})) (total-amount uint))
    (let ((batch-count (+ (var-get total-batches-processed) u1))
          (transfer-count (+ (var-get total-transfers-processed) (len recipients)))
          (volume (+ (var-get total-volume-transferred) total-amount))
          (daily-bucket (/ block-height u144))) ;; Assuming ~144 blocks per day
        
        ;; Update global stats
        (var-set total-batches-processed batch-count)
        (var-set total-transfers-processed transfer-count)
        (var-set total-volume-transferred volume)
        
        ;; Update user stats
        (map-set user-stats tx-sender {
            batches-sent: (+ (get-user-batch-count tx-sender) u1),
            total-sent: (+ (get-user-total-sent tx-sender) total-amount),
            total-transfers: (+ (get-user-transfer-count tx-sender) (len recipients)),
            last-batch-block: block-height
        })
        
        ;; Update daily stats
        (map-set daily-stats daily-bucket {
            batches: (+ (get-daily-batch-count daily-bucket) u1),
            transfers: (+ (get-daily-transfer-count daily-bucket) (len recipients)),
            volume: (+ (get-daily-volume daily-bucket) total-amount),
            unique-senders: (+ (get-daily-unique-senders daily-bucket) u1)
        })
        
        ;; Emit analytics update event
        (print {
            event: "analytics-updated",
            global-stats: {
                total-batches: batch-count,
                total-transfers: transfer-count,
                total-volume: volume
            },
            user-stats: (map-get? user-stats tx-sender),
            daily-stats: (map-get? daily-stats daily-bucket)
        })
    )
)

;; Generate detailed batch statistics for analytics
(define-private (get-batch-statistics (recipients (list 50 {recipient: principal, amount: uint})))
    (let ((amounts (map get-amount recipients))
          (total (calculate-total-amount recipients)))
        {
            min-amount: (fold min-amount amounts u999999999),
            max-amount: (fold max-amount amounts u0),
            avg-amount: (/ total (len recipients)),
            total-amount: total,
            recipient-count: (len recipients),
            unique-recipients: (len (get-unique-recipients recipients))
        }
    )
)

;; Helper functions for validation
(define-private (validate-recipients (recipients (list 50 {recipient: principal, amount: uint})))
    (is-eq (len (filter validate-single-recipient recipients)) (len recipients))
)

(define-private (validate-single-recipient (transfer-data {recipient: principal, amount: uint}))
    (and 
        (> (get amount transfer-data) u0)
        (not (is-eq (get recipient transfer-data) tx-sender))
    )
)

;; Calculate total amount for batch
(define-private (calculate-total-amount (recipients (list 50 {recipient: principal, amount: uint})))
    (fold + (map get-amount recipients) u0)
)

(define-private (get-amount (transfer-data {recipient: principal, amount: uint}))
    (get amount transfer-data)
)

;; Analytics helper functions
(define-private (get-user-batch-count (user principal))
    (default-to u0 (get batches-sent (map-get? user-stats user)))
)

(define-private (get-user-total-sent (user principal))
    (default-to u0 (get total-sent (map-get? user-stats user)))
)

(define-private (get-user-transfer-count (user principal))
    (default-to u0 (get total-transfers (map-get? user-stats user)))
)

(define-private (get-daily-batch-count (day uint))
    (default-to u0 (get batches (map-get? daily-stats day)))
)

(define-private (get-daily-transfer-count (day uint))
    (default-to u0 (get transfers (map-get? daily-stats day)))
)

(define-private (get-daily-volume (day uint))
    (default-to u0 (get volume (map-get? daily-stats day)))
)

(define-private (get-daily-unique-senders (day uint))
    (default-to u0 (get unique-senders (map-get? daily-stats day)))
)

;; Utility functions for statistics
(define-private (min-amount (a uint) (b uint))
    (if (< a b) a b)
)

(define-private (max-amount (a uint) (b uint))
    (if (> a b) a b)
)

(define-private (is-err-response (response (response bool uint)))
    (is-err response)
)

(define-private (get-gas-estimate (recipient-count uint))
    (* recipient-count u1000) ;; Rough gas estimate per transfer
)

(define-private (get-unique-recipients (recipients (list 50 {recipient: principal, amount: uint})))
    ;; Simplified unique count - in practice, you'd need a more complex deduplication
    recipients
)

;; Read-only functions for analytics dashboard
(define-read-only (get-global-stats)
    {
        total-batches: (var-get total-batches-processed),
        total-transfers: (var-get total-transfers-processed),
        total-volume: (var-get total-volume-transferred),
        contract-paused: (var-get contract-paused)
    }
)

(define-read-only (get-user-analytics (user principal))
    (map-get? user-stats user)
)

(define-read-only (get-daily-analytics (day uint))
    (map-get? daily-stats day)
)

(define-read-only (get-contract-health)
    {
        is-paused: (var-get contract-paused),
        total-batches: (var-get total-batches-processed),
        avg-batch-size: (if (> (var-get total-batches-processed) u0)
                           (/ (var-get total-transfers-processed) (var-get total-batches-processed))
                           u0)
    }
)

;; Admin functions
(define-public (pause-contract)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
        (var-set contract-paused true)
        (print {
            event: "contract-paused",
            admin: tx-sender,
            timestamp: block-height
        })
        (ok true)
    )
)

(define-public (unpause-contract)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
        (var-set contract-paused false)
        (print {
            event: "contract-unpaused",
            admin: tx-sender,
            timestamp: block-height
        })
        (ok true)
    )
)