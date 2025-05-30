;; Enhanced Event Emitting Storage Contract
;; Advanced storage functionality with comprehensive event emission and access control

;; =============================================================================
;; ERROR CONSTANTS
;; =============================================================================
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INVALID-VALUE (err u101))
(define-constant ERR-VALUE-TOO-LARGE (err u102))
(define-constant ERR-VALUE-TOO-SMALL (err u103))
(define-constant ERR-PAUSED (err u104))
(define-constant ERR-NOT-FOUND (err u105))
(define-constant ERR-ALREADY-EXISTS (err u106))
(define-constant ERR-INSUFFICIENT-PERMISSION (err u107))
(define-constant ERR-RATE-LIMITED (err u108))
(define-constant ERR-INVALID-BATCH-SIZE (err u109))

;; =============================================================================
;; CONSTANTS
;; =============================================================================
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-VALUE u1000000)
(define-constant MIN-VALUE u0)
(define-constant MAX-BATCH-SIZE u10)
(define-constant RATE-LIMIT-WINDOW u10) ;; blocks
(define-constant MAX-UPDATES-PER-WINDOW u5)

;; =============================================================================
;; DATA VARIABLES
;; =============================================================================
;; Primary storage
(define-data-var stored-value uint u0)
(define-data-var previous-value uint u0)

;; Contract state
(define-data-var contract-paused bool false)
(define-data-var emergency-stop bool false)

;; Event tracking
(define-data-var event-counter uint u0)
(define-data-var total-updates uint u0)

;; Access control
(define-data-var admin-count uint u1)

;; Rate limiting
(define-data-var last-update-block uint u0)
(define-data-var updates-in-window uint u0)

;; Statistics
(define-data-var max-value-reached uint u0)
(define-data-var min-value-reached uint u0)
(define-data-var total-value-sum uint u0)

;; =============================================================================
;; DATA MAPS
;; =============================================================================
;; Enhanced event log with metadata
(define-map event-log 
  { event-id: uint }
  { 
    old-value: uint,
    new-value: uint,
    updated-by: principal,
    block-height: uint,
    event-type: (string-ascii 20),
    gas-used: uint,
    metadata: (optional (string-ascii 100))
  }
)

;; Access control lists
(define-map admins principal bool)
(define-map authorized-updaters principal bool)
(define-map banned-users principal bool)

;; User activity tracking
(define-map user-stats
  principal
  {
    total-updates: uint,
    last-update-block: uint,
    first-update-block: uint,
    total-value-set: uint
  }
)

;; Value history snapshots
(define-map value-snapshots
  uint ;; block-height
  {
    value: uint,
    snapshot-by: principal,
    event-id: uint
  }
)

;; Batch operations
(define-map batch-operations
  uint ;; batch-id
  {
    values: (list 10 uint),
    executed-by: principal,
    block-height: uint,
    success-count: uint
  }
)

;; Permission levels
(define-map user-permissions
  principal
  {
    can-update: bool,
    can-read: bool,
    can-snapshot: bool,
    can-batch: bool,
    permission-level: uint
  }
)

;; =============================================================================
;; INITIALIZATION
;; =============================================================================
;; Initialize contract owner as admin
(map-set admins CONTRACT-OWNER true)
(map-set authorized-updaters CONTRACT-OWNER true)
(map-set user-permissions CONTRACT-OWNER {
  can-update: true,
  can-read: true,
  can-snapshot: true,
  can-batch: true,
  permission-level: u999
})

;; =============================================================================
;; PRIVATE FUNCTIONS
;; =============================================================================
(define-private (is-admin (user principal))
  (default-to false (map-get? admins user))
)

(define-private (is-authorized-updater (user principal))
  (or (is-admin user) (default-to false (map-get? authorized-updaters user)))
)

(define-private (is-banned (user principal))
  (default-to false (map-get? banned-users user))
)

(define-private (update-user-stats (user principal) (value uint))
  (let ((current-stats (default-to 
                      { total-updates: u0, last-update-block: u0, first-update-block: block-height, total-value-set: u0 }
                      (map-get? user-stats user))))
    (map-set user-stats user {
      total-updates: (+ (get total-updates current-stats) u1),
      last-update-block: block-height,
      first-update-block: (if (is-eq (get total-updates current-stats) u0) 
                              block-height 
                              (get first-update-block current-stats)),
      total-value-set: (+ (get total-value-set current-stats) value)
    })
  )
)

(define-private (check-rate-limit (user principal))
  (let ((current-block block-height)
    (last-block (var-get last-update-block))
    (current-updates (var-get updates-in-window)))
    (if (> (- current-block last-block) RATE-LIMIT-WINDOW)
      (begin
        (var-set updates-in-window u1)
        (var-set last-update-block current-block)
        true
      )
      (if (< current-updates MAX-UPDATES-PER-WINDOW)
        (begin
          (var-set updates-in-window (+ current-updates u1))
          true
        )
        false
      )
    )
  )
)

(define-private (validate-value (value uint))
  (and (>= value MIN-VALUE) (<= value MAX-VALUE))
)

(define-private (log-event (old-val uint) (new-val uint) (event-type (string-ascii 20)) (metadata (optional (string-ascii 100))))
  (let ((event-id (var-get event-counter)))
    (map-set event-log
      { event-id: event-id }
      {
        old-value: old-val,
        new-value: new-val,
        updated-by: tx-sender,
        block-height: block-height,
        event-type: event-type,
        gas-used: u0, ;; Would need actual gas tracking in real implementation
        metadata: metadata
      }
    )
      
    (print {
      event: event-type,
      event-id: event-id,
      old-value: old-val,
      new-value: new-val,
      updated-by: tx-sender,
      block-height: block-height,
      metadata: metadata
    })
    
    (var-set event-counter (+ event-id u1))
    event-id
  )
)

;; =============================================================================
;; PUBLIC FUNCTIONS - CORE STORAGE
;; =============================================================================
(define-public (update-value (new-value uint))
  (begin
    ;; Security checks
    (asserts! (not (var-get contract-paused)) ERR-PAUSED)
    (asserts! (not (var-get emergency-stop)) ERR-PAUSED)
    (asserts! (not (is-banned tx-sender)) ERR-UNAUTHORIZED)
    (asserts! (is-authorized-updater tx-sender) ERR-UNAUTHORIZED)
    (asserts! (validate-value new-value) ERR-INVALID-VALUE)
    (asserts! (check-rate-limit tx-sender) ERR-RATE-LIMITED)
    
    (let ((current-value (var-get stored-value)))
      ;; Update statistics
      (var-set previous-value current-value)
      (var-set total-updates (+ (var-get total-updates) u1))
      (var-set total-value-sum (+ (var-get total-value-sum) new-value))
      
      ;; Update min/max tracking
      (if (> new-value (var-get max-value-reached))
        (var-set max-value-reached new-value)
        true
      )
      (if (< new-value (var-get min-value-reached))
        (var-set min-value-reached new-value)
        true
      )
          
      ;; Log event
      (log-event current-value new-value "value-updated" none)
      
      ;; Update user stats
      (update-user-stats tx-sender new-value)
      
      ;; Set new value
      (var-set stored-value new-value)
      
      (ok {
        event-id: (- (var-get event-counter) u1),
        old-value: current-value,
        new-value: new-value,
        total-updates: (var-get total-updates)
      })
    )
  )
)

(define-public (increment-value (amount uint))
  (let ((current-value (var-get stored-value)))
    (update-value (+ current-value amount))
  )
)

(define-public (decrement-value (amount uint))
  (let ((current-value (var-get stored-value)))
    (if (>= current-value amount)
      (update-value (- current-value amount))
      (update-value u0)
    )
  )
)

(define-public (multiply-value (factor uint))
  (let ((current-value (var-get stored-value)))
    (update-value (* current-value factor))
  )
)

;; =============================================================================
;; PRIVATE FUNCTIONS - BATCH HELPERS
;; =============================================================================
(define-private (process-batch-value (value uint) (previous-result (response uint uint)))
  (match previous-result
    success-count (match (update-value value)
                    update-result (ok (+ success-count u1))
                    error-code (err error-code)
                  )
    error-code (err error-code)
  )
)

;; =============================================================================
;; PUBLIC FUNCTIONS - BATCH OPERATIONS
;; =============================================================================
(define-public (batch-update (values (list 10 uint)))
  (begin
    (asserts! (is-authorized-updater tx-sender) ERR-UNAUTHORIZED)
    (asserts! (not (var-get contract-paused)) ERR-PAUSED)
    (asserts! (<= (len values) MAX-BATCH-SIZE) ERR-INVALID-BATCH-SIZE)
    
    (let ((batch-id (var-get event-counter))
          (batch-result (fold process-batch-value values (ok u0))))
      (match batch-result
        success-count (begin
            (map-set batch-operations batch-id {
              values: values,
              executed-by: tx-sender,
              block-height: block-height,
              success-count: success-count
            })
            (log-event u0 success-count "batch-completed" (some "Batch update completed"))
            (ok {
              batch-id: batch-id,
              values-processed: success-count,
              total-values: (len values)
            }))
        error-code (begin
            (map-set batch-operations batch-id {
              values: values,
              executed-by: tx-sender,
              block-height: block-height,
              success-count: u0
            })
            (log-event u0 u0 "batch-failed" (some "Batch update failed"))
            (err error-code))
      )
    )
  )
)

;; =============================================================================
;; PUBLIC FUNCTIONS - SNAPSHOTS
;; =============================================================================
(define-public (create-snapshot)
  (begin
    (asserts! (is-authorized-updater tx-sender) ERR-UNAUTHORIZED)
    (let ((current-value (var-get stored-value))
      (event-id (log-event current-value current-value "snapshot-created" none)))
      (map-set value-snapshots block-height {
        value: current-value,
        snapshot-by: tx-sender,
        event-id: event-id
      })
      (ok { snapshot-block: block-height, value: current-value, event-id: event-id })
    )
  )
)

(define-public (restore-snapshot (snapshot-block uint))
  (begin
    (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
    (match (map-get? value-snapshots snapshot-block)
      snapshot-data (let ((snapshot-value (get value snapshot-data)))
                      (log-event (var-get stored-value) snapshot-value "snapshot-restored" 
                        (some "Restored from snapshot")
                      )
                      (var-set stored-value snapshot-value)
                      (ok snapshot-value)
                    )
      ERR-NOT-FOUND
    )
  )
)

;; =============================================================================
;; PUBLIC FUNCTIONS - ACCESS CONTROL
;; =============================================================================
(define-public (add-admin (new-admin principal))
  (begin
    (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
    (asserts! (not (is-banned new-admin)) ERR-UNAUTHORIZED)
    (map-set admins new-admin true)
    (map-set authorized-updaters new-admin true)
    (var-set admin-count (+ (var-get admin-count) u1))
    (log-event u0 u0 "admin-added" (some "New admin added"))
    (ok true)
  )
)

(define-public (remove-admin (admin principal))
  (begin
    (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
    (asserts! (not (is-eq admin CONTRACT-OWNER)) ERR-UNAUTHORIZED) ;; Can't remove owner
    (map-delete admins admin)
    (var-set admin-count (- (var-get admin-count) u1))
    (log-event u0 u0 "admin-removed" (some "Admin removed"))
    (ok true)
  )
)

(define-public (authorize-updater (user principal))
  (begin
    (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
    (map-set authorized-updaters user true)
    (log-event u0 u0 "updater-authorized" none)
    (ok true)
  )
)

(define-public (revoke-updater (user principal))
  (begin
    (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
    (map-delete authorized-updaters user)
    (log-event u0 u0 "updater-revoked" none)
    (ok true)
  )
)

(define-public (ban-user (user principal))
  (begin
    (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
    (asserts! (not (is-eq user CONTRACT-OWNER)) ERR-UNAUTHORIZED)
    (map-set banned-users user true)
    (log-event u0 u0 "user-banned" none)
    (ok true)
  )
)

(define-public (unban-user (user principal))
  (begin
    (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
    (map-delete banned-users user)
    (log-event u0 u0 "user-unbanned" none)
    (ok true)
  )
)

;; =============================================================================
;; PUBLIC FUNCTIONS - CONTRACT CONTROL
;; =============================================================================
(define-public (pause-contract)
  (begin
    (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
    (var-set contract-paused true)
    (log-event u0 u0 "contract-paused" none)
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
    (var-set contract-paused false)
    (log-event u0 u0 "contract-unpaused" none)
    (ok true)
  )
)

(define-public (activate-emergency-stop)
  (begin
    (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
    (var-set emergency-stop true)
    (var-set contract-paused true)
    (log-event u0 u0 "emergency-stop" (some "Emergency stop activated"))
    (ok true)
  )
)

(define-public (reset-storage)
  (begin
    (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
    (let ((current-value (var-get stored-value)))
      (log-event current-value u0 "storage-reset" none)
      (var-set stored-value u0)
      (var-set previous-value current-value)
      (ok true)
    )
  )
)

;; =============================================================================
;; READ-ONLY FUNCTIONS - BASIC GETTERS
;; =============================================================================
(define-read-only (get-value)
  (var-get stored-value)
)

(define-read-only (get-previous-value)
  (var-get previous-value)
)

(define-read-only (get-contract-info)
  {
    current-value: (var-get stored-value),
    previous-value: (var-get previous-value),
    total-updates: (var-get total-updates),
    event-count: (var-get event-counter),
    is-paused: (var-get contract-paused),
    emergency-stop: (var-get emergency-stop),
    admin-count: (var-get admin-count)
  }
)

(define-read-only (get-statistics)
  {
    max-value-reached: (var-get max-value-reached),
    min-value-reached: (var-get min-value-reached),
    total-value-sum: (var-get total-value-sum),
    average-value: (if (> (var-get total-updates) u0) 
                      (/ (var-get total-value-sum) (var-get total-updates)) 
                      u0),
    total-updates: (var-get total-updates)
  }
)

;; =============================================================================
;; READ-ONLY FUNCTIONS - EVENT SYSTEM
;; =============================================================================
(define-read-only (get-event (event-id uint))
  (map-get? event-log { event-id: event-id })
)

(define-read-only (get-latest-event)
  (let ((latest-id (- (var-get event-counter) u1)))
    (if (>= latest-id u0)
      (map-get? event-log { event-id: latest-id })
      none
    )
  )
)

(define-read-only (get-event-count)
  (var-get event-counter)
)

(define-read-only (get-events-by-type (event-type (string-ascii 20)))
  ;; This would need a more complex implementation to filter events
  ;; For now, returns the count of total events
  (var-get event-counter)
)

;; =============================================================================
;; READ-ONLY FUNCTIONS - USER MANAGEMENT
;; =============================================================================
(define-read-only (get-user-stats (user principal))
  (map-get? user-stats user)
)

(define-read-only (is-user-admin (user principal))
  (is-admin user)
)

(define-read-only (is-user-authorized (user principal))
  (is-authorized-updater user)
)

(define-read-only (is-user-banned (user principal))
  (is-banned user)
)

(define-read-only (get-user-permissions (user principal))
  (map-get? user-permissions user)
)

;; =============================================================================
;; READ-ONLY FUNCTIONS - SNAPSHOTS AND HISTORY
;; =============================================================================
(define-read-only (get-snapshot (snapshot-block uint))
  (map-get? value-snapshots snapshot-block)
)

(define-read-only (get-batch-operation (batch-id uint))
  (map-get? batch-operations batch-id)
)

;; =============================================================================
;; READ-ONLY FUNCTIONS - VALIDATION
;; =============================================================================
(define-read-only (can-update (user principal) (value uint))
  (and 
    (not (var-get contract-paused))
    (not (var-get emergency-stop))
    (not (is-banned user))
    (is-authorized-updater user)
    (validate-value value)
  )
)

(define-read-only (get-value-limits)
  { min-value: MIN-VALUE, max-value: MAX-VALUE }
)

(define-read-only (get-rate-limit-info)
  {
    max-updates-per-window: MAX-UPDATES-PER-WINDOW,
    window-size: RATE-LIMIT-WINDOW,
    current-updates: (var-get updates-in-window),
    last-update-block: (var-get last-update-block)
  }
)