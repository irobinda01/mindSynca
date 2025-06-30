;; Enhanced FileRegistry Smart Contract
;; Purpose: Register and manage file metadata linked to IPFS hashes with advanced features

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-empty-data (err u104))
(define-constant err-invalid-access-level (err u105))
(define-constant err-file-locked (err u106))
(define-constant err-insufficient-payment (err u107))
(define-constant err-invalid-time (err u108))
(define-constant err-quota-exceeded (err u109))
(define-constant err-invalid-category (err u110))
(define-constant err-backup-limit-reached (err u111))
(define-constant err-not-shared (err u112))

;; Data Variables
(define-data-var next-file-id uint u1)
(define-data-var next-category-id uint u1)
(define-data-var contract-paused bool false)
(define-data-var registration-fee uint u1000000) ;; 1 STX in microSTX
(define-data-var max-files-per-user uint u1000)
(define-data-var max-file-size uint u1073741824) ;; 1GB in bytes

;; Data Maps
;; Enhanced file metadata with more features
(define-map files
  { file-id: uint }
  {
    filename: (string-ascii 256),
    ipfs-cid: (string-ascii 128),
    uploader: principal,
    timestamp: uint,
    updated-at: uint,
    file-size: uint,
    file-type: (string-ascii 64),
    description: (string-utf8 512),
    access-level: (string-ascii 16), ;; "public", "private", "restricted"
    download-count: uint,
    is-locked: bool,
    expiry-time: (optional uint),
    category-id: uint,
    tags: (list 10 (string-ascii 32)),
    checksum: (string-ascii 64),
    encryption-key-hash: (optional (string-ascii 64)),
    version: uint,
    license: (string-ascii 128)
  }
)

;; File categories
(define-map categories
  { category-id: uint }
  {
    name: (string-ascii 64),
    description: (string-utf8 256),
    created-by: principal,
    file-count: uint
  }
)

;; File sharing permissions
(define-map file-permissions
  { file-id: uint, user: principal }
  {
    permission-type: (string-ascii 16), ;; "read", "write", "admin"
    granted-by: principal,
    granted-at: uint,
    expires-at: (optional uint)
  }
)

;; File access logs
(define-map access-logs
  { file-id: uint, user: principal, timestamp: uint }
  {
    action: (string-ascii 32), ;; "download", "view", "update", "share"
    ip-hash: (optional (string-ascii 64)),
    user-agent-hash: (optional (string-ascii 64))
  }
)

;; File versions/history
(define-map file-versions
  { file-id: uint, version: uint }
  {
    ipfs-cid: (string-ascii 128),
    updated-by: principal,
    timestamp: uint,
    change-log: (string-utf8 256),
    file-size: uint
  }
)

;; File backup copies
(define-map file-backups
  { file-id: uint, backup-id: uint }
  {
    backup-ipfs-cid: (string-ascii 128),
    backup-timestamp: uint,
    backup-location: (string-ascii 64),
    is-verified: bool
  }
)

;; User storage quotas
(define-map user-quotas
  { user: principal }
  {
    used-storage: uint,
    max-storage: uint,
    file-count: uint,
    last-updated: uint
  }
)

;; File collections/folders
(define-map collections
  { collection-id: uint }
  {
    name: (string-ascii 128),
    owner: principal,
    description: (string-utf8 256),
    created-at: uint,
    is-public: bool
  }
)

;; Files in collections
(define-map collection-files
  { collection-id: uint, file-id: uint }
  { added-at: uint }
)

;; Existing maps (unchanged)
(define-map cid-to-file-id
  { ipfs-cid: (string-ascii 128) }
  { file-id: uint }
)

(define-map file-owners
  { file-id: uint }
  { owner: principal }
)

(define-map owner-files
  { owner: principal, file-id: uint }
  { exists: bool }
)

;; Private Functions
(define-private (is-file-owner (file-id uint) (user principal))
  (match (map-get? file-owners { file-id: file-id })
    owner-data (is-eq (get owner owner-data) user)
    false
  )
)

(define-private (file-exists (file-id uint))
  (is-some (map-get? files { file-id: file-id }))
)

(define-private (cid-exists (ipfs-cid (string-ascii 128)))
  (is-some (map-get? cid-to-file-id { ipfs-cid: ipfs-cid }))
)

(define-private (has-file-permission (file-id uint) (user principal) (required-permission (string-ascii 16)))
  (or 
    (is-file-owner file-id user)
    (match (map-get? file-permissions { file-id: file-id, user: user })
      permission-data 
      (and 
        (or 
          (is-eq (get permission-type permission-data) required-permission)
          (is-eq (get permission-type permission-data) "admin")
        )
        (match (get expires-at permission-data)
          expiry (< (unwrap-panic (get-block-info? time (- block-height u1))) expiry)
          true
        )
      )
      false
    )
  )
)

(define-private (update-user-quota (user principal) (file-size uint) (is-adding bool))
  (let
    (
      (current-quota (default-to 
        { used-storage: u0, max-storage: u10737418240, file-count: u0, last-updated: u0 }
        (map-get? user-quotas { user: user })
      ))
      (new-used-storage (if is-adding
        (+ (get used-storage current-quota) file-size)
        (if (>= (get used-storage current-quota) file-size)
          (- (get used-storage current-quota) file-size)
          u0
        )
      ))
      (new-file-count (if is-adding
        (+ (get file-count current-quota) u1)
        (if (> (get file-count current-quota) u0)
          (- (get file-count current-quota) u1)
          u0
        )
      ))
    )
    (map-set user-quotas
      { user: user }
      (merge current-quota {
        used-storage: new-used-storage,
        file-count: new-file-count,
        last-updated: (unwrap-panic (get-block-info? time (- block-height u1)))
      })
    )
  )
)

(define-private (log-file-access (file-id uint) (user principal) (action (string-ascii 32)))
  (let
    (
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )
    (map-set access-logs
      { file-id: file-id, user: user, timestamp: current-time }
      {
        action: action,
        ip-hash: none,
        user-agent-hash: none
      }
    )
  )
)

;; Enhanced Read-only Functions
(define-read-only (get-file-metadata (file-id uint))
  (map-get? files { file-id: file-id })
)

(define-read-only (get-file-by-cid (ipfs-cid (string-ascii 128)))
  (match (map-get? cid-to-file-id { ipfs-cid: ipfs-cid })
    cid-data (map-get? files { file-id: (get file-id cid-data) })
    none
  )
)

(define-read-only (get-file-owner (file-id uint))
  (map-get? file-owners { file-id: file-id })
)

(define-read-only (get-current-file-id)
  (var-get next-file-id)
)

(define-read-only (is-owner (file-id uint) (user principal))
  (is-file-owner file-id user)
)

(define-read-only (user-owns-file (owner principal) (file-id uint))
  (default-to false (get exists (map-get? owner-files { owner: owner, file-id: file-id })))
)

(define-read-only (get-user-quota (user principal))
  (map-get? user-quotas { user: user })
)

(define-read-only (get-file-permissions (file-id uint) (user principal))
  (map-get? file-permissions { file-id: file-id, user: user })
)

(define-read-only (get-file-versions (file-id uint) (version uint))
  (map-get? file-versions { file-id: file-id, version: version })
)

(define-read-only (get-category (category-id uint))
  (map-get? categories { category-id: category-id })
)

(define-read-only (get-collection (collection-id uint))
  (map-get? collections { collection-id: collection-id })
)

(define-read-only (is-contract-paused)
  (var-get contract-paused)
)

(define-read-only (get-registration-fee)
  (var-get registration-fee)
)

(define-read-only (can-access-file (file-id uint) (user principal))
  (match (map-get? files { file-id: file-id })
    file-data
    (or
      (is-eq (get access-level file-data) "public")
      (is-file-owner file-id user)
      (has-file-permission file-id user "read")
    )
    false
  )
)

;; Enhanced Public Functions
;; Create file category
(define-public (create-category (name (string-ascii 64)) (description (string-utf8 256)))
  (let
    (
      (category-id (var-get next-category-id))
    )
    (asserts! (> (len name) u0) err-empty-data)
    (asserts! (not (var-get contract-paused)) err-owner-only)
    
    (map-set categories
      { category-id: category-id }
      {
        name: name,
        description: description,
        created-by: tx-sender,
        file-count: u0
      }
    )
    
    (var-set next-category-id (+ category-id u1))
    
    (print {
      event: "category-created",
      category-id: category-id,
      name: name,
      creator: tx-sender
    })
    
    (ok category-id)
  )
)

;; Enhanced file registration with payment and advanced features
(define-public (register-file-advanced
  (filename (string-ascii 256))
  (ipfs-cid (string-ascii 128))
  (file-size uint)
  (file-type (string-ascii 64))
  (description (string-utf8 512))
  (access-level (string-ascii 16))
  (category-id uint)
  (tags (list 10 (string-ascii 32)))
  (checksum (string-ascii 64))
  (license (string-ascii 128))
  (expiry-time (optional uint))
)
  (let
    (
      (file-id (var-get next-file-id))
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
      (user-quota (default-to 
        { used-storage: u0, max-storage: u10737418240, file-count: u0, last-updated: u0 }
        (map-get? user-quotas { user: tx-sender })
      ))
    )
    ;; Contract state checks
    (asserts! (not (var-get contract-paused)) err-owner-only)
    
    ;; Validate inputs
    (asserts! (> (len filename) u0) err-empty-data)
    (asserts! (> (len ipfs-cid) u0) err-empty-data)
    (asserts! (not (cid-exists ipfs-cid)) err-already-exists)
    (asserts! (<= file-size (var-get max-file-size)) err-quota-exceeded)
    (asserts! (or (is-eq access-level "public") (is-eq access-level "private") (is-eq access-level "restricted")) err-invalid-access-level)
    
    ;; Check user quotas
    (asserts! (< (get file-count user-quota) (var-get max-files-per-user)) err-quota-exceeded)
    (asserts! (<= (+ (get used-storage user-quota) file-size) (get max-storage user-quota)) err-quota-exceeded)
    
    ;; Validate category exists
    (asserts! (is-some (map-get? categories { category-id: category-id })) err-invalid-category)
    
    ;; Validate expiry time
    (match expiry-time
      expiry (asserts! (> expiry current-time) err-invalid-time)
      true
    )
    
    ;; Process payment (registration fee)
    (if (> (var-get registration-fee) u0)
      (unwrap! (stx-transfer? (var-get registration-fee) tx-sender contract-owner) err-insufficient-payment)
      true
    )
    
    ;; Store enhanced file metadata
    (map-set files
      { file-id: file-id }
      {
        filename: filename,
        ipfs-cid: ipfs-cid,
        uploader: tx-sender,
        timestamp: current-time,
        updated-at: current-time,
        file-size: file-size,
        file-type: file-type,
        description: description,
        access-level: access-level,
        download-count: u0,
        is-locked: false,
        expiry-time: expiry-time,
        category-id: category-id,
        tags: tags,
        checksum: checksum,
        encryption-key-hash: none,
        version: u1,
        license: license
      }
    )
    
    ;; Store initial version
    (map-set file-versions
      { file-id: file-id, version: u1 }
      {
        ipfs-cid: ipfs-cid,
        updated-by: tx-sender,
        timestamp: current-time,
        change-log: u"Initial version",
        file-size: file-size
      }
    )
    
    ;; Update existing mappings
    (map-set cid-to-file-id { ipfs-cid: ipfs-cid } { file-id: file-id })
    (map-set file-owners { file-id: file-id } { owner: tx-sender })
    (map-set owner-files { owner: tx-sender, file-id: file-id } { exists: true })
    
    ;; Update user quota
    (update-user-quota tx-sender file-size true)
    
    ;; Update category file count
    (match (map-get? categories { category-id: category-id })
      category-data
      (map-set categories
        { category-id: category-id }
        (merge category-data { file-count: (+ (get file-count category-data) u1) })
      )
      false
    )
    
    ;; Increment file ID counter
    (var-set next-file-id (+ file-id u1))
    
    ;; Log access
    (log-file-access file-id tx-sender "register")
    
    ;; Emit event
    (print {
      event: "file-registered-advanced",
      file-id: file-id,
      filename: filename,
      ipfs-cid: ipfs-cid,
      uploader: tx-sender,
      access-level: access-level,
      category-id: category-id,
      timestamp: current-time
    })
    
    (ok file-id)
  )
)

;; Grant file permissions
(define-public (grant-file-permission 
  (file-id uint) 
  (user principal) 
  (permission-type (string-ascii 16))
  (expires-at (optional uint))
)
  (let
    (
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )
    ;; Check authorization
    (asserts! (has-file-permission file-id tx-sender "admin") err-unauthorized)
    (asserts! (file-exists file-id) err-not-found)
    
    ;; Validate permission type
    (asserts! (or 
      (is-eq permission-type "read") 
      (is-eq permission-type "write") 
      (is-eq permission-type "admin")
    ) err-invalid-access-level)
    
    ;; Validate expiry time
    (match expires-at
      expiry (asserts! (> expiry current-time) err-invalid-time)
      true
    )
    
    ;; Grant permission
    (map-set file-permissions
      { file-id: file-id, user: user }
      {
        permission-type: permission-type,
        granted-by: tx-sender,
        granted-at: current-time,
        expires-at: expires-at
      }
    )
    
    ;; Emit event
    (print {
      event: "permission-granted",
      file-id: file-id,
      user: user,
      permission-type: permission-type,
      granted-by: tx-sender,
      expires-at: expires-at
    })
    
    (ok true)
  )
)

;; Revoke file permissions
(define-public (revoke-file-permission (file-id uint) (user principal))
  (begin
    ;; Check authorization
    (asserts! (has-file-permission file-id tx-sender "admin") err-unauthorized)
    (asserts! (file-exists file-id) err-not-found)
    
    ;; Revoke permission
    (map-delete file-permissions { file-id: file-id, user: user })
    
    ;; Emit event
    (print {
      event: "permission-revoked",
      file-id: file-id,
      user: user,
      revoked-by: tx-sender
    })
    
    (ok true)
  )
)

;; Download file (increments download count)
(define-public (download-file (file-id uint))
  (let
    (
      (file-data (unwrap! (map-get? files { file-id: file-id }) err-not-found))
    )
    ;; Check access permissions
    (asserts! (can-access-file file-id tx-sender) err-unauthorized)
    
    ;; Check if file is locked
    (asserts! (not (get is-locked file-data)) err-file-locked)
    
    ;; Check expiry
    (match (get expiry-time file-data)
      expiry (asserts! (< (unwrap-panic (get-block-info? time (- block-height u1))) expiry) err-invalid-time)
      true
    )
    
    ;; Increment download count
    (map-set files
      { file-id: file-id }
      (merge file-data { download-count: (+ (get download-count file-data) u1) })
    )
    
    ;; Log access
    (log-file-access file-id tx-sender "download")
    
    ;; Emit event
    (print {
      event: "file-downloaded",
      file-id: file-id,
      downloader: tx-sender,
      download-count: (+ (get download-count file-data) u1)
    })
    
    (ok (get ipfs-cid file-data))
  )
)

;; Lock/unlock file
(define-public (toggle-file-lock (file-id uint))
  (let
    (
      (file-data (unwrap! (map-get? files { file-id: file-id }) err-not-found))
      (new-lock-status (not (get is-locked file-data)))
    )
    ;; Check authorization
    (asserts! (is-file-owner file-id tx-sender) err-unauthorized)
    
    ;; Toggle lock status
    (map-set files
      { file-id: file-id }
      (merge file-data { is-locked: new-lock-status })
    )
    
    ;; Emit event
    (print {
      event: "file-lock-toggled",
      file-id: file-id,
      is-locked: new-lock-status,
      changed-by: tx-sender
    })
    
    (ok new-lock-status)
  )
)

;; Create file backup
(define-public (create-file-backup 
  (file-id uint) 
  (backup-ipfs-cid (string-ascii 128))
  (backup-location (string-ascii 64))
)
  (let
    (
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
      (backup-id u1) ;; Simplified - in real implementation, generate unique backup ID
    )
    ;; Check authorization
    (asserts! (is-file-owner file-id tx-sender) err-unauthorized)
    (asserts! (file-exists file-id) err-not-found)
    
    ;; Create backup record
    (map-set file-backups
      { file-id: file-id, backup-id: backup-id }
      {
        backup-ipfs-cid: backup-ipfs-cid,
        backup-timestamp: current-time,
        backup-location: backup-location,
        is-verified: false
      }
    )
    
    ;; Emit event
    (print {
      event: "backup-created",
      file-id: file-id,
      backup-id: backup-id,
      backup-ipfs-cid: backup-ipfs-cid,
      location: backup-location
    })
    
    (ok backup-id)
  )
)

;; Admin functions
(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set contract-paused true)
    (print { event: "contract-paused", by: tx-sender })
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set contract-paused false)
    (print { event: "contract-unpaused", by: tx-sender })
    (ok true)
  )
)

(define-public (set-registration-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set registration-fee new-fee)
    (print { event: "registration-fee-updated", new-fee: new-fee })
    (ok true)
  )
)

(define-public (set-max-file-size (new-max-size uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set max-file-size new-max-size)
    (print { event: "max-file-size-updated", new-max-size: new-max-size })
    (ok true)
  )
)

;; Existing functions (updated for compatibility)
(define-public (register-file 
  (filename (string-ascii 256))
  (ipfs-cid (string-ascii 128))
  (file-size uint)
  (file-type (string-ascii 64))
)
  (register-file-advanced 
    filename 
    ipfs-cid 
    file-size 
    file-type 
    u"" 
    "public" 
    u1 
    (list) 
    "" 
    "MIT" 
    none
  )
)

(define-public (update-file-metadata
  (file-id uint)
  (filename (string-ascii 256))
  (file-size uint)
  (file-type (string-ascii 64))
)
  (let
    (
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
      (existing-file (unwrap! (map-get? files { file-id: file-id }) err-not-found))
    )
    ;; Check authorization
    (asserts! (has-file-permission file-id tx-sender "write") err-unauthorized)
    (asserts! (not (get is-locked existing-file)) err-file-locked)
    
    ;; Validate inputs
    (asserts! (> (len filename) u0) err-empty-data)
    
    ;; Update file metadata
    (map-set files
      { file-id: file-id }
      (merge existing-file {
        filename: filename,
        updated-at: current-time,
        file-size: file-size,
        file-type: file-type,
        version: (+ (get version existing-file) u1)
      })
    )
    
    ;; Log access
    (log-file-access file-id tx-sender "update")
    
    ;; Emit event
    (print {
      event: "file-updated",
      file-id: file-id,
      filename: filename,
      updater: tx-sender,
      updated-at: current-time,
      new-version: (+ (get version existing-file) u1)
    })
    
    (ok true)
  )
)

;; Transfer ownership (enhanced)
(define-public (transfer-ownership (file-id uint) (new-owner principal))
  (let
    (
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
      (file-data (unwrap! (map-get? files { file-id: file-id }) err-not-found))
    )
    ;; Check authorization
    (asserts! (is-file-owner file-id tx-sender) err-unauthorized)
    (asserts! (not (get is-locked file-data)) err-file-locked)
    
    ;; Update quota for both users
    (update-user-quota tx-sender (get file-size file-data) false)
    (update-user-quota new-owner (get file-size file-data) true)
    
    ;; Remove file from old owner's tracking
    (map-delete owner-files { owner: tx-sender, file-id: file-id })
    
    ;; Update file owner
    (map-set file-owners
      { file-id: file-id }
      { owner: new-owner }
    )
    
    ;; Add file to new owner's tracking
    (map-set owner-files
      { owner: new-owner, file-id: file-id }
      { exists: true }
    )
    
    ;; Log access
    (log-file-access file-id tx-sender "transfer")
    
    ;; Emit event
    (print {
      event: "ownership-transferred",
      file-id: file-id,
      previous-owner: tx-sender,
      new-owner: new-owner,
      timestamp: current-time
    })
    
    (ok true)
  )
)

;; Delete file (enhanced)
(define-public (delete-file (file-id uint))
  (let
    (
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
      (file-data (unwrap! (map-get? files { file-id: file-id }) err-not-found))
    )
    ;; Check authorization
    (asserts! (is-file-owner file-id tx-sender) err-unauthorized)
    (asserts! (not (get is-locked file-data)) err-file-locked)
    
    ;; Update user quota
    (update-user-quota tx-sender (get file-size file-data) false)
    
    ;; Update category file count
    (match (map-get? categories { category-id: (get category-id file-data) })
      category-data
      (map-set categories
        { category-id: (get category-id file-data) }
        (merge category-data { file-count: (- (get file-count category-data) u1) })
      )
      false
    )
    
    ;; Remove all mappings
    (map-delete files { file-id: file-id })
    (map-delete cid-to-file-id { ipfs-cid: (get ipfs-cid file-data) })
    (map-delete file-owners { file-id: file-id })
    (map-delete owner-files { owner: tx-sender, file-id: file-id })
    
    ;; Emit event
    (print {
      event: "file-deleted",
      file-id: file-id,
      ipfs-cid: (get ipfs-cid file-data),
      owner: tx-sender,
      timestamp: current-time
    })
    
    (ok true)
  )
)