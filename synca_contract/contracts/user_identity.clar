;; Meta-Transaction Wallet Contract
;; Allows third-party relayers to submit transactions on behalf of users

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INVALID_SIGNATURE (err u101))
(define-constant ERR_NONCE_USED (err u102))
(define-constant ERR_EXPIRED (err u103))
(define-constant ERR_INSUFFICIENT_BALANCE (err u104))

;; Data Variables
(define-data-var contract-nonce uint u0)

;; Data Maps
;; Track nonces for each user to prevent replay attacks
(define-map user-nonces principal uint)

;; Track wallet balances
(define-map wallet-balances principal uint)

;; Store authorized relayers
(define-map authorized-relayers principal bool)

;; Helper Functions

;; Get user's current nonce
(define-read-only (get-user-nonce (user principal))
  (default-to u0 (map-get? user-nonces user))
)

;; Get wallet balance
(define-read-only (get-wallet-balance (user principal))
  (default-to u0 (map-get? wallet-balances user))
)

;; Check if relayer is authorized
(define-read-only (is-authorized-relayer (relayer principal))
  (default-to false (map-get? authorized-relayers relayer))
)

;; Increment user nonce
(define-private (increment-user-nonce (user principal))
  (let ((current-nonce (get-user-nonce user)))
    (map-set user-nonces user (+ current-nonce u1))
    (+ current-nonce u1)
  )
)

;; Create message hash for signature verification
(define-read-only (create-message-hash 
  (from principal)
  (to principal) 
  (amount uint)
  (nonce uint)
  (expiry uint))
  (sha256 (concat
    (concat
      (concat (unwrap-panic (to-consensus-buff? from)) (unwrap-panic (to-consensus-buff? to)))
      (concat (unwrap-panic (to-consensus-buff? amount)) (unwrap-panic (to-consensus-buff? nonce)))
    )
    (unwrap-panic (to-consensus-buff? expiry))
  ))
)

;; Admin Functions

;; Add authorized relayer
(define-public (add-relayer (relayer principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-set authorized-relayers relayer true)
    (ok true)
  )
)

;; Remove authorized relayer
(define-public (remove-relayer (relayer principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-delete authorized-relayers relayer)
    (ok true)
  )
)

;; Wallet Functions

;; Deposit STX to wallet
(define-public (deposit (amount uint))
  (let ((current-balance (get-wallet-balance tx-sender)))
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set wallet-balances tx-sender (+ current-balance amount))
    (ok amount)
  )
)

;; Withdraw STX from wallet
(define-public (withdraw (amount uint))
  (let ((current-balance (get-wallet-balance tx-sender)))
    (asserts! (>= current-balance amount) ERR_INSUFFICIENT_BALANCE)
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    (map-set wallet-balances tx-sender (- current-balance amount))
    (ok amount)
  )
)

;; Meta-Transaction Functions

;; Execute meta-transaction (called by relayer)
(define-public (execute-meta-transaction
  (from principal)
  (to principal)
  (amount uint)
  (nonce uint)
  (expiry uint)
  (signature (buff 65)))
  (let (
    (current-nonce (get-user-nonce from))
    (current-balance (get-wallet-balance from))
    (message-hash (create-message-hash from to amount nonce expiry))
  )
    ;; Verify relayer is authorized
    (asserts! (is-authorized-relayer tx-sender) ERR_NOT_AUTHORIZED)
    
    ;; Verify nonce hasn't been used
    (asserts! (is-eq nonce current-nonce) ERR_NONCE_USED)
    
    ;; Verify transaction hasn't expired
    (asserts! (> expiry block-height) ERR_EXPIRED)
    
    ;; Verify signature
    (let ((recovered-pubkey (unwrap! (secp256k1-recover? message-hash signature) ERR_INVALID_SIGNATURE)))
      (asserts! (is-eq from (unwrap! (principal-of? recovered-pubkey) ERR_INVALID_SIGNATURE)) ERR_INVALID_SIGNATURE)
    )
    
    ;; Verify sufficient balance
    (asserts! (>= current-balance amount) ERR_INSUFFICIENT_BALANCE)
    
    ;; Execute the transaction
    (map-set wallet-balances from (- current-balance amount))
    (map-set wallet-balances to (+ (get-wallet-balance to) amount))
    
    ;; Increment nonce to prevent replay
    (increment-user-nonce from)
    
    (ok {
      from: from,
      to: to,
      amount: amount,
      nonce: nonce,
      relayer: tx-sender
    })
  )
)

;; Batch meta-transactions
(define-public (batch-meta-transactions
  (transactions (list 10 {
    from: principal,
    to: principal,
    amount: uint,
    nonce: uint,
    expiry: uint,
    signature: (buff 65)
  })))
  (begin
    (asserts! (is-authorized-relayer tx-sender) ERR_NOT_AUTHORIZED)
    (ok (map execute-single-meta-tx transactions))
  )
)

;; Helper for batch processing
(define-private (execute-single-meta-tx (tx {
  from: principal,
  to: principal,
  amount: uint,
  nonce: uint,
  expiry: uint,
  signature: (buff 65)
}))
  (execute-meta-transaction
    (get from tx)
    (get to tx)
    (get amount tx)
    (get nonce tx)
    (get expiry tx)
    (get signature tx)
  )
)

;; Utility Functions

;; Get domain separator for EIP-712 style signatures
(define-read-only (get-domain-separator)
  (sha256 (concat
    (unwrap-panic (to-consensus-buff? "MetaTransactionWallet"))
    (unwrap-panic (to-consensus-buff? contract-caller))
  ))
)

;; Emergency Functions

;; Emergency pause (only owner)
(define-data-var contract-paused bool false)

(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set contract-paused true)
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set contract-paused false)
    (ok true)
  )
)

(define-read-only (is-contract-paused)
  (var-get contract-paused)
)

;; View Functions

;; Get user's meta-transaction data
(define-read-only (get-user-meta-data (user principal))
  {
    nonce: (get-user-nonce user),
    balance: (get-wallet-balance user)
  }
)

;; Verify meta-transaction without executing
(define-read-only (verify-meta-transaction
  (from principal)
  (to principal)
  (amount uint)
  (nonce uint)
  (expiry uint)
  (signature (buff 65)))
  (let (
    (current-nonce (get-user-nonce from))
    (current-balance (get-wallet-balance from))
    (message-hash (create-message-hash from to amount nonce expiry))
    (recovered-pubkey-result (secp256k1-recover? message-hash signature))
  )
    {
      valid-nonce: (is-eq nonce current-nonce),
      not-expired: (> expiry block-height),
      valid-signature: (is-ok recovered-pubkey-result),
      signature-match: (match recovered-pubkey-result
        ok-pubkey (match (principal-of? ok-pubkey)
          ok-principal (is-eq from ok-principal)
          err-val false
        )
        err-val false
      ),
      sufficient-balance: (>= current-balance amount)
    }
  )
)