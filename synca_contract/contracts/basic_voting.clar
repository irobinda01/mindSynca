;; Advanced Voting Contract
;; A comprehensive voting system with multiple features and security measures

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-VOTED (err u101))
(define-constant ERR-CANDIDATE-NOT-FOUND (err u102))
(define-constant ERR-CANDIDATE-ALREADY-EXISTS (err u103))
(define-constant ERR-VOTING-NOT-STARTED (err u104))
(define-constant ERR-VOTING-ENDED (err u105))
(define-constant ERR-INSUFFICIENT-TOKENS (err u106))
(define-constant ERR-INVALID-TIME (err u107))
(define-constant ERR-CANDIDATE-INACTIVE (err u108))
(define-constant ERR-VOTER-NOT-ELIGIBLE (err u109))
(define-constant ERR-DELEGATE-NOT-FOUND (err u110))
(define-constant ERR-CANNOT-DELEGATE-TO-SELF (err u111))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u112))
(define-constant ERR-VOTING-POWER-REQUIRED (err u113))
(define-constant ERR-ALREADY-DELEGATED (err u114))

;; Contract constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MIN-VOTING-POWER u100) ;; Minimum tokens required to vote
(define-constant PROPOSAL-THRESHOLD u1000) ;; Minimum tokens to create proposal

;; Data variables
(define-data-var voting-active bool false)
(define-data-var total-candidates uint u0)
(define-data-var total-voters uint u0)
(define-data-var total-votes-cast uint u0)
(define-data-var voting-start-time uint u0)
(define-data-var voting-end-time uint u0)
(define-data-var election-title (string-ascii 100) "")
(define-data-var election-description (string-ascii 500) "")
(define-data-var weighted-voting-enabled bool false)
(define-data-var delegation-enabled bool false)
(define-data-var total-proposals uint u0)
(define-data-var emergency-stop bool false)

;; Data maps
;; Enhanced candidate information
(define-map candidates 
    { candidate-id: uint } 
    { 
        name: (string-ascii 50),
        description: (string-ascii 200),
        votes: uint,
        weighted-votes: uint,
        active: bool,
        registration-time: uint,
        category: (string-ascii 30)
    }
)

;; Enhanced voter information
(define-map voter-status 
    { voter: principal } 
    { 
        has-voted: bool,
        vote-time: uint,
        candidate-voted: uint,
        voting-power: uint,
        is-eligible: bool,
        registration-time: uint
    }
)

;; Candidate name to ID mapping
(define-map candidate-name-to-id 
    { name: (string-ascii 50) } 
    { candidate-id: uint }
)

;; Vote delegation system
(define-map vote-delegation
    { delegator: principal }
    { 
        delegate: principal,
        is-active: bool,
        delegation-time: uint
    }
)

;; Voting power tracking
(define-map voting-power
    { voter: principal }
    { 
        own-power: uint,
        delegated-power: uint,
        total-power: uint
    }
)

;; Proposal system
(define-map proposals
    { proposal-id: uint }
    {
        title: (string-ascii 100),
        description: (string-ascii 500),
        proposer: principal,
        votes-for: uint,
        votes-against: uint,
        active: bool,
        creation-time: uint,
        voting-deadline: uint
    }
)

;; Proposal votes
(define-map proposal-votes
    { proposal-id: uint, voter: principal }
    { 
        vote: bool, ;; true for yes, false for no
        voting-power: uint,
        vote-time: uint
    }
)

;; Election results and statistics
(define-map election-stats
    { election-id: uint }
    {
        total-votes: uint,
        total-voters: uint,
        winner-id: uint,
        winner-votes: uint,
        participation-rate: uint,
        completion-time: uint
    }
)

;; Admin and moderator roles
(define-map admin-roles
    { admin: principal }
    { 
        role: (string-ascii 20), ;; "admin", "moderator", "observer"
        granted-by: principal,
        granted-time: uint,
        active: bool
    }
)

;; Vote history for transparency
(define-map vote-history
    { voter: principal, election-round: uint }
    {
        candidate-id: uint,
        vote-time: uint,
        voting-power-used: uint
    }
)

;; Public functions

;; === ELECTION MANAGEMENT ===

;; Initialize election with details
(define-public (initialize-election (title (string-ascii 100)) (description (string-ascii 500)) (duration uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (not (var-get voting-active)) ERR-VOTING-NOT-STARTED)
        (var-set election-title title)
        (var-set election-description description)
        (var-set voting-end-time (+ burn-block-height duration))
        (ok true)
    )
)

;; Start voting with time controls
(define-public (start-voting)
    (begin
        (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-admin tx-sender)) ERR-NOT-AUTHORIZED)
        (asserts! (not (var-get emergency-stop)) ERR-VOTING-ENDED)
        (var-set voting-active true)
        (var-set voting-start-time burn-block-height)
        (ok true)
    )
)

;; End voting
(define-public (end-voting)
    (begin
        (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-admin tx-sender)) ERR-NOT-AUTHORIZED)
        (var-set voting-active false)
        (unwrap-panic (calculate-final-results))
        (ok true)
    )
)

;; Emergency stop
(define-public (emergency-stop-voting)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set emergency-stop true)
        (var-set voting-active false)
        (ok true)
    )
)

;; === CANDIDATE MANAGEMENT ===

;; Register candidate with enhanced details
(define-public (register-candidate (candidate-name (string-ascii 50)) (description (string-ascii 200)) (category (string-ascii 30)))
    (let
        (
            (current-total (var-get total-candidates))
            (new-candidate-id (+ current-total u1))
        )
        (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-admin tx-sender)) ERR-NOT-AUTHORIZED)
        (asserts! (is-none (map-get? candidate-name-to-id { name: candidate-name })) ERR-CANDIDATE-ALREADY-EXISTS)
        
        (map-set candidates 
            { candidate-id: new-candidate-id }
            { 
                name: candidate-name,
                description: description,
                votes: u0,
                weighted-votes: u0,
                active: true,
                registration-time: burn-block-height,
                category: category
            }
        )
        (map-set candidate-name-to-id 
            { name: candidate-name }
            { candidate-id: new-candidate-id }
        )
        
        (var-set total-candidates new-candidate-id)
        (ok new-candidate-id)
    )
)

;; Deactivate candidate
(define-public (deactivate-candidate (candidate-id uint))
    (let
        (
            (candidate-info (unwrap! (map-get? candidates { candidate-id: candidate-id }) ERR-CANDIDATE-NOT-FOUND))
        )
        (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-admin tx-sender)) ERR-NOT-AUTHORIZED)
        (map-set candidates 
            { candidate-id: candidate-id }
            (merge candidate-info { active: false })
        )
        (ok true)
    )
)

;; === VOTER MANAGEMENT ===

;; Register voter with voting power
(define-public (register-voter (voter principal) (voting-power-amount uint))
    (begin
        (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-admin tx-sender)) ERR-NOT-AUTHORIZED)
        (map-set voter-status 
            { voter: voter }
            { 
                has-voted: false,
                vote-time: u0,
                candidate-voted: u0,
                voting-power: voting-power-amount,
                is-eligible: true,
                registration-time: burn-block-height
            }
        )
        (map-set voting-power
            { voter: voter }
            {
                own-power: voting-power-amount,
                delegated-power: u0,
                total-power: voting-power-amount
            }
        )
        (var-set total-voters (+ (var-get total-voters) u1))
        (ok true)
    )
)

;; Update voter eligibility
(define-public (update-voter-eligibility (voter principal) (eligible bool))
    (let
        (
            (voter-info (unwrap! (map-get? voter-status { voter: voter }) ERR-CANDIDATE-NOT-FOUND))
        )
        (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-admin tx-sender)) ERR-NOT-AUTHORIZED)
        (map-set voter-status 
            { voter: voter }
            (merge voter-info { is-eligible: eligible })
        )
        (ok true)
    )
)

;; === VOTING FUNCTIONS ===

;; Enhanced voting with weighted support
(define-public (vote-for-candidate (candidate-id uint))
    (let
        (
            (voter tx-sender)
            (candidate-info (unwrap! (map-get? candidates { candidate-id: candidate-id }) ERR-CANDIDATE-NOT-FOUND))
            (voter-info (default-to { has-voted: false, vote-time: u0, candidate-voted: u0, voting-power: u0, is-eligible: false, registration-time: u0 } 
                        (map-get? voter-status { voter: voter })))
            (voter-power (default-to { own-power: u0, delegated-power: u0, total-power: u0 } 
                         (map-get? voting-power { voter: voter })))
        )
        (asserts! (var-get voting-active) ERR-VOTING-NOT-STARTED)
        (asserts! (not (var-get emergency-stop)) ERR-VOTING-ENDED)
        (asserts! (not (get has-voted voter-info)) ERR-ALREADY-VOTED)
        (asserts! (get active candidate-info) ERR-CANDIDATE-INACTIVE)
        (asserts! (get is-eligible voter-info) ERR-VOTER-NOT-ELIGIBLE)
        (asserts! (>= (get total-power voter-power) MIN-VOTING-POWER) ERR-VOTING-POWER-REQUIRED)
        
        (let
            (
                (vote-weight (if (var-get weighted-voting-enabled) (get total-power voter-power) u1))
            )
            ;; Update candidate votes
            (map-set candidates 
                { candidate-id: candidate-id }
                (merge candidate-info { 
                    votes: (+ (get votes candidate-info) u1),
                    weighted-votes: (+ (get weighted-votes candidate-info) vote-weight)
                })
            )
            
            ;; Update voter status
            (map-set voter-status 
                { voter: voter }
                (merge voter-info { 
                    has-voted: true,
                    vote-time: burn-block-height,
                    candidate-voted: candidate-id
                })
            )
            
            ;; Record vote history
            (map-set vote-history
                { voter: voter, election-round: u1 }
                {
                    candidate-id: candidate-id,
                    vote-time: burn-block-height,
                    voting-power-used: vote-weight
                }
            )
            
            (var-set total-votes-cast (+ (var-get total-votes-cast) u1))
            (ok true)
        )
    )
)

;; === DELEGATION SYSTEM ===

;; Delegate voting power
(define-public (delegate-vote (delegate principal))
    (let
        (
            (delegator tx-sender)
            (delegator-power (unwrap! (map-get? voting-power { voter: delegator }) ERR-CANDIDATE-NOT-FOUND))
            (delegate-power (default-to { own-power: u0, delegated-power: u0, total-power: u0 } 
                           (map-get? voting-power { voter: delegate })))
        )
        (asserts! (var-get delegation-enabled) ERR-NOT-AUTHORIZED)
        (asserts! (not (is-eq delegator delegate)) ERR-CANNOT-DELEGATE-TO-SELF)
        (asserts! (is-none (map-get? vote-delegation { delegator: delegator })) ERR-ALREADY-DELEGATED)
        
        ;; Create delegation
        (map-set vote-delegation
            { delegator: delegator }
            {
                delegate: delegate,
                is-active: true,
                delegation-time: burn-block-height
            }
        )
        
        ;; Update voting powers
        (map-set voting-power
            { voter: delegate }
            (merge delegate-power { 
                delegated-power: (+ (get delegated-power delegate-power) (get own-power delegator-power)),
                total-power: (+ (get total-power delegate-power) (get own-power delegator-power))
            })
        )
        
        (map-set voting-power
            { voter: delegator }
            (merge delegator-power { total-power: u0 })
        )
        
        (ok true)
    )
)

;; Revoke delegation
(define-public (revoke-delegation)
    (let
        (
            (delegator tx-sender)
            (delegation-info (unwrap! (map-get? vote-delegation { delegator: delegator }) ERR-DELEGATE-NOT-FOUND))
            (delegate (get delegate delegation-info))
            (delegator-power (unwrap! (map-get? voting-power { voter: delegator }) ERR-CANDIDATE-NOT-FOUND))
            (delegate-power (unwrap! (map-get? voting-power { voter: delegate }) ERR-DELEGATE-NOT-FOUND))
        )
        ;; Remove delegation
        (map-delete vote-delegation { delegator: delegator })
        
        ;; Restore voting powers
        (map-set voting-power
            { voter: delegate }
            (merge delegate-power { 
                delegated-power: (- (get delegated-power delegate-power) (get own-power delegator-power)),
                total-power: (- (get total-power delegate-power) (get own-power delegator-power))
            })
        )
        
        (map-set voting-power
            { voter: delegator }
            (merge delegator-power { total-power: (get own-power delegator-power) })
        )
        
        (ok true)
    )
)

;; === PROPOSAL SYSTEM ===

;; Create proposal
(define-public (create-proposal (title (string-ascii 100)) (description (string-ascii 500)) (voting-period uint))
    (let
        (
            (proposer tx-sender)
            (proposer-power (default-to { own-power: u0, delegated-power: u0, total-power: u0 } 
                           (map-get? voting-power { voter: proposer })))
            (proposal-id (+ (var-get total-proposals) u1))
        )
        (asserts! (>= (get total-power proposer-power) PROPOSAL-THRESHOLD) ERR-VOTING-POWER-REQUIRED)
        
        (map-set proposals
            { proposal-id: proposal-id }
            {
                title: title,
                description: description,
                proposer: proposer,
                votes-for: u0,
                votes-against: u0,
                active: true,
                creation-time: burn-block-height,
                voting-deadline: (+ burn-block-height voting-period)
            }
        )
        
        (var-set total-proposals proposal-id)
        (ok proposal-id)
    )
)

;; Vote on proposal
(define-public (vote-on-proposal (proposal-id uint) (vote-yes bool))
    (let
        (
            (voter tx-sender)
            (proposal-info (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
            (voter-power (unwrap! (map-get? voting-power { voter: voter }) ERR-VOTER-NOT-ELIGIBLE))
            (existing-vote (map-get? proposal-votes { proposal-id: proposal-id, voter: voter }))
        )
        (asserts! (get active proposal-info) ERR-PROPOSAL-NOT-FOUND)
        (asserts! (< burn-block-height (get voting-deadline proposal-info)) ERR-VOTING-ENDED)
        (asserts! (is-none existing-vote) ERR-ALREADY-VOTED)
        (asserts! (>= (get total-power voter-power) MIN-VOTING-POWER) ERR-VOTING-POWER-REQUIRED)
        
        ;; Record vote
        (map-set proposal-votes
            { proposal-id: proposal-id, voter: voter }
            {
                vote: vote-yes,
                voting-power: (get total-power voter-power),
                vote-time: burn-block-height
            }
        )
        
        ;; Update proposal counts
        (if vote-yes
            (map-set proposals
                { proposal-id: proposal-id }
                (merge proposal-info { votes-for: (+ (get votes-for proposal-info) (get total-power voter-power)) })
            )
            (map-set proposals
                { proposal-id: proposal-id }
                (merge proposal-info { votes-against: (+ (get votes-against proposal-info) (get total-power voter-power)) })
            )
        )
        
        (ok true)
    )
)

;; === ADMIN FUNCTIONS ===

;; Grant admin role
(define-public (grant-admin-role (admin principal) (role (string-ascii 20)))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (map-set admin-roles
            { admin: admin }
            {
                role: role,
                granted-by: tx-sender,
                granted-time: burn-block-height,
                active: true
            }
        )
        (ok true)
    )
)

;; Enable/disable features
(define-public (toggle-weighted-voting)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set weighted-voting-enabled (not (var-get weighted-voting-enabled)))
        (ok (var-get weighted-voting-enabled))
    )
)

(define-public (toggle-delegation)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set delegation-enabled (not (var-get delegation-enabled)))
        (ok (var-get delegation-enabled))
    )
)

;; === UTILITY FUNCTIONS ===

;; Calculate final results
(define-private (calculate-final-results)
    (let
        (
            (total-votes (var-get total-votes-cast))
            (total-registered (var-get total-voters))
            (participation-rate (if (> total-registered u0) (/ (* total-votes u100) total-registered) u0))
        )
        (map-set election-stats
            { election-id: u1 }
            {
                total-votes: total-votes,
                total-voters: total-registered,
                winner-id: u0, ;; Would need additional logic to determine winner
                winner-votes: u0,
                participation-rate: participation-rate,
                completion-time: burn-block-height
            }
        )
        (ok true)
    )
)

;; === READ-ONLY FUNCTIONS ===

;; Check if user is admin
(define-read-only (is-admin (user principal))
    (match (map-get? admin-roles { admin: user })
        admin-info (get active admin-info)
        false
    )
)

;; Get candidate with full details
(define-read-only (get-candidate-details (candidate-id uint))
    (map-get? candidates { candidate-id: candidate-id })
)

;; Get voter information
(define-read-only (get-voter-info (voter principal))
    (map-get? voter-status { voter: voter })
)

;; Get voting power
(define-read-only (get-voting-power (voter principal))
    (map-get? voting-power { voter: voter })
)

;; Get delegation info
(define-read-only (get-delegation-info (delegator principal))
    (map-get? vote-delegation { delegator: delegator })
)

;; Get proposal details
(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals { proposal-id: proposal-id })
)

;; Get election statistics
(define-read-only (get-election-stats)
    {
        title: (var-get election-title),
        description: (var-get election-description),
        total-candidates: (var-get total-candidates),
        total-voters: (var-get total-voters),
        total-votes: (var-get total-votes-cast),
        voting-active: (var-get voting-active),
        weighted-voting: (var-get weighted-voting-enabled),
        delegation-enabled: (var-get delegation-enabled),
        emergency-stop: (var-get emergency-stop)
    }
)

;; Get vote history
(define-read-only (get-vote-history (voter principal) (election-round uint))
    (map-get? vote-history { voter: voter, election-round: election-round })
)

;; Check voting time validity
(define-read-only (is-voting-time-valid)
    (and 
        (var-get voting-active)
        (not (var-get emergency-stop))
        (or 
            (is-eq (var-get voting-end-time) u0)
            (< burn-block-height (var-get voting-end-time))
        )
    )
)

;; Get leaderboard (top candidates by votes)
(define-read-only (get-candidate-ranking (candidate-id uint))
    (match (map-get? candidates { candidate-id: candidate-id })
        info (some {
            candidate-id: candidate-id,
            name: (get name info),
            votes: (get votes info),
            weighted-votes: (get weighted-votes info),
            active: (get active info)
        })
        none
    )
)