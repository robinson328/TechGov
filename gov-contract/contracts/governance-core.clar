;; TechGov Protocol - Governance Core Contract
;; Main governance logic and protocol management

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_PROPOSAL (err u101))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u102))
(define-constant ERR_VOTING_PERIOD_ENDED (err u103))
(define-constant ERR_ALREADY_VOTED (err u104))
(define-constant ERR_INSUFFICIENT_STAKE (err u105))
(define-constant ERR_PROTOCOL_PAUSED (err u106))
(define-constant ERR_INVALID_PARAMETERS (err u107))
(define-constant ERR_PROPOSAL_NOT_READY (err u108))
(define-constant ERR_EXECUTION_FAILED (err u109))
(define-constant ERR_INVALID_VOTE_TYPE (err u110))

;; Protocol States
(define-constant PROTOCOL_ACTIVE u1)
(define-constant PROTOCOL_PAUSED u2)
(define-constant PROTOCOL_EMERGENCY u3)

;; Proposal States
(define-constant PROPOSAL_PENDING u1)
(define-constant PROPOSAL_ACTIVE u2)
(define-constant PROPOSAL_PASSED u3)
(define-constant PROPOSAL_REJECTED u4)
(define-constant PROPOSAL_EXECUTED u5)
(define-constant PROPOSAL_CANCELLED u6)

;; Proposal Types
(define-constant PROPOSAL_TYPE_PARAMETER u1)
(define-constant PROPOSAL_TYPE_CONTRACT_UPGRADE u2)
(define-constant PROPOSAL_TYPE_TREASURY u3)
(define-constant PROPOSAL_TYPE_EMERGENCY u4)
(define-constant PROPOSAL_TYPE_GENERAL u5)

;; Vote Types
(define-constant VOTE_FOR u1)
(define-constant VOTE_AGAINST u2)
(define-constant VOTE_ABSTAIN u3)

;; Data Variables
(define-data-var protocol-state uint PROTOCOL_ACTIVE)
(define-data-var min-proposal-stake uint u1000000000) ;; 1000 TGOV tokens
(define-data-var voting-period uint u1008) ;; ~1 week in blocks
(define-data-var execution-delay uint u144) ;; ~1 day in blocks
(define-data-var quorum-threshold uint u20) ;; 20% of total supply
(define-data-var approval-threshold uint u51) ;; 51% of votes
(define-data-var proposal-counter uint u0)
(define-data-var emergency-council (list 5 principal) (list))
(define-data-var treasury-balance uint u0)

;; Data Maps
(define-map proposals
  { proposal-id: uint }
  {
    proposer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposal-type: uint,
    target-contract: (optional principal),
    function-name: (optional (string-ascii 50)),
    parameters: (optional (buff 1024)),
    stake-amount: uint,
    start-block: uint,
    end-block: uint,
    votes-for: uint,
    votes-against: uint,
    votes-abstain: uint,
    total-voting-power: uint,
    state: uint,
    execution-block: (optional uint),
    executed: bool
  }
)

(define-map proposal-votes
  { proposal-id: uint, voter: principal }
  {
    vote-type: uint,
    vote-power: uint,
    block-height: uint,
    is-delegated: bool
  }
)

(define-map governance-parameters
  { parameter-name: (string-ascii 50) }
  { parameter-value: uint }
)

(define-map authorized-contracts
  { contract-address: principal }
  { is-authorized: bool, authorization-block: uint }
)

(define-map proposal-execution-queue
  { proposal-id: uint }
  { execution-ready-block: uint, queued: bool }
)

;; ;; Helper functions - DEFINED FIRST
;; (define-private (buff-to-uint-safe (input (buff 1024)))
;;   (let
;;     (
;;       ;; Take only the first 8 bytes for conversion (safer size)
;;       (truncated-buff (unwrap-panic (slice? input u0 u8)))
;;     )
;;     ;; Convert the truncated buffer to uint
;;   )
;; )

;; Read-only functions
(define-read-only (get-protocol-state)
  (var-get protocol-state)
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-proposal-vote (proposal-id uint) (voter principal))
  (map-get? proposal-votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-governance-parameter (parameter-name (string-ascii 50)))
  (map-get? governance-parameters { parameter-name: parameter-name })
)

(define-read-only (is-contract-authorized (contract-address principal))
  (default-to false 
    (get is-authorized 
      (map-get? authorized-contracts { contract-address: contract-address })))
)

(define-read-only (get-voting-power (stakeholder principal))
  (contract-call? .tech-gov-token get-effective-voting-power stakeholder)
)

(define-read-only (get-total-supply)
  (unwrap-panic (contract-call? .tech-gov-token get-total-supply))
)

(define-read-only (calculate-proposal-result (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal-data
    (let
      (
        (total-votes (+ (+ (get votes-for proposal-data) 
                          (get votes-against proposal-data))
                       (get votes-abstain proposal-data)))
        (total-supply (unwrap-panic (contract-call? .tech-gov-token get-total-supply)))
        (quorum-met (>= (* total-votes u100) (* total-supply (var-get quorum-threshold))))
        (approval-met (and (> total-votes u0)
                          (>= (* (get votes-for proposal-data) u100) 
                             (* total-votes (var-get approval-threshold)))))
        (voting-ended (> stacks-block-height (get end-block proposal-data)))
      )
      {
        quorum-met: quorum-met,
        approval-met: approval-met,
        voting-ended: voting-ended,
        can-execute: (and quorum-met approval-met voting-ended),
        total-votes: total-votes,
        participation-rate: (/ (* total-votes u100) total-supply),
        votes-for: (get votes-for proposal-data),
        votes-against: (get votes-against proposal-data),
        votes-abstain: (get votes-abstain proposal-data)
      }
    )
    { 
      quorum-met: false, 
      approval-met: false, 
      voting-ended: false,
      can-execute: false, 
      total-votes: u0, 
      participation-rate: u0,
      votes-for: u0,
      votes-against: u0,
      votes-abstain: u0
    }
  )
)

(define-read-only (get-proposal-status (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal-data
    (let
      (
        (current-block stacks-block-height)
        (start-block (get start-block proposal-data))
        (end-block (get end-block proposal-data))
        (state (get state proposal-data))
      )
      {
        state: state,
        is-active: (and (>= current-block start-block) 
                       (<= current-block end-block)
                       (is-eq state PROPOSAL_PENDING)),
        blocks-remaining: (if (<= current-block end-block) 
                           (- end-block current-block) 
                           u0),
        can-vote: (and (>= current-block start-block) 
                      (<= current-block end-block)
                      (is-eq state PROPOSAL_PENDING))
      }
    )
    { state: u0, is-active: false, blocks-remaining: u0, can-vote: false }
  )
)

;; Private functions
(define-private (is-protocol-admin (caller principal))
  (or (is-eq caller CONTRACT_OWNER)
      (is-contract-authorized caller))
)

(define-private (is-emergency-council-member (caller principal))
  (is-some (index-of (var-get emergency-council) caller))
)

(define-private (validate-proposal-parameters 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (proposal-type uint))
  (and (> (len title) u0)
       (> (len description) u0)
       (and (>= proposal-type u1) (<= proposal-type u5)))
)

(define-private (calculate-stake-requirement (proposal-type uint))
  (if (is-eq proposal-type PROPOSAL_TYPE_EMERGENCY)
    (* (var-get min-proposal-stake) u5) ;; 5x for emergency proposals
    (if (is-eq proposal-type PROPOSAL_TYPE_CONTRACT_UPGRADE)
      (* (var-get min-proposal-stake) u3) ;; 3x for contract upgrades
      (var-get min-proposal-stake))) ;; Standard stake for other proposals
)

;; Private execution functions
(define-private (execute-parameter-change (proposal (tuple 
  (proposer principal) (title (string-ascii 100)) (description (string-ascii 500))
  (proposal-type uint) (target-contract (optional principal)) 
  (function-name (optional (string-ascii 50))) (parameters (optional (buff 1024)))
  (stake-amount uint) (start-block uint) (end-block uint)
  (votes-for uint) (votes-against uint) (votes-abstain uint)
  (total-voting-power uint) (state uint) (execution-block (optional uint)) (executed bool))))
;;   (match (get function-name proposal)
;;     param-name
;;     (match (get parameters proposal)
;;       param-value-buff
;;       (let
;;         (
;;           (param-value (buff-to-uint-safe param-value-buff))
;;         )
;;         (try! (update-governance-parameter param-name param-value))
;;         (ok true))
;;       (ok true))
    (ok true))


(define-private (execute-treasury-action (proposal (tuple 
  (proposer principal) (title (string-ascii 100)) (description (string-ascii 500))
  (proposal-type uint) (target-contract (optional principal)) 
  (function-name (optional (string-ascii 50))) (parameters (optional (buff 1024)))
  (stake-amount uint) (start-block uint) (end-block uint)
  (votes-for uint) (votes-against uint) (votes-abstain uint)
  (total-voting-power uint) (state uint) (execution-block (optional uint)) (executed bool))))
  ;; Treasury actions implementation
  (ok true)
)

;; Public functions

;; Initialize protocol parameters
(define-public (initialize-protocol)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    ;; Initialize governance parameters
    (map-set governance-parameters { parameter-name: "min-proposal-stake" } 
             { parameter-value: (var-get min-proposal-stake) })
    (map-set governance-parameters { parameter-name: "voting-period" } 
             { parameter-value: (var-get voting-period) })
    (map-set governance-parameters { parameter-name: "execution-delay" } 
             { parameter-value: (var-get execution-delay) })
    (map-set governance-parameters { parameter-name: "quorum-threshold" } 
             { parameter-value: (var-get quorum-threshold) })
    (map-set governance-parameters { parameter-name: "approval-threshold" } 
             { parameter-value: (var-get approval-threshold) })
    
    ;; Initialize token contract integration
    (try! (contract-call? .tech-gov-token set-governance-contract (as-contract tx-sender)))
    
    (print { event: "protocol-initialized", block: stacks-block-height })
    (ok true)
  )
)

;; Create a new governance proposal
(define-public (create-proposal 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (proposal-type uint)
  (target-contract (optional principal))
  (function-name (optional (string-ascii 50)))
  (parameters (optional (buff 1024))))
  (let
    (
      (proposal-id (+ (var-get proposal-counter) u1))
      (stake-amount (calculate-stake-requirement proposal-type))
      (proposer-power (contract-call? .tech-gov-token get-effective-voting-power tx-sender))
      (proposer-balance (unwrap! (contract-call? .tech-gov-token get-balance tx-sender) ERR_INSUFFICIENT_STAKE))
    )
    (asserts! (is-eq (var-get protocol-state) PROTOCOL_ACTIVE) ERR_PROTOCOL_PAUSED)
    (asserts! (validate-proposal-parameters title description proposal-type) ERR_INVALID_PROPOSAL)
    (asserts! (>= proposer-balance stake-amount) ERR_INSUFFICIENT_STAKE)
    (asserts! (>= proposer-power stake-amount) ERR_INSUFFICIENT_STAKE)
    
    ;; Transfer stake to governance contract
    (try! (contract-call? .tech-gov-token transfer 
           stake-amount tx-sender (as-contract tx-sender) none))
    
    ;; Create proposal
    (map-set proposals
      { proposal-id: proposal-id }
      {
        proposer: tx-sender,
        title: title,
        description: description,
        proposal-type: proposal-type,
        target-contract: target-contract,
        function-name: function-name,
        parameters: parameters,
        stake-amount: stake-amount,
        start-block: (+ stacks-block-height u1),
        end-block: (+ stacks-block-height (var-get voting-period)),
        votes-for: u0,
        votes-against: u0,
        votes-abstain: u0,
        total-voting-power: u0,
        state: PROPOSAL_PENDING,
        execution-block: none,
        executed: false
      }
    )
    
    (var-set proposal-counter proposal-id)
    (print { 
      event: "proposal-created", 
      proposal-id: proposal-id, 
      proposer: tx-sender,
      proposal-type: proposal-type,
      stake-amount: stake-amount
    })
    (ok proposal-id)
  )
)

;; Vote on a proposal
(define-public (vote-on-proposal (proposal-id uint) (vote-type uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR_PROPOSAL_NOT_FOUND))
      (voter-power (contract-call? .tech-gov-token get-effective-voting-power tx-sender))
      (existing-vote (map-get? proposal-votes { proposal-id: proposal-id, voter: tx-sender }))
      (proposal-status (get-proposal-status proposal-id))
    )
    (asserts! (is-eq (var-get protocol-state) PROTOCOL_ACTIVE) ERR_PROTOCOL_PAUSED)
    (asserts! (is-none existing-vote) ERR_ALREADY_VOTED)
    (asserts! (get can-vote proposal-status) ERR_VOTING_PERIOD_ENDED)
    (asserts! (and (>= vote-type VOTE_FOR) (<= vote-type VOTE_ABSTAIN)) ERR_INVALID_VOTE_TYPE)
    (asserts! (> voter-power u0) ERR_INSUFFICIENT_STAKE)
    
    ;; Record vote
    (map-set proposal-votes
      { proposal-id: proposal-id, voter: tx-sender }
      { 
        vote-type: vote-type, 
        vote-power: voter-power, 
        block-height: stacks-block-height,
        is-delegated: false
      })
    
    ;; Update proposal vote counts
    (let
      (
        (current-votes-for (get votes-for proposal))
        (current-votes-against (get votes-against proposal))
        (current-votes-abstain (get votes-abstain proposal))
        (current-total-power (get total-voting-power proposal))
        (new-votes-for (if (is-eq vote-type VOTE_FOR) 
                         (+ current-votes-for voter-power) 
                         current-votes-for))
        (new-votes-against (if (is-eq vote-type VOTE_AGAINST) 
                             (+ current-votes-against voter-power) 
                             current-votes-against))
        (new-votes-abstain (if (is-eq vote-type VOTE_ABSTAIN) 
                             (+ current-votes-abstain voter-power) 
                             current-votes-abstain))
      )
      (map-set proposals 
        { proposal-id: proposal-id }
        (merge proposal {
          votes-for: new-votes-for,
          votes-against: new-votes-against,
          votes-abstain: new-votes-abstain,
          total-voting-power: (+ current-total-power voter-power)
        }))
    )
    
    (print { 
      event: "vote-cast", 
      proposal-id: proposal-id, 
      voter: tx-sender, 
      vote-type: vote-type, 
      vote-power: voter-power 
    })
    (ok true)
  )
)

;; Queue proposal for execution
(define-public (queue-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR_PROPOSAL_NOT_FOUND))
      (result (calculate-proposal-result proposal-id))
    )
    (asserts! (is-eq (var-get protocol-state) PROTOCOL_ACTIVE) ERR_PROTOCOL_PAUSED)
    (asserts! (get can-execute result) ERR_PROPOSAL_NOT_READY)
    (asserts! (is-eq (get state proposal) PROPOSAL_PENDING) ERR_INVALID_PROPOSAL)
    
    ;; Queue for execution
    (let
      (
        (execution-block (+ stacks-block-height (var-get execution-delay)))
      )
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal { 
          state: PROPOSAL_PASSED,
          execution-block: (some execution-block)
        }))
      
      (map-set proposal-execution-queue
        { proposal-id: proposal-id }
        { execution-ready-block: execution-block, queued: true })
    )
    
    ;; Return stake to proposer
    (try! (as-contract (contract-call? .tech-gov-token transfer 
           (get stake-amount proposal) (as-contract tx-sender) (get proposer proposal) none)))
    
    (print { event: "proposal-queued", proposal-id: proposal-id })
    (ok true)
  )
)

;; Execute a queued proposal
(define-public (execute-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR_PROPOSAL_NOT_FOUND))
      (queue-info (unwrap! (map-get? proposal-execution-queue { proposal-id: proposal-id }) ERR_PROPOSAL_NOT_READY))
      (proposal-type (get proposal-type proposal))
    )
    (asserts! (is-eq (var-get protocol-state) PROTOCOL_ACTIVE) ERR_PROTOCOL_PAUSED)
    (asserts! (is-eq (get state proposal) PROPOSAL_PASSED) ERR_INVALID_PROPOSAL)
    (asserts! (not (get executed proposal)) ERR_INVALID_PROPOSAL)
    (asserts! (>= stacks-block-height (get execution-ready-block queue-info)) ERR_PROPOSAL_NOT_READY)
    
    ;; Mark as executed
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { state: PROPOSAL_EXECUTED, executed: true }))
    
    (map-delete proposal-execution-queue { proposal-id: proposal-id })
    
    ;; Execute based on proposal type using if statements
   ;; Alternative approach with unwrap! on the entire if expression
(unwrap! 
  (if (is-eq proposal-type PROPOSAL_TYPE_PARAMETER)
    (execute-parameter-change proposal)
    (if (is-eq proposal-type PROPOSAL_TYPE_TREASURY)
      (execute-treasury-action proposal)
      (ok true)))
  ERR_EXECUTION_FAILED);; Other types handled separately or no action needed
    
    (print { event: "proposal-executed", proposal-id: proposal-id })
    (ok true)
  )
)

;; Cancel a proposal (only by proposer or admin)
(define-public (cancel-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR_PROPOSAL_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender (get proposer proposal))
                  (is-protocol-admin tx-sender)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get state proposal) PROPOSAL_PENDING) ERR_INVALID_PROPOSAL)
    
    ;; Cancel proposal
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { state: PROPOSAL_CANCELLED }))
    
    ;; Return stake to proposer
    (try! (as-contract (contract-call? .tech-gov-token transfer 
           (get stake-amount proposal) (as-contract tx-sender) (get proposer proposal) none)))
    
    (print { event: "proposal-cancelled", proposal-id: proposal-id })
    (ok true)
  )
)

;; Emergency pause protocol
(define-public (emergency-pause)
  (begin
    (asserts! (or (is-protocol-admin tx-sender) 
                  (is-emergency-council-member tx-sender)) ERR_UNAUTHORIZED)
    (var-set protocol-state PROTOCOL_EMERGENCY)
    (print { event: "protocol-paused", caller: tx-sender })
    (ok true)
  )
)

;; Resume protocol
(define-public (resume-protocol)
  (begin
    (asserts! (is-protocol-admin tx-sender) ERR_UNAUTHORIZED)
    (var-set protocol-state PROTOCOL_ACTIVE)
    (print { event: "protocol-resumed", caller: tx-sender })
    (ok true)
  )
)

;; Authorize contract
(define-public (authorize-contract (contract-address principal))
  (begin
    (asserts! (is-protocol-admin tx-sender) ERR_UNAUTHORIZED)
    (map-set authorized-contracts
      { contract-address: contract-address }
      { is-authorized: true, authorization-block: stacks-block-height })
    (print { event: "contract-authorized", contract: contract-address })
    (ok true)
  )
)

;; Update governance parameter
(define-public (update-governance-parameter 
  (parameter-name (string-ascii 50)) 
  (parameter-value uint))
  (begin
    (asserts! (is-protocol-admin tx-sender) ERR_UNAUTHORIZED)
    (map-set governance-parameters
      { parameter-name: parameter-name }
      { parameter-value: parameter-value })
    
    ;; Update local variables for commonly used parameters
    (if (is-eq parameter-name "min-proposal-stake")
      (var-set min-proposal-stake parameter-value)
      (if (is-eq parameter-name "voting-period")
        (var-set voting-period parameter-value)
        (if (is-eq parameter-name "quorum-threshold")
          (var-set quorum-threshold parameter-value)
          (if (is-eq parameter-name "approval-threshold")
            (var-set approval-threshold parameter-value)
            true))))
    
    (print { event: "parameter-updated", parameter: parameter-name, value: parameter-value })
    (ok true)
  )
)

;; Set emergency council
(define-public (set-emergency-council (council-members (list 5 principal)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set emergency-council council-members)
    (print { event: "emergency-council-updated", members: council-members })
    (ok true)
  )
)