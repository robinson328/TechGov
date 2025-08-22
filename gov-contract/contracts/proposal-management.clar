;; Proposal Manager Smart Contract
;; Handles proposal creation, submission, and lifecycle management

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u101))
(define-constant ERR_INVALID_STATUS_TRANSITION (err u102))
(define-constant ERR_PROPOSAL_ALREADY_EXISTS (err u103))
(define-constant ERR_INVALID_PROPOSAL_DATA (err u104))
(define-constant ERR_VOTING_PERIOD_ACTIVE (err u105))
(define-constant ERR_VOTING_PERIOD_ENDED (err u106))

;; Proposal Status Enum
(define-constant STATUS_DRAFT u0)
(define-constant STATUS_SUBMITTED u1)
(define-constant STATUS_UNDER_REVIEW u2)
(define-constant STATUS_APPROVED u3)
(define-constant STATUS_REJECTED u4)
(define-constant STATUS_WITHDRAWN u5)
(define-constant STATUS_EXECUTED u6)

;; Data Variables
(define-data-var proposal-counter uint u0)
(define-data-var voting-period uint u1008) ;; ~1 week in blocks

;; Data Maps
(define-map proposals
  { proposal-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposer: principal,
    status: uint,
    created-at: uint,
    submitted-at: (optional uint),
    voting-ends-at: (optional uint),
    votes-for: uint,
    votes-against: uint,
    execution-delay: uint,
    metadata-uri: (optional (string-ascii 200))
  }
)

(define-map proposal-votes
  { proposal-id: uint, voter: principal }
  { vote: bool, voted-at: uint }
)

(define-map user-voting-power
  { user: principal }
  { power: uint, updated-at: uint }
)

(define-map proposal-reviewers
  { proposal-id: uint, reviewer: principal }
  { assigned-at: uint, review-status: uint }
)

;; Authorization Maps
(define-map authorized-reviewers principal bool)
(define-map authorized-executors principal bool)

;; Read-only functions

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-proposal-vote (proposal-id uint) (voter principal))
  (map-get? proposal-votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-voting-power (user principal))
  (default-to { power: u1, updated-at: u0 }
    (map-get? user-voting-power { user: user }))
)

(define-read-only (get-proposal-counter)
  (var-get proposal-counter)
)

(define-read-only (is-authorized-reviewer (user principal))
  (default-to false (map-get? authorized-reviewers user))
)

(define-read-only (is-authorized-executor (user principal))
  (default-to false (map-get? authorized-executors user))
)

(define-read-only (can-vote (proposal-id uint))
  (match (get-proposal proposal-id)
    proposal (and 
      (is-eq (get status proposal) STATUS_UNDER_REVIEW)
      (match (get voting-ends-at proposal)
        voting-end (< stacks-block-height voting-end)
        false))
    false)
)

(define-read-only (get-proposal-results (proposal-id uint))
  (match (get-proposal proposal-id)
    proposal 
    ;; Wrap tuple in 'some' to match return type with 'none' arm
    (some {
      proposal-id: proposal-id,
      votes-for: (get votes-for proposal),
      votes-against: (get votes-against proposal),
      total-votes: (+ (get votes-for proposal) (get votes-against proposal)),
      status: (get status proposal)
    })
    none)
)

;; Private functions

(define-private (is-valid-status-transition (current-status uint) (new-status uint))
  (or
    ;; Draft -> Submitted
    (and (is-eq current-status STATUS_DRAFT) (is-eq new-status STATUS_SUBMITTED))
    ;; Submitted -> Under Review
    (and (is-eq current-status STATUS_SUBMITTED) (is-eq new-status STATUS_UNDER_REVIEW))
    ;; Under Review -> Approved/Rejected
    (and (is-eq current-status STATUS_UNDER_REVIEW) 
         (or (is-eq new-status STATUS_APPROVED) (is-eq new-status STATUS_REJECTED)))
    ;; Approved -> Executed
    (and (is-eq current-status STATUS_APPROVED) (is-eq new-status STATUS_EXECUTED))
    ;; Any status -> Withdrawn (by proposer)
    (is-eq new-status STATUS_WITHDRAWN)
  )
)

(define-private (increment-proposal-counter)
  (let ((current-counter (var-get proposal-counter)))
    (var-set proposal-counter (+ current-counter u1))
    (+ current-counter u1))
)

;; Public functions

(define-public (create-proposal 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (execution-delay uint)
  (metadata-uri (optional (string-ascii 200))))
  (let ((proposal-id (increment-proposal-counter)))
    (if (and (> (len title) u0) (> (len description) u0))
      (begin
        (map-set proposals
          { proposal-id: proposal-id }
          {
            title: title,
            description: description,
            proposer: tx-sender,
            status: STATUS_DRAFT,
            created-at: stacks-block-height,
            submitted-at: none,
            voting-ends-at: none,
            votes-for: u0,
            votes-against: u0,
            execution-delay: execution-delay,
            metadata-uri: metadata-uri
          }
        )
        (print { event: "proposal-created", proposal-id: proposal-id, proposer: tx-sender })
        (ok proposal-id))
      ERR_INVALID_PROPOSAL_DATA))
)

(define-public (submit-proposal (proposal-id uint))
  (match (get-proposal proposal-id)
    proposal
    (if (is-eq (get proposer proposal) tx-sender)
      (if (is-eq (get status proposal) STATUS_DRAFT)
        (begin
          (map-set proposals
            { proposal-id: proposal-id }
            (merge proposal { 
              status: STATUS_SUBMITTED, 
              submitted-at: (some stacks-block-height) 
            }))
          (print { event: "proposal-submitted", proposal-id: proposal-id })
          (ok true))
        ERR_INVALID_STATUS_TRANSITION)
      ERR_UNAUTHORIZED)
    ERR_PROPOSAL_NOT_FOUND)
)

(define-public (start-review (proposal-id uint))
  (if (is-authorized-reviewer tx-sender)
    (match (get-proposal proposal-id)
      proposal
      (if (is-eq (get status proposal) STATUS_SUBMITTED)
        (let ((voting-end (+ stacks-block-height (var-get voting-period))))
          (map-set proposals
            { proposal-id: proposal-id }
            (merge proposal { 
              status: STATUS_UNDER_REVIEW,
              voting-ends-at: (some voting-end)
            }))
          (print { event: "review-started", proposal-id: proposal-id, voting-ends-at: voting-end })
          (ok true))
        ERR_INVALID_STATUS_TRANSITION)
      ERR_PROPOSAL_NOT_FOUND)
    ERR_UNAUTHORIZED)
)

(define-public (vote-on-proposal (proposal-id uint) (vote bool))
  (if (can-vote proposal-id)
    (let ((voter-power (get power (get-voting-power tx-sender))))
      (match (get-proposal-vote proposal-id tx-sender)
        existing-vote ERR_UNAUTHORIZED ;; Already voted
        (match (get-proposal proposal-id)
          proposal
          (begin
            ;; Record the vote
            (map-set proposal-votes
              { proposal-id: proposal-id, voter: tx-sender }
              { vote: vote, voted-at: stacks-block-height })
            ;; Update proposal vote counts
            (map-set proposals
              { proposal-id: proposal-id }
              (merge proposal {
                votes-for: (if vote 
                  (+ (get votes-for proposal) voter-power)
                  (get votes-for proposal)),
                votes-against: (if vote
                  (get votes-against proposal)
                  (+ (get votes-against proposal) voter-power))
              }))
            (print { event: "vote-cast", proposal-id: proposal-id, voter: tx-sender, vote: vote })
            (ok true))
          ERR_PROPOSAL_NOT_FOUND)))
    ERR_VOTING_PERIOD_ENDED)
)

(define-public (finalize-proposal (proposal-id uint))
  (if (is-authorized-reviewer tx-sender)
    (match (get-proposal proposal-id)
      proposal
      (if (is-eq (get status proposal) STATUS_UNDER_REVIEW)
        (match (get voting-ends-at proposal)
          voting-end
          (if (>= stacks-block-height voting-end)
            (let ((votes-for (get votes-for proposal))
                  (votes-against (get votes-against proposal))
                  (new-status (if (> votes-for votes-against) STATUS_APPROVED STATUS_REJECTED)))
              (map-set proposals
                { proposal-id: proposal-id }
                (merge proposal { status: new-status }))
              (print { 
                event: "proposal-finalized", 
                proposal-id: proposal-id, 
                status: new-status,
                votes-for: votes-for,
                votes-against: votes-against 
              })
              (ok new-status))
            ERR_VOTING_PERIOD_ACTIVE)
          ERR_VOTING_PERIOD_ACTIVE)
        ERR_INVALID_STATUS_TRANSITION)
      ERR_PROPOSAL_NOT_FOUND)
    ERR_UNAUTHORIZED)
)

(define-public (execute-proposal (proposal-id uint))
  (if (is-authorized-executor tx-sender)
    (match (get-proposal proposal-id)
      proposal
      (if (is-eq (get status proposal) STATUS_APPROVED)
        (begin
          (map-set proposals
            { proposal-id: proposal-id }
            (merge proposal { status: STATUS_EXECUTED }))
          (print { event: "proposal-executed", proposal-id: proposal-id })
          (ok true))
        ERR_INVALID_STATUS_TRANSITION)
      ERR_PROPOSAL_NOT_FOUND)
    ERR_UNAUTHORIZED)
)

(define-public (withdraw-proposal (proposal-id uint))
  (match (get-proposal proposal-id)
    proposal
    (if (is-eq (get proposer proposal) tx-sender)
      (begin
        (map-set proposals
          { proposal-id: proposal-id }
          (merge proposal { status: STATUS_WITHDRAWN }))
        (print { event: "proposal-withdrawn", proposal-id: proposal-id })
        (ok true))
      ERR_UNAUTHORIZED)
    ERR_PROPOSAL_NOT_FOUND)
)

(define-public (set-voting-power (user principal) (power uint))
  (if (is-eq tx-sender CONTRACT_OWNER)
    (begin
      (map-set user-voting-power
        { user: user }
        { power: power, updated-at: stacks-block-height })
      (print { event: "voting-power-updated", user: user, power: power })
      (ok true))
    ERR_UNAUTHORIZED)
)

(define-public (authorize-reviewer (reviewer principal))
  (if (is-eq tx-sender CONTRACT_OWNER)
    (begin
      (map-set authorized-reviewers reviewer true)
      (print { event: "reviewer-authorized", reviewer: reviewer })
      (ok true))
    ERR_UNAUTHORIZED)
)

(define-public (authorize-executor (executor principal))
  (if (is-eq tx-sender CONTRACT_OWNER)
    (begin
      (map-set authorized-executors executor true)
      (print { event: "executor-authorized", executor: executor })
      (ok true))
    ERR_UNAUTHORIZED)
)

(define-public (revoke-reviewer (reviewer principal))
  (if (is-eq tx-sender CONTRACT_OWNER)
    (begin
      (map-delete authorized-reviewers reviewer)
      (print { event: "reviewer-revoked", reviewer: reviewer })
      (ok true))
    ERR_UNAUTHORIZED)
)

(define-public (update-voting-period (new-period uint))
  (if (is-eq tx-sender CONTRACT_OWNER)
    (begin
      (var-set voting-period new-period)
      (print { event: "voting-period-updated", new-period: new-period })
      (ok true))
    ERR_UNAUTHORIZED)
)

;; Initialize contract with owner as first reviewer and executor
(map-set authorized-reviewers CONTRACT_OWNER true)
(map-set authorized-executors CONTRACT_OWNER true)
