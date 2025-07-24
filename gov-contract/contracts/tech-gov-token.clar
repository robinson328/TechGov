;; TechGov Token - Governance Token Contract
;; SIP-010 compliant fungible token for TechGov Protocol governance

(impl-trait .sip-010-trait.sip-010-trait)

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_TOKEN_OWNER (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))
(define-constant ERR_TRANSFER_FAILED (err u104))
(define-constant ERR_MINT_FAILED (err u105))
(define-constant ERR_BURN_FAILED (err u106))
(define-constant ERR_INVALID_RECIPIENT (err u107))
(define-constant ERR_STAKING_PERIOD_NOT_ENDED (err u108))
(define-constant ERR_ALREADY_STAKED (err u109))
(define-constant ERR_NOT_STAKED (err u110))

;; Token Configuration
(define-constant TOKEN_NAME "TechGov Token")
(define-constant TOKEN_SYMBOL "TGOV")
(define-constant TOKEN_DECIMALS u6)
(define-constant TOKEN_URI u"https://techgov.protocol/token-metadata.json")
(define-constant TOTAL_SUPPLY u1000000000000000) ;; 1 billion tokens with 6 decimals

;; Data Variables
(define-data-var token-total-supply uint TOTAL_SUPPLY)
(define-data-var governance-contract (optional principal) none)
(define-data-var minting-enabled bool true)
(define-data-var transfer-restrictions bool false)

;; Data Maps
(define-map token-balances principal uint)
(define-map token-allowances { owner: principal, spender: principal } uint)

;; Staking Maps
(define-map staked-balances 
  { staker: principal }
  { 
    amount: uint, 
    stake-block: uint, 
    lock-period: uint,
    reward-multiplier: uint
  }
)

(define-map delegation-map
  { delegator: principal }
  { 
    delegate: principal, 
    amount: uint, 
    delegation-block: uint 
  }
)

(define-map voting-power-cache
  { account: principal }
  { 
    power: uint, 
    last-update: uint 
  }
)

;; Authorized minters for governance rewards
(define-map authorized-minters
  { minter: principal }
  { is-authorized: bool, authorization-block: uint }
)

;; SIP-010 Standard Functions

(define-read-only (get-name)
  (ok TOKEN_NAME)
)

(define-read-only (get-symbol)
  (ok TOKEN_SYMBOL)
)

(define-read-only (get-decimals)
  (ok TOKEN_DECIMALS)
)

(define-read-only (get-balance (who principal))
  (ok (default-to u0 (map-get? token-balances who)))
)

(define-read-only (get-total-supply)
  (ok (var-get token-total-supply))
)

(define-read-only (get-token-uri)
  (ok (some TOKEN_URI))
)

(define-public (transfer (amount uint) (from principal) (to principal) (memo (optional (buff 34))))
  (begin
    (asserts! (or (is-eq tx-sender from) (is-eq contract-caller from)) ERR_NOT_TOKEN_OWNER)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (not (is-eq from to)) ERR_INVALID_RECIPIENT)
    (asserts! (not (var-get transfer-restrictions)) ERR_UNAUTHORIZED)
    
    (let
      (
        (from-balance (unwrap! (get-balance from) ERR_INSUFFICIENT_BALANCE))
        (to-balance (unwrap! (get-balance to) ERR_INSUFFICIENT_BALANCE))
      )
      (asserts! (>= from-balance amount) ERR_INSUFFICIENT_BALANCE)
      
      ;; Update balances
      (map-set token-balances from (- from-balance amount))
      (map-set token-balances to (+ to-balance amount))
      
      ;; Update voting power cache
      (unwrap! (update-voting-power from) ERR_TRANSFER_FAILED)
      (unwrap! (update-voting-power to) ERR_BURN_FAILED)
      
      (print { 
        event: "transfer", 
        from: from, 
        to: to, 
        amount: amount, 
        memo: memo 
      })
      (ok true)
    )
  )
)

;; Extended Token Functions

(define-read-only (get-staked-balance (staker principal))
  (match (map-get? staked-balances { staker: staker })
    stake-info (get amount stake-info)
    u0
  )
)

(define-read-only (get-delegation-info (delegator principal))
  (map-get? delegation-map { delegator: delegator })
)

(define-read-only (get-voting-power (account principal))
  (let
    (
      (base-balance (unwrap-panic (get-balance account)))
      (staked-info (map-get? staked-balances { staker: account }))
      (staked-power (match staked-info
        info (* (get amount info) (get reward-multiplier info))
        u0))
    )
    (+ base-balance staked-power)
  )
)

(define-read-only (get-effective-voting-power (account principal))
  (let
    (
      (own-power (get-voting-power account))
      (delegation-info (map-get? delegation-map { delegator: account }))
    )
    (match delegation-info
      info (if (is-eq (get delegate info) account)
             (+ own-power (get amount info))
             own-power)
      own-power)
  )
)

;; Private Functions

(define-private (is-authorized-minter (caller principal))
  (or (is-eq caller CONTRACT_OWNER)
      (match (var-get governance-contract)
        gov-contract (is-eq caller gov-contract)
        false)
      (default-to false 
        (get is-authorized 
          (map-get? authorized-minters { minter: caller }))))
)

(define-private (update-voting-power (account principal))
  (let
    (
      (current-power (get-effective-voting-power account))
    )
    (map-set voting-power-cache
      { account: account }
      { power: current-power, last-update: stacks-block-height })
    (ok current-power)
  )
)

;; Public Functions

;; Initialize token distribution
(define-public (initialize-token)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    ;; Mint initial supply to contract owner
    (map-set token-balances CONTRACT_OWNER TOTAL_SUPPLY)
    (unwrap! (update-voting-power CONTRACT_OWNER) ERR_INSUFFICIENT_BALANCE)
    (print { event: "token-initialized", total-supply: TOTAL_SUPPLY })
    (ok true)
  )
)

;; Set governance contract
(define-public (set-governance-contract (gov-contract principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set governance-contract (some gov-contract))
    (print { event: "governance-contract-set", contract: gov-contract })
    (ok true)
  )
)

;; Mint tokens (only authorized minters)
(define-public (mint (amount uint) (recipient principal))
  (begin
    (asserts! (is-authorized-minter tx-sender) ERR_UNAUTHORIZED)
    (asserts! (var-get minting-enabled) ERR_MINT_FAILED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    
    (let
      (
        (current-balance (unwrap! (get-balance recipient) ERR_MINT_FAILED))
        (new-total-supply (+ (var-get token-total-supply) amount))
      )
      (map-set token-balances recipient (+ current-balance amount))
      (var-set token-total-supply new-total-supply)
      (unwrap! (update-voting-power recipient) ERR_MINT_FAILED)
      
      (print { event: "mint", recipient: recipient, amount: amount })
      (ok true)
    )
  )
)

;; Burn tokens
(define-public (burn (amount uint))
  (let
    (
      (current-balance (unwrap! (get-balance tx-sender) ERR_BURN_FAILED))
      (new-total-supply (- (var-get token-total-supply) amount))
    )
    (asserts! (>= current-balance amount) ERR_INSUFFICIENT_BALANCE)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    
    (map-set token-balances tx-sender (- current-balance amount))
    (var-set token-total-supply new-total-supply)
    (unwrap! (update-voting-power tx-sender) ERR_INVALID_AMOUNT)
    
    (print { event: "burn", burner: tx-sender, amount: amount })
    (ok true)
  )
)

;; Stake tokens for enhanced voting power
(define-public (stake-tokens (amount uint) (lock-period uint))
  (let
    (
      (current-balance (unwrap! (get-balance tx-sender) ERR_INSUFFICIENT_BALANCE))
      (existing-stake (map-get? staked-balances { staker: tx-sender }))
      (multiplier (if (>= lock-period u52560) ;; ~1 year in blocks
                    u3
                    (if (>= lock-period u26280) ;; ~6 months
                      u2
                      u1)))
    )
    (asserts! (is-none existing-stake) ERR_ALREADY_STAKED)
    (asserts! (>= current-balance amount) ERR_INSUFFICIENT_BALANCE)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= lock-period u1008) ERR_INVALID_AMOUNT) ;; Minimum 1 week
    
    ;; Transfer tokens to staking
    (map-set token-balances tx-sender (- current-balance amount))
    
    ;; Record stake
    (map-set staked-balances
      { staker: tx-sender }
      {
        amount: amount,
        stake-block: stacks-block-height,
        lock-period: lock-period,
        reward-multiplier: multiplier
      })
    
    (unwrap! (update-voting-power tx-sender) ERR_INSUFFICIENT_BALANCE)
    (print { event: "tokens-staked", staker: tx-sender, amount: amount, lock-period: lock-period })
    (ok true)
  )
)

;; Unstake tokens
(define-public (unstake-tokens)
  (let
    (
      (stake-info (unwrap! (map-get? staked-balances { staker: tx-sender }) ERR_NOT_STAKED))
      (current-balance (unwrap! (get-balance tx-sender) ERR_INSUFFICIENT_BALANCE))
      (stake-amount (get amount stake-info))
      (unlock-block (+ (get stake-block stake-info) (get lock-period stake-info)))
    )
    (asserts! (>= stacks-block-height unlock-block) ERR_STAKING_PERIOD_NOT_ENDED)
    
    ;; Return staked tokens
    (map-set token-balances tx-sender (+ current-balance stake-amount))
    (map-delete staked-balances { staker: tx-sender })
    
    (unwrap! (update-voting-power tx-sender) ERR_INVALID_AMOUNT)
    (print { event: "tokens-unstaked", staker: tx-sender, amount: stake-amount })
    (ok true)
  )
)

;; Delegate voting power
(define-public (delegate-voting-power (delegate principal) (amount uint))
  (let
    (
      (current-balance (unwrap! (get-balance tx-sender) ERR_INSUFFICIENT_BALANCE))
      (existing-delegation (map-get? delegation-map { delegator: tx-sender }))
    )
    (asserts! (is-none existing-delegation) ERR_ALREADY_STAKED)
    (asserts! (>= current-balance amount) ERR_INSUFFICIENT_BALANCE)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (not (is-eq tx-sender delegate)) ERR_INVALID_RECIPIENT)
    
    (map-set delegation-map
      { delegator: tx-sender }
      {
        delegate: delegate,
        amount: amount,
        delegation-block: stacks-block-height
      })
    
    (unwrap! (update-voting-power tx-sender) ERR_NOT_STAKED)
    (unwrap! (update-voting-power delegate) ERR_INSUFFICIENT_BALANCE)
    
    (print { event: "voting-power-delegated", delegator: tx-sender, delegate: delegate, amount: amount })
    (ok true)
  )
)

;; Revoke delegation
(define-public (revoke-delegation)
  (let
    (
      (delegation-info (unwrap! (map-get? delegation-map { delegator: tx-sender }) ERR_NOT_STAKED))
      (delegate (get delegate delegation-info))
    )
    (map-delete delegation-map { delegator: tx-sender })
    
    (unwrap! (update-voting-power tx-sender) ERR_INVALID_AMOUNT)
    (unwrap! (update-voting-power delegate) ERR_INVALID_RECIPIENT)
    
    (print { event: "delegation-revoked", delegator: tx-sender, delegate: delegate })
    (ok true)
  )
)

;; Authorize minter
(define-public (authorize-minter (minter principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set authorized-minters
      { minter: minter }
      { is-authorized: true, authorization-block: stacks-block-height })
    (print { event: "minter-authorized", minter: minter })
    (ok true)
  )
)

;; Revoke minter authorization
(define-public (revoke-minter (minter principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-delete authorized-minters { minter: minter })
    (print { event: "minter-revoked", minter: minter })
    (ok true)
  )
)

;; Toggle minting
(define-public (toggle-minting)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set minting-enabled (not (var-get minting-enabled)))
    (print { event: "minting-toggled", enabled: (var-get minting-enabled) })
    (ok true)
  )
)

;; Toggle transfer restrictions
(define-public (toggle-transfer-restrictions)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set transfer-restrictions (not (var-get transfer-restrictions)))
    (print { event: "transfer-restrictions-toggled", enabled: (var-get transfer-restrictions) })
    (ok true)
  )
)

;; Batch transfer for airdrops
(define-public (batch-transfer (recipients (list 200 { recipient: principal, amount: uint })))
  (begin
    (asserts! (is-authorized-minter tx-sender) ERR_UNAUTHORIZED)
    (fold batch-transfer-helper recipients (ok true))
  )
)

(define-private (batch-transfer-helper 
  (transfer-info { recipient: principal, amount: uint })
  (previous-result (response bool uint)))
  (match previous-result
    success (transfer (get amount transfer-info) tx-sender (get recipient transfer-info) none)
    error (err error)
  )
)