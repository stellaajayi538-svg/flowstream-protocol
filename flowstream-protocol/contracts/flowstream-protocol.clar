;; FlowStream Protocol
;; Adaptive liquidity allocation system with community expertise verification
;; Clarity Version: 2 | Epoch: 2.1

;; ============================================================
;; CONSTANTS
;; ============================================================

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-REGISTERED (err u101))
(define-constant ERR-NOT-REGISTERED (err u102))
(define-constant ERR-INSUFFICIENT-STAKE (err u103))
(define-constant ERR-POOL-NOT-FOUND (err u104))
(define-constant ERR-INVALID-ALLOCATION (err u105))
(define-constant ERR-ALREADY-VOTED (err u106))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u107))
(define-constant ERR-PROPOSAL-CLOSED (err u108))
(define-constant ERR-BELOW-CONSENSUS-THRESHOLD (err u109))
(define-constant ERR-ZERO-AMOUNT (err u110))
(define-constant ERR-INVALID-DOMAIN (err u111))

;; Minimum STX stake to register as a Strategy Node
(define-constant MIN-NODE-STAKE u1000000) ;; 1 STX in uSTX

;; Consensus threshold: 66% of weighted votes must agree
(define-constant CONSENSUS-THRESHOLD u66)

;; Maximum basis points (100%)
(define-constant MAX-BPS u10000)

;; Quadratic allocation dampener (sqrt approximation scaling factor)
(define-constant QUADRATIC-SCALE u100)

;; Proposal voting window in blocks (~7 days at ~10 min/block)
(define-constant VOTING-WINDOW u1008)

;; Performance history window for accuracy scoring
(define-constant ACCURACY-WINDOW u100)

;; ============================================================
;; DATA VARS
;; ============================================================

(define-data-var total-deposited uint u0)
(define-data-var proposal-nonce uint u0)
(define-data-var rebalance-locked bool false)

;; ============================================================
;; DATA MAPS
;; ============================================================

;; Strategy Node registry
;; domain: 0 = lending, 1 = DEX arbitrage, 2 = general
(define-map strategy-nodes
  { node: principal }
  {
    domain: uint,
    staked: uint,
    accuracy-score: uint,   ;; 0-100, updated after each consensus round
    predictions-made: uint,
    predictions-correct: uint,
    registered-at: uint,
    active: bool
  }
)

;; Yield pools tracked by the protocol
(define-map yield-pools
  { pool-id: uint }
  {
    name: (string-ascii 32),
    domain: uint,           ;; domain specialization matching Strategy Nodes
    current-allocation: uint, ;; basis points of total TVL allocated here
    apy-bps: uint,          ;; current reported APY in basis points
    risk-score: uint,       ;; 0-100, lower = safer
    total-migrated: uint,
    last-rebalance: uint
  }
)

;; Pool counter
(define-data-var pool-nonce uint u0)

;; Depositor balances
(define-map depositor-balances
  { depositor: principal }
  {
    deposited: uint,
    deposit-block: uint,
    accrued-yield: uint,
    last-claim-block: uint
  }
)

;; Allocation proposals submitted by Strategy Nodes
(define-map proposals
  { proposal-id: uint }
  {
    proposer: principal,
    pool-id: uint,
    proposed-allocation-bps: uint,
    domain: uint,
    votes-for: uint,        ;; weighted voting power in favor
    votes-against: uint,
    status: uint,           ;; 0 = open, 1 = approved, 2 = rejected
    created-at: uint,
    closes-at: uint,
    executed: bool
  }
)

;; Vote tracking to prevent double voting
(define-map votes-cast
  { proposal-id: uint, voter: principal }
  { vote: bool, weight: uint }
)

;; Risk scores per pool updated by consensus
(define-map pool-risk-scores
  { pool-id: uint }
  { score: uint, updated-at: uint }
)

;; ============================================================
;; PRIVATE HELPERS
;; ============================================================

;; Approximate integer square root via Newton's method (3 iterations)
(define-private (isqrt (n uint))
  (if (is-eq n u0)
    u0
    (let
      (
        (x0 (+ u1 (/ n u2)))
        (x1 (/ (+ x0 (/ n x0)) u2))
        (x2 (/ (+ x1 (/ n x1)) u2))
        (x3 (/ (+ x2 (/ n x2)) u2))
      )
      x3
    )
  )
)

;; Compute quadratic voting weight from deposited amount
;; Returns sqrt(deposited) * QUADRATIC-SCALE to favor smaller depositors
(define-private (quadratic-weight (amount uint))
  (* (isqrt amount) QUADRATIC-SCALE)
)

;; Compute node voting power: accuracy-score * sqrt(staked)
(define-private (node-voting-power (node principal))
  (match (map-get? strategy-nodes { node: node })
    node-data
      (if (get active node-data)
        (* (get accuracy-score node-data) (isqrt (get staked node-data)))
        u0
      )
    u0
  )
)

;; Check if caller is a registered active Strategy Node
(define-private (is-active-node (caller principal))
  (match (map-get? strategy-nodes { node: caller })
    node-data (get active node-data)
    false
  )
)

;; Check if a proposal's voting window is still open
(define-private (proposal-open (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    p (and
        (is-eq (get status p) u0)
        (<= block-height (get closes-at p))
      )
    false
  )
)

;; ============================================================
;; STRATEGY NODE MANAGEMENT
;; ============================================================

;; Register as a Strategy Node by staking STX
;; domain: 0 = lending, 1 = DEX arbitrage, 2 = general
(define-public (register-strategy-node (domain uint))
  (let
    (
      (caller tx-sender)
      (stake-amount MIN-NODE-STAKE)
    )
    (asserts! (is-none (map-get? strategy-nodes { node: caller })) ERR-ALREADY-REGISTERED)
    (asserts! (< domain u3) ERR-INVALID-DOMAIN)
    (try! (stx-transfer? stake-amount caller (as-contract tx-sender)))
    (map-set strategy-nodes
      { node: caller }
      {
        domain: domain,
        staked: stake-amount,
        accuracy-score: u50,    ;; start at neutral 50/100
        predictions-made: u0,
        predictions-correct: u0,
        registered-at: block-height,
        active: true
      }
    )
    (ok true)
  )
)

;; Add more stake to increase voting power
(define-public (add-stake (amount uint))
  (let
    (
      (caller tx-sender)
      (node-data (unwrap! (map-get? strategy-nodes { node: caller }) ERR-NOT-REGISTERED))
    )
    (asserts! (> amount u0) ERR-ZERO-AMOUNT)
    (try! (stx-transfer? amount caller (as-contract tx-sender)))
    (map-set strategy-nodes
      { node: caller }
      (merge node-data { staked: (+ (get staked node-data) amount) })
    )
    (ok true)
  )
)

;; Deregister and withdraw stake
(define-public (deregister-strategy-node)
  (let
    (
      (caller tx-sender)
      (node-data (unwrap! (map-get? strategy-nodes { node: caller }) ERR-NOT-REGISTERED))
      (stake (get staked node-data))
    )
    (map-set strategy-nodes
      { node: caller }
      (merge node-data { active: false, staked: u0 })
    )
    (as-contract (stx-transfer? stake tx-sender caller))
  )
)

;; ============================================================
;; POOL MANAGEMENT (owner only)
;; ============================================================

(define-public (add-yield-pool
    (name (string-ascii 32))
    (domain uint)
    (initial-apy-bps uint)
    (initial-risk-score uint)
  )
  (let
    (
      (pool-id (var-get pool-nonce))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (< domain u3) ERR-INVALID-DOMAIN)
    (map-set yield-pools
      { pool-id: pool-id }
      {
        name: name,
        domain: domain,
        current-allocation: u0,
        apy-bps: initial-apy-bps,
        risk-score: initial-risk-score,
        total-migrated: u0,
        last-rebalance: block-height
      }
    )
    (var-set pool-nonce (+ pool-id u1))
    (ok pool-id)
  )
)

;; Update a pool's APY (called by owner after reading on-chain data)
(define-public (update-pool-apy (pool-id uint) (new-apy-bps uint))
  (let
    (
      (pool (unwrap! (map-get? yield-pools { pool-id: pool-id }) ERR-POOL-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set yield-pools
      { pool-id: pool-id }
      (merge pool { apy-bps: new-apy-bps })
    )
    (ok true)
  )
)

;; ============================================================
;; DEPOSITOR INTERFACE
;; ============================================================

;; Deposit STX into the FlowStream protocol
(define-public (deposit (amount uint))
  (let
    (
      (caller tx-sender)
      (existing (default-to
        { deposited: u0, deposit-block: block-height, accrued-yield: u0, last-claim-block: block-height }
        (map-get? depositor-balances { depositor: caller })
      ))
    )
    (asserts! (> amount u0) ERR-ZERO-AMOUNT)
    (try! (stx-transfer? amount caller (as-contract tx-sender)))
    (map-set depositor-balances
      { depositor: caller }
      (merge existing {
        deposited: (+ (get deposited existing) amount),
        deposit-block: block-height
      })
    )
    (var-set total-deposited (+ (var-get total-deposited) amount))
    (ok true)
  )
)

;; Withdraw deposited STX
(define-public (withdraw (amount uint))
  (let
    (
      (caller tx-sender)
      (bal (unwrap! (map-get? depositor-balances { depositor: caller }) ERR-NOT-REGISTERED))
    )
    (asserts! (> amount u0) ERR-ZERO-AMOUNT)
    (asserts! (>= (get deposited bal) amount) ERR-INSUFFICIENT-STAKE)
    (map-set depositor-balances
      { depositor: caller }
      (merge bal { deposited: (- (get deposited bal) amount) })
    )
    (var-set total-deposited (- (var-get total-deposited) amount))
    (as-contract (stx-transfer? amount tx-sender caller))
  )
)

;; Compute and claim accrued yield for depositor
;; Yield = deposited * apy-bps * blocks-elapsed / (blocks-per-year * MAX-BPS)
;; blocks-per-year ~ 52560 at 10 min/block
(define-public (claim-yield)
  (let
    (
      (caller tx-sender)
      (bal (unwrap! (map-get? depositor-balances { depositor: caller }) ERR-NOT-REGISTERED))
      (blocks-elapsed (- block-height (get last-claim-block bal)))
      ;; Use a simple blended APY from total deposited and pool 0 for MVP
      ;; A production version would weight across all pools
      (blended-apy-bps (match (map-get? yield-pools { pool-id: u0 })
        pool (get apy-bps pool)
        u500 ;; 5% default fallback
      ))
      ;; Quadratic weight reduces yield for large depositors relative to small
      (q-weight (quadratic-weight (get deposited bal)))
      (base-yield (/
        (* (get deposited bal) blended-apy-bps blocks-elapsed)
        (* u52560 MAX-BPS)
      ))
      ;; Scale by quadratic weight factor (dampened, not fully quadratic for safety)
      (adjusted-yield (/ (* base-yield q-weight) (* QUADRATIC-SCALE u100)))
      (total-yield (+ (get accrued-yield bal) adjusted-yield))
    )
    (asserts! (> total-yield u0) ERR-ZERO-AMOUNT)
    (map-set depositor-balances
      { depositor: caller }
      (merge bal { accrued-yield: u0, last-claim-block: block-height })
    )
    (as-contract (stx-transfer? total-yield tx-sender caller))
  )
)

;; ============================================================
;; PREDICTIVE YIELD CONSENSUS - PROPOSALS
;; ============================================================

;; Strategy Node submits an allocation proposal for a pool
(define-public (submit-allocation-proposal
    (pool-id uint)
    (proposed-allocation-bps uint)
  )
  (let
    (
      (caller tx-sender)
      (proposal-id (var-get proposal-nonce))
      (node-data (unwrap! (map-get? strategy-nodes { node: caller }) ERR-NOT-REGISTERED))
      (pool (unwrap! (map-get? yield-pools { pool-id: pool-id }) ERR-POOL-NOT-FOUND))
    )
    (asserts! (get active node-data) ERR-NOT-AUTHORIZED)
    (asserts! (<= proposed-allocation-bps MAX-BPS) ERR-INVALID-ALLOCATION)
    ;; Node must match pool domain (or be general domain u2)
    (asserts!
      (or (is-eq (get domain node-data) (get domain pool)) (is-eq (get domain node-data) u2))
      ERR-INVALID-DOMAIN
    )
    (map-set proposals
      { proposal-id: proposal-id }
      {
        proposer: caller,
        pool-id: pool-id,
        proposed-allocation-bps: proposed-allocation-bps,
        domain: (get domain pool),
        votes-for: u0,
        votes-against: u0,
        status: u0,
        created-at: block-height,
        closes-at: (+ block-height VOTING-WINDOW),
        executed: false
      }
    )
    (var-set proposal-nonce (+ proposal-id u1))
    (ok proposal-id)
  )
)

;; Strategy Node votes on an open proposal
;; Voting power = accuracy-score * sqrt(staked)
(define-public (vote-on-proposal (proposal-id uint) (vote bool))
  (let
    (
      (caller tx-sender)
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
      (node-data (unwrap! (map-get? strategy-nodes { node: caller }) ERR-NOT-REGISTERED))
      (power (node-voting-power caller))
    )
    (asserts! (get active node-data) ERR-NOT-AUTHORIZED)
    (asserts! (proposal-open proposal-id) ERR-PROPOSAL-CLOSED)
    (asserts! (is-none (map-get? votes-cast { proposal-id: proposal-id, voter: caller })) ERR-ALREADY-VOTED)
    ;; Domain check: voter must match proposal domain or be general
    (asserts!
      (or (is-eq (get domain node-data) (get domain proposal)) (is-eq (get domain node-data) u2))
      ERR-INVALID-DOMAIN
    )
    (map-set votes-cast
      { proposal-id: proposal-id, voter: caller }
      { vote: vote, weight: power }
    )
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal {
        votes-for:     (if vote (+ (get votes-for proposal) power) (get votes-for proposal)),
        votes-against: (if vote (get votes-against proposal) (+ (get votes-against proposal) power))
      })
    )
    (ok true)
  )
)

;; Finalize a proposal after voting window closes
;; If consensus threshold (66%) is met, proposal is approved
(define-public (finalize-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
      (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
    )
    (asserts! (is-eq (get status proposal) u0) ERR-PROPOSAL-CLOSED)
    (asserts! (> block-height (get closes-at proposal)) ERR-PROPOSAL-CLOSED)
    (let
      (
        (approved (and
          (> total-votes u0)
          (>= (* (get votes-for proposal) u100) (* total-votes CONSENSUS-THRESHOLD))
        ))
        (new-status (if approved u1 u2))
      )
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal { status: new-status })
      )
      (ok approved)
    )
  )
)

;; Execute an approved proposal: update pool allocation
;; Gradual migration: only moves 20% of the delta per execution (temporal weighting)
(define-public (execute-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
      (pool (unwrap! (map-get? yield-pools { pool-id: (get pool-id proposal) }) ERR-POOL-NOT-FOUND))
    )
    (asserts! (is-eq (get status proposal) u1) ERR-BELOW-CONSENSUS-THRESHOLD)
    (asserts! (not (get executed proposal)) ERR-PROPOSAL-CLOSED)
    (asserts! (not (var-get rebalance-locked)) ERR-NOT-AUTHORIZED)
    (let
      (
        (current-alloc (get current-allocation pool))
        (target-alloc (get proposed-allocation-bps proposal))
        ;; Gradual migration: move 20% of delta each execution
        (delta (if (> target-alloc current-alloc)
          (/ (- target-alloc current-alloc) u5)
          (/ (- current-alloc target-alloc) u5)
        ))
        (new-alloc (if (> target-alloc current-alloc)
          (+ current-alloc delta)
          (- current-alloc delta)
        ))
      )
      (var-set rebalance-locked true)
      (map-set yield-pools
        { pool-id: (get pool-id proposal) }
        (merge pool {
          current-allocation: new-alloc,
          last-rebalance: block-height,
          total-migrated: (+ (get total-migrated pool) delta)
        })
      )
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal { executed: true })
      )
      (var-set rebalance-locked false)
      (ok new-alloc)
    )
  )
)

;; ============================================================
;; ADAPTIVE RISK SCORING
;; ============================================================

;; Strategy Nodes can update community risk scores for pools (must be approved node)
;; New score is averaged with existing score (community validation dampening)
(define-public (update-pool-risk-score (pool-id uint) (new-score uint))
  (let
    (
      (caller tx-sender)
      (pool (unwrap! (map-get? yield-pools { pool-id: pool-id }) ERR-POOL-NOT-FOUND))
      (node-data (unwrap! (map-get? strategy-nodes { node: caller }) ERR-NOT-REGISTERED))
      (current-risk (get risk-score pool))
    )
    (asserts! (get active node-data) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-score u100) ERR-INVALID-ALLOCATION)
    ;; Weight new score by node accuracy vs baseline 50
    (let
      (
        (node-weight (get accuracy-score node-data))
        (weighted-score (/ (+ (* new-score node-weight) (* current-risk (- u100 node-weight))) u100))
      )
      (map-set yield-pools
        { pool-id: pool-id }
        (merge pool { risk-score: weighted-score })
      )
      (ok weighted-score)
    )
  )
)

;; Owner updates a node's accuracy score after a consensus round resolves
(define-public (update-node-accuracy (node principal) (correct bool))
  (let
    (
      (node-data (unwrap! (map-get? strategy-nodes { node: node }) ERR-NOT-REGISTERED))
      (made (+ (get predictions-made node-data) u1))
      (correct-count (if correct (+ (get predictions-correct node-data) u1) (get predictions-correct node-data)))
      ;; Accuracy = (correct / made) * 100, clamped 1-99
      (raw-accuracy (/ (* correct-count u100) made))
      (clamped-high (if (> raw-accuracy u99) u99 raw-accuracy))
      (new-accuracy (if (< clamped-high u1) u1 clamped-high))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set strategy-nodes
      { node: node }
      (merge node-data {
        predictions-made: made,
        predictions-correct: correct-count,
        accuracy-score: new-accuracy
      })
    )
    (ok new-accuracy)
  )
)

;; ============================================================
;; READ-ONLY VIEWS
;; ============================================================

(define-read-only (get-strategy-node (node principal))
  (map-get? strategy-nodes { node: node })
)

(define-read-only (get-yield-pool (pool-id uint))
  (map-get? yield-pools { pool-id: pool-id })
)

(define-read-only (get-depositor-balance (depositor principal))
  (map-get? depositor-balances { depositor: depositor })
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes-cast { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-total-deposited)
  (var-get total-deposited)
)

(define-read-only (get-node-voting-power (node principal))
  (node-voting-power node)
)

;; Estimate current depositor yield (unclaimed)
(define-read-only (estimate-yield (depositor principal))
  (match (map-get? depositor-balances { depositor: depositor })
    bal
      (let
        (
          (blocks-elapsed (- block-height (get last-claim-block bal)))
          (blended-apy (match (map-get? yield-pools { pool-id: u0 })
            pool (get apy-bps pool)
            u500
          ))
          (q-weight (quadratic-weight (get deposited bal)))
          (base-yield (/
            (* (get deposited bal) blended-apy blocks-elapsed)
            (* u52560 MAX-BPS)
          ))
          (adjusted-yield (/ (* base-yield q-weight) (* QUADRATIC-SCALE u100)))
        )
        (some (+ (get accrued-yield bal) adjusted-yield))
      )
    none
  )
)

(define-read-only (get-proposal-consensus-pct (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal
      (let
        (
          (total (+ (get votes-for proposal) (get votes-against proposal)))
        )
        (if (> total u0)
          (some (/ (* (get votes-for proposal) u100) total))
          (some u0)
        )
      )
    none
  )
)
