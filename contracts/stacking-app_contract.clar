
;; stacking-app_contract
;; A decentralized stacking application that allows users to stack STX tokens,
;; track rewards, and manage delegation with advanced features.

;; constants
;;
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-stacked (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-minimum-not-met (err u105))
(define-constant err-stacking-locked (err u106))
(define-constant err-cooldown-period (err u107))
(define-constant err-invalid-duration (err u108))
(define-constant err-pool-full (err u109))
(define-constant err-already-delegated (err u110))

;; Minimum amount required to stack
(define-constant min-stacking-amount u100000000) ;; 100 STX
;; Maximum number of stacking pools
(define-constant max-pools u10)
;; Reward rate per cycle (in basis points, 100 = 1%)
(define-constant reward-rate u500) ;; 5%
;; Cooldown period after unstacking (in blocks)
(define-constant cooldown-period u144) ;; ~1 day

;; data maps and vars
;;

;; Track user stacking information
(define-map stacker-info
  { stacker: principal }
  {
    amount: uint,
    lock-period: uint,
    unlock-height: uint,
    rewards-claimed: uint,
    last-reward-cycle: uint,
    delegated: bool,
    delegated-to: (optional principal)
  }
)

;; Track stacking pools
(define-map stacking-pools
  { pool-id: uint }
  {
    operator: principal,
    total-stacked: uint,
    members-count: uint,
    active: bool,
    commission-rate: uint,
    min-duration: uint
  }
)

;; Track pool membership
(define-map pool-members
  { pool-id: uint, member: principal }
  { amount: uint, joined-height: uint }
)

;; Track user cooldown periods
(define-map cooldown-tracker
  { stacker: principal }
  { until-height: uint }
)

;; Global stacking stats
(define-data-var total-stacked uint u0)
(define-data-var total-stackers uint u0)
(define-data-var total-rewards-distributed uint u0)
(define-data-var current-cycle uint u0)
(define-data-var next-pool-id uint u1)

;; private functions
;;

;; Calculate rewards for a stacker based on amount and time
(define-private (calculate-rewards (amount uint) (cycles uint))
  (/ (* amount (* cycles reward-rate)) u10000)
)

;; Check if a stacker is in cooldown period
(define-private (is-in-cooldown (stacker principal))
  (match (map-get? cooldown-tracker { stacker: stacker })
    cooldown-data (< block-height (get until-height cooldown-data))
    false
  )
)

;; Update stacker info when stacking
(define-private (update-stacker-info (stacker principal) (amount uint) (lock-period uint))
  (let (
    (current-info (default-to 
      { 
        amount: u0, 
        lock-period: u0, 
        unlock-height: u0, 
        rewards-claimed: u0, 
        last-reward-cycle: u0,
        delegated: false,
        delegated-to: none
      } 
      (map-get? stacker-info { stacker: stacker })))
    (new-amount (+ (get amount current-info) amount))
    (new-unlock-height (+ block-height lock-period))
  )
    (map-set stacker-info
      { stacker: stacker }
      (merge current-info {
        amount: new-amount,
        lock-period: lock-period,
        unlock-height: new-unlock-height
      })
    )
  )
)

;; Check if a stacker can unstack
(define-private (can-unstack (stacker principal))
  (match (map-get? stacker-info { stacker: stacker })
    info (and 
           (>= block-height (get unlock-height info))
           (not (is-in-cooldown stacker)))
    false
  )
)

;; public functions
;;

;; Stack STX tokens for a specified lock period
(define-public (stack-stx (amount uint) (lock-period uint))
  (let (
    (stacker tx-sender)
  )
    (asserts! (>= amount min-stacking-amount) err-minimum-not-met)
    (asserts! (>= lock-period u100) err-invalid-duration)
    (asserts! (not (is-in-cooldown stacker)) err-cooldown-period)
    
    ;; Transfer STX to contract
    (try! (stx-transfer? amount stacker (as-contract tx-sender)))
    
    ;; Update stacker info
    (update-stacker-info stacker amount lock-period)
    
    ;; Update global stats
    (var-set total-stacked (+ (var-get total-stacked) amount))
    
    ;; Increment total stackers if new stacker
    (if (is-none (map-get? stacker-info { stacker: stacker }))
      (var-set total-stackers (+ (var-get total-stackers) u1))
      true
    )
    
    (ok amount)
  )
)

;; Unstack STX tokens after lock period
(define-public (unstack-stx)
  (let (
    (stacker tx-sender)
    (stacker-data (unwrap! (map-get? stacker-info { stacker: stacker }) err-not-found))
  )
    (asserts! (can-unstack stacker) err-stacking-locked)
    (asserts! (> (get amount stacker-data) u0) err-not-found)
    
    ;; Calculate rewards
    (let (
      (amount (get amount stacker-data))
      (cycles-stacked (/ (- block-height (- (get unlock-height stacker-data) (get lock-period stacker-data))) u144))
      (rewards (calculate-rewards amount cycles-stacked))
    )
      ;; Transfer STX back to stacker
      (try! (as-contract (stx-transfer? amount (as-contract tx-sender) stacker)))
      
      ;; Transfer rewards
      (try! (as-contract (stx-transfer? rewards (as-contract tx-sender) stacker)))
      
      ;; Update global stats
      (var-set total-stacked (- (var-get total-stacked) amount))
      (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) rewards))
      
      ;; Set cooldown period
      (map-set cooldown-tracker
        { stacker: stacker }
        { until-height: (+ block-height cooldown-period) }
      )
      
      ;; Clear stacker info
      (map-delete stacker-info { stacker: stacker })
      
      (ok (+ amount rewards))
    )
  )
)

;; Create a new stacking pool
(define-public (create-pool (commission-rate uint) (min-duration uint))
  (let (
    (pool-id (var-get next-pool-id))
    (operator tx-sender)
  )
    (asserts! (< pool-id max-pools) err-pool-full)
    (asserts! (<= commission-rate u3000) err-unauthorized) ;; Max 30% commission
    
    (map-set stacking-pools
      { pool-id: pool-id }
      {
        operator: operator,
        total-stacked: u0,
        members-count: u0,
        active: true,
        commission-rate: commission-rate,
        min-duration: min-duration
      }
    )
    
    (var-set next-pool-id (+ pool-id u1))
    (ok pool-id)
  )
)

;; Join a stacking pool
(define-public (join-pool (pool-id uint) (amount uint))
  (let (
    (stacker tx-sender)
    (pool (unwrap! (map-get? stacking-pools { pool-id: pool-id }) err-not-found))
  )
    (asserts! (get active pool) err-unauthorized)
    (asserts! (>= amount min-stacking-amount) err-minimum-not-met)
    (asserts! (not (is-in-cooldown stacker)) err-cooldown-period)
    
    ;; Transfer STX to contract
    (try! (stx-transfer? amount stacker (as-contract tx-sender)))
    
    ;; Update pool stats
    (map-set stacking-pools
      { pool-id: pool-id }
      (merge pool {
        total-stacked: (+ (get total-stacked pool) amount),
        members-count: (+ (get members-count pool) u1)
      })
    )
    
    ;; Add member to pool
    (map-set pool-members
      { pool-id: pool-id, member: stacker }
      { amount: amount, joined-height: block-height }
    )
    
    ;; Update global stats
    (var-set total-stacked (+ (var-get total-stacked) amount))
    
    (ok amount)
  )
)

;; Get stacker information
(define-read-only (get-stacker-info (stacker principal))
  (map-get? stacker-info { stacker: stacker })
)

;; Get pool information
(define-read-only (get-pool-info (pool-id uint))
  (map-get? stacking-pools { pool-id: pool-id })
)

;; Get global stacking stats
(define-read-only (get-stacking-stats)
  {
    total-stacked: (var-get total-stacked),
    total-stackers: (var-get total-stackers),
    total-rewards-distributed: (var-get total-rewards-distributed),
    current-cycle: (var-get current-cycle)
  }
)

