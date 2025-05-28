
;; stacking-app_contract
;; A decentralized stacking application that allows users to stack STX tokens
;; and earn rewards

;; constants
;;
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-minimum-not-met (err u105))
(define-constant err-stacking-locked (err u106))
(define-constant err-invalid-duration (err u108))

;; Minimum amount required to stack
(define-constant min-stacking-amount u100000000) ;; 100 STX
;; Reward rate per cycle (in basis points, 100 = 1%)
(define-constant reward-rate u500) ;; 5%

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
    last-reward-cycle: uint
  }
)

;; Global stacking stats
(define-data-var total-stacked uint u0)
(define-data-var total-stackers uint u0)
(define-data-var total-rewards-distributed uint u0)
(define-data-var current-cycle uint u0)

;; private functions
;;

;; Calculate rewards for a stacker based on amount and time
(define-private (calculate-rewards (amount uint) (cycles uint))
  (/ (* amount (* cycles reward-rate)) u10000)
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
        last-reward-cycle: u0
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
    info (>= block-height (get unlock-height info))
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
      
      ;; Clear stacker info
      (map-delete stacker-info { stacker: stacker })
      
      (ok (+ amount rewards))
    )
  )
)

;; Get stacker information
(define-read-only (get-stacker-info (stacker principal))
  (map-get? stacker-info { stacker: stacker })
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

