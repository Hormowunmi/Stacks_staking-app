
;; stacking-app_contract
;; A decentralized stacking application that allows users to stack STX tokens

;; constants
;;
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-minimum-not-met (err u105))
(define-constant err-invalid-duration (err u108))

;; Minimum amount required to stack
(define-constant min-stacking-amount u100000000) ;; 100 STX

;; data maps and vars
;;

;; Track user stacking information
(define-map stacker-info
  { stacker: principal }
  {
    amount: uint,
    lock-period: uint,
    unlock-height: uint
  }
)

;; Global stacking stats
(define-data-var total-stacked uint u0)
(define-data-var total-stackers uint u0)

;; private functions
;;

;; Update stacker info when stacking
(define-private (update-stacker-info (stacker principal) (amount uint) (lock-period uint))
  (let (
    (current-info (default-to 
      { 
        amount: u0, 
        lock-period: u0, 
        unlock-height: u0
      } 
      (map-get? stacker-info { stacker: stacker })))
    (new-amount (+ (get amount current-info) amount))
    (new-unlock-height (+ block-height lock-period))
  )
    (map-set stacker-info
      { stacker: stacker }
      {
        amount: new-amount,
        lock-period: lock-period,
        unlock-height: new-unlock-height
      }
    )
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

;; Get stacker information
(define-read-only (get-stacker-info (stacker principal))
  (map-get? stacker-info { stacker: stacker })
)

;; Get global stacking stats
(define-read-only (get-stacking-stats)
  {
    total-stacked: (var-get total-stacked),
    total-stackers: (var-get total-stackers)
  }
)

