;; Mental Health Support Network - Peer-to-peer counseling with privacy and reputation systems
;; A decentralized platform for secure mental health support services

;; Error constants
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-EXISTS (err u102))
(define-constant ERR-UNAUTHORIZED (err u103))
(define-constant ERR-INVALID-AMOUNT (err u104))
(define-constant ERR-INVALID-STATUS (err u105))
(define-constant ERR-SELF-INTERACTION (err u106))
(define-constant ERR-INSUFFICIENT-FUNDS (err u107))
(define-constant ERR-INVALID-RATING (err u108))
(define-constant ERR-ALREADY-RATED (err u109))
(define-constant ERR-PAYMENT-FAILED (err u110))

;; Contract constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MIN-SESSION-FEE u1000000) ;; 1 STX
(define-constant MAX-RATING u500) ;; 5.00 stars (in basis points)
(define-constant MIN-RATING u100) ;; 1.00 star (in basis points)

;; Status constants
(define-constant STATUS-PENDING u0)
(define-constant STATUS-ACTIVE u1)
(define-constant STATUS-SUSPENDED u2)
(define-constant STATUS-COMPLETED u3)

;; Session status constants
(define-constant SESSION-REQUESTED u0)
(define-constant SESSION-ACCEPTED u1)
(define-constant SESSION-IN-PROGRESS u2)
(define-constant SESSION-COMPLETED u3)
(define-constant SESSION-CANCELLED u4)

;; Data variables
(define-data-var next-session-id uint u1)
(define-data-var platform-fee-rate uint u5) ;; 5%
(define-data-var total-sessions uint u0)
(define-data-var total-counselors uint u0)

;; Core data maps
(define-map counselors
  principal
  {
    name-hash: (buff 32),
    specializations: (list 5 (string-ascii 50)),
    hourly-rate: uint,
    total-sessions: uint,
    rating-sum: uint,
    rating-count: uint,
    status: uint,
    joined-at: uint,
    is-available: bool
  }
)

(define-map clients
  principal
  {
    joined-at: uint,
    total-sessions: uint,
    total-spent: uint,
    is-active: bool
  }
)

(define-map sessions
  uint
  {
    client: principal,
    counselor: principal,
    fee: uint,
    platform-fee: uint,
    status: uint,
    created-at: uint,
    started-at: (optional uint),
    completed-at: (optional uint),
    duration-minutes: uint,
    notes-hash: (optional (buff 32))
  }
)

(define-map session-payments
  uint
  {
    total-locked: uint,
    counselor-paid: bool,
    platform-paid: bool,
    refunded: bool
  }
)

(define-map session-ratings
  { session-id: uint, rater: principal }
  {
    rating: uint,
    review-hash: (optional (buff 32)),
    created-at: uint
  }
)

(define-map counselor-availability
  { counselor: principal, date: uint }
  {
    slots: (list 24 bool),
    updated-at: uint
  }
)

;; Read-only functions
(define-read-only (get-counselor (counselor principal))
  (map-get? counselors counselor)
)

(define-read-only (get-client (client principal))
  (map-get? clients client)  
)

(define-read-only (get-session (session-id uint))
  (map-get? sessions session-id)
)

(define-read-only (get-session-payment (session-id uint))
  (map-get? session-payments session-id)
)

(define-read-only (get-rating (session-id uint) (rater principal))
  (map-get? session-ratings { session-id: session-id, rater: rater })
)

(define-read-only (get-availability (counselor principal) (date uint))
  (map-get? counselor-availability { counselor: counselor, date: date })
)

(define-read-only (get-platform-stats)
  {
    total-sessions: (var-get total-sessions),
    total-counselors: (var-get total-counselors),
    platform-fee-rate: (var-get platform-fee-rate),
    next-session-id: (var-get next-session-id)
  }
)

(define-read-only (calculate-fees (amount uint))
  (let ((platform-fee (/ (* amount (var-get platform-fee-rate)) u100)))
    {
      session-fee: amount,
      platform-fee: platform-fee,
      total: (+ amount platform-fee)
    }
  )
)

(define-read-only (get-counselor-rating (counselor principal))
  (match (get-counselor counselor)
    counselor-data
    (if (> (get rating-count counselor-data) u0)
      (some (/ (get rating-sum counselor-data) (get rating-count counselor-data)))
      none
    )
    none
  )
)

(define-read-only (is-counselor-verified (counselor principal))
  (match (get-counselor counselor)
    counselor-data (is-eq (get status counselor-data) STATUS-ACTIVE)
    false
  )
)

;; Private helper functions
(define-private (is-owner)
  (is-eq tx-sender CONTRACT-OWNER)
)

(define-private (increment-session-id)
  (let ((current-id (var-get next-session-id)))
    (var-set next-session-id (+ current-id u1))
    current-id
  )
)

;; Public functions

;; Register as counselor (auto-verified)
(define-public (register-counselor 
  (name-hash (buff 32))
  (specializations (list 5 (string-ascii 50)))
  (hourly-rate uint)
)
  (let ((counselor tx-sender))
    (asserts! (is-none (get-counselor counselor)) ERR-ALREADY-EXISTS)
    (asserts! (>= hourly-rate MIN-SESSION-FEE) ERR-INVALID-AMOUNT)
    
    (map-set counselors counselor {
      name-hash: name-hash,
      specializations: specializations, 
      hourly-rate: hourly-rate,
      total-sessions: u0,
      rating-sum: u0,
      rating-count: u0,
      status: STATUS-ACTIVE, ;; Auto-verified upon registration
      joined-at: block-height,
      is-available: true
    })
    
    (var-set total-counselors (+ (var-get total-counselors) u1))
    (ok counselor)
  )
)

;; Register as client
(define-public (register-client)
  (let ((client tx-sender))
    (asserts! (is-none (get-client client)) ERR-ALREADY-EXISTS)
    
    (map-set clients client {
      joined-at: block-height,
      total-sessions: u0,
      total-spent: u0,
      is-active: true
    })
    
    (ok client)
  )
)

;; Set availability
(define-public (set-availability (date uint) (slots (list 24 bool)))
  (let ((counselor tx-sender))
    (asserts! (is-counselor-verified counselor) ERR-UNAUTHORIZED)
    
    (map-set counselor-availability
      { counselor: counselor, date: date }
      {
        slots: slots,
        updated-at: block-height
      }
    )
    (ok true)
  )
)

;; Request session
(define-public (request-session 
  (counselor principal)
  (duration-minutes uint)
  (session-fee uint)
)
  (let 
    (
      (client tx-sender)
      (session-id (increment-session-id))
      (fees (calculate-fees session-fee))
      (total-amount (get total fees))
    )
    
    (asserts! (not (is-eq client counselor)) ERR-SELF-INTERACTION)
    (asserts! (is-counselor-verified counselor) ERR-UNAUTHORIZED)
    (asserts! (>= session-fee MIN-SESSION-FEE) ERR-INVALID-AMOUNT)
    
    ;; Ensure client is registered
    (if (is-none (get-client client))
      (begin
        (try! (register-client))
        true
      )
      true
    )
    
    ;; Lock funds in contract
    (try! (stx-transfer? total-amount client (as-contract tx-sender)))
    
    ;; Create session record
    (map-set sessions session-id {
      client: client,
      counselor: counselor,
      fee: session-fee,
      platform-fee: (get platform-fee fees),
      status: SESSION-REQUESTED,
      created-at: block-height,
      started-at: none,
      completed-at: none,
      duration-minutes: duration-minutes,
      notes-hash: none
    })
    
    ;; Create payment record
    (map-set session-payments session-id {
      total-locked: total-amount,
      counselor-paid: false,
      platform-paid: false,
      refunded: false
    })
    
    (ok session-id)
  )
)

;; Accept session
(define-public (accept-session (session-id uint))
  (match (get-session session-id)
    session-data
    (begin
      (asserts! (is-eq tx-sender (get counselor session-data)) ERR-UNAUTHORIZED)
      (asserts! (is-eq (get status session-data) SESSION-REQUESTED) ERR-INVALID-STATUS)
      
      (map-set sessions session-id
        (merge session-data { status: SESSION-ACCEPTED }))
      (ok true)
    )
    ERR-NOT-FOUND
  )
)

;; Start session
(define-public (start-session (session-id uint))
  (match (get-session session-id)
    session-data
    (begin
      (asserts! (or 
        (is-eq tx-sender (get client session-data))
        (is-eq tx-sender (get counselor session-data))
      ) ERR-UNAUTHORIZED)
      (asserts! (is-eq (get status session-data) SESSION-ACCEPTED) ERR-INVALID-STATUS)
      
      (map-set sessions session-id
        (merge session-data { 
          status: SESSION-IN-PROGRESS,
          started-at: (some block-height)
        }))
      (ok true)
    )
    ERR-NOT-FOUND
  )
)

;; Complete session
(define-public (complete-session (session-id uint) (notes-hash (optional (buff 32))))
  (match (get-session session-id)
    session-data
    (begin
      (asserts! (is-eq tx-sender (get counselor session-data)) ERR-UNAUTHORIZED)
      (asserts! (is-eq (get status session-data) SESSION-IN-PROGRESS) ERR-INVALID-STATUS)
      
      (map-set sessions session-id
        (merge session-data { 
          status: SESSION-COMPLETED,
          completed-at: (some block-height),
          notes-hash: notes-hash
        }))
      
      ;; Update counselor stats
      (match (get-counselor (get counselor session-data))
        counselor-data
        (map-set counselors (get counselor session-data)
          (merge counselor-data { 
            total-sessions: (+ (get total-sessions counselor-data) u1)
          }))
        false
      )
      
      ;; Update client stats  
      (match (get-client (get client session-data))
        client-data
        (map-set clients (get client session-data)
          (merge client-data {
            total-sessions: (+ (get total-sessions client-data) u1),
            total-spent: (+ (get total-spent client-data) (get fee session-data))
          }))
        false
      )
      
      (var-set total-sessions (+ (var-get total-sessions) u1))
      (ok true)
    )
    ERR-NOT-FOUND
  )
)

;; Release payment
(define-public (release-payment (session-id uint))
  (match (get-session session-id)
    session-data
    (match (get-session-payment session-id)
      payment-data
      (begin
        (asserts! (or 
          (is-eq tx-sender (get client session-data))
          (is-owner)
        ) ERR-UNAUTHORIZED)
        (asserts! (is-eq (get status session-data) SESSION-COMPLETED) ERR-INVALID-STATUS)
        (asserts! (not (get counselor-paid payment-data)) ERR-ALREADY-EXISTS)
        
        ;; Pay counselor
        (try! (as-contract (stx-transfer? 
          (get fee session-data)
          tx-sender
          (get counselor session-data)
        )))
        
        ;; Pay platform fee
        (try! (as-contract (stx-transfer? 
          (get platform-fee session-data)
          tx-sender
          CONTRACT-OWNER
        )))
        
        ;; Update payment status
        (map-set session-payments session-id
          (merge payment-data {
            counselor-paid: true,
            platform-paid: true
          }))
        
        (ok true)
      )
      ERR-NOT-FOUND
    )
    ERR-NOT-FOUND
  )
)

;; Rate session
(define-public (rate-session 
  (session-id uint) 
  (rating uint) 
  (review-hash (optional (buff 32)))
)
  (match (get-session session-id)
    session-data
    (begin
      (asserts! (and (>= rating MIN-RATING) (<= rating MAX-RATING)) ERR-INVALID-RATING)
      (asserts! (is-eq (get status session-data) SESSION-COMPLETED) ERR-INVALID-STATUS)
      (asserts! (or 
        (is-eq tx-sender (get client session-data))
        (is-eq tx-sender (get counselor session-data))
      ) ERR-UNAUTHORIZED)
      (asserts! (is-none (get-rating session-id tx-sender)) ERR-ALREADY-RATED)
      
      ;; Store rating
      (map-set session-ratings
        { session-id: session-id, rater: tx-sender }
        {
          rating: rating,
          review-hash: review-hash,
          created-at: block-height
        }
      )
      
      ;; Update counselor rating if client is rating
      (if (is-eq tx-sender (get client session-data))
        (match (get-counselor (get counselor session-data))
          counselor-data
          (map-set counselors (get counselor session-data)
            (merge counselor-data {
              rating-sum: (+ (get rating-sum counselor-data) rating),
              rating-count: (+ (get rating-count counselor-data) u1)
            }))
          false
        )
        true
      )
      
      (ok true)
    )
    ERR-NOT-FOUND
  )
)

;; Cancel session
(define-public (cancel-session (session-id uint))
  (match (get-session session-id)
    session-data
    (match (get-session-payment session-id)
      payment-data
      (begin
        (asserts! (or 
          (is-eq tx-sender (get client session-data))
          (is-eq tx-sender (get counselor session-data))
        ) ERR-UNAUTHORIZED)
        (asserts! (< (get status session-data) SESSION-IN-PROGRESS) ERR-INVALID-STATUS)
        (asserts! (not (get refunded payment-data)) ERR-ALREADY-EXISTS)
        
        ;; Refund client
        (try! (as-contract (stx-transfer? 
          (get total-locked payment-data)
          tx-sender
          (get client session-data)
        )))
        
        ;; Update records
        (map-set sessions session-id
          (merge session-data { status: SESSION-CANCELLED }))
        
        (map-set session-payments session-id
          (merge payment-data { refunded: true }))
        
        (ok true)
      )
      ERR-NOT-FOUND
    )
    ERR-NOT-FOUND
  )
)

;; Update platform fee rate (admin only)
(define-public (set-platform-fee-rate (new-rate uint))
  (begin
    (asserts! (is-owner) (err u100))
    (asserts! (<= new-rate u20) (err u104))
    (var-set platform-fee-rate new-rate)
    (ok true)
  )
)

;; Suspend counselor (admin only)  
(define-public (suspend-counselor (counselor principal))
  (begin
    (asserts! (is-owner) (err u100))
    (match (get-counselor counselor)
      counselor-data
      (begin
        (map-set counselors 
          counselor
          (merge counselor-data { status: STATUS-SUSPENDED }))
        (ok true)
      )
      (err u101)
    )
  )
)