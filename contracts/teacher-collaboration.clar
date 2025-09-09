;; Educational Resource Sharing Smart Contract
;; A teacher collaboration platform for lesson plan sharing, material exchange, and peer review

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-EXISTS (err u102))
(define-constant ERR-INVALID-INPUT (err u103))
(define-constant ERR-REVIEW-EXISTS (err u104))

;; Data Variables
(define-data-var next-resource-id uint u1)
(define-data-var next-review-id uint u1)

;; Data Maps
(define-map resources
  { resource-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    category: (string-ascii 50),
    description: (string-ascii 500),
    content-hash: (string-ascii 64),
    created-at: uint,
    rating-sum: uint,
    rating-count: uint,
    is-public: bool
  }
)

(define-map reviews
  { review-id: uint }
  {
    resource-id: uint,
    reviewer: principal,
    rating: uint,
    comment: (string-ascii 300),
    created-at: uint
  }
)

(define-map user-profiles
  { user: principal }
  {
    name: (string-ascii 50),
    bio: (string-ascii 200),
    subject-areas: (string-ascii 100),
    total-resources: uint,
    total-reviews: uint
  }
)

(define-map resource-access
  { resource-id: uint, user: principal }
  { granted-at: uint }
)

(define-map user-resource-reviews
  { resource-id: uint, reviewer: principal }
  { review-id: uint }
)

;; Public Functions

;; Create or update user profile
(define-public (create-profile (name (string-ascii 50)) (bio (string-ascii 200)) (subject-areas (string-ascii 100)))
  (begin
    (asserts! (> (len name) u0) ERR-INVALID-INPUT)
    (ok (map-set user-profiles
      { user: tx-sender }
      {
        name: name,
        bio: bio,
        subject-areas: subject-areas,
        total-resources: u0,
        total-reviews: u0
      }
    ))
  )
)

;; Share a new educational resource
(define-public (share-resource 
  (title (string-ascii 100)) 
  (category (string-ascii 50))
  (description (string-ascii 500))
  (content-hash (string-ascii 64))
  (is-public bool)
)
  (let (
    (resource-id (var-get next-resource-id))
    (current-block-height stacks-block-height)
  )
    (asserts! (> (len title) u0) ERR-INVALID-INPUT)
    (asserts! (> (len content-hash) u0) ERR-INVALID-INPUT)
    
    ;; Create the resource
    (map-set resources
      { resource-id: resource-id }
      {
        creator: tx-sender,
        title: title,
        category: category,
        description: description,
        content-hash: content-hash,
        created-at: current-block-height,
        rating-sum: u0,
        rating-count: u0,
        is-public: is-public
      }
    )
    
    ;; Update user profile resource count
    (match (map-get? user-profiles { user: tx-sender })
      profile (map-set user-profiles
        { user: tx-sender }
        (merge profile { total-resources: (+ (get total-resources profile) u1) })
      )
      ;; Create default profile if none exists
      (map-set user-profiles
        { user: tx-sender }
        {
          name: "Teacher",
          bio: "",
          subject-areas: "",
          total-resources: u1,
          total-reviews: u0
        }
      )
    )
    
    ;; Increment next resource ID
    (var-set next-resource-id (+ resource-id u1))
    (ok resource-id)
  )
)

;; Submit a peer review for a resource
(define-public (submit-review (resource-id uint) (rating uint) (comment (string-ascii 300)))
  (let (
    (review-id (var-get next-review-id))
    (current-block-height stacks-block-height)
  )
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-INPUT)
    (asserts! (is-some (map-get? resources { resource-id: resource-id })) ERR-NOT-FOUND)
    
    ;; Check if user already reviewed this resource
    (asserts! (is-none (map-get? user-resource-reviews { resource-id: resource-id, reviewer: tx-sender })) ERR-REVIEW-EXISTS)
    
    ;; Create the review
    (map-set reviews
      { review-id: review-id }
      {
        resource-id: resource-id,
        reviewer: tx-sender,
        rating: rating,
        comment: comment,
        created-at: current-block-height
      }
    )
    
    ;; Track user-resource review relationship
    (map-set user-resource-reviews
      { resource-id: resource-id, reviewer: tx-sender }
      { review-id: review-id }
    )
    
    ;; Update resource rating
    (match (map-get? resources { resource-id: resource-id })
      resource (map-set resources
        { resource-id: resource-id }
        (merge resource {
          rating-sum: (+ (get rating-sum resource) rating),
          rating-count: (+ (get rating-count resource) u1)
        })
      )
      false
    )
    
    ;; Update reviewer profile
    (match (map-get? user-profiles { user: tx-sender })
      profile (map-set user-profiles
        { user: tx-sender }
        (merge profile { total-reviews: (+ (get total-reviews profile) u1) })
      )
      ;; Create default profile if none exists
      (map-set user-profiles
        { user: tx-sender }
        {
          name: "Teacher",
          bio: "",
          subject-areas: "",
          total-resources: u0,
          total-reviews: u1
        }
      )
    )
    
    ;; Increment next review ID
    (var-set next-review-id (+ review-id u1))
    (ok review-id)
  )
)

;; Grant access to a private resource
(define-public (grant-resource-access (resource-id uint) (user principal))
  (let (
    (resource (unwrap! (map-get? resources { resource-id: resource-id }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get creator resource)) ERR-UNAUTHORIZED)
    (ok (map-set resource-access
      { resource-id: resource-id, user: user }
      { granted-at: stacks-block-height }
    ))
  )
)

;; Read-only functions

;; Get resource details
(define-read-only (get-resource (resource-id uint))
  (map-get? resources { resource-id: resource-id })
)

;; Get user profile
(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles { user: user })
)

;; Get review details
(define-read-only (get-review (review-id uint))
  (map-get? reviews { review-id: review-id })
)

;; Check if user has access to a resource
(define-read-only (has-resource-access (resource-id uint) (user principal))
  (let (
    (resource (map-get? resources { resource-id: resource-id }))
  )
    (match resource
      res (or
        ;; Public resource
        (get is-public res)
        ;; Resource creator
        (is-eq user (get creator res))
        ;; Granted access
        (is-some (map-get? resource-access { resource-id: resource-id, user: user }))
      )
      false
    )
  )
)

;; Get resource average rating
(define-read-only (get-resource-rating (resource-id uint))
  (match (map-get? resources { resource-id: resource-id })
    resource (if (> (get rating-count resource) u0)
      (some (/ (get rating-sum resource) (get rating-count resource)))
      none
    )
    none
  )
)

;; Get total number of resources
(define-read-only (get-total-resources)
  (- (var-get next-resource-id) u1)
)

;; Get total number of reviews
(define-read-only (get-total-reviews)
  (- (var-get next-review-id) u1)
)

;; Helper function to get reviews for a resource (simplified)
(define-read-only (get-resource-reviews (resource-id uint))
  (list)
)


;; title: teacher-collaboration
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

