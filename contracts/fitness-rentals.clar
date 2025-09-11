;; Fitness Equipment & Facility Rentals Contract
;; Rent fitness equipment and book facilities using fitness tokens

(define-constant ERR_UNAUTHORIZED (err u500))
(define-constant ERR_ITEM_NOT_FOUND (err u501))
(define-constant ERR_ITEM_UNAVAILABLE (err u502))
(define-constant ERR_INSUFFICIENT_TOKENS (err u503))
(define-constant ERR_RENTAL_NOT_FOUND (err u504))

(define-data-var next-item-id uint u1)
(define-data-var next-rental-id uint u1)

(define-map rental-items uint {
    owner: principal, name: (string-utf8 64), category: (string-utf8 32),
    price-per-hour: uint, location: (string-utf8 64), available: bool,
    rating-sum: uint, rating-count: uint
})

(define-map rentals uint {
    renter: principal, item-id: uint, start-time: uint, end-time: uint,
    total-cost: uint, is-active: bool
})

(define-map user-stats principal { total-rentals: uint, total-spent: uint })

(define-map reviews { rental-id: uint, reviewer: principal } {
    rating: uint, comment: (string-utf8 128)
})

(define-public (list-item (name (string-utf8 64)) (category (string-utf8 32))
    (price-per-hour uint) (location (string-utf8 64)))
    (let ((item-id (var-get next-item-id)))
        (asserts! (> price-per-hour u0) (err u400))
        (map-set rental-items item-id {
            owner: tx-sender, name: name, category: category,
            price-per-hour: price-per-hour, location: location, available: true,
            rating-sum: u0, rating-count: u0
        })
        (var-set next-item-id (+ item-id u1))
        (ok item-id)
    )
)

(define-public (rent-item (item-id uint) (duration-hours uint))
    (let (
        (item (unwrap! (map-get? rental-items item-id) ERR_ITEM_NOT_FOUND))
        (rental-id (var-get next-rental-id))
        (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
        (total-cost (* (get price-per-hour item) duration-hours))
        (user-stats-data (default-to { total-rentals: u0, total-spent: u0 }
                                     (map-get? user-stats tx-sender)))
    )
        (asserts! (get available item) ERR_ITEM_UNAVAILABLE)
        (asserts! (> duration-hours u0) (err u400))
        (asserts! (not (is-eq tx-sender (get owner item))) (err u401))
        ;; Note: Token balance check and transfer would be handled by fitness-mining contract
        ;; For now, assume the user has sufficient tokens
        
        ;; Create rental record
        (map-set rentals rental-id {
            renter: tx-sender, item-id: item-id, start-time: current-time,
            end-time: (+ current-time (* duration-hours u3600)),
            total-cost: total-cost, is-active: true
        })
        
        ;; Update item and user stats
        (map-set rental-items item-id (merge item { available: false }))
        (map-set user-stats tx-sender {
            total-rentals: (+ (get total-rentals user-stats-data) u1),
            total-spent: (+ (get total-spent user-stats-data) total-cost)
        })
        
        (var-set next-rental-id (+ rental-id u1))
        (ok rental-id)
    )
)

(define-public (end-rental (rental-id uint))
    (let (
        (rental (unwrap! (map-get? rentals rental-id) ERR_RENTAL_NOT_FOUND))
        (item (unwrap! (map-get? rental-items (get item-id rental)) ERR_ITEM_NOT_FOUND))
        (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
        (asserts! (get is-active rental) (err u400))
        (asserts! (or (is-eq tx-sender (get renter rental))
                      (is-eq tx-sender (get owner item))
                      (> current-time (get end-time rental))) ERR_UNAUTHORIZED)
        
        (map-set rentals rental-id (merge rental { is-active: false }))
        (map-set rental-items (get item-id rental) (merge item { available: true }))
        (ok true)
    )
)

(define-public (review-rental (rental-id uint) (rating uint) (comment (string-utf8 128)))
    (let (
        (rental (unwrap! (map-get? rentals rental-id) ERR_RENTAL_NOT_FOUND))
        (item (unwrap! (map-get? rental-items (get item-id rental)) ERR_ITEM_NOT_FOUND))
    )
        (asserts! (is-eq tx-sender (get renter rental)) ERR_UNAUTHORIZED)
        (asserts! (not (get is-active rental)) (err u400))
        (asserts! (and (>= rating u1) (<= rating u5)) (err u401))
        
        (map-set reviews { rental-id: rental-id, reviewer: tx-sender } 
            { rating: rating, comment: comment })
        
        (map-set rental-items (get item-id rental)
            (merge item {
                rating-sum: (+ (get rating-sum item) rating),
                rating-count: (+ (get rating-count item) u1)
            }))
        (ok true)
    )
)

(define-public (toggle-item-availability (item-id uint))
    (let ((item (unwrap! (map-get? rental-items item-id) ERR_ITEM_NOT_FOUND)))
        (asserts! (is-eq tx-sender (get owner item)) ERR_UNAUTHORIZED)
        (map-set rental-items item-id (merge item { available: (not (get available item)) }))
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-item (item-id uint))
    (map-get? rental-items item-id))

(define-read-only (get-rental (rental-id uint))
    (map-get? rentals rental-id))

(define-read-only (get-user-stats (user principal))
    (map-get? user-stats user))

(define-read-only (get-review (rental-id uint) (reviewer principal))
    (map-get? reviews { rental-id: rental-id, reviewer: reviewer }))

(define-read-only (get-item-average-rating (item-id uint))
    (match (map-get? rental-items item-id)
        item-data (if (> (get rating-count item-data) u0)
                      (/ (get rating-sum item-data) (get rating-count item-data))
                      u0)
        u0))

(define-read-only (get-total-items)
    (var-get next-item-id))

(define-read-only (is-item-available (item-id uint))
    (match (map-get? rental-items item-id)
        item-data (get available item-data)
        false))

;; Platform integration functions
(define-read-only (get-items-by-category (category (string-utf8 32)))
    ;; Returns boolean indicating if any items exist in this category
    ;; Full implementation would require more complex data structures
    (> (var-get next-item-id) u1))

(define-read-only (get-rental-cost (item-id uint) (duration-hours uint))
    (match (map-get? rental-items item-id)
        item-data (* (get price-per-hour item-data) duration-hours)
        u0))

(define-read-only (can-user-rent (user principal) (item-id uint) (duration-hours uint))
    (match (map-get? rental-items item-id)
        item-data (and (get available item-data)
                       (not (is-eq user (get owner item-data))))
        false))

;; User reputation score based on rental history  
(define-read-only (get-user-reputation-score (user principal))
    (match (map-get? user-stats user)
        stats (if (> (get total-rentals stats) u0)
                  (/ (get total-spent stats) (get total-rentals stats))
                  u0)
        u0))

;; Equipment owner earnings tracker
(define-read-only (get-owner-potential-earnings (owner principal) (item-id uint))
    (match (map-get? rental-items item-id)
        item-data (if (is-eq owner (get owner item-data))
                      (get price-per-hour item-data)
                      u0)
        u0))
