(define-constant err-template-not-found (err u300))
(define-constant err-template-exists (err u301))
(define-constant err-not-template-creator (err u302))
(define-constant err-template-inactive (err u303))
(define-constant err-unauthorized (err u304))

(define-data-var next-template-id uint u1)
(define-data-var template-creation-fee uint u1000000)

(define-map challenge-templates
    uint
    {
        creator: principal,
        name: (string-utf8 64),
        description: (string-utf8 256),
        goal-type: (string-utf8 32),
        goal-amount: uint,
        duration-days: uint,
        suggested-entry-fee: uint,
        suggested-max-participants: uint,
        category: (string-utf8 32),
        difficulty: (string-utf8 16),
        is-active: bool,
        usage-count: uint,
        created-at: uint
    }
)

(define-map template-ratings
    { template-id: uint, rater: principal }
    { rating: uint, timestamp: uint }
)

(define-map template-stats
    uint
    {
        total-ratings: uint,
        average-rating: uint,
        total-challenges-created: uint
    }
)

(define-public (create-template
    (name (string-utf8 64))
    (description (string-utf8 256))
    (goal-type (string-utf8 32))
    (goal-amount uint)
    (duration-days uint)
    (suggested-entry-fee uint)
    (suggested-max-participants uint)
    (category (string-utf8 32))
    (difficulty (string-utf8 16)))
    (let
        (
            (template-id (var-get next-template-id))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
        )
        (asserts! (> goal-amount u0) (err u400))
        (asserts! (> duration-days u0) (err u401))
        (asserts! (> suggested-max-participants u0) (err u402))
        
        (try! (stx-transfer? (var-get template-creation-fee) tx-sender (as-contract tx-sender)))
        
        (map-set challenge-templates template-id
            {
                creator: tx-sender,
                name: name,
                description: description,
                goal-type: goal-type,
                goal-amount: goal-amount,
                duration-days: duration-days,
                suggested-entry-fee: suggested-entry-fee,
                suggested-max-participants: suggested-max-participants,
                category: category,
                difficulty: difficulty,
                is-active: true,
                usage-count: u0,
                created-at: current-time
            }
        )
        
        (map-set template-stats template-id
            {
                total-ratings: u0,
                average-rating: u0,
                total-challenges-created: u0
            }
        )
        
        (var-set next-template-id (+ template-id u1))
        (ok template-id)
    )
)

(define-public (use-template (template-id uint))
    (let
        (
            (template (unwrap! (map-get? challenge-templates template-id) err-template-not-found))
            (stats (unwrap! (map-get? template-stats template-id) err-template-not-found))
        )
        (asserts! (get is-active template) err-template-inactive)
        
        (map-set challenge-templates template-id
            (merge template { usage-count: (+ (get usage-count template) u1) })
        )
        
        (map-set template-stats template-id
            (merge stats { total-challenges-created: (+ (get total-challenges-created stats) u1) })
        )
        
        (ok template)
    )
)

(define-public (rate-template (template-id uint) (rating uint))
    (let
        (
            (template (unwrap! (map-get? challenge-templates template-id) err-template-not-found))
            (rating-key { template-id: template-id, rater: tx-sender })
            (existing-rating (map-get? template-ratings rating-key))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
            (stats (unwrap! (map-get? template-stats template-id) err-template-not-found))
        )
        (asserts! (get is-active template) err-template-inactive)
        (asserts! (and (>= rating u1) (<= rating u5)) (err u403))
        
        (map-set template-ratings rating-key
            { rating: rating, timestamp: current-time }
        )
        
        (let
            (
                (new-total-ratings (if (is-some existing-rating) 
                    (get total-ratings stats) 
                    (+ (get total-ratings stats) u1)))
                (current-average (get average-rating stats))
                (new-average (if (is-some existing-rating)
                    current-average
                    (/ (+ (* current-average (get total-ratings stats)) rating) new-total-ratings)))
            )
            (map-set template-stats template-id
                (merge stats 
                    { 
                        total-ratings: new-total-ratings,
                        average-rating: new-average
                    }
                )
            )
        )
        
        (ok true)
    )
)

(define-public (toggle-template-status (template-id uint))
    (let
        (
            (template (unwrap! (map-get? challenge-templates template-id) err-template-not-found))
        )
        (asserts! (is-eq tx-sender (get creator template)) err-not-template-creator)
        
        (map-set challenge-templates template-id
            (merge template { is-active: (not (get is-active template)) })
        )
        
        (ok true)
    )
)

(define-public (set-template-creation-fee (fee uint))
    (begin
        (asserts! (is-eq tx-sender (as-contract tx-sender)) err-unauthorized)
        (var-set template-creation-fee fee)
        (ok true)
    )
)

(define-read-only (get-template (template-id uint))
    (map-get? challenge-templates template-id)
)

(define-read-only (get-template-rating (template-id uint) (rater principal))
    (map-get? template-ratings { template-id: template-id, rater: rater })
)

(define-read-only (get-template-stats (template-id uint))
    (map-get? template-stats template-id)
)

(define-read-only (get-total-templates)
    (var-get next-template-id)
)

(define-read-only (get-template-creation-fee)
    (var-get template-creation-fee)
)

(define-read-only (get-template-by-category (template-id uint) (category (string-utf8 32)))
    (match (map-get? challenge-templates template-id)
        template-data (if (is-eq (get category template-data) category)
            (some template-data)
            none)
        none
    )
)
