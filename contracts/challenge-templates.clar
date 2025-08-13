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

;; Dynamic Challenge Pricing & Demand System
;; Tracks demand patterns and adjusts pricing based on popularity

(define-data-var base-multiplier uint u100) ;; Base pricing multiplier (100 = 1.0x)
(define-data-var surge-threshold uint u10) ;; Joins per hour to trigger surge pricing
(define-data-var max-surge-multiplier uint u300) ;; Maximum surge multiplier (300 = 3.0x)
(define-data-var early-adopter-discount uint u80) ;; Early adopter discount (80 = 0.8x)
(define-data-var trending-window uint u604800) ;; 7 days in seconds for trending calculation

;; Track template demand metrics
(define-map template-demand-metrics
    uint
    {
        total-joins-24h: uint,
        total-joins-7d: uint,
        total-joins-all-time: uint,
        last-join-timestamp: uint,
        current-demand-score: uint,
        is-trending: bool,
        surge-active: bool,
        current-multiplier: uint
    }
)

;; Track hourly join patterns for surge pricing
(define-map hourly-join-tracking
    { template-id: uint, hour-slot: uint }
    uint
)

;; Track category-wide demand
(define-map category-demand
    (string-utf8 32)
    {
        total-templates: uint,
        active-templates: uint,
        total-joins-24h: uint,
        average-demand-score: uint,
        is-hot-category: bool
    }
)

(define-public (track-template-usage (template-id uint))
    (let
        (
            (template (unwrap! (map-get? challenge-templates template-id) err-template-not-found))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
            (current-hour (/ current-time u3600))
            (current-metrics (default-to 
                {
                    total-joins-24h: u0,
                    total-joins-7d: u0,
                    total-joins-all-time: u0,
                    last-join-timestamp: u0,
                    current-demand-score: u0,
                    is-trending: false,
                    surge-active: false,
                    current-multiplier: (var-get base-multiplier)
                }
                (map-get? template-demand-metrics template-id)
            ))
            (hour-key { template-id: template-id, hour-slot: current-hour })
            (current-hour-joins (default-to u0 (map-get? hourly-join-tracking hour-key)))
        )
        
        ;; Update hourly tracking
        (map-set hourly-join-tracking hour-key (+ current-hour-joins u1))
        
        ;; Calculate new metrics
        (let
            (
                (new-joins-24h (+ (get total-joins-24h current-metrics) u1))
                (new-joins-7d (+ (get total-joins-7d current-metrics) u1))
                (new-joins-all-time (+ (get total-joins-all-time current-metrics) u1))
                (joins-per-hour (+ current-hour-joins u1))
                (surge-triggered (>= joins-per-hour (var-get surge-threshold)))
                (demand-score (calculate-demand-score template-id new-joins-24h new-joins-7d))
                (new-multiplier (calculate-pricing-multiplier demand-score surge-triggered))
            )
            
            ;; Update template metrics
            (map-set template-demand-metrics template-id
                {
                    total-joins-24h: new-joins-24h,
                    total-joins-7d: new-joins-7d,
                    total-joins-all-time: new-joins-all-time,
                    last-join-timestamp: current-time,
                    current-demand-score: demand-score,
                    is-trending: (> demand-score u75),
                    surge-active: surge-triggered,
                    current-multiplier: new-multiplier
                }
            )
            
            (ok new-multiplier)
        )
    )
)

(define-private (calculate-demand-score (template-id uint) (joins-24h uint) (joins-7d uint))
    (let
        (
            (velocity-score (* joins-24h u10)) ;; Recent activity weighted heavily
            (consistency-score (/ joins-7d u7)) ;; Average daily joins over week
            (popularity-base (if (< joins-24h u50) joins-24h u50)) ;; Cap base popularity
            (total-score (+ velocity-score consistency-score popularity-base))
        )
        (if (< total-score u100) total-score u100)
    )
)

(define-private (calculate-pricing-multiplier (demand-score uint) (surge-active bool))
    (let
        (
            (base-mult (var-get base-multiplier))
            (surge-mult (if surge-active (var-get max-surge-multiplier) base-mult))
            (demand-mult (+ base-mult (/ (* demand-score u50) u100))) ;; Scale demand impact
            (max-surge (var-get max-surge-multiplier))
        )
        (if surge-active
            surge-mult
            (if (< demand-mult max-surge) demand-mult max-surge)
        )
    )
)

(define-private (update-category-demand (category (string-utf8 32)))
    (let
        (
            (current-category-data (default-to
                {
                    total-templates: u0,
                    active-templates: u0,
                    total-joins-24h: u0,
                    average-demand-score: u0,
                    is-hot-category: false
                }
                (map-get? category-demand category)
            ))
        )
        ;; Simple category update - in production would aggregate across all templates
        (map-set category-demand category
            (merge current-category-data 
                { 
                    total-joins-24h: (+ (get total-joins-24h current-category-data) u1),
                    is-hot-category: (> (get total-joins-24h current-category-data) u20)
                }
            )
        )
        (ok true)
    )
)

(define-public (calculate-dynamic-entry-fee (template-id uint) (base-fee uint))
    (let
        (
            (template (unwrap! (map-get? challenge-templates template-id) err-template-not-found))
            (metrics (default-to
                {
                    total-joins-24h: u0,
                    total-joins-7d: u0,
                    total-joins-all-time: u0,
                    last-join-timestamp: u0,
                    current-demand-score: u0,
                    is-trending: false,
                    surge-active: false,
                    current-multiplier: (var-get base-multiplier)
                }
                (map-get? template-demand-metrics template-id)
            ))
            (multiplier (get current-multiplier metrics))
            (is-early-adopter (< (get total-joins-all-time metrics) u5))
            (final-multiplier (if is-early-adopter
                (var-get early-adopter-discount)
                multiplier))
        )
        (ok (/ (* base-fee final-multiplier) u100))
    )
)

(define-read-only (get-trending-templates)
    (let
        (
            (total-templates (var-get next-template-id))
        )
        (scan-trending-templates u1 total-templates)
    )
)

(define-private (scan-trending-templates (start-id uint) (end-id uint))
    (fold check-if-trending
        (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10)
        { current-id: start-id, end-id: end-id, trending-count: u0 }
    )
)

(define-private (check-if-trending 
    (index uint) 
    (acc { current-id: uint, end-id: uint, trending-count: uint }))
    (let
        (
            (current-id (get current-id acc))
            (metrics (map-get? template-demand-metrics current-id))
        )
        (if (and (< current-id (get end-id acc)) (is-some metrics))
            (let
                (
                    (metrics-data (unwrap-panic metrics))
                    (is-trending (get is-trending metrics-data))
                )
                (merge acc 
                    { 
                        current-id: (+ current-id u1),
                        trending-count: (if is-trending 
                            (+ (get trending-count acc) u1) 
                            (get trending-count acc))
                    }
                )
            )
            acc
        )
    )
)

(define-public (set-pricing-parameters 
    (new-base-multiplier uint)
    (new-surge-threshold uint)
    (new-max-surge uint)
    (new-early-discount uint))
    (begin
        (asserts! (is-eq tx-sender (as-contract tx-sender)) err-unauthorized)
        (asserts! (> new-base-multiplier u0) (err u500))
        (asserts! (> new-surge-threshold u0) (err u501))
        (asserts! (>= new-max-surge new-base-multiplier) (err u502))
        (asserts! (<= new-early-discount new-base-multiplier) (err u503))
        
        (var-set base-multiplier new-base-multiplier)
        (var-set surge-threshold new-surge-threshold)
        (var-set max-surge-multiplier new-max-surge)
        (var-set early-adopter-discount new-early-discount)
        (ok true)
    )
)

;; Read-only functions for demand analytics
(define-read-only (get-template-demand-metrics (template-id uint))
    (map-get? template-demand-metrics template-id)
)

(define-read-only (get-category-demand (category (string-utf8 32)))
    (map-get? category-demand category)
)

(define-read-only (get-pricing-parameters)
    {
        base-multiplier: (var-get base-multiplier),
        surge-threshold: (var-get surge-threshold),
        max-surge-multiplier: (var-get max-surge-multiplier),
        early-adopter-discount: (var-get early-adopter-discount),
        trending-window: (var-get trending-window)
    }
)

(define-read-only (is-template-trending (template-id uint))
    (match (map-get? template-demand-metrics template-id)
        metrics (get is-trending metrics)
        false
    )
)

(define-read-only (get-current-pricing-multiplier (template-id uint))
    (match (map-get? template-demand-metrics template-id)
        metrics (get current-multiplier metrics)
        (var-get base-multiplier)
    )
)



