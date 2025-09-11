;; Achievement Badges Contract
;; Tracks and rewards personal fitness milestones with collectible badges

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u400))
(define-constant ERR_BADGE_NOT_FOUND (err u401))
(define-constant ERR_ALREADY_EARNED (err u402))
(define-constant ERR_REQUIREMENTS_NOT_MET (err u403))
(define-constant ERR_INVALID_BADGE_TYPE (err u404))
(define-constant ERR_USER_NOT_FOUND (err u405))

;; Data variables for tracking
(define-data-var badge-type-counter uint u0)
(define-data-var total-badges-earned uint u0)
(define-data-var streak-threshold uint u7) ;; Days for streak badges

;; Badge type definitions
(define-map badge-types
  uint
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    category: (string-ascii 30),
    requirement-type: (string-ascii 20),
    requirement-value: uint,
    rarity: (string-ascii 15),
    token-reward: uint,
    icon-uri: (string-ascii 100),
    is-active: bool
  }
)

;; User badge collection
(define-map user-badges
  { user: principal, badge-type-id: uint }
  {
    earned-block: uint,
    earned-timestamp: uint,
    progress-when-earned: uint,
    is-claimed: bool
  }
)

;; User achievement progress tracking
(define-map user-progress
  principal
  {
    total-activities: uint,
    total-duration: uint,
    longest-streak: uint,
    current-streak: uint,
    activities-this-week: uint,
    activities-this-month: uint,
    unique-activity-types: uint,
    first-activity-timestamp: uint,
    badges-earned: uint
  }
)

;; Badge rarity statistics
(define-map badge-rarity-stats
  (string-ascii 15)
  {
    total-count: uint,
    earned-count: uint,
    rarity-multiplier: uint
  }
)

;; Initialize default badge types
(define-public (initialize-default-badges)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    ;; First Timer badges
    (try! (create-badge-type "First Steps" "Complete your first activity" "Beginner" "activities" u1 "Common" u50 "first-steps.svg" true))
    (try! (create-badge-type "Early Bird" "Submit 5 activities" "Beginner" "activities" u5 "Common" u100 "early-bird.svg" true))
    
    ;; Streak badges  
    (try! (create-badge-type "Week Warrior" "7-day activity streak" "Consistency" "streak" u7 "Uncommon" u200 "week-warrior.svg" true))
    (try! (create-badge-type "Month Master" "30-day activity streak" "Consistency" "streak" u30 "Rare" u500 "month-master.svg" true))
    (try! (create-badge-type "Iron Will" "100-day activity streak" "Consistency" "streak" u100 "Epic" u1000 "iron-will.svg" true))
    
    ;; Duration milestones
    (try! (create-badge-type "Hour Hero" "60 minutes total activity" "Duration" "duration" u60 "Common" u150 "hour-hero.svg" true))
    (try! (create-badge-type "Marathon Mindset" "1000 minutes total" "Duration" "duration" u1000 "Rare" u400 "marathon-mindset.svg" true))
    
    ;; Volume achievements
    (try! (create-badge-type "Century Club" "Complete 100 activities" "Volume" "activities" u100 "Epic" u800 "century-club.svg" true))
    (try! (create-badge-type "Fitness Legend" "Complete 500 activities" "Volume" "activities" u500 "Legendary" u2000 "fitness-legend.svg" true))
    
    (ok true)
  )
)

;; Create custom badge types
(define-public (create-badge-type 
  (name (string-ascii 50))
  (description (string-ascii 200))
  (category (string-ascii 30))
  (requirement-type (string-ascii 20))
  (requirement-value uint)
  (rarity (string-ascii 15))
  (token-reward uint)
  (icon-uri (string-ascii 100))
  (is-active bool))
  (let
    (
      (badge-id (+ (var-get badge-type-counter) u1))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    (map-set badge-types badge-id
      {
        name: name,
        description: description,
        category: category,
        requirement-type: requirement-type,
        requirement-value: requirement-value,
        rarity: rarity,
        token-reward: token-reward,
        icon-uri: icon-uri,
        is-active: is-active
      }
    )
    
    ;; Initialize rarity stats if new rarity
    (map-set badge-rarity-stats rarity
      (merge (default-to { total-count: u0, earned-count: u0, rarity-multiplier: u100 } 
                         (map-get? badge-rarity-stats rarity))
             { total-count: (+ (default-to u0 (get total-count (map-get? badge-rarity-stats rarity))) u1) })
    )
    
    (var-set badge-type-counter badge-id)
    (ok badge-id)
  )
)

;; Check and award badges based on user activity
(define-public (check-and-award-badges (user principal))
  (let
    (
      (user-stats (default-to 
        {
          total-activities: u0,
          total-duration: u0,
          longest-streak: u0,
          current-streak: u0,
          activities-this-week: u0,
          activities-this-month: u0,
          unique-activity-types: u0,
          first-activity-timestamp: u0,
          badges-earned: u0
        }
        (map-get? user-progress user)))
    )
    (begin
      (check-single-badge user u1 (get total-activities user-stats))
      (check-single-badge user u2 (get total-activities user-stats))
      (check-single-badge user u3 (get current-streak user-stats))
      (check-single-badge user u4 (get current-streak user-stats))
      (check-single-badge user u5 (get current-streak user-stats))
      (check-single-badge user u6 (get total-duration user-stats))
      (check-single-badge user u7 (get total-duration user-stats))
      (check-single-badge user u8 (get total-activities user-stats))
      (check-single-badge user u9 (get total-activities user-stats))
      (ok true)
    )
  )
)

;; Check and award a specific badge
(define-private (check-single-badge (user principal) (badge-type-id uint) (user-value uint))
  (let
    (
      (badge-type (map-get? badge-types badge-type-id))
      (user-badge-key { user: user, badge-type-id: badge-type-id })
      (already-earned (map-get? user-badges user-badge-key))
    )
    (match badge-type
      badge-data 
        (if (and (is-none already-earned) 
                 (get is-active badge-data)
                 (>= user-value (get requirement-value badge-data)))
          (begin
            (map-set user-badges user-badge-key
              {
                earned-block: stacks-block-height,
                earned-timestamp: stacks-block-height,
                progress-when-earned: user-value,
                is-claimed: false
              }
            )
            
            ;; Update user badge count
            (let ((user-stats (default-to 
                  {
                    total-activities: u0,
                    total-duration: u0,
                    longest-streak: u0,
                    current-streak: u0,
                    activities-this-week: u0,
                    activities-this-month: u0,
                    unique-activity-types: u0,
                    first-activity-timestamp: u0,
                    badges-earned: u0
                  }
                  (map-get? user-progress user))))
              (map-set user-progress user
                (merge user-stats { badges-earned: (+ (get badges-earned user-stats) u1) })
              )
            )
            
            ;; Update global stats
            (var-set total-badges-earned (+ (var-get total-badges-earned) u1))
            true
          )
          false)
      false)
  )
)

;; Update user progress when activity is verified  
(define-public (update-user-progress 
  (user principal)
  (activity-duration uint)
  (current-streak uint))
  (let
    (
      (current-stats (default-to 
        {
          total-activities: u0,
          total-duration: u0,
          longest-streak: u0,
          current-streak: u0,
          activities-this-week: u0,
          activities-this-month: u0,
          unique-activity-types: u0,
          first-activity-timestamp: stacks-block-height,
          badges-earned: u0
        }
        (map-get? user-progress user)))
      (new-total-activities (+ (get total-activities current-stats) u1))
      (new-total-duration (+ (get total-duration current-stats) activity-duration))
      (new-longest-streak (if (> current-streak (get longest-streak current-stats)) 
                             current-streak 
                             (get longest-streak current-stats)))
    )
    
    (map-set user-progress user
      {
        total-activities: new-total-activities,
        total-duration: new-total-duration,
        longest-streak: new-longest-streak,
        current-streak: current-streak,
        activities-this-week: (+ (get activities-this-week current-stats) u1),
        activities-this-month: (+ (get activities-this-month current-stats) u1),
        unique-activity-types: (get unique-activity-types current-stats),
        first-activity-timestamp: (get first-activity-timestamp current-stats),
        badges-earned: (get badges-earned current-stats)
      }
    )
    
    ;; Check for new badges
    (let ((result (check-and-award-badges user)))
      (ok true))
  )
)

;; Claim badge rewards
(define-public (claim-badge-reward (badge-type-id uint))
  (let
    (
      (badge-type (unwrap! (map-get? badge-types badge-type-id) ERR_BADGE_NOT_FOUND))
      (user-badge-key { user: tx-sender, badge-type-id: badge-type-id })
      (user-badge (unwrap! (map-get? user-badges user-badge-key) ERR_BADGE_NOT_FOUND))
      (token-reward (get token-reward badge-type))
    )
    (asserts! (not (get is-claimed user-badge)) ERR_ALREADY_EARNED)
    
    ;; Mark badge as claimed
    (map-set user-badges user-badge-key
      (merge user-badge { is-claimed: true })
    )
    
    ;; Award tokens (would integrate with fitness-mining token)
    (ok token-reward)
  )
)

;; Read-only functions

(define-read-only (get-badge-type (badge-type-id uint))
  (map-get? badge-types badge-type-id)
)

(define-read-only (get-user-badge (user principal) (badge-type-id uint))
  (map-get? user-badges { user: user, badge-type-id: badge-type-id })
)

(define-read-only (get-user-progress (user principal))
  (map-get? user-progress user)
)

(define-read-only (has-badge (user principal) (badge-type-id uint))
  (is-some (map-get? user-badges { user: user, badge-type-id: badge-type-id }))
)

(define-read-only (get-user-badge-count (user principal))
  (match (map-get? user-progress user)
    stats (get badges-earned stats)
    u0)
)

(define-read-only (get-total-badge-types)
  (var-get badge-type-counter)
)

(define-read-only (get-total-badges-earned)
  (var-get total-badges-earned)
)

(define-read-only (get-badge-rarity-stats (rarity (string-ascii 15)))
  (map-get? badge-rarity-stats rarity)
)

(define-read-only (get-eligible-badges (user principal))
  (let
    (
      (user-stats (map-get? user-progress user))
    )
    (match user-stats
      stats (list 
        (badge-eligibility-check user u1 (get total-activities stats))
        (badge-eligibility-check user u2 (get total-activities stats))
        (badge-eligibility-check user u3 (get current-streak stats))
      )
      (list false false false))
  )
)

(define-private (badge-eligibility-check (user principal) (badge-type-id uint) (user-value uint))
  (let
    (
      (badge-type (map-get? badge-types badge-type-id))
      (already-earned (map-get? user-badges { user: user, badge-type-id: badge-type-id }))
    )
    (match badge-type
      badge-data (and (is-none already-earned)
                      (get is-active badge-data)
                      (>= user-value (get requirement-value badge-data)))
      false)
  )
)
