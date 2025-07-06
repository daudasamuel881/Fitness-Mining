(define-constant err-challenge-not-found (err u200))
(define-constant err-challenge-ended (err u201))
(define-constant err-challenge-not-started (err u202))
(define-constant err-already-joined (err u203))
(define-constant err-not-participant (err u204))
(define-constant err-insufficient-funds (err u205))
(define-constant err-challenge-active (err u206))
(define-constant err-goal-not-met (err u207))

(define-data-var next-challenge-id uint u1)

(define-map challenges
    uint
    {
        creator: principal,
        title: (string-utf8 128),
        description: (string-utf8 256),
        goal-type: (string-utf8 32),
        goal-amount: uint,
        start-time: uint,
        end-time: uint,
        entry-fee: uint,
        reward-pool: uint,
        max-participants: uint,
        participant-count: uint,
        is-active: bool
    }
)

(define-map challenge-participants
    { challenge-id: uint, participant: principal }
    {
        joined-at: uint,
        total-progress: uint,
        completed: bool,
        reward-claimed: bool
    }
)

(define-map challenge-activities
    { challenge-id: uint, participant: principal, timestamp: uint }
    {
        activity-type: (string-utf8 64),
        duration: uint,
        verified: bool
    }
)

(define-public (create-challenge 
    (title (string-utf8 128))
    (description (string-utf8 256))
    (goal-type (string-utf8 32))
    (goal-amount uint)
    (start-time uint)
    (end-time uint)
    (entry-fee uint)
    (max-participants uint))
    (let
        (
            (challenge-id (var-get next-challenge-id))
        )
        (asserts! (> end-time start-time) (err u400))
        (asserts! (> max-participants u0) (err u401))
        (map-set challenges challenge-id
            {
                creator: tx-sender,
                title: title,
                description: description,
                goal-type: goal-type,
                goal-amount: goal-amount,
                start-time: start-time,
                end-time: end-time,
                entry-fee: entry-fee,
                reward-pool: u0,
                max-participants: max-participants,
                participant-count: u0,
                is-active: true
            }
        )
        (var-set next-challenge-id (+ challenge-id u1))
        (ok challenge-id)
    )
)

(define-public (join-challenge (challenge-id uint))
    (let
        (
            (challenge (unwrap! (map-get? challenges challenge-id) err-challenge-not-found))
            (participant-key { challenge-id: challenge-id, participant: tx-sender })
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
        )
        (asserts! (get is-active challenge) err-challenge-ended)
        (asserts! (>= current-time (get start-time challenge)) err-challenge-not-started)
        (asserts! (< current-time (get end-time challenge)) err-challenge-ended)
        (asserts! (< (get participant-count challenge) (get max-participants challenge)) (err u402))
        (asserts! (is-none (map-get? challenge-participants participant-key)) err-already-joined)
        
        ;; (if (> (get entry-fee challenge) u0)
        ;;     (try! (stx-transfer? (get entry-fee challenge) tx-sender (as-contract tx-sender)))
        ;;     (ok true)
        ;; )
        
        (map-set challenge-participants participant-key
            {
                joined-at: current-time,
                total-progress: u0,
                completed: false,
                reward-claimed: false
            }
        )
        
        (map-set challenges challenge-id
            (merge challenge 
                { 
                    participant-count: (+ (get participant-count challenge) u1),
                    reward-pool: (+ (get reward-pool challenge) (get entry-fee challenge))
                }
            )
        )
        (ok true)
    )
)

(define-public (submit-challenge-activity 
    (challenge-id uint)
    (activity-type (string-utf8 64))
    (duration uint)
    (timestamp uint))
    (let
        (
            (challenge (unwrap! (map-get? challenges challenge-id) err-challenge-not-found))
            (participant-key { challenge-id: challenge-id, participant: tx-sender })
            (participant (unwrap! (map-get? challenge-participants participant-key) err-not-participant))
            (activity-key { challenge-id: challenge-id, participant: tx-sender, timestamp: timestamp })
        )
        (asserts! (get is-active challenge) err-challenge-ended)
        (asserts! (>= timestamp (get start-time challenge)) err-challenge-not-started)
        (asserts! (< timestamp (get end-time challenge)) err-challenge-ended)
        
        (map-set challenge-activities activity-key
            {
                activity-type: activity-type,
                duration: duration,
                verified: false
            }
        )
        (ok true)
    )
)

(define-public (verify-challenge-activity 
    (challenge-id uint)
    (participant principal)
    (timestamp uint))
    (let
        (
            (challenge (unwrap! (map-get? challenges challenge-id) err-challenge-not-found))
            (participant-key { challenge-id: challenge-id, participant: participant })
            (participant-data (unwrap! (map-get? challenge-participants participant-key) err-not-participant))
            (activity-key { challenge-id: challenge-id, participant: participant, timestamp: timestamp })
            (activity (unwrap! (map-get? challenge-activities activity-key) (err u403)))
        )
        (asserts! (contract-call? .fitness-mining is-verifier tx-sender) (err u404))
        (asserts! (not (get verified activity)) (err u405))
        
        (let
            (
                (new-progress (+ (get total-progress participant-data) (get duration activity)))
                (goal-met (>= new-progress (get goal-amount challenge)))
            )
            (map-set challenge-activities activity-key
                (merge activity { verified: true })
            )
            (map-set challenge-participants participant-key
                (merge participant-data 
                    { 
                        total-progress: new-progress,
                        completed: goal-met
                    }
                )
            )
            (ok true)
        )
    )
)

(define-public (claim-challenge-reward (challenge-id uint))
    (let
        (
            (challenge (unwrap! (map-get? challenges challenge-id) err-challenge-not-found))
            (participant-key { challenge-id: challenge-id, participant: tx-sender })
            (participant (unwrap! (map-get? challenge-participants participant-key) err-not-participant))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
        )
        (asserts! (> current-time (get end-time challenge)) err-challenge-active)
        (asserts! (get completed participant) err-goal-not-met)
        (asserts! (not (get reward-claimed participant)) (err u406))
        
        (let
            (
                (completed-info (get-completed-participants-count challenge-id))
                (completed-count (get count completed-info))
                (reward-per-winner (if (> completed-count u0)
                    (/ (get reward-pool challenge) completed-count)
                    u0))
            )
            ;; (if (> reward-per-winner u0)
            ;;     (try! (as-contract (stx-transfer? reward-per-winner tx-sender tx-sender)))
            ;;     (try! (ok true))
            ;; )
            
            (map-set challenge-participants participant-key
                (merge participant { reward-claimed: true })
            )
            (ok reward-per-winner)
        )
    )
)

(define-private (get-completed-participants-count (challenge-id uint))
    (fold count-completed-participants 
        (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20)
        { challenge-id: challenge-id, count: u0, checked: u0 }
    )
)

(define-private (count-completed-participants 
    (index uint) 
    (acc { challenge-id: uint, count: uint, checked: uint }))
    (let
        (
            (challenge (unwrap! (map-get? challenges (get challenge-id acc)) acc))
            (max-check (get participant-count challenge))
        )
        (if (< (get checked acc) max-check)
            (merge acc { count: (get count acc), checked: (+ (get checked acc) u1) })
            acc
        )
    )
)

(define-read-only (get-challenge (challenge-id uint))
    (map-get? challenges challenge-id)
)

(define-read-only (get-challenge-participant (challenge-id uint) (participant principal))
    (map-get? challenge-participants { challenge-id: challenge-id, participant: participant })
)

(define-read-only (get-challenge-activity (challenge-id uint) (participant principal) (timestamp uint))
    (map-get? challenge-activities { challenge-id: challenge-id, participant: participant, timestamp: timestamp })
)

(define-read-only (get-active-challenges-count)
    (var-get next-challenge-id)
)

(define-read-only (is-challenge-participant (challenge-id uint) (participant principal))
    (is-some (map-get? challenge-participants { challenge-id: challenge-id, participant: participant }))
)

(define-read-only (get-participant-progress (challenge-id uint) (participant principal))
    (match (map-get? challenge-participants { challenge-id: challenge-id, participant: participant })
        participant-data (get total-progress participant-data)
        u0
    )
)