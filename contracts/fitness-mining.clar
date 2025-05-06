
;; title: fitness-mining
;; version:
;; summary:
;; description:


(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-invalid-activity (err u102))
(define-constant err-already-claimed (err u103))
(define-constant err-invalid-proof (err u104))

(define-fungible-token fitness-token)

(define-data-var token-uri (string-utf8 256) u"")
(define-data-var tokens-per-activity uint u100)
(define-data-var min-activity-duration uint u30)

(define-map activity-claims 
    { user: principal, timestamp: uint } 
    { claimed: bool, activity-type: (string-utf8 64), duration: uint }
)

(define-map authorized-verifiers principal bool)

(define-public (set-token-uri (new-uri (string-utf8 256)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set token-uri new-uri)
        (ok true)
    )
)

(define-public (set-tokens-per-activity (amount uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set tokens-per-activity amount)
        (ok true)
    )
)

(define-public (add-verifier (verifier principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set authorized-verifiers verifier true)
        (ok true)
    )
)

(define-public (remove-verifier (verifier principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set authorized-verifiers verifier false)
        (ok true)
    )
)

(define-public (submit-activity (activity-type (string-utf8 64)) (duration uint) (timestamp uint))
    (let
        (
            (claim-key { user: tx-sender, timestamp: timestamp })
        )
        (asserts! (>= duration (var-get min-activity-duration)) err-invalid-activity)
        (asserts! (is-none (map-get? activity-claims claim-key)) err-already-claimed)
        (map-set activity-claims claim-key { claimed: false, activity-type: activity-type, duration: duration })
        (ok true)
    )
)

(define-public (verify-activity (user principal) (timestamp uint))
    (let
        (
            (claim-key { user: user, timestamp: timestamp })
            (claim (unwrap! (map-get? activity-claims claim-key) err-invalid-proof))
        )
        (asserts! (default-to false (map-get? authorized-verifiers tx-sender)) err-not-authorized)
        (asserts! (not (get claimed claim)) err-already-claimed)
        (try! (ft-mint? fitness-token (var-get tokens-per-activity) user))
        (map-set activity-claims claim-key (merge claim { claimed: true }))
        (ok true)
    )
)

(define-read-only (get-activity-claim (user principal) (timestamp uint))
    (map-get? activity-claims { user: user, timestamp: timestamp })
)

(define-read-only (get-tokens-per-activity)
    (var-get tokens-per-activity)
)

(define-read-only (is-verifier (address principal))
    (default-to false (map-get? authorized-verifiers address))
)

(define-read-only (get-token-uri)
    (var-get token-uri)
)


(define-map user-streaks 
    principal 
    { current-streak: uint, last-activity: uint }
)

(define-data-var streak-bonus-multiplier uint u10)

(define-read-only (get-user-streak (user principal))
    (default-to { current-streak: u0, last-activity: u0 } 
        (map-get? user-streaks user))
)

(define-public (set-streak-multiplier (multiplier uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set streak-bonus-multiplier multiplier)
        (ok true))
)

(define-private (update-streak (user principal) (timestamp uint))
    (let (
        (user-data (get-user-streak user))
        (last-activity (get last-activity user-data))
        (current-streak (get current-streak user-data))
        (one-day u86400)
        (new-streak (if (< (- timestamp last-activity) one-day)
            (+ current-streak u1)
            u1))
    )
    (map-set user-streaks user 
        { current-streak: new-streak, last-activity: timestamp })
    new-streak)
)


(define-map weekly-points
    { user: principal, week: uint }
    uint
)

(define-map monthly-points 
    { user: principal, month: uint }
    uint
)

(define-read-only (get-weekly-points (user principal) (week uint))
    (default-to u0 
        (map-get? weekly-points { user: user, week: week }))
)

(define-read-only (get-monthly-points (user principal) (month uint))
    (default-to u0 
        (map-get? monthly-points { user: user, month: month }))
)

(define-private (add-points (user principal) (timestamp uint) (duration uint))
    (let (
        (week (/ timestamp u604800))
        (month (/ timestamp u2592000))
        (points (* duration u1))
        (current-weekly (get-weekly-points user week))
        (current-monthly (get-monthly-points user month))
    )
    (map-set weekly-points { user: user, week: week }
        (+ current-weekly points))
    (map-set monthly-points { user: user, month: month }
        (+ current-monthly points))
    (ok true))
)