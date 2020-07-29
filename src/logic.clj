(ns hospital5.logic
  (:require [hospital5.model :as h.model]
            [schema.core :as s]))

; also works even when the  department is nil
(defn fits-in-queue?
  [hospital department]
  (some-> hospital
          department
          count
          (< 5))
  )

(defn arrived-at
  [hospital department patient]
  (if (fits-in-queue? hospital department)
    (update hospital department conj patient)
    ;(throw (ex-info "This department is full or doesn't exist" {:patient patient
    ;                                                            :type :full-department}))
    (throw (java.lang.IllegalStateException. "This department is full or doesn't exist"))
    )
  )

(s/defn was-attended-to :- h.model/Hospital
        [hospital :- h.model/Hospital, department :- s/Keyword]
        (update hospital department pop)
        )

(s/defn next-patient :- (s/maybe h.model/PatientID)
  [hospital :- h.model/Hospital, department :- s/Keyword]
        (-> hospital
            department
            peek)
        )

(defn same-size?
  [hospital hospital-after from to]
  (= (+ (count (get hospital-after from)) (count (get hospital-after to)))
     (+ (count (get hospital from)) (count (get hospital to))))
  )

(s/defn transfer :- h.model/Hospital
        [hospital :- h.model/Hospital, from :- s/Keyword, to :- s/Keyword]
        {
         :pre  [(contains? hospital from), (contains? hospital to)]
         :post [(same-size? hospital, %, from, to)]
         }
        (if-let [patient (next-patient hospital from)]
          (-> hospital
              (was-attended-to from)
              (arrived-at to patient))
          hospital)
        )

(defn total-of-patients
  [hospital]
  (reduce + (map count (vals hospital)))
  )