(ns hospital5.logic-test
  (:use clojure.pprint)
  (:require [clojure.test :refer :all]
            [hospital5.logic :refer :all]
            [hospital5.model :as h.model]
            [schema.core :as s]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.clojure-test :refer (defspec)]
            [clojure.test.check.properties :as prop]
            [schema-generators.generators :as g]))

(s/set-fn-validation! true)

(deftest fits-in-queue?-test
  ;zero boundary
  (testing "That it fits in the queue"
    (is (fits-in-queue? {:g-queue []} :g-queue)))

  ;limit boundary
  (testing "That it doesn't fit a new patient when the queue is full"
    (is (not (fits-in-queue? {:g-queue [1 59 27 41 32]} :g-queue))))

  ;one above limit boundary
  (testing "That it doesn't fit a new patient when the queue is above the limit"
    (is (not (fits-in-queue? {:g-queue [1 2 3 4 5 6]} :g-queue))))

  ;below limit boundary
  (testing "That it fits a new patient when the queue is below the limit"
    (is (fits-in-queue? {:g-queue [1 2 3 4]} :g-queue))
    (is (fits-in-queue? {:g-queue [1 2]} :g-queue)))

  (testing "That is does not fit in queue when the deparment doesn't exist"
    (is (not (fits-in-queue? {:g-queue [1 2 3 4]} :x-ray))))

  (testing "that a new element fits in a queue that is not full"
    (doseq [queue (gen/sample (gen/vector gen/string-alphanumeric 0 4) 100)]
      (is (fits-in-queue? {:g-queue queue} :g-queue))
      ))
  )

;(deftest arrived-at-test
;  (testing "that a new patient fits in a queue that is not full"
;    (doseq [queue (gen/sample (gen/vector gen/string-alphanumeric 0 4) 10)
;            patient (gen/sample gen/string-alphanumeric 5)]
;      (println patient queue)
;      (is (= 1 1))
;      ))
;  )

;(defspec inserts-patient-into-queues-limited-to-five 100
;         (prop/for-all
;           [queue (gen/vector gen/string-alphanumeric 0 4)
;            patient gen/string-alphanumeric]
;
;           (= {:g-queue (conj queue patient)}
;              (arrived-at {:g-queue queue} :g-queue patient))
;           ))

(def random-name-gen
  (gen/fmap clojure.string/join
            (gen/vector gen/char-alphanumeric 5 10)))

(defn transforms-vector-into-queue
  [vector]
  (reduce conj h.model/empty-queue vector)
  )

(def not-full-queue-gen
  (gen/fmap transforms-vector-into-queue
            (gen/vector random-name-gen 0 4)))

; uses log and rethrow, uses cond with type to catch an Exception
;(defn transfers-ignoring-errors
;  [hospital to]
;  (try
;       (transfer hospital :g-queue to)
;       (catch clojure.lang.ExceptionInfo e
;         (cond
;           (= :full-department (:type (ex-data e))) hospital
;           :else (throw e)
;           )
;
;         ;(println "It didn't work."
;         ;         (= :schema.core/error (:type (ex-data e)))
;         ;         (ex-data e))
;         ;hospital
;         ))
;  )

(defn transfers-ignoring-errors
  [hospital to]
  (try
       (transfer hospital :g-queue to)
       (catch java.lang.IllegalStateException e
         hospital
         ))
  )

(defn adds-general-queue-to-hospital
  [[hospital queue]]
  (assoc hospital :g-queue queue)
  )

(def hospital-gen
  (gen/fmap
    adds-general-queue-to-hospital
    (gen/tuple
      (gen/not-empty (g/generator h.model/Hospital))
      not-full-queue-gen)
    ))

(defspec transfer-must-have-the-same-amount-of-people 50
         (prop/for-all
           [g-queue (gen/fmap transforms-vector-into-queue (gen/vector random-name-gen 0 50))
            x-ray not-full-queue-gen
            ultrasound not-full-queue-gen
            patient-goes-to (gen/vector (gen/elements [:x-ray, :ultrasound]) 0 50)]

           ;(println (count g-queue) (count patient-goes-to) patient-goes-to)

           (let [hospital-before {:g-queue g-queue, :x-ray x-ray, :ultrasound ultrasound}
                 hospital-after
                 (reduce transfers-ignoring-errors hospital-before patient-goes-to)]

             ;(println (count (get hospital-after :x-ray)))

             (= (total-of-patients hospital-before)
                (total-of-patients hospital-after)))
           )
         )

(def arrived-at-gen
  (gen/tuple
    (gen/return arrived-at),
    (gen/return :g-queue),
    random-name-gen
    (gen/return 1)))

(defn adds-nonexistent-to-department
  [department]
  (keyword (str department "-nonexistent"))
  )

(defn transfer-gen
  [hospital]
  (let [departments (keys hospital)
        nonexistent-departments (map adds-nonexistent-to-department departments)
        all-departments (concat departments nonexistent-departments)]
    (gen/tuple
      (gen/return transfer),
      (gen/elements all-departments),
      (gen/elements all-departments)
      (gen/return 0)))
  )

(defn action-gen
  [hospital]
  (gen/one-of [arrived-at-gen (transfer-gen hospital)]))

(defn actions-gen
  [hospital]
  (gen/not-empty (gen/vector (action-gen hospital) 1 100)))

(defn perform-an-action
  [initial-situation [function param1 param2 difference-if-successful]]
  (let [initial-hospital (:hospital initial-situation)
        initial-difference (:difference initial-situation)]

    ;(println function param1 param2)
    (try
      (let [new-hospital (function initial-hospital param1 param2)]
        {:hospital new-hospital,
         :difference (+ difference-if-successful initial-difference)})
      (catch IllegalStateException e
        initial-situation
        )
      (catch AssertionError e
        initial-situation
        ))

    )
  )

(defspec simulates-a-day-in-the-hospital-and-loses-no-patient 50
         (prop/for-all [hospital-before hospital-gen]
                       (let [actions (gen/generate (actions-gen hospital-before))
                             situation-before {:hospital hospital-before, :difference 0}
                             total-of-patients-before (total-of-patients hospital-before)
                             situation-after (reduce perform-an-action situation-before actions)
                             total-of-patients-after (total-of-patients (:hospital situation-after))]

                         ;(pprint hospital)
                         ;(println actions)

                         ;let ([[function param1 param2]])
                         ;(function param1 param2)
                         ;(println total-of-patients-before "+" (:difference situation-after) "=" total-of-patients-after)
                         (is (=
                               (- total-of-patients-after (:difference situation-after))
                               total-of-patients-before)))
                       )
         )