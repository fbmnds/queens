(ns queens.core)

(defn
  cons-items-in-alist
  [items alist]
  (cond (nil? items) nil
        (nil? alist) nil
        :else (cons
               (cons (first items) alist)
               (cons-items-in-alist (next items) alist))))


(defn items-as-list
  [items]
  (cond (nil? items) nil
        :else (cons (list (first items)) (items-as-list (next items)))))


(defn cross-product
  ; not needed for queens
  [items list-of-lists]
  (cond (nil? items) nil
        (nil? list-of-lists) nil
        :else (concat
               (cons-items-in-alist items (first list-of-lists))
               (cross-product items (next list-of-lists)))))


(defn blocked-pos
  [board i Q]
  (cond (nil? board) nil
        :else (set                    ; retain unique positions
               (concat
                (list
                 (- (first board) i)  ; don't care, if off the board
                 (first board)
                 (+ (first board) i)) ; don't care, if off the board
                (blocked-pos (next board) (inc i) Q)))))


(defn save-pos
  [board i Q]
  (loop [j 1
         new-pos nil]
    (cond (> j Q) new-pos
          (nil? board) nil
          :else (recur
                 (inc j)
                 ; user> (nil? (#{1 3} 2))
                 ; true
                 ; user> (nil? (#{1 3} 3))
                 ; false
                 (cond (nil? ((blocked-pos board 1 Q) j)) (cons j new-pos)
                       :else new-pos)))))


(defn init-solution
  [Q]
  (cond (> Q 0) (cons (list Q) (init-solution (dec Q)))
        :else '()))


(defn solve
  [list-of-boards i Q]
  (cond (nil? list-of-boards) nil
        :else (concat
               (cons-items-in-alist
                (save-pos (first list-of-boards) i Q)
                (first list-of-boards))
               (solve (next list-of-boards) i Q))))


(defn queens
  [Q] ; size of board = number of queens
  (loop [i 0
         solution (init-solution Q)]
    (cond (= i (dec Q)) (doseq [s solution]
                          (println s))
          :else (recur (inc i) (solve solution i Q)))))

(defn -main []
  (queens 8))
