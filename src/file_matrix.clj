(use '(incanter core processing))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; project file functions

(defn match-file-extension [file-name]
  (reduce bool-or (map #(.endsWith file-name %) file-extensions)))

(defn get-proj-files [proj-src-root]
  (filter #(not (.isDirectory %)) (file-seq (java.io.File. proj-src-root))))

(defn fix-proj-file-names [the-proj-files proj-root-prefix]
  (filter
   match-file-extension
   (map
    #(.replace (.replace (.getCanonicalPath %) proj-root-prefix "") "\\" "/")
    the-proj-files)))

(defn prepare-file-map [files file-map-x]
  (loop [result []
         rest-files files]
    (let [two-part (split-at file-map-x rest-files)]
      (if (= 0 (count (second two-part)))
        (conj result (first two-part))
        (recur (conj result (first two-part)) (second two-part))))))

(defn get-clean-files [the-proj-file-names the-scan-file-names]
  (filter #(not (in? the-scan-file-names %)) the-proj-file-names))


(defn get-inter-files [the-proj-file-names the-sink-files the-clean-files]
  (filter
   #(and (not (in? the-sink-files %)) (not (in? the-clean-files %)))
   the-proj-file-names))

;;;;;;;;;;;;;;;;;;;;
;; drawing functions

(def cube-w 10)
(def cube-shift 15)
(def h-margin 2)

(def stroke-colors
     {:DEFAULT (color 50)
      :HOVER (color 255 255 0)})

(def file-colors
     {:SINK (color 255 0 0)
      :SCANNED (color 220 220 0)
      :EP (color 0 0 255)
      :CLEAN (color 255 255 255)})

(defn draw-stat [sktch pos-x pos-y the-color file-count]
  (doto sktch
    (fill the-color)
    (rect pos-x pos-y cube-w cube-w)
    (fill 0)
    (text file-count (+ cube-shift pos-x) (+ cube-w pos-y))))

(defn draw-file-stats
  [sktch the-file-stats file-map-x]
  (doseq [stat the-file-stats]
    (apply draw-stat sktch (+ 50 file-map-x) stat)))

(def file-x (ref 20))
(def file-y (ref 20))
(def mouseX (ref nil))
(def mouseY (ref nil))
(def hover-file (ref ""))

(defn mouse-over? [x1 y1 x2 y2]
  (and (> @mouseX x1)
       (< @mouseX x2)
       (> @mouseY y1)
       (< @mouseY y2)))

(defn draw-file-row [sktch file-row file-cube-color]
  (doseq [a-file file-row]
    (fill sktch (file-cube-color a-file))
    (rect sktch @file-x @file-y cube-w cube-w)
    (if (mouse-over? @file-x @file-y (+ @file-x cube-w) (+ @file-y cube-w))
      (dosync
       (ref-set hover-file a-file)
       (doto sktch
	 (no-fill)
	 (stroke (:HOVER stroke-colors))
	 (rect (- @file-x h-margin)
	       (- @file-y h-margin)
	       (+ cube-w (* 2 h-margin))
	       (+ cube-w (* 2 h-margin)))
	 (stroke (:DEFAULT stroke-colors)))))
    (dosync (ref-set file-x (+ cube-shift @file-x)))))

(defn draw-file-map [sktch the-file-map file-cube-color]
  (doseq [file-row the-file-map]
    (draw-file-row sktch file-row file-cube-color)
    (dosync (ref-set file-x 20)
            (ref-set file-y (+ cube-shift @file-y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; preparing what we need

(def xml-report
     (parse "C:\\testcase\\xml_report\\SI_POC\\SuperSecureBank.xml"))
(def proj-root "C:\\testcase\\xml_report\\SI_POC\\SuperSecureBank")
(def proj-src-loc "C:\\testcase\\xml_report\\SI_POC\\SuperSecureBank")
(def vulns (get-vulns xml-report))
(def xss (filter-vulns "XSS" vulns))
(def sql (filter-vulns "SQL" vulns))
(def sink-files (get-sink-files vulns))
(def xss-sink-files (get-sink-files xss))
(def sql-sink-files (get-sink-files sql))

(defn get-intersection [a-set b-set]
  (filter #(in? a-set %) b-set))

(def both-sink-files (filter #(in? xss-sink-files %) sql-sink-files))

(def scan-file-names
     (get-scan-file-names xml-report))

(def proj-file-names
     (fix-proj-file-names
      (get-proj-files proj-src-loc)
      proj-root))

(def clean-file-count
     (count (get-clean-files proj-file-names scan-file-names)))

(def inter-file-count
     (count
      (get-inter-files
       proj-file-names
       sink-files
       (get-clean-files proj-file-names scan-file-names))))

(def proj-file-map
     (prepare-file-map
      proj-file-names
      (ceil (sqrt (count proj-file-names)))))

(defn change-cursor [sktch]
  (if (= @hover-file "")
    (cursor sktch 0)
    (cursor sktch 12)))

(let [sktch (sketch
	     (setup []
                    (doto this
                      (size 300 280)
                      (background 200)
                      (fill 255)
                      (stroke 50)
                      ))
	     (draw []
                   (doto this
                      (dosync (ref-set file-x 20)
                              (ref-set file-y 20))
                      (draw-file-map proj-file-map)
                      (draw-file-stats
                       (count both-sink-files)
                       (- (count sink-files) (count both-sink-files))
                       inter-file-count
                       clean-file-count)
		      (change-cursor)
                    )))]
  (view sktch :size [300 280]))