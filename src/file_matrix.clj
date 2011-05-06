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

(def cube-w 7)

(defn draw-stat [sktch the-color pos-y file-count]
   (let [pos-x 200
         cube-w 7]
     (doto sktch
       (fill the-color)
       (rect pos-x pos-y cube-w cube-w)
       (fill 0)
       (text file-count (+ 10 pos-x) (+ 10 pos-y)))))

(defn draw-file-stats
  [sktch
   both-sink-count
   single-sink-count
   inter-file-count
   clean-file-count]
  (doto sktch
    (draw-stat (color 255 0 0) 20 both-sink-count)
    (draw-stat (color 200 200 0) 40 single-sink-count)
    (draw-stat (color 0 0 255) 60 inter-file-count)
    (draw-stat (color 255 255 255) 80 clean-file-count)))

(def file-x (ref 20))
(def file-y (ref 20))

(defn draw-file-row [sktch file-row]
  (doseq [a-file file-row]
    (if (in? sink-files a-file)
      (do
        (if (in? both-sink-files a-file)
          (fill sktch 255 0 0)
          (fill sktch 200 200 0)))
      (if (not (in? scan-file-names a-file))
        (fill sktch 255)
        (fill sktch 0 0 255)))
    (rect sktch @file-x @file-y cube-w cube-w)
    (dosync (ref-set file-x (+ 10 @file-x)))))

(defn draw-file-map [sktch the-file-map]
  (doseq [file-row the-file-map]
    (draw-file-row sktch file-row)
    (dosync (ref-set file-x 20)
            (ref-set file-y (+ 10 @file-y)))))

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
                       clean-file-count
                       )
                    )))]
  (view sktch :size [300 280]))