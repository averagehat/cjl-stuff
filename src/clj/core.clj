(ns clj.core
  (:require [instaparse.core :as insta]
            [clj-biosequence.genbank :as g]
            [cheshire.core :refer :all :as json]
            [clojurewerkz.elastisch.rest  :as esr]
            [clj-biosequence.core :as bio]
            [clojurewerkz.elastisch.rest.index :as esi] 
            [clojurewerkz.elastisch.rest.document :as esd]
            [clojure.string :as str]
            [clj-biosequence.alphabet :as alpha] 
            [defun :refer [defun]] 
            [clojure.test :refer :all]
            [clojure.core.typed :as t]
            [clojurewerkz.elastisch.rest.response :as esrsp] 
            [clojure.core.match :refer [match]])
  (:gen-class))



;
;(defn -main
;  "I don't do a whole lot ... yet."
;  [& args]
;  (println "Hello, World!"))
;
;(defn get-indices [xs e] (keep-indexed #(when (= e %2) %1) xs))
;
;;(let [t (:ncbieaa (tables 1))
;
(defn aa->codon [c & {:keys [table] :or {table (alpha/codon-tables 1)}}] 
  (let [t (:ncbieaa table)
        v [\T \C \A \G]
        n (count v)
        idxs (get-indices t c)]
  (->>
    (for [idx idxs]
      ;(filter  (fn [xs] (contains? (vec (map t idxs))  (alpha/codon->aa (map v xs) table)))
      (filter  (fn [xs] (= (nth t idx) (alpha/codon->aa (map v xs) table)))
        (for [i (range n) j (range n) k (range n)]
          [i j k])))
    (apply concat)
    (map (fn [xs] (map v xs)))
    set
    (map str/join ))))

(defn product [[x & xs]]
    (for [e x  more (product xs)]
      (cons e more)))

(defn protein->dna [seq-]
  (let [table ( alpha/codon-tables 1)]
   (->> (map #(aa->codon % :table table) seq-)
        product
        (map str/join))))
(protein->dna "FFF")  
(def codon-map {\F '("TTT" "TTC") \L '("TTA" "TTG" "CTT" "CTC" "CTA" "CTG")})

;
;;;;
;;
;;;; x & xs is equivalent to [x:xs]; giving xs as a result
;;;; note we need the inner [] for the list to be caught
;;;; [[fst snd]] will also work
;;
;;;(defn head-rest- [[x & xs]] [x xs])
;;;(head-rest- [1 2 3])
;;;(t/cf rest )
;;;(t/cf conj)t/Seqable
;;;(t/cf (t/inst conj int int))
;;;(t/cf (t/ann signum [t/Num -> t/Num]))
;;;(t/cf signum)
;;;;; have to wrap all calls in t/cf for this to work in the REPL
;;;(t/cf (signum [0]))
;;;(t/cf 1 (t/Value 1))
;;;(defn inc-all- [seq-] (map inc seq-))
;;;(defn signum [i] (- 0 i))
;;;
;;;(t/cf (defn inc-all- [seq-] (map inc seq-)))
;;;(t/ann my-identity (Int [x] (Fn [x -> x]))))
;;;(t/ann my-identity (t/Int [x] (t/Fn [x -> x]))))
;;;(t/cf (t/ann my-identity (t/All [x] (t/Fn [x -> x])))) t/All
;;;(t/cf (t/ann id (t/All [x]  [(t/I x t/Num) -> x])))
;;;(t/D t/Int)
;;;(t/cf (t/ann id (t/All [x]  [x -> (t/Difference t/Num t/Int)])))
;;;(t/cf (t/ann id (All [x]  [x ->  (Difference All x)])))
;;;(t/cf (defn id [x]  (inc x)))
;;;(t/cf (id "i"))
;;;(t/cf (id 3)) 
;;;(t/cf (t/ann my-identity- (t/All [x] (t/Fn [x -> x]))))
;;;; clojure typed: incorrect funciton syntax [x]
;;;(t/cf (defn my-identity- [x]
;;;  (if (number? x)
;;;    (inc x)
;;;    x)))
;;;
;;;      t/Difference
;;;(t/cf my-identity)
;;;; clojure typed cannot invoke type
;;;(t/cf (t/ann head-rest- (t/All [x] (t/Fn [(t/NonEmptySeq x) -> (t/NonEmptySeq (t/U x (t/NonEmptySeq x)))]))))
;;;(t/cf (t/ann inc-all (t/Num [x] (t/Fn [(t/NonEmptySeq x) -> (t/NonEmptySeq x)]))))
;;;(t/cf (t/ann inc-all- [(t/NonEmptySeqable t/Num) -> (t/NonEmptySeq t/Num)]))
;;;(t/ann inc-all- (t/Int [x] (t/Fn [ x ->  x])))
;;;       ;;note vector is not a t/Seq
;;;(t/cf (inc-all-   [ ]))
;;;(t/cf (inc-all-   [1 2 3 ]))
;;(aa->codon \F :table (alpha/codon-tables 1))
;;
;;
;;(let [c \L]
;;(is (=
;;     (frequencies (codon-map c) )
;;(frequencies (aa->codon c :table (alpha/codon-tables 1))))))
;;
;;(is (= 
;;      #{"TTC" "TTT"}
;;      (set(aa->codon \F))))
;;(let [prot-seq "FFF"]
;;(is
;;  (every? #(= prot-seq %)
;;    (map
;;      (comp str/join :sequence
;;            #(-> (bio/init-fasta-sequence 1 ""   :iupacNucleicAcids %)
;;               (bio/translate 1))) (flatten (protein->dna prot-seq))))))
;;
;;(-> (bio/init-fasta-sequence 1 ""   :iupacNucleicAcids "TTTTTTTTT")
;;     (bio/translate 1))
;
;
;(require '[clj-http.client :as client]) 
;
;; (with-open [out-file (java.io.FileWriter. "results.xml")]
;;    (emit res out-file))
;
;;(zf/xml1-> x (zf/text #(-> % true ))))
;
;(defn acc->xml [acc]
;  (let [url (format "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=nuccore&id=%s&rettype=xml" acc)
;      s (slurp url) 
;      parsed (parse-str s)] parsed))
;(defn xml->loc [x]
;  (->> (tree-seq map? :content  x) 
;       (filter #(-> % :tag (= :SubSource)))
;       (map :content)
;        (filter 
;          (fn [e] 
;             (some #(-> % :attrs :value (= "country")) e)))
;       flatten
;       (filter  #(-> % :tag (= :SubSource_name)))
;       (map :content)
;       flatten
;       first))
;
;;(xml->loc (acc->xml "EU569704" )) 
;
;(emit-str res)
;(comment 
;(let [ids (line-seq (clojure.java.io/reader "ids.txt"))
;      xmls (map acc->xml ids)
;      locs (map xml->loc xmls)]
;   (map #(println (str %1 \t  %2)) ids locs))
;(let [ids (line-seq (clojure.java.io/reader "ids.txt"))
;      xmls (map acc->xml ids)]
;(doall xmls))
;
;(def res (acc->xml "EU569704"))
;(def x (z/xml-zip res))  
;
;(->> (dz/descendants x)
;    (map #( zf/xml-> % "11060")))
;(->> (tree-seq (some-fn map? vector?) :content res)
;     (filter (fn [e] 
;       (let [con (first (:content e))] 
;         (when (string? con)  (re-find #"USA" con)) )))))
; 
;
;(zf/xml-> x :Seq-entry zf/text)
;zf/xml->
;(z/node x)
;
;((xml-seq x)
;(->> (dz/children x)
;     print)
;
;(with-open [out-file (java.io.FileWriter. "foo.xml")]
;    (indent res out-file))
;
;(z/node (drop-while #(not (zf/xml1-> % :Seq-entry_set)) (iterate z/next x)))
;
;
;(z/node (drop-while #(not (zf/xml1-> % :Seq-entry_set)) (iterate z/next x)))
;;() is truthy!
;(->> (tree-seq map? :content  res) 
;     (filter #(-> % :tag (= :SubSource)))
;(map
;(fn [x]
;  (-> (z/xml-zip x)
;      z/down
;      (zf/xml-> (zf/attr= :value "country")))))
;  (remove empty?))
;(count  (dz/descendants x)))
;(count (filter #(some :SubSource %) (dz/descendants x)))
;(count 
;  (filter #(not-empty (zf/xml-> %  :SubSource))
;          (dz/descendants x)))
;
;
;(count (filter-xml x :SubSource :SubSource_subtype (zf/attr= :value "country")))
; 
;
;
;(require ['clojure.data.zip :as 'dz])
;
;
;[org.clojure/data.xml "0.0.8"]
;
;(use 'clojure.data.xml)
;
;(require '[clojure.data.zip.xml :as zf])
;(require ['clojure.zip :as 'z])
;
;(def ex
;"<table>
;  <column name=\"col1\" type=\"varchar\"  length=\"8\"/>
;  <column name=\"col2\" type=\"varchar\"  length=\"16\"/>
;  <column name=\"col3\" type=\"int\"  length=\"16\"/>
;</table>")
;
;(let [x (z/xml-zip (clojure.data.xml/parse-str ex))]
;  (->> (zf/xml-> x :column)
;         flatten
;        (keep :attrs)
;        (map vals)))
;
;(let [x  (clojure.data.xml/parse-str ex)]
;  (->> (zf/xml-> x :column #(keep :attrs %))
;  (map vals)))
;
;(count ) (dz/descendants x)
;
;(defn filter-xml [x & preds]
;  (filter (comp not-empty #(apply zf/xml-> % preds))  (dz/descendants x)))
;
;
;(let [ sources (->> (tree-seq map? :content  res) 
;     (filter #(-> % :tag (= :SubSource))))]
;(for [s sources :let [x (z/xml-zip s)
;                       kids (dz/children x)]
;                 :when (some (zf/attr= :value "country") kids)]
;  (indent-str x)))
;
;(fn [x]
;  (-> (z/xml-zip x)
;      z/down
;      (zf/xml-> (zf/attr= :value "country")))))
;  (remove empty?))
;
;
;       (map :content)
;        (filter 
;          (fn [e] 
;             (some #(-> % :attrs :value (= "country")) e)))
;       flatten
;       (filter  #(-> % :tag (= :SubSource_name)))
;       (map :content)
;       flatten
;       first))
;
;
;(def ex " <data> <products> <product> <section>Red Section</section> <images> <image>img.jpg</image> <image>img2.jpg</image> </images> </product> <product> <section>Blue Section</section> <images> <image>img.jpg</image> <image>img3.jpg</image> </images> </product> <product> <section>Green Section</section> <images> <image>img.jpg</image> <image>img2.jpg</image> </images> </product> </products> </data>")
;
;
;(def x (z/xml-zip (clojure.data.xml/parse-str ex)))
;  (zf/xml-> x :products :product))
;
;(let [products  (zf/xml-> x :products :product)]
;    (map #(filter-xml % (zf/text= "img2.jpg")) x)))
;
;
;
;;
;;
;;
;
;
;(use '[defun :only [defun]])
;
;(require '[clojure.core.match :refer [match]])
;
;(let [a true 
;      b true
;      ]
;(match [ a b]
;
;       [true false] "tf"
;       [false false] "ff"
;       [_ _] "other")
;)
;
;;; works with vectors
;;; doesn't work with anything else
;(let [a ["foo"]]
;(match [ a ] 
;       [[]] "empty"
;       [_] "other"
;))
;
;(let [a ["foo"]]
;(match [ a ] 
;       [[]] "empty"
;       [_] "other"
;))
;
;;; doesn't work with anything else
;(let [a '()]
;(match [ a ]
;
;       [([] :seq)] "empty"
;       [(_ :seq)] "other"
;))
;
;(let [a [1 2]]
;(match [ a ] 
;       [[]] "empty"
;       [[1 _]] "one_"
;       [_] "other"
;))
;
;
;(let [a [ 2]]
;(match [ a ] 
;       [[]] "empty"
;       [[_ _]] "two elems"
;       [_] "other"
;))
;
;(let [x [1 2 nil nil nil]]
;  (match [x]
;    [([1] :seq)] :a0
;    [([1 2] :seq)] :a1
;    [([1 2 nil nil nil] :seq)] :a2
;    :else nil))
;
;
;(let [a '()]
;(match [ a ] 
;       [([] :seq)] "empty"
;       [([_] :seq)] "other"
;))
;
;
;(let [a {:hat "blue"}]
;(match [ a ] 
;       [([] :seq)] "empty"
;       [([_] :seq)] "list"
;       [_] "other"))
;
;(let [a {:hat "blue"}]
;  (match [ a ] 
;    [{:hat _}] "has it"))
;
;( (fun 
;    ([{:hat x}] (str "hat colored " x))
;    ({} "no hat...")
;    ) {:hat "red"})
;;; okay does use local vars.
;;; the below has no matching pattern.
;(let [x false]
;  (match [false true]
;    [_ x] "foo"
;    [false x :as a] (str a "foo")))
;
;
;(let [x "foo.txt" y true]
;  (match [x y]
;    [#".*.txt" :as f b] f
;    [a 2] a
;   :else nil))
;(defcmd samtools_view [#".*\.bam" :-S :-b
;;; defun does support regex pattern matching.
;( (fun 
;    ([#".*\.fq" :as f] f)) "foo.fq")
;
;( (fn [f] {:post [(re-find #".*\.sam" %)]} f) "foo.sam")
;( (defn foo [f] {:post [string?]} f) 3)
;;post requires weird syntax
;( (fn [f] {:post [(string? %)]} f) "foo")
;( (fun 
;    ([f] {:post [(string? %)]} f)) "foo")
;(foo 3)
;((partial re-matches #".*\.sam") "foo")
;
;
;(re-find #"ffoo" "foo")
;; extend :pre and :post to accept just regex
;; extend defun to allow pre/post conditions
;(defun samview
;  ([#".*\.bam" :as f ])
;  {:post (re-find #"*.sam" %)})
;
;( (fun
;    (["foo" :as f & {:keys [c d] :or {c 10 d 20}}] f) "foo"))
;;can't get keyword (default) args with pattern-matching that way
;((fn baz [a b & {:keys [c d] :or {c 10 d 20}}]
;         (* a b c d)) 2 3 )
;
;
;(let [x 1 y 2]
;  (match [x y]
;    [1 b] b
;    [a 2] a
;   :else nil))
;
;;; usefuls for schema validation 
;( (fun 
;    ([{:hat x}] (str "hat colored " x))
;    ({} "no hat...")
;    ) {:hat "red"})
;(
;(defun map-
;    ([f []] [])
;    ([f [x & xs]]  (cons (f x) (map- f xs))))
;
;(defun filter-
;  ([f []] [])
;  ;; below doesn't work
;  ([f [x & xs] :guard (f x)] (cons  x (filter- xs)))
;  ([f [x & xs]]  (filter- f xs)))
;
;(defun filter-
;  ([f []] [])
;  ([f [x & xs]] (let [more (filter- f xs)]
;                  (if (f x) (cons x more) more))))
;
;(defun reduce-
;  ([f v []]      v)
;  ;;passing acc is optional
;  ([f [x & xs]] (reduce- f x xs))
;  ([f v [x & xs]] (reduce- f (f v x) xs)))
;
;(defun member?
;  ([e []] false)
;  ([e [x & xs]] (or (= e x) (fst e xs))))
;
;;(in shen
;; (define member?
;;   X [] -> false
;;   X [X|_] -> true 
;;   X [_|YS] -> (member? YS))
;
;(defn fst [[x & xs]] x)
;(defun fst ([[x & xs]] x))
;
;(fst [ 1 3 4])
;(fst [])
;(member- 3 [1 2 3 4])
;(member- 0 [1 2 3 4])
;(reduce- + [1 2 3 4 5])
;reduce
;(filter- even? [ 1 2 3 4 4])
;(filter- even? [])
;(is (=
;(map #(+ 1 %) [1  2 3])
;(map- #(+ 1 %) [1  2 3])))
;;; could implement pattern caclulus in clojure?
;            (require '[clojure.core.match :refer [match]])
;;; the sleeping barber problem 
;;(def waiting (atom 0))
;;(def busy (atom false))
;;(def serviced (atom 0))
;;
;;(defn barber-done [] 
;;  (do (swap! serviced inc)
;;    (if (pos? waiting)
;;      (do
;;      (swap! waiting dec)
;;      (Thread/sleep 20)
;;        (recur))
;;    (reset! busy false)))
;;
;;
;;(def new-cust
;;  #(swap! waiting
;;         (fun 
;;           ([3] 3)
;;           ([x] (inc x)))))
;;
;;(new-cust)
;;(print waiting)
;            (require '[defun :refer [defun fun]])
;;
;;(require '[clojure.core.async :as async :refer [<! >! <!! timeout chan alt! go]])
;;
;;(do (go (<! ( doseq [i (range 10)] 
;;             (do 
;;               Thread/sleep 1000)
;;             print i)))))
;;
;;(do
;;(go (<!
;;  (let [sleep (+ 100 (rand-int 20))]
;;        (do (new-cust)
;;            (Thread/sleep sleep))
;;  (println (System/currentTimeMillis)))))
;;)))
;;
(require '[instaparse.core :as insta])


(require ['clojure.zip :as 'z])
(require '[clojure.test :refer :all])

(def fq-parse (insta/parser (clojure.java.io/file "fastq.grammar")))
 
(def raw-parse (insta/parser 
  "file : (record <'\n'>)* record <'\n'?>
  record : id <'\n'> sequence
  id : <'>'>  #'[^\n]+'
  sequence : ('A' | 'G' | 'C' | 'T' | <'\n'>)+")) 
(s 0)
(def s (vec "GGg+2acGGGGG+1AGGG+3CCCgg"))
(def num? (into #{} (map str #{0 1 2 3 4 5 6 7 8 9})))
(num? "a")
(num "0")
(def sub (partial subvec s)
(let [pluses (keep-indexed #(when (#{\+ \-} %2) %1) s)
      nums (map (comp #(Integer. %) str/join (fn [i] take-while num? (subvec s i))) pluses)
      ends (map #(+ %1 %2) nums pluses)
      starts (map #(+ 1 (count (str %1)) %2) pluses nums)
      to-fix (map sub pluses ends)
      ;; will below fail if index 0 is an insert?
      goods  (concat (sub 0 (first pluses)) (map sub ends pluses))
      fixed  (map #(str \i % \i) to-fix)]
 (interleave goods fixed))
look



      ;nums (map (comp #(Integer/parseInt %) s #(+ 1 %) ) pluses)
      ;starts (map #(+ 2 %) pluses)
      ])



(seq s)

"
ref : <'.'> | <','> 
nil : <'%'> 
indel : number = ('+'|'-') <#'[0-9]+'> 

(fq-parse
"@M02261:12:000000000-A6FJ2:1:1101:15914:1452 1:N:0:12
CTTATCTCCTGTTCCACTTCAAACAGCAGTTGTAATGCTTGCATGAATGTTATTTGTTCAAAGCTATTTTCAGTTGTTCTTAATCTGTGTCTCACTTCTTCAATTAGCCATCTTATCTCTTCAAACTTCTGACCTAGCTGTTCTCGCCATTTCCCGTTTCTGTTTTGGAGTAAGTGGAGGTCCCCCATTTTCATTACTGCTTCTCCTAGCGAATCTCTGTAGATTTTTAGAGACTCGAACTGTGTT
+
1>AA1FD3DFFFG3BBF11FGGGGHHGFHGFGGHHFGBHHFHHHFFFFF1DFGHHHGF1FFH10G1HHBHFHEHHGGHHHHH2FFGHABGAGHFHHHFHHBGFHF1GBFFHHHFEGGHBGB1B@EGGHFHFEHGGFFHFFGH2EGGGCFGFHHFBE/FBGEDEBFCB?10FFHDDDHDC//@@FGE?CG2<FGGGHEHBGFHHHHH1<>C--@>CHFHDDDBD000DG./<DDGGB..:.GFHHHH
"))

(def tr2
(fq-parse
"@EAS54_6_R1_2_1_443_348
GTTGCTTCTGGCGTGGGTGGGGGGG
+
ZZZZZZZZZZZXZVZZMVZRXRRRR
"))

(defn get-encoding [t] (-> (z/vector-zip t) z/down z/right z/right z/right z/right z/node first)) 

(is
  (= :sanger (get-encoding tr1)))

(is
  (= :illumina-1-3 (get-encoding tr2)))
j

reduce
(reduce 


try chunked parsing--probably it doesn't work properly. Look at another library?
cfg1->processing->cfg2
cfg1->processing->cfg1
cfg1->json
instaparse discussion of optimizing memory
LL/LALR1 grammar 
VCF parser header + line-by-line
pileup parser
SAM-view parser

"
; laziness in instaparse
; attempt cfg1->json
; attempt cfg1->change->cfg1, using instagenerate or instaparse
; could replace +3AAA -> +IIIAAA
; could use lookahead to ensure length of quality/match it up 

(require '[instaparse.core :as insta])
(use 'instaparse)

(def parse (insta/parser abc-grammar-map :start :S))

(use 'instaparse.combinators)
(Integer/parseInt "12")
(repeat  10 \a )
(def abc-grammar-map
  {:S (cat (look (cat (nt :A) (string "c")))
           (plus (string "a"))
           (nt :B))
   :A (cat (string "a") (opt (nt :A)) (string "b"))
   :B (hide-tag (cat (string "b") (opt (nt :B)) (string "c")))})

(parse "aabbcc" )
(parse "aabbcc" )

:S

2aa
"S = int "
((insta/parser "A = 'a' '!' 'b' | 'a' A 'b'")  "aa!bb")



(def parse' (insta/parser simple :start :S))
(def simple
  {:S #(cat % (rep (Integer/parseInt %) (Integer/parseInt %) (string "a") ))})




























