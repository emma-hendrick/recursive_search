; Define the namespace
(ns search)

; Dependencies
(require '[clojure.java.io :as io])
(use 'clojure.string)

; Clear the screen
(print (str (char 27) "[2J"))
(print (str (char 27) "[;H"))

; A function to read the nth line of a file, to load variables
;; .env should be in the recursive_search folder, not in src
;; .env should be a list of file extensions, each on its own line, with no commas seperating them
(defn read_variables [] 
    (split-lines (slurp "./.env")))

; Set up constants, namely, the directories that can be read from
(def dir_to_ignore (list "node_modules" ".git"))
(def filetypes_to_search (read_variables))
(declare handle_folder_contents)

; Find the index of a string in an array
(defn index_of_array [item coll]
  (count (take-while (partial not= item) coll)))

; Return true if elm is in coll
(defn in? [coll elm]  
  (some #(= elm %) coll))

; Check if the files type is in the list of filetypes to search
(defn filetype_match? [file]
    (some #(ends-with? (.getName file) %) filetypes_to_search))

; Recursively search through a folder and list all files within it
(defn search_folder [s_dir s_term]
    (def files (.listFiles (io/file s_dir)))
    (if (= (count files) 0)
        "the folder was empty, so we should not attempt to handle its contents as there are no contents to handle"
        (handle_folder_contents (filter #(not (in? dir_to_ignore (.getName %))) files) s_term)))

; Search through a file one line at a time
(defn search_line [filename contents s_term]
    (def trimmed_lines (map trim (split-lines contents)))
    (def matching_lines 
        (filter #(includes? % s_term) 
            (filter #(not (starts-with? (subs % 50) "//")) 
                (map #(str (format "%-30s%-20s" filename (str " line " (+ (index_of_array % trimmed_lines) 1))) %) trimmed_lines))))
    (if (not (empty? matching_lines))
        (run! println matching_lines)
        ()))

; Go through each line of a file and list all occurences of a string in the file
(defn search_file [file s_term] 
    (if (filetype_match? file)
        (search_line (.getName file) (slurp file) s_term)
        ()))

; Handle folder contents
(defn handle_folder_contents [files s_term]
    (loop [decumulator files accumulator (count files)]
        (if (.isDirectory (first decumulator))
            (search_folder (first decumulator) s_term)
            (search_file (first decumulator) s_term))
        (if (= (count decumulator) 1) 
            accumulator
            (recur (drop 1 decumulator) (+ accumulator (cond (.isDirectory (first decumulator)) (count (.listFiles (first decumulator))) :else 1))))))

; Search using defined directories
(defn search [s_dir s_term] 
    (newline)
    (println (str "Searching for" " '" s_term "' " "in") s_dir)
    (newline)
    (search_folder s_dir s_term)
    (newline))

; Search repeatedly
(defn repeated_search []
    (print "Enter directory => ")
    (flush)
    (def directory (read-line))
    (print "Enter search term => ")
    (flush)
    (def term (read-line))
    (search directory term)
    (print "Search again? (Y/N) => ")
    (flush)
    (def continue (in? (list "Y" "y") (read-line)))
    (if continue (repeated_search) "All searches completed!")
    )

; Run the program using user input
(defn -main []
    (repeated_search))