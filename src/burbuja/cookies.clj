(ns burbuja.cookies
  "Extract cookies from Linux Chrome via browser_cookie3 (Python)."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]))

(defn get-cookies
  "Extract cookies for `domain` from Chrome's cookie store.
   Requires `pip install browser-cookie3`.
   Returns a seq of {:name :value :path :host} maps."
  [domain]
  (let [script (str "import browser_cookie3, json\n"
                    "cj = browser_cookie3.chrome(domain_name='" domain "')\n"
                    "print(json.dumps([{"
                    "'name':c.name,'value':c.value,'path':c.path,'host':c.domain"
                    "} for c in cj]))")
        pb (ProcessBuilder. ["python3" "-c" script])
        proc (.start pb)
        out (str/trim (slurp (.getInputStream proc)))
        err (str/trim (slurp (.getErrorStream proc)))
        exit (.waitFor proc)]
    (when-not (zero? exit)
      (throw (ex-info (str "Cookie extraction failed: " err) {:exit exit})))
    (json/read-str out :key-fn keyword)))
