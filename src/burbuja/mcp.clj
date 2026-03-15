(ns burbuja.mcp
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [burbuja.forum :as forum])
  (:import [java.io BufferedReader InputStreamReader])
  (:gen-class))

(def ^:private tools
  [{:name "read_thread"
    :description "Read a thread from burbuja.info forum. Returns posts with author, date, and content. Pass /page-N in the URL for other pages."
    :inputSchema {:type "object"
                  :properties {:thread_url {:type "string"
                                            :description "Full thread URL, e.g. https://www.burbuja.info/inmobiliaria/temas/example.12345/"}}
                  :required ["thread_url"]}}
   {:name "reply_comment"
    :description "Reply to a specific post on burbuja.info, quoting the original post."
    :inputSchema {:type "object"
                  :properties {:post_url {:type "string"
                                          :description "URL of the post to reply to, e.g. https://www.burbuja.info/inmobiliaria/temas/example.12345/post-67890 or https://www.burbuja.info/inmobiliaria/posts/67890/"}
                               :message {:type "string"
                                         :description "Reply text (BBCode supported)"}}
                  :required ["post_url" "message"]}}])

(defn- respond [id result]
  {:jsonrpc "2.0" :id id :result result})

(defn- error-response [id code message]
  {:jsonrpc "2.0" :id id :error {:code code :message message}})

(defn- tool-result [text & {:keys [error?]}]
  {:content [{:type "text" :text text}]
   :isError (boolean error?)})

(defn- handle-initialize [id _params]
  (respond id
    {:protocolVersion "2024-11-05"
     :capabilities {:tools {}}
     :serverInfo {:name "burbuja-mcp" :version "0.1.0"}
     :instructions "MCP server for burbuja.info forum. Uses Chrome cookies for authentication - make sure you're logged in to burbuja.info in Chrome."}))

(defn- handle-tools-list [id _params]
  (respond id {:tools tools}))

(defn- handle-tools-call [id {:keys [name arguments]}]
  (try
    (let [result (case name
                   "read_thread"
                   (forum/read-thread (:thread_url arguments))

                   "reply_comment"
                   (forum/reply-comment (:post_url arguments) (:message arguments))

                   (throw (ex-info (str "Unknown tool: " name) {})))]
      (respond id (tool-result result)))
    (catch Exception e
      (respond id (tool-result (str "Error: " (.getMessage e)) :error? true)))))

(defn- handle-message [msg]
  (let [{:keys [id method params]} msg]
    (case method
      "initialize"                (handle-initialize id params)
      "notifications/initialized" nil
      "tools/list"                (handle-tools-list id params)
      "tools/call"                (handle-tools-call id params)
      "ping"                      (respond id {})
      ;; unknown
      (if id
        (error-response id -32601 (str "Method not found: " method))
        nil))))

(defn- write-response [resp]
  (let [out System/out
        line (str (json/write-str resp) "\n")]
    (.write out (.getBytes line "UTF-8"))
    (.flush out)))

(defn -main [& _args]
  (let [reader (BufferedReader. (InputStreamReader. System/in))]
    (loop []
      (when-let [line (.readLine reader)]
        (when-not (str/blank? line)
          (try
            (let [msg (json/read-str line :key-fn keyword)
                  resp (handle-message msg)]
              (when resp
                (write-response resp)))
            (catch Exception e
              (binding [*out* *err*]
                (println "Error processing message:" (.getMessage e))))))
        (recur)))))
