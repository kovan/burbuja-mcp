(ns burbuja.forum
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [burbuja.cookies :as chrome])
  (:import [org.jsoup Jsoup]
           [org.jsoup.nodes Element]
           [java.net URLEncoder]))

(def ^:private ua
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36")

;; Cookie jar file shared across requests
(def ^:private cookie-jar (atom nil))

(defn- init-cookies! []
  (when-not @cookie-jar
    (let [cks (chrome/get-cookies "burbuja.info")
          f (java.io.File/createTempFile "burbuja-cookies" ".txt")]
      (.deleteOnExit f)
      (with-open [w (io/writer f)]
        (.write w "# Netscape HTTP Cookie File\n")
        (doseq [{:keys [name value path host]} cks]
          (when (seq value)
            (.write w (str host "\t"
                          (if (str/starts-with? host ".") "TRUE" "FALSE") "\t"
                          path "\t"
                          "TRUE\t"
                          "0\t"
                          name "\t"
                          value "\n")))))
      (reset! cookie-jar (.getAbsolutePath f))
      (binding [*out* *err*]
        (println (str "Loaded " (count cks) " cookies for burbuja.info"))))))

(defn- curl-request [url & {:keys [method params ajax?]}]
  (init-cookies!)
  (let [body-file (java.io.File/createTempFile "curl-body" ".html")
        args (cond-> ["curl" "-sSL"
                      "-b" @cookie-jar "-c" @cookie-jar
                      "-H" (str "User-Agent: " ua)
                      "-o" (.getAbsolutePath body-file)
                      "-w" "%{http_code}"]
               (= method :post)
               (into ["-X" "POST"])

               (seq params)
               (into ["-d" (str/join "&"
                             (map (fn [[k v]]
                                    (str (URLEncoder/encode k "UTF-8") "="
                                         (URLEncoder/encode v "UTF-8")))
                                  params))])

               ajax?
               (into ["-H" "X-Requested-With: XMLHttpRequest"])

               true
               (conj url))
        pb (ProcessBuilder. ^java.util.List args)
        proc (.start pb)
        status-str (str/trim (slurp (.getInputStream proc)))
        err (str/trim (slurp (.getErrorStream proc)))
        exit (.waitFor proc)
        body (slurp body-file)]
    (.delete body-file)
    (when-not (zero? exit)
      (binding [*out* *err*]
        (println "curl error:" err)))
    {:status (or (parse-long status-str) 0) :body body}))

(defn- get-page [url]
  (curl-request url))

(defn- post-form [url params & {:keys [ajax?]}]
  (curl-request url :method :post :params params :ajax? ajax?))

(defn- extract-csrf [body]
  (let [doc (Jsoup/parse ^String body)]
    (or (some-> (.selectFirst doc "input[name=_xfToken]") (.val))
        (some-> (.selectFirst doc "html[data-csrf]") (.attr "data-csrf")))))

;; Sentinel for preserving newlines through Jsoup text extraction
(def ^:private nl "\u2424")

(defn- extract-content
  "Extract readable text from a .bbWrapper element."
  [^Element wrapper]
  (when wrapper
    (-> (.html wrapper)
        (str/replace #"(?i)<br\s*/?>" "\n")
        (str/replace #"(?i)</(?:p|div|li)>" "\n")
        (str/replace #"<[^>]+>" "")
        (str/replace "\n" nl)
        Jsoup/parse
        (.text)
        (str/replace nl "\n")
        (str/replace #"\n{3,}" "\n\n")
        str/trim)))

(defn- parse-post [^Element article]
  (let [author (.attr article "data-author")
        post-id (str/replace (.attr article "data-content") "post-" "")
        date (or (some-> (.selectFirst article "time[datetime]") (.attr "datetime")) "")
        number (or (some-> (.selectFirst article ".message-attribution-opposite a:last-child") (.text)) "")
        wrapper (.selectFirst article ".bbWrapper")
        content (extract-content wrapper)]
    {:author author
     :post-id post-id
     :date date
     :number number
     :content (or content "")}))

(defn- format-post [{:keys [author post-id date number content]}]
  (str "### " (when (seq number) (str "#" number " - "))
       author
       (when (seq date) (str " (" date ")"))
       " [post:" post-id "]\n\n"
       content))

(defn list-alerts []
  (let [base "https://www.burbuja.info/inmobiliaria/account/alerts"
        pages (for [p [1 2 3]]
                (let [url (if (= p 1) base (str base "?page=" p))
                      resp (get-page url)]
                  (when (= 200 (:status resp))
                    (Jsoup/parse ^String (:body resp)))))
        alerts (for [doc pages
                     :when doc
                     li (.select doc "li[data-alert-id]")
                     :let [body (.text li)
                           is-quote (str/includes? body "citó tu mensaje")
                           is-reply (str/includes? body "respondió al tema")
                           post-link (some-> (.selectFirst li "a.fauxBlockLink-blockLink")
                                             (.attr "href"))
                           user (some-> (.selectFirst li "a.username") (.text))
                           time-el (some-> (.selectFirst li "time") (.attr "data-short"))]
                     :when (and (or is-quote is-reply) (seq post-link))]
                 {:user (or user "?")
                  :type (if is-quote "quote" "reply")
                  :time (or time-el "?")
                  :url (if (str/starts-with? post-link "http")
                         post-link
                         (str "https://www.burbuja.info" post-link))})]
    (str "# Alerts (" (count alerts) " quotes/replies)\n\n"
         (str/join "\n" (map (fn [{:keys [user type time url]}]
                               (str "[" time "] " user " (" type "): " url))
                             alerts)))))

(defn list-new-posts []
  (let [resp (get-page "https://www.burbuja.info/inmobiliaria/whats-new/posts/")]
    (when-not (= 200 (:status resp))
      (throw (ex-info (str "HTTP " (:status resp) " fetching new posts") {})))
    (let [doc (Jsoup/parse ^String (:body resp))
          links (.select doc "a[data-preview-url]")
          seen (atom #{})
          threads (for [^Element a links
                        :let [href (.attr a "href")
                              title (str/trim (.text a))
                              clean (str/replace href #"(page-\d+|post-\d+|unread|/latest)/?$" "")]
                        :when (and (re-find #"/temas/" href)
                                   (seq title)
                                   (not (contains? @seen clean)))]
                    (do (swap! seen conj clean)
                        {:title title
                         :url (if (str/starts-with? clean "http")
                                clean
                                (str "https://www.burbuja.info" clean))}))]
      (str "# New posts (" (count threads) " threads)\n\n"
           (str/join "\n" (map-indexed
                            (fn [i {:keys [title url]}]
                              (str (inc i) ". " title "\n   " url))
                            threads))))))

(defn list-trending []
  (let [resp (get-page "https://www.burbuja.info/inmobiliaria/trending/")]
    (when-not (= 200 (:status resp))
      (throw (ex-info (str "HTTP " (:status resp) " fetching trending") {})))
    (let [doc (Jsoup/parse ^String (:body resp))
          items (.select doc "a[data-preview-url]")
          threads (for [^Element a items
                        :let [href (.attr a "href")
                              title (str/trim (.text a))]
                        :when (and (re-find #"/temas/" href) (seq title))]
                    {:title title
                     :url (if (str/starts-with? href "http")
                            href
                            (str "https://www.burbuja.info" href))})]
      (str "# Trending threads (" (count threads) ")\n\n"
           (str/join "\n" (map-indexed
                            (fn [i {:keys [title url]}]
                              (str (inc i) ". " title "\n   " url))
                            threads))))))

(defn read-thread [thread-url]
  (let [resp (get-page thread-url)]
    (when-not (= 200 (:status resp))
      (throw (ex-info (str "HTTP " (:status resp) " fetching thread") {:url thread-url})))
    (let [doc (Jsoup/parse ^String (:body resp))
          title (or (some-> (.selectFirst doc "h1.p-title-value") (.text)) "Untitled")
          articles (.select doc "article.message--post")
          posts (mapv parse-post articles)
          last-page-el (.selectFirst doc ".pageNav-page:last-child a")
          total-pages (or (some-> last-page-el (.text) parse-long) 1)
          current-page-el (.selectFirst doc ".pageNav-page--current a, .pageNav-page--current span")
          current-page (or (some-> current-page-el (.text) parse-long) 1)]
      (str "# " title "\n"
           "Page " current-page " of " total-pages
           " (" (count posts) " posts on this page)\n\n"
           (str/join "\n\n---\n\n" (map format-post posts))))))

(defn reply-comment [post-url message]
  (let [resp (get-page post-url)]
    (when-not (= 200 (:status resp))
      (throw (ex-info (str "HTTP " (:status resp) " fetching post") {:url post-url})))
    (let [doc (Jsoup/parse ^String (:body resp))
          ;; Extract post ID from URL
          post-id (or (second (re-find #"post-(\d+)" post-url))
                      (second (re-find #"posts/(\d+)" post-url))
                      (throw (ex-info "Cannot extract post ID from URL" {:url post-url})))
          ;; Find the post on the page
          post-el (or (.selectFirst doc (str "article[data-content=post-" post-id "]"))
                      (.selectFirst doc (str "#js-post-" post-id))
                      (throw (ex-info (str "Post " post-id " not found on page") {})))
          author (.attr post-el "data-author")
          content (or (some-> (.selectFirst post-el ".bbWrapper") (.text)) "")
          ;; Truncate quoted content if very long
          quoted (if (> (count content) 500)
                   (str (subs content 0 500) "...")
                   content)
          ;; CSRF token
          csrf (or (extract-csrf (:body resp))
                   (throw (ex-info "No CSRF token on page" {})))
          ;; Reply URL from quick-reply form
          reply-form (.selectFirst doc "form.js-quickReply")
          reply-url (cond
                      reply-form
                      (let [action (.attr reply-form "action")]
                        (if (str/starts-with? action "http")
                          action
                          (str "https://www.burbuja.info" action)))

                      :else
                      (let [canonical (some-> (.selectFirst doc "link[rel=canonical]")
                                              (.attr "href"))
                            thread-base (when canonical
                                          (second (re-find #"(.*?/temas/[^/]+/)" canonical)))]
                        (if thread-base
                          (str thread-base "add-reply")
                          (throw (ex-info "Cannot determine reply URL - thread may be locked" {})))))
          ;; Build BBCode with quote
          bbcode (str "[QUOTE=\"" author ", post: " post-id "\"]\n"
                      quoted "\n"
                      "[/QUOTE]\n\n"
                      message)
          ;; POST the reply
          reply-resp (post-form reply-url
                       {"message" bbcode
                        "_xfToken" csrf
                        "_xfRequestUri" (or (some-> (.selectFirst doc "link[rel=canonical]")
                                                    (.attr "href"))
                                            post-url)
                        "_xfNoRedirect" "1"
                        "_xfResponseType" "json"}
                       :ajax? true)]
      (if (<= 200 (:status reply-resp) 303)
        (str "Reply posted successfully to post " post-id " by " author ".")
        (throw (ex-info (str "Reply failed: HTTP " (:status reply-resp))
                        {:body (subs (or (:body reply-resp) "")
                                     0 (min 500 (count (or (:body reply-resp) ""))))}))))))
