;; -*-scheme-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; This is a sample guile report generator for GnuCash.
;; It illustrates the basic techniques used to create
;; new reports for GnuCash.

(define-module (gnucash reports example portfolio-allocations))

(use-modules (gnucash engine))
(use-modules (gnucash utilities)) 
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (gnucash html))
(use-modules (srfi srfi-1))
(use-modules (ice-9 format))
(use-modules (oop goops))

(debug-enable 'backtrace)

(define optname-report-currency (N_ "Report's currency"))
(define optname-report-date (N_ "Date"))
(define optname-accounts (N_ "Accounts"))
(define optname-price-source (N_ "Price Source"))
(define optname-show-tables (N_ "Show Tables"))

(define pagename-categories (N_ "Report categories"))
(define optname-category-currencies (N_ "Currencies"))
(define optname-category-regions (N_ "Regions"))
(define optname-market-to-region-mapping (N_ "Market to Region mapping for Stock accounts"))

(define (find-stock-base-currency commodity)
  (let* ((pricedb (gnc-pricedb-get-db (gnc-get-current-book)))
         (price-list (gnc-pricedb-lookup-latest-any-currency pricedb commodity)))
    (if (eqv? price-list '())
      #f
      (let* ((price (car price-list))
             (price-currency (gnc-price-get-currency price)))
        (if (gnc-commodity-equiv commodity price-currency)
          (gnc-price-get-commodity price)
          price-currency
        )
      )
    )
  )
)

(define (convert-amount-to-currency value src-currency target-currency exchange-fn)
  (let* ((src-currency-fraction (gnc-commodity-get-fraction src-currency))
        (value-monetary (gnc:make-gnc-monetary src-currency (gnc-numeric-convert value src-currency-fraction GNC-RND-ROUND)))
        (value-in-target-currency-monetary (exchange-fn value-monetary target-currency)))
    (gnc:gnc-monetary-amount value-in-target-currency-monetary)
  )
)

(define (format-amount amount currency)
  (let ((currency-fraction (gnc-commodity-get-fraction currency)))
    (gnc:make-gnc-monetary currency (gnc-numeric-convert amount currency-fraction GNC-RND-ROUND))
  )
)

(define (val-if pred val def)
  (if (pred val)
    val
    def
  )
)

(define (first lst)
  (if lst
    (if (> (length lst) 0)
      (car lst)
      #f
    )
    #f
  )
)

(define (find-line-with-prefix lines prefix)
  (first
    (filter
      (lambda (line)
        (if (>= (string-length line) (string-length prefix))
          (string=? prefix (substring line 0 (string-length prefix)))
          #f
        )
      )
      lines
    )
  )
)

(define (filter-accounts-by-type accounts types)
  (filter
    (lambda (acct)
      (let ((type (xaccAccountGetType acct)))
        (memq type types)
      )
    )
    accounts
  )
)

;; Parses the following structure:
;; #CUR:EUR=0.6105;USD=0.2697;CHF=0.0316;GBP=0.0139
;; #TYPE:STOCK=0.5139;BOND=0.4319;REALESTATE=0.0125;CASH=-0.0212;DERIVATIVES=0.0488;COMMODITIES=0.0151
;; #REGIO:NA=0.4095;EU=0.3997;AS=0.1761
;; #SECTOR:COMM=0.0544;CONSD=0.0941;CONSS=0.0941;ENER=0.0325;FINA=0.1201;HEAL=0.1179;INDU=0.1724;IT=0.2169;MAT=0.0325;REALEST=0.0325;UTIL=0.0325
(define (parse-notes-for-fractions notes line-prefix splitter)
  (let* ((lines (string-split notes #\newline))
         (line (find-line-with-prefix lines line-prefix)))
    (if line
      (map
        ;; convert the last value to a factional number
        (lambda (x)
          (list (car x) (rationalize (inexact->exact (string->number (cadr x))) 1/1000000))
        )
        (parse-txt-line-for-mappings (substring line (string-length line-prefix) (string-length line)))
      )
      #f
    )
  )
)

;; Parses the following structure:
;; EUR=0.6105;USD=0.2697;CHF=0.0316;GBP=0.0139
(define (parse-txt-line-for-mappings line)
  ;; split by ';'. We then have "EUR=0.6105"
  (let ((fractions (string-split line #\;)))
    (val-if (lambda (x) (> (length x) 0)) ; filter out empty results
      (filter (lambda (lst) (= 2 (length lst))) ; filter out invalid lines
        (map
          ;; split on '='. We now have ("EUR", "0.6105")
          (lambda (fraction)
            (string-split fraction #\=)
          )
          fractions
        )
      )
      #f
    )
  )
)

;; Parses the following structure:
;; NYSE=NA;AEX=EU;SIX=CH;LSE=EU
(define (parse-market-to-region-map line)
  (let ((result (make-hash-table 15)))
    (for-each
      (lambda (row)
        (hash-set! result (car row) (cadr row))
      )
      (parse-txt-line-for-mappings line)
    )
    result
  )
)

(define (calculate-currency-distribution acct)
  (let ((tmp-map (make-hash-table 15))
        (type (xaccAccountGetType acct))
        (acct-commodity (xaccAccountGetCommodity acct)))
    (if (or (eq? type ACCT-TYPE-STOCK) (eq? type ACCT-TYPE-ASSET))
      (let ((notes (xaccAccountGetNotes acct)))
        (let ((fractions (parse-notes-for-fractions notes "#CUR:" ";")))
          (if fractions
            (for-each
              (lambda (row)
                (hash-set! tmp-map (car row) (cadr row))
              )
              fractions
            )
            (if (eq? type ACCT-TYPE-STOCK)
              ; for stock, we find the base currency
              (let ((base-currency (find-stock-base-currency acct-commodity)))
                (if base-currency
                  (hash-set! tmp-map (gnc-commodity-get-mnemonic base-currency) '1)
                  (hash-set! tmp-map (gnc-commodity-get-mnemonic acct-commodity) '1)
                )
              )
              ; otherwise, we take the account commodity
              (hash-set! tmp-map (gnc-commodity-get-mnemonic acct-commodity) '1)
            )
          )
        )
      )
      (hash-set! tmp-map (gnc-commodity-get-mnemonic acct-commodity) '1)
    )
    tmp-map
  )
)

(define (calculate-asset-distribution acct)
  (let ((tmp-map (make-hash-table 15))
        (type (xaccAccountGetType acct)))
    (if (or (eq? type ACCT-TYPE-STOCK) (eq? type ACCT-TYPE-ASSET))
      (let ((notes (xaccAccountGetNotes acct)))
        (let ((fractions (parse-notes-for-fractions notes "#TYPE:" ";")))
          (if fractions
            (for-each
              (lambda (row)
                (hash-set! tmp-map (car row) (cadr row))
              )
              fractions
            )
            (if (eq? type ACCT-TYPE-STOCK)
              (hash-set! tmp-map "STOCK" '1) ; assume all STOCK for stocks.
              #f
            )
          )
        )
      )
      (if (eq? type ACCT-TYPE-BANK)
        (hash-set! tmp-map "CASH" '1)
        #f
      )
    )
    tmp-map
  )
)

(define (calculate-region-distribution acct market-to-region-map)
  (let* ((tmp-map (make-hash-table 15))
         (type (xaccAccountGetType acct))
         (notes (xaccAccountGetNotes acct))
         (fractions        (parse-notes-for-fractions notes "#REGION:" ";")))
        (if fractions
          (for-each
            (lambda (row)
              (hash-set! tmp-map (car row) (cadr row))
            )
            fractions
          )
          ; for STOCK, try to take the market from the commodity
          (if (eq? type ACCT-TYPE-STOCK)
            (let ((market (gnc-commodity-get-namespace (xaccAccountGetCommodity acct))))
              (hash-set! tmp-map (cdr (hash-create-handle! market-to-region-map market #f)) '1)
            )
            #f
          )
        )
    tmp-map
  )
)

(define (calculate-generic-distribution acct line-prefix)
  (let* ((tmp-map (make-hash-table 15))
         (notes (xaccAccountGetNotes acct))
         (fractions (parse-notes-for-fractions notes line-prefix ";")))
    (if fractions
      (for-each
        (lambda (row)
          (hash-set! tmp-map (car row) (cadr row))
        )
        fractions
      )
      #f
    )
    tmp-map
  )
)

(define (calculate-market-distribution acct)
  (calculate-generic-distribution acct "#MARKET_TYPE:")
)

(define (calculate-size-distribution acct)
  (calculate-generic-distribution acct "#SIZE_FACTOR:")
)

(define (calculate-value-distribution acct)
  (calculate-generic-distribution acct "#VALUE_FACTOR:")
)

;; Analyzes a set of accounts in categories
;; accounts                     - the accounts to analyze
;; accounts-relevant-fractions  - the relevant fraction of the amount of the account
;; categories                   - a list of the categories into which to categorize 
;; distribution-fn              - the function that will be used to categorize the money in the accounts
;; report-currency              - the currency to print the report in
;; report-date                  - the date for which the report is generated
;; exchange-fn                  - the exchange function to be used when converting currencies
(define (analyze-categories accounts accounts-relevant-fractions categories distribution-fn report-currency report-date exchange-fn)
  (let ((tmp-map             (make-hash-table 15)))
    (filter identity ; filters out the #f elements
      (map
        (lambda (acct acct-relev-frac)
          (let ((balance (* acct-relev-frac (xaccAccountGetBalanceAsOfDate acct report-date)))
                (acct-commodity (xaccAccountGetCommodity acct)))
            (if (= balance 0)
              #f ; we filter them out later
              (let ((distr (distribution-fn acct))
                    (balance-in-report-currency (convert-amount-to-currency balance acct-commodity report-currency exchange-fn))
                    (fractioned-amount 0))
                (append
                  (list (gnc-account-get-full-name acct) (format-amount balance acct-commodity))
                  (map
                    (lambda (category)
                      (let* ((fraction (hash-create-handle! distr category 0))
                             (amount-in-fraction (* balance-in-report-currency (cdr fraction))))
                        (set! fractioned-amount (+ fractioned-amount amount-in-fraction))
                        (format-amount amount-in-fraction report-currency)
                      )
                    )
                    categories
                  )
                  (list (format-amount (- balance-in-report-currency fractioned-amount) report-currency))
                )
              )
            )
          )
        )
        accounts
        accounts-relevant-fractions 
      )
    )
  )
)

(define (analyze-assets accounts asset-categories report-currency report-date exchange-fn)
  (analyze-categories accounts (make-list (length accounts) 1) asset-categories calculate-asset-distribution report-currency report-date exchange-fn)
)

(define (analyze-currencies accounts currency-categories report-currency report-date exchange-fn)
  (analyze-categories accounts (make-list (length accounts) 1) currency-categories calculate-currency-distribution report-currency report-date exchange-fn)
)

(define (analyze-markets stock-accounts account-stock-fractions market-categories report-currency report-date exchange-fn)
  (analyze-categories stock-accounts account-stock-fractions market-categories calculate-market-distribution report-currency report-date exchange-fn)
)

(define (analyze-regions stock-accounts account-stock-fractions region-categories market-to-region-map report-currency report-date exchange-fn)
  (analyze-categories 
    stock-accounts
    account-stock-fractions
    region-categories
    (lambda (acct)
      (calculate-region-distribution acct market-to-region-map)
    )
    report-currency
    report-date
    exchange-fn
  )
)

(define (analyze-size-factor stock-accounts account-stock-fractions size-categories report-currency report-date exchange-fn)
  (analyze-categories stock-accounts account-stock-fractions size-categories calculate-size-distribution report-currency report-date exchange-fn)
)

(define (analyze-value-factor stock-accounts account-stock-fractions value-categories report-currency report-date exchange-fn)
  (analyze-categories stock-accounts account-stock-fractions value-categories calculate-value-distribution report-currency report-date exchange-fn)
)

(define (asset-category-to-name cat)
  (cond ((string=? "STOCK" cat) (G_ "Stock"))
        ((string=? "BOND" cat) (G_ "Bonds"))
        ((string=? "DERIVATIVES" cat) (G_ "Derivatives"))
        ((string=? "CASH" cat) (G_ "Cash"))
        ((string=? "COMMODITIES" cat) (G_ "Commodities"))
        ((string=? "REALESTATE" cat) (G_ "Real Estate"))
        (else (G_ "Unknown Category!!!"))
  )
)

(define (region-category-to-name cat)
  (cond ((string=? "NA" cat) (G_ "North America"))
        ((string=? "LA" cat) (G_ "Latin America"))
        ((string=? "EU" cat) (G_ "European Union"))
        ((string=? "CH" cat) (G_ "Switzerland"))
        ((string=? "AS" cat) (G_ "Asia"))
        (else (G_ "Unknown Category!!!"))
  )
)

(define (market-category-to-name cat)
  (cond ((string=? "DEVELOPED" cat) (G_ "Developed Markets"))
        ((string=? "EMERGING" cat) (G_ "Emerging Markets"))
        (else (G_ "Unknown Category!!!"))
  )
)

(define (value-category-to-name cat)
  (cond ((string=? "VALUE" cat) (G_ "Value"))
        ((string=? "GROWTH" cat) (G_ "Growth"))
        (else (G_ "Unknown Category!!!"))
  )
)

(define (size-category-to-name cat)
  (cond ((string=? "GIANT" cat) (G_ "Giant"))
        ((string=? "LARGE" cat) (G_ "Large"))
        ((string=? "MID" cat) (G_ "Mid"))
        ((string=? "SMALL" cat) (G_ "Small"))
        ((string=? "MICRO" cat) (G_ "Micro"))
        (else (G_ "Unknown Category!!!"))
  )
)

(define (calculate-totals table startcol colcount report-currency exchange-fn)
  (let* ((collectors (make-list colcount 0))
         (collectors (map (lambda (t) (gnc:make-commodity-collector)) collectors)))
    (for-each
      (lambda (row)
        (map
          (lambda (amount collector)
            (collector 'add (gnc:gnc-monetary-commodity amount) (gnc:gnc-monetary-amount amount))
            #f ; return something. We don't really want to map here....
          )
          (take (drop row startcol) colcount)
          collectors
        )
      )
      table
    )
    (map
      (lambda (collector)
        (gnc:sum-collector-commodity collector report-currency exchange-fn)
      )
      collectors
    )
  )
)

(define (calculate-percentages grand-total totals)
  (let ((grand-total-val (gnc:gnc-monetary-amount grand-total)))
    (map
      (lambda (t)
        (let ((amt (gnc:gnc-monetary-amount t)))
          (if (= amt 0)
            0
            (/ amt grand-total-val)
          )
        )
      )
      totals
    )
  )
)

(define (order-by-percentage titles values percentages)
  (sort
    (map
      (lambda (title value percentage)
        (list title value percentage)
      )
      titles
      values
      percentages
    )
    (lambda (r1 r2)
      (> (caddr r1) (caddr r2))
    )
  )
)

;; Renders a set of allocation data as a pie-chart + a table.
;; document         - the document to render to
;; title            - the title of the pie chart
;; show-tables      - whether to show table or only pie chart
;; categories       - a list of the categories. These will be used in the headers of the table. Must match the 'n' of category-data below.
;; category-data    - the main data to be displayed. Must be a table, with one row per account. Each row must have the following fields:
;;                    ( account name, account value in account commodity, value-cat-1, value-cat-2 ... value-cat-n, value in 'other' category)
;; report-currency  - the currency to print the report in
;; report-date      - the date for which the report is generated
;; exchange-fn      - the exchange function to be used when converting currencies
;; cat-display-fn   - a function to create a display name from the category keys from the 'categories' variable
(define (render-category-allocation document title show-tables categories category-data report-currency report-date exchange-fn cat-display-fn)
  (let* ((table                    (gnc:make-html-table))
         (report-currency-fraction (gnc-commodity-get-fraction report-currency))
         (totals                   (calculate-totals category-data 1 (+ 2 (length categories)) report-currency exchange-fn))
         (percentages              (calculate-percentages (car totals) (cdr totals))))
    (gnc:html-document-add-object! document (gnc:make-html-text "<div class=\"graph\">"))
    (gnc:html-document-add-object! document
      (let ((chart (gnc:make-html-chart)))
        (gnc:html-chart-set-type! chart 'doughnut)

        ;; title is either a string, or a list of strings
        (gnc:html-chart-set-title! chart title)
        (gnc:html-chart-set-width! chart '(percent . 100))
        (gnc:html-chart-set-height! chart '(pixels . 400))
        (gnc:html-chart-set! chart '(options chartArea backgroundColor) "white")
        ; (gnc:html-chart-set! chart '(options animation duration) 500)

        ;; data-labels and data-series should be the same length
        (let ((ordered (order-by-percentage 
                (append (map cat-display-fn categories) (list "Other"))
                (cdr totals)
                percentages
              )))
          (gnc:html-chart-set-data-labels! chart
            (map
              (lambda (item) 
                (let ((category (car item))
                      (value (cadr item))
                      (percentage (caddr item)))
                  (format #f "~A - ~A (~,2f%)"
                    category
                    (gnc:monetary->string value)
                    (* 100 percentage)
                  )
                )
              )
              ordered
            )
          )
          (gnc:html-chart-add-data-series! chart
                                          "Fraction"                                    ;series name
                                          (map caddr ordered)                           ;pie ratios
                                          (gnc:assign-colors (length category-data)))   ;colours
        )
        ;; piechart doesn't need axes display:
        (gnc:html-chart-set-axes-display! chart #f)
        ; the html-chart library actually defines a second axis for both x and y, which seems to be used as a border only. The function
        ; html-chart-set-axes-display! does NOT hide that one, resulting in a weird top and right border. Below, we hack those static
        ; axis out...
        (gnc:html-chart-set! chart '(options scales xAxes) (vector (vector-ref (gnc:html-chart-get chart '(options scales xAxes)) 0)))
        (gnc:html-chart-set! chart '(options scales yAxes) (vector (vector-ref (gnc:html-chart-get chart '(options scales yAxes)) 0)))
        chart
      )
    )
    (gnc:html-document-add-object! document (gnc:make-html-text "</div>"))
    (if show-tables
      (begin
        (gnc:html-table-set-col-headers! table (append (list (G_ "Name") (G_ "Value")) (map cat-display-fn categories) (list (G_ "Other"))))
        (for-each
          (lambda (row)
            (let* ((name (car row))
                    (value (cadr row))
                    (fractions (cddr row)))
              (gnc:html-table-append-row!
                table
                (map
                  gnc:make-html-table-cell/markup
                  (append (list "text-cell" "number-cell") (make-list (length categories) "number-cell") (list "number-cell"))
                  (append (list name value) fractions)
                )
              )
            )
          )
          category-data
        )
        (gnc:html-table-append-row/markup! table "grand-total" (list (gnc:make-html-table-cell/size 1 (+ 3 (length categories)) (gnc:make-html-text (gnc:html-markup-hr)))))
        (gnc:html-table-append-row/markup! table "grand-total"
          (append
            (list (gnc:make-html-table-cell/markup "total-label-cell" "Total"))
            (map
              (lambda (sum)
                (gnc:make-html-table-cell/markup "total-number-cell" sum)
              )
              totals
            )
          )
        )
        (gnc:html-document-add-object! document (gnc:make-html-text "<div class=\"data-table\">"))
        (gnc:html-document-add-object! document table)
        (gnc:html-document-add-object! document (gnc:make-html-text "</div>"))
      )
    )
  )
)

;; Calculates for all accounts how many of the value is in stocks. Accounts should only be STOCK or ASSET accounts!!
(define (calculate-stock-fractions accounts)
  (map
    (lambda (acct)
      (let* ((type                    (xaccAccountGetType acct))
             (asset-distr             (calculate-asset-distribution acct))
             (stock-fraction-fallback (if (eq? type ACCT-TYPE-STOCK) '1 '0)))
        (if asset-distr
          (cdr (hash-create-handle! asset-distr "STOCK" stock-fraction-fallback))
          stock-fraction-fallback
        )
      )
    )
    accounts
  )
)

;; This function will generate a set of options that GnuCash
;; will use to display a dialog where the user can select
;; values for your report's parameters.
(define (options-generator)    
  (let* ((options (gnc:new-options)) 
         ;; This is just a helper function for making options.
         ;; See libgnucash/app-utils/options.scm for details.
         (add-option 
          (lambda (new-option)
            (gnc:register-option options new-option))))
    
    ;; General tab

      ;; Report's currency
      (gnc:options-add-currency! options gnc:pagename-general optname-report-currency "c")
      ;; Report date
      (gnc:options-add-report-date! options gnc:pagename-general optname-report-date "a")
      ;; Price source
      (add-option (gnc:make-multichoice-option gnc:pagename-general optname-price-source "d" (N_ "The source of price information.") 'pricedb-nearest
          (list 
            (vector 'average-cost
              (N_ "Average cost")
              (N_ "The volume-weighted average cost of purchases."))
            (vector 'weighted-average
              (N_ "Weighted average")
              (N_ "The weighted average of all currency transactions of the past."))
            (vector 'pricedb-latest
              (N_ "Most recent")
              (N_ "The most recent recorded price."))
            (vector 'pricedb-nearest
              (N_ "Nearest in time")
              (N_ "The price recorded nearest in time to the report date."))
          )
        )
      )
      ;; Show tables
      (add-option (gnc:make-simple-boolean-option gnc:pagename-general optname-show-tables "a" (N_ "Show tables with detailed information.") #t))

    ;; Categories tab

      ;; Currencies to group by
      (add-option (gnc:make-string-option pagename-categories optname-category-currencies "c" (N_ "A semicolon-separated list of currencies to group by.") (N_ "EUR;USD;CHF;GBP")))
      ;; Regions to group by
      (add-option (gnc:make-string-option pagename-categories optname-category-regions "c" (N_ "A semicolon-separated list of regions to group by.") (N_ "NA;LA;EU;CH;AS")))
      (add-option (gnc:make-string-option pagename-categories optname-market-to-region-mapping "c" (N_ "A semicolon-separated list of mappings from markets to regions.") (N_ "NYSE=NA;AIX=EU;NASDAQ=NA")))

    ;; Account tab

    ;; This is an account list option. The user can select one
    ;; or (possibly) more accounts from the list of accounts
    ;; in the current file. Values are scheme handles to actual
    ;; C pointers to accounts. 
    ;; The #f value indicates that any account will be accepted.
    ;; Instead of a #f values, you could provide a function that
    ;; accepts a list of account values and returns a pair. If
    ;; the first element is #t, the second element is the list
    ;; of accounts actually accepted. If the first element is
    ;; #f, the accounts are rejected and the second element is
    ;; and error string. The last argument is #t which means
    ;; the user is allowed to select more than one account.
    ;; The default value for this option is the currently
    ;; selected account in the main window, if any.
    (add-option
     (gnc:make-account-list-option
      gnc:pagename-accounts optname-accounts
      "g" (N_ "Report on these accounts.")
      ;; FIXME : this used to be gnc:get-current-accounts, but 
      ;; that doesn't exist any more.
      (lambda () '())
      #f #t))
    
    (gnc:options-set-default-section options gnc:pagename-accounts)      
    options))

;; This is the rendering function. It accepts a database of options
;; and generates an object of type <html-document>.  See the file
;; report-html.txt for documentation; the file report-html.scm
;; includes all the relevant Scheme code. The option database passed
;; to the function is one created by the options-generator function
;; defined above.
(define (portfolio-allocations-renderer report-obj)
  ;; These are some helper functions for looking up option values.
  (define (get-option section name)
    (gnc:option-value 
     (gnc:lookup-option (gnc:report-options report-obj) section name)))

  ;; The first thing we do is make local variables for all the specific
  ;; options in the set of options given to the function. This set will
  ;; be generated by the options generator above.
  (let ((accounts            (get-option gnc:pagename-accounts optname-accounts))
        (report-currency     (get-option gnc:pagename-general optname-report-currency))
        (report-date         (gnc:time64-end-day-time (gnc:date-option-absolute-time (get-option gnc:pagename-general optname-report-date))))
        (price-source        (get-option gnc:pagename-general optname-price-source))
        (show-tables         (get-option gnc:pagename-general optname-show-tables))
        (asset-categories    (list "STOCK" "BOND" "DERIVATIVES" "CASH" "COMMODITIES" "REALESTATE"))
        (region-categories   (string-split (get-option pagename-categories optname-category-regions) #\;))
        (currency-categories (string-split (get-option pagename-categories optname-category-currencies) #\;))
        (market-region-map   (parse-market-to-region-map (get-option pagename-categories optname-market-to-region-mapping)))
        (market-categories   (list "DEVELOPED" "EMERGING"))
        (value-categories    (list "VALUE" "GROWTH"))
        (size-categories     (list "MICRO" "SMALL" "MID" "LARGE" "GIANT"))
        (document            (gnc:make-html-document)))

    ;; these are samples of different date options. for a simple
    ;; date with day, month, and year but no time you should use
    ;; qof-print-date
    (let ((time-string (gnc-print-time64 report-date "%c"))
          (exchange-fn         (gnc:case-exchange-fn price-source report-currency report-date)))

      (gnc:html-document-set-title! document (G_ "Portfolio Allocations"))

      (gnc:html-document-add-object!
        document
        (gnc:make-html-text
          (gnc:html-markup-p (gnc:html-markup/format (G_ "Report date: ~a.") (gnc:html-markup-b time-string)))
        )
      )

      (if show-tables
        (gnc:html-document-add-object! document (gnc:make-html-text "<div class=\"container\" style=\"display: grid; grid-template-columns: 1fr;\">"))
        (gnc:html-document-add-object! document (gnc:make-html-text "<div class=\"container\" style=\"display: grid; grid-template-columns: 1fr 1fr;\">"))
      )
      
      (if (not (null? accounts))
        (begin
          (let* ((stock-accounts          (filter-accounts-by-type accounts (list ACCT-TYPE-STOCK ACCT-TYPE-ASSET)))
                 (account-stock-fractions (calculate-stock-fractions stock-accounts))
                 (asset-category-data     (analyze-assets       accounts asset-categories report-currency report-date exchange-fn))
                 (currency-category-data  (analyze-currencies   accounts currency-categories report-currency report-date exchange-fn))
                 (region-category-data    (analyze-regions      stock-accounts account-stock-fractions region-categories market-region-map report-currency report-date exchange-fn))
                 (market-category-data    (analyze-markets      stock-accounts account-stock-fractions market-categories report-currency report-date exchange-fn))
                 (value-category-data     (analyze-value-factor stock-accounts account-stock-fractions value-categories report-currency report-date exchange-fn))
                 (size-category-data      (analyze-size-factor  stock-accounts account-stock-fractions size-categories report-currency report-date exchange-fn)))
            (render-category-allocation document (G_ "Asset Types") show-tables asset-categories asset-category-data report-currency report-date exchange-fn asset-category-to-name)
            (render-category-allocation document (G_ "Currencies") show-tables currency-categories currency-category-data report-currency report-date exchange-fn identity)
            (render-category-allocation document (G_ "Regions (Stock)") show-tables region-categories region-category-data report-currency report-date exchange-fn region-category-to-name)
            (render-category-allocation document (G_ "Market Types (Stock)") show-tables market-categories market-category-data report-currency report-date exchange-fn market-category-to-name)
            (render-category-allocation document (G_ "Value Factor (Stock)") show-tables value-categories value-category-data report-currency report-date exchange-fn value-category-to-name)
            (render-category-allocation document (G_ "Size Factor (Stock)") show-tables size-categories size-category-data report-currency report-date exchange-fn size-category-to-name)
          )
        )
        (gnc:html-document-add-object!
          document
          (gnc:make-html-text (gnc:html-markup-p (G_ "You have selected no accounts.")))
        )
      )
      (gnc:html-document-add-object! document (gnc:make-html-text "</div>"))

      document
    )
  )
)

;; Here we define the actual report with gnc:define-report
(gnc:define-report
 
 ;; The version of this report.
 'version 1
 
 ;; The name of this report. This will be used, among other things,
 ;; for making its menu item in the main menu. You need to use the
 ;; untranslated value here!
 'name (N_ "Portfolio Allocations")

 ;; The GUID for this report. This string should be unique, set once
 ;; and left alone forever after that. In theory, you could use any
 ;; unique string, even a meaningful one (!) but its probably best to
 ;; use a true uuid. Get them from `uuidgen | sed -e s/-//g` and paste
 ;; the results in here. You must make a new guid for each report!
 'report-guid "07fd12b9da4d4891813957d9c7469e41"

 ;; The name in the menu
 ;; (only necessary if it differs from the name)
 ;;'menu-name (N_ "Portfolio Allocations")

 ;; A tip that is used to provide additional information about the
 ;; report to the user.
 'menu-tip (N_ "Shows the distribution of the portfolio to certain assets types and categories.")

 ;; A path describing where to put the report in the menu system.
 ;; In this case, it's going under the utility menu.
 'menu-path (list gnc:menuname-example)

 ;; The options generator function defined above.
 'options-generator options-generator
 
 ;; The rendering function defined above.
 'renderer portfolio-allocations-renderer)
