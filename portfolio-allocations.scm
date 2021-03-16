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

(define pagename-categories (N_ "Report categories"))
(define optname-category-currencies (N_ "Currencies"))

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

;; Parses the following structure:
;; #CUR:EUR=0.6105;USD=0.2697;CHF=0.0316;GBP=0.0139
;; #TYPE:STOCK=0.5139;BOND=0.4319;REALEST=0.0125;CASH=-0.0212;DERIV=0.0488;COMM=0.0151
;; #REGIO:NA=0.4095;EU=0.3997;AS=0.1761
;; #SECTOR:COMM=0.0544;CONSD=0.0941;CONSS=0.0941;ENER=0.0325;FINA=0.1201;HEAL=0.1179;INDU=0.1724;IT=0.2169;MAT=0.0325;REALEST=0.0325;UTIL=0.0325
(define (parse-notes-for-fractions notes line-prefix splitter)
  (let* ((lines (string-split notes #\newline))
         (line (find-line-with-prefix lines line-prefix)))
    (if line
      ;; split by ';'. We then have "EUR=0.6105"
      (let ((fractions (string-split (substring line (string-length line-prefix) (string-length line)) #\;)))
        (val-if (lambda (x) (> (length x) 0)) ; filter out empty results
          (map
            ;; convert the last value to a factional number
            (lambda (x)
              (list (car x) (rationalize (inexact->exact (string->number (cadr x))) 1/10000))
            )
            (filter (lambda (lst) (= 2 (length lst))) ; filter out invalid lines
              (map
                ;; split on '='. We now have ("EUR", "0.6105")
                (lambda (fraction)
                  (string-split fraction #\=)
                )
                fractions
              )
            )
          )
          #f
        )
      )
      #f
    )
  )
)

(define (calculate-currency-distribution acct currency-categories)
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

(define (analyze-currencies accounts report-currency report-date price-source exchange-fn currency-categories)
  (let ((tmp-map             (make-hash-table 15)))
    (filter identity ; filters out the #f elements
      (map
        (lambda (acct)
          (let ((balance (xaccAccountGetBalanceAsOfDate acct report-date))
                (acct-commodity (xaccAccountGetCommodity acct)))
            (if (= balance 0)
              #f ; we filter them out later
              (let ((distr (calculate-currency-distribution acct currency-categories))
                    (balance-in-report-currency (convert-amount-to-currency balance acct-commodity report-currency exchange-fn))
                    (fractioned-amount 0))
                (append
                  (list (gnc-account-get-full-name acct) (format-amount balance acct-commodity))
                  (map
                    (lambda (currency)
                      (let* ((fraction (hash-create-handle! distr currency 0))
                             (amount-in-fraction (* balance-in-report-currency (cdr fraction))))
                        (set! fractioned-amount (+ fractioned-amount amount-in-fraction))
                        (format-amount amount-in-fraction report-currency)
                      )
                    )
                    currency-categories
                  )
                  (list (format-amount (- balance-in-report-currency fractioned-amount) report-currency))
                )
              )
            )
          )
        )
        accounts
      )
    )
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
        (/ (gnc:gnc-monetary-amount t) grand-total-val)
      )
      totals
    )
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
      ;; Currencies to group by
      (add-option (gnc:make-string-option pagename-categories optname-category-currencies "c" (N_ "A semicolon-separated list of currencies to group by.") (N_ "EUR;USD;CHF")))

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
        (currency-categories (string-split (get-option pagename-categories optname-category-currencies) #\;))
                
        ;; document will be the HTML document that we return.
        (document (gnc:make-html-document)))

    ;; these are samples of different date options. for a simple
    ;; date with day, month, and year but no time you should use
    ;; qof-print-date
    (let ((time-string (gnc-print-time64 report-date "%c"))
          (exchange-fn         (gnc:case-exchange-fn price-source report-currency report-date)))

      (gnc:html-document-set-title! document (G_ "Portfolio Allocations"))

      (gnc:html-document-add-object!
       document
       (gnc:make-html-text         
        
        (gnc:html-markup-p
         (gnc:html-markup/format
          (G_ "Report date: ~a.") 
          (gnc:html-markup-b time-string)))
            ))

      (if (not (null? accounts))
          (begin
            (let* ((table                    (gnc:make-html-table))
                   (report-currency-fraction (gnc-commodity-get-fraction report-currency))
                   (currency-data            (analyze-currencies accounts report-currency report-date price-source exchange-fn currency-categories))
                   (totals                   (calculate-totals currency-data 1 (+ 2 (length currency-categories)) report-currency exchange-fn))
                   (percentages              (calculate-percentages (car totals) (cdr totals))))
              (gnc:html-document-add-object! document
                (let ((chart (gnc:make-html-chart)))
                  ;; the minimum chartjs-based html-chart requires the following settings
                  (gnc:html-chart-set-type! chart 'pie)

                  ;; title is either a string, or a list of strings
                  (gnc:html-chart-set-title! chart "Asset Currency Allocation")
                  ; (gnc:html-chart-set-width! chart '(pixels . 480))
                  (gnc:html-chart-set-height! chart '(pixels . 400))

                  ;; data-labels and data-series should be the same length
                  (gnc:html-chart-set-data-labels! chart
                    (map
                      (lambda (category value percentage) 
                        (format #f "~A - ~A (~,2f%)"
                          category
                          (gnc:monetary->string value)
                          (* 100 percentage)
                        )
                      )
                      (append currency-categories (list "Other"))
                      (cdr totals)
                      percentages
                    )
                  )
                  (gnc:html-chart-add-data-series! chart
                                                  "Fraction"                                    ;series name
                                                  percentages                                   ;pie ratios
                                                  (gnc:assign-colors (length currency-data)))   ;colours

                  ;; piechart doesn't need axes display:
                  (gnc:html-chart-set-axes-display! chart #f) chart)
              )
              (gnc:html-table-set-col-headers! table (append (list (G_ "Name") (G_ "Value")) currency-categories (list (G_ "Other"))))
              (for-each
                (lambda (row)
                  (let* ((name (car row))
                         (value (cadr row))
                         (fractions (cddr row)))
                    (gnc:html-table-append-row!
                      table
                      (map
                        gnc:make-html-table-cell/markup
                        (append (list "text-cell" "number-cell") (make-list (length currency-categories) "number-cell") (list "number-cell"))
                        (append (list name value) fractions)
                      )
                    )
                  )
                )
                currency-data
              )
              (gnc:html-table-append-row/markup! table "grand-total" (list (gnc:make-html-table-cell/size 1 (+ 3 (length currency-categories)) (gnc:make-html-text (gnc:html-markup-hr)))))
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
              (gnc:html-document-add-object! document table)
            )
          )
          (gnc:html-document-add-object!
           document
           (gnc:make-html-text
            (gnc:html-markup-p (G_ "You have selected no accounts.")))))
      
      document)))

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
