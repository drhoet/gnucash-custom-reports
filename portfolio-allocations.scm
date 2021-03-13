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

(debug-enable 'backtrace)

(define optname-report-currency (N_ "Report's currency"))
(define optname-accounts (N_ "Accounts"))

(define (analyze-currencies accounts report-currency report-date)
  (let ((tmp-map (make-hash-table 15)))
    ;; collect per currency
    (for-each
      (lambda (acct)
        (let* ((commodity (xaccAccountGetCommodity acct))
               (value (xaccAccountGetPresentBalance acct))
               (key (gnc-commodity-get-mnemonic commodity))
               (sum (hash-get-handle tmp-map key)))
          (if sum
              (hash-set! tmp-map key (list commodity (+ (caddr sum) value)))
              (hash-set! tmp-map key (list commodity value))
          )
        )
      )
      accounts
    )
    (let ((exchange-fn (gnc:case-exchange-fn 'pricedb-latest report-currency report-date))
          (grand-total-collector (gnc:make-commodity-collector)))
      ;; calculate grand total
      (hash-for-each
        (lambda (key lst)
          (let ((currency (car lst))
                (sum (cadr lst)))
            (grand-total-collector 'add currency sum)
          )
        )
        tmp-map
      )
      ;; convert to table rows
      (let* ((grand-total-monetary (gnc:sum-collector-commodity grand-total-collector report-currency exchange-fn))
             (grand-total (gnc:gnc-monetary-amount grand-total-monetary)))
        (hash-map->list
          (lambda (key lst)
            (let* ((currency (car lst))
                  (currency-fraction (gnc-commodity-get-fraction currency))
                  (sum (cadr lst))
                  (sum-monetary (gnc:make-gnc-monetary currency (gnc-numeric-convert sum currency-fraction GNC-RND-ROUND)))
                  (sum-in-report-currency-monetary (exchange-fn sum-monetary report-currency))
                  (sum-in-report-currency (gnc:gnc-monetary-amount sum-in-report-currency-monetary)))
              (list currency sum-monetary sum-in-report-currency-monetary (/ sum-in-report-currency grand-total))
            )
          )
          tmp-map
        )
      )
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
      (gnc:options-add-currency! 
      options gnc:pagename-general optname-report-currency "c")

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
  (let ((accounts        (get-option gnc:pagename-accounts optname-accounts))
        (report-currency (get-option gnc:pagename-general optname-report-currency))
                
        ;; document will be the HTML document that we return.
        (document (gnc:make-html-document)))

    ;; these are samples of different date options. for a simple
    ;; date with day, month, and year but no time you should use
    ;; qof-print-date
    (let ((time-string (gnc-print-time64 (current-time) "%X")))

      ;; Here's where we fill the report document with content.  We
      ;; do this by adding 'html objects' such as text, tables, and
      ;; graphs to the html document we already created.
      
      ;; the title of the report will be rendered by the 
      ;; selected style sheet.  All we have to do is set it in the
      ;; HTML document.
      
      ;; Note we invoke the _ function upon this string.
      ;; The _ function works the same way as in C -- if a
      ;; translation of the given string is available for the
      ;; current locale, then the translation is returned,
      ;; otherwise the original string is returned.
      (gnc:html-document-set-title! document (G_ "Portfolio Allocations"))

      ;; we make a "text object" to add a bunch of text to.
      ;; the function gnc:make-html-text can take any number of 
      ;; arguments.  The gnc:html-markup functions are designed
      ;; to work with the style system so that you can control
      ;; the appearance of the report from the Gnucash UI; you 
      ;; should use the HTML markup functions whenever possible
      ;; rather than including literal HTML in your report.

      (gnc:html-document-add-object!
       document
       (gnc:make-html-text         
        
        (gnc:html-markup-p
         (gnc:html-markup/format
          (G_ "The current time is ~a.") 
          (gnc:html-markup-b time-string)))


        ;; Here we print the value of the number option formatted as
        ;; currency. When printing currency values, you should use
        ;; the function (xaccPrintAmount), which is defined in
        ;; report-utilities. This functions will format the number
        ;; appropriately in the current locale. Don't try to format
        ;; it yourself -- it will be wrong in other locales.
        ; (gnc:html-markup-p 
        ;  (gnc:html-markup/format
        ;   (G_ "The number option formatted as currency is ~a.")
        ;   (gnc:html-markup-b
        ;    (xaccPrintAmount
        ;     (inexact->exact num-val)
        ;     (gnc-default-print-info #f)))))
            ))

      ;; here's a bullet list of accounts.  We can mark up the
      ;; account name with an <a></a> anchor with a special HREF to
      ;; open a Gnucash register when the link is clicked.  What you
      ;; need to do is pass the HREF "gnc-register:account=My
      ;; Account Name" to html-markup-anchor.  The account name
      ;; passed must be the "full" account name that you get from
      ;; gnc-account-get-full-name.  You should build this url using
      ;; (gnc-build-url ...)
      ;;
      ;; html-markup-anchor takes the link to jump to as its first
      ;; arg and then puts the remaining args in the body of the
      ;; link).
      ;;
      ;; html-markup-ul makes a "<ul>" unnumbered list, and takes as
      ;; its one argument a list of items to put in <li> blocks.
      (if (not (null? accounts))
          (begin
            (let ((table (gnc:make-html-table))
                  (report-currency-fraction (gnc-commodity-get-fraction report-currency))
                  (currency-data (analyze-currencies accounts report-currency current-time))
                )
              (gnc:html-table-set-col-headers!
                table (list (G_ "Name") (G_ "Actual Value") (G_ "Actual Value (EUR)") (G_ "Fraction")))
              (for-each
                (lambda (row)
                  (let* ((currency (car row))
                         (sum (cadr row))
                         (sum-report-currency (caddr row))
                         (fraction (cadddr row)))
                    (gnc:html-table-append-row!
                      table
                      (map
                        gnc:make-html-table-cell/markup
                        (list "text-cell" "number-cell" "number-cell" "number-cell")
                        (list (gnc-commodity-get-mnemonic currency)
                              sum
                              sum-report-currency
                              (format #f "~,2f%" (* 100 fraction))
                        )
                      )
                    )
                  )
                )
                currency-data)
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
