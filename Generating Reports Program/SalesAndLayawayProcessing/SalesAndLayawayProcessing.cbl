       identification division.
       program-id. SalesAndLayawayProcessing.
       author. Dishant Patel.
       date-written. 2017-04-12.
       
       environment division.
       input-output section.
       
       file-control.
       
           select records-file assign to "../../../data/report-sale-layaway.dat"
               organization is line sequential.
           
           select output-file assign to "../../../data/report-sale-layaway.out"
               organization is line sequential.
       
       data division.
       file section.
       
       fd records-file
           data record is input-line.
           
       01 input-line.
           05 transaction-code                                 pic x.
           05 transaction-amount                               pic 9(5)v99.
           05 payment-type                                     pic xx.
           05 store-number                                     pic xx.
           05 invoice-number                                   pic x(9).
           05 sku-code                                         pic x(15).
           
       fd output-file
           data record is output-line.
           
       01 output-line.
           05 filler                                           pic x(3).
           05 output-code                                      pic x.
           05 filler                                           pic x(4).
           05 output-amount                                    pic $zz,zz9.99.
           05 filler                                           pic x(4).
           05 output-payment                                   pic xx.
           05 filler                                           pic x(4).
           05 output-store                                     pic xx.
           05 filler                                           pic x(4).
           05 output-invoice                                   pic x(9).
           05 filler                                           pic x(4).
           05 output-sku-code                                  pic x(15).
           05 filler                                           pic x(4).
           05 output-taxes                                     pic $zz,zz9.99.
       
       working-storage section.
       
       01 ws-counter-cash                                      pic 999 value 0.
       01 ws-counter-credit                                    pic 999 value 0.
       01 ws-counter-debit                                     pic 999 value 0.
       01 ws-record-tot                                        pic 999 value 0.
       01 ws-tot-tax                                           pic 9(12)v99.
       01 ws-tax                                               pic 9(6)v99.
       
       77 ws-tax-percentage                                    pic 9v99 value 0.13.
       01 ws-counter-sales                                     pic 999 value 0.
       01 ws-amount-sales                                      pic 9(12)v99.
       01 ws-counter-lay                                       pic 999 value 0.
       01 ws-amount-lay                                        pic 9(12)v99.
       
       01 ws-table-sub.
           05 ws-store01                                       pic 9 value 1.
           05 ws-store02                                       pic 9 value 2.
           05 ws-store03                                       pic 9 value 3.
           05 ws-store07                                       pic 9 value 4.
       
       01 ws-counter-page                                      pic 9 value 0.
       01 ws-counter-record                                    pic 99 value 20.
       77 ws-records-on-page                                   pic 99 value 20.
       
       
       01 ws-eof                                               pic x value "n".
       
       01 ws-table-store.
           05 amount-store                                     pic 9(12)v99 occurs 4 times.
          
       77 ws-cash                                              pic xx value "CA".
       77 ws-debit                                             pic xx value "DB".
       77 ws-credit                                            pic xx value "CR".
       
       77 ws-sale                                              pic x value 'S'.
       77 ws-layaway                                           pic x value 'L'.
       
       01 highest-store.
           05 ws-store-num-highest                             pic xx.
           05 ws-amount-highest                                pic 9(12)v99 value 0.
           
       77 store-1                                              pic xx value "01".
       77 store-2                                              pic xx value "02".
       77 store-3                                              pic xx value "03".
       77 store-7                                              pic xx value "07".
       
       01 ws-percentages.
           05 ws-cash-perc                                     pic 999v99 value 0.
           05 ws-cre-perc                                      pic 999v99 value 0.
           05 ws-deb-perc                                      pic 999v99 value 0.
       
       77 ws-num-stores                                        pic 9 value 4.
       01 ws-table-subscript                                   pic 9.
       
       01 ws-head-a.
           05 filler                                           pic x.
           05 filler                                           pic x(4) value "TRAN".
           05 filler                                           pic x(5).
           05 filler                                           pic x(6) value "AMOUNT".
           05 filler                                           pic x(5).
           05 filler                                           pic x(3) value "PAY".
           05 filler                                           pic x(3).
           05 filler                                           pic x(5) value "STORE".
           05 filler                                           pic x(3). 
           05 filler                                           pic x(7) value "INVOICE".
           05 filler                                           pic x(8). 
           05 filler                                           pic x(7) value "PRODUCT".
           05 filler                                           pic x(12).
           05 filler                                           pic x(3) value "TAX".
               
       01 ws-head-b.
           05 filler                                           pic x.
           05 filler                                           pic x(4) value "CODE".
           05 filler                                           pic x(5).
           05 filler                                           pic x(5) value "OWING".
           05 filler                                           pic x(6).
           05 filler                                           pic x(4) value "TYPE".
           05 filler                                           pic x(3).
           05 filler                                           pic x(3) value "NUM".
           05 filler                                           pic x(4).
           05 filler                                           pic x(6) value "NUMBER".
           05 filler                                           pic x(11).
           05 filler                                           pic x(3) value "SKU".
           05 filler                                           pic x(13).
           05 filler                                           pic x(5) value "OWING".
               
       01 ws-head-page.
           05 filler                                           pic x(7).
           05 filler                                           pic x(24) value "SALES AND LAYAWAY REPORT".
           05 filler                                           pic x(15).
           05 filler                                           pic x(6) value "PAGE  ".
           05 ws-page-num                                      pic 9.
           
       01 ws-total-sale.
           05 filler                                           pic x.
           05 filler                                           pic x(16) value "S TRANSACTIONS: ".
           05 ws-count-sale                                    pic zz9.
           05 filler                                           pic x(3).
           05 filler                                           pic x(7) value "total: ".
           05 ws-amount-sale                                   pic $z(3),zz9.99.
           
       01 ws-total-lay.
           05 filler                                           pic x.
           05 filler                                           pic x(16) value "L TRANSACTIONS: ".
           05 ws-count-lay                                     pic zz9.
           05 filler                                           pic x(3).
           05 filler                                           pic x(7) value "total: ".
           05 ws-amount-lay-tot                                pic $z(3),zz9.99.
           
           
       01 ws-line-percentage.
           05 filler                                           pic x.
           05 filler                                           pic x(6) value "CASH: ".
           05 ws-cash-percentage                               pic zz9.99.
           05 filler                                           pic x value "%".
           05 filler                                           pic x(3).
           05 filler                                           pic x(8) value "CREDIT: ".
           05 ws-credit-percentage                             pic zz9.99.
           05 filler                                           pic x value "%".
           05 filler                                           pic x(3).
           05 filler                                           pic x(7) value "DEBIT: ".
           05 ws-debit-percentage                              pic zz9.99.
           05 filler                                           pic x value "%".
           
       01 ws-line-tax.
           05 filler                                           pic x.
           05 filler                                           pic x(19) value "TOTAL TAXES OWING: ".
           05 ws-total-tax                                     pic $zzz,zz9.99.
           
       procedure division.
           open input records-file,
               output output-file.
               
           read records-file at end move "y" to ws-eof.
           
           perform 000-processing until ws-eof = "y".
           
           perform 300-highest-store.
           perform 400-output-totals.
           
           perform 050-display.
           
           close records-file
                 output-file.
                 
           accept return-code.
           stop run.
           
           goback.
         
       000-processing.
       
               if ws-counter-record = ws-records-on-page then
                   move 0 to ws-counter-record
                   add 1 to ws-counter-page
                   move spaces to output-line
                   move ws-counter-page to ws-page-num
                   write output-line from ws-head-page after advancing page
                   move spaces to output-line
                   write output-line from ws-head-a after advancing 2 lines
                   move spaces to output-line
                   write output-line from ws-head-b after advancing 1 line
                   move spaces to output-line
                   write output-line
               end-if. 
               
               compute ws-tax rounded = transaction-amount * ws-tax-percentage
               
               evaluate payment-type
                   when = ws-cash add 1 to ws-counter-cash
                   when = ws-credit add 1 to ws-counter-credit
                   when = ws-debit add 1 to ws-counter-debit
               end-evaluate.
               
               evaluate store-number
                   when = store-1 add transaction-amount to amount-store(ws-store01)
                   when = store-2 add transaction-amount to amount-store(ws-store02)
                   when = store-3 add transaction-amount to amount-store(ws-store03)
                   when = store-7 add transaction-amount to amount-store(ws-store07)
               end-evaluate.
               
               evaluate transaction-code
                   when = ws-sale add transaction-amount to ws-amount-sales 
                                  add 1 to ws-counter-sales
                                  
                   when = ws-layaway add transaction-amount to ws-amount-lay
                                     add 1 to ws-counter-lay
               end-evaluate.
               
               add 1 to ws-record-tot
               add ws-tax to ws-tot-tax
               
               perform 500-print-record
               
               read records-file at end move "y" to ws-eof.
               
       050-display.
           display ws-counter-cash
           display ws-counter-credit
           display ws-counter-debit
           display ws-record-tot.
       
       300-highest-store.
       
           perform 
                   varying ws-table-subscript
                   from 1
                   by 1
                   until ws-table-subscript > ws-num-stores
               
                       if amount-store(ws-table-subscript) > ws-amount-highest then
                           move amount-store(ws-table-subscript) to ws-amount-highest
                           
                           evaluate ws-table-subscript
                               when = ws-store01 
                                   move store-1 to ws-store-num-highest
                               when = ws-store02
                                   move store-2 to ws-store-num-highest
                               when = ws-store03
                                   move store-3 to ws-store-num-highest
                               when = ws-store07
                                   move store-7 to ws-store-num-highest
                           end-evaluate
                       end-if
               end-perform.
       
       400-output-totals.
       
               move ws-counter-sales to ws-count-sale
               move ws-amount-sales to ws-amount-sale
               move ws-count-lay to ws-count-lay
               move ws-amount-lay to ws-amount-lay-tot
               
               compute ws-cash-perc rounded =
                       (ws-counter-cash / ws-record-tot) * 100
                       
               compute ws-cre-perc rounded =
                       (ws-counter-credit / ws-record-tot) * 100
                       
               compute ws-deb-perc rounded =
                       (ws-counter-debit / ws-record-tot) * 100
                       
               move ws-cash-perc to ws-cash-percentage
               move ws-cre-perc to ws-credit-percentage
               move ws-deb-perc to ws-debit-percentage
               
               move ws-tot-tax to ws-total-tax.
               
               write output-line from spaces
               write output-line from ws-total-sale
               write output-line from spaces
               write output-line from ws-total-lay
               write output-line from spaces
               write output-line from ws-line-percentage
               write output-line from spaces
               write output-line from ws-line-tax.
               
       500-print-record.
       
               move transaction-code to output-code
               move transaction-amount to output-amount
               move payment-type to output-payment
               move store-number to output-store
               move invoice-number to output-invoice
               move sku-code to output-sku-code
               move ws-tax to output-taxes
               
               write output-line
               move spaces to output-line
               add 1 to ws-counter-record.
       
       end program SalesAndLayawayProcessing.
