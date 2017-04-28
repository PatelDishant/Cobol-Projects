     
       program-id. ProgramReturns.
       author. Dishant Patel.
       date-written. 2017-04-13.
       
       environment division.
       input-output section.
       file-control. 
      
           
       select records-file assign to "../../../data/report-returns.dat"
               organization is line sequential.
           
           select output-file assign to "../../../data/report-returns.out"
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
       
       77 ws-percentage-tax                                    pic 9v99 value 0.13.
       77 ws-records-count-page                                pic 99 value 20.
       01 ws-record-count-total                                pic 99 value 20.
       01 ws-grand-record                                      pic 999 value 0.
       01 ws-tot-tax                                           pic 9(12)v99.
       01 ws-amount-tax                                        pic 9(6)v99.
       01 ws-counter-return                                    pic 999 value 0.
       01 ws-amount-return                                     pic 9(12)v99 value 0.
       01 ws-num-pages                                         pic 9 value 0.
       01 ws-sw-eof                                            pic x value "n".
       
       
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
           05 filler                                           pic x(24) value "RETURNS REPORT".
           05 filler                                           pic x(15).
           05 filler                                           pic x(6) value "PAGE  ".
           05 ws-page-num                                      pic 9.
           
       01 ws-total-ret-line.
           05 filler                                           pic x.
           05 filler                                           pic x(16) value "R TRANSACTIONS: ".
           05 ws-count-ret                                     pic zz9.
           05 filler                                           pic x(3).
           05 filler                                           pic x(7) value "total: ".
           05 ws-amount-ret                                    pic $z(3),zz9.99.
           
           
           
       01 ws-line-tax.
           05 filler                                           pic x.
           05 filler                                           pic x(19) value "TOTAL TAXES OWING: ".
           05 ws-total-tax                                     pic $zzz,zz9.99.
           
       procedure division.
           
           open input records-file,
               output output-file.
               
           read records-file at end move "y" to ws-sw-eof.
           
           perform 000-processing until ws-sw-eof = "y".
           
           perform 100-totals.
           
           close records-file
                 output-file.
                 
           accept return-code.
           stop run.
           
           goback.
         
       000-processing.
           
           if ws-record-count-total = ws-records-count-page then
                   move 0 to ws-record-count-total
                   add 1 to ws-num-pages
           
                   move spaces to output-line
                   move ws-num-pages to ws-page-num
                   write output-line from ws-head-page after 
                   advancing page
           
                   move spaces to output-line
                   write output-line from ws-head-a after 
                   advancing 2 lines
           
                   move spaces to output-line
                   write output-line from ws-head-b after 
                   advancing 1 line
           
                   move spaces to output-line
                   write output-line
               end-if. 
               
               compute ws-amount-tax rounded = transaction-amount * ws-percentage-tax
               add transaction-amount to ws-amount-return
               add 1 to ws-counter-return
               
               add 1 to ws-grand-record
               add ws-amount-tax to ws-tot-tax
               
               move transaction-code to output-code
               move transaction-amount to output-amount
               move payment-type to output-payment
               move store-number to output-store
               move invoice-number to output-invoice
               move sku-code to output-sku-code
               move ws-amount-tax to output-taxes
               write output-line
               move spaces to output-line
               add 1 to ws-record-count-total.
               
               read records-file at end move "y" to ws-sw-eof.
       
       100-totals.
               move ws-record-count-total to ws-count-ret
               move ws-amount-return to ws-amount-ret
               move ws-tot-tax to ws-total-tax.
               write output-line from spaces
               write output-line from ws-total-ret-line
               write output-line from spaces
               write output-line from ws-line-tax.
               
       end program ProgramReturns.

