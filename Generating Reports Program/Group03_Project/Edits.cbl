       identification division. 
       program-id. Edits.
       author. Dishant Patel.
       date-written. 2017-04-10.
       
       environment division.
       input-output section.
      
      *
       file-control.
      *
           select records-file assign to "../../../data/project1.dat"
               organization is line sequential.
      *
           select file-errors assign to "../../../data/file-errors.out"
               organization is line sequential.
      *
           select file-valid assign to "../../../data/file-valid.out"
               organization is line sequential.
      *    
           select file-invalid assign to "../../../data/file-invalid.dat"
               organization is line sequential.
      *
       
       data division.
       
       file section.
      
      *
       fd records-file
           data record is input-line.
      *
       01 input-line.
           05 transaction-code                     pic x.
           05 transaction-amount                   pic 9(5)V99.
           05 payment-type                         pic xx.
           05 store-number                         pic 99.
           05 invoice-number                       pic x(9).
           05 sku-code                             pic x(15).
      
      *
       fd file-errors 
           data record is errors.
      *
       01 errors                                   pic x(47).
     
      *
       fd file-valid
           data record is valid.
      *
       01 valid                                    pic x(36).
    
      *
       fd file-invalid 
           data record is invlid.
      *
       01 invlid                                   pic x(36).
     
      *
       working-storage section.
     
      *
       01 ws-sw-eof                                pic x value 'n'.
             
      * Constants for Payment types
       77 ws-cash                                  pic xx value "CA".
       77 ws-credit                                pic xx value "CR".
       77 ws-debit                                 pic xx value "DB".
      
      * Constants for store number
       77 ws-num-1                                 pic xx value "01".
       77 ws-num-2                                 pic xx value "02".
       77 ws-num-3                                 pic xx value "03".
       77 ws-num-7                                 pic xx value "07".
     
      * Constants for transaction types
       77 ws-sale                                  pic x value 'S'.
       77 ws-return                                pic x value 'R'.
       77 ws-layaway                               pic x value 'L'. 
       
      * Counter variable for errors
        01 ws-num-errors                           pic 9 value 0.
        
      * Variable for invoice number 
       01 ws-invoice-num.
           05 ws-invoice-letters                   pic xx.
           05 ws-invoice-dash                      pic x.
           05 ws-invoice-integers                  pic 9(6).
      
      * Variable for error
       01 ws-error-line.
           05 ws-record                            pic x(36).
           05 filler                               pic xx.
           05 ws-errors                            pic 9.
           
      * Variables for total values
       01 ws-total-records                         pic 999 value 0.
       01 ws-total-good                            pic 999 value 0.
       01 ws-total-error                           pic 999 value 0.
       
       procedure division.
       
           open input records-file,
               output file-errors, file-valid, file-invalid.
               
           read records-file at end move "y" to ws-sw-eof.
           
           perform until ws-sw-eof = 'y'
           
           perform 100-transaction-code
           
           perform 150-transaction-amount
           
           perform 200-payment-type
           
           perform 250-store-num
           
           perform 300-invoice-num
           
           perform 400-processing
           
           add 1 to ws-total-records
           
           read records-file at end move "y" to ws-sw-eof
           
           end-perform.
           
           close records-file
                 file-errors
                 file-invalid
                 file-valid.
           
           display "File Processed".
           display "Total Records: " ws-total-records.
           display "Good Records: " ws-total-good.
           display "Records with Errors: " ws-total-error.
           
           accept return-code.
           goback.
           
           
       100-transaction-code.
       
          if transaction-code is not equal to ws-sale and 
              transaction-code is not equal to ws-return and
              transaction-code is not equal to ws-layaway then
                   add 1 to ws-num-errors
           end-if.     
       
       150-transaction-amount.
       
           if transaction-amount is not numeric then
                   add 1 to ws-num-errors
           end-if.
           
       200-payment-type.
       
           if payment-type is not equal to ws-cash and
              payment-type is not equal to ws-debit and
              payment-type is not equal to ws-credit then
                   add 1 to ws-num-errors
           end-if.
           
       250-store-num.
       
           if store-number is not equal to ws-num-1 and 
              store-number is not equal to ws-num-2 and
              store-number is not equal to ws-num-3 and
              store-number is not equal to ws-num-7 then
                   add 1 to ws-num-errors
           end-if.
       
       300-invoice-num.
       
           move invoice-number to ws-invoice-num
           
           if ws-invoice-letters is not alphabetic then
               add 1 to ws-num-errors
           end-if
           
           if ws-invoice-dash not = '-' then
               add 1 to ws-num-errors
           end-if
           
           if ws-invoice-integers not numeric then
               add 1 to ws-num-errors
           end-if
           
           if sku-code equals spaces then
               add 1 to ws-num-errors
           end-if.
          
       400-processing.
       
           if ws-num-errors not = 0 then
               write invlid from input-line
               move input-line to ws-record
               move ws-num-errors to ws-errors
               write errors from ws-error-line
               move 0 to ws-num-errors
               move spaces to ws-record
               move 0 to ws-errors
               add 1 to ws-total-error
           else
               write valid from input-line
               add 1 to ws-total-good
           end-if.
           
           
       end program Edits.
