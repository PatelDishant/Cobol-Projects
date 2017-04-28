       program-id. DataSplitAndCount.
       author. Dishant Patel.
       date-written. 2017-04-11.
       
       environment division.
       input-output section.
       
       file-control.
       
           select records-file assign to "../../../data/file-valid.out"
               organization is line sequential.
               
           select report-sale-layaway assign to "../../../data/report-sale-layaway.dat"
               organization is line sequential.
               
           select report-returns assign to "../../../data/report-returns.dat"
               organization is line sequential.
           
           select report-counts assign to "../../../data/report-counts.dat"
               organization is line sequential.

       data division.
       
       fd records-file 
           data record is input-line.
           
       01 input-line.
           05 transaction-code                             pic x.
           05 transaction-amount                           pic 9(5)v99.
           05 payment-type                                 pic xx.
           05 store-number                                 pic xx.
           05 invoice-number                               pic x(9).
           05 sku-code                                     pic x(15).
       
       fd report-sale-layaway
           data record is sale-lay-line.
           
       01 sale-lay-line                                    pic x(210).
       
       
       fd report-returns
           data record is returns-line.
           
       01 returns-line                                      pic x(210).
       
       
       fd report-counts
           data record is counts-line.
           
       01 counts-line                                      pic x(210).
       
       working-storage section.

       01 ws-title-a.
           05 filler                                       pic x(22) value "Sales & Layaway Totals".
       
       01 ws-title-ab. 
           05 filler                                       value "==================================".
           
       01 ws-title-b.
           05 filler                                       pic x(35) value " Returns ".
       
       01 ws-sale-lay-head-a.
           05 filler                                       pic x(19) value " Number of Records ".
           05 filler                                       pic x(5) value spaces.
           05 filler                                       pic x(14) value " Amount Sales ".
           05 filler                                       pic x(5) value spaces.
           05 filler                                       pic x(14) value " Num of Sales ".
           05 filler                                       pic x(4) value spaces.
           
       01 ws-sale-lay-head-b.
           05 filler                                       pic x(14) value " and Layaways ".
           05 filler                                       pic x(10) value spaces.
           05 filler                                       pic x(14) value " and Layaways ".
           
       01 ws-sale-head-dollars.
           05 filler                                       pic x(18) value " Dollars in Sales ".
           05 filler                                       pic x(1) value spaces.
           05 filler                                       pic x(15) value " Num in Layaway".
           05 filler                                       pic x(1) value spaces.
           05 filler                                       pic x(20) value " Dollars in Layaway ".
           05 filler                                       pic x(6) value spaces.
           
       01 ws-sale-head-percentage.
           05 filler                                       pic x(18) value " Sales Percentage ".
           05 filler                                       pic x(20) value " Layaway Percentage ".
       
       01 ws-sale-store-head.
           05 filler                                       pic x(4) value spaces.
           05 filler                                       pic x(12) value " Store 01 ".
           05 filler                                       pic x(8) value spaces.
           05 filler                                       pic x(12) value " Store 02 ".
           05 filler                                       pic x(7) value spaces.
           05 filler                                       pic x(12) value " Store 03 ".
           05 filler                                       pic x(7) value spaces.
           05 filler                                       pic x(12) value " Store 07 ".
           05 filler                                       pic x(5) value spaces.
       
       
       01 ws-return-head.
           05 filler                                       pic x(17) value "Number of Returns".
           05 filler                                       pic x(10).
           05 filler                                       pic x(18) value "Dollars in Returns".
           05 filler                                       pic x(10).
           05 filler                                       pic x(10) value "Store 01".
           05 filler                                       pic x(10).
           05 filler                                       pic x(10) value "Store 02".
           05 filler                                       pic x(10).
           05 filler                                       pic x(10) value "Store 03".
           05 filler                                       pic x(10).
           05 filler                                       pic x(10) value "Store 07".
       
       01 ws-return-head-b.
           05 filler                                       pic x(17) value "_________________".
           05 filler                                       pic x(10).
           05 filler                                       pic x(18) value "__________________".
           05 filler                                       pic x(10).
           05 filler                                       pic x(10) value "__________".
           05 filler                                       pic x(10).
           05 filler                                       pic x(10) value "__________".
           05 filler                                       pic x(10).
           05 filler                                       pic x(10) value "__________".
           05 filler                                       pic x(10).
           05 filler                                       pic x(10) value "__________".
       
       01 ws-totals-sale-lay-1.
           05 filler                                       pic x(6) value spaces.
           05 ws-tot-sale-lay                              pic zz9.              
           05 filler                                       pic x(13) value spaces.
           05 ws-tot-sale-lay-trans                        pic $(3),$(3),$$9.99. 
           05 filler                                       pic x(12) value spaces.
           05 ws-totals-sale                               pic zz9.              
           
           
           
       01 ws-totals-sale-lay-2.
            
           05 ws-sale-trans                                pic $(3),$(3),$$9.99.
           05 filler                                       pic x(12) value spaces.
           05 ws-totals-lay                                pic zz9.     
           05 filler                                       pic x(7) value spaces.
           05 ws-lay-trans                                 pic $(3),$(3),$$9.99.
           
           
       01 ws-totals-sale-lay-3.
          
           05 ws-tot-store1-sl                             pic $(3),$(3),$$9.99. 
           05 filler                                       pic x(5) value spaces.
           05 ws-tot-store2-sl                             pic $(3),$(3),$$9.99. 
           05 filler                                       pic x(5) value spaces.
           05 ws-tot-store3-sl                             pic $(3),$(3),$$9.99. 
           05 filler                                       pic x(5) value spaces.
           05 ws-tot-store7-sl                             pic $(3),$(3),$$9.99. 
       
           
       01 ws-totals-sale-lay-4.
           05 filler                                       pic x(6) value spaces.
           05 ws-total-lay-perc                            pic Z9.
           05 filler                                       pic x(8) value '%'.
           05 filler                                       pic x(6) value spaces.
           05 ws-total-sale-perc                           pic Z9.    
           05 filler                                       pic x value '%'.
       
       01 ws-totals-return.
           05 filler                                       pic x(6) value spaces.
           05 ws-return                                    pic zz9. 
           05 filler                                       pic x(15) value spaces.
           05 ws-total-return                              pic $(3),$(3),$$9.99. 
           05 filler                                       pic x(11) value spaces.
           05 ws-total-store1-ret                          pic $(3),$(3),$$9.99. 
           05 filler                                       pic x(6) value spaces.
           05 ws-total-store2-ret                          pic $(3),$(3),$$9.99. 
           05 filler                                       pic x(6) value spaces.
           05 ws-total-store3-ret                          pic $(3),$(3),$$9.99. 
           05 filler                                       pic x(6) value spaces.
           05 ws-total-store7-ret                          pic $(3),$(3),$$9.99. 
           05 filler                                       pic x(6) value spaces.
           
       01 ws-final-total.
           05 filler                                       pic x(20) value " Net Gain: ".
           05 ws-grand-total                               pic $(3),$(3),$$9.99.
           
       01 ws-sw-eof                                        pic x value 'N'.
       
       01 ws-total-sale-lay                                pic 999 value 0.
       01 ws-total-sale                                    pic 999 value 0.
       01 ws-total-lay                                     pic 999 value 0.
       01 ws-total-ret                                     pic 999 value 0.
       
       01 ws-total-sale-lay-trans                          pic 9(10)v99 value 0.
       01 ws-total-sale-trans                              pic 9(10)v99 value 0.
       01 ws-total-lay-trans                               pic 9(10)v99 value 0.
       01 ws-total-ret-trans                               pic 9(10)v99 value 0.
       
       01 ws-total-store1-trans                            pic 9(10)v99 value 0.
       01 ws-total-store2-trans                            pic 9(10)v99 value 0.
       01 ws-total-store3-trans                            pic 9(10)v99 value 0.
       01 ws-total-store7-trans                            pic 9(10)v99 value 0.
       
       01 ws-total-store1-returns                          pic 9(10)v99 value 0.
       01 ws-total-store2-returns                          pic 9(10)v99 value 0.
       01 ws-total-store3-returns                          pic 9(10)v99 value 0.
       01 ws-total-store7-returns                          pic 9(10)v99 value 0.
      
       01 ws-sale-perc-trans                               pic 99.
       01 ws-lay-perc-trans                                pic 99.
       01 ws-grand-totals                                  pic 9(10)v99.
           
       
       procedure division.

           open input records-file,
           open output report-sale-layaway,
                       report-returns,
                       report-counts.
           
           read records-file at end move "Y" to ws-sw-eof.
           
           perform until ws-sw-eof = "Y"
           
           evaluate transaction-code 
               when 'S'
                   write sale-lay-line from input-line
                   perform 100-count-sale
                   perform 150-count-sale-lay
               when 'L'
                   write sale-lay-line from input-line
                   perform 200-count-layaway
                   perform 150-count-sale-lay
               when 'R'
                   write returns-line from input-line
                   perform 300-count-returns
           end-evaluate
           
           read records-file at end move "Y" to ws-sw-eof
           
           end-perform.
           
           compute ws-grand-totals = ws-total-sale-lay-trans - ws-total-ret-trans.
       
           compute ws-lay-perc-trans rounded = (ws-total-lay / ws-total-sale-lay) * 100.
           
           compute  ws-sale-perc-trans rounded = (ws-total-sale / ws-total-sale-lay) * 100.
           
           perform 500-output.
           
           close records-file,
                 report-sale-layaway,
                 report-returns,
                 report-counts.
                 
           goback.
           
       100-count-sale.
           add 1 to ws-total-sale.
           add transaction-amount to ws-total-sale-trans.
           perform 400-count-store.
           
       
       150-count-sale-lay.
           add 1 to ws-total-sale-lay
           add transaction-amount to ws-total-sale-lay-trans.
       
       
       200-count-layaway.
           add 1 to ws-total-lay.
           add transaction-amount to ws-total-lay-trans.
           perform 400-count-store.
       
       300-count-returns.
           add 1 to ws-total-ret
           add transaction-amount to ws-total-ret-trans
           perform 450-count-store-ret.
          
       
       400-count-store.
           
           evaluate store-number 
               when "01"
                   add transaction-amount to ws-total-store1-trans
               when "02"
                   add transaction-amount to ws-total-store2-trans
               when "03"
                   add transaction-amount to ws-total-store3-trans
               when "07"
                   add transaction-amount to ws-total-store7-trans
           end-evaluate.
       
       450-count-store-ret.
          
          evaluate store-number 
               when "01"
                   add transaction-amount                  to ws-total-store1-returns
               when "02"
                   add transaction-amount                  to ws-total-store2-returns
               when "03"
                   add transaction-amount                  to ws-total-store3-returns
               when "07"
                   add transaction-amount                  to ws-total-store7-returns
           end-evaluate.
       
       500-output.
           move ws-total-lay-trans                         to ws-lay-trans.
           move ws-total-sale-lay-trans                    to ws-tot-sale-lay-trans.
           move ws-total-sale-trans                        to ws-sale-trans.
           move ws-lay-perc-trans                          to ws-total-lay-perc.
           move ws-sale-perc-trans                         to ws-total-sale-perc.
           move ws-total-sale-lay                          to ws-tot-sale-lay.
           move ws-total-sale                              to ws-totals-sale.
           move ws-total-lay                               to ws-totals-lay.
           move ws-total-store1-trans                      to ws-tot-store1-sl.
           move ws-total-store2-trans                      to ws-tot-store2-sl.
           move ws-total-store3-trans                      to ws-tot-store3-sl.
           move ws-total-store7-trans                      to ws-tot-store7-sl.
           move ws-total-ret                               to ws-return.
           move ws-total-ret-trans                         to ws-total-return.
           move ws-total-store1-returns                    to ws-total-store1-ret.
           move ws-total-store2-returns                    to ws-total-store2-ret.
           move ws-total-store3-returns                    to ws-total-store3-ret.
           move ws-total-store7-returns                    to ws-total-store7-ret.
           move ws-grand-totals                            to ws-grand-total.
           
           write counts-line                        from ws-title-a.
           write counts-line                        from ws-sale-lay-head-a after advancing 2 lines.
           write counts-line                        from ws-sale-lay-head-b after advancing 1 lines.
           write counts-line                        from ws-totals-sale-lay-1 after advancing 1 lines.
           write counts-line                        from ws-sale-head-dollars after advancing 3 lines.
           write counts-line                        from ws-totals-sale-lay-2 after advancing 2 lines.
           write counts-line                        from ws-sale-head-percentage after advancing 3 lines.
           write counts-line                        from ws-totals-sale-lay-4 after advancing 2 lines.
           write counts-line                        from ws-sale-store-head after advancing 3 lines.
           write counts-line                        from ws-totals-sale-lay-3 after advancing 2 lines.
           write counts-line                        from ws-title-b after advancing 3 lines.
           write counts-line                        from ws-return-head after advancing 3 lines.
           write counts-line                        from ws-totals-return after advancing 2 lines.
           write counts-line                        from ws-final-total after advancing 2 lines.
       
       
       
       end program DataSplitAndCount.
