
solve mpec_calib using mpec minimizing mpec_obj ; 

rep("mpec_calib_cournot","ratio_mining",i,r)$qbar_ir(i,r) = qm.l(i,r) / qbar_ir(i,r) ; 
rep("mpec_calib_cournot","ratio_refining",j,s)$qbar_js(j,s) = qn.l(j,s) / qbar_js(j,s) ; 


*$ontext
* reset switches
sw_cournot_lower = 1 ; 
sw_cournot_upper = 1 ; 
sw_stackelberg = 0 ; 
sw_upper_linear = 1 ;

* reset switches
*qm.l(i,r)$qbar_ir(i,r) = qbar_ir(i,r) ; 
*qn.l(j,s)$qbar_js(j,s) = qbar_js(j,s) ; 

solve mpec_calib using mpec minimizing mpec_obj ; 


rep("mpec_calib_cournot_linear","ratio_mining",i,r)$qbar_ir(i,r) = qm.l(i,r) / qbar_ir(i,r) ; 
rep("mpec_calib_cournot_linear","ratio_refining",j,s)$qbar_js(j,s) = qn.l(j,s) / qbar_js(j,s) ; 


* reset switches
sw_cournot_lower = 1 ; 
sw_cournot_upper = 0 ; 
sw_stackelberg = 1 ; 
sw_upper_linear = 0 ;

solve mpec_calib using mpec minimizing mpec_obj ; 

rep("mpec_stackleberg_cournot","ratio_mining",i,r)$qbar_ir(i,r) = qm.l(i,r) / qbar_ir(i,r) ; 
rep("mpec_stackleberg_cournot","ratio_refining",j,s)$qbar_js(j,s) = qn.l(j,s) / qbar_js(j,s) ; 



execute_unload 'fig1.gdx' ;
*$offtext

sw_cournot_lower = 1 ; 
sw_cournot_upper = 1 ; 
sw_stackelberg = 0 ; 
sw_upper_linear = 0 ;

solve mpec_calib using mpec minimizing mpec_obj ; 

rep("mpec_calib_cournot","ratio_mining",i,r)$qbar_ir(i,r) = qm.l(i,r) / qbar_ir(i,r) ; 
rep("mpec_calib_cournot","ratio_refining",j,s)$qbar_js(j,s) = qn.l(j,s) / qbar_js(j,s) ; 
