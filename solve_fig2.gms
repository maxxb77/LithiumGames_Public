


* need to...
* record output, price, cost from stackelberg setup
* note - lithium carbonate market reflected in li-equivalents and needs adjustment via theta

parameter rep2; 
* see equations in Appendix B for derivations here
rep2("stackelberg",i,r,"quantity")$v_qm(i,r) = qm.l(i,r) ; 
rep2("stackelberg",i,r,"production_cost")$v_qm(i,r) = 
      beta_v.l(i,r) * cbar_ir(i,r) * (QM.l(i,r)**(1/beta_v.l(i,r) + 1)) 
            /((1+beta_v.l(i,r)) * qbar_ir(i,r)**(1/beta_v.l(i,r)));

rep2("stackelberg",i,r,"price")$v_qm(i,r) = lambda_m.l(i,r) +  (cbar_i_v.l(i) * (sum(rr$V_QM(i,rr), QM.l(i,rr)) / qbar_i(i))**(1/i_elas_v.l(i)))$(not sw_pc) / sum(j,theta(i,j)) ; 

rep2("stackelberg",j,s,"quantity")$v_qn(j,s) = qn.l(j,s) ; 
rep2("stackelberg",j,s,"production_cost")$[v_qn(j,s)$sameas(j,"li2co3")] =  (QN.l(j,s) * (c_s.l(j,s) + d_s.l(j,s) * QN.l(j,s))) / sum(i,theta(i,j))  ;

rep2("stackelberg",j,s,"production_cost")$[v_qn(j,s)] =  gamma_v.l(j,s) * cbar_js(j,s) * (QN.l(j,s)**(1/gamma_v.l(j,s) + 1)) 
                                                            /(2 * ((1+gamma_v.l(j,s)) * qbar_js(j,s)**(1/gamma_v.l(j,s))));

rep2("stackelberg",j,s,"price")$[v_qn(j,s)$sameas(j,"li2co3")] = ( (alpha_j.l + beta_j.l * sum(ss$v_qn(j,ss), qn.l(j,ss)) )) / sum(i,theta(i,j))  ; 
rep2("stackelberg",j,s,"price")$[v_qn(j,s)] = (pbar_j_v.l(j) * (sum(ss$V_QN(j,ss), QN.l(j,ss)) / qbar_j(j))
      **(1/j_elas_v.l(j)))   ; 

* profit = revenue - cost
rep2("stackelberg",i,r,"profit")$v_qm(i,r) = (rep2("stackelberg",i,r,"price")*qm.l(i,r) - rep2("stackelberg",i,r,"production_cost"))  ; 
rep2("stackelberg",j,s,"profit")$v_qn(j,s) = (rep2("stackelberg",j,s,"price")*qn.l(j,s) - rep2("stackelberg",j,s,"production_cost")) 
                                                - sum((i,r)$v_qm(i,r), rep2("stackelberg",i,r,"price")*QS.l(i,j,r,s)); 

* fix variable parameters to MPEC solution

alpha_j.fx = alpha_j.l  ; 
beta_j.fx = beta_j.l  ; 
c_s.fx(j,s) = c_s.l(j,s)  ; 
d_s.fx(j,s) = d_s.l(j,s)  ; 
i_elas_v.fx(i) = i_elas_v.l(i)  ; 
j_elas_v.fx(j) = j_elas_v.l(j)  ; 
gamma_v.fx(j,s) = gamma_v.l(j,s)  ; 
cbar_js_v.fx(j,s) = cbar_js_v.l(j,s)  ; 
pbar_j_v.fx(j) = pbar_j_v.l(j)  ; 
cbar_i_v.fx(i) = cbar_i_v.l(i)  ; 
beta_v.fx(i,r) = beta_v.l(i,r) ; 
* solve the perfect competition outcome

sw_cournot_lower = 0 ; 
sw_cournot_upper = 0 ; 
sw_stackelberg = 0 ; 
sw_upper_linear = 0 ;
sw_pc = 1 ; 

parameter rep_par ; 

rep_par("Li2CO3",'N/A','alpha') = alpha_j.l ; 
rep_par("Li2CO3",'N/A','beta') = beta_j.l ; 
rep_par(j,s,'c_s') = c_s.l(j,s) ;
rep_par(j,s,'d_s') = d_s.l(j,s) ; 
rep_par(i,"N/A","elas") = i_elas_v.l(i) ; 
rep_par(j,"N/A","elas") = j_elas_v.l(j) ; 
rep_par(j,s,"gamma") = gamma_v.l(j,s) ; 
rep_par(j,s,"cbar_js") = cbar_js_v.l(j,s) ; 

rep_par(j,"N/A","pbar_j") = pbar_j_v.l(j) ; 
rep_par(i,"N/A","pbar_i") =  cbar_i_v.l(i)  ; ; 
rep_par(i,r,"beta_v") = beta_v.l(i,r) ; 


qm.up(i,r) = qm.l(i,r) * 1.5 ; 

solve bilevel_mcp using mcp ; 




rep2("mcp_pc",i,r,"quantity")$v_qm(i,r) = qm.l(i,r) ; 
rep2("mcp_pc",i,r,"production_cost")$v_qm(i,r) = cbar_ir(i,r) * (QM.l(i,r)/qbar_ir(i,r)) ** (1/beta_v.l(i,r))  / sum(j,theta(i,j)) ; 
rep2("mcp_pc",i,r,"price")$v_qm(i,r) = ( (cbar_i_v.l(i) * (sum(rr$V_QM(i,rr), QM.l(i,rr)) / qbar_i(i))**(-1/i_elas_v.l(i))))  ; 

rep2("mcp_pc",j,s,"quantity")$v_qn(j,s) = qn.l(j,s) ; 
; 
rep2("mcp_pc",j,s,"production_cost")$[v_qn(j,s)$sameas(j,"li2co3")] =  (QN.l(j,s) * (c_s.l(j,s) + d_s.l(j,s) * QN.l(j,s))) / sum(i,theta(i,j))  ;
rep2("mcp_pc",j,s,"production_cost")$[v_qn(j,s)] =  gamma_v.l(j,s) * cbar_js(j,s) * (QN.l(j,s)**(1/gamma_v.l(j,s) + 1)) 
                                                    /(2*((1+gamma_v.l(j,s)) * qbar_js(j,s)**(1/gamma_v.l(j,s))));

rep2("mcp_pc",j,s,"price")$[v_qn(j,s)$sameas(j,"li2co3")] = ( (alpha_j.l + beta_j.l * sum(ss$v_qn(j,ss), qn.l(j,ss)) )) / sum(i,theta(i,j))  ; 
rep2("mcp_pc",j,s,"price")$[v_qn(j,s)] =  (pbar_j_v.l(j) * (sum(ss$V_QN(j,ss), QN.l(j,ss)) / qbar_j(j))
      **(1/j_elas_v.l(j)))  ; 

* profit = revenue - cost
rep2("mcp_pc",i,r,"profit")$v_qm(i,r) = (rep2("mcp_pc",i,r,"price")*qm.l(i,r) - rep2("mcp_pc",i,r,"production_cost"))  ; 
rep2("mcp_pc",j,s,"profit")$v_qn(j,s) = (rep2("mcp_pc",j,s,"price")*qn.l(j,s) - rep2("mcp_pc",j,s,"production_cost")) - sum((i,r)$v_qm(i,r), rep2("mcp_pc",i,r,"price")*QS.l(i,j,r,s)); 


parameter rep3 "dwl";


rep3("q_st",r,i) = rep2("stackelberg",i,r,"quantity") ; 
rep3("q_pc",r,i) = rep2("mcp_pc",i,r,"quantity") ; 

$ontext
parameter rep_temp; 

rep_temp(i,"a") = (1/i_elas_v.l(i)) * cbar_i_v.l(i)  ; 
rep_temp(i,"b") = sum(r,rep3("q_pc",r,i)) ; //**((1/i_elas_v.l(i)+1)) ; 
rep_temp(i,"c") = sum(r,rep3("q_st",i,r))**((1/i_elas_v.l(i)+1));
rep_temp(i,"d") = ((1+i_elas_v.l(i))*(qbar_i(i))**(1/i_elas_v.l(i))) ; 
$offtext

rep3("delta_cs","N/A",i) = 
     (1/i_elas_v.l(i)) * cbar_i_v.l(i) 
      * (sum(r,rep3("q_pc",r,i))-sum(r,rep3("q_st",r,i)))**((1/i_elas_v.l(i)+1))
      / ((1+i_elas_v.l(i))*(qbar_i(i))**(1/i_elas_v.l(i))) ; 


* shown as  negative for plotting purposes
rep3("delta_ps",r,i) = - abs((rep2("stackelberg",i,r,"profit") - rep2("mcp_pc",i,r,"profit")) );


rep3("q_st",s,j) = rep2("stackelberg",j,s,"quantity") ; 
rep3("q_pc",s,j) = rep2("mcp_pc",j,s,"quantity") ; 

rep3("delta_cs","N/A",j) = 
      (1/j_elas_v.l(j)) * pbar_j_v.l(j) 
      * (sum(s,rep3("q_pc",s,j))**((1/j_elas_v.l(j)+1))-sum(s,rep3("q_st",s,j))**((1/j_elas_v.l(j)+1))) 
      / ((1+j_elas_v.l(j))*(qbar_j(j))**(1/j_elas_v.l(j))) ; 

*rep3("delta_cs","N/A",j)$sameas(j,"li2co3") = alpha_j.l * (sum(s,rep3("q_pc",s,j)) - sum(s,rep3("q_pc",s,j))) + beta_j.l * ((sum(s,rep3("q_pc",s,j)) - sum(s,rep3("q_st",s,j)))**2) ; 
* shown as  negative for plotting purposes

rep3("delta_cs_cost","N/A2",j) = sum(i$theta(i,j),rep3("delta_cs","N/A",i)) ; 

rep3("delta_cs","N/A",j) = rep3("delta_cs","N/A",j) - rep3("delta_cs_cost","N/A2",j) ; 

rep3("delta_ps",s,j) = - (abs((rep2("stackelberg",j,s,"profit") - rep2("mcp_pc",j,s,"profit")))  )/2; 
*rep3("delta_ps",s,j)$sameas(j,"lioh") = - (abs((rep2("stackelberg",j,s,"profit") - rep2("mcp_pc",j,s,"profit")))  ); 

* convert to li-equivalents for plotting
rep2("stackelberg",j,s,"quantity") = (rep2("stackelberg",j,s,"quantity") / sum(i, theta(i,j))) ; 
rep2("mcp_pc",j,s,"quantity") = rep2("mcp_pc",j,s,"quantity") / sum(i, theta(i,j)) ; 


execute_unload 'fig2.gdx' ; 

* record costs and 


* calculate dwl
* then need to add a new entrant to each stage, representing 10/30/50% of total production