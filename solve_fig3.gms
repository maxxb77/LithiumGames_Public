


* need to...

parameter rep4; 

* fix variable parameters

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

set i_add /0,10, 20, 30, 40/ ; 
set j_add /0,10, 20, 30, 40/ ; 
alias(i_add,ii_add); 
alias(j_add,jj_add) ; 
*set j_add /0/ ; 

    solve bilevel_mcp using mcp ; 


loop(ii_add,
    loop(jj_add,
* set up exogenous new entrant
    qbar_i_add(i) = (ii_add.val / 100) * qbar_i(i) ; 
    qbar_j_add(j) = (jj_add.val / 100) * qbar_j(j) ; 

* solve the model
    solve bilevel_mcp using mcp ; 


rep4("stackelberg",i,r,ii_add,jj_add,"quantity")$v_qm(i,r) = qm.l(i,r) ; 
rep4("stackelberg",i,r,ii_add,jj_add,"production_cost")$v_qm(i,r) = 
*sum(j,theta(i,j)) * cbar_ir_v.l(i,r) * (QM.l(i,r)/qbar_ir(i,r)) ** (1/beta_v.l(i,r))   ; 
             - (beta_v.l(i,r) * (cbar_ir(i,r)) * (QM.l(i,r)**(1/beta_v.l(i,r) + 1)) /2) 
             / (2 *  (1+beta_v.l(i,r)) * qbar_ir(i,r)**(1/beta_v.l(i,r)));

rep4("stackelberg",i,r,ii_add,jj_add,"price")$v_qm(i,r) = cbar_i_v.l(i) * ((sum(rr$V_QM(i,rr), QM.l(i,rr)) + sum(j,theta(i,j) * qbar_j_add(j)) - qbar_i_add(i)  ) / (qbar_i(i)+qbar_i_add(i) ))**(-1/(i_elas_v.l(i)))  ; 

rep4("stackelberg",j,s,ii_add,jj_add,"quantity")$v_qn(j,s) = qn.l(j,s) ; 
*report costs as negative for reporting purposes
rep4("stackelberg",j,s,ii_add,jj_add,"production_cost")$[v_qn(j,s)$sameas(j,"li2co3")] = - (QN.l(j,s) * (c_s.l(j,s) + d_s.l(j,s) * QN.l(j,s)))  ;

rep4("stackelberg",j,s,ii_add,jj_add,"production_cost")$[v_qn(j,s)] =  
            -  gamma_v.l(j,s) * (cbar_js(j,s)) * (QN.l(j,s)**(1/gamma_v.l(j,s) + 1)) 
             / ( (1+gamma_v.l(j,s)) * qbar_js(j,s)**(1/gamma_v.l(j,s))/2);

*rep4("stackelberg",j,s,ii_add,jj_add,"price")$[v_qn(j,s)$sameas(j,"li2co3")] = 1.3 * abs(((alpha_j.l + beta_j.l * (-qbar_j_add(j) + sum(ss$v_qn(j,ss), qn.l(j,ss)) ))) / sum(i,theta(i,j))  ); 
*rep4("stackelberg",j,s,ii_add,jj_add,"price")$[v_qn(j,s)$sameas(j,"lioh")] =  ; 
rep4("stackelberg",j,s,ii_add,jj_add,"price")$[v_qn(j,s)] = pbar_j_v.l(j) *(((sum(ss$V_QN(j,ss), QN.l(j,ss))) - qbar_j_add(j))/( qbar_j(j) - qbar_j_add(j)  )) **(-1/(j_elas_v.l(j))) 
*/ sum(i,theta(i,j))
 +  lambda_n.l(j,s) 
 ;

* profit = revenue - cost
rep4("stackelberg",i,r,ii_add,jj_add,"revenue")$v_qm(i,r) = rep4("stackelberg",i,r,ii_add,jj_add,"price")*qm.l(i,r)   ; 
rep4("stackelberg",j,s,ii_add,jj_add,"revenue")$v_qn(j,s) = rep4("stackelberg",j,s,ii_add,jj_add,"price")*qn.l(j,s) ; 

rep4("stackelberg",j,s,ii_add,jj_add,"ore purchase")$v_qn(j,s) = sum((i,r), theta(i,j) * rep4("stackelberg",i,r,ii_add,jj_add,"price")*[QS.l(i,j,r,s) ]) 
                                                                    + (sum(i,theta(i,j)) * qn.l(j,s)/ (sum((ss),qn.l(j,ss))+qbar_j_add(j)) * sum((i,r), rep4("stackelberg",i,r,ii_add,jj_add,"price")* qbar_i_add(i) / sum((rr,ii)$V_QS(ii,j,rr,s),1))) 
)  ;

rep4("stackelberg",j,s,ii_add,jj_add,"ore purchase")$[v_qn(j,s)] = - sum((i)$theta(i,j), rep4("stackelberg",i,"china",ii_add,jj_add,"price") * qn.l(j,s)) ; 

rep4("stackelberg",i,r,ii_add,jj_add,"profit")$v_qm(i,r) = (rep4("stackelberg",i,r,ii_add,jj_add,"price")*qm.l(i,r) + rep4("stackelberg",i,r,ii_add,jj_add,"production_cost"))  ; 
rep4("stackelberg",j,s,ii_add,jj_add,"profit")$v_qn(j,s) = (sum(i, (1+theta(i,j)$[not sameas(s,"china")]) * ((rep4("stackelberg",j,s,ii_add,jj_add,"price")*qn.l(j,s) -rep4("stackelberg",j,s,ii_add,jj_add,"production_cost")) )))
                                                            - sum((i,r), rep4("stackelberg",i,r,ii_add,jj_add,"price")*QS.l(i,j,r,s)); 


    ) ; 
) ; 


execute_unload 'fig3.gdx' ; 