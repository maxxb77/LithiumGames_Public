option profile = 1;
option limrow = 0; option limcol = 0;

scalar sw_pc /0/ ; 

* ---------------------------------------------
* Mining Stage
* ---------------------------------------------
Set
r 'Mining Countries' / Australia,
                       Chile,
                       China,
                       Argentina,
                       Brazil,
                       Zimbabwe,
                       Canada,
                       USA /
                       

i 'ore type' /         brine,
                       hardrock
                        /


V(i,r)
;

* ---------------------------------------------
* Mining Parameters (Cost, etc.)
* ---------------------------------------------
Table data(r,i,*) 'cost function data'
                           c             qbar        beta
   Australia.hardrock     6109.2         88000       0.75
   china.hardrock         8389.2         28700       0.75
   China.brine            7647.65        12300       0.75
   Chile.brine            5646.4         49000       0.75
   Argentina.brine        6575.8         18000       0.75
   Brazil.hardrock        8125           10000       0.75
   Zimbabwe.hardrock      6607.5         2700        0.75  
   USA.brine              6072.5         870         0.75
   Canada.hardrock        7192.02        4300        0.75
;

parameter cbar_i(i) ; 
cbar_i(i) = sum(r,data(r,i,"c")*data(r,i,"qbar")) / sum(r,data(r,i,"qbar")) ; 
   
* ---------------------------------------------
* Defininig Parameters
* ---------------------------------------------
Parameter
   cbar_ir(i,r)
   qbar_ir(i,r)
   beta(i,r);

cbar_ir(i,r)     = data(r,i,"c");
qbar_ir(i,r)  = data(r,i,"qbar");
beta(i,r)  = data(r,i,"beta");

V(i,r)$qbar_ir(i,r) = yes ; 




* ---------------------------------------------------------
* Processing Stage
* ---------------------------------------------------------
Set
    s  'Processing countries'
        / China, SouthKorea, Japan, Germany, Australia, Argentina, Chile, USA, Brazil /
        
    j  'Processed products'
        / LiOH, Li2CO3 /
        
    f(s) / SouthKorea, Japan, Germany, Australia, Argentina, Chile, USA, Brazil /
    
    ore_pr(i,j) /hardrock.LiOH, brine.Li2CO3/
    
    np(r, s, i) 'Valid network from mining (i) to processing countries (j) by ore type'
;

alias(r,rr) ; 
alias(s,ss) ; 

set l(j,s) "leader" ;
*!!! come back to this
l(j,s)$(not f(s)) = yes ; 


* ---------- PARAMETERS FROM STAGE 1 ----------
Parameter
    qbar_ir(i,r)      'Mining quantities loaded from Cournot stage'
    share(r,s,i) 'Share of ore o from mining country i sent to processor pc'
;

* -------------------------------
* Share Tables (Split by Ore Type)
* -------------------------------
Table share_hardrock(r,s)

                   China    SouthKorea     Japan    Germany   Australia   Argentina   Chile    USA   Brazil
Australia           0.9        0.05        0.04      0.01          0.0       0.0      0.0      0.0     0.0
Zimbabwe            1.0        0.0         0.0       0.0           0.0       0.0      0.0      0.0     0.0
Brazil              0.7        0.0         0.0       0.0           0.0       0.0      0.0      0.0     0.3
China               1.0        0.0         0.0       0.0           0.0       0.0      0.0      0.0     0.0
USA                 0.0        0.0         0.0       0.0           0.0       0.0      0.0      0.0     0.0 
canada              1.0        0.0         0.0       0.0           0.0       0.0      0.0      0.0     0.0;

Table share_brine(r,s)

                    China   SouthKorea     Japan   Germany   Australia   Argentina   Chile     USA   Brazil
Chile                0.7        0.0         0.0      0.0        0.0       0.0         0.3      0.0     0.0
Argentina            0.6        0.0         0.1      0.0        0.0       0.3         0.0      0.0     0.0
China                1.0        0.0         0.0      0.0        0.0       0.0         0.0      0.0     0.0
USA                  1.0        0.0         0.0      0.0        0.0       0.0         0.0      0.0     0.0
;

*----------------Shipment network--------------
share(r,s,'hardrock') = share_hardrock(r,s);
share(r,s,'brine')    = share_brine(r,s);
np(r,s,i)$(share(r,s,i) > 0) = yes;

$ontext
* -------------------------------
* Ore Grades (Li2O % or ppm)
* -------------------------------
Parameter grade(i, o) 'Ore grade: Li2O % for hardrock, ppm for brine';

grade('Australia', 'hardrock')   = 0.5;    
grade('Zimbabwe', 'hardrock')    = 0.5;   
grade('Brazil', 'hardrock')      = 0.5;    
grade('China', 'hardrock')       = 0.5;    
grade('USA', 'hardrock')         = 0.5;
    
grade('Chile', 'brine')          = 800;    
grade('Argentina', 'brine')      = 700;   
grade('China', 'brine')          = 900;    
grade('USA', 'brine')            = 1100;   
$offtext

* -------------------------------
* Processing Parameters (Recovery, Conversion, Capacity, Cost, etc.)
* -------------------------------
Table proc_data(s, i, *)
                         recovery   conversion   ybar_proc   c_bar    gamma
   China.hardrock          0.85        6.048      701000     6350        3
   China.brine             0.60        5.324      414000     5500        3
   Australia.hardrock      0.85        6.048      2178       6600        3
   Chile.brine             0.65        5.323      170000     4000        3
   SouthKorea.hardrock     0.80        6.048      6000       6500        3
   Japan.hardrock          0.80        6.048      7000       6500        3
   Germany.hardrock        0.78        6.048      5000       7500        3
   Argentina.brine         0.65        5.324      74600      5000        3       
   Brazil.hardrock         0.75        6.048      12000      6000        3
;

parameter proc_share(s,i)
/
china.brine 0.653, 
chile.brine 0.293,
argentina.brine 0.054,
china.hardrock 0.85,
australia.hardrock 0.10,
japan.hardrock 0.02,
southkorea.hardrock 0.02
*USA.hardrock 0.01
*germany.hardrock 0.005
/;

proc_data(s,i,"ybar_proc")$proc_share(s,i) = 
    proc_share(s,i) * sum(ss, proc_data(ss,i,"ybar_proc")) ; 

proc_data(s,i,"ybar_proc")$[not proc_share(s,i)] = 0 ; 


set i_j(i, j) /hardrock.LiOH, brine.Li2CO3/ ;

* ---------- PARAMETERS: PROCESSING CHAIN ----------
parameter
    recovery(j,s)     'Li recovery efficiency'
    conversion(j,s)   'Conversion factor to final product'
    ybar_procbar_ir(j,s)    'refrence processing output level '
    qbar_js(j,s)
    cbar_js(j,s)        'refrence processing cost'
    pbar_j(j)    "refernece weighted average cost"
    gamma(j,s)        'Supply elasticity for refining cost'
;

* Extract from table
cbar_js(j,s)     = sum(i$i_j(i,j), proc_data(s,i,"c_bar"));
recovery(j,s)   = sum(i$i_j(i,j), proc_data(s,i,"recovery"));
conversion(j,s) = sum(i$i_j(i,j), proc_data(s,i,"conversion"));
qbar_js(j,s)  = sum(i$i_j(i,j), proc_data(s,i,"ybar_proc"));
gamma(j,s)      = sum(i$i_j(i,j), proc_data(s,i,"gamma"));

pbar_j(j) = sum(s,cbar_js(j,s) * qbar_js(j,s)) / sum(s, qbar_js(j,s)) ; 
* these get to be best guesses from https://www.lme.com/en/metals/ev/lme-lithium-hydroxide-cif-fastmarkets-mb#Price+graph
pbar_j("lioh") = 11350 ; 
pbar_j("li2co3") = 11218; 

positive variable gamma_v(j,s), cbar_js_v(j,s) ; 

set
V_QM(i,r)
V_QN(j,s)
V_QS(i,j,r,s)
;

V_QM(i,r)$qbar_ir(i,r) = yes ; 
V_QN(j,s)$qbar_js(j,s) = yes ; 
V_QS(i,j,r,s)$[V_QM(i,r)$V_QN(j,s)] = yes ; 

* -- Begin primal -- *
positive variables QM(i,r) "quantity mined", 
                   QS(i,j,r,s) "quantity shipped and used in refining", 
                   QN(j,s) "quantity refined",
                   LAMBDA_M(i,r),
                   lambda_n(j,s) ; 

scalar sw_cournot_lower /0/ ; 
scalar sw_cournot_upper /0/ ; 

parameter i_elas(i);
parameter j_elas(j);
* starting values - assume inelastic
i_elas(i) = -0.5 ; 
j_elas(j) = -0.5 ;  

parameter qbar_i(i) ; 
* initial value - variable gets pinned
qbar_i(i) = sum(r,qbar_ir(i,r)) * 0.75 ** (i_elas(i))  ;

parameter qbar_j(j) ;
* initial value - variable gets pinned
qbar_j(j) = sum(s,qbar_js(j,s)) ;  

parameter qbar_i_add(i) "additional qbar_i from exogenous entrant"
          qbar_j_add(j) "additional qbar_j from exogenous entrant"
;
qbar_i_add(i) = 0 ; 
qbar_j_add(j) = 0 ; 


parameter theta(i,j) ; 
theta("brine","Li2CO3") = 7.45 ; 
theta("hardrock","LiOH") = 11.322 ; 
* tinker tinker
*theta(i,j)$[qbar_i(i)$qbar_j(j)] = 100 * qbar_i(i) / qbar_j(j) ;



equation M_Balance(i,r), N_Balance(j,s) ; 

* -- market clearing conditions -- *
M_balance(i,r)$V_QM(i,r)..
    QM(i,r) =g= sum((j,s)$V_QS(i,j,r,s), QS(i,j,r,s) ) ;

alias(j,jj); 
alias(s,ss) ; 
alias(i,ii); 
alias(r,rr); 

N_balance(j,s)$V_QN(j,s)..
    sum((i,r)$V_QS(i,j,r,s), theta(i,j) * [QS(i,j,r,s) + qbar_i_add(i) / sum((ii,rr)$V_QS(ii,j,rr,s),1)]  )  =g= QN(j,s) + qbar_j_add(j) / sum((jj,ss)$V_QN(jj,ss), 1) ; 
    ;

equation extra_accounting ; 

* -- zero profit conditions -- *

Equations
FOC_QM(i,r),
FOC_QS(i,j,r,s),
FOC_QN(j,s)
; 

variable i_elas_v(i), j_elas_v(j) ; 
i_elas_v.fx(i) = i_elas(i) ; 
j_elas_v.fx(j) = j_elas(j) ; 

positive variable cbar_i_v(i) ; 
cbar_i_v.fx(i) = cbar_i(i) ; 

positive variable beta_v(i,r), cbar_ir_v(i,r) ; 

FOC_QM(i,r)$V_QM(i,r).. 
    cbar_ir_v(i,r) * (QM(i,r)/qbar_ir(i,r)) ** (1/beta(i,r))
    =g= 
    LAMBDA_M(i,r) 
* partial price / partial QM
    - QM(i,r) * (cbar_i_v(i) / (i_elas_v(i) * qbar_i(i)) * 
        (sum(rr$V_QM(i,rr), QM(i,rr)) / qbar_i(i))
      **(1/i_elas_v(i) - 1) )$[Sw_Cournot_Lower$(not sw_pc)]
;

FOC_QS(i,j,r,s)$V_QS(i,j,r,s).. 
    (cbar_i_v(i) * (sum(rr$V_QM(i,rr), (QM(i,rr)+qbar_i_add(i))) / qbar_i(i))**(1/i_elas_v(i)))$(not sw_pc)
    + LAMBDA_M(i,r)
    =g=
    theta(i,j) * LAMBDA_N(j,s) 
;


positive variable alpha_j, beta_j, c_s(j,s), d_s(j,s) ; 

alpha_j.fx = 103200; 
beta_j.fx = 110.3 ;
c_s.fx(j,s)$qbar_js(j,s) = 10;  
d_s.fx(j,s)$qbar_js(j,s) = 1 ; 

scalar Sw_Stackelberg /1/ ; 
scalar sw_upper_linear /1/ ; 

set upper_linear(j) ; 

* options..
* cournot upper, non-linear (sw_cournot_upper = 1)
* upper non-linear, no stackelberg (sw_linear = 0, sw_stackelberg = 0)
* upper linear, no stackelberg (sw_linear = 1, sw_stackelberg = 0)
* upper linear, with stackelberg (sw_linear = 1, sw_stackelberg = 1)

scalar sw_adj /0/ ; 

positive variable pbar_j_v(j) ; 

FOC_QN(j,s)$V_QN(j,s)..

* -- begin stackelberg -- *
* parentheses here to capture j = li2co3
* optimality condition for ARGENTINA
    + (( ( (alpha_j - beta_j * (QN(j,"China") + QN(j,"Chile") -qbar_j_add(j) ) - c_s(j,"Argentina") - LAMBDA_N(j,"Argentina"))
         / (2 * (beta_j + d_s(j,"Argentina")))
  )$[sameas(s,"Argentina")]
    
* optimality condition for CHILE
    + ( ( 2 * (beta_j + d_s(j,"Argentina")) * (c_s(j,"Chile") + LAMBDA_N(j,"Chile")) 
        + (beta_j + 2 * d_s(j,"Argentina")) * beta_j * (QN(j,"China") + qbar_j_add(j) )
        - (beta_j + 2 * d_s(j,"Argentina"))*alpha_j 
        - beta_j * (c_s(j,"Argentina") + LAMBDA_N(j,"Argentina")) )
    / (-2 * (beta_j + 2 * d_s(j,"Argentina")) * beta_j - 4 * d_s(j,"Chile") * (beta_j + d_s(j,"Argentina")))
    )$[sameas(s,"Chile")]

* optimality condition for CHINA
    + (
        (((beta_j + 2 * d_s(j,"Argentina"))**2) * (alpha_j - c_s(j,"China") - LAMBDA_N(j,"China")) 
        - (beta_j + 2 * d_s(j,"Argentina"))*beta_j * (c_s(j,"Chile") + LAMBDA_N(j,"Chile")) 
        - beta_j * (beta_j + 2 * d_s(j,"Argentina")) * (c_s(j,"Argentina") + LAMBDA_N(j,"Argentina"))
        ) / (
        ((beta_j + 2 * d_s(j,"Argentina"))**2) * ( beta_j + d_s(j,"China") + d_s(j,"Chile") ) 
        + beta_j * (beta_j + 2 * d_s(j,"Argentina"))**2  )  + beta_j * qbar_j_add(j) / 4
      )$[sameas(s,"China")]
* note here - optimality conditions are definitional and thus
* can use the difference from QN to activate the complementarity constraint for all producers
      - QN(j,s)
    )$[sameas(j,"Li2Co3")$Sw_Stackelberg$(not sw_cournot_upper)$(not sw_pc)]

    + [c_s(j,s) + d_s(j,s) * QN(j,s) + lambda_n(j,s)]$[sameas(j,"Li2Co3")$(not Sw_Stackelberg)$(sw_upper_linear or sw_pc)]

    + ((cbar_js_v(j,s) * (QN(j,s)/qbar_js(j,s)) ** (1/gamma_v(j,s))
        + LAMBDA_N(j,s)))$[(not sw_upper_linear)$sameas(j,"Li2CO3")$(not sw_stackelberg)]

    + [c_s(j,s) + d_s(j,s) * QN(j,s) + lambda_n(j,s)]$[sameas(j,"lioh")$sw_upper_linear])$sw_adj

* default cournot representation
    + ((cbar_js_v(j,s) * (QN(j,s)/qbar_js(j,s)) ** (1/gamma_v(j,s))
        + LAMBDA_N(j,s)))$[(not sw_upper_linear)]


    =g= 

    (pbar_j_v(j) * ((qbar_j_add(j) + sum(ss$V_QN(j,ss), QN(j,ss))) / qbar_j(j))
      **(1/j_elas_v(j)))$[(not sw_upper_linear)]


* partial price / partial QM - nonlinear
* note separate terms here for refined products
* just making it easy instead of having a bunch of chained switches
    + (QN(j,s) * (pbar_j_v(j) / (j_elas_v(j) * (qbar_j(j) - qbar_j_add(j))  ) * 
        ((sum(ss$V_QN(j,ss), QN(j,ss)) + qbar_j_add(j)) / qbar_j(j))
      **(1/j_elas_v(j) - 1) )
        )$[sw_cournot_upper$(not sw_pc)$sameas(j,"LiOH")]

    + (QN(j,s) * (pbar_j_v(j) / (j_elas_v(j) * (qbar_j(j) - qbar_j_add(j))  ) * 
        (sum(ss$V_QN(j,ss), QN(j,ss)) / qbar_j(j))
      **(1/j_elas_v(j) - 1) )
        )$[sw_cournot_upper$(not sw_stackelberg)
          $sameas(j,"Li2CO3")$(not sw_pc)]

* linear terms, with and without stackelberg
    + [alpha_j - 2 * beta_j * sum((ss)$v_qn(j,ss), QN(j,ss))]$[sameas(j,"Li2Co3")$(not Sw_Stackelberg)$sw_upper_linear$(not sw_pc)]
    + [alpha_j - 2 * beta_j * sum((ss)$v_qn(j,ss), QN(j,ss))]$[(not sameas(j,"Li2Co3"))$(sw_upper_linear)$(not sw_pc)]
;

model bilevel_mcp
/
M_balance.LAMBDA_M
N_balance.LAMBDA_N
FOC_QM.QM
FOC_QS.QS
FOC_QN.QN
/
;


QM.lo(i,r)$V_QM(i,r) = 0.1 * qbar_ir(i,r) ; 
QN.lo(j,s)$V_QN(j,s) = 0.1 * qbar_js(j,s) ; 

j_elas(j) = -0.4; 
i_elas(i) = -0.5 ; 
gamma(j,s) = 0.5 ; 
beta(i,r) = 0.5 ; 

qbar_ir("hardrock",r) = qbar_ir("hardrock",r) / 2 ; 
i_j(i,j) = no ; 
i_j(i,j)$theta(i,j) = yes ; 

qbar_i(i) = sum(r,qbar_ir(i,r)) * 0.75 ** (i_elas(i))  ;
qbar_j(j) = sum(s,qbar_js(j,s)) * 0.75 ** (j_elas(j)) ;  

i_elas_v.fx(i) = i_elas(i) ; 
j_elas_v.fx(j) = j_elas(j) ; 
pbar_j_v.fx(j) = pbar_j(j) ; 

* reset switches
sw_cournot_lower = 1 ; 
sw_cournot_upper = 1 ; 
sw_stackelberg = 0 ; 
sw_upper_linear = 0 ;

gamma_v.fx(j,s) = gamma(j,s) ; 
cbar_js_v.fx(j,s) = cbar_js(j,s) ; 

beta_v.fx(i,r) = beta(i,r) ; 
cbar_ir_v.fx(i,r) = cbar_ir(i,r) ; 


solve bilevel_mcp using mcp ; 
parameter rep ; 
rep("mcp_cournot","ratio_mining",i,r)$qbar_ir(i,r) = qm.l(i,r) / qbar_ir(i,r) ; 
rep("mcp_cournot","ratio_refining",j,s)$qbar_js(j,s) = qn.l(j,s) / qbar_js(j,s) ; 


* reset switches
sw_cournot_lower = 1 ; 
sw_cournot_upper = 0 ; 
sw_stackelberg = 1 ; 
sw_upper_linear = 0 ;

c_s.fx(j,s)$qbar_js(j,s) = 1; 
d_s.fx(j,s)$qbar_js(j,s) = 1; 

i_elas_v.fx(i) = i_elas(i) ; 
j_elas_v.fx(j) = j_elas(j) ; 
pbar_j_v.fx(j) = pbar_j(j) ; 
beta_j.fx = 0.5 ; 

*!! this solve can be ignored - stackelberg game not fully calibrated to PC setup
solve bilevel_mcp using mcp ; 
rep("mcp_stackelberg","ratio_mining",i,r)$qbar_ir(i,r) = qm.l(i,r) / qbar_ir(i,r) ; 
rep("mcp_stackelberg","ratio_refining",j,s)$qbar_js(j,s) = qn.l(j,s) / qbar_js(j,s) ; 






* -- begin structural calibration -- *
equation eq_mpec_obj ; 
variable mpec_obj ; 

scalar sw_mpec_ir /1/, sw_mpec_js /1/, mpec_obj_scale /1e5/ ; 


eq_mpec_obj.. mpec_obj =e= (sum((i,r)$v_qm(i,r),power(QM(i,r)-qbar_ir(i,r),2))$sw_mpec_ir 
                           + sum((j,s)$v_qn(j,s),power(QN(j,s)-qbar_js(j,s),2))$sw_mpec_js ) / mpec_obj_scale    ; 

model mpec_calib
/
M_balance.LAMBDA_M
N_balance.LAMBDA_N
FOC_QM.QM
FOC_QS.QS
FOC_QN.QN
eq_mpec_obj
/
;

* note that this kicks off the mpec option file (nlpec.opt)
* which just directs to the conopt.opt file 
mpec_calib.optfile = 1; 


sw_cournot_lower = 1 ; 
sw_cournot_upper = 1 ; 
sw_stackelberg = 0 ; 
sw_upper_linear = 0 ;
solve bilevel_mcp using mcp ; 

scalar low_bound /1e-2/ ; 


* ranges here are very important and can make or break the solution stability
* be very careful going to extremes here, it will not converge if too far from a reasonable point
alpha_j.lo = 0.01 ; 
beta_j.lo =  low_bound ; 
c_s.lo(j,s) =  low_bound ; 
c_s.up(j,s) = 1e5 ; 
*c_s.l(j,s) = 1 ; 
*d_s.l(j,s) = 10 ; 
d_s.lo(j,s) =  low_bound ; 
d_s.up(j,s) = 10000 ; 

i_elas_v.up(i) = -1e-2 ; 
i_elas_v.l(i) = i_elas(i) ; 
i_elas_v.lo(i) = -100 ; 

j_elas_v.up(j) = -0.01 ; 
j_elas_v.l(j) = j_elas(j) ; 
j_elas_v.lo(j) = -0.95 ; 

gamma_v.lo(j,s) = 0.3 ; 
gamma_v.up(j,s) = 0.99 ; 
cbar_js_v.lo(j,s) = 0.1 * cbar_js(j,s) ; 
cbar_js_v.up(j,s) = 5 * cbar_js(j,s); 
pbar_j_v.lo(j) = 0.5 * pbar_j(j) ; 
pbar_j_v.up(j) = 3 * pbar_j(j) ; 
cbar_i_v.lo(i) = 0.2 * cbar_i(i) ; 
cbar_i_v.up(i) = 3 * cbar_i(i) ; 

beta_v.lo(i,r) = 0.5 * beta(i,r) ; 
beta_v.up(i,r) = 1.5 * beta(i,r) ; 

