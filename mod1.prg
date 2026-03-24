'1 create the workfile 

wfcreate(wf=model11) u 1 1000

'2 create series 

series Y  


'NFC sector 


   
series I
series z
series RR
series K
series Ff
series Yn
series i0
series i1
series i2
series v
series lambda 
series Dimp                    'demand for deposits 
series NLimp                  ' net lending imp
series Limp
series sLimp

series REPimp
series phiImp

series kappa
series Kt



'intereset rate 
series iL                      'interest on loan

series iD                      'interest on deposit

'household
series ct 
series c1
series c2

series alpha1
series alpha2
series w1
series w2
series Lhh1

series REP
series Lhh2

series REP2
series Dhh1
series Dhh2
series W11
series NW11
series Ph
series W22             'wealth hh2
series NW22           'netwealth hh2
series Yd1     'disposable income hh1
series Yd2      'disposable income hh2
series NLhh1
series NLhh2
series betha1
series betha2 

series EMU
series gamma1
series DLhh2
series DLhh1
series phi1
series phi2

series C0
series envy
series alpha3
series rho
series sigma1

series gamma2









'banks 


series Ld         'totals loan demanded 
series Dh         'depositis held 

series DS         'depositis supplied 

series Fb            'banks profits 

series Ls              'supply of loans 


series Dd1
series Dd2
series NLtot

series Fu           'MARCO CHANGE
series theta       'MARCO CHANGE
series Div       'MARCO CHANGE





' set parameteres values and initial value for the variables 
NLtot=0
Y=0

I=0
z=0
RR=0
K=1
Ff=0

theta =0.85           '0.9     'Profit retention rate  <-----------------------------------------------------------------------------------------------------------------------------------------
Fu=theta*Ff     'Retained profits     'MARCO CHANGE
Div = 0  'Distributed profits (dividends)  'MARCO CHANGE


Yn=1
i0=100   '0
i1=0.15
i2=0.53                
lambda=0.2 
v=143.22/106.98   
NLimp=0
Limp=0
sLimp=0
REPimp=0

Kt=0
kappa=1'0.5  '0.5    '0.9            '0.8

iD=0'.01   <- To perform sensitivity
iL=0.02 

'household
ct=0                       'total cons
c1=1                    'cons1 
c2=1                     'cons2

alpha1=0.3           ' cons par.  0.4
alpha2=0.6'0.7    '0.7 '1         ' cons par.
w1=650                       'wage
w2=0'400                      'wage 
Lhh1=0     '1000                     ' demand for loans 

REP=0                        'parameters for Lhh1
Lhh2=0                          'demand for loans 
REP2=0                     'parameters for Lhh2
W11=0  '1000                             'wealth 1
NW11=0                        'netwealth1
W22=0                          'wealth2
NW22=0                         'netwealth2
Yd1=0                             'disposable income 1
Yd2=2                              'disposable income 2
NLhh1=0                          'netlending 1
NLhh2=0                         'netlending 2
betha1=0.2
betha2=0.4 

EMU=0     
gamma1=0.7     '1  '0.4     
gamma2=0.8
DLhh2=0
DLhh1=0
phi1=0.1
phi2=0.1    '0.09       '0.03
phiImp=0'0.09   '1    '0.07            '0.07 
C0= 0
envy=0
alpha3=0
rho=0'.2'.2 '(yd1/yd2)-1 '0.6
sigma1=1

'banks 

Ld=0    '1000      
Dh=0         '1000        
DS = Dh
Fb=0

Ls=0      '1000      







'etha=0.5


'portfolio function 
'hh1


Dd1=0      '1000      '4000
Dd2=0        '4000
Dimp=0
'create model 

model model11

model11.append Y=ct+I

'government                il modello non è più consistiente dopo aver introdotto nuovamente i gamma nelle funzioni di risparmio e domanda di prestiti <---------------------------------------------------------------------------------------------------------------------------------------------------------------

'imprese 

model11.append w2=y*0.3      
model11.append Ff=ct+I-w1-w2-iL*Limp(-1)-REPimp   'profitti    aggiungere interessi su df se reinseriti
model11.append Fu = Ff*theta                                  'profitti non dis
model11.append Div = Ff-Fu                                       'dividendi
model11.append K=K(-1)+I(-1)-lambda*K(-1)           'law of motition of capital
model11.append Dimp=0  
model11.append NLimp=I-Fu                                            'netlending imprese
model11.append Limp=Limp(-1)-REPimp+I-Fu                                                                   'domanda di prestiti imprese                              
model11.append Kt=kappa*Y(-1)
model11.append I=i1*(Kt-K(-1))+lambda*K(-1)    'investimenti

'banks 
model11.append Ld=Limp+Lhh1+Lhh2                                                                     'total loans demanded   Lw 
model11.append Dh=Dd1+Dd2                                                                       'total deposits held '<-----------------------------------------------
model11.append Fb=iL*Ld(-1)-iD*Dh(-1)       'banks profits   
model11.append Ls=Ls(-1)+(Ld-Ld(-1))                                                                   'total loans 
model11.append iL=@recode(rho=0.2, 0.02, 0.021)                                   '@recode((lhh2/yd2)<1, 0.02 , 0.02)                                         '@recode(rho=0.2, 0.02, 0.023)

'households 
model11.append Yd1= w1+iD*Dd1(-1)+Fb+Div-iL*Lhh1(-1)-REP'-iL*Lhh1(-1)                                                                           'reddito disponibile rentiers 
model11.append c1=alpha1*Yd1+betha1*W11(-1)'+((0.7*(yd1(-1))+0.8*(yd1(-1)-yd1(-2))))*rho                        'consumo rentiers 
model11.append c2=(C0)+Yd2*alpha2+betha2*W22(-1)                              'consumo workers            '
model11.append C0=EMU                                                                                 'componente "autonoma"  del consumo
model11.append EMU= alpha3*envy                                                                            'componente emulativa
'model11.append envy=@recode(lhh2/yd2<2,(((c1+c2)/2))*0.8, (((c1+c2)/2))*0.5)             'domanda di credito per emulazione consumi medi                
model11.append envy=((c1+c2)/2)*0.5                                                    '                               '                                                                                                                                                                                                                                                                                                                                                                                                                                                  
'model11.append envy=@recode((lhh2/yd2)<2, (0.8*(yd2(-1))*alpha3+0.7*(yd2(-1)-yd2(-2))*alpha3) , (0.7*(yd2(-1))*alpha3+0.6*(yd2(-1)-yd2(-2))*alpha3)  )            '   domanda di credito Palley con leverage ceiling
model11.append  alpha3=sigma1*rho                                                                                                               'accesso al credito  versione base no leverage ceiling 
'model11.append  alpha3=@recode((lhh2/yd2)<1.85, sigma1*rho, sigma1*0.25)                                      'accesso al credito   minskyan ext.   con leverage ceiling 
'model11.append  alpha3=@recode((lhh2/yd2)<2.5, sigma1*rho, sigma1*0.25)                                              'accesso al credito emulazione consumo con leverage ceiling 
model11.append ct=c1+c2                                                                                 'consumo totale
                                                                        
model11.append  W11=W11(-1)+@recode(Yd1-c1>0,Yd1-c1,0)     'ricchezza rentiers  
                                                                            
model11.append NW11=W11-Lhh1                                                                   'ricchezza netta rentiers
model11.append DLhh1=@recode(c1-Yd1>0,c1-Yd1,0)                                'domanda prestiti rentiers 

'model11.append Yd2=w2+iD*Dd2(-1)-iL*Lhh2(-1)'-REP
model11.append Yd2=w2+iD*Dd2(-1)-iL*Lhh2(-1)'-REP2'-REP2'1)'-REP2                          'reddito disponibile workers 
model11.append REP2=phi2*Lhh2(-1)                                                           'rimborso debito workers 
model11.append REP=phi1*Lhh1(-1)                                                              'rimborso debito rentiers 
model11.append REPimp=phiImp*Limp(-1)
model11.append Lhh1= Lhh1(-1)+DLhh1-REP                                               'stock debito rentiers   
                                                                              
model11.append Lhh2=Lhh2(-1)+DLhh2-REP2                                                'stock debito rentiers 




model11.append DLhh2=@recode((c2)-Yd2>0,(c2)-Yd2,0)                                           
'model11.append DLhh2=C0+@recode((c2-C0)-Yd2>0,(c2-C0)-Yd2,0)
'model11.append DLhh2=C0'@recode(c2-Yd2>0,c2-Yd2,0)                                                                     'domanda prestiti workers 
'model11.append W22=W22(-1)+@recode(Yd2-(c2-C0)>0,Yd2-(c2-C0),0)'-REP2'-iL*Lhh2(-1)   'ricchezza workers
model11.append W22=W22(-1)+@recode(Yd2-c2>0,Yd2-c2,0)-REP2'-(iL*Lhh2(-1))'-REP2            '-(iL*Lhh2(-1))-REP2                    '-REP2-iL*Lhh2(-1) 

model11.append NLhh1= w1+iD*Dd1(-1)-c1 -iL*Lhh1(-1)                      'netlending rentiers 
model11.append NLhh2=w2+iD*Dd2(-1)-c2-iL*Lhh2(-1)                        'netlending workers 
model11.append Dd2=W22                                                                                 'stock di depositi workers 

model11.append Dd1=W11                                                                  '  stock di depositi rentiers 



'housing





model11.append DS=DS(-1) + d(LS)                                                    

model11.append NLtot=NLimp+Fb+NLhh1+NLhh2                  'netlending totale    'NLhh1+NLhh2+NLimp+Fb+DEF


  
'select scenario

model11.scenario "baseline"  

'set the sample 

smpl 1 1000

'solve the model 

model11.solve 


'scenario 1

model11.scenario "Scenario 1"
model11.override rho
copy rho rho_1
smpl 250 @last
rho_1=0.5'0.8'0.8'0.8'0.8'0.9'0.9'0.9  '0.8  '0.5 IS THE ORIGINAL PARAMETER IN THE ARTICLE
'rho_1=0.9     'shock per scenario con mpc=1 e funzione domanda prestiti minsky 
'smpl 270 @last
'rho_1 =0.6'0.8  '0.8  '0.5
'smpl 290 @last
'rho_1 =0.85'0.8  '0.8  '0.5
smpl @all
model11.solve(i=p)

'scenario 2

'
'model11.scenario(n, a="2")  "Scenario 2"
'model11.override rho
'copy rho rho_2
'smpl 250 @last
'rho_2=1'0.8'0.8'0.8'0.8'0.9'0.9'0.9  '0.8  '0.5
''rho_1=0.9     'shock per scenario con mpc=1 e funzione domanda prestiti minsky 
''smpl 270 @last
''rho_1 =0.6'0.8  '0.8  '0.5
''smpl 290 @last
''rho_1 =0.85'0.8  '0.8  '0.5
'smpl @all
'model11.solve(i=p)
'


'plot the result 

smpl 0 1000

'line Y_0 I_0 c1_0 c2_0 ct_0
'line Yd1_0 NLhh1_0 Lhh1_0  c1_0
'line Yd2_0 W22_0 NLhh2_0 Lhh2_0  c2_0

line DS_0-Dh_0 'hidden equation
line DS_1-Dh_1
smpl 248 270

'line Lhh2_2/Lhh2_0 
 'line c2_1/c2_0 Y_1/Y_0  I_1/I_0  c1_1/c1_0 ct_1/ct_0
'line lhh2_1/lhh2_0
'line lhh1_1/lhh1_0
'line W11_1/W11_0 
'line W22_1/W22_0 
'line y_1/Y_0
'line c2_1/c2_0
'line c1_1/c1_0
'line I_1/I_0
'line w11_1/W11_0  
'line Lhh2_1/Lhh2_0


graph fig1.line       c2_1/c2_0  (lhh2_1*0.001)+1  'c2_2/c2_0  (lhh2_2*0.001)+1 '1.65  1.85'  (lhh2_1/yd2_1)      (lhh2_1/yd2_1)                        yd2_1/yd2_0 lhh2_1/yd2_1        c2_1/c2_0  Yd2_1/Yd2_0 (lhh2_1*0.001)+1    (lhh2_1/yd2_1) yd2_1/yd2_0 (lhh2_1)*0.001 c2_1/c2_0
'fig1.draw(shade, bottom, color(246, 245, 238)) 248 270
fig1.draw(line, left, color(black),width(1),pattern(1)) '0.1
fig1.options linepat
fig1.options gridl gridb gridauto frameaxes("none")
fig1.axis mirror
'fig1.setelem(1) lcolor(black) lwidth(2) lpat(1)
'fig1.setelem(2) lcolor(black) lwidth(2) lpat(2)
'fig1.setelem(3) lcolor(grey) lwidth(2) lpat(1)
'fig1.setelem(4) lcolor(grey) lwidth(2) lpat(2)
'fig1.setelem(5) lcolor(grey) lwidth(2) lpat(2)
'fig1.name(1) w disp income 
fig1.name(1)  w consumption ' (lower emulation)                                 'w loans  'rentiers consumption
fig1.name(2) w stock of debt '(lower emulation)
'fig1.name(3)  w consumption (stronger emulation)
'fig1.name(4) w stock of debt (stronger emulation)
fig1.addtext(t,just(c),font(16))  Workers Consumption and stock of debt   'rentiers consumption and disposable income 
fig1.legend -inbox
show fig1
fig1.save(t=jpg) fig1

graph fig2.line      yd2_1/yd2_0  (lhh2_1*0.001)+1 'yd2_2/yd2_0  (lhh2_2*0.001)+1 '1.65  1.85'  (lhh2_1/yd2_1)      (lhh2_1/yd2_1)                        yd2_1/yd2_0 lhh2_1/yd2_1        c2_1/c2_0  Yd2_1/Yd2_0 (lhh2_1*0.001)+1    (lhh2_1/yd2_1) yd2_1/yd2_0 (lhh2_1)*0.001 c2_1/c2_0
'fig2.draw(shade, bottom, color(246, 245, 238)) 248 270
fig2.draw(line, left, color(black),width(1),pattern(1)) '0.1
fig2.options linepat
fig2.options gridl gridb gridauto frameaxes("none")
fig2.axis mirror
'fig2.setelem(1) lcolor(black) lwidth(2) lpat(1)
'fig2.setelem(2) lcolor(black) lwidth(2) lpat(2)
'fig2.setelem(3) lcolor(grey) lwidth(2) lpat(1)
'fig2.setelem(4) lcolor(grey) lwidth(2) lpat(2)
'fig2.setelem(5) lcolor(green) lwidth(2) lpat(2)
fig2.name(1) w disp income (lower emulation)
'fig2.name(1)  w consumption                                   'w loans  'rentiers consumption
fig2.name(2) w stock of debt (lower emulation)
'fig2.name(3) w disp income (stronger emulation)
''fig2.name(1)  w consumption                                   'w loans  'rentiers consumption
'fig2.name(4) w stock of debt (stronger emulation)
'fig2.name(4) demand for loans 
'fig2.name(5) leverage
fig2.addtext(t,just(c),font(16))  Workers Disposable Income and stock of debt   'rentiers consumption and disposable income 
fig2.legend -inbox
show fig2
fig2.save(t=jpg) fig2




graph fig4.line yd2_1/yd2_0  (dlhh2_1*0.001)+1     c2_1/c2_0 'yd2_2/yd2_0  (dlhh2_2*0.001)+1     c2_2/c2_0 '( lhh1_1/yd1_1)+1
'fig4.draw(shade, bottom, color(246, 245, 238)) 248 270
fig4.draw(line, left, color(black),width(1),pattern(1)) '0.1
fig4.options linepat
fig4.options gridl gridb gridauto frameaxes("none")
fig4.axis mirror
'fig4.setelem(1) lcolor(black) lwidth(2) lpat(1)
'fig4.setelem(2) lcolor(black) lwidth(2) lpat(2)
'fig4.setelem(3) lcolor(black) lwidth(2) lpat(3)
'fig4.setelem(4) lcolor(grey) lwidth(2) lpat(1)
'fig4.setelem(5) lcolor(grey) lwidth(2) lpat(2)
'fig4.setelem(6) lcolor(grey) lwidth(2) lpat(3)
fig4.name(1) w disp income  (lower emulation) 
fig4.name(2) Workers demand for loans ' (lower emulation)                       'w loans  'rentiers consumption
'fig4.name(3) workers stock of debt
fig4.name(3) workers consumption  ' (lower emulation) 
'fig4.name(4) w disp income  (stronger emulation) 
'fig4.name(5) Workers demand for loans   (stronger emulation)                      'w loans  'rentiers consumption
'fig4.name(3) workers stock of debt
fig4.name(6) workers consumption   (stronger emulation) 

'fig1.name(5) constot
fig4.addtext(t,just(c),font(16))     'rentiers consumption and disposable income 
fig4.legend -inbox
show fig4
fig4.save(t=jpg) fig4




graph fig5.line c1_1/c1_0 'c1_2/c1_0'( lhh1_1/yd1_1)+1
'fig5.draw(shade, bottom, color(246, 245, 238)) 248 270
fig5.draw(line, left, color(black),width(1),pattern(1)) '0.1
fig5.options linepat
fig5.options gridl gridb gridauto frameaxes("none")
fig5.axis mirror
'fig5.setelem(1) lcolor(black) lwidth(2) lpat(1)
'fig5.setelem(2) lcolor(black) lwidth(2) lpat(2)
'fig5.setelem(2) lcolor(grey) lwidth(2) lpat(1)
'fig5.setelem(4) lcolor(grey) lwidth(2) lpat(2)
'fig4.setelem(5) lcolor(green) lwidth(2) lpat(2)
'fig5.name(1)  rentiers disposable income'Gdp 
'fig5.name(2) rentiers stock of debt                    'w loans  'rentiers consumption
fig5.name(1) rentiers consumption ' (lower emulation) 
'fig5.name(2) rentiers consumption  (stronger emulation)
'fig5.name(4) workers consumption 
'fig1.name(5) constot
fig5.addtext(t,just(c),font(16))  rentiers consumption     'rentiers consumption and disposable income 
fig5.legend -inbox
show fig5
fig5.save(t=jpg) fig5




graph fig7.line  y_1/y_0 'y_2/y_0                                        'yd2_1/yd2_0 (lhh2_1*0.001)+0.5 c2_1/c2_0'( lhh1_1/yd1_1)+1
'fig7.draw(shade, bottom, color(246, 245, 238)) 248 270
fig5.draw(line, left, color(black),width(1),pattern(1)) '0.1
fig7.options linepat
fig7.options gridl gridb gridauto frameaxes("none")
fig7.axis mirror
'fig7.setelem(1) lcolor(black) lwidth(2) lpat(1)
'fig7.setelem(2) lcolor(black) lwidth(2) lpat(2)
'fig7.setelem(2) lcolor(grey) lwidth(2) lpat(1)
'fig7.setelem(4) lcolor(grey) lwidth(2) lpat(2)
'fig4.setelem(5) lcolor(green) lwidth(2) lpat(2)
fig7.name(1)  Gdp  ' (lower emulation) 
'fig7.name(2) Gdp      (stronger emulation)                 'w loans  'rentiers consumption
'fig7.name(3) workers consumption 
'fig5.name(4) workers consumption 
'fig1.name(5) constot
fig7.addtext(t,just(c),font(16))GDP'  workers disposable income and stock of debt after shock    'rentiers consumption and disposable income 
fig7.legend -inbox
show fig7
fig7.save(t=jpg) fig7



graph fig9.line    c2_1/c2_0  Y_1/Y_0    I_1/I_0  c1_1/c1_0  'c2_2/c2_0  Y_2/Y_0    I_2/I_0  c1_2/c1_0                                              '(lhh2_1)*0.001 yd2_1/yd2_0 lhh2_1/yd2_1 
'fig9.draw(shade, bottom, color(246, 245, 238)) 248 295
fig9.draw(line, left, color(black),width(1),pattern(1)) '0.1
fig9.options linepat
fig9.options gridl gridb gridauto frameaxes("none")
fig9.axis mirror
'fig9.setelem(1) lcolor(black) lwidth(2) lpat(1)
'fig9.setelem(2) lcolor(black) lwidth(2) lpat(2)
'fig9.setelem(3) lcolor(black) lwidth(2) lpat(3)
'fig9.setelem(4) lcolor(black) lwidth(2) lpat(4)
'fig9.setelem(5) lcolor(grey) lwidth(2) lpat(1)
'fig9.setelem(6) lcolor(grey) lwidth(2) lpat(2)
'fig9.setelem(7) lcolor(grey) lwidth(2) lpat(3)
'fig9.setelem(8) lcolor(grey) lwidth(2) lpat(4)
'fig1.setelem(5) lcolor(green) lwidth(2) lpat(2)
fig9.name(1) Workers consumption   '(lower emulation)
fig9.name(2) GDP    '  (lower emulation)                       'w loans  'rentiers consumption
fig9.name(3) Investment ' (lower emulation)
fig9.name(4) Rentiers consumption'   (lower emulation)
'fig9.name(5) Workers consumption   (stronger emulation)
'fig9.name(6) GDP                (stronger emulation)              'w loans  'rentiers consumption
'fig9.name(7) Investment   (stronger emulation)
'fig9.name(8) Rentiers consumption    (stronger emulation)
'fig1.name(5) constot
fig9.addtext(t,just(c),font(16))   Demand dynamic and GDP 'rentiers consumption and disposable income 
fig9.legend -inbox
show fig9
fig9.save(t=jpg) fig9

