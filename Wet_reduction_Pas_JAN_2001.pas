
unit  Wet_reduction_Pas_JAN_2001;

interface

uses sysutils;                           

const
    Sectors_N1=50;
    Sectors_N2=100;
    R_air=287.055; {j/kg K}
    Cpair=1006.0;
    gravity_const=9.806;
    TUBE_EXPANSION_RATIO=0.96;
    FILE_EXTENSION_SECTOR='SEC';
    FILE_EXTENSION_EQU='EQU';
    P_NIL=-1;
    P_BEGIN_END_DATA=0;
    P_VOLUME=1;
    P_FINS=2;
    P_FT=3;
    P_PT=4;
    P_PL=5;
    P_ROW_NO=6;
    P_CIRCUIT_NO=7;
    P_TUBE_PER_ROW=8;
    P_DC=9;
    P_Tt=10;
    P_Flow_Arrangement=11;
    P_ENHANCE_RATIO=12;
    P_Surface_Treatment=13;
    P_surface_Type=14;
    P_Fin_Type=15;
    P_Fin_Material=16;
    P_Waffle_Height=17; {including fin thickness}
    P_Xf=18;
    P_COUNTER_CROSS_FLOW=19;
    P_Tube_Type=20;
    P_Tube_Layout=21;
    Equivalent_circular=0;
    Sector_method=1;
    Schmidt=3;
    Hong_and_Webb=4;
type
    Sector_type1=array[1..Sectors_N1] of double;
    Sector_type2=array[1..Sectors_N2] of double;
    h_eff_type  = record
                    efficiency,h_co,effectiveness,j,h_mass,j_mass,Le,T_water_film_mean,eta_h:
                    double;
                  end;
    reduce_data = record
                    T_water_in,T_water_out,m_water:double;
                    T_wall_in,T_wall_out:double;
                    h_inside:double;
                    T_DB_in,T_DB_out,T_WB_in,T_WB_out:double;
                    T_dew_in,T_dew_out:double;
                    ma,G_air,Re_Dc,Re_Dh,Re_Dc_Ave,Re_Dh_Ave:double;
                    Enhance_Ratio:double;
                    humidity_wall_in,humidity_wall_out,humidity_inlet,humidity_outlet,Gamma,Condensate_Reynolds_Number:double;
                    humidity_air_in,humidity_air_out:double;
                    k_fin,k_wall:double;
                    staggered:boolean;
                    Mass_transfer,Mass_Transfer_Coeff,Lewis_analogy:double;
                    u_frontal:double;
                    Equ_Circle,Schmidt,Sector,Hong_Webb:h_eff_type;
                    Qs,Ql,Qt,Uo_w:double;
                    hs,hi,hd,js,jd,friction_factor,friction_factor_Ke_Kc:double;
                    Q_air,Q_water,Q_average,Q_latent,Q_sensible:double;
                    Ri,Ro,Rw,Rc,Rall:double;
                    P_value,R_value,F_value:double;
                    pressure_drop:double;
                    D_i,D_c,D_o:double;
                    air_prandtl_no:double;
                    T_water_film:double;
                    W,H,Depth:double;
                    fin_thickness,wall_thickness,L_eff,Req,R_collar,M_2:double;
                    Af,Ai,Ac,Ao,At,Aci,Aw,Afr,Ati,Apm:double;
                    epslon:double;
                    K_entrance,K_exit:double;
                    fin_pitch,fin_spacing:double;
                    sigama:double;
                    row_number,circuit_number:byte;
                    fin_no:integer;
                    wavy_fin,Counter_Cross_Flow:boolean;
                    waffle_height,Xf:double;
                    tube_no_per_row:byte;
                    Pt,pl,beta,Dh,deviation:double;
                    System_pressure,nozzle_T:double;
                  end;

    enthalpy_Position= record
                         air,coolant,wall_outside,wall_inside,mean:double;
                       end;
    air_type= record
                 T_dry:double;
                 T_wet:double;
                 T_dew:double;
                 Ps,Pw,Pa,pws:double;
                 m_water_vapor,x_water_vapor:double;
                 relative_humidity:double;
                 Cpm:double;
                 enthalpy:double;
                 W,Ws:double;
                 density:double;
              end;

 var
   Data:reduce_data;
   wet_efficiency,Cpa:double;
   G_mass_Coeff,Le_by_Gmass,jd_mass:double;
   ss:string;
   inlet_air,outlet_air:air_type;
   WET_DATAFILE:boolean;
   temp,A_region1,A_region2:double;
   area1,R1:sector_type1;
   area2,R2:sector_type2;
   i_in,i_out,i_mean,T_in,T_out,T_mean,Humidity_mean:enthalpy_position;
   file_ext,Surface_Type,Surface_Treatment,Tube_Layout,Flow_Arrangement,
   Fin_material,Fin_Type,Tube_Type:String[80];
   function  power(x,y:double):double;
   function  log10(x:double):double;
   function  water_pressure(T:double):double; {bar}

implementation

 function power(x,y:double):double;
   begin
     power:=exp(y*ln(x));
   end;

 function open_type_correction_DP(V:double):double;
   begin
     open_type_correction_DP:=0.2491101-0.1075205*V+0.4286242*sqr(V);
   end;

 function air_hs(Ts:double):double;  {saturated enthalpy of air, valid from 0 deg.-55 deg.}
   var
     Ts2:double;
   begin
     Ts2:=sqr(Ts);
     air_hs:=(9473.3729+1632.4365*Ts+7.7212754*Ts2)/(1.0-0.0078046131*Ts-0.00018915577*Ts2+2.1591282E-6*Ts*Ts2);
     {9729.206+1417.136*Ts+61.50031*Ts2-1.212219*Ts2*Ts+0.03090319*sqr(Ts2); }
   end;

 function air_Dhs(Ts:double):double;  {derivative of saturated of air enthalpy}
   var
     Ts2:double;
   begin
     Ts2:=sqr(Ts);
{     air_Dhs:=1417.136+123.00062*Ts-3.636657*Ts2+0.12361276*Ts*Ts2; }
     air_Dhs:=1707.0735+45.154976*Ts+1.54787265*Ts2+0.008563686*Ts*Ts2+6.8638445E-4*sqr(Ts2);
   end;


 function air_Ws2(T:double):double; {T:dry bulb temp, deg. C, 1 atm, 0~50 deg. C}
   begin
     air_Ws2:=-0.0018965936+0.005492993*exp(T/17.963336);
   end;

 function air_Pr(T:double):double;
   var TK:double;
   begin
     TK:=T+273.15;
     air_Pr:=0.837-6.1E-4*TK+6.0E-7*sqr(TK);
   end;

 function air_Ws(Ts:double):double;  {saturated humidity ratio, valid from 0 deg.-55 deg., kgw/kga}
   var
     Ts2,Ts4:double;
   begin
     Ts2:=sqr(Ts);
     Ts4:=sqr(Ts2);
     air_Ws:=3.77252E-3+3.075632E-4*Ts+2.954685E-6*Ts2+5.94542E-7*Ts2*Ts-1.009741E-8*Ts4+1.572892E-10*Ts*Ts4;
   end;

 function air_hs_to_ts(hs:double):double; { valid from 0-55 deg., hs in J/kg dry air}
   var hs2:double;
   begin
{     hs:=hs/1000.0;
     air_hs_to_ts:=683.3858-1435.88*power(hs,0.1)+955.7192*power(hs,0.2)-195.52*power(hs,0.3); }
     hs2:=sqr(hs);
     air_hs_to_ts:=(-5.9296052+0.00031166778*hs+3.3175938E-8*hs2)/(1.0+5.8894455E-5*hs+5.335959E-10*hs2-3.2510287E-16*hs*hs2);
   end;

 function Mean_ha(ha_in,ha_out,hr_in,hr_out:double):double;
   begin
     Mean_ha:=ha_in+(ha_in-ha_out)/ln((ha_in-hr_out)/(ha_out-hr_in))
                   -(ha_in-ha_out)*(ha_in-hr_out)/((ha_in-hr_out)-(ha_out-hr_in));
   end;

 function Mean_hr(ha_in,ha_out,hr_in,hr_out:double):double;
   begin
     Mean_hr:=hr_out+(hr_out-hr_in)/ln((ha_in-hr_out)/(ha_out-hr_in))
                    -(hr_out-hr_in)*(ha_in-hr_out)/((ha_in-hr_out)-(ha_out-hr_in));
   end;

 function log10(x:double):double;
   begin
     log10:=ln(x)/2.30258509;
   end;

 function water_pressure(T:double):double; {in Pa}
   begin
     water_pressure:=exp(14.43509-5333.3/(273.15+T))*1.0E5;
   end;

 function water_vapor_enthalpy(Ts:double):double; {C J/kg,0-55 C only}
   begin
     water_vapor_enthalpy:=2500730.0+1848.961*Ts-0.7648352*sqr(Ts);
   end;

 function water_pressure2(T:double):double; {in Pa}
   var TK,TK2:double;
   begin
     TK:=T+273.15;
     TK2:=sqr(TK);
     water_pressure2:=exp(-5800.2206/TK-5.516256-0.048640239*TK+4.1764768E-5*TK2
                         -1.4452093E-8*TK*TK2+6.54597673*ln(TK));
   end;

 function arcsin(x:double):double;
   begin
     arcsin:=arctan(x/sqrt(1.0-sqr(x)));
   end;

 function arccos(x:double):double;
   begin
     arccos:=arctan(sqrt(1.0-sqr(x))/x);
   end;

  function I0(X:double):double;
    const A1:array[1..14] of double=(1.0,1.0,0.25,0.0277778,0.0017631,0.0000694,0.0000019,
                                     3.936759889E-8,6.151187327E-10,7.594058428E-12,7.594058428E-14,
                                     6.276081345E-16,4.358389823E-18,2.57892889E-20);
    var
       HX,Y,HX2,sum:double;
       i:integer;
    begin
       HX:=X/2.0;
       Y:=sqr(HX);
       sum:=A1[14];
       for i:=13 downto 1 do sum:=sum*Y+A1[i];
       I0:=sum;
    end;

  function I1(X:double):double;
    const A1:array[1..14] of double=(1.0,0.5,0.083333,0.006944,0.0003472,0.0000116,0.0000003,
                                     4.907561178E-9,6.816057191E-11,7.594058428E-13,
                                     6.90368948E-15,5.230067788E-17,3.352607556E-19,
                                     1.842092064E-21);
    var HX,sum,Y:double;
        i:integer;
    begin
	HX:=X/2.0;
        sum:=A1[14];
        Y:=sqr(HX);
        for i:=13 downto 1 do sum:=sum*Y+A1[i];
        sum:=sum*HX;
        I1:=sum;
    end;

  function K0(X:double):double;
    var TINOT,GAMMA,HX,DH,Y:double;
    begin
      TINOT:=I0(X);
      HX:=X/2.0;
      DH:=1.0/HX;
      IF X>2.0 THEN
	 Y:=power(X,-0.5)*EXP(-X)*(1.255331414-0.07832358*DH
            +0.02189568*sqr(DH)-0.01062446*power(DH,3.0)
            +0.00587872*power(DH,4.0)-0.00251540*power(DH,5.0)
            +0.00053280*power(DH,6.0))
	ELSE
	 Y:=-Ln(HX)*TINOT-0.57721566+0.4228420*sqr(HX)
     	    +0.23069756*power(HX,4.0)+0.03488590*power(HX,6.0)+0.00262698*power(HX,8.0)
            +0.00010750*power(HX,10.0)+0.00000740*power(HX,12.0);
      K0:=Y;
    end;

   function K1(X:double):double;
     var ALPA,HX,TIONE,DH,Y:double;
     begin
       HX:=X/2.0;
       DH:=1.0/HX;
       TIONE:=I1(X);
       IF X>2.0 THEN
	  Y:=power(X,-0.5)*EXP(-X)*(1.25331414+0.23498619*DH
             -0.03655620*sqr(DH)+0.01504268*power(DH,3.0)-0.00780353*power(DH,4.0)
             +0.00325614*power(DH,5.0)-0.00068245*power(DH,6.0))
       ELSE
 	  Y:=Ln(HX)*TIONE+((1.0/X)*(1.0+0.15443144*sqr(HX)
     	     -0.67278579*power(HX,4.0)-0.18156897*power(HX,6.0)-0.01919402*power(HX,8.0)
     	     -0.00110404*power(HX,10.0)-0.00004686*power(HX,12.0)));
       K1:=Y;
     end;


   function Corection_F_Row(P,R,R1m:double;ROW:INTEGER):double;
     var AIK,temp:array[1..4,1..4] of double;
         i,k:integer;
         sum:double;
     begin
        case Row of
           1: begin
                AIK[1,1]:=-0.462; AIK[2,1]:=-0.0313; AIK[3,1]:=-0.174;  AIK[4,1]:=-0.042;
                AIK[1,2]:= 5.08;  AIK[2,2]:= 0.529 ; AIK[3,2]:= 1.32 ;  AIK[4,2]:= 0.347;
                AIK[1,3]:=-15.7 ; AIK[2,3]:=-2.37  ; AIK[3,3]:=-2.93 ;  AIK[4,3]:=-0.853;
                AIK[1,4]:=17.2  ; AIK[2,4]:=3.18   ; AIK[3,4]:=1.99  ;  AIK[4,4]:= 0.649;
              end;
           2: begin
                AIK[1,1]:=-0.334; AIK[2,1]:=-0.154 ; AIK[3,1]:=-0.0865; AIK[4,1]:= 0.0553;
                AIK[1,2]:= 3.3 ;  AIK[2,2]:= 1.28  ; AIK[3,2]:= 0.546;  AIK[4,2]:=-0.405;
                AIK[1,3]:=-8.7  ; AIK[2,3]:=-3.35  ; AIK[3,3]:=-0.929;  AIK[4,3]:= 0.953;
                AIK[1,4]:= 8.7  ; AIK[2,4]:=2.83   ; AIK[3,4]:=0.471 ;  AIK[4,4]:=-0.717;
              end;
        end;
        sum:=0;
        for i:=1 to 4 do begin
            for k:=1 to 4 do begin
                temp[i,k]:=power((1-R1m),k)*sin(2*i*arctan(R));
                sum:=AIK[i,k]*temp[i,k];
            end;
        end;
        Corection_F_Row:=1.0-sum;
     end;

   function Correction_F(P,R:double):double;
     var a,b,c,x1,x2,x3,y1,y2,y3,tmp1,tmp2:double;
         function FR02(P:double):double;
           begin
             FR02:=0.998312+0.02767832*P-0.1058272*sqr(P);
           end;
         function FR04(P:double):double;
           begin
             FR04:=0.9997879+0.009160839*P-0.1319347*sqr(P);
           end;
         function FR06(P:double):double;
           begin
             FR06:=0.9977333+0.05255944*P-0.3039627*sqr(P);
           end;
         function FR08(P:double):double;
           begin
             FR08:=0.9973455+0.07386014*P-0.420979*sqr(P);
           end;
         function FR10(P:double):double;
           begin
             FR10:=0.9951515+0.07216783*P-0.5081585*sqr(P);
           end;
         function FR15(P:double):double;
           begin
             FR15:=0.9786667+0.3154545*P-1.30303*sqr(P);
           end;
         function FR20(P:double):double;
           begin
             FR20:=0.98+0.3864286*P-1.928571*sqr(P);
           end;
         function FR30(P:double):double;
           begin
             FR30:=0.962+0.8242857*P-4.428571*sqr(P);
           end;
         function FR40(P:double):double;
           begin
             FR40:=0.98675+0.459*P-4.1*sqr(P);
           end;
     begin
       if R<0.6 then begin
          x1:=0.2;
          x2:=0.4;
          x3:=0.6;
          y1:=FR02(P);
          y2:=FR04(P);
          y3:=FR06(P);
       end
       else if R<0.8 then begin
               x1:=0.4;
               x2:=0.6;
               x3:=0.8;
               y1:=FR04(P);
               y2:=FR06(P);
               y3:=FR08(P);
            end
            else if R<1.0 then begin
                    x1:=0.6;
                    x2:=0.8;
                    x3:=1.0;
                    y1:=FR06(P);
                    y2:=FR08(P);
                    y3:=FR10(P);
                 end
                 else if R<1.5 then begin
                         x1:=0.8;
                         x2:=1.0;
                         x3:=1.5;
                         y1:=FR08(P);
                         y2:=FR10(P);
                         y3:=FR15(P);
                      end
                      else if R<2.0 then begin
                              x1:=1.0;
                              x2:=1.5;
                              x3:=2.0;
                              y1:=FR10(P);
                              y2:=FR15(P);
                              y3:=FR20(P);
                           end
                           else if R<3.0 then begin
                                   x1:=1.5;
                                   x2:=2.0;
                                   x3:=3.0;
                                   y1:=FR15(P);
                                   y2:=FR20(P);
                                   y3:=FR30(P);
                                end
                                else begin
                                       x1:=2.0;
                                       x2:=3.0;
                                       x3:=4.0;
                                       y1:=FR20(P);
                                       y2:=FR30(P);
                                       y3:=FR40(P);
                                     end;
       tmp1:=(y1-y2)/(x1-x2);
       tmp2:=(y2-y3)/(x2-x3);
       c:=(tmp1-tmp2)/(x1-x3);
       b:=tmp2-c*(x2+x3);
       a:=y1-b*x1-c*sqr(x1);
       Correction_F:=a+b*R+c*sqr(R);
     end;

  function air_viscosity(Td:double):double; {250-400 K only}
   var
     TK:double;
   begin
     TK:=Td+273.15;
     air_viscosity:=(0.09213+0.0069326*TK-3.62E-6*sqr(TK))*1.0E-5;
   end;

 function air_k(Td:double):double;
   begin
     air_k:=(-1.344929-0.1085393*Td-5.6616E-5*sqr(Td))*1.0E-3;
   end;

 function air_density(P,T:double):double; {T in degree °C}
   begin
     air_density:=P/R_air/(T+273.15);
   end;

 function air_Sc(Td:double):double;
   var Dab,v:double;
   begin
     Dab:=1.171968E-9*power(Td+273.15,1.75);
     v:=air_viscosity(Td)/air_density(data.System_Pressure,Td);
     air_Sc:=v/Dab;
   end;

 function water_density(T:double):double; {T in C}
   begin
     water_density:=997.9352+4.489218*T-3.852803*power(T,1.05);
   end;

  function water_cp(T:double):double; { T in C}
    begin
      water_Cp:=4240.873-229.3324*T+350.5767*power(T,1.05)-134.0144*power(T,1.1);
    end;

 function get_conduction_R(thickness,k,A:double):double; {Conduction resistance}
    begin
      get_conduction_R:=thickness/k/A;
    end;

  function Reynolds_no(G,uf,Di:double):double;
   begin
     Reynolds_no:=G*Di/uf;
   end;

 function water_Prw(T:double):double;
   begin
     water_Prw:=13.2164+1.003894534*T-1.77058692*power(T,0.9);
   end;

 function water_viscosity(T:double):double;
   begin
     water_viscosity:=(1791.84511+123.2452*T-219.03304*power(T,0.9))*1.0E-6;
   end;

 function water_k(T:double):double;
   begin
     water_k:=(569.2622+1.827975*T-0.007124*sqr(T))/1000.0;
   end;

 function water_friction(Re:double):double;
   var A,B,m:double;
   begin
     water_friction:=1.0/sqr(1.58*ln(Re)-3.28);
   end;

 function Nusselt(Re,Pr:double):double;
   var f2:double;
   begin
     f2:=water_friction(Re)/2.0;
     Nusselt:=f2*(Re-1000.0)*Pr/(1.0+12.7*sqrt(f2)*(power(Pr,0.666667)-1.0));
   end;

 function get_humidity_ratio(relative_humidity,Psat,P:double):double;
   begin
     get_humidity_ratio:=0.62198*relative_humidity*Psat/(P-relative_humidity*Psat);
   end;

 function get_enthalpy(humidity_ratio,Td:double):double;
   begin
     get_enthalpy:=1000.0*Td+humidity_ratio*(2501000.0+1806*Td);
   end;

 function get_humidity(Td,enthalpy:double):double;
   begin
     get_humidity:=(enthalpy-1000.0*Td)/(2501000.0+1806*Td);
   end;

 function get_Tdew(Td,pw:double):double;
 var alpha,alpha2:double;
 begin
   pw:=pw/1000.0;  {unit in kPa}
   alpha:=ln(pw);
   alpha2:=sqr(alpha);
   if Td < 0.0 then
      get_tdew:=6.09+12.608*alpha+0.4959*alpha2
   else
      get_Tdew:=6.54+14.526*alpha+0.7389*alpha2+0.09486*alpha*alpha2+0.4569*power(pw,0.1984);
 end;

 function tanh(x:double):double;
   begin
     tanh:=(exp(x)-exp(-x))/(exp(x)+exp(-x));
   end;

 procedure Initialize(var data:reduce_data);
   begin
     with data do begin
          Enhance_ratio:=1.0;
          T_water_in:=59.9;
          T_water_out:=53.73;
          m_water:=4.82/60.0; {kg/s}
          T_DB_in:=30.237;
          T_DB_out:=57.811;
          T_WB_in:=26.024;
          T_WB_out:=17.005;
          Qt:=2666.6;
          W:=0.6; {m}
          H:=0.279;  {m}
          Depth:=0.01905;
          k_fin:=204.0;
          k_wall:=386.0;
          D_c:=0.01023;
          fin_no:=314;
          fin_thickness:=0.00013;
          wall_thickness:=0.00035*TUBE_EXPANSION_RATIO;
          D_o:=D_c-2.0*fin_thickness;
          D_i:=D_o-2.0*wall_thickness;
          Pt:=0.0254;
          Pl:=0.0195;
          Req:=sqrt(Pt*Pl/pi);
          System_Pressure:=101325.0;
          R_collar:=D_c/2.0;
          L_eff:=(Req-R_collar)*(1.0+0.35*ln(Req/R_collar));
          tube_no_per_row:=14;
          row_number:=1;
          fin_pitch:=0.002449;
          Af:=2.919;
          Ao:=3.1442;
          At:=Ao-Af;
          Afr:=W*H;
          Ai:=pi*D_i*row_number*tube_no_per_row*W;
          Aci:=pi*sqr(D_i)/4.0;
     end;
   end;

function get_twet(initial_twet,W,td,System_P:double):double;
  var
   i,j:integer;
   x,x1,x2,x3,temp,gx,dx:double;
   function wet2(twet:double):double;
     var pwets,w_wet:double;
     begin
       Pwets:=water_pressure(Twet);
       W_wet:=get_humidity_ratio(1.0,Pwets,System_P);
       wet2:=twet-(2501.0*(W-W_wet)+Td*(1.0+1.805*W))/(1.0-2.381*W_wet+4.186*W);
     end;
   begin
     x:=initial_twet;
     for i:=1 to 20 do begin
         gx:=(wet2(x+0.1)-wet2(x-0.1))/0.2;
         temp:=wet2(x)/gx;
         x:=x-temp;
         dx:=abs(x-initial_twet);
         if dx<0.00001 then break;
     end;
     get_twet:=x;
   end;

function get_Ws(Td,T_wet,system_p:double):double;
  var
    Pwet,wwet,w,pws,pw,ws:double;
  begin
    Pwet:=water_pressure(T_wet);
    Wwet:=get_humidity_ratio(1.0,Pwet,System_P);
    W:=((2501-2.381*T_wet)*Wwet-(Td-T_wet))/(2501+1.805*Td-4.186*T_wet);
    pws:=water_pressure(Td);
    pw:=System_p*W/(0.62198+W);
    Ws:=get_humidity_ratio(1.0,Pws,System_P);
  end;

procedure Situation1(var airdata:air_type;Td,Twet,System_P:double); {given Tdry, Twet}
 var i:integer;
     Pwets,Pwet,Wwet,u:double;
 begin
   with airdata do begin
        T_dry:=Td;
        T_wet:=Twet;
        Pwet:=water_pressure(Twet);
        Wwet:=get_humidity_ratio(1.0,Pwet,System_P);
        W:=((2501-2.381*T_wet)*Wwet-(Td-Twet))/(2501+1.805*Td-4.186*Twet);
        pws:=water_pressure(Td);
        pw:=System_p*W/(0.62198+W);
        Ws:=get_humidity_ratio(1.0,Pws,System_P);
        density:=System_p/R_air/(273.15+Td)/(1.0+1.6078*W);
        enthalpy:=get_enthalpy(W,Td);
        u:=W/Ws;
        relative_humidity:=u/(1.0-(1.0-u)*pws/System_p);
        T_dew:=get_Tdew(Td,pw);
        Cpm:=(1.0+W)*Cpair;
        x_water_vapor:=pw/System_P;
        m_water_vapor:=x_water_vapor*18/(x_water_vapor*18+(1.0-x_water_vapor)*28.9645);
   end;
 end;

Procedure Situation2(var airdata:air_type;Td,Tdew,System_P:double); {given Tdry, Tdew }
 var i:integer;
     Twet,Pwets,W_wet,u:double;
 begin
   with airdata do begin
        T_dry:=Td;
        T_dew:=Tdew;
        pw:=water_pressure(Tdew);
        pws:=water_pressure(Td);
        W:=get_humidity_ratio(1.0,Pw,System_P);
        Ws:=get_humidity_ratio(1.0,Pws,System_P);
        density:=System_p/R_air/(273.15+Td)/(1.0+1.6078*W);
        enthalpy:=get_enthalpy(W,Td);
        u:=W/Ws;
        relative_humidity:=u/(1.0-(1.0-u)*pws/System_p);
        Twet:=Td;
        T_wet:=get_twet(Twet,W,td,System_P);
        Cpm:=(1.0+W)*Cpair;
        x_water_vapor:=pw/System_P;
        m_water_vapor:=x_water_vapor*18/(x_water_vapor*18+(1.0-x_water_vapor)*28.9645);
   end;
 end;

Procedure Situation3(var airdata:air_type;Td,RH,System_P:double); { given Tdry, RH }
 var i:integer;
     Twet,Pwets,W_wet:double;
 begin
   with airdata do begin
        T_dry:=Td;
        relative_humidity:=RH;
        pws:=water_pressure(Td);
        pw:=pws*RH;
        W:=get_humidity_ratio(RH,Pws,System_P);
        Ws:=get_humidity_ratio(1.0,Pws,System_P);
        density:=System_p/R_air/(273.15+Td)/(1.0+1.6078*W);
        enthalpy:=get_enthalpy(W,Td);
        T_dew:=get_Tdew(Td,pw);
        Twet:=Td;
        T_wet:=get_twet(Twet,W,td,System_P);
        Cpm:=(1.0+W)*Cpair;
        x_water_vapor:=pw/System_P;
        m_water_vapor:=x_water_vapor*18/(x_water_vapor*18+(1.0-x_water_vapor)*28.9645);
   end;
 end;

 procedure get_mass_coeff(data:reduce_data;Le,br,effectiveness:double; var T_dry:double);
  var di:double;
      i,j:integer;
      i_sw,T_sw,W,W_sw,i_air,ratio,ir,Tdry,i_gt,Tr,dT:double;
  begin
      i:=40;
      di:=-(inlet_air.enthalpy-outlet_air.enthalpy)/i;
      Tdry:=data.T_DB_in;
      W:=inlet_air.W;
      i_air:=inlet_air.enthalpy;
      ir:=air_hs(data.T_water_out); {Counter-Flow}
      Tr:=data.T_water_out;         {Counter-Flow}
      ratio:=1.0-br*data.Uo_w*data.Ao/data.hi/data.Ati;
      for j:=1 to i do begin
          i_gt:=water_vapor_enthalpy(Tdry);
          i_sw:=i_air-effectiveness*ratio*(i_air-ir);
          T_sw:=air_hs_to_ts(i_sw);
          W_sw:=air_Ws(T_sw);
          W:=W+di/(Le*(i_air-i_sw)/(W-W_sw)+(i_gt-2500900.0*Le));
          i_air:=i_air+di;
          Tdry:=(i_air-2501300.0*W)/(1006.0+1860.0*W);
          dT:=data.ma*di/data.m_water/water_Cp(Tr);
          Tr:=Tr+dT;
          ir:=air_hs(Tr);
      end;
      T_dry:=Tdry;
  end;

procedure Process_Line(data:reduce_data;Le,br,effectiveness:double; var T_dry:double);
  var di:double;
      i,j:integer;
      i_sw,T_sw,W,W_sw,i_air,ratio,ir,Tdry,i_gt,Tr,dT:double;
  begin
      i:=50;
      di:=-(inlet_air.enthalpy-outlet_air.enthalpy)/i;
      Tdry:=data.T_DB_in;
      W:=inlet_air.W;
      i_air:=inlet_air.enthalpy;
      ir:=air_hs(data.T_water_out); {Counter-Flow}
      Tr:=data.T_water_out;         {Counter-Flow}
      ratio:=1.0-br*data.Uo_w*data.Ao/data.hi/data.Ati;
      for j:=1 to i do begin
          i_gt:=water_vapor_enthalpy(Tdry);
          i_sw:=i_air-effectiveness*ratio*(i_air-ir);
          T_sw:=air_hs_to_ts(i_sw);
          W_sw:=air_Ws(T_sw);
          W:=W+di/(Le*(i_air-i_sw)/(W-W_sw)+(i_gt-2500900.0*Le));
          i_air:=i_air+di;
          Tdry:=(i_air-2501300.0*W)/(1006.0+1860.0*W);
          dT:=data.ma*di/data.m_water/water_Cp(Tr);
          Tr:=Tr+dT;
          ir:=air_hs(Tr);
      end;
      T_dry:=Tdry;
  end;

procedure get_Le(data:reduce_data;var Le:double;br,effectiveness:double);
  var i,j:integer;
      Tdry:double;
  begin
      Le:=0.35;
      for i:=1 to 250 do begin
          Process_Line(data,Le,br,effectiveness,Tdry);
          if abs(data.T_DB_out-Tdry)<0.2 then exit;
          Le:=Le+0.005;
      end;
  end;

procedure Get_Sectors(var A_region1,A_region2:double;
                      var Area1,R1:Sector_type1;
                      var Area2,R2:Sector_Type2;
                      rc:double);
            { rc:radius of collar diameter }
   var
    i,j:integer;
    ZZ,YY,MM,NN,WW,UU,
    theta_1,theta1_now,theta2_now,theta_i,theta_2,DZ,DZ2,theta_sum,tan_thetai:double;
   begin
     ZZ:=data.pl/3.0;
     YY:=data.pl-ZZ;
     MM:=data.Pt/2.0;
     theta_1:=arctan(ZZ/MM);
     NN:=sqrt(sqr(MM)+sqr(ZZ));
     DZ:=ZZ/sectors_N1;
     DZ2:=DZ/2.0;
     theta_sum:=0.0;
     A_region1:=0.0;
     for i:=1 to sectors_N1 do begin
         theta1_now:=arctan(i*DZ/MM)-theta_sum;
         theta_sum:=theta_sum+theta1_now;
         R1[i]:=sqrt(sqr(MM)+sqr((2*i-1)*DZ2));
         area1[i]:=theta1_now/2.0*(sqr(R1[i])-sqr(rc));
         A_region1:=A_region1+area1[i];
     end;
     theta_2:=pi/2.0-theta_1;
     theta2_now:=theta_2/sectors_N2;
     WW:=MM/(ZZ-YY);
     UU:=WW*YY;
     theta_i:=-theta2_now/2.0;
     A_region2:=0.0;
     for i:=1 to sectors_N2 do begin
         theta_i:=(2*i-1)*theta2_now/2.0;
         tan_thetai:=sin(theta_i)/cos(theta_i);
         R2[i]:=(UU/(WW-tan_thetai))*sqrt(1.0+sqr(tan_thetai));
         Area2[i]:=theta2_now/2.0*(sqr(R2[i])-sqr(rc));
         A_region2:=A_region2+area2[i];
      end;
   end;

 procedure REDUCTION_WET(var data:reduce_data);
   var
     i,j:integer;
     Methods:byte;
     Twater_mean,ifg,m22:double;
     Re,Pr,k_w,u_water,G_water,d_water,Q_reduction:double;
     R_i,R_o,R_c,R_equ,Rii,Rio:double;
     Nu,hs_in,hs_out,Ws_in,Ws_out:double;
     R,hs_eff2,ho,Sc:double;
     air_q,water_q:double;
     pwi,pwo,Wwi,Wwo,iwi,iwo:double;
     LMHD,LMTD,UA1,TEMP_TD:double;
     LMWD,DW1,DW2:double;
     Dh1,Dh2,DT1,DT2:double;
     PP,RR,P_ROW,Q_ROw,R_Row,R1M,F_Correction:double;
     how,wet_eff:double;
     T_wall_mean:double;
     B,B1,B2,B4,B5,m1,m2,m3:double; { tempory variables }
     br,bp,bwp,bwm,Z:double;
     m_1_out,m_1_s,m_1_in,x_1_s,LNZ:double;
     Lewis:double;
     T_wall_in_mean,i_wall_in_mean,T_wall_out_mean,i_wall_out_mean:double;
     i_water_mean,T_water_mean:double;
     procedure solve_by_Threlkeld2(option:byte;Uo_w,req,ro,xp,kp,kf,Ap_m,Ap_i,Ap_o,br,bp,bwp:double;
                                   var bwm:double;Af,Ao,delta_fin,hi,
                                   i_wall_inside,i_wall_outside,i_refrigerant,i_air:double;
                                   var reduced_data:h_eff_type);
            {
            option: fin efficiency used:
               0:Equivalent_radius
               1:Sector_method
            Uo_w:overall heat transfer coefficient
            req:equivalent radius
            ro:outer tube radius (=rc)
            xp:wall thickness
            kp:thermal conductivity of wall
            kf:thermal conductivity of fin
            Ap_m:mean value of wall surface
            Ap_i:Inside wall surface
            Ap_o:Outside wall surface
            delta_fin: fin thickness
            hi:inside heat transfer coefficient
            Af:fin surface area
            Ao:total surface area
            br,bp,bwp,bwm:same nomenclature in Myer's thesis.
            i_air:mean value of air enthalpy
            i_wall_inside:mean value of saturated air enthalpy in inner wall
            i_wall_outside:mean value of saturated air enthalpy in outer wall
            i_refrigerant:mean value of saturated air enthalpy evaluated at mean temperature of refrigerant
            }
       var
         i,j:integer;
         fx,gx,Z,i_s_w_m,Tw_m,bwm_bwp,tolerance:double;
         function fin_efficiency(how:double):double;
           var
             M,Mri,Mro:double;
           begin
             M:=sqrt(2*how/kf/delta_fin);
             Mri:=M*ro;
             Mro:=M*req;
             wet_efficiency:=2.0*ro/M/(sqr(req)-sqr(ro))*
                            ((K1(Mri)*I1(Mro)-K1(Mro)*I1(Mri))/(K1(Mro)*I0(Mri)+K0(Mri)*I1(Mro)));
             wet_eff:=wet_efficiency;
             fin_efficiency:=wet_efficiency;
           end;

         function Sector_fin_efficiency(how:double):double;
            var
              i,j:integer;
              M,Mri,Mro,Area,temp,sum1,sum2,Q0,Q1,Q2,Q3,Q4,Q5:double;
            begin
              M:=sqrt(2*how/kf/delta_fin);
              Mri:=M*ro;
              Q0:=2*ro/M;
              Q1:=K1(Mri);
              Q2:=I1(Mri);
              Q3:=I0(Mri);
              Q4:=K0(Mri);
              Q5:=sqr(ro);
              Area:=A_region1+A_region2;
              sum1:=0.0;
              sum2:=0.0;
              for i:=1 to sectors_N1 do begin
                  Mro:=M*R1[i];
                  sum1:=sum1+Area1[i]*((Q1*I1(Mro)-Q2*K1(Mro))/(Q3*K1(Mro)+Q4*I1(Mro))/(sqr(R1[i])-Q5));
              end;
              for i:=1 to sectors_N2 do begin
                  Mro:=M*R2[i];
                  sum2:=sum2+Area2[i]*((Q1*I1(Mro)-Q2*K1(Mro))/(Q3*K1(Mro)+Q4*I1(Mro))/(sqr(R2[i])-Q5));
              end;
              wet_efficiency:=Q0/Area*(sum1+sum2);
              wet_eff:=wet_efficiency;
              sector_fin_efficiency:=wet_efficiency;
            end;

         function Y(how,Z:double):double;
           var efficiency:double;
           begin
             if option=equivalent_circular then efficiency:=fin_efficiency(how)
             else efficiency:=Sector_fin_efficiency(how);
             Y:=Z-how*(Ap_o/bwp/Ao+1.0/bwm*Af/Ao*efficiency);
           end;

       begin
         Z:=1.0/(1.0/Uo_w-br*Ao/hi/Ap_i-bp*xp*Ao/kp/Ap_m);
         for j:=1 to 5 do begin
             for i:=1 to 10 do begin
                 gx:=(Y(how+0.01,Z)-Y(how-0.01,Z))/0.02;
                 how:=how-Y(how,Z)/gx;
             end;
             i_s_w_m:=i_air-wet_eff*(1.0-Uo_w*Ao*(br/hi/Ap_i+xp*bp/kp/Ap_m))*(i_air-i_refrigerant);
             Tw_m:=air_hs_to_Ts(i_s_w_m);
             bwm:=air_Dhs(Tw_m);
         end;
         reduced_data.T_water_film_mean:=Tw_m;
         reduced_data.h_co:=how*Cpa/bwm;
         reduced_data.efficiency:=wet_eff;
         bwm_bwp:=bwm/bwp;
         reduced_data.effectiveness:=(bwm_bwp-Af/Ao*(bwm_bwp-wet_eff));
         reduced_data.eta_h:=reduced_data.effectiveness*reduced_data.h_co;
         Lewis:=0.67;
         get_Le(data,Lewis,br,wet_eff);
         reduced_data.Le:=Lewis;
         reduced_data.h_mass:=reduced_data.h_co/Cpa/Lewis;
         Sc:=air_Sc(data.T_DB_in);
         data.air_prandtl_no:=air_pr(data.T_DB_in);
         reduced_data.j_mass:=reduced_data.h_mass/data.G_air*power(Sc,0.66666667);
         reduced_data.j:=reduced_data.h_co/data.G_air/Cpa*power(data.air_prandtl_no,0.6666667);
       end;

   begin
     with data do begin
          Twater_mean:=(T_water_in+T_water_out)/2.0;
          Twater_mean:=(T_water_in+T_water_out)/2.0;
          hs_in:=air_hs(T_water_in);
          hs_out:=air_hs(T_water_out);
          DH1:=inlet_air.enthalpy-hs_out;
          DH2:=outlet_air.enthalpy-hs_in;
          LMHD:=(Dh1-Dh2)/ln(Dh1/Dh2);
          PP:=(hs_out-hs_in)/(inlet_air.enthalpy-hs_in);
          RR:=(inlet_air.enthalpy-outlet_air.enthalpy)/(hs_out-hs_in);
          F_Correction:=Correction_F(PP,RR);
          P_value:=PP;
          R_value:=RR;
          P_ROW:=PP;
          Q_ROW:=(inlet_air.enthalpy-outlet_air.enthalpy)/(inlet_air.enthalpy-hs_in);
          R_ROW:=P_ROW/Q_ROW;
          R1M:=(P_ROW-Q_ROW)/ln((1-Q_ROW)/(1-P_ROW));
          TEMP_TD:=LMHD/(inlet_air.enthalpy-hs_in);
         { F_value:=Corection_F_Row(P_ROW,R_ROW,R1M,ROW_Number);
          if F_value > 1.0 then F_value:=1.0; }


          F_value:=F_Correction;

          Q_reduction:=Q_water;
          Uo_w:=Q_reduction/Ao/LMHD/F_Correction;
          u_water:=water_Viscosity(Twater_mean);
          G_water:=m_water/Aci/circuit_number;
          Re:=G_water*D_i/u_water;
          Pr:=water_Prw(Twater_mean);
          k_w:=water_k(Twater_mean);
          Nu:=Nusselt(Re,Pr);
          hi:=Enhance_ratio*Nu*k_w/D_i;
     { begin reduction }
          i_in.air:=inlet_air.enthalpy;
          i_in.coolant:=hs_in;
          i_out.air:=outlet_air.enthalpy;
          i_out.coolant:=hs_out;
          i_mean.coolant:=Mean_hr(i_in.air,i_out.air,i_in.coolant,i_out.coolant);
          i_mean.air:=Mean_ha(i_in.air,i_out.air,i_in.coolant,i_out.coolant);
          T_mean.air:=air_hs_to_ts(i_mean.air);
          Humidity_mean.air:=get_humidity(T_mean.air,i_mean.air);
          T_mean.coolant:=air_hs_to_ts(i_mean.coolant);
          T_wall_in_mean:=T_mean.coolant+Q_reduction/Ati/hi;
          i_wall_in_mean:=air_hs(T_wall_in_mean);
          br:=(i_wall_in_mean-i_mean.coolant)/(T_wall_in_mean-T_mean.coolant);
          T_wall_out_mean:=T_wall_in_mean+wall_thickness*Ao*Uo_w*LMHD/k_wall/Apm;
          i_wall_out_mean:=air_hs(T_wall_out_mean);
          bp:=(i_wall_out_mean-i_wall_in_mean)/(T_wall_out_mean-T_wall_in_mean);
          Uo_w:=Q_reduction/Ao/(i_mean.air-i_mean.coolant)/F_Correction;
          bwp:=air_Dhs(T_wall_out_mean);
          bwm:=bwp; {initial guess}
          how:=Uo_w*bwm;
          R_i:=D_i/2.0;
          R_o:=D_o/2.0;
          R_c:=D_c/2.0;
          solve_by_Threlkeld2(Equivalent_Circular,Uo_w,req,R_c,wall_thickness,k_wall,k_fin,Apm,Ati,At,br,bp,bwp,
                              bwm,Af,Ao,fin_thickness,hi,
                              i_wall_in_mean,i_wall_out_mean,i_mean.coolant,i_mean.air,data.Equ_Circle);

{* Special reduction of mass transfer Coeff}
          x_1_s:=water_pressure(data.Equ_Circle.T_water_film_mean)/system_pressure;
          m_1_s:=x_1_s*18/(x_1_s*18+(1.0-x_1_s)*28.9645);
          LNZ:=(m_1_s-outlet_air.m_water_vapor)/(m_1_s-inlet_air.m_water_vapor);
          if LNZ > 0 then begin
             LNZ:=ln(LNZ);
             G_mass_Coeff:=-lnZ*ma/Ao;
             Le_by_Gmass:=data.Equ_Circle.h_co/Cpa/G_Mass_Coeff;
             jd_mass:=G_mass_Coeff/data.G_air*power(Sc,0.66666667);
             end
          else begin
             writeln('Negative Mass Transfer Using Special mass transfer reduction');
             LNZ:=0;
             G_mass_Coeff:=0;
             Le_by_Gmass:=0;
             jd_mass:=0;
          end;

{* ends}
          Ws_in:=air_Ws(data.Equ_Circle.T_water_film_mean);
          Ws_out:=air_ws(T_wall_out_mean);
          LMWD:=Humidity_mean.air-Ws_in;
          Mass_Transfer:=ma*(inlet_air.W-outlet_air.W);
          Gamma:=Mass_Transfer/Row_Number/W;   {Average mass flowrate in a tube}
          Mass_Transfer_coeff:=Mass_Transfer/LMWD/data.Equ_Circle.effectiveness/Ao/F_Correction;
          Lewis_analogy:=data.Equ_Circle.h_co/Cpa/Mass_Transfer_Coeff;
          solve_by_Threlkeld2(Sector_method,Uo_w,req,R_c,wall_thickness,k_wall,k_fin,Apm,Ati,At,br,bp,bwp,
                              bwm,Af,Ao,fin_thickness,hi,
                              i_wall_in_mean,i_wall_out_mean,i_mean.coolant,i_mean.air,data.Sector);
          Condensate_Reynolds_number:=2*Gamma/water_viscosity(data.sector.T_water_film_mean);
          writeln(bwm:12:5,bwp:12:5,br:12:5,bp:12:5,Lewis_analogy:6:3,LMWD:9:6,Mass_Transfer:9:6);
     end;
   end;

procedure openfile2(filename:string);
   const
     total_parameters=21;
     tmpfile='WET_DATA.TMP';
     Judgement:array[1..total_parameters] of string=('VOLUME=','FINS=','FT=','PT=','PL=',
                                                     'ROW_NO=','CIRCUIT_NO=','TUBE_PER_ROW=',
                                                     'DC=','TT=','FLOW_ARRANGEMENT=','ENHANCE_RATIO=',
                                                     'SURFACE_TREATMENT=','SURFACE_TYPE=','FIN_TYPE=','FIN_MATERIAL=',
                                                     'WAFFLE_HEIGHT=','XF=','COUNTER_CROSS_FLOW',
                                                     'TUBE_TYPE','TUBE_LAYOUT=');
   var
     DRY_Data:boolean;
     f1,f2,f:text;
     i,check_data,j:integer;
     CONTROL:byte;
     fn:string;
     s1:string[8];
     s2:string[116];
     s3,s4,s5,outfile:string;
     Term1,Term2,term3,density_in,density_out,density_mean:double;
     ufr,DB_in,DB_out,WB_in,WB_out,DP,RH_in,RH_out,Dev,DewTi,DewTo,
     W_in,W_out,Air_CMM,Tw_in,Tw_out,water_CMM,P_system,P_nozzle,
     Tw_mean,_d_water,cp_water,d_water,D_air_in,
     T_nozzle,DP_nozzle,V_nozzle,Nozzle_no,T_room,
     D_air,Dev1,Dev2:double;
     function get_string(start_s,end_s:string;var s:string):string;
       var i:integer;
       begin
         if start_s<>'' then begin
            i:=pos(start_s,s)+1;
            s:=copy(s,i,255);
         end;
         i:=pos(end_s,s)-1;
         if i=-1 then i:=255;
         get_string:=copy(s,1,i);
         s:=copy(s,i+2,255);
       end;
     function Check_Parameter(s:string):integer;
       var i:integer;
           s1:string;
       begin
         i:=pos('=====',s);  { check begin or end of data }
         if i <> 0 then begin
            Check_parameter:=P_BEGIN_END_DATA;
            exit;
         end;
         for i:=1 to total_parameters do begin
             s1:=uppercase(s);
             if pos(Judgement[i],s1) <> 0 then begin
                Check_parameter:=i;
                exit;
             end;
         end;
         Check_parameter:=P_NIL;
       end;

   begin
     dry_data:=false;
     fn:=uppercase(filename);
     for i:=length(fn) downto 1 do if fn[i]='.' then break;
     if i=1 then s3:=s3+'.' { File name without an extension}
     else s3:=copy(fn,1,i);
     file_ext:='CCW';
     s5:=s3+file_ext;
     outfile:=s5;
     assignfile(f2,tmpfile);
     rewrite(f2);
     assignfile(f1,outfile);
     rewrite(f1);
     assignfile(f,fn);
     reset(f);
     WRITELN(FN);
     with data do begin
          wavy_fin:=false;
          repeat
            readln(f,s3);
            check_data:=check_parameter(s3);
            case check_data of
                 P_NIL:          begin
                                 end;
                 P_VOLUME:       begin
                                   s1:=get_string('=','*',s3);
                                   val(s1,W,i);
                                   W:=W/1000.0;
                                   s1:=get_string('','*',s3);
                                   val(s1,H,i);
                                   H:=H/1000.0;
                                   s1:=get_string('',' ',s3);
                                   val(s1,Depth,i);
                                   Depth:=Depth/1000.0;
                                 end;
                 P_FINS:         begin
                                   s1:=get_string('=',' ',s3);
                                   fin_no:=strtoint(s1);
                                 end;
                 P_FT:           begin
                                   s1:=get_string('=',' ',s3);
                                   val(s1,fin_thickness,i);
                                   fin_thickness:=fin_thickness/1000.0;
                                 end;
                 P_PT:           begin
                                   s1:=get_string('=',' ',s3);
                                   val(s1,Pt,i);
                                   Pt:=Pt/1000.0;
                                 end;
                 P_PL:           begin
                                   s1:=get_string('=',' ',s3);
                                   val(s1,Pl,i);
                                   Pl:=Pl/1000.0;
                                 end;
                 P_ROW_NO:       begin
                                   s1:=get_string('=',' ',s3);
                                   row_number:=strtoint(s1);
                                 end;
                 P_CIRCUIT_NO:   begin
                                   s1:=get_string('=',' ',s3);
                                   circuit_number:=strtoint(s1);
                                 end;
                 P_TUBE_PER_ROW: begin
                                   s1:=get_string('=',' ',s3);
                                   tube_no_per_row:=strtoint(s1);
                                 end;
                 P_DC:           begin
                                   s1:=get_string('=',' ',s3); {Dc}
                                   val(s1,D_c,i);
                                   D_c:=D_c/1000.0; {mm}
                                 end;
                 P_Tt:           begin
                                   s1:=get_string('=',' ',s3);   {tube_thickness}
                                   val(s1,wall_thickness,i);
                                   wall_thickness:=wall_thickness/1000.0;
                                   wall_thickness:=wall_thickness*TUBE_EXPANSION_RATIO;
                                 end;
                 P_ENHANCE_RATIO:begin
                                   s1:=get_string('=',' ',s3); {Dc}
                                   val(s1,Enhance_Ratio,i);
                                 end;
                 P_Surface_treatment: begin
                                    surface_treatment:=uppercase(s3);
                                 end;
                 P_Surface_Type: begin
                                   surface_Type:=uppercase(get_string('=',' ',s3));
                                   Wet_datafile:=true;
                                   if Surface_type='DRY' then Wet_Datafile:=false else Wet_datafile:=true;
                                   if Wet_datafile then Surface_Type:='Wet';
                                 end;
                 P_Flow_Arrangement:
                                 begin
                                   s2:=uppercase(s3);
                                   Flow_arrangement:=uppercase(get_string('=',' ',s3));
                                   if pos('STAGGERED',s2)<>0 then staggered:=true else staggered:=false;
                                 end;
                 P_FIN_MATERIAL: begin
                                   Fin_material:=uppercase(get_string('=',' ',s3));
                                 end;
                 P_FIN_TYPE    : begin
                                   Fin_Type:=Uppercase(s3);
                                   if pos('WAVY',Fin_Type)<>0 then wavy_fin:=true;
                                   if pos('CONVEX',Fin_Type) <> 0 then wavy_fin:=true;
                                 end;
                 P_Waffle_Height:begin
                                   s1:=get_string('=',' ',s3); {Waffle_height}
                                   val(s1,Waffle_Height,i);
                                 end;
                 P_Xf          : begin
                                   s1:=get_string('=',' ',s3); {Xf}
                                   val(s1,Xf,i);
                                 end;
                 P_COUNTER_CROSS_FLOW:
                                 begin
                                   Tube_Layout:=Uppercase(s3);
                                   if pos('COUNTER',Tube_Layout)<>0 then Counter_Cross_Flow:=true;
                                 end;
                 P_TUBE_TYPE   : begin
                                   Tube_Type:=uppercase(s3);
                                   if (pos('ALUMINUM',Tube_Type)<>0) then k_wall:=204.0 else k_wall:=386.0;
                                 end;
                 P_Tube_Layout : begin
                                   Tube_Layout:=uppercase(s3);
                                 end;
            end;
          until check_data=P_BEGIN_END_DATA;
          if abs((Depth-row_number*Pl)/Depth)>0.01 then begin
             writeln('Depth of the HX does not equal to N*Pl');
             writeln('Please hit "ENTER" to stop');
             readln;
             halt;
          end;
          if abs((H-tube_no_per_row*Pt)/H)>0.01 then begin
             writeln('Height of the HX does not equal to tube number N*Pt');
             writeln('Please hit "ENTER" to stop');
             readln;
             halt;
          end;
          Req:=sqrt(Pt*Pl/pi);
          D_o:=D_c-2.0*fin_thickness;
          D_i:=D_o-2.0*wall_thickness;
          Af:=2.0*fin_no*(Pl*H-pi/4.0*sqr(D_c)*tube_no_per_row)*row_number
             +2.0*fin_thickness*(H+Pl*row_number);
          if wavy_fin then begin
             Af:=Af/cos(arctan(Waffle_Height/Xf));
          end;
          At:=pi*D_c*(W-fin_no*fin_thickness)*tube_no_per_row*row_number;
          Ati:=pi*D_i*W*tube_no_per_row*row_number;
          Apm:=pi*W*tube_no_per_row*row_number*(D_c-D_i)/ln(D_c/D_i);
          Ao:=Af+At;
          Ac:=W*H-tube_no_per_row*(D_c*W+fin_no*fin_thickness*(Pt-D_c));
          Ai:=pi*D_i*row_number*tube_no_per_row*W;
          Aci:=pi*sqr(D_i)/4.0;
          epslon:=Ao/At;
          Afr:=W*H;
          fin_pitch:=W/fin_no;
          fin_spacing:=fin_pitch-fin_thickness;
          sigama:=Ac/Afr;
          Beta:=Ao/W/H/Depth;
          Dh:=4.0*Ac*Depth/Ao;
          Get_Sectors(A_region1,A_region2,Area1,R1,Area2,R2,D_c/2.0);
          writeln(f1,chr(27),'E');
          writeln(f1,chr(27),'(s16.66H');
          writeln(f1,chr(27),'&l1O');
          writeln(f1,chr(27),'&l6C');
          writeln(f1,'Original Raw Data Filename    =',fn);
          writeln(f1,'This Analyzing Output Filename=',s5);
          s5:=' Using Exact Modified Bessel Functions in Sector/Circular Analysis, With the number of Sectors used = ';
          writeln(f1,s5,4*(Sectors_N1+Sectors_N2));
          writeln(f1,' Standard SI unit, Area use m^2, pressure drop= Pa, temperature = degree C, length = m');
          writeln(f1,' mass flow rate m = kg/s, heat transfer coefficient = W/m^2 K, Mass Transfer Coeff, hd= kg/m^2 s, Le=Lewis Number');
          writeln(f1,' _EQU: Use Equivalent Circular Area for Fin efficiency Calculation, _SEC: Use Sector Method,',
                     ' _SCH: Use Schmidt, _H&W: Use Hong and Webb; EQU and SEC is accompanied with Threlkeld method');
          s3:='*********************************************************************************************';
          s3:=s3+s3;
          writeln(f1,s3);
          writeln(f1,Surface_treatment,',   Surface Conditions = ',surface_type,',  Fin_material=',Fin_material,',  ',Fin_Type);
          writeln(f1,Tube_type,',  ',Tube_Layout,', Flow_arrangement=',Flow_arrangement);
          writeln(f1,'  Epsolon =',epslon:9:3,   ' sigama=',sigama:9:3,  '           Row=',Row_number:9,'   Fin no=',fin_no:9);
          writeln(f1,'Beta(Ao/V)=',Beta:9:4,     '  Dh(m)=',Dh:9:6,      '         Di(m)=',D_i:9:6,     '   Do(m) =',D_o:9:6,' Circuit no=',circuit_number:9);
          writeln(f1,'       Ao =',Ao:9:4,       '     Af=',Af:9:4,'            Ac=',Ac:9:4,
                     '      At =',At:9:5,' Afr=',Afr:9:5);
          writeln(f1,'Fin pitch =',fin_pitch:9:5,'     Dc=',D_c:9:5,     '         Fp/Dc=',fin_pitch/D_c:9:5,' Tube enhance_ratio =',enhance_ratio:8:3,
                     '    Fin Spacing (m)=',fin_pitch-Fin_thickness:9:6);
          if wavy_fin then writeln(f1,'  Waffle height (mm)=',Waffle_height:9:5,'  Xf (mm) =',Xf:9:5);
          writeln(f1,'    W (m) =',W:9:4,        '   H(m)=',H:9:4,       '      tube/row=',tube_no_per_row:9,'  Hydraulic Diameter (m)=',Dh:9:6);
          writeln(f1,'   Pl (m) =',Pl:9:5,       '  Pt(m)=',Pt:9:5,      ' Fin Thickness=',fin_thickness:9:6);
          writeln(f1,s3);
          writeln(f1,'Re_Dc':7,'Re_Dm':7,'Re_Dh':7,'Re_Dhm':7,'j_Equ':8,'eff_E':6,'fin_E':6,'jm_Equ':8,'Le_E':6,'Le_LM':6,'hd_LMWD':9,'Le_Ma':6,'hd_Mass':9,'jd_Mass':8,
                               'j_Sec':8,'eff_S':6,'fin_S':6,'jm_Sec':8,'Le_Sec':7,'E*h_co':7,
                               'f(KeKc)':8,'f(-KeKc)':8,'P':6,'R':6,'F':6,'2T/u':7);
          writeln(f2,'u_fr':6,'h_Equ':8,'h_Sec':7,
                     'DP-(Pa)':8,'Qt':7,'Qs':7,'Ql':7,'m-air':7,'m-water':8,
                     'DB:in':6,'WB:in':6,'RH:in':6,'W:in':7,'i:in':8,'DB:out':7,'WB:out':7,'RH:Out':7,'W:out':7,'i:out':8,
                     'WC_IN':6,'WC_Out':7,'m_con':9,'Gamma':9,'System_P':9,'Dev/Air%':9);
     end;
     j:=0;
     repeat
         read(f,s1);
         if pos('==',s1) <> 0 then break;
         read(f,ufr,DB_in,DB_out,WB_in,WB_out,DP,RH_in,RH_out);
         read(f,Air_CMM,Tw_in,Tw_out,water_CMM,P_Nozzle);
         read(f,T_nozzle,DP_nozzle,V_nozzle,Nozzle_no,T_room);
         read(f,data.Q_sensible,data.Q_latent,data.Q_air,data.Q_average);
         readln(f,Dev,DewTi,DewTo,P_system);
         inc(j);
         with data do begin
              u_frontal:=ufr;
              T_water_in:=Tw_in;
              T_water_out:=Tw_out;
              T_DB_in:=DB_in;
              T_DB_out:=DB_out;
              T_WB_in:=WB_in;
              T_WB_out:=WB_out;
              System_Pressure:=P_system*1000.0; {in Pa}
              Situation1(inlet_air,T_DB_in,T_WB_in,System_pressure);
              Situation1(outlet_air,T_DB_out,T_WB_out,System_pressure-DP);
              D_air:=air_density(P_nozzle*1000.0,T_Nozzle);
              Cpa:=(inlet_air.Cpm+outlet_air.Cpm)/2.0;
              ma:=Air_CMM/60.0*D_air; {T in degree °C}
              D_air_in:=air_density(System_pressure,T_DB_in);
              u_frontal:=ma/inlet_air.density/W/H;
              Tw_mean:=(T_water_in+T_water_out)/2.0;
              d_water:=water_density(Tw_mean);
              cp_water:=water_cp(Tw_mean);
              m_water:=water_CMM*d_water/60.0; {kg/s}
              Q_water:=m_water*cp_water*(T_water_out-T_water_in);
              Q_sensible:=ma*Cpa*(T_DB_in-T_DB_out);
              Q_air:=ma*(inlet_air.enthalpy-outlet_air.enthalpy);
              if not Wet_datafile then begin
                 Q_water:=abs(Q_water);
                 Q_sensible:=abs(Q_sensible);
                 Q_air:=Q_sensible;
              end;
              Q_average:=(Q_water+Q_air)/2.0;
              Qs:=Q_sensible;
              Qt:=Q_average;
              Dev1:=100.0*(1.0-Q_air/Q_average);
              Dev2:=100.0*(1.0-Q_water/Q_average);
              G_air:=ma/Ac;   {based on minimum flow area }
              Re_Dc:=G_air*D_c/air_viscosity(T_DB_in);
              Re_Dh:=G_air*Dh/air_viscosity(T_DB_in);
              Re_Dc_ave:=2.0*G_air*D_c/(air_viscosity(T_DB_in)+air_viscosity(T_DB_out));
              Re_Dh_ave:=2.0*G_air*Dh/(air_viscosity(T_DB_in)+air_viscosity(T_DB_out));
              K_entrance:=0.80846-1.0568*sigama;
              K_exit:=0.47227-0.3261*sigama;
              density_in:=inlet_air.density;
              density_out:=outlet_air.density;
              density_mean:=(density_in+density_out)/2.0;
              Term1:=K_entrance+1.0-sqr(sigama);
              Term2:=2.0*(density_in/density_out-1.0);
              Term3:=(1.0-sqr(sigama)-K_exit)*density_in/density_out;
              pressure_drop:=DP;
              friction_factor_Ke_Kc:=density_mean*Ac/density_in/Ao*
                               (density_in*2.0*DP/sqr(G_air)-Term1-Term2+Term3);
              Term1:=1.0-sqr(sigama);
              Term2:=2.0*(density_in/density_out-1.0);
              Term3:=(1.0-sqr(sigama))*density_in/density_out;
              pressure_drop:=DP;
              friction_factor:=density_mean*Ac/density_in/Ao*
                               (density_in*2.0*DP/sqr(G_air)-Term1-Term2+Term3);
              REDUCTION_WET(data);
              writeln(f1,Re_Dc:7:1,Re_Dc_ave:7:1,Re_Dh:7:1,Re_Dh_ave:7:1,Equ_circle.j:8:5,Equ_Circle.effectiveness:6:3,
                                   Equ_circle.efficiency:6:3,Equ_circle.j_mass:8:5,Equ_circle.Le:6:3,Lewis_analogy:6:3,Mass_Transfer_coeff:9:6,Le_by_Gmass:6:3,G_mass_Coeff:9:6,jd_mass:8:5,
                                   Sector.j:8:5,Sector.effectiveness:6:3,Sector.efficiency:6:3,
                                   Sector.j_mass:8:5,Sector.Le:7:3,
                                   Equ_Circle.Eta_h:7:2,
                                   friction_factor:8:5,friction_factor_Ke_Kc:8:5,P_value:6:3,R_value:6:3,F_value:6:3,
                                   condensate_Reynolds_number:7:3);
              writeln(f2,u_frontal:6:3,Equ_circle.h_co:7:2,Sector.h_co:7:2,
                         DP:8:2,Qt:8:1,Q_sensible:7:1,Qt-Q_Sensible:7:1,ma:7:4,m_water:8:5,
                         T_DB_in:6:2,T_WB_in:6:2,100*inlet_air.Relative_humidity:5:1,inlet_air.W:8:5,inlet_air.enthalpy:8:1,T_DB_out:7:2,T_WB_out:7:2,
                         100*outlet_air.Relative_humidity:6:1,outlet_air.W:8:5,outlet_air.enthalpy:8:1,
                         T_water_in:6:2,T_water_out:7:2,mass_transfer:9:6,Gamma:9:6,System_pressure:9:1,Dev1:9:3);
         end;
     until j > 100;
     close(f);
     close(f1);
     closefile(f2);
     assignfile(f1,outfile);
     Append(f1); { Add more text onto end }
     assignfile(f2,tmpfile);
     reset(f2);
     writeln(f1);
     writeln(f1,'Dimensional Parameters for the data file ');
     writeln(f1,s3);
     while not eof(f2) do begin
           readln(f2,s5);
           writeln(f1,s5);
     end;
     CloseFile(f1);
     Closefile(f2);
   end;

initialization
   Initialize(data);
   Writeln('Wet data Reduction program, Written by Chi-Chuan Wang, All Rights Reserved');
   writeln('Use this reduction program beyond ERL/ITRI is not allowed');
   writeln('--------------------------------------------------------------------------');
   openfile2('d:\figure.gs2\lee_nthu_2000\Heat transfer engng\B01.ggg');
   openfile2('d:\figure.gs2\lee_nthu_2000\Heat transfer engng\B02.ggg');
   openfile2('d:\figure.gs2\lee_nthu_2000\Heat transfer engng\B03.ggg');
   openfile2('d:\figure.gs2\lee_nthu_2000\Heat transfer engng\B07.ggg');
   openfile2('d:\figure.gs2\lee_nthu_2000\Heat transfer engng\B08.ggg');
   openfile2('d:\figure.gs2\lee_nthu_2000\Heat transfer engng\B09.ggg');
(*   if paramcount<>0 then begin
      ss:=paramstr(1);
      if not fileexists(ss) then begin
         writeln('The GGG File ',ss,' does not exist! Please Hit the Enter Key to Return to Windows');
         readln;
         halt;
      end;
      openfile2(ss);
   end
   else begin
      write('Please input your datafile [.GGG] file-->');
      readln(ss);
      ss:=uppercase(ss);
      if pos('.GGG',ss) = 0 then ss:=ss+'.GGG';
      if not fileexists(ss) then begin
         writeln('The GGG File ',ss,' does not exist! Please Hit the Enter Key to Return to Windows');
         readln;
         halt;
      end;
      openfile2(ss);
   { openfile2('d:\figure.gs2\Chang_pccu\wet_data\slit_n_Dc10_Fp2.5_Pt25_PL22_R2.ggg');
    openfile2('d:\figure.gs2\Chang_pccu\wet_data\slit_n_Dc10_Fp1.7_Pt25_PL22_R2.ggg');
    openfile2('d:\figure.gs2\Chang_pccu\wet_data\slit_n_Dc10_Fp1.2_Pt25_PL22_R2.ggg');
    openfile2('d:\figure.gs2\Chang_pccu\wet_data\slit_n_Dc10_Fp2.5_Pt25_PL22_R1.ggg');
    openfile2('d:\figure.gs2\Chang_pccu\wet_data\slit_n_Dc10_Fp1.7_Pt25_PL22_R1.ggg');
    openfile2('d:\figure.gs2\Chang_pccu\wet_data\slit_n_Dc10_Fp1.2_Pt25_PL22_R1.ggg');

 {  openfile2('d:\figure.gs2\Chang_pccu\wet_data\plain_n_dc10_Fp1.2_Pt25_PL22_R1.ggg');
   openfile2('d:\figure.gs2\Chang_pccu\wet_data\plain_n_dc10_Fp1.7_Pt25_PL22_R1.ggg');
 {
   openfile2('d:\figure.gs2\Chang_pccu\wet_data\plain_n_dc10_Fp2.5_Pt25_PL22_R1.ggg');

   openfile2('d:\figure.gs2\Chang_pccu\wet_data\slit_n_Dc10_Fp2.5_Pt25_PL22_R1.ggg');
   openfile2('d:\figure.gs2\Chang_pccu\wet_data\slit_n_Dc10_Fp1.7_Pt25_PL22_R1.ggg');
   openfile2('d:\figure.gs2\Chang_pccu\wet_data\slit_n_Dc10_Fp1.2_Pt25_PL22_R1.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\compact_louver_7\louver_C_Dc7_Fp2_N1_Pt17.7_PL13.6.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\compact_louver_7\louver_N_Dc7_Fp2_N1_Pt17.7_PL13.6.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\compact_louver_7\louver_N_Dc7_Fp1.2_N1_Pt17.7_PL13.6.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\compact_louver_7\plain_N_Dc7_Fp2_N1_Pt17.7_PL13.6.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\compact_louver_7\plain_N_Dc7_Fp1.2_N1_Pt17.7_PL13.6.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\compact_louver_7\plain_C_Dc7_Fp2_N1_Pt17.7_PL13.6.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\compact_louver_7\plain_C_Dc7_Fp1.2_N1_Pt17.7_PL13.6.ggg');
   openfile2('d:\figure.gs2\Du_Pccu\wet_data\Wavy_Row_4_Fp_1.7_Pd_1.18_Pt_25.4_Pl_19.05_Dc_8.62.ggg');
   openfile2('d:\figure.gs2\Du_Pccu\wet_data\Wavy_Row_4_Fp_3.1_Pd_1.18_Pt_25.4_Pl_19.05_Dc_8.62.ggg');

   openfile2('d:\figure.gs2\Du_Pccu\wet_data\Wavy_Row_6_Fp_1.59_Pd_1.18_Pt_25.4_Pl_19.05_Dc_10.38_2.ggg');
   openfile2('d:\figure.gs2\Du_Pccu\wet_data\Wavy_Row_6_Fp_2.85_Pd_1.18_Pt_25.4_Pl_19.05_Dc_10.38_2.ggg');
   openfile2('d:\figure.gs2\Du_Pccu\wet_data\Wavy_Row_6_Fp_3.09_Pd_1.58_Pt_25.4_Pl_19.05_Dc_8.62_2.ggg');
   openfile2('d:\figure.gs2\Du_Pccu\wet_data\Wavy_Row_6_Fp_2.87_Pd_1.58_Pt_25.4_Pl_19.05_Dc_10.38_2.ggg');
   openfile2('d:\figure.gs2\Du_Pccu\wet_data\Wavy_Row_6_Fp_1.63_Pd_1.58_Pt_25.4_Pl_19.05_Dc_10.38_2.ggg');
   openfile2('d:\figure.gs2\Du_Pccu\wet_data\Wavy_Row_6_Fp_1.67_Pd_1.58_Pt_25.4_Pl_19.05_Dc_8.62.ggg');
   openfile2('d:\figure.gs2\Du_Pccu\wet_data\Wavy_Row_6_Fp_1.59_Pd_1.18_Pt_25.4_Pl_19.05_Dc_10.38.ggg');
   openfile2('d:\figure.gs2\Du_Pccu\wet_data\Wavy_Row_6_Fp_2.87_Pd_1.58_Pt_25.4_Pl_19.05_Dc_10.38.ggg');
   openfile2('d:\figure.gs2\Du_Pccu\wet_data\Wavy_Row_6_Fp_1.63_Pd_1.58_Pt_25.4_Pl_19.05_Dc_10.38.ggg');
   openfile2('d:\figure.gs2\Du_Pccu\wet_data\Wavy_Row_6_Fp_3.09_Pd_1.58_Pt_25.4_Pl_19.05_Dc_8.62.ggg');
   openfile2('d:\figure.gs2\Du_Pccu\wet_data\Wavy_Row_6_Fp_2.85_Pd_1.18_Pt_25.4_Pl_19.05_Dc_10.38.ggg');


   openfile2('d:\figure.gs2\Du_Pccu\wet_data\Wavy_Row_4_Fp_3.14_Pd_1.58_Pt_25.4_Pl_19.05_Dc_8.62.ggg');
   openfile2('d:\figure.gs2\Du_Pccu\wet_data\Wavy_Row_6_Fp_2.85_Pd_1.18_Pt_25.4_Pl_19.05_Dc_10.38.ggg');

   openfile2('d:\figure.gs2\Du_Pccu\wet_data\Wavy_Row_1_Fp_2.95_Pd_1.58_Pt_25.4_Pl_19.05_Dc_8.62.ggg');
   openfile2('d:\figure.gs2\Du_Pccu\wet_data\Wavy_Row_1_Fp_2.85_Pd_1.18_Pt_25.4_Pl_19.05_Dc_10.38.ggg');
   openfile2('d:\figure.gs2\Du_Pccu\wet_data\Wavy_Row_1_Fp_1.62_Pd_1.18_Pt_25.4_Pl_19.05_Dc_10.38.ggg');
   openfile2('d:\figure.gs2\Du_Pccu\wet_data\Wavy_Row_1_Fp_3.66_Pd_1.68_Pt_25.4_Pl_25.4_Dc_8.62.ggg');
   openfile2('d:\figure.gs2\Du_Pccu\wet_data\Wavy_Row_1_Fp_1.65_Pd_1.58_Pt_25.4_Pl_19.05_Dc_8.62.ggg');
   openfile2('d:\figure.gs2\Du_Pccu\wet_data\Wavy_Row_1_Fp_3.66_Pd_1.68_Pt_25.4_Pl_25.4_Dc_8.62.ggg');
   openfile2('d:\figure.gs2\Du_Pccu\wet_data\Wavy_Row_4_Fp_1.64_Pd_1.58_Pt_25.4_Pl_19.05_Dc_8.62.ggg');

   openfile2('d:\figure.gs2\chiou_ncku\wet_data\wavy_Pd1.18_Dc10.38_N1_Fp2.85_Pt25.4_Pl19.05.ggg');

   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver_June_1998\louver_N_Dc10_N2_Fp1.2_Pt25.4_Pl22_T_10.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver_June_1998\louver_N_Dc10_N2_Fp1.2_Pt25.4_Pl22_Third.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver_June_1998\louver_N_Dc10_N2_Fp1.2_Pt25.4_Pl22_SEcond.ggg');

   openfile2('d:\figure.gs2\chi\wet\data\plain_n_Dc10_N1_Fp1.2_Pt25.4_Pl22.ggg');
   openfile2('d:\figure.gs2\chi\wet\data\plain_n_Dc10_N1_Fp2.5_Pt25.4_Pl22.ggg');
   openfile2('d:\figure.gs2\chi\wet\data\plain_n_Dc8_N1_Fp1.19_Pt25.4_Pl19.05.ggg');
   openfile2('d:\figure.gs2\chi\wet\data\plain_n_Dc8_N1_Fp2.04_Pt25.4_Pl19.05.ggg');
   openfile2('d:\figure.gs2\chi\wet\data\plain_n_Dc8_N2_Fp1.23_Pt25.4_Pl19.05.ggg');
   openfile2('d:\figure.gs2\chi\wet\data\plain_n_Dc8_N2_Fp2.06_Pt25.4_Pl19.05.ggg');
   openfile2('d:\figure.gs2\chi\wet\data\plain_n_Dc8_N4_Fp1.21_Pt25.4_Pl19.05.ggg');
   openfile2('d:\figure.gs2\chi\wet\data\plain_n_Dc8_N4_Fp1.6_Pt25.4_Pl19.05.ggg');


   openfile2('d:\figure.gs2\chi\wet\data\plain_n_Dc7_N2_Fp1.23_Pt21_Pl12.7.ggg');

   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver_June_1998\louver_c_Dc10_N1_Fp1.2_Pt25.4_Pl19.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver_June_1998\louver_c_Dc10_N1_Fp1.8_Pt25.4_Pl19.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver_June_1998\louver_c_Dc10_N1_Fp2.5_Pt25.4_Pl19.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver_June_1998\louver_c_Dc10_N2_Fp1.2_Pt25.4_Pl19.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver_June_1998\louver_c_Dc10_N2_Fp1.8_Pt25.4_Pl19.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver_June_1998\louver_c_Dc10_N2_Fp2.5_Pt25.4_Pl19.ggg');

   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver_June_1998\louver_n_Dc10_N1_Fp1.2_Pt25.4_Pl19.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver_June_1998\louver_n_Dc10_N1_Fp1.8_Pt25.4_Pl19.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver_June_1998\louver_n_Dc10_N1_Fp2.5_Pt25.4_Pl19.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver_June_1998\louver_n_Dc10_N2_Fp1.2_Pt25.4_Pl19.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver_June_1998\louver_n_Dc10_N2_Fp1.8_Pt25.4_Pl19.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver_June_1998\louver_n_Dc10_N2_Fp2.5_Pt25.4_Pl19.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver_June_1998\louver_n_Dc10_N1_Fp1.2_Pt25.4_Pl22.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver_June_1998\louver_n_Dc10_N1_Fp2.5_Pt25.4_Pl22.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver_June_1998\louver_n_Dc10_N2_Fp1.2_Pt25.4_Pl22.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver_June_1998\louver_n_Dc10_N2_Fp2.5_Pt25.4_Pl22.ggg');


   openfile2('d:\figure.gs2\chiou_ncku\wet_data\wavy_Pd1.18_Dc8.62_N2_Fp1.7_Pt25.4_Pl19.05.ggg');
   openfile2('d:\figure.gs2\chiou_ncku\wet_data\wavy_Pd1.58_Dc8.62_N2_Fp1.7_Pt25.4_Pl19.05.ggg');
   openfile2('d:\figure.gs2\chiou_ncku\wet_data\wavy_Pd1.18_Dc8.62_N2_Fp3.1_Pt25.4_Pl19.05.ggg');
   openfile2('d:\figure.gs2\chiou_ncku\wet_data\wavy_Pd1.58_Dc8.62_N2_Fp3.1_Pt25.4_Pl19.05.ggg');
   openfile2('d:\figure.gs2\chiou_ncku\wet_data\wavy_Pd1.18_Dc8.62_N4_Fp1.7_Pt25.4_Pl19.05.ggg');
   openfile2('d:\figure.gs2\chiou_ncku\wet_data\wavy_Pd1.18_Dc8.62_N4_Fp3.1_Pt25.4_Pl19.05.ggg');

   openfile2('d:\figure.gs2\chiou_ncku\wet_data\plain_Dc8.62_N2_Fp1.7_Pt25.4_Pl19.05.ggg');
   openfile2('d:\figure.gs2\chiou_ncku\wet_data\plain_Dc8.62_N4_Fp1.7_Pt25.4_Pl19.05.ggg');
   openfile2('d:\figure.gs2\chiou_ncku\wet_data\plain_Dc8.62_N2_Fp3.1_Pt25.4_Pl19.05.ggg');
   openfile2('d:\figure.gs2\chiou_ncku\wet_data\plain_Dc8.62_N4_Fp3.1_Pt25.4_Pl19.05.ggg');

   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver\louver_n_Dc10_N1_Fp1.2_Pt25.4_Pl22.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver\louver_n_Dc10_N2_Fp1.2_Pt25.4_Pl22.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver\louver_n_Dc10_N1_Fp2.5_Pt25.4_Pl22.ggg');

   openfile2('d:\figure.gs2\chi\wet\data\plain_n_Dc7_N4_Fp1.23_Pt21_Pl12.7.ggg');
   openfile2('d:\figure.gs2\chi\wet\data\louver_c_Dc7_N2_Fp1.23_Pt21_Pl12.7.ggg');
   openfile2('d:\figure.gs2\chi\wet\data\plain_c_Dc7_N2_Fp1.79_Pt21_Pl12.7.ggg');
   openfile2('d:\figure.gs2\chi\wet\data\plain_n_Dc7_N2_Fp1.23_Pt21_Pl12.7.ggg');
   openfile2('d:\figure.gs2\chi\wet\data\plain_c_Dc7_N2_Fp1.23_Pt21_Pl12.7.ggg');
   openfile2('d:\figure.gs2\chi\wet\data\plain_n_Dc7_N2_Fp1.78_Pt21_Pl12.7.ggg');
   openfile2('d:\figure.gs2\chi\wet\data\plain_c_Dc7_N2_Fp1.23_Pt21_Pl12.7.ggg');
   openfile2('d:\figure.gs2\chi\wet\data\louver_c_Dc7_N2_Fp1.69_Pt21_Pl12.7.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver\louver_n_Dc10_N1_Fp1.2_Pt25.4_Pl19.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver\louver_n_Dc10_N1_Fp1.8_Pt25.4_Pl19.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver\louver_n_Dc10_N2_Fp1.2_Pt25.4_Pl19.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver\louver_n_Dc10_N2_Fp1.8_Pt25.4_Pl19.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver\louver_c_Dc10_N2_Fp2.5_Pt25.4_Pl19.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver\louver_n_Dc10_N2_Fp2.5_Pt25.4_Pl22.ggg');

   openfile2('d:\figure.gs2\lee_bird\wet_surface\louver\louver_n_Dc10_N1_Fp1.2_Pt25.4_Pl19.ggg');

   openfile2('d:\figure.gs2\chi\wet\data\plain_n_Dc10_N1_Fp2.23_Pt25.4_Pl19.05.ggg');
   openfile2('d:\figure.gs2\chi\wet\data\plain_n_Dc10_N2_Fp1.23_Pt25.4_Pl19.05.ggg');
   openfile2('d:\figure.gs2\chi\wet\data\plain_n_Dc10_N4_Fp1.55_Pt25.4_Pl19.05.ggg');
   openfile2('d:\figure.gs2\chi\wet\data\plain_n_Dc10_N4_Fp1.23_Pt25.4_Pl19.05.ggg');
   openfile2('d:\figure.gs2\chi\wet\data\plain_n_Dc10_N4_Fp2.31_Pt25.4_Pl19.05.ggg');
   openfile2('d:\figure.gs2\chi\wet\data\plain_c_Dc7_N4_Fp1.22_Pt21_Pl12.7.ggg');
   openfile2('d:\figure.gs2\chi\wet\data\plain_n_Dc7_N4_Fp1.78_Pt21_Pl12.7.ggg');
   openfile2('d:\figure.gs2\lee_bird\wet_surface\Te_slit\bird2.ggg');
   openfile2('d:\figure.gs2\Lee_bird\wet_surface\Te_slit\chun9.ggg');
   openfile2('d:\figure.gs2\Lee_bird\wet_surface\Te_slit\chun.ggg');
   openfile2('d:\figure.gs2\Lee\wavy_fin_wet\b01.ggg');
   openfile2('d:\figure.gs2\Lee\wavy_fin_wet\b02.ggg');
   openfile2('d:\figure.gs2\Lee\wavy_fin_wet\b03.ggg');
   openfile2('d:\figure.gs2\Lee\wavy_fin_wet\b04.ggg');
   openfile2('d:\figure.gs2\Lee\wavy_fin_wet\rb01.ggg');
   openfile2('d:\figure.gs2\Lee\wavy_fin_wet\rb02.ggg');
   openfile2('d:\figure.gs2\Lee\wavy_fin_wet\b13.ggg');
   openfile2('d:\figure.gs2\Lee\wavy_fin_wet\b14.ggg');
   openfile2('d:\figure.gs2\Lee\wavy_fin_wet\b15.ggg');
   openfile2('d:\figure.gs2\Lee\wavy_fin_wet\b16.ggg');
   openfile2('d:\figure.gs2\Lee\wavy_fin_wet\b17.ggg');
   openfile2('d:\figure.gs2\Lee\wavy_fin_wet\b18.ggg');
   openfile2('d:\figure.gs2\Lee\Tai_Chuan\small3.ggg');
   openfile2('d:\figure.gs2\Lee\louver_and_plain_7mm_wet\a01.ggg');
   openfile2('d:\figure.gs2\Lee\louver_and_plain_7mm_wet\a02.ggg');
   openfile2('d:\figure.gs2\Lee\louver_and_plain_7mm_wet\a03.ggg');
   openfile2('d:\figure.gs2\Lee\louver_and_plain_7mm_wet\a04.ggg');
   openfile2('d:\figure.gs2\Lee\louver_and_plain_7mm_wet\a05.ggg');
   openfile2('d:\figure.gs2\Lee\louver_and_plain_7mm_wet\a06.ggg');
   openfile2('d:\figure.gs2\Lee\louver_and_plain_7mm_wet\a07.ggg');
   openfile2('d:\figure.gs2\Lee\louver_and_plain_7mm_wet\a08.ggg');
   openfile2('d:\figure.gs2\Lee\louver_and_plain_7mm_wet\a09.ggg');
   openfile2('d:\figure.gs2\Lee\louver_and_plain_7mm_wet\a10.ggg');
   openfile2('d:\figure.gs2\Lee\TE_HX_Louver_wet\Tec1r50.ggg');
   openfile2('d:\figure.gs2\Lee\TE_HX_Louver_wet\Tec1r90.ggg');
   openfile2('d:\figure.gs2\Lee\TE_HX_Louver_wet\Tec2r90.ggg');
   openfile2('d:\figure.gs2\Lee\TE_HX_Louver_wet\Tec3r90.ggg');
   openfile2('d:\figure.gs2\Lee\TE_HX_Louver_wet\Tec4r90.ggg');
   openfile2('d:\figure.gs2\Lee\TE_HX_Louver_wet\Tec5r90.ggg');
   openfile2('d:\figure.gs2\Lee\TE_HX_Louver_wet\1350.ggg');
   openfile2('d:\figure.gs2\Lee\TE_HX_Louver_wet\1390.ggg');
   openfile2('d:\figure.gs2\Lee\TE_HX_Louver_wet\1450.ggg');
   openfile2('d:\figure.gs2\Lee\TE_HX_Louver_wet\1490.ggg');
   openfile2('d:\figure.gs2\Lee\TE_HX_Louver_wet\1550.ggg');
   openfile2('d:\figure.gs2\Lee\TE_HX_Louver_wet\1590.ggg');
   openfile2('d:\figure.gs2\platefin.wet\data\pr6f2w90.avg');
   openfile2('d:\figure.gs2\platefin.wet\data\pr6f2w50.avg');
   openfile2('d:\figure.gs2\platefin.wet\data\pr6f3w90.avg');
   openfile2('d:\figure.gs2\platefin.wet\data\pr6f3w50.avg');
   openfile2('d:\figure.gs2\platefin.wet\data\pr6f1w90.avg');
   openfile2('d:\figure.gs2\platefin.wet\data\pr6f1w50.avg');
   openfile2('d:\figure.gs2\platefin.wet\data\pr4f2w90.avg');
   openfile2('d:\figure.gs2\platefin.wet\data\pr4f2w50.avg');
   openfile2('d:\figure.gs2\platefin.wet\data\pr4f3w90.avg');
   openfile2('d:\figure.gs2\platefin.wet\data\pr4f3w50.avg');
   openfile2('d:\figure.gs2\platefin.wet\data\pr4f1w90.avg');
   openfile2('d:\figure.gs2\platefin.wet\data\pr4f1w50.avg');
   openfile2('d:\figure.gs2\platefin.wet\data\pr2f2w90.avg');
   openfile2('d:\figure.gs2\platefin.wet\data\pr2f2w50.avg');
   openfile2('d:\figure.gs2\platefin.wet\data\pr2f3w90.avg');
   openfile2('d:\figure.gs2\platefin.wet\data\pr2f3w50.avg');
   openfile2('d:\figure.gs2\platefin.wet\data\pr2f1w90.avg');
   openfile2('d:\figure.gs2\platefin.wet\data\pr2f1w50.avg'); }
   end; *)
end.
