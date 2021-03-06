﻿Module Evap

    Public Sub Evap()

        Dim state As String = "  "
        ' there are 2 conditions
        ' 1. saturation state
        ' 2. superheated state


        HPCD.E_Q_sat.Text = 0
        HPCD.E_Q_sup.Text = 0
        HPCD.E_Q_tot.Text = 0

        Dim F As Double

        'Evap_Input <===========================================================================
        ' // [Evap_Geometry_init]  <===========================================================================

        E_W = CDbl(HPCD.E_W_txt.Text) '[m] HX width
        E_H = CDbl(HPCD.E_H_txt.Text) '[m] HX height
        Nrows = CDbl(HPCD.E_Rox_txt.Text) 'Number of rows
        Ncr = CDbl(HPCD.E_Ncr_txt.Text) 'Number of passes 冷媒迴路數

        Fp = CDbl(HPCD.E_FinPitch_txt.Text) '[m] Fin pitch (i.e, distance between fins)
        thickFIN = CDbl(HPCD.E_df_txt.Text) '[m] Fin thickness
        kf = CDbl(HPCD.E_kf_txt.Text) '[W/m⋅K] Fin thermal conductivity

        Pt = CDbl(HPCD.E_Pt_txt.Text) '[m] Transversal fin pitch (i.e., normal to the flow)
        Pl = CDbl(HPCD.E_Pl_txt.Text) '[m] Longitudinal fin pitch (i.e., Parallel to the flow)
        d_o = CDbl(HPCD.E_do__txt.Text) '[m] Tube outer diameter     
        thickTUBE = CDbl(HPCD.E_Xp_txt.Text) '[m] Tube thickness
        kp = CDbl(HPCD.E_kp_txt.Text) '[W/m⋅K] tube thermal conductivity, for brass[銅管]

        Ntube = E_H / Pt 'Number of tubes per rowC:\Users\BH\Dropbox\VS_MASTER\Porject_Beta\HVAC\Cond_Rating\Cond_Rating\PublicFunctions.vb
        HPCD.E_Ntube_txt.Text = Ntube.ToString("0.###")

        ' // [Evap_Geometry_derived]  <===========================================================================

        d_c = d_o + 2 * thickFIN                        '[m] Collar diameter
        d_i = d_o - 2 * thickTUBE * 0.96                '[m] where 0.96 is the efficiency


        Afr = E_W * E_H '[m²] Frontal flow area
        Nfins = Math.Ceiling(E_W / Fp) 'Number of fins
        Ac = Afr - Ntube * (d_c * E_W + Nfins * thickFIN * (Pt - d_c)) '[m²] Minimun free flow Area
        contractionR = Ac / Afr 'Contraction Ratio
        Apo = Math.PI * d_c * (E_W - Nfins * thickFIN) * Ntube * Nrows '[m²] Outside tube surface area
        Afin = 2 * Nfins * (Pl * E_H - 0.25 * Math.PI * d_c ^ 2 * Ntube) * Nrows + 2 * thickFIN * Nfins * (E_H + Pl * Nrows) '[m²] Fin surface Area
        Ao = Apo + Afin  '[m²] Total Outside heat transfer area
        Api = Math.PI * d_i * E_W * Ntube * Nrows '[m²] Total inside heat transfer area
        Apm = (d_c - d_i) / (Math.Log(d_c / d_i)) * Math.PI * E_W * Ntube * Nrows '[m²] mean tube area


        ' // [Air_Property_init]  <===========================================================================

        '-Air'

        'MoistAirProperty.DBT = 27

        'Dim TEST As New Fluid("r22", "SI", "TP")
        'TEST.SatProp(CtoK(MoistAirProperty.DBT))
        'MsgBox(TEST.iL)

        'Dim SatWater As New Fluid("WATER.FLD", "si", "tp")
        'SatWater.SatProp(CtoK(MoistAirProperty.DBT))

        'MoistAirProperty.Ps = SatWater.P * 1000
        'MsgBox(MoistAirProperty.Ps)

        MoistAir()

        Tdry = CtoK(CDbl(HPCD.E_DBT_txt.Text)) '[K] Dry-bulk temperature
        RH = CDbl(HPCD.E_RH_txt.Text) 'Percentage 
        Pai = CDbl(HPCD.E_Patm_txt.Text) '[MPa]
        mair = CDbl(HPCD.E_mair_txt.Text) ' 0.1666 '[kg/s]

        'Twet = CtoK(CDbl(HPCD.E_WBT_txt.Text)) '[K] Wet-bulc temperature
        'Tdew = CtoK(TdewPoint(KtoC(Tdry), RH)) '[K] Dew point temperature
        'Wai = 0.0110835 '[kg-water-vapor/kg-dry-air]

        Twet = MoistAirProperty.WBT '[K] Wet-bulc temperature
        Tdew = CtoK(MoistAirProperty.Dew) '[K] Dew point temperature
        Wai = MoistAirProperty.W '[kg-water-vapor/kg-dry-air]

        ' // [Air_Property_derived]  <===========================================================================




        ' // [Refrigerant_Property_init]  <===========================================================================
        'Refrigerant
        Tri = CDbl(HPCD.Tevap_sat.Text) '[K] Saturation temperature of refrigerant
        xri = CDbl(HPCD.Xevap_in.Text) 'Quality of refrigerant
        mref = CDbl(HPCD.MFR.Text) '[kg/s]


        If Tri > Tdry Then
            MsgBox("Error ! the WF temperature is higher than air temperature")
        End If


        ' Evap_Run <===========================================================================



        'Properties of air
        'Create Fluid
        Dim outFluid, inFluid As String
        outFluid = "air"
        ' outFluid = "nitrogen;7812;argon;0092;oxygen;2096"

        Dim vapor As New Fluid("water.FLD", "SI", "TP")
        Dim air As New Fluid(outFluid, "SI", "TP")
        air.Properties(Tdry, Pai)


        Dim ReiL, hiL, Gr, xravg As Double
        Dim Q3avg, xre2, delta_Q As Double


        Dim Rea, Gc As Double
        Gc = mair / Ac                      ' [kg/m²s] Mass Flux maximum (i.e, at minimum free flow area, Ac)
        Rea = Gc * d_c / air.Visc           ' [] Reynolds number of air

        Dim ja, fa, eFiningF, hco As Double
        eFiningF = Ao / Apo



        ' ja = 0.4 * Rea ^ (-0.468 + 0.04076 * Nrows) * (eFiningF ^ 0.159) * Nrows ^ -1.261   '[dimensionless] Colburn j factor for air, wet - Plain fins
        ' hco = (ja * Gc * air.cp) * air.Pr ^ (-2 / 3) 'wet heat transfer coefficient
        fa = 28.209 * (Rea ^ -0.5653) * Nrows ^ -0.1026 * ((Fp / d_c) ^ -1.3405) * eFiningF ^ -1.3343



        'ja = 19.36 * Rea ^ j1 * (Fp / d_c) ^ 1.352 * (Pl / Pt) ^ 0.6795 * Nrows ^ -1.291 ' for plain fin

        Dim jl1, jl2, jl3 As Double
        Dim Lp As Double
        Lp = 0.0025
        jl1 = -0.023634 - 1.2475 * (Fp / d_c) ^ 0.65 * (Pl / Pt) ^ 0.2 * Nrows ^ -0.18
        jl2 = 0.856 * Math.Exp(0.36397) '(Math.Tan(angle))0.36397
        jl3 = 0.25 * Math.Log(Rea)
        ja = 9.717 * Rea ^ jl1 * (Fp / d_c) ^ jl2 * (Pl / Pt) ^ jl3 * Math.Log(3 - Lp / Fp) ^ 0.07162 * Nrows ^ -0.543 ' for louver fin P224

        hco = (ja * Gc * air.cp) * air.Pr ^ (-2 / 3) 'wet heat transfer coefficient<............................................................................Debug

        '------------------------------Inside Tube
        'inFluid = "R32;0.5;R125;0.5"

        'inFluid = "CO2"

        'inFluid = "R32;0.5;R125;0.5"
        inFluid = HPCD.InFluid.Text '"r22"

        Dim mix As New Fluid(inFluid, "SI", "TP")

        Prsat = mix.Psat(Tri)   'Regrigerant Psat corresponding to Tsat
        mix.SatProp(Tri)

        HPCD.Pevap_sat.Text = mix.P.ToString("0.###")

        '_________________________________________________________________________________


        xre = 1 'guess exit quality (i.e, completely evaporated)



        xravg = (xri + xre) / 2 'average refrigerant quality
        Gr = mref / ((0.25 * Math.PI * d_i ^ 2) * Ncr) ' [kg/m²s] mass flux of refrigerant inside tube

        ReiL = (Gr * (1 - xravg) * d_i) / mix.viscL

        hiL = 0.023 * ReiL ^ 0.8 * (mix.PrL ^ 0.4) * mix.kL / d_i '[W/m²K] liquid Heat trans coeff





        Dim hiTP, FrLO, Qr, fs2wet As Double
        Dim irm, iam, iro, iri, iai, iao, delta_im As Double
        Dim bwm, how, Twp, bwp, Mw, nwetf, req, rc, MwP, Uow, A_Uow, B_Uow, C_Uow, C_Uow_box, iswm, i, A_iswm, braket As Double
        Dim Twm, bwm2 As Double
        Dim A, B, C As Double


        '_________________________________________________________________________________

        ' 1.________Qr, Calculate  Heat transfer from refrigerant side,Qr= mr (xe - xi) * Ifg
        '_________________________________________________________________________________
        Dim Q2new As Double
        Dim Tpi, Trm, Tro, ispi, br, Tpo, Xp, ispo, bp, delta_bwm As Double

        Do


            Qr = mref * (xre - xri) * mix.ifg 'Heat transfer calculated from Refrigerant side, asuming complete evaporation.i.e., xe=1

            If xre < xri Then
                MsgBox("Error !!!  xri is  larger than  xre")
            End If



            '_________________________________________________________________________________

            ' 2.________iao, Calculate air oulet enthalpy * error part
            '_________________________________________________________________________________

            iri = iasat(Tri) '[J/kg] saturation enthalpy from Tri, i.e. at refrigerant inlet temperature

            iro = iri

            air.Properties(Tdry, Pai)
            iai = iamoist(Tdry, Wai) '/ 43.5 ' 55260 '[J/kg] Air inlet Enthalpy - using correlation


            iao = iai - Qr / mair

            '_________________________________________________________________________________

            ' 3.________hi, inside tube heat transfer coefficient
            '_________________________________________________________________________________

            xravg = (xri + xre) / 2 'average refrigerant quality
            Gr = mref / ((0.25 * Math.PI * d_i ^ 2) * Ncr) ' [kg/m²s] mass flux of refrigerant inside tube
            ReiL = (Gr * (1 - xravg) * d_i) / mix.viscL
            hiL = 0.023 * ReiL ^ 0.8 * (mix.PrL ^ 0.4) * mix.kL / d_i '[W/m²K] liquid Heat trans coeff

            ' 0610 add

            '_______(3) Frounde number with all flow as liQruid, FrLO
            Dim qflux, Bo, Co, Ffl As Double
            fs2wet = 1
            FrLO = Gr ^ 2 / (mix.rhoL ^ 2 * 9.8 * d_i) ' Frounde number with all flow as liQruid
            '_______(4) Boiling Number
            qflux = Qr / (Api * fs2wet)      '[W/m²] Heat transfer
            Bo = qflux / (Gr * mix.ifg)       ' Boiling Number
            '_______(5) Convection Number, Co

            Co = (((1 - xravg) / xravg) ^ 0.8) * ((mix.rhoG / mix.rhoL) ^ 0.5)
            Ffl = 1.4 'Kandlikar Factor, Fk. See table 4-2 (Page.111)
            KandlikarCoeffs(Co, FrLO) 'Calculate C1~C4 coefficients
            '_______(6) Two Phase Heat transfer coefficient, HiTP

            hiTP = hiL * ((C1 * Co ^ C2 * (25 * FrLO) ^ C5) + (C3 * Bo ^ C4 * Ffl)) ''[W/m²K] Two Phase Heat trans coeff

            hi = hiTP

            '_________________________________________________________________________________
            ' 4.________Enthalpy, i
            '_________________________________________________________________________________

            A = Math.Log((iai - iro) / (iao - iri))

            If ((iai - iro) / (iao - iri)) < 0 Then

                MsgBox("Evap A log(<0) error => Sat")


            Else

                B = (iai - iro) - (iao - iri)
                C = iai - iro

                If iro - iri < 1 Then
                    irm = iri '[J/kg] Mean Refrigerant enthalpy 
                Else
                    irm = iro + ((iro - iri) / A) - (((iro - iri) * C) / B) '[J/kg] Mean Refrigerant enthalpy 
                End If

                iam = iai + (iai - iao) / A - (((iai - iao) * C)) / B   '[J/kg] Mean Air enthalpy [J/kg]
                delta_im = B / A   '[J/kg] Mean Air-refrigerant enthalpy difference


                '_________________________________________________________________________________
                ' 5.________b'-Enthalpy
                '_________________________________________________________________________________

                Tro = Tri

                Trm = (Tri + Tro) / 2
                ''====(1) Find b'r
                Tpi = Trm + Qr / (hiTP * Api * fs2wet)   'tube inside temperature [K]
                ispi = iasat(Tpi)   '[J/kg] Saturation enthalpy tube inside

                br = (ispi - irm) / (Tpi - Trm) 'Slope of saturation-enthalpy curve evaluated at Tpi and Trm
                'br = bsat(Trm)

                ''====(2) Find b'p
                'Heat through the entire tube under steady-state

                Xp = thickTUBE

                Tpo = (Qr * Xp) / (kp * Apm * fs2wet) + Tpi
                ispo = iasat(Tpo)        '[J/kg] Saturation enthalpy tube outside

                bp = (ispo - ispi) / (Tpo - Tpi) 'Slope of saturation-enthalpy curve evaluated at Tpo and Tpi

                '====(3) Find b'w,p -  相對於鰭片基部水膜溫度Twp下飽和空氣焓值的斜率
                ' slope of the saturated air enthalpy fin base relative to the the water film temperature Twp 

                Twp = Tpo '{K} Fin base temperature, can equal the outer tube temperature 
                bwp = bsat(Twp) '[J/kg]

                '
                '====(4) Find b'w,m

                '-----------(1)-----Guess b'w,m
                bwm = bwp   '[J/kg] slope of saturated enthalpy relative to the mean water-film temperature, di/dTw,m
                Do



                    '-----------(2)-----calculate ho,w
                    how = (air.cp / (bwm * hco)) ^ -1 '[W/m²K] Heat transfer coefficient of wet coil 濕盤管的溼熱傳系數

                    '-----------(3)-----Wet fin efficiency

                    rc = d_c / 2                        'colar radius
                    req = ((Pt * Pl) / Math.PI) ^ 0.5 'Equivalent radius


                    Mw = Math.Sqrt((2 * how) / (kf * thickFIN))

                    MwP = Mw * (req ^ 2 - rc ^ 2) 'Mw(req-rc) parameter

                    Dim Mwri, Mwro As Double
                    Mwri = Mw * rc
                    Mwro = Mw * req


                    ' nwetf=(2*rc /MwP )*( (K1(Mwri)*I1w(Mwro )) / () )
                    'nwetf = (2 * rc / MwP) * ((k1(Mwri) * I1w(Mwro) - k1(Mwro) * I1w(Mwri)) / (k1(Mwro) * I0(Mwri) + K0(Mwri) * I1w(Mwro)))
                    nwetf = (2 * rc / MwP) * ((K1(Mwri) * I1w(Mwro) - K1(Mwro) * I1w(Mwri)) / (K1(Mwro) * I0(Mwri) + K0(Mwri) * I1w(Mwro)))
                    'Wet fin efficiency function from book Eq. 677, page 213


                    '-(4)-----Uo,w  Wet coil overal heat transfer coefficient

                    A_Uow = (br * Ao) / (hi * Api)
                    B_Uow = (bp * Xp * Ao) / (kp * Apm)
                    Af = Afin

                    C_Uow_box = (Apo / (bwp * Ao)) + (Af / Ao) * (nwetf / bwm)
                    C_Uow = 1 / (how * C_Uow_box)

                    Uow = 1 / (A_Uow + B_Uow + C_Uow) '[kg/m²s] from page 242 <---check units, must be [W/m²K]
                    '-(5)-----is,w,m, Mean Saturation-enthalpy of air at Tw,m

                    i = iam
                    A_iswm = (br / (hi * Api)) + (Xp * bp) / (kp * Apm)
                    braket = (1 - Uow * Ao * A_iswm)
                    iswm = i - nwetf * braket * (i - irm)


                    '-(6)-----Tw,m Mean wet coil temperature

                    Twm = TaSat(iswm) '[K] -Tw,m Mean wet coil temperature
                    ' Check(KtoC(Twm), "Twm")

                    '(7)-----compare bwm and iswm, iterate from (1) to (7)
                    bwm2 = bsat(Twm) 'Calculated bwm

                    delta_bwm = Math.Abs(bwm2 - bwm) 'diference from guessed to calculated bw,m
                    'set new value for bw,m=bw,m2
                    bwm = bwm2

                Loop Until (delta_bwm < 0.001)




                '_________________________________________________________________________________
                ' 6.________Calculate New Heat transfer Q = Uo,w * Ao * F * delta_im
                '_________________________________________________________________________________
                'Dim F, P, R As Double
                Dim P, R As Double

                P = (iro - iri) / (iai - iri)
                R = (iao - iai) / (iro - iri)
                If Math.Abs(iro - iri) < 0.1 Then
                    F = 1
                Else
                    F = 0.9
                End If

                Q2new = Uow * Ao * F * delta_im 'New calculated Q, WET COIL heat transfer rate


                '_________________________________________________________________________________
                ' 7.________Compare Qr and Q2new &
                '           xre2, Calculate new refrigerant exit quality 
                '________________________________________________________________________________

                delta_Q = Math.Abs(Qr - Q2new)
                ' If Qnew < Q. It indicates that the outlet refrigerant  state is below the saturation state, i.e.,
                'still in the two-phase region, therefore the exit quality, xe, must be calculated.
                ' 此現象表示冷媒出口狀態未達飽和, 而仍是在兩相區, 因此必須算冷媒出口的乾度xe.
                'First of all calculate a new (average) heat transfer, Qavg = Q + Qnew / 2
                '首先計算新的熱傳量, Qavg = Q + Qnew / 2


                Q3avg = (Qr + Q2new) / 2 'new average heat transfer
                xre2 = Q3avg / (mref * mix.ifg) + xri 'outlet regrigerant quality


                ' make a flag to make sure the state is in sat or sup.
                If Q2new > Qr Then
                    state = "Esup"
                    'MsgBox("goto superheated.Evap")
                    Exit Do
                End If


                If xre2 > 1 Then
                    MsgBox("xre>1")
                End If

                '_________________________________________________________________________________
                ' 8.________ xe-ITERATION,  Substitute new xre = xr2  
                '________________________________________________________________________________
                'set values for iteration
                xre = xre2 'substitute assumed xre=1 for calculated xre2
                'MsgBox(xre2)
                state = "Esat"

            End If


        Loop Until delta_Q < 0.001
        '-----------------DEBUG iterrated new values ----------------  <-------------------------------------INTERFACE

        'MsgBox(xre)
        'MsgBox("Stage One")

        If (state = "Esat") Then

            HPCD.Tcomp_in.Text = CDbl(HPCD.Tevap_sat.Text).ToString("0.###")
            HPCD.E_Q_sat.Text = Qr
            HPCD.E_Q_sup.Text = 0
            HPCD.E_Q_tot.Text = CDbl(HPCD.E_Q_sat.Text) + CDbl(HPCD.E_Q_sup.Text)


        ElseIf (state = "Esup") Then


            '======================================================================================================
            '-----------------------------Calculus X Fail ===> Guess Asat
            '======================================================================================================

            If xre >= 1 Then

                xre = 1
                '_________________________________________________________________________________

                ' 1.________Qr, Calculate  Heat transfer from refrigerant side,Qr= mr (xe - xi) * Ifg
                '_________________________________________________________________________________

                Qr = mref * (xre - xri) * mix.ifg 'Heat transfer calculated from Refrigerant side, asuming complete evaporation.i.e., xe=1

                A4 = Qr / (Uow * Ao * F * delta_im)

                'MsgBox(A4)

            End If

            'MsgBox(Qr)
            'MsgBox(Q2new)

            HPCD.E_Q_sat.Text = Qr.ToString("0.###")

            '------------------------------------
            '_________________________________________________________________________________
            ' 9.________ Dehumidification curve equation - 除濕曲線方程式
            '________________________________________________________________________________

            '-(1)----- Divide n-segments, fin enthalpy for each segment, delta_i
            Dim Tai, igt, W, W1, W2, i1, Wswm, nLe, delta_iLe As Double
            Dim Le, LeCO2, LeR410A, i2, Ta2, igt2 As Double

            '///////////////////// Initial Setup ///////////////////////
            iao = iao
            iai = MoistAirProperty.iai * 1000
            Wai = MoistAirProperty.W
            Tdry = MoistAirProperty.DBT + 273.15
            Pai = MoistAirProperty.Patm / 1000
            hi = hi
            Api = Api
            br = br
            Xp = Xp
            bp = bp
            kp = kp
            Apm = Apm
            i = iai
            irm = irm
            Uow = Uow
            Ao = Ao
            nwetf = nwetf

            '///////////////////// Testing ////////////////////////////
            'iao = 36248.59  'J/kg [p243,Cal Result]
            'iai = 55260.2   'J/kg [p234,Moist Air]
            'Wai = 0.0110835 'kg/kg-dry-air [p234,Moist Air] 
            'Tdry = 300.15   'K [p231 for given]
            'Pai = 0.101325  'MPa [p231,Cal Result]

            'hi = 4422.58    'W/m^2.K [p243,Cal Result]
            'Api = 0.20399   'm^2 [p234, Initial Area]
            'br = 2209.21    'J/kg.K [p239, br',bp',bwp',bwm']

            'Xp = 0.00026    'm [p240, geometry basic]
            'bp = 2345.06    'J/kg.K [p239, br',bp',bwp',bwm']
            'kp = 387        'W/m.K [p240, geometry basic]
            'Apm = 0.21466   'm^2 [p234, Initial Area]

            'i = 55260.2     'J/kg [~ iai ]
            'irm = 23135.5   'J/kg [p238 ]

            'Uow = 0.02452   'kg/m^2.s [p243,Cal Result]
            'Ao = 5.82165    'm^2 [p234, Initial Area]

            'nwetf = 0.6613   'X [p243,Cal Result]

            '///////////////////// Testing ////////////////////////////

            nLe = 40        ' number of segments, 40 is finite enough
            delta_iLe = (iao - iai) / nLe

            W1 = Wai        '[kg-water-vapor/kg-dry-air]
            i1 = iai        'air inlet enthalpy
            W = Wai
            'nLe = 19 '<----------------------------------------------------DEBUG Temp, enough number of iterations, delete after writing humidity ratio for water
            Dim firstTime As Boolean = True



            For i = 0 To nLe Step 1


                If firstTime = True Then
                    '-(2)-----Find Wswm, humidity ration for water film
                    Tai = Tdry 'Water inlet temperature dry
                    '[error]vapor.Properties(Tdry, Pai)
                End If

                If firstTime = False Then
                    Tai = Ta2 'Water inlet temperature dry
                End If



                vapor.SatProp(Tai)
                igt = vapor.iG '[J/kg] water vapor enthalpy at Tai
                ' igt = 2676


                '-calculate iswm
                A_iswm = (br / (hi * Api)) + (Xp * bp) / (kp * Apm)
                braket = (1 - Uow * Ao * A_iswm)

                iswm = i1 - nwetf * braket * (i1 - irm)

                '[TODO] module P6-10 equations/////////////////////////////////
                '       [error] -> Twm = TwSat(iswm) 'water film temperature for correspondant Tw,m 
                '       [error] -> Wswm = waterHumidityRATIO(Twm, Pai)

                Twm = istoTs(iswm / 1000)  'oC
                Wswm = TstoWs(Twm)         'kg/kg dry-air

                '       Wswm = 0.0105237 '[kg-water-vapor/kg-dry-air]  <----------------------------------------------------DEBUG Temp

                '[TODO] /////////////////////////////////

                '-(3)----- Find W2

                LeCO2 = 0.94
                LeR410A = 0.847

                Le = LeR410A

                'Relative humidity of second segment, n=2
                W2 = W1 + delta_iLe / ((Le * ((i1 - iswm) / (W1 - Wswm))) + (igt - 2501000 * Le))

                i2 = i1 + delta_iLe ' new air inlet enthalpy
                'from i2 = Cp,a * Ta,2  +  W2*(2501 + 1.805 *Ta2)
                Ta2 = (i2 - 2501000 * W2) / (air.cp + 1805 * W2) ' [C] new Air temperature 
                Ta2 = CtoK(Ta2) ' [K] new Air temperature 

                vapor.SatProp(Ta2)

                igt2 = vapor.iG '[J/kg] water vapor enthalpy at Ta2


                '-(4)-----Ta,40-&-W40 -ITERATION. Calculate Air outlet Temperature and Relative humidity,  
                'set values for iteration
                igt = igt2 'now we move to the next segment, so entrance enthalpy for the next segment is set
                W1 = W2
                i1 = i2

                firstTime = False

            Next

            Tdry_out = KtoC(Ta2) '[C] Air outlet temperature
            Wair_out = W2        ' humidity ratio 

            DAP.RH_In = Get_RH_from_DBT_W(Tdry_out, Wair_out)
            HPCD.Dryer_RH_In_txt.Text = DAP.RH_In.ToString("0.###")

            'Tdry_out = KtoC(Ta2)
            'MsgBox(Tdry_out)

            E_Tair_sat_out = Tdry_out



            '======================================================================================================
            '-----------------------------PRESSURE DROP
            '======================================================================================================
            '======================================================================================================


            '_________________________________________________________________________________
            ' 1.________ air - Dehumidification curve equation - 除濕曲線方程式
            '________________________________________________________________________________
            Dim Tdry_in, rho1, rho2, rhom, fam, delta_Pa As Double
            Tdry_in = Tdry

            fam = 0.1141 'Friction factor [] <----------------------------------------------------DEBUG Temp -Calculate from Eq-6-89

            rho1 = air._rho(Tdry_in, Pai) '[kg/m³] inlet air  density
            rho2 = air._rho(Ta2, Pai) '[kg/m³] oulet air  density
            rhom = (rho1 + rho2) / 2    '[kg/m³] Mean air density

            Dim A_P, B_P, C_P, D_P As Double
            A_P = (1 - contractionR ^ 2) / rho1
            B_P = (fam / rhom) * (Ao / Ac)
            C_P = (1 / rho2) - (1 / rho1)
            D_P = (1 - contractionR ^ 2) / rho2
            delta_Pa = ((Gc ^ 2) / 2) * (A_P + B_P + 2 * C_P - D_P)

            '_________________________________________________________________________________
            ' 2.________ Refrigerant  Pressure drop 
            '________________________________________________________________________________


            Dim L2, Xtt As Double
            L2 = E_W * Ntube * Nrows * fs2wet / Ncr ' Total length of tube


            xravg = (xri + xre2) / 2 'New average refrigerant quality

            '---(1) Martinelli parameter
            Xtt = (mix.rhoG / mix.rhoL) ^ 0.5 * (mix.viscL / mix.viscG) ^ 0.125 * ((1 - xravg) / xravg) ^ 0.875
            '  MsgBox(Xtt)

            '_______(2) two-phase average Reynolds Number
            Dim ReL, ReG, GL, C_coff, alpha2 As Double

            ReL = (Gr * (1 - xravg) * d_i) / mix.viscL 'Liquid Reynolds number
            GL = Gr * (1 - xravg) 'Liquid mass flux

            ReG = (Gr * xravg * d_i) / (mix.viscG) ' Gas-portion Reynolds number
            ' Xtt = 0.159612 '<----------------------------------------------------DEBUG Temp

            C_coff = C_Chisholm(ReL, ReG) 'Chisholm NUmber
            alpha2 = 1 + C_coff / Xtt + 1 / (Xtt ^ 2) ' Alpha squared number

            '---(3) 'single-phase Liquid Pressure drop
            Dim fL, Pdrop_rL As Double
            fL = 0.0791 * ReL ^ -0.25
            Pdrop_rL = ((4 * L2) / d_i) * fL * ((GL ^ 2) / (2 * mix.rhoL)) '[Pa] single-phase Liquid Pressure drop

            '---(4) Friction Pressure Drop
            Dim pdrop_rF As Double
            pdrop_rF = alpha2 * Pdrop_rL '[Pa] 


            '_________________________________________________________________________________
            ' 2.________ Momentum change pressure drop 
            '________________________________________________________________________________
            Dim void_ri, void_ro, momRo, momRi, pdrop_rMom As Double
            ' ---(1) Lochard and Martinelli void fractions
            void_ri = voidFraction(xri, mix.rhoL, mix.rhoG, mix.viscL, mix.viscG) '[] refrigerant output void fraction
            void_ro = voidFraction(xre2, mix.rhoL, mix.rhoG, mix.viscL, mix.viscG) '[] refrigerant output void fraction
            '---(2) Momentum change 
            momRo = pdrop_momentum(Gr, xre2, mix.rhoG, mix.rhoL, void_ro) '[Pa] Regrigerant output Momentum change

            momRi = pdrop_momentum(Gr, xri, mix.rhoG, mix.rhoL, void_ri) '[Pa] Regrigerant input Momentum pressure change

            pdrop_rMom = momRo - momRi ' [Pa] Momentum Change pressure drop



            '_________________________________________________________________________________
            ' 3.________ Total two-phase region  pressure drop 
            '________________________________________________________________________________
            Dim Pdrop_r As Double
            Pdrop_r = pdrop_rF + pdrop_rMom ' [Pa] Total regrigrant Pdrop = friction + Momentum
            Pdrop_r = pdrop_rF + momRi '<------------------temporary the one above is the right one

            'Check(Pdrop_r, "Pdrop_r")
            Tro = KtoC(Tro)



            ' MsgBox("Stage Two")
            '====================================================================================================================================================================

            '-----------------------------If xre  = 1  Asup = 1 - Asat  => Calculate TAsup

            '====================================================================================================================================================================

            'Dim R5, L5 As Double
            'Dim G5, ReG5, f_5, Nu5, hi5 As Double
            'Dim Visc5, kG5, Pr5, Cp5 As Double



            If xre = 1 Then
                A5 = 1 - A4


                '[1] calculate ho 
                Dim E_air As New Fluid("air", "si", "tp")
                E_air.Properties(CtoK(CDbl(HPCD.E_DBT_txt.Text)), 0.101325)


                Dim Vfr, Redc, Lhsup, Lpsup, EJ5, EJ6, EJ7, EJ8, E_j, Dh, ho, Vc As Double
                Vfr = mair / E_air.rho / Ac '1.5 'm/s
                Redc = E_air.rho * Vfr * d_c / E_air.Visc / contractionR
                Lhsup = 0.005 'm
                Lpsup = 0.0025 'm
                Dh = 4 * Ac * Pl * Nrows / Ao

                EJ5 = -0.6027 + 0.02593 * (Pl / Dh) ^ 0.52 * (Nrows) ^ -0.5 * Math.Log(Lhsup / Lpsup)
                EJ6 = -0.4776 + 0.40774 * (Nrows ^ 0.7 / (Math.Log(Redc) - 4.4))
                EJ7 = -0.58655 * (Fp / Dh) ^ 2.3 * (Pl / Pt) ^ -1.6 * Nrows ^ -0.65
                EJ8 = 0.0814 * (Math.Log(Redc) - 3)
                E_j = 1.1373 * Redc ^ EJ5 * (Fp / Pl) ^ EJ6 * (Lhsup / Lpsup) ^ EJ7 * (Pl / Pt) ^ EJ8 * (Nrows) ^ 0.3545
                Vc = Vfr / contractionR
                ho = E_j / (E_air.Pr) ^ 1.5 * (E_air.rho * Vc * E_air.cp)

                '[2] calculate noho
                Dim m, XL, XM, r, reqr, phi, fin_E, Sur_E, df, noho As Double
                df = thickFIN 'm
                m = (2 * ho / kf / df) ^ 0.5
                XL = ((Pt / 2) ^ 2 + Pl ^ 2) ^ 0.5 / 2 'staggerd
                XM = Pt / 2
                r = d_c / 2
                reqr = 1.28 * XM / r * (XL / XM - 0.2) ^ 0.5
                phi = (reqr - 1) * (1 + 0.35 * Math.Log(reqr))
                fin_E = Math.Tanh(m * r * phi) / (m * r * phi)
                Sur_E = 1 - Af / Ao * (1 - fin_E)
                noho = Sur_E * ho

                '[3] calculate Tsup (Guess and iteration "range from Tri to Tain")
                Dim Tsup As Double
                Tsup = 300.15 'K

                Dim Lsup, Rsup, Qsup, QsupNew As Double
                Lsup = Tri
                Rsup = Tdry
                Tsup = (Lsup + Rsup) / 2

                While (1)

                    Dim E_sup As New Fluid(inFluid, "si", "tp")
                    E_sup.Properties(Tsup, mix.P)

                    'Dim Qsup, Taout, LMTD As Double
                    'Qsup = mref * (mix.CpG + E_sup.cp) / 2 * (Tsup - CDbl(Form1.Tevap_sat.Text))
                    'Taout = Tdry - Qsup / mair / E_air.cp

                    'Dim Thi, Tho, Tci, Tco As Double
                    'Thi = Tdry
                    'Tho = Taout
                    'Tci = CDbl(Form1.Tevap_sat.Text)
                    'Tco = Tsup

                    'LMTD = ((Thi - Tco) - (Tho - Tci)) / Math.Log((Thi - Tco) / (Tho - Tci))

                    '[4] Calculate hi
                    Dim Visc_avg, k_avg, Pr_avg As Double
                    Visc_avg = (mix.viscG + E_sup.Visc) / 2
                    k_avg = (mix.kG + E_sup.k) / 2
                    Pr_avg = (mix.PrG + E_sup.Pr) / 2

                    Dim Gsup, Resup, fri_sup, Nusup, hisup As Double
                    Gsup = mref / (Math.PI * d_i ^ 2 / 4)
                    Resup = Gsup * d_i / Visc_avg
                    fri_sup = (1.58 * Math.Log(Resup) - 3.28) ^ -2
                    Nusup = ((fri_sup / 2) * (Resup - 1000) * Pr_avg) / (1.07 + 12.7 * (fri_sup / 2) ^ 0.5 * (Pr_avg ^ (2 / 3) - 1))
                    hisup = Nusup * k_avg / d_i

                    '[5] LMTD to satisfy Qnew =Q
                    Dim UA, AoAi, Asup As Double
                    AoAi = Ao / Api
                    Asup = A5 * Ao
                    UA = (1 / noho / Asup + AoAi / hisup / Asup) ^ -1
                    'Fsup = 0.9
                    'QsupNew = UA * LMTD * Fsup

                    Dim Cmin, Ca, Cr, E, K, NTU, Cmax, Cstar, Qmax As Double
                    Ca = mair * E_air.cp
                    Cr = mref * (mix.CpG + E_sup.cp) / 2
                    Cmin = Math.Min(Ca, Cr)
                    Cmax = Math.Max(Ca, Cr)
                    Cstar = Cmin / Cmax
                    NTU = UA / Cmin


                    If Nrows = 4 Then
                        If Cmin = Ca Then
                            K = 1 - Math.Exp(-NTU / 4)
                            E = 1 / Cstar * (1 - Math.Exp(-4 * K * Cstar) * (1 + Cstar * K ^ 2 * (6 - 4 * K + K ^ 2) + 4 * Cstar ^ 2 * K ^ 4 * (2 - K) + 8 * Cstar ^ 3 * K ^ 6 / 3))
                        ElseIf Cmin = Cr Then
                            K = 1 - Math.Exp(-NTU * Cstar / 4)
                            E = 1 - Math.Exp(-4 * K / Cstar) * (1 + K ^ 2 * (6 - 4 * K + K ^ 2) / Cstar + 4 * K ^ 4 * (2 - K) / Cstar ^ 2 + 8 * K ^ 6 / 3 / Cstar ^ 3)
                        End If
                    End If

                    If Nrows > 4 Then
                        E = 1 - Math.Exp(NTU ^ 0.22 * (Math.Exp(-Cstar * NTU ^ 0.78) - 1) / Cstar)
                    End If

                    Qmax = Cr * (Tdry - Tri)
                    Qsup = E * Qmax
                    QsupNew = Cr * (Tsup - Tri)

                    If Qsup - QsupNew > 0.01 Then
                        Lsup = Tsup
                        Tsup = (Lsup + Rsup) / 2
                    ElseIf QsupNew - Qsup > 0.01 Then
                        Rsup = Tsup
                        Tsup = (Lsup + Rsup) / 2
                    End If

                    If Math.Abs(Qsup - QsupNew) < 0.01 Then Exit While

                End While

                'MsgBox(Tsup)
                'MsgBox(Qsup)
                'MsgBox(QsupNew)
                'MsgBox("ESup")

                HPCD.Tcomp_in.Text = Tsup.ToString("0.###")

                HPCD.E_Q_sup.Text = Qsup



                ' Air outlet Tempeature of Superheated region 
                air.Properties(CtoK(MoistAirProperty.DBT), MoistAirProperty.Patm / 1000)
                E_Tair_sup_out = MoistAirProperty.DBT - Qsup / air.cp / mair


                HPCD.E_Q_tot.Text = CDbl(HPCD.E_Q_sat.Text) + CDbl(HPCD.E_Q_sup.Text)

            End If

            E_Tair_out = E_Tair_sup_out * A5 + E_Tair_sat_out * A4 ' Air outlet temperature of Evap (oC)

        End If

    End Sub


End Module
