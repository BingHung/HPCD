Module EvaporatorModel

    Public Sub Evap_Goemetry_Init()

        ' // Evap_Geometry_Initialization
        E_W = CDbl(HPCD.E_W_txt.Text)        '[m] HX width
        E_H = CDbl(HPCD.E_H_txt.Text)        '[m] HX height
        Nrows = CDbl(HPCD.E_N_txt.Text)      'Number of rows
        Ncr = CDbl(HPCD.E_Ncr_txt.Text)        'Number of passes 冷媒迴路數
        Fp = CDbl(HPCD.E_FP_txt.Text)         '[m] Fin pitch (i.e, distance between fins)
        thickFIN = CDbl(HPCD.E_df_txt.Text)   '[m] Fin thickness
        kf = CDbl(HPCD.E_kf_txt.Text)         '[W/m⋅K] Fin thermal conductivity
        Pt = CDbl(HPCD.E_pt_txt.Text)         '[m] Transversal fin pitch (i.e., normal to the flow)
        Pl = CDbl(HPCD.E_Pl_txt.Text)         '[m] Longitudinal fin pitch (i.e., Parallel to the flow)
        d_o = CDbl(HPCD.E_do_txt.Text)        '[m] Tube outer diameter     
        thickTUBE = CDbl(HPCD.E_Xp_txt.Text)  '[m] Tube thickness
        kp = CDbl(HPCD.E_kp_txt.Text)         ' [W/m⋅K] tube thermal conductivity, for brass[銅管]

        Ntube = E_H / Pt                         'Number of tubes per row
        HPCD.E_Ntube_txt.Text = Ntube

        ' // Evap_Geometry_Initialization
        d_c = d_o + 2 * thickFIN                        '[m] Collar diameter
        d_i = d_o - 2 * thickTUBE * 0.96                '[m] where 0.96 is the efficiency
        Afr = E_W * E_H                                 '[m²] Frontal flow area
        Nfins = Math.Ceiling(E_W / Fp)                  'Number of fins
        Ac = Afr - Ntube * (d_c * E_W + Nfins * thickFIN * (Pt - d_c))             '[m²] Minimun free flow Area
        contractionR = Ac / Afr                                                    'Contraction Ratio
        Apo = Math.PI * d_c * (E_W - Nfins * thickFIN) * Ntube * Nrows             '[m²] Outside tube surface area
        Afin = 2 * Nfins * (Pl * E_H - 0.25 * Math.PI * d_c ^ 2 * Ntube) * Nrows + 2 * thickFIN * Nfins * (E_H + Pl * Nrows)       '[m²] Fin surface Area
        Ao = Apo + Afin                                 '[m²] Total Outside heat transfer area
        Api = Math.PI * d_i * E_W * Ntube * Nrows       '[m²] Total inside heat transfer area
        Apm = (d_c - d_i) / (Math.Log(d_c / d_i)) * Math.PI * E_W * Ntube * Nrows             '[m²] mean tube area


    End Sub

    Public Sub Evap_AirSide()


        ' [ Patm ] Abosulte Pressure (kPa)
        ' [ Ra ] gas constant (J/kg.K)

        'Patm = 101.325
        'Ra = 287.055
        'DBT = CDbl(HPCD.Tevap_ain_txt.Text)
        'RH = CDbl(HPCD.RH_txt.Text)

        'Dim SatWater As New Fluid("water", "si", "tp")
        'SatWater.SatProp(DBT)

        'Ps = SatWater.P * 1000
        'Pv = Ps * RH
        'W = 0.62198 * Pv / (Patm - Pv)
        'a = Math.Log(Pv)
        'Dew = 6.54 + 14.526 * a + 0.7398 * a ^ 2 + 0.09486 * a ^ 3 + 0.4569 * Pv ^ 0.1984
        'rhoai = Patm * 1000 / (Ra * CtoK(DBT) * (1 + 1.6078 * W))
        'iai = 1.006 * DBT + W * (2501 + 1.805 * DBT)

        'Dim Air As New Fluid("air", "si", "tp")
        'Air.Properties(DBT, 0.101325)

        'Cpa = Air.cp        'J/kg.K
        'Visca = Air.Visc    'N.s/m^2
        'Pra = Air.Pr        'X

        ''# WBT iteration => To Get WBT
        'Dim L, R As Double

        'R = DBT
        'L = 273.15
        'WBT = (R + L) / 2

        'While (1)

        '    hg1 = SatWater.iG / 1000        'kJ/kg

        '    'WBT = 30

        '    Dim WBSatWater As New Fluid("water", "si", "tp")
        '    WBSatWater.SatProp(WBT)

        '    WBT = KtoC(WBT)
        '    Pg2 = WBSatWater.P * 1000       'kPa
        '    W2 = 0.62198 * Pg2 / (Patm - Pg2)
        '    hfg2 = WBSatWater.ifg / 1000    'kJ/kg
        '    hf2 = WBSatWater.iL / 1000      'kJ/kg


        '    W1 = (Cpa / 1000 * (WBT - DBT) + W2 * hfg2) / (hg1 - hf2)


        '    If Math.Abs(W - W1) < 0.0001 Then
        '        Console.WriteLine("WBT : {0}", WBT)
        '        Exit While
        '    ElseIf (W - W1) > 0.0001 Then
        '        L = CtoK(WBT)
        '        WBT = (R + L) / 2
        '    ElseIf (W1 - W) > 0.0001 Then
        '        R = CtoK(WBT)
        '        WBT = (R + L) / 2
        '    End If

        'End While


        ''-Air'
        'Tdry = DBT
        'Twet = WBT
        'Pai = Patm / 1000
        'RH = RH
        'mair = CDbl(HPCD.E_mair_txt.Text) '[kg/s]
        'Tdew = Dew                      '[K] Dew point temperature
        'Wai = W                          '[kg-water-vapor/kg-dry-air]


        'Dim outFluid As String

        'outFluid = "air"
        '' outFluid = "nitrogen;7812;argon;0092;oxygen;2096"

        'Dim vapor As New Fluid("water", "SI", "TP")

        'Dim Rea, Gc As Double
        'Gc = mair / Ac                      ' [kg/m²s] Mass Flux maximum (i.e, at minimum free flow area, Ac)
        'Rea = Gc * d_c / Air.Visc           ' [] Reynolds number of air
        'Dim ja, fa, eFiningF, hco As Double
        'eFiningF = Ao / Apo
        '' ja = 0.4 * Rea ^ (-0.468 + 0.04076 * Nrows) * (eFiningF ^ 0.159) * Nrows ^ -1.261   '[dimensionless] Colburn j factor for air, wet - Plain fins
        '' hco = (ja * Gc * air.cp) * air.Pr ^ (-2 / 3) 'wet heat transfer coefficient
        'fa = 28.209 * (Rea ^ -0.5653) * Nrows ^ -0.1026 * ((Fp / d_c) ^ -1.3405) * eFiningF ^ -1.3343
        ''Dim j1 As Double '<............................................................................Debug
        ''j1 = 0.3745 - 1.554 * (Fp / d_c) ^ 0.24 * (Pl / Pt) ^ 0.12 * Nrows ^ -0.19
        ''ja = 19.36 * Rea ^ j1 * (Fp / d_c) ^ 1.352 * (Pl / Pt) ^ 0.6795 * Nrows ^ -1.291 ' for plain fin
        'Dim jl1, jl2, jl3 As Double
        'Dim Lp As Double
        ''Dim angle As Double
        'Lp = 0.0025
        'jl1 = -0.023634 - 1.2475 * (Fp / d_c) ^ 0.65 * (Pl / Pt) ^ 0.2 * Nrows ^ -0.18
        'jl2 = 0.856 * Math.Exp(0.36397)
        'jl3 = 0.25 * Math.Log(Rea)
        'ja = 9.717 * Rea ^ jl1 * (Fp / d_c) ^ jl2 * (Pl / Pt) ^ jl3 * Math.Log(3 - Lp / Fp) ^ 0.07162 * Nrows ^ -0.543 ' for louver fin P224

        'hco = (ja * Gc * Air.cp) * Air.Pr ^ (-2 / 3) 'wet heat transfer coefficient<............................................................................Debug


    End Sub

    Public Sub Evap_Iteration()


        ' [ Patm ] Abosulte Pressure (kPa)
        ' [ Ra ] gas constant (J/kg.K)

        Dim Ax, iaix As Double

        Patm = 101.325
        Ra = 287.055
        DBT = CDbl(HPCD.Tevap_ain_txt.Text)
        RH = CDbl(HPCD.RH_txt.Text)

        Dim SatWater As New Fluid("water", "si", "tp")
        SatWater.SatProp(DBT)

        Ps = SatWater.P * 1000
        Pv = Ps * RH
        W = 0.62198 * Pv / (Patm - Pv)
        Ax = Math.Log(Pv)
        Dew = 6.54 + 14.526 * Ax + 0.7398 * Ax ^ 2 + 0.09486 * Ax ^ 3 + 0.4569 * Pv ^ 0.1984
        rhoai = Patm * 1000 / (Ra * CtoK(DBT) * (1 + 1.6078 * W))
        iaix = 1.006 * DBT + W * (2501 + 1.805 * DBT)

        Dim Air As New Fluid("air", "si", "tp")
        Air.Properties(DBT, 0.101325)

        Cpa = Air.cp        'J/kg.K
        Visca = Air.Visc    'N.s/m^2
        Pra = Air.Pr        'X

        '# WBT iteration => To Get WBT
        Dim L, R As Double

        R = DBT
        L = 273.15
        WBT = (R + L) / 2

        While (1)

            hg1 = SatWater.iG / 1000        'kJ/kg

            'WBT = 30

            Dim WBSatWater As New Fluid("water", "si", "tp")
            WBSatWater.SatProp(WBT)

            WBT = KtoC(WBT)
            Pg2 = WBSatWater.P * 1000       'kPa
            W2 = 0.62198 * Pg2 / (Patm - Pg2)
            hfg2 = WBSatWater.ifg / 1000    'kJ/kg
            hf2 = WBSatWater.iL / 1000      'kJ/kg


            W1 = (Cpa / 1000 * (WBT - DBT) + W2 * hfg2) / (hg1 - hf2)


            If Math.Abs(W - W1) < 0.0001 Then
                Console.WriteLine("WBT : {0}", WBT)
                Exit While
            ElseIf (W - W1) > 0.0001 Then
                L = CtoK(WBT)
                WBT = (R + L) / 2
            ElseIf (W1 - W) > 0.0001 Then
                R = CtoK(WBT)
                WBT = (R + L) / 2
            End If

        End While


        '-Air'
        Tdry = DBT
        Twet = WBT
        Pai = Patm / 1000
        RH = RH
        mair = CDbl(HPCD.TextBox3.Text) '[kg/s]
        Tdew = Dew                      '[K] Dew point temperature
        Wai = W                          '[kg-water-vapor/kg-dry-air]


        Dim outFluid As String

        outFluid = "air"
        ' outFluid = "nitrogen;7812;argon;0092;oxygen;2096"

        Dim vapor As New Fluid("water", "SI", "TP")

        Dim Rea, Gc As Double
        Gc = mair / Ac                      ' [kg/m²s] Mass Flux maximum (i.e, at minimum free flow area, Ac)
        Rea = Gc * d_c / Air.Visc           ' [] Reynolds number of air
        Dim ja, fa, eFiningF, hco As Double
        eFiningF = Ao / Apo
        ' ja = 0.4 * Rea ^ (-0.468 + 0.04076 * Nrows) * (eFiningF ^ 0.159) * Nrows ^ -1.261   '[dimensionless] Colburn j factor for air, wet - Plain fins
        ' hco = (ja * Gc * air.cp) * air.Pr ^ (-2 / 3) 'wet heat transfer coefficient
        fa = 28.209 * (Rea ^ -0.5653) * Nrows ^ -0.1026 * ((Fp / d_c) ^ -1.3405) * eFiningF ^ -1.3343
        'Dim j1 As Double '<............................................................................Debug
        'j1 = 0.3745 - 1.554 * (Fp / d_c) ^ 0.24 * (Pl / Pt) ^ 0.12 * Nrows ^ -0.19
        'ja = 19.36 * Rea ^ j1 * (Fp / d_c) ^ 1.352 * (Pl / Pt) ^ 0.6795 * Nrows ^ -1.291 ' for plain fin
        Dim jl1, jl2, jl3 As Double
        Dim Lp As Double
        'Dim angle As Double
        Lp = 0.0025
        jl1 = -0.023634 - 1.2475 * (Fp / d_c) ^ 0.65 * (Pl / Pt) ^ 0.2 * Nrows ^ -0.18
        jl2 = 0.856 * Math.Exp(0.36397)
        jl3 = 0.25 * Math.Log(Rea)
        ja = 9.717 * Rea ^ jl1 * (Fp / d_c) ^ jl2 * (Pl / Pt) ^ jl3 * Math.Log(3 - Lp / Fp) ^ 0.07162 * Nrows ^ -0.543 ' for louver fin P224

        hco = (ja * Gc * Air.cp) * Air.Pr ^ (-2 / 3) 'wet heat transfer coefficient<............................................................................Debug



        HPCD.Qevap_sat_txt.Text = ""
        HPCD.Qevap_sup_txt.Text = ""
        HPCD.Qevap_tot_txt.Text = ""

        Tri = CDbl(HPCD.Tevap_sat_txt.Text)     '[K] Saturation temperature of refrigerant
        xri = CDbl(HPCD.Xevap_rin_txt.Text)     'Quality of refrigerant
        mref = CDbl(HPCD.Comp_MFR_txt.Text)     '[kg/s]

        Dim mix As New Fluid(Influid, "SI", "TP")
        Prsat = mix.Psat(Tri)   'Regrigerant Psat corresponding to Tsat
        mix.SatProp(Tri)

        '=> Calculation procedure ////////////////////////////////////////////////////////////////////
        ' Need to handle !!!
        ' Saturation :  Asat.evap (goto Sup) / Xsat.Evap
        ' Super heated region  : Tsup.evap

        '# check xre = 1 , => sat , or  sup  , if sat (get xre) , if sup (get Tsup)

        Dim xravg, Gr, ReiL, hiL As Double

        xre = 1                                         'guess exit quality (i.e, completely evaporated)
        xravg = (xri + xre) / 2                         'average refrigerant quality
        Gr = mref / ((0.25 * Math.PI * d_i ^ 2) * Ncr)  ' [kg/m²s] mass flux of refrigerant inside tube
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
                    nwetf = (2 * rc / MwP) * ((k1(Mwri) * I1w(Mwro) - k1(Mwro) * I1w(Mwri)) / (k1(Mwro) * I0(Mwri) + K0(Mwri) * I1w(Mwro)))
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

    End Sub

End Module
