Public Module PublicFunctions
    Public Function createRefpropName(ByVal name) As String
        Dim refpropName As String = ""
        If name = "Ar" Then refpropName = "argon" : Return refpropName
        If name = "N2" Then refpropName = "nitrogen" : Return refpropName
        If name = "O2" Then refpropName = "oxygen" : Return refpropName
        If name = "H2" Then refpropName = "hydrogen" : Return refpropName
        If name = "Water" Or name = "CO2" Or name = "Air" Or name = "CO" Then
            refpropName = name
        End If
        Return refpropName
    End Function

    Function xQuality(ByVal name As String, ByVal T As Double, ByVal P As Double)
        Dim x, h, hl, hg As Double  'where x is the quality and the h is the enthalphy
        h = RefProp.Enthalpy(name, "TP", "SI", T, P)
        hl = RefProp.Enthalpy(name, "Tliq", "SI", T)
        hg = RefProp.Enthalpy(name, "Tvap", "SI", T)

        If h <= hl Then
            Return 0
        ElseIf h >= hg Then
            Return 1
        Else
            x = (h - hl) / (hg - hl)
        End If
        Return x
    End Function


    Public Function CtoK(ByVal Celcius As Double)
        Return Celcius + 273.15
    End Function
    Public Function KtoC(ByVal Kelvin As Double)
        Return Kelvin - 273.15
    End Function

    Function PatoMPa(ByVal Pa As Double) As Double
        Return Pa / 1000000
    End Function
    Function is_Turbulent(ByVal Re As Double) As Boolean '
        'Returns boolean
        'True: Turbulent Flow
        'False: Laminar
        If Re > 4000 Then
            Return True
        Else
            Return False
        End If

    End Function

    Function Percentage(ByVal value As Double, total As Double) As Double
        Return MsgBox((value * 100) / total)
    End Function
    Function error_func(ByVal value As Double, total As Double) As Double
        Return MsgBox(100 - (value * 100) / total)
    End Function

    Function Check(ByVal value As Double, ByVal name As String)
        Return MsgBox(name & " : " & value)

    End Function

    Public Function mmTOm(ByVal mm As Double) As Double
        Return mm / 1000 '[m]
    End Function
    Public Function kPaTOMPa(ByVal kPa As Double) As Double
        Return kPa / 1000
    End Function
    Public Function kpaTOPa(ByVal kPa As Double) As Double
        Return kPa * 1000
    End Function
    Public Function MPaTOPa(ByVal MPa As Double) As Double
        Return MPa * 1000000
    End Function

    Function bsat(ByVal Tsat As Double) As Double
        Dim x = KtoC(Tsat)

        Dim a, b, c, d, e, f, g, h As Double
        a = 7.8054
        b = 1.0903
        c = 0.0388
        d = 0.0027
        e = -0.000025391
        f = -0.000002237
        g = 0.000000025527
        h = 0.000000000829
        'Return b  +2 * c * x + 3 * d * x ^ 2 + 4 * e * x ^ 3 + 5 * f * x ^ 4 + 6 * g * x ^ 5 + 7 * h * x ^ 6

        Return -0.0000004 * x ^ 6 + 0.000009 * x ^ 5 + 0.0016 * x ^ 4 - 0.0021 * x ^ 3 + 0.6148 * x ^ 2 + 53.146 * x + 1747.4
    End Function
    Function iasat(ByVal Tsat As Double) 'Figure 6-10 (Page 215) saturation enthalpy of air or other fluid
        Dim x = KtoC(Tsat)

        Return 0.00008 * x ^ 5 + 0.0152 * x ^ 4 - 0.2592 * x ^ 3 + 41.697 * x ^ 2 + 1494.5 * x + 9850.7

    End Function
    Function TaSat(ByVal iam As Double) As Double

        Dim x As Double = iam
        'return [Celcius T]
        Dim y As Double
        y = -7.0E-32 * x ^ 6 + 1.0E-25 * x ^ 5 - 6.0E-20 * x ^ 4 + 0.00000000000002 * x ^ 3 - 0.000000005 * x ^ 2 + 0.0007 * x - 6.2496

        Return CtoK(y)

    End Function
    Function iamoist(ByVal Tsat As Double, ByVal Wai As Double) As Double 'Enthalpy of moist air
        Dim Cpa, Cpw, iwe, x As Double
        Tsat = KtoC(Tsat)
        Cpa = 1.006
        Cpw = 1.805 '1.84
        iwe = 2501 '[kJ/kg] Water evaporation hea/enthalpy, latent heat
        'x = Cpa * Tsat + Wai * Cpw * Tsat + iwe * Wai
        x = Cpa * Tsat + Wai * (iwe + Cpw * Tsat)
        Return x * 1000
    End Function


    Public Function TdewPoint(ByVal Tdry As Double, ByVal RH As Double) As Double
        '-45 °C < Tdry < 60 °C
        If RH > 0.6 Then
            Return Tdry - (1 - RH) / 0.05   '[°C]
        Else
            Dim a, b, c, dummy As Double
            a = 6.112   '[millibar]]
            b = 17.62
            c = 243.12  '[°C]
            dummy = Math.Log(RH) + b * Tdry / (c + Tdry)
            Return c * dummy / (b - dummy)  '[°C]
        End If
    End Function
    Public Function humidityRATIO(ByVal Tdry As Double, ByVal Patm As Double) As Double
        Dim Psw As Double
        Psw = (Math.Exp(77.345 + 0.0057 * Tdry - 7235 / Tdry)) / Tdry ^ 8.2 'maximum saturation pressure of water vapor

        Return 0.62198 * Psw / (Patm - Psw) '[] dimensionless ratio
    End Function
    Public Function DensityWaterVapor(ByVal Tdry As Double)
        Dim Psw As Double
        Psw = Math.Exp(77.345 + 0.0057 * Tdry - 7235 / Tdry) / Tdry ^ 8.2 'maximum saturation pressure of water vapor
        Return 0.0022 * Psw / Tdry  '[kg/m^3]
    End Function
    Public Function HL(ByVal ReL As Double, ByVal PrL As Double, kL As Double) As Double

        Return 0
    End Function

    Public Function hTP()


        Return 0
    End Function

    Public Function pdrop_momentum(ByVal Gr As Double, ByVal x As Double, ByVal rhoG As Double, ByVal rhoL As Double, ByVal voidFrac As Double)
        Return (Gr ^ 2) * ((x ^ 2 / (rhoG * voidFrac)) + ((1 - x) ^ 2 / (rhoL * (1 - voidFrac))))
    End Function


    Function voidFraction(ByVal x As Double, ByVal rhoL As Double, rhoG As Double, uL As Double, uG As Double)
        Dim Bb, n1, n2, n3 As Double
        'Lochard and Martinelli
        '   Coefficient as suggested by Lochard and Martinelli
        Bb = 0.28
        n1 = 0.64
        n2 = 0.36
        n3 = 0.07

        Return (1 + Bb * ((1 - x) / x) ^ n1 * (rhoG / rhoL) ^ n2 * (uL / uG) ^ n3) ^ -1
    End Function

    Function C_Chisholm(ByVal ReL As Double, ByVal ReG As Double) As Double
        'Use flow condition of each phase to determine C, i.e., Chisholm Number
        'Refer to Table 4-4. Page 132
        Dim L, G As Boolean
        L = is_Turbulent(ReL)
        G = is_Turbulent(ReG)

        If L And G Then
            Return 20
        ElseIf L Or G Then
            If G Then
                Return 12
            Else
                Return 10
            End If
        Else
            Return 5
        End If
    End Function

    Function waterHumidityRATIO(ByVal T As Double, ByVal P As Double)
        Return 0
    End Function

    Function TwSat(ByVal iwSat As Double)
        Dim x As Double = iwSat

        Return 2.0E-17 * x ^ 3 + 0.00000000002 * x ^ 2 + 0.0002 * x + 273.91
    End Function

    Sub KandlikarCoeffs(ByVal Co As Double, ByVal FrLo As Double)
        'Table 4-3 (Page.120) Kandlikar (1990)
        If Co < 0.65 Then
            C1 = 1.136
            C2 = -0.9
            C3 = 667.2
            C4 = 0.7
            If FrLo < 0.04 Then
                C5 = 0.3
            Else
                C5 = 0
            End If
        Else
            C1 = 0.6683
            C2 = -0.2
            C3 = 1058.0
            C4 = 0.7
            If FrLo < 0.04 Then
                C5 = 0.3
            Else
                C5 = 0
            End If
        End If
    End Sub

    '-----------------------------debuggin
    Sub here()
        MsgBox("Here")
    End Sub
    Public Sub MoistAir()
        '# Initial Set up 

        'Dim Patm, Ra As Double

        ' [ Patm ] Abosulte Pressure (kPa)
        ' [ Ra ] gas constant (J/kg.K)

        MoistAirProperty.Patm = CDbl(HPCD.E_Patm_txt.Text) * 1000 '101.325
        MoistAirProperty.Ra = 287.055

        'Dim RH, DBT, Ps, Pv As Double

        ' [ RH ] Relative Humidity (%)
        ' [ DBT ] Dry Ball Temperature (oC)
        ' [ Ps ] Sat Pressure of water , in DBT (kPa) 
        ' [ Pv ] Vapor Pressure (kPa)

        MoistAirProperty.RH = CDbl(HPCD.E_RH_txt.Text)
        MoistAirProperty.DBT = CDbl(HPCD.E_DBT_txt.Text)

        'Dim SatWater As New Fluid("water", "si", "tp")
        'SatWater.SatProp(CtoK(MoistAirProperty.DBT))

        MoistAirProperty.Ps = Pressure("WATER.FLD", "Tliq", "SI", CtoK(MoistAirProperty.DBT)) * 1000
        'Console.WriteLine("Sat Pressure : {0} kPa ", MoistAirProperty.Ps)

        MoistAirProperty.Pv = MoistAirProperty.Ps * MoistAirProperty.RH
        'Console.WriteLine("Vap Pressure : {0} kPa ", MoistAirProperty.Pv)

        'Dim Dew, W, rhoai, iai As Double
        Dim a As Double

        ' [ W ] Humidity Ratio (kg/kg dry air)  //6-7
        ' [ Dew ] Dew Point Temperature (oC)    //6-12
        ' [ rhoai ]                   (kg/m^3)  //6-16
        ' [ iai ]                     (kJ/kg)   //6-20

        MoistAirProperty.W = 0.62198 * MoistAirProperty.Pv / (MoistAirProperty.Patm - MoistAirProperty.Pv)
        'Console.WriteLine("Humidity Ratio  : {0} (kg/kg dry air) ", MoistAirProperty.W)

        a = Math.Log(MoistAirProperty.Pv)
        MoistAirProperty.Dew = 6.54 + 14.526 * a + 0.7398 * a ^ 2 + 0.09486 * a ^ 3 + 0.4569 * MoistAirProperty.Pv ^ 0.1984
        'Console.WriteLine("Dew Point Temperature  : {0} (oC) ", MoistAirProperty.Dew)

        MoistAirProperty.rhoai = MoistAirProperty.Patm * 1000 / (MoistAirProperty.Ra * CtoK(MoistAirProperty.DBT) * (1 + 1.6078 * MoistAirProperty.W))
        'Console.WriteLine("rhoai  : {0} (kg/m^3) ", MoistAirProperty.rhoai)

        MoistAirProperty.iai = 1.006 * MoistAirProperty.DBT + MoistAirProperty.W * (2501 + 1.805 * MoistAirProperty.DBT)
        'Console.WriteLine("iai  : {0} (kJ/kg) ", MoistAirProperty.iai)



        'Dim Cpa, Visca, Pra As Double

        Dim Air As New Fluid("air", "si", "tp")
        Air.Properties(CtoK(MoistAirProperty.DBT), 0.101325)

        MoistAirProperty.Cpa = Air.cp        'J/kg.K
        MoistAirProperty.Visca = Air.Visc    'N.s/m^2
        MoistAirProperty.Pra = Air.Pr        'X

        'Console.WriteLine("Cpa : {0} ; Visca : {1} ; Pra : {2}", MoistAirProperty.Cpa, MoistAirProperty.Visca, MoistAirProperty.Pra)

        '# WBT iteration => To get WBT

        Dim WBT As Double
        Dim W2, Pg2, hfg2, hf2 As Double
        Dim W1, hg1 As Double
        Dim L, R As Double
        R = CtoK(MoistAirProperty.DBT)
        L = 273.15
        WBT = (R + L) / 2

        While (1)

            'hg1 = SatWater.iG / 1000        'kJ/kg

            hg1 = Enthalpy("WATER.FLD", "Tvap", "SI", CtoK(MoistAirProperty.DBT))
            'WBT = 30

            Dim WBSatWater As New Fluid("WATER.FLD", "si", "tp")
            WBSatWater.SatProp(WBT)

            WBT = KtoC(WBT)
            Pg2 = WBSatWater.P * 1000       'kPa
            W2 = 0.62198 * Pg2 / (MoistAirProperty.Patm - Pg2)
            hfg2 = WBSatWater.ifg / 1000    'kJ/kg
            hf2 = WBSatWater.iL / 1000      'kJ/kg


            W1 = (MoistAirProperty.Cpa / 1000 * (WBT - MoistAirProperty.DBT) + W2 * hfg2) / (hg1 - hf2)


            If Math.Abs(MoistAirProperty.W - W1) < 0.0001 Then
                'Console.WriteLine("WBT : {0}", WBT)
                Exit While
            ElseIf (MoistAirProperty.W - W1) > 0.0001 Then
                L = CtoK(WBT)
                WBT = (R + L) / 2
            ElseIf (W1 - MoistAirProperty.W) > 0.0001 Then
                R = CtoK(WBT)
                WBT = (R + L) / 2
            End If

        End While

    End Sub


End Module
