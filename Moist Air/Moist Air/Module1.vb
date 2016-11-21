Module Module1



    Sub Main()

        '# Initial Set up 

        Dim Patm, Ra As Double

        ' [ Patm ] Abosulte Pressure (kPa)
        ' [ Ra ] gas constant (J/kg.K)

        Patm = 101.325
        Ra = 287.055

        Dim RH, DBT, Ps, Pv As Double

        ' [ RH ] Relative Humidity (%)
        ' [ DBT ] Dry Ball Temperature (oC)
        ' [ Ps ] Sat Pressure of water , in DBT (kPa) 
        ' [ Pv ] Vapor Pressure (kPa)

        RH = 0.5
        DBT = 27

        Dim SatWater As New Fluid("water", "si", "tp")
        SatWater.SatProp(CtoK(DBT))

        Ps = SatWater.P * 1000
        Console.WriteLine("Sat Pressure : {0} kPa ", Ps)

        Pv = Ps * RH
        Console.WriteLine("Vap Pressure : {0} kPa ", Pv)

        Dim Dew, W, rhoai, iai As Double
        Dim a As Double

        ' [ W ] Humidity Ratio (kg/kg dry air)  //6-7
        ' [ Dew ] Dew Point Temperature (oC)    //6-12
        ' [ rhoai ]                   (kg/m^3)  //6-16
        ' [ iai ]                     (kJ/kg)   //6-20

        W = 0.62198 * Pv / (Patm - Pv)
        Console.WriteLine("Humidity Ratio  : {0} (kg/kg dry air) ", W)

        a = Math.Log(Pv)
        Dew = 6.54 + 14.526 * a + 0.7398 * a ^ 2 + 0.09486 * a ^ 3 + 0.4569 * Pv ^ 0.1984
        Console.WriteLine("Dew Point Temperature  : {0} (oC) ", Dew)

        rhoai = Patm * 1000 / (Ra * CtoK(DBT) * (1 + 1.6078 * W))
        Console.WriteLine("rhoai  : {0} (kg/m^3) ", rhoai)

        iai = 1.006 * DBT + W * (2501 + 1.805 * DBT)
        Console.WriteLine("iai  : {0} (kJ/kg) ", iai)



        Dim Cpa, Visca, Pra As Double

        Dim Air As New Fluid("air", "si", "tp")
        Air.Properties(CtoK(DBT), 0.101325)

        Cpa = Air.cp        'J/kg.K
        Visca = Air.Visc    'N.s/m^2
        Pra = Air.Pr        'X

        Console.WriteLine("Cpa : {0} ; Visca : {1} ; Pra : {2}", Cpa, Visca, Pra)

        '# WBT iteration => To get WBT

        Dim W2, Pg2, hfg2, hf2, WBT As Double
        Dim W1, Ps1, Pg1, hg1 As Double
        Dim L, R As Double
        R = CtoK(DBT)
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

        Console.Read()

    End Sub

End Module
