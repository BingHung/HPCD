
Module Module1



    Sub Main()

        'R_TEST()
        '# Initial Set up 

        'Dim Patm, Ra As Double

        ' [ Patm ] Abosulte Pressure (kPa)
        ' [ Ra ] gas constant (J/kg.K)

        MoistAirProperty.Patm = 101.325
        MoistAirProperty.Ra = 287.055

        'Dim RH, DBT, Ps, Pv As Double

        ' [ RH ] Relative Humidity (%)
        ' [ DBT ] Dry Ball Temperature (oC)
        ' [ Ps ] Sat Pressure of water , in DBT (kPa) 
        ' [ Pv ] Vapor Pressure (kPa)

        MoistAirProperty.RH = 0.5
        MoistAirProperty.DBT = 25

        'Console.Write("Please Enter Dry Ball Temerature (oC) :  ")
        'MoistAirProperty.RH = Console.ReadLine()
        'Console.Write("Please Enter Moist Air Relative Humidity :  ")
        'MoistAirProperty.DBT = Console.ReadLine()


        Dim SatWater As New Fluid("water", "si", "tp")
        SatWater.SatProp(CtoK(MoistAirProperty.DBT))

        MoistAirProperty.Ps = SatWater.P * 1000
        Console.WriteLine("Sat Pressure : {0} kPa ", MoistAirProperty.Ps)

        MoistAirProperty.Pv = MoistAirProperty.Ps * MoistAirProperty.RH
        Console.WriteLine("Vap Pressure : {0} kPa ", MoistAirProperty.Pv)

        'Dim Dew, W, rhoai, iai As Double
        Dim a As Double

        ' [ W ] Humidity Ratio (kg/kg dry air)  //6-7
        ' [ Dew ] Dew Point Temperature (oC)    //6-12
        ' [ rhoai ]                   (kg/m^3)  //6-16
        ' [ iai ]                     (kJ/kg)   //6-20

        MoistAirProperty.W = 0.62198 * MoistAirProperty.Pv / (MoistAirProperty.Patm - MoistAirProperty.Pv)
        Console.WriteLine("Humidity Ratio  : {0} (kg/kg dry air) ", MoistAirProperty.W)

        a = Math.Log(MoistAirProperty.Pv)
        MoistAirProperty.Dew = 6.54 + 14.526 * a + 0.7398 * a ^ 2 + 0.09486 * a ^ 3 + 0.4569 * MoistAirProperty.Pv ^ 0.1984
        Console.WriteLine("Dew Point Temperature  : {0} (oC) ", MoistAirProperty.Dew)

        MoistAirProperty.rhoai = MoistAirProperty.Patm * 1000 / (MoistAirProperty.Ra * CtoK(MoistAirProperty.DBT) * (1 + 1.6078 * MoistAirProperty.W))
        Console.WriteLine("rhoai  : {0} (kg/m^3) ", MoistAirProperty.rhoai)

        MoistAirProperty.iai = 1.006 * MoistAirProperty.DBT + MoistAirProperty.W * (2501 + 1.805 * MoistAirProperty.DBT)
        Console.WriteLine("iai  : {0} (kJ/kg) ", MoistAirProperty.iai)



        'Dim Cpa, Visca, Pra As Double

        Dim Air As New Fluid("air", "si", "tp")
        Air.Properties(CtoK(MoistAirProperty.DBT), 0.101325)

        MoistAirProperty.Cpa = Air.cp        'J/kg.K
        MoistAirProperty.Visca = Air.Visc    'N.s/m^2
        MoistAirProperty.Pra = Air.Pr        'X

        Console.WriteLine("Cpa : {0} ; Visca : {1} ; Pra : {2}", MoistAirProperty.Cpa, MoistAirProperty.Visca, MoistAirProperty.Pra)

        '# WBT iteration => To get WBT

        Dim WBT As Double
        Dim W2, Pg2, hfg2, hf2 As Double
        Dim W1, hg1 As Double
        Dim L, R As Double
        R = CtoK(MoistAirProperty.DBT)
        L = 273.15
        WBT = (R + L) / 2

        While (1)

            hg1 = SatWater.iG / 1000        'kJ/kg

            'WBT = 30

            Dim WBSatWater As New Fluid("water.FLD", "si", "tp")
            WBSatWater.SatProp(WBT)

            WBT = KtoC(WBT)
            Pg2 = WBSatWater.P * 1000       'kPa
            W2 = 0.62198 * Pg2 / (MoistAirProperty.Patm - Pg2)
            hfg2 = WBSatWater.ifg / 1000    'kJ/kg
            hf2 = WBSatWater.iL / 1000      'kJ/kg


            W1 = (MoistAirProperty.Cpa / 1000 * (WBT - MoistAirProperty.DBT) + W2 * hfg2) / (hg1 - hf2)


            If Math.Abs(MoistAirProperty.W - W1) < 0.0001 Then
                Console.WriteLine("WBT : {0}", WBT)
                Exit While
            ElseIf (MoistAirProperty.W - W1) > 0.0001 Then
                L = CtoK(WBT)
                WBT = (R + L) / 2
            ElseIf (W1 - MoistAirProperty.W) > 0.0001 Then
                R = CtoK(WBT)
                WBT = (R + L) / 2
            End If

        End While

        Console.Read()

    End Sub

End Module
