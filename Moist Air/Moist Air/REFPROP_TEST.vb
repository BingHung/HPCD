Module REFPROP_TEST

    Public Sub R_TEST()

        MoistAirProperty.DBT = 27

        Dim SatWater As New Fluid("water", "si", "tp")
        SatWater.SatProp(CtoK(MoistAirProperty.DBT))

        MoistAirProperty.Ps = SatWater.P * 1000
        Console.WriteLine(MoistAirProperty.Ps)
        Console.Read()

    End Sub


End Module
