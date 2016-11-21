Module Comp

    Public Sub Comp()

        Dim Pdis, Psuc, Vcom, Ncom, T1, mri As Double
        Dim h1, h2s, h2, T2 As Double

        'C.Sat_Refrigerants
        Dim C_sat As New Fluid(Influid, "si", "tp")
        C_sat.SatProp(CDbl(Form1.CTsat.Text))

        Form1.Discharge_Pressure.Text = C_sat.P.ToString("0.###")
        Pdis = C_sat.P

        'E.Sat_Refrigerants
        Dim E_sat As New Fluid(Influid, "si", "tp")
        E_sat.SatProp(CDbl(Form1.ETsat.Text))

        Form1.Suction_Pressure.Text = E_sat.P.ToString("0.###")
        Psuc = E_sat.P

        'MsgBox("PL")
        'MsgBox(Psuc)

        ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Initial Guess (LowPressure_Superheated)
        'C.Sup_Refrigerants
        'T1 = 300 ' Initial Guess ===== for debug
        'T4_1.Text = T1
        T1 = CDbl(Form1.T4_1.Text)

        Dim E_sup As New Fluid(Influid, "si", "tp")
        E_sup.Properties(T1, E_sat.P)

        ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Calculation
        ' Get masss flow rate
        Form1.Volumetric_Efficiency.Text = (0.9207 - 0.0756 * (Pdis / Psuc) + 0.0018 * (Pdis / Psuc) ^ 2).ToString("0.###")
        Vcom = CDbl(Form1.SweptVolume.Text)
        'Ncom = CDbl(Form1.CompressorSpeed.Text) / 60
        'mri = CDbl(Form1.Volumetric_Efficiency.Text) * E_sup.rho * Vcom * Ncom
        'Form1.MFR.Text = mri.ToString("0.######")

        ' change to debug mode => initial setup
        mri = CDbl(Form1.MFR.Text)
        Ncom = mri * 60 / (CDbl(Form1.Volumetric_Efficiency.Text) * E_sup.rho * Vcom)


        'Get state2 (HighPressure_Superheated)
        Form1.Isentropic_Efficiency.Text = (-0.26 + 0.7952 * (Pdis / Psuc) - 0.2803 * (Pdis / Psuc) ^ 2 + 0.0414 * (Pdis / Psuc) ^ 3 - 0.0022 * (Pdis / Psuc) ^ 4).ToString("0.###")
        h1 = Entropy(Influid, "PT", "SI", Psuc, T1)
        h2s = Entropy(Influid, "PS", "SI", Pdis, h1)
        h2 = h1 - (h1 - h2s) / CDbl(Form1.Isentropic_Efficiency.Text)
        T2 = Temperature(Influid, "PS", "SI", Pdis, h2)

        Form1.T1_2.Text = T2.ToString("0.###")

        Dim targetT2 As Double
        targetT2 = 365.15

        Dim C_sup As New Fluid(Influid, "si", "tp")
        C_sup.Properties(targetT2, C_sat.P)

        Form1.TextBox11.Text = (mri * (C_sup.i - E_sup.i)).ToString("0.###")

    End Sub

End Module
