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

        Patm = 101.325
        Ra = 287.055
        DBT = CDbl(HPCD.Tevap_ain_txt.Text)
        RH = CDbl(HPCD.RH_txt.Text)

        Dim SatWater As New Fluid("water", "si", "tp")
        SatWater.SatProp(DBT)

        Ps = SatWater.P * 1000
        Pv = Ps * RH
        W = 0.62198 * Pv / (Patm - Pv)
        a = Math.Log(Pv)
        Dew = 6.54 + 14.526 * a + 0.7398 * a ^ 2 + 0.09486 * a ^ 3 + 0.4569 * Pv ^ 0.1984
        rhoai = Patm * 1000 / (Ra * CtoK(DBT) * (1 + 1.6078 * W))
        iai = 1.006 * DBT + W * (2501 + 1.805 * DBT)

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
        mair = CDbl(HPCD.E_mair_txt.Text) '[kg/s]
        Tdew = Dew                      '[K] Dew point temperature
        Wai = W                          '[kg-water-vapor/kg-dry-air]


    End Sub

End Module
