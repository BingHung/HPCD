Module CondenserModel

    Public Sub Cond_Goemetry_Init()
        Dim Condenser As New Condensor_Init

        ' // Cond_Geometry_Initialization
        Condenser.C_W = CDbl(HPCD.C_H_txt.Text)
        Condenser.C_H = CDbl(HPCD.C_H_txt.Text)
        Condenser.C_N = CDbl(HPCD.C_N_txt.Text)
        Condenser.C_df = CDbl(HPCD.C_df_txt.Text)
        Condenser.C_Fp = CDbl(HPCD.C_Fp_txt.Text)
        Condenser.C_Pt = CDbl(HPCD.C_Pt_txt.Text)
        Condenser.C_Pl = CDbl(HPCD.C_Pl_txt.Text)
        Condenser.C_dc = CDbl(HPCD.C_dc_txt.Text)
        Condenser.C_dw = CDbl(HPCD.C_dw_txt.Text)
        Condenser.C_di = CDbl(HPCD.C_di_txt.Text)

        ' // Cond_Geometry_Derived
        Condenser.C_Afr = Condenser.C_W * Condenser.C_H
        Condenser.C_NT = Condenser.C_H / Condenser.C_Pt
        Condenser.C_NF = Condenser.C_W / Condenser.C_Fp
        Condenser.C_Ac = Condenser.C_Afr - Condenser.C_NT * (Condenser.C_dc * Condenser.C_W + Condenser.C_NF * Condenser.C_df * (Condenser.C_Pt - Condenser.C_dc))
        Condenser.C_sigma = Condenser.C_Ac / Condenser.C_Afr

        Condenser.C_Af = 2 * Condenser.C_NF * (Condenser.C_Pl * Condenser.C_H - Math.PI / 4 * Condenser.C_dc * Condenser.C_dc * Condenser.C_NT) * Condenser.C_N + 2 * Condenser.C_df * Condenser.C_NF * (Condenser.C_H + Condenser.C_Pl * Condenser.C_N)
        Condenser.C_At = 


        Condenser.C_ratio = Condenser.C_Ao / Condenser.C_Ai

    End Sub




End Module
