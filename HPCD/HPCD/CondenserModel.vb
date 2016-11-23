Module CondenserModel

    Public Sub Cond_Goemetry_Init()

        ' // Cond_Geometry_Initialization
        C_W = CDbl(HPCD.C_W_txt.Text)
        C_H = CDbl(HPCD.C_H_txt.Text)
        C_N = CDbl(HPCD.C_N_txt.Text)
        C_df = CDbl(HPCD.C_df_txt.Text)
        C_Fp = CDbl(HPCD.C_Fp_txt.Text)
        C_Pt = CDbl(HPCD.C_Pt_txt.Text)
        C_Pl = CDbl(HPCD.C_Pl_txt.Text)
        C_dc = CDbl(HPCD.C_dc_txt.Text)
        C_dw = CDbl(HPCD.C_dw_txt.Text)
        C_di = CDbl(HPCD.C_di_txt.Text)
        C_kf = CDbl(HPCD.C_kf_txt.Text)

        ' // Cond_Geometry_Derived
        C_Afr = C_W * C_H
        C_NT = C_H / C_Pt
        C_NF = C_W / C_Fp
        C_Ac = C_Afr - C_NT * (C_dc * C_W + C_NF * C_df * (C_Pt - C_dc))
        C_sigma = C_Ac / C_Afr
        C_Af = 2 * C_NF * (C_Pl * C_H - Math.PI / 4 * C_dc * C_dc * C_NT) * C_N + 2 * C_df * C_NF * (C_H + C_Pl * C_N)
        C_At = Math.PI * C_dc * (C_W - C_NF * C_df) * C_NT * C_N
        C_Ao = C_Af + C_At
        C_Dh = 4 * C_Ac * C_Pl * C_N / C_Ao
        C_Ai = Math.PI * C_di * C_W * C_NT * C_N
        C_ratio = C_Ao / C_Ai

    End Sub

    Public Sub Cond_AirSide()

        C_Tain = CDbl(HPCD.Tcond_ain_txt.Text)
        C_Vfr = CDbl(HPCD.Cond_Vfr_txt.Text)

        'C.In_Air // declare air inlet propery (Tain(oC) -> (K), P (atmosphere pressure(MPa)) )
        Dim C_air As New Fluid("air", "si", "tp")
        C_air.Properties(C_Tain, 0.101325)

        C_rhoa = C_air.rho
        C_Visca = C_air.Visc
        C_Cpa = C_air.cp
        C_Pra = C_air.Pr

        '// Get the air-side heat tansfer coefficient
        C_Redc = C_rhoa * C_Vfr * C_dc / C_Visca / C_sigma
        C_j = Louver_Fin()

        C_Vc = C_Vfr / C_sigma                                                      '(m/s)
        C_ho = C_j * C_rhoa * C_Vc * C_Cpa / (C_Pra) ^ (2 / 3)                      '(W/m^2.K)
        C_m = (2 * C_ho / C_kf / C_df) ^ (0.5)                                      '(m^-1)

        '// Staggered and  Inline Tube Arrangement Issue
        C_XL = ((C_Pt / 2) ^ 2 + C_Pl ^ 2) ^ 0.5 / 2 'staggerd 'C_Pl / 2 Inline     '(m)
        C_XM = C_Pt / 2                                                             '(m)
        C_r = C_dc / 2                                                              '(m)
        C_reqr = 1.28 * C_XM * ((C_XL / C_XM) - 0.2) ^ (0.5) / C_r                  '(X)
        C_phi = (C_reqr - 1) * (1 + 0.35 * Math.Log(C_reqr))                        '(X)
        C_nf_efficiency = Math.Tanh(C_m * C_r * C_phi) / (C_m * C_r * C_phi)        '(X)
        C_no_efficiency = 1 - (C_Af / C_Ao) * (1 - C_nf_efficiency)                 '(X)
        C_noho = C_no_efficiency * C_ho                                             '(W/m^K)

    End Sub

    Function Louver_Fin() As Double

        Dim CJ5, CJ6, CJ7, CJ8, Lh, Lp As Double
        Lh = 0.005
        Lp = 0.0025
        CJ5 = -0.6027 + 0.02593 * (C_Pl / C_Dh) ^ 0.52 * (C_N) ^ -0.5 * Math.Log(Lh / Lp)
        CJ6 = -0.4776 + 0.40774 * (C_N ^ 0.7 / (Math.Log(C_Redc) - 4.4))
        CJ7 = -0.58655 * (C_Fp / C_Dh) ^ 2.3 * (C_Pl / C_Pt) ^ -1.6 * C_N ^ -0.65
        CJ8 = 0.0814 * (Math.Log(C_Redc) - 3)
        C_j = 1.1373 * C_Redc ^ CJ5 * (C_Fp / C_Pl) ^ CJ6 * (Lh / Lp) ^ CJ7 * (C_Pl / C_Pt) ^ CJ8 * (C_N) ^ 0.3545
        Return C_j

    End Function

    Dim Cond_State_Flag As String = "" '// define cond outlet state

    Public Sub Cond_Iteration()

        C_Ts = CDbl(HPCD.Tcond_sat_txt.Text)       '(K)
        C_Trin = CDbl(HPCD.Tcond_rin_txt.Text)     '(K)
        C_mr = CDbl(HPCD.Comp_MFR_txt.Text)        '(kg/s)

        'C.Sat_Refrigerants // declare condenser saturation property(Tsat(K))
        Dim C_sat As New Fluid(Influid, "si", "tp")
        C_sat.SatProp(C_Ts)

        C_PrsG = C_sat.PrG                      '(X)
        C_PrsL = C_sat.PrL                      '(X)
        C_rhosL = C_sat.rhoL                    '(kg/m^3)
        C_rhosG = C_sat.rhoG                    '(kg/m^3)
        C_ViscsL = C_sat.viscL                  '(N.s/m^2)
        C_ViscsG = C_sat.viscG                  '(N.s/m^2)
        C_CpsL = C_sat.CpL                      '(J/kg.K)
        C_CpsG = C_sat.CpG                      '(J/kg.K)
        C_ksL = C_sat.kL                        '(W/m.k)
        C_ksG = C_sat.kG                        '(W/m.k)
        C_Pr = 0.437                            '(reduce pressure)
        C_isLG = C_sat.ifg / 1000               '(kJ/kg)

        'C.Sup_Refrigerants // declare condenser superheated region property (Tsup(K) , P(Psat))
        Dim C_sup As New Fluid(Influid, "si", "tp")
        C_sup.Properties(C_Trin, C_sat.P)

        C_Cprin = C_sup.cp                      '(J/kg.K)
        C_rhorin = C_sup.rho                    '(kg/m^3)
        C_krin = C_sup.k                        '(W/m.k)
        C_Viscrin = C_sup.Visc                  '(N.s/m^2)
        C_Prin = C_sup.Pr                       '(X)


    End Sub



End Module
