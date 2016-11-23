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

        Dim C_QA3 As Double = 0 '// [TODO]

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

        '# Calculation concept  =================================================================================

        'Super heated region -> Asup (& goto Saturated) / Tsup.out
        'Saturated region -> Asat (& goto Subcooled) / Xsat.out
        'Subcooled region -> Tsub.out

        '# Super Heated Calculation -> Asup / Tsup ===============================================================

        '[1] get hiA1 -> Get hi Property
        C_A1_CprG = (C_Cprin + C_CpsG) / 2
        C_QA1 = C_mr * C_A1_CprG * (C_Trin - C_Ts)
        C_A1_Cr = C_mr * C_A1_CprG
        C_ma = C_rhoa * C_Vfr * C_Afr
        C_CA = C_ma * C_Cpa

        C_A1_ViscG = (C_Viscrin + C_ViscsG) / 2
        C_A1_kG = (C_krin + C_ksG) / 2
        C_A1_PrG = (C_Prin + C_PrsG) / 2
        C_A1_G = C_mr / (Math.PI * C_di * C_di / 4)
        C_A1_ReG = C_A1_G * C_di / C_A1_ViscG
        C_A1_f = (1.58 * Math.Log(C_A1_ReG) - 3.28) ^ (-2)
        C_A1_Nu = ((C_A1_f / 2) * (C_A1_ReG - 1000) * C_A1_PrG) / (1.07 + 12.7 * (C_A1_f / 2) ^ 0.5 * (C_A1_PrG ^ (2 / 3) - 1))
        C_A1_hi = C_A1_Nu * C_A1_kG / C_di

        '[2] get A1* , A1 area
        Dim LMTD_A1, TAir_A1_out, dT1_A1, dT2_A1, UA_A1, F, A1 As Double
        F = 0.9
        TAir_A1_out = C_QA1 / (C_ma * C_Cpa) + C_Tain
        dT1_A1 = C_Trin - TAir_A1_out
        dT2_A1 = C_Ts - C_Tain
        LMTD_A1 = (dT1_A1 - dT2_A1) / Math.Log(dT1_A1 / dT2_A1)
        UA_A1 = C_QA1 / LMTD_A1 / F
        A1 = (1 / C_noho + C_ratio / C_A1_hi) * UA_A1 '// ......... Sup heated region End

        '# Saturation Calculation -> Asat / Xsat ===============================================================

        '[1] get hcm
        C_A2_hL = (C_ksL / C_di) * 0.023 * (C_A1_G * C_di / C_ViscsL) ^ 0.8 * C_PrsL ^ 0.4
        C_A2_hcm = C_A2_hL * (0.55 + 2.09 / (C_Pr ^ 0.38))

        '[2] condition 1 , check if it's totally condense
        C_QA2 = C_mr * C_isLG * 1000 ' 1000

        Dim LMTD_A2, TAir_A2_out, dT1_A2, dT2_A2, UA_A2, A2, A2_real As Double
        TAir_A2_out = C_QA2 / (C_ma * C_Cpa) + C_Tain
        dT1_A2 = C_Ts - TAir_A2_out
        dT2_A2 = C_Ts - C_Tain
        LMTD_A2 = (dT1_A2 - dT2_A2) / Math.Log(dT1_A2 / dT2_A2)
        UA_A2 = C_QA2 / LMTD_A2 / F
        A2 = (1 / C_noho + C_ratio / C_A2_hcm) * UA_A2
        A2_real = C_Ao - A1



        If A2 > A2_real Then
            'MsgBox("saturation region") ===============================================================================================
            State_A2 = 1

            Dim xL, xR As Double
            xL = 0
            xR = 1
            x = (xL + xR) / 2

            ' Guess x

            While (1)

                C_QA2 = C_mr * C_isLG * 1000 * (1 - x)
                TAir_A2_out = C_QA2 / (C_ma * C_Cpa) + C_Tain
                dT1_A2 = C_Ts - TAir_A2_out
                dT2_A2 = C_Ts - C_Tain
                LMTD_A2 = (dT1_A2 - dT2_A2) / Math.Log(dT1_A2 / dT2_A2)
                UA_A2 = C_QA2 / LMTD_A2 / F
                A2 = (1 / C_noho + C_ratio / C_A2_hcm) * UA_A2
                A2_real = C_Ao - A1

                If A2 - A2_real < 0.01 Then
                    xR = x
                    x = (xL + xR) / 2
                ElseIf A2_real - A2 < 0.01 Then
                    xL = x
                    x = (xL + xR) / 2
                End If

                If Math.Abs(A2 - A2_real) < 0.01 Then Exit While

            End While

            'MsgBox(x)
            HPCD.Xexpa_rin_txt.Text = x.ToString("0.###")   '// Set the Xsat of condenser
            HPCD.Teapa_rin_txt.Text = CDbl(HPCD.Tcond_sat_txt.Text).ToString("0.###") '// Set the Tcond.out = Tsat

        ElseIf A2 < A2_real Then
            'MsgBox("subcooled region") ===============================================================================================
            State_A2 = 2

            Dim A3, UA_A3, Q3_check As Double
            A3 = C_Ao - A1 - A2
            C_QA3 = 0

            '[1] get hiA3
            '[2] guess Trout 
            Dim C_sub As New Fluid("r22", "si", "tp")

            Dim TL, TR As Double
            TL = C_Tain
            TR = KtoC(CDbl(HPCD.Tcond_sat_txt.Text))
            C_Tout = (TL + TR) / 2

            While (1)

                C_sub.Properties(CtoK(C_Tout), C_sat.P)
                ' calculation hi
                '=======================================================================================================
                C_A3_ViscL = (C_sat.viscL + C_sub.Visc) / 2
                C_A3_kL = (C_sat.kL + C_sub.k) / 2
                C_A3_PrL = (C_sat.PrL + C_sub.Pr) / 2
                C_A3_CpL = (C_sat.CpL + C_sub.cp) / 2

                C_A3_G = C_mr * 4 / (Math.PI * C_di ^ 2)
                C_A3_ReG = C_A3_G * C_di / C_A3_ViscL
                C_A3_f = (1.58 * Math.Log(C_A3_ReG) - 3.28) ^ -2
                C_A3_Nu = (C_A3_f / 2) * (C_A3_ReG - 1000) * C_A3_PrL / (1.07 + 12.7 * (C_A3_f / 2) ^ 0.5 * (C_A3_PrL ^ (2 / 3) - 1))
                C_A3_hi = C_A3_Nu * C_A3_kL / C_di
                '=======================================================================================================

                UA_A3 = (1 / C_noho / A3 + C_ratio / C_A3_hi / A3) ^ -1

                Dim NTU, Cmin, Ca, Cr, Cmax, Cstar, K, E As Double
                Ca = C_CA
                Cr = C_mr * C_A3_CpL
                Cmin = Math.Min(Ca, Cr)
                Cmax = Math.Max(Ca, Cr)
                Cstar = Cmin / Cmax
                NTU = UA_A3 / Cmin

                If C_N = 4 Then
                    If Cmin = Ca Then
                        K = 1 - Math.Exp(-NTU / 4)
                        E = 1 / Cstar * (1 - Math.Exp(-4 * K * Cstar) * (1 + Cstar * K ^ 2 * (6 - 4 * K + K ^ 2) + 4 * Cstar ^ 2 * K ^ 4 * (2 - K) + 8 * Cstar ^ 3 * K ^ 6 / 3))
                    ElseIf Cmin = Cr Then
                        K = 1 - Math.Exp(-NTU * Cstar / 4)
                        E = 1 - Math.Exp(-4 * K / Cstar) * (1 + K ^ 2 * (6 - 4 * K + K ^ 2) / Cstar + 4 * K ^ 4 * (2 - K) / Cstar ^ 2 + 8 * K ^ 6 / 3 / Cstar ^ 3)
                    End If
                End If

                If C_N > 4 Then
                    E = 1 - Math.Exp(NTU ^ 0.22 * (Math.Exp(-Cstar * NTU ^ 0.78) - 1) / Cstar)
                End If

                Dim Qmax As Double
                Qmax = Cr * (C_Ts - C_Tain)
                Q3_check = E * Qmax
                C_QA3 = Cr * (C_Ts - C_Tout)


                If C_QA3 - Q3_check < 0.01 Then
                    TR = C_Tout
                    C_Tout = (TL + TR) / 2
                ElseIf Q3_check - C_QA3 < 0.01 Then
                    TL = C_Tout
                    C_Tout = (TL + TR) / 2
                End If

                If Math.Abs(C_QA3 - Q3_check) < 0.01 Then Exit While

            End While

            ' MsgBox(C_Tout)
            HPCD.Xexpa_rin_txt.Text = 0.ToString("0.###")
            HPCD.Teapa_rin_txt.Text = (CtoK(CDbl(C_Tout)))
        End If

        HPCD.Qcond_sup_txt.Text = C_QA1.ToString("0.###")
        HPCD.Qcond_sat_txt.Text = C_QA2.ToString("0.###")
        HPCD.Qcond_sub_txt.Text = C_QA3.ToString("0.###")
        HPCD.Qcond_tot_txt.Text = ((C_QA1 + C_QA2 + C_QA3) / 1000).ToString("0.###")


    End Sub

    Sub Cond_System()
        Cond_Goemetry_Init()
        Cond_AirSide()
        Cond_Iteration()
    End Sub

End Module
