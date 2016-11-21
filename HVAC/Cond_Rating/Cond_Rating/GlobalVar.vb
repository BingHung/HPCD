Module GlobalVar

    '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ' Condensor Parameters

    '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Public C_Trin, C_Cprin, C_rhorin, C_mr, C_krin, C_Viscrin, C_Prin, C_PrsG, C_PrsL, C_Pr, C_rhosL, C_rhosG, C_isLG, C_Ts, C_ViscsL, C_ViscsG, C_CpsL, C_CpsG, C_ksL, C_ksG As Double
    Public C_Tain, C_rhoa, C_Vfr, C_Visca, C_Cpa, C_Pra, C_Taout, CT1, CT2, CT3 As Double

    Public C_W, C_H, C_N, C_df, C_Fp, C_Pt, C_Pl, C_dc, C_dw, C_di As Double

    Public pi As Double
    Public C_NT, C_NF As Double
    Public C_Afr, C_sigma, C_Ac As Double
    Public C_Ao, C_Af, C_At, C_Dh, C_Ai As Double
    Public C_P1, C_P2, C_Redc, C_j, C_Vc, C_ho As Double



    Public C_m, C_no_efficiency, C_XL, C_XM, C_r, C_reqr, C_phi, C_nf_efficiency, C_noho, C_kf As Double


    'A1_Cond'
    Public C_A1_CprG, C_QA1, C_A1_Cr, C_ma, C_CA As Double 'C_ma C_CA
    Public C_A1_Cmin, C_QmaxA1, C_EA1 As Double
    Public C_A1_ViscG, C_A1_kG, C_A1_PrG, C_A1_G, C_A1_ReG, C_A1_f, C_A1_hi, C_A1_Nu As Double
    Public C_A1_star, C_CA1, C_A1, C_CA1_star, C_A1i, C_A1_NTU, C_A1_UA_NTU, C_A1_UA_HR As Double

    'A2_Cond'
    Public C_A2_hL, C_A2_hcm, C_QA2, Cmin_A2, Cmax_A2, C_star_A2, C_EA2, C_A2_star, C_QmaxA2, C_A2_NTU, C_A2, C_A2i, C_CA2, C_A2_UA_NTU, C_A2_UA_HR, x As Double
    Public C_Qtot, QualityX, XL, XR, Xg As Double

    'A3_Cond'
    Public C_A3, C_Tout, C_QA3, C_A3_CpL, C_QmaxA3, C_A3_Cr, C_A3_CA, C_A3_Cmin, C_EA3, C_CA3, C_CA3_Star, C_A3_NTU, C_A3_UA_NTU, C_A3_UA_HR, C_A3i, C_A3_G, C_A3_ReG, C_A3_ViscL, C_A3_f, C_A3_Nu, C_A3_PrL, C_A3_kL, C_A3_hi, C_A3_star As Double

    Public S_initial As Double


    ' Transfer parameters
    Public State_A2 As Double ' Cond => (TwoPhase / Subcooled)
    Public i_state3 As Double ' Expansion Start
    Public Influid As String

    '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ' Evaporator Parameters

    '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Public E_W, E_H, d_i, d_o, d_c, Fp, Pt, Pl, thickFIN, thickTUBE, Nrows, Ntube, Ncr, kp, kf As Double
    Public Tdry, Twet, Tdew, RH, Wai, mair As Double
    Public Pai, Prsat, Tri, xri, xre, mref As Double
    Public C1, C2, C3, C4, C5 As Double 'Kandlikar Coefficients
    Public Tdry_out, Wair_out As Double
    Public Nfins As Integer

    Public Afr, Ac, contractionR, Apo, Api, Apm, Afin, Ao, hi, Af As Double
    Public A4, A5, T5, Ta5 As Double

End Module
