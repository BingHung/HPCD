Module GlobalVar
    Public C1, C2, C3, C4, C5 As Double 'Kandlikar Coefficients

    '[ Overall Parameters ]
    Public Influid As String = "R22"

    '==========================================================================================
    '[ Compressor Parameters ] -


    '==========================================================================================

    '==========================================================================================
    '[ Evaporator Parameters ] 


    '==========================================================================================

    '==========================================================================================
    '[ Condensor Parameters ] 


    ' // Refrigeration Inputs -> (Superheated , Saturation state) -> (mass flwo rate , reduced pressure)
    Public C_Trin, C_Cprin, C_rhorin, C_krin, C_Viscrin, C_Prin As Double
    Public C_Pr, C_mr As Double
    Public C_PrsG, C_PrsL, C_rhosL, C_rhosG, C_isLG, C_Ts, C_ViscsL, C_ViscsG, C_CpsL, C_CpsG, C_ksL, C_ksG As Double

    '   C_Trin > (oC)
    '   C_Cprin > (J/kg.K)
    '   C_rhorin > (kg/m^3)
    '   C_krin > (W/m.K)
    '   C_Viscrin > (N.s/m^2)
    '   C_Prin > (X)

    '   C_mr > (kg/s)
    '   C_Pr > (X)

    '   C_PrsG > (oC)
    '   C_PrsL > (J/kg.K)
    '   C_rhosL > (kg/m^3)
    '   C_rhosG > (W/m.K)
    '   C_isLG > (N.s/m^2)
    '   C_Ts > (X) 
    '   C_ViscsL > (N.s/m^2)
    '   C_ViscsG > (N.s/m^2)
    '   C_CpsL > (J/kg.K)
    '   C_CpsG > (J/kg.K)
    '   C_ksL > (W/m.K)
    '   C_ksG > (W/m.K)

    '// Air Inputs
    Public C_Tain, C_rhoa, C_Visca, C_Cpa, C_Pra As Double
    Public C_Vfr, CT1, CT2, CT3, C_Taout As Double

    '   C_Tain > (oC)
    '   C_rhoa > (kg/m^3)
    '   C_Visca > (N.s/m^2)
    '   C_Cpa > (J/kg.K)
    '   C_Pra > (X)
    '   C_Vfr > (m/s)

    '// Geometry Inputs -> (original Geometry) -> (Derived Geometry)
    Public C_W, C_H, C_N, C_df, C_Fp, C_Pt, C_Pl, C_dc, C_dw, C_di As Double

    '    All of the units above is (m)

    Public C_NT, C_NF As Double
    Public C_Afr, C_sigma, C_Ac, C_ratio As Double
    Public C_Ao, C_Af, C_At, C_Dh, C_Ai As Double
    Public C_P1, C_P2, C_Redc, C_j, C_Vc, C_ho As Double
    Public C_m, C_no_efficiency, C_XL, C_XM, C_r, C_reqr, C_phi, C_nf_efficiency, C_noho, C_kf As Double

    '// HX Iteration

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

    '==========================================================================================

    '==========================================================================================
    '[ Expansion Device ] 


    '==========================================================================================


End Module
