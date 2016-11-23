Module GlobalVar
    Public C1, C2, C3, C4, C5 As Double 'Kandlikar Coefficients

    '==========================================================================================
    '[ Compressor Parameters ] - Geometry


    '==========================================================================================

    '==========================================================================================
    '[ Evaporator Parameters ] - Geometry


    '==========================================================================================

    '==========================================================================================
    '[ Condensor Parameters ] - Geometry

    Public Class Condensor_Init
        ' // Refrigeration Inputs -> (Superheated , Saturation state) -> (mass flwo rate , reduced pressure)
        Public C_Trin, C_Cprin, C_rhorin, C_krin, C_Viscrin, C_Prin As Double
        Public C_Pr, C_mr As Double
        Public C_PrsG, C_PrsL, C_rhosL, C_rhosG, C_isLG, C_Ts, C_ViscsL, C_ViscsG, C_CpsL, C_CpsG, C_ksL, C_ksG As Double

        '// Air Inputs
        Public C_Tain, C_rhoa, C_Visca, C_Cpa, C_Pra, C_Taout As Double
        Public C_Vfr, CT1, CT2, CT3 As Double

        '// Geometry Inputs -> (original Geometry) -> (Derived Geometry)
        Public C_W, C_H, C_N, C_df, C_Fp, C_Pt, C_Pl, C_dc, C_dw, C_di As Double

        Public pi As Double
        Public C_NT, C_NF As Double
        Public C_Afr, C_sigma, C_Ac As Double
        Public C_Ao, C_Af, C_At, C_Dh, C_Ai As Double
        Public C_P1, C_P2, C_Redc, C_j, C_Vc, C_ho As Double
        Public C_m, C_no_efficiency, C_XL, C_XM, C_r, C_reqr, C_phi, C_nf_efficiency, C_noho, C_kf As Double

    End Class


    '==========================================================================================

    '==========================================================================================
    '[ Expansion Device ] - Geometry


    '==========================================================================================


End Module
