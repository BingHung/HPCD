Module Dryer


    Public Structure DryerAir

        Public T, RH, Pws, Pw, Pa, nw, mw, P As Double
        Public mPws, mPw, dm As Double

    End Structure

    Public Structure DryerCondition

        Public V, W_height, A, Patm, R, MolecularWeight As Double
        Public t As Double
        Public G, dmLM, zeta As Double
    End Structure

    Public DryerAirIn As New DryerAir
    Public DryerAirOut As New DryerAir
    Public DryerInitial As New DryerCondition

    Sub DryerModel()



        DryerAirOut.T = 0

        DryerInitial.A = 1 '(m^3)
        DryerInitial.W_height = 0.1 '(m)
        DryerInitial.V = 0.9 '(m^3)
        DryerInitial.Patm = 101.325 '(kPa)
        DryerInitial.R = 8314.41 '(X)
        DryerInitial.MolecularWeight = 18 '(g/mol)

        'DryerInitial.t = 20 '(s)
        DryerInitial.t = CDbl(HPCD.Time_Slot_txt.Text) '(s)

        '[1] Calculate DryerAirIn
        'DryerAirIn.T = 298.15 '(K)
        'DryerAirIn.RH = 0.1 '(X)
        DryerAirIn.T = DAP.DWT_In '(K)
        DryerAirIn.RH = DAP.RH_In '(X)

        DryerAirIn.Pws = Fun_Pws(DryerAirIn.T) '(Pa)
        DryerAirIn.Pw = DryerAirIn.Pws * DryerAirIn.RH '(Pa)
        DryerAirIn.Pa = DryerInitial.Patm - DryerAirIn.Pw / 1000 '(kPa)
        DryerAirIn.P = DryerInitial.Patm '(Pa)
        DryerAirIn.nw = (DryerAirIn.Pw * DryerInitial.V / DryerInitial.R / DryerAirIn.T) * 1000 '(mole)
        DryerAirIn.mw = DryerAirIn.nw * DryerInitial.MolecularWeight '(g)

        Dim L_RH, R_RH As Double
        Dim G_iterate As Double
        L_RH = 0
        R_RH = 1
        DryerAirOut.RH = (L_RH + R_RH) / 2

        While (1)


            '[2] Calculate G , rho 

            '   [2-1] guess RH_out
            'DryerAirOut.RH = 0.3
            DryerAirOut.Pws = DryerAirIn.Pws '(Pa)
            DryerAirOut.Pw = DryerAirOut.Pws * DryerAirOut.RH '(Pa)
            DryerAirOut.P = DryerAirOut.Pw / 1000 + DryerAirIn.Pa '(Pa)
            DryerAirOut.nw = (DryerAirOut.Pw * DryerInitial.V / DryerInitial.R / DryerAirIn.T) * 1000 '(mole)
            DryerAirOut.mw = DryerAirOut.nw * DryerInitial.MolecularWeight '(g)

            '   [2-2] Get G
            '[Not Correct ???] DryerInitial.G = (DryerAirOut.mw - DryerAirIn.mw) / 1000 / DryerInitial.t / DryerInitial.A '(kg/m^2.s)
            DryerInitial.G = (DryerAirOut.mw - DryerAirIn.mw) / 1000 / DryerInitial.t / DryerInitial.A '(kg/m^2.s)

            '   [2-3] Get mass transfer coef
            DryerAirIn.mPws = DryerAirIn.Pws / 0.62198 / DryerAirIn.P / 1000
            DryerAirIn.mPw = DryerAirIn.Pw / 0.62198 / DryerAirIn.P / 1000
            DryerAirIn.dm = DryerAirIn.mPws - DryerAirIn.mPw

            DryerAirOut.mPws = DryerAirIn.mPws 'DryerAirOut.Pws / 0.62198 / DryerAirOut.P / 1000
            DryerAirOut.mPw = DryerAirOut.Pw / 0.62198 / DryerAirOut.P / 1000
            DryerAirOut.dm = DryerAirOut.mPws - DryerAirOut.mPw

            DryerInitial.dmLM = (DryerAirIn.dm - DryerAirOut.dm) / Math.Log(DryerAirIn.dm / DryerAirOut.dm)
            'DryerInitial.zeta = DryerInitial.G / DryerInitial.dmLM
            DryerInitial.zeta = 0.0004013

            G_iterate = DryerInitial.zeta * DryerInitial.dmLM

            If (DryerInitial.G - G_iterate) > 0.0000001 Then
                R_RH = DryerAirOut.RH
                DryerAirOut.RH = (L_RH + R_RH) / 2
            ElseIf (G_iterate - DryerInitial.G) > 0.0000001 Then
                L_RH = DryerAirOut.RH
                DryerAirOut.RH = (L_RH + R_RH) / 2
            Else
                Exit While
            End If

        End While

        HPCD.Dryer_DBT_Out_txt.Text = DryerAirIn.T.ToString("0.###")
        HPCD.Dryer_RH_Out_txt.Text = DryerAirOut.RH.ToString("0.###")

        HPCD.E_DBT_txt.Text = DryerAirIn.T.ToString("0.###")
        HPCD.E_RH_txt.Text = DryerAirOut.RH.ToString("0.###")


    End Sub

    Public Function Fun_Pws(T As Double) As Double

        ' The Unit of T =>  (K)

        Dim C8, C9, C10, C11, C12, C13 As Double
        C8 = -5800.2206
        C9 = 1.3914993
        C10 = -0.048640239
        C11 = 0.0000417676769
        C12 = -0.000000014452093
        C13 = 6.5459653

        Dim Pws_val As Double
        Pws_val = Math.Exp(C8 / T + C9 + C10 * T + C11 * T ^ 2 + C12 * T ^ 3 + C13 * Math.Log(T))

        Return Pws_val 'return unit (Pa)
    End Function

End Module
