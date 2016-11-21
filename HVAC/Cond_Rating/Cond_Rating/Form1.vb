Public Class Form1

    Dim fileNum As Integer   ' 檔案代號變數，供所有事件共用
    Dim fileName As String   ' 檔案名稱變數，供所有事件共用

    Dim counter As Integer = 0

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        PictureBox1.SizeMode = PictureBoxSizeMode.StretchImage
        PictureBox1.Image = Image.FromFile("CyclePath.PNG")

        PictureBox2.SizeMode = PictureBoxSizeMode.StretchImage
        PictureBox2.Image = Image.FromFile("Cond.PNG")

        PictureBox3.SizeMode = PictureBoxSizeMode.StretchImage
        PictureBox3.Image = Image.FromFile("Evap.PNG")

        fileName = ""                         ' 檔名清成空白
        Me.Text = "未命名-記事本"             ' 表單標題欄名稱



    End Sub


    Private Sub Start_Click(sender As Object, e As EventArgs) Handles Start.Click
        GlobalVar.Influid = InFluid.Text '"R134a" '"r22" 
        'Influid = "R32;0.5;R125;0.5"

        'C.Sat_Refrigerants
        Dim C_sat As New Fluid(GlobalVar.Influid, "si", "tp")
        C_sat.SatProp(CDbl(CTsat.Text))
        CPsat.Text = C_sat.P.ToString("0.###")


        Evap.Evap()
        'MsgBox("EVAP")
        Comp.Comp()
        'MsgBox("COMP")
        Cond_LMTD.Cond_LMTD()
        'MsgBox("COND")

        Evap.Evap()
        'MsgBox("EVAP")
        Comp.Comp()
        'MsgBox("COMP")
        Cond_LMTD.Cond_LMTD()
        'MsgBox("COND")

        TextBox16.Text = CDbl(QEtxt.Text) + CDbl(TextBox11.Text)
        TextBox18.Text = (CDbl(TextBox16.Text) - CDbl(QCtxt.Text) * 1000)
        MsgBox("done")
        'Dim CondPower As Double
        'CondPower = CDbl(QEtxt.Text) + CDbl(TextBox11.Text)

        'MsgBox(CondPower)

        'Evap()
        'Cond_LMTD()


        If Math.Abs(CDbl(TextBox18.Text)) < 25 Then
            TextBox35.BackColor = Color.Green
            TextBox35.Text = "Energy Converge"
            counter = counter + 1
        Else
            TextBox35.BackColor = Color.Red
            TextBox35.Text = "Energy Diverge"
            counter = 0
        End If

        If counter >= 3 Then
            MessageBox.Show("The ANS is Found", "Congradulations", MessageBoxButtons.OK, MessageBoxIcon.Asterisk)
        End If




    End Sub

    Private Sub Compbtn_Click(sender As Object, e As EventArgs) Handles Compbtn.Click
        GlobalVar.Influid = "r22"

        'C.Sat_Refrigerants
        Dim C_sat As New Fluid(GlobalVar.Influid, "si", "tp")
        C_sat.SatProp(CDbl(CTsat.Text))
        CPsat.Text = C_sat.P

        Comp.Comp()
    End Sub

    Private Sub Condbtn_Click(sender As Object, e As EventArgs) Handles Condbtn.Click
        GlobalVar.Influid = "r22"

        'C.Sat_Refrigerants
        Dim C_sat As New Fluid(GlobalVar.Influid, "si", "tp")
        C_sat.SatProp(CDbl(CTsat.Text))
        CPsat.Text = C_sat.P

        Cond_LMTD.Cond_LMTD()
    End Sub

    Private Sub Evapbtn_Click(sender As Object, e As EventArgs) Handles Evapbtn.Click
        GlobalVar.Influid = "r22"

        'C.Sat_Refrigerants
        Dim C_sat As New Fluid(GlobalVar.Influid, "si", "tp")
        C_sat.SatProp(CDbl(CTsat.Text))
        CPsat.Text = C_sat.P

        Evap.Evap()
    End Sub


    Private Sub Expabtn_Click(sender As Object, e As EventArgs) Handles Expabtn.Click
        GlobalVar.Influid = InFluid.Text '"r22" 

        'C.Sat_Refrigerants
        Dim C_sat As New Fluid(GlobalVar.Influid, "si", "tp")
        C_sat.SatProp(CDbl(CTsat.Text))
        CPsat.Text = C_sat.P

        State_A2 = 2

        Expa.Expa()

        ETsat.Text = Temperature(GlobalVar.Influid, "PLIQ", "SI", CDbl(EPsat.Text))
        ETsat.Text = CDbl(ETsat.Text).ToString("0.###")
        T3_4.Text = CDbl(ETsat.Text).ToString("0.###")

    End Sub


    Sub test()
        GlobalVar.Influid = "r22"

        'C.Sat_Refrigerants
        Dim C_sat As New Fluid(GlobalVar.Influid, "si", "tp")
        C_sat.SatProp(CDbl(CTsat.Text))
        CPsat.Text = C_sat.P

        ' Define working fluid of the system
        ' Define the initial guess of the high pressure CTsat

        Cond()
        Expa.Expa()

        ETsat.Text = Temperature(GlobalVar.Influid, "PLIQ", "SI", CDbl(EPsat.Text))
        T3_4.Text = CDbl(ETsat.Text)
        'E.Sat_Refrigerants
        Dim E_sat As New Fluid(GlobalVar.Influid, "si", "tp")
        E_sat.SatProp(CDbl(ETsat.Text))
        'EPsat.Text = E_sat.P

        Evap.Evap()
    End Sub


    Sub Cond()

        'Class Fluid Setup (C_Sat / C_Sup / C_Air_Inlet) <===============================================================================================================================

        'C.Sat_Refrigerants
        Dim C_sat As New Fluid(GlobalVar.Influid, "si", "tp")
        C_sat.SatProp(CDbl(CTsat.Text))

        'C.Sup_Refrigerants
        Dim C_sup As New Fluid(GlobalVar.Influid, "si", "tp")
        C_sup.Properties(CDbl(T1_2.Text), CDbl(CPsat.Text))

        'C.In_Air
        Dim C_air As New Fluid("air", "si", "tp")
        C_air.Properties(CtoK(TextBox31.Text), 0.101325)

        'Initialization <==========================================================================================================================

        'S_initial = Entropy("r22", "TP", "SI", CtoK(T1_2.Text), C_sat.P)  ' for Cycle iteration

        'refrigerant side'
        C_Trin = KtoC(CDbl(T1_2.Text))
        C_Cprin = C_sup.cp : C_rhorin = C_sup.rho : C_krin = C_sup.k : C_Viscrin = C_sup.Visc : C_Prin = C_sup.Pr
        C_PrsG = C_sat.PrG : C_PrsL = C_sat.PrL : C_rhosL = C_sat.rhoL : C_rhosG = C_sat.rhoG
        C_ViscsL = C_sat.viscL : C_ViscsG = C_sat.viscG : C_CpsL = C_sat.CpL : C_CpsG = C_sat.CpG : C_ksL = C_sat.kL : C_ksG = C_sat.kG
        C_Ts = KtoC(CDbl(CTsat.Text)) : C_mr = CDbl(MFR.Text) '0.02083
        C_Pr = 0.437 : C_isLG = C_sat.ifg / 1000

        'air side'
        C_Tain = CDbl(TextBox31.Text) : C_Vfr = CDbl(TextBox36.Text)
        C_rhoa = CDbl(TextBox5.Text) : C_Visca = CDbl(TextBox6.Text) : C_Cpa = CDbl(TextBox7.Text) : C_Pra = CDbl(TextBox8.Text)

        'HX geometry'
        C_W = TextBox20.Text : C_H = TextBox19.Text : C_N = TextBox21.Text : C_df = TextBox26.Text : C_Fp = TextBox22.Text
        C_Pt = TextBox23.Text : C_Pl = TextBox24.Text : C_dc = TextBox29.Text : C_dw = TextBox28.Text : C_di = TextBox27.Text


        '_________________________________________________________________________________
        ' Cond (Superheated => Subcooled or Saturation)
        '_________________________________________________________________________________
        'Geometry(Ao / Sigma/ Ao/Ai) & AirSide (noho) <===============================================================================================================================================

        pi = Math.PI

        C_Afr = C_W * C_H
        C_NT = C_H / C_Pt
        C_NF = C_W / C_Fp
        C_Ac = C_Afr - C_NT * (C_dc * C_W + C_NF * C_df * (C_Pt - C_dc))
        C_sigma = C_Ac / C_Afr

        TextBox3.Text = C_sigma

        'HX total area'
        C_Af = 2 * C_NF * (C_Pl * C_H - pi / 4 * C_dc * C_dc * C_NT) * C_N + 2 * C_df * C_NF * (C_H + C_Pl * C_N)
        C_At = pi * C_dc * (C_W - C_NF * C_df) * C_NT * C_N
        C_Ao = C_Af + C_At
        C_Dh = 4 * C_Ac * C_Pl / C_Ao
        C_Ai = pi * C_di * C_W * C_NT * C_N

        TextBox2.Text = C_Ao
        TextBox4.Text = C_Ao / C_Ai

        'heat transfer coefficient'
        C_Redc = C_rhoa * C_Vfr * C_dc / C_Visca / C_sigma
        C_P1 = 1.9 - 0.23 * Math.Log(C_Redc)
        C_P2 = -0.236 + 0.126 * Math.Log(C_Redc)
        C_j = 0.108 * C_Redc ^ (-0.29) * (C_Pt / C_Pl) ^ (C_P1) * (C_Fp / C_dc) ^ (-1.084) * (C_Fp / C_Dh) ^ (-0.786) * (C_Fp / C_Pt) ^ (C_P2)
        C_Vc = C_Vfr / C_sigma
        C_ho = C_j * C_rhoa * C_Vc * C_Cpa / (C_Pra) ^ (2 / 3)

        C_kf = TextBox38.Text
        C_m = (2 * C_ho / C_kf / C_df) ^ (0.5)
        C_XL = C_Pl / 2
        C_XM = C_Pt / 2 '((Pl / 2) ^ 2 + (Pl) ^ 2) ^ 0.5 / 2'
        C_r = C_dc / 2
        C_reqr = 1.28 * C_XM * ((C_XL / C_XM) - 0.2) ^ (0.5) / C_r
        C_phi = (C_reqr - 1) * (1 + 0.35 * Math.Log(C_reqr))
        C_nf_efficiency = Math.Tanh(C_m * C_r * C_phi) / (C_m * C_r * C_phi)
        C_no_efficiency = 1 - (C_Af / C_Ao) * (1 - C_nf_efficiency)
        C_noho = C_no_efficiency * C_ho

        TextBox1.Text = C_noho

        'Condensor_Superheated_A1 <==========================================================================================================================
        'A1'
        C_A1_CprG = (C_Cprin + C_CpsG) / 2
        C_QA1 = C_mr * C_A1_CprG * (C_Trin - C_Ts)
        C_A1_Cr = C_mr * C_A1_CprG
        C_ma = C_rhoa * C_Vfr * C_Afr
        C_CA = C_ma * C_Cpa

        ' Get hi Property
        C_A1_ViscG = (C_Viscrin + C_ViscsG) / 2
        C_A1_kG = (C_krin + C_ksG) / 2
        C_A1_PrG = (C_Prin + C_PrsG) / 2
        C_A1_G = C_mr / (pi * C_di * C_di / 4)
        C_A1_ReG = C_A1_G * C_di / C_A1_ViscG
        C_A1_f = (1.58 * Math.Log(C_A1_ReG) - 3.28) ^ (-2)
        C_A1_Nu = ((C_A1_f / 2) * (C_A1_ReG - 1000) * C_A1_PrG) / (1.07 + 12.7 * (C_A1_f / 2) ^ 0.5 * (C_A1_PrG ^ (2 / 3) - 1))
        C_A1_hi = C_A1_Nu * C_A1_kG / C_di

        ' Set Bisection ranges (0 => 1)
        Dim L, R As Double
        L = 0
        R = 1
        C_A1_star = (L + R) / 2

        ' Guess A1*
        While (1)

            ' P152 ESDU
            C_A1_Cmin = Math.Min(C_A1_Cr, C_CA)
            Dim state_A1_side As String = ""

            If C_A1_Cmin = C_A1_Cr Then
                state_A1_side = " TubeSide"
                C_QmaxA1 = C_A1_Cmin * (C_Trin - C_Tain)
            ElseIf (C_A1_Cmin = C_CA) Then
                state_A1_side = " AirSide"
                C_QmaxA1 = C_A1_Cmin * (C_Trin - C_Tain) * C_A1_star
            End If

            C_EA1 = C_QA1 / C_QmaxA1

            If C_EA1 > 1 Then
                L = C_A1_star
                C_A1_star = (L + R) / 2
            ElseIf C_EA1 < 1 Then

                C_A1 = C_A1_star * C_Ao
                C_A1i = C_A1 / (C_Ao / C_Ai)
                C_CA1 = C_A1_star * C_CA
                C_CA1_star = C_A1_Cr / C_CA1


                ' Add P152 mathods in ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                'C_A1_NTU = -Math.Log(C_CA1_star * Math.Log(1 - C_EA1) + 1) / C_CA1_star

                Dim Offset, dNTU, NTU_guess, EA1_dNTU, K, C_EA1_o As Double
                dNTU = 0.0001
                Offset = C_EA1
                NTU_guess = 1

                If state_A1_side = " AirSide" Then
                    If CDbl(TextBox21.Text) = 1 Then
                        While (1)
                            C_EA1_o = (1 / C_CA1_star) * (1 - Math.Exp(-C_CA1_star * (1 - Math.Exp(-NTU_guess)))) - Offset
                            EA1_dNTU = (1 / C_CA1_star) * (1 - Math.Exp(-C_CA1_star * (1 - Math.Exp(-(NTU_guess + dNTU))))) - Offset

                            C_A1_NTU = NTU_guess
                            NTU_guess = NTU_guess - C_EA1_o / ((EA1_dNTU - C_EA1_o) / dNTU)

                            If (Math.Abs(C_A1_NTU - NTU_guess) < 0.0001) Then Exit While
                        End While
                    ElseIf CDbl(TextBox21.Text) = 2 Then
                        While (1)
                            K = 1 - Math.Exp(-NTU_guess / 2)
                            C_EA1_o = (1 / C_CA1_star) * (1 - Math.Exp(-2 * K * C_CA1_star) * (1 + C_CA1_star * K ^ 2)) - Offset
                            K = 1 - Math.Exp(-(NTU_guess + dNTU) / 2)
                            EA1_dNTU = (1 / C_CA1_star) * (1 - Math.Exp(-2 * K * C_CA1_star) * (1 + C_CA1_star * K ^ 2)) - Offset

                            C_A1_NTU = NTU_guess
                            NTU_guess = NTU_guess - C_EA1_o / ((EA1_dNTU - C_EA1_o) / dNTU)

                            If (Math.Abs(C_A1_NTU - NTU_guess) < 0.0001) Then Exit While
                        End While
                    ElseIf CDbl(TextBox21.Text) = 3 Then
                        While (1)
                            K = 1 - Math.Exp(-NTU_guess / 3)
                            C_EA1_o = (1 / C_CA1_star) * (1 - Math.Exp(-3 * K * C_CA1_star) * (1 + C_CA1_star * K ^ 2 * (3 - K) + (3 * (C_CA1_star) ^ 2 * K ^ 4) / 2)) - Offset
                            K = 1 - Math.Exp(-(NTU_guess + dNTU) / 3)
                            EA1_dNTU = (1 / C_CA1_star) * (1 - Math.Exp(-3 * K * C_CA1_star) * (1 + C_CA1_star * K ^ 2 * (3 - K) + (3 * (C_CA1_star) ^ 2 * K ^ 4) / 2)) - Offset

                            C_A1_NTU = NTU_guess
                            NTU_guess = NTU_guess - C_EA1_o / ((EA1_dNTU - C_EA1_o) / dNTU)

                            If (Math.Abs(C_A1_NTU - NTU_guess) < 0.0001) Then Exit While
                        End While
                    ElseIf CDbl(TextBox21.Text) = 4 Then
                        While (1)
                            K = 1 - Math.Exp(-NTU_guess / 4)
                            C_EA1_o = (1 / C_CA1_star) * (1 - Math.Exp(-4 * K * C_CA1_star) * (1 + C_CA1_star * K ^ 2 * (6 - 4 * K + K ^ 2) + (4 * (C_CA1_star) ^ 2 * K ^ 4 * (2 - K)) + (8 * C_CA1_star ^ 3 * K ^ 6) / 3)) - Offset
                            K = 1 - Math.Exp(-(NTU_guess + dNTU) / 4)
                            EA1_dNTU = (1 / C_CA1_star) * (1 - Math.Exp(-4 * K * C_CA1_star) * (1 + C_CA1_star * K ^ 2 * (6 - 4 * K + K ^ 2) + (4 * (C_CA1_star) ^ 2 * K ^ 4 * (2 - K)) + (8 * C_CA1_star ^ 3 * K ^ 6) / 3)) - Offset

                            C_A1_NTU = NTU_guess
                            NTU_guess = NTU_guess - C_EA1_o / ((EA1_dNTU - C_EA1_o) / dNTU)

                            If (Math.Abs(C_A1_NTU - NTU_guess) < 0.0001) Then Exit While
                        End While
                    ElseIf CDbl(TextBox21.Text) > 4 Then
                        While (1)
                            C_EA1_o = 1 - Math.Exp(NTU_guess ^ 0.22 * (Math.Exp(-C_CA1_star * NTU_guess ^ 0.78) - 1) / C_CA1_star) - Offset
                            EA1_dNTU = 1 - Math.Exp((NTU_guess + dNTU) ^ 0.22 * (Math.Exp(-C_CA1_star * (NTU_guess + dNTU) ^ 0.78) - 1) / C_CA1_star) - Offset

                            C_A1_NTU = NTU_guess
                            NTU_guess = NTU_guess - C_EA1_o / ((EA1_dNTU - C_EA1_o) / dNTU)

                            If (Math.Abs(C_A1_NTU - NTU_guess) < 0.0001) Then Exit While
                        End While
                    End If

                ElseIf state_A1_side = " TubeSide" Then
                    If CDbl(TextBox21.Text) = 1 Then
                        While (1)

                            C_EA1_o = 1 - Math.Exp(-(1 - Math.Exp(-NTU_guess * C_CA1_star)) / C_CA1_star) - Offset
                            EA1_dNTU = 1 - Math.Exp(-(1 - Math.Exp(-(NTU_guess + dNTU) * C_CA1_star)) / C_CA1_star) - Offset

                            C_A1_NTU = NTU_guess
                            NTU_guess = NTU_guess - C_EA1_o / ((EA1_dNTU - C_EA1_o) / dNTU)

                            If (Math.Abs(C_A1_NTU - NTU_guess) < 0.0001) Then Exit While
                        End While
                    ElseIf CDbl(TextBox21.Text) = 2 Then
                        While (1)
                            K = 1 - Math.Exp(-NTU_guess * C_CA1_star / 2)
                            C_EA1_o = 1 - Math.Exp(-2 * K / C_CA1_star) * (1 + K ^ 2 / C_CA1_star) - Offset
                            K = 1 - Math.Exp(-(NTU_guess + dNTU) * C_CA1_star / 2)
                            EA1_dNTU = 1 - Math.Exp(-2 * K / C_CA1_star) * (1 + K ^ 2 / C_CA1_star) - Offset

                            C_A1_NTU = NTU_guess
                            NTU_guess = NTU_guess - C_EA1_o / ((EA1_dNTU - C_EA1_o) / dNTU)

                            If (Math.Abs(C_A1_NTU - NTU_guess) < 0.0001) Then Exit While
                        End While
                    ElseIf CDbl(TextBox21.Text) = 3 Then
                        While (1)
                            K = 1 - Math.Exp(-NTU_guess * C_CA1_star / 3)
                            C_EA1_o = 1 - Math.Exp(-3 * K / C_CA1_star) * (1 + K ^ 2 * (3 - K) / C_CA1_star + (3 * K ^ 4) / (2 * C_CA1_star ^ 2)) - Offset
                            K = 1 - Math.Exp(-(NTU_guess + dNTU) * C_CA1_star / 3)
                            EA1_dNTU = 1 - Math.Exp(-3 * K / C_CA1_star) * (1 + K ^ 2 * (3 - K) / C_CA1_star + (3 * K ^ 4) / (2 * C_CA1_star ^ 2)) - Offset

                            C_A1_NTU = NTU_guess
                            NTU_guess = NTU_guess - C_EA1_o / ((EA1_dNTU - C_EA1_o) / dNTU)

                            If (Math.Abs(C_A1_NTU - NTU_guess) < 0.0001) Then Exit While
                        End While
                    ElseIf CDbl(TextBox21.Text) = 4 Then
                        While (1)
                            K = 1 - Math.Exp(-NTU_guess * C_CA1_star / 4)
                            C_EA1_o = 1 - Math.Exp(-4 * K / C_CA1_star) * (1 + K ^ 2 * (6 - 4 * K + K ^ 2) / C_CA1_star + (4 * K ^ 4 * (2 - K)) / (C_CA1_star ^ 2) + 8 * K ^ 6 / (3 * C_CA1_star ^ 3)) - Offset
                            K = 1 - Math.Exp(-(NTU_guess + dNTU) * C_CA1_star / 4)
                            EA1_dNTU = 1 - Math.Exp(-4 * K / C_CA1_star) * (1 + K ^ 2 * (6 - 4 * K + K ^ 2) / C_CA1_star + (4 * K ^ 4 * (2 - K)) / (C_CA1_star ^ 2) + 8 * K ^ 6 / (3 * C_CA1_star ^ 3)) - Offset

                            C_A1_NTU = NTU_guess
                            NTU_guess = NTU_guess - C_EA1_o / ((EA1_dNTU - C_EA1_o) / dNTU)

                            If (Math.Abs(C_A1_NTU - NTU_guess) < 0.0001) Then Exit While
                        End While
                    ElseIf CDbl(TextBox21.Text) > 4 Then
                        While (1)
                            C_EA1_o = 1 - Math.Exp(NTU_guess ^ 0.22 * (Math.Exp(-C_CA1_star * NTU_guess ^ 0.78) - 1) / C_CA1_star) - Offset
                            EA1_dNTU = 1 - Math.Exp((NTU_guess + dNTU) ^ 0.22 * (Math.Exp(-C_CA1_star * (NTU_guess + dNTU) ^ 0.78) - 1) / C_CA1_star) - Offset

                            C_A1_NTU = NTU_guess
                            NTU_guess = NTU_guess - C_EA1_o / ((EA1_dNTU - C_EA1_o) / dNTU)

                            If (Math.Abs(C_A1_NTU - NTU_guess) < 0.0001) Then Exit While
                        End While
                    End If
                End If
                ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                C_A1_UA_NTU = C_A1_NTU * C_A1_Cmin
                C_A1_UA_HR = ((1 / (C_noho * C_A1)) + (1 / (C_A1_hi * C_A1i))) ^ (-1)
                ' Math.Abs(C_A1_UA_NTU - C_A1_UA_HR) < 0.1 

                If (C_A1_UA_HR - C_A1_UA_NTU) > 0.01 Then
                    R = C_A1_star
                    C_A1_star = (R + L) / 2
                ElseIf (C_A1_UA_NTU - C_A1_UA_HR) > 0.01 Then
                    L = C_A1_star
                    C_A1_star = (R + L) / 2
                End If

                If Math.Abs(C_A1_UA_HR - C_A1_UA_NTU) < 0.01 Then Exit While
            End If
        End While




        'MsgBox(C_A1_UA_NTU)
        'MsgBox(C_A1_UA_HR)

        'Condensor_Saturation_A2 <==========================================================================================================================
        'A2'
        C_A2_hL = (C_ksL / C_di) * 0.023 * (C_A1_G * C_di / C_ViscsL) ^ 0.8 * C_PrsL ^ 0.4
        C_A2_hcm = C_A2_hL * (0.55 + 2.09 / (C_Pr ^ 0.38))

        C_A2_star = 1 - C_A1_star

        C_QmaxA2 = C_ma * C_Cpa * (C_Ts - C_Tain) * C_A2_star
        C_QA2 = C_mr * C_isLG * 1000 ' 1000
        C_EA2 = C_QA2 / C_QmaxA2
        C_A2_NTU = Math.Log(1 / (1 - C_EA2))

        C_CA2 = C_A2_star * C_CA
        C_A2 = C_A2_star * C_Ao
        C_A2i = C_A2 / (C_Ao / C_Ai)

        C_A2_UA_NTU = C_A2_NTU * C_CA2
        C_A2_UA_HR = ((1 / (C_noho * C_A2)) + (1 / (C_A2_hcm * C_A2i))) ^ (-1)

        'Dim State_A2 As Double
        ' Check if it's in x or A2*

        If C_A2_UA_NTU < C_A2_UA_HR Then
            'calculate A2* (0=> 1 - A1*)
            MsgBox("totally condense")
            TextBox17.Text = 0
            State_A2 = 2

            L = 0
            R = 1 - C_A1_star
            C_A2_star = (L + R) / 2

            While (1)

                C_QmaxA2 = C_ma * C_Cpa * (C_Ts - C_Tain) * C_A2_star
                C_EA2 = C_QA2 / C_QmaxA2

                If C_EA2 > 1 Then
                    L = C_A2_star
                    C_A2_star = (R + L) / 2
                ElseIf C_EA2 < 1 Then
                    C_A2_NTU = Math.Log(1 / (1 - C_EA2))
                    C_CA2 = C_A2_star * C_CA
                    C_A2 = C_A2_star * C_Ao
                    C_A2i = C_A2 / (C_Ao / C_Ai)

                    C_A2_UA_NTU = C_A2_NTU * C_CA2
                    C_A2_UA_HR = ((1 / (C_noho * C_A2)) + (1 / (C_A2_hcm * C_A2i))) ^ (-1)

                    If (C_A2_UA_HR - C_A2_UA_NTU) > 0.01 Then
                        R = C_A2_star
                        C_A2_star = (R + L) / 2
                    ElseIf (C_A2_UA_NTU - C_A2_UA_HR) > 0.01 Then
                        L = C_A2_star
                        C_A2_star = (R + L) / 2
                    End If
                End If

                If Math.Abs(C_A2_UA_NTU - C_A2_UA_HR) < 0.01 Then Exit While

            End While
            'MsgBox(C_A2_star)

        Else
            ' calcutae x (0=>1)
            MsgBox("clculate X")
            State_A2 = 1

            L = 0
            R = 1
            x = (L + R) / 2

            While (1)
                C_QA2 = C_mr * C_isLG * 1000 * (1 - x)
                C_EA2 = C_QA2 / C_QmaxA2
                C_A2_NTU = Math.Log(1 / (1 - C_EA2))
                C_A2_UA_NTU = C_A2_NTU * C_CA2

                If (C_A2_UA_HR - C_A2_UA_NTU) > 0.01 Then
                    R = x
                    x = (R + L) / 2
                ElseIf (C_A2_UA_NTU - C_A2_UA_HR) > 0.01 Then
                    L = x
                    x = (R + L) / 2
                End If

                If Math.Abs(C_A2_UA_HR - C_A2_UA_NTU) < 0.01 Then Exit While

            End While
            ' MsgBox(x)
            TextBox17.Text = x
        End If



        'Condensor_Subcooled_A3 <==========================================================================================================================

        If State_A2 = 2 Then


            ' Make sure completely condensation or not ( => at subcooled state) <----------------------------------


            C_A3_star = 1 - C_A2_star - C_A1_star
            C_A3 = C_A3_star * C_Ao
            C_A3i = C_A3 / (C_Ao / C_Ai)
            C_CA3 = C_A3_star * C_CA


            ' C.Sub_Refrigerants
            Dim C_sub As New Fluid("r22", "si", "tp")


            L = C_Tain
            R = CDbl(CTsat.Text)
            C_Tout = (L + R) / 2

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

                C_A3_Cr = C_mr * C_A3_CpL
                C_A3_CA = C_ma * C_Cpa

                C_A3_Cmin = Math.Min(C_A3_Cr, C_A3_CA)
                Dim state_A3_side As String = ""

                If C_A3_Cmin = C_A3_Cr Then
                    state_A3_side = " TubeSide"
                    C_QmaxA3 = C_A3_Cmin * (C_Ts - C_Tain)
                ElseIf C_A3_Cmin = C_A3_CA Then
                    state_A3_side = " AirSide"
                    C_QmaxA3 = C_A3_Cmin * (C_Ts - C_Tain) * C_A3_star
                End If

                C_QA3 = C_mr * C_A3_CpL * (C_Ts - C_Tout)
                C_EA3 = C_QA3 / C_QmaxA3

                If C_EA3 > 1 Then
                    L = C_A3_star
                    C_A3_star = (L + R) / 2
                ElseIf C_EA3 < 1 Then
                    C_CA3_Star = C_A3_Cr / C_CA3

                    ' Add P152 mathods in ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


                    Dim Offset, dNTU, NTU_guess, EA3_dNTU, K, C_EA3_o As Double
                    dNTU = 0.0001
                    Offset = C_EA3
                    NTU_guess = 1

                    If state_A3_side = " AirSide" Then
                        If CDbl(TextBox21.Text) = 1 Then
                            While (1)

                                C_EA3_o = (1 / C_CA3_Star) * (1 - Math.Exp(-C_CA3_Star * (1 - Math.Exp(-NTU_guess)))) - Offset
                                EA3_dNTU = (1 / C_CA3_Star) * (1 - Math.Exp(-C_CA3_Star * (1 - Math.Exp(-(NTU_guess + dNTU))))) - Offset

                                C_A3_NTU = NTU_guess
                                NTU_guess = NTU_guess - C_EA3_o / ((EA3_dNTU - C_EA3_o) / dNTU)

                                If (Math.Abs(C_A3_NTU - NTU_guess) < 0.0001) Then Exit While
                            End While
                        ElseIf CDbl(TextBox21.Text) = 2 Then
                            While (1)
                                K = 1 - Math.Exp(-NTU_guess / 2)
                                C_EA3_o = (1 / C_CA3_Star) * (1 - Math.Exp(-2 * K * C_CA3_Star) * (1 + C_CA3_Star * K ^ 2)) - Offset
                                K = 1 - Math.Exp(-(NTU_guess + dNTU) / 2)
                                EA3_dNTU = (1 / C_CA3_Star) * (1 - Math.Exp(-2 * K * C_CA3_Star) * (1 + C_CA3_Star * K ^ 2)) - Offset

                                C_A3_NTU = NTU_guess
                                NTU_guess = NTU_guess - C_EA3_o / ((EA3_dNTU - C_EA3_o) / dNTU)

                                If (Math.Abs(C_A3_NTU - NTU_guess) < 0.0001) Then Exit While
                            End While
                        ElseIf CDbl(TextBox21.Text) = 3 Then
                            While (1)
                                K = 1 - Math.Exp(-NTU_guess / 3)
                                C_EA3_o = (1 / C_CA3_Star) * (1 - Math.Exp(-3 * K * C_CA3_Star) * (1 + C_CA3_Star * K ^ 2 * (3 - K) + (3 * (C_CA3_Star) ^ 2 * K ^ 4) / 2)) - Offset
                                K = 1 - Math.Exp(-(NTU_guess + dNTU) / 3)
                                EA3_dNTU = (1 / C_CA3_Star) * (1 - Math.Exp(-3 * K * C_CA3_Star) * (1 + C_CA3_Star * K ^ 2 * (3 - K) + (3 * (C_CA3_Star) ^ 2 * K ^ 4) / 2)) - Offset

                                C_A3_NTU = NTU_guess
                                NTU_guess = NTU_guess - C_EA3_o / ((EA3_dNTU - C_EA3_o) / dNTU)

                                If (Math.Abs(C_A3_NTU - NTU_guess) < 0.0001) Then Exit While
                            End While
                        ElseIf CDbl(TextBox21.Text) = 4 Then
                            While (1)
                                K = 1 - Math.Exp(-NTU_guess / 4)
                                C_EA3_o = (1 / C_CA3_Star) * (1 - Math.Exp(-4 * K * C_CA3_Star) * (1 + C_CA3_Star * K ^ 2 * (6 - 4 * K + K ^ 2) + (4 * (C_CA3_Star) ^ 2 * K ^ 4 * (2 - K)) + (8 * C_CA3_Star ^ 3 * K ^ 6) / 3)) - Offset
                                K = 1 - Math.Exp(-(NTU_guess + dNTU) / 4)
                                EA3_dNTU = (1 / C_CA3_Star) * (1 - Math.Exp(-4 * K * C_CA3_Star) * (1 + C_CA3_Star * K ^ 2 * (6 - 4 * K + K ^ 2) + (4 * (C_CA3_Star) ^ 2 * K ^ 4 * (2 - K)) + (8 * C_CA3_Star ^ 3 * K ^ 6) / 3)) - Offset

                                C_A3_NTU = NTU_guess
                                NTU_guess = NTU_guess - C_EA3_o / ((EA3_dNTU - C_EA3_o) / dNTU)

                                If (Math.Abs(C_A3_NTU - NTU_guess) < 0.0001) Then Exit While
                            End While
                        ElseIf CDbl(TextBox21.Text) > 4 Then
                            While (1)
                                C_EA3_o = 1 - Math.Exp(NTU_guess ^ 0.22 * (Math.Exp(-C_CA3_Star * NTU_guess ^ 0.78) - 1) / C_CA3_Star) - Offset
                                EA3_dNTU = 1 - Math.Exp((NTU_guess + dNTU) ^ 0.22 * (Math.Exp(-C_CA3_Star * (NTU_guess + dNTU) ^ 0.78) - 1) / C_CA3_Star) - Offset

                                C_A3_NTU = NTU_guess
                                NTU_guess = NTU_guess - C_EA3_o / ((EA3_dNTU - C_EA3_o) / dNTU)

                                If (Math.Abs(C_A3_NTU - NTU_guess) < 0.0001) Then Exit While
                            End While
                        End If

                    ElseIf state_A3_side = " TubeSide" Then
                        If CDbl(TextBox21.Text) = 1 Then
                            While (1)
                                C_EA3_o = 1 - Math.Exp(-(1 - Math.Exp(-NTU_guess * C_CA3_Star)) / C_CA3_Star) - Offset
                                EA3_dNTU = 1 - Math.Exp(-(1 - Math.Exp(-(NTU_guess + dNTU) * C_CA3_Star)) / C_CA3_Star) - Offset

                                C_A3_NTU = NTU_guess
                                NTU_guess = NTU_guess - C_EA3_o / ((EA3_dNTU - C_EA3_o) / dNTU)

                                If (Math.Abs(C_A3_NTU - NTU_guess) < 0.0001) Then Exit While
                            End While
                        ElseIf CDbl(TextBox21.Text) = 2 Then
                            While (1)
                                K = 1 - Math.Exp(-NTU_guess * C_CA3_Star / 2)
                                C_EA3_o = 1 - Math.Exp(-2 * K / C_CA3_Star) * (1 + K ^ 2 / C_CA3_Star) - Offset
                                K = 1 - Math.Exp(-(NTU_guess + dNTU) * C_CA3_Star / 2)
                                EA3_dNTU = 1 - Math.Exp(-2 * K / C_CA3_Star) * (1 + K ^ 2 / C_CA3_Star) - Offset

                                C_A3_NTU = NTU_guess
                                NTU_guess = NTU_guess - C_EA3_o / ((EA3_dNTU - C_EA3_o) / dNTU)

                                If (Math.Abs(C_A3_NTU - NTU_guess) < 0.0001) Then Exit While
                            End While
                        ElseIf CDbl(TextBox21.Text) = 3 Then
                            While (1)
                                K = 1 - Math.Exp(-NTU_guess * C_CA3_Star / 3)
                                C_EA3_o = 1 - Math.Exp(-3 * K / C_CA3_Star) * (1 + K ^ 2 * (3 - K) / C_CA3_Star + (3 * K ^ 4) / (2 * C_CA3_Star ^ 2)) - Offset
                                K = 1 - Math.Exp(-(NTU_guess + dNTU) * C_CA3_Star / 3)
                                EA3_dNTU = 1 - Math.Exp(-3 * K / C_CA3_Star) * (1 + K ^ 2 * (3 - K) / C_CA3_Star + (3 * K ^ 4) / (2 * C_CA3_Star ^ 2)) - Offset

                                C_A3_NTU = NTU_guess
                                NTU_guess = NTU_guess - C_EA3_o / ((EA3_dNTU - C_EA3_o) / dNTU)

                                If (Math.Abs(C_A3_NTU - NTU_guess) < 0.0001) Then Exit While
                            End While
                        ElseIf CDbl(TextBox21.Text) = 4 Then
                            While (1)
                                K = 1 - Math.Exp(-NTU_guess * C_CA3_Star / 4)
                                C_EA3_o = 1 - Math.Exp(-4 * K / C_CA3_Star) * (1 + K ^ 2 * (6 - 4 * K + K ^ 2) / C_CA3_Star + (4 * K ^ 4 * (2 - K)) / (C_CA3_Star ^ 2) + 8 * K ^ 6 / (3 * C_CA3_Star ^ 3)) - Offset
                                K = 1 - Math.Exp(-(NTU_guess + dNTU) * C_CA3_Star / 4)
                                EA3_dNTU = 1 - Math.Exp(-4 * K / C_CA3_Star) * (1 + K ^ 2 * (6 - 4 * K + K ^ 2) / C_CA3_Star + (4 * K ^ 4 * (2 - K)) / (C_CA3_Star ^ 2) + 8 * K ^ 6 / (3 * C_CA3_Star ^ 3)) - Offset

                                C_A3_NTU = NTU_guess
                                NTU_guess = NTU_guess - C_EA3_o / ((EA3_dNTU - C_EA3_o) / dNTU)

                                If (Math.Abs(C_A3_NTU - NTU_guess) < 0.0001) Then Exit While
                            End While
                        ElseIf CDbl(TextBox21.Text) > 4 Then
                            While (1)
                                C_EA3_o = 1 - Math.Exp(NTU_guess ^ 0.22 * (Math.Exp(-C_CA3_Star * NTU_guess ^ 0.78) - 1) / C_CA3_Star) - Offset
                                EA3_dNTU = 1 - Math.Exp((NTU_guess + dNTU) ^ 0.22 * (Math.Exp(-C_CA3_Star * (NTU_guess + dNTU) ^ 0.78) - 1) / C_CA3_Star) - Offset

                                C_A3_NTU = NTU_guess
                                NTU_guess = NTU_guess - C_EA3_o / ((EA3_dNTU - C_EA3_o) / dNTU)

                                If (Math.Abs(C_A3_NTU - NTU_guess) < 0.0001) Then Exit While
                            End While
                        End If
                    End If
                    ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                    C_A3_UA_NTU = C_A3_NTU * C_A3_Cmin
                    C_A3_UA_HR = (1 / (C_noho * C_A3) + 1 / (C_A3_hi * C_A3i)) ^ -1

                    ' check system running out
                    If (C_Tout - C_Tain) < 0.00000001 Then
                        C_Tout = C_Tain
                        Exit While
                    End If

                    If (C_Ts - C_Tout) < 0.00000001 Then
                        C_Tout = C_Tout
                        Exit While
                    End If

                    ' Bisection of A3
                    If (C_A3_UA_NTU - C_A3_UA_HR) > 0.1 Then
                        L = C_Tout
                        C_Tout = (L + R) / 2
                    ElseIf (C_A3_UA_HR - C_A3_UA_NTU) > 0.1 Then
                        R = C_Tout
                        C_Tout = (L + R) / 2
                    End If

                    If Math.Abs(C_A3_UA_NTU - C_A3_UA_HR) < 0.1 Then Exit While
                End If
            End While

        End If

        'Rest of the cycle

        'Dim i_state3 As Double

        If State_A2 = 2 Then
            ' Subcooled Region
            'MsgBox("subcooled")
            T2_3.Text = CtoK(C_Tout)
            i_state3 = Enthalpy("r22", "TP", "SI", CtoK(T2_3.Text), C_sat.P)
        Else
            ' Saturation Region
            'MsgBox("saturation")
            T2_3.Text = CtoK(C_Ts)
            i_state3 = Enthalpy("r22", "TP", "SI", CtoK(T2_3.Text), C_sat.P)
        End If

        ' Test

        'T3_4.Text = TextBox18.Text
        'Dim E_sat As New Fluid("r22", "si", "tp")
        'E_sat.SatProp(CtoK(TextBox18.Text))
        'P3_4.Text = E_sat.P
        'P4_1.Text = E_sat.P

        'S_initial = Entropy("r22", "TP", "SI", CtoK(T1_2.Text), C_sat.P)
        'T4_1.Text = KtoC(Temperature("r22", "PS", "SI", E_sat.P, S_initial))


    End Sub

    Sub ReverseExpa()

        '[1] initialization capillary tube
        Dim Pin, mr, xin, Tpre, L, D, e, N, dL, A, G, xout As Double
        Pin = CDbl(EPsat.Text)
        mr = CDbl(MFR.Text)
        xin = CDbl(TextBox10.Text)
        Tpre = CDbl(ETsat.Text)

        L = CDbl(TextBox13.Text)
        D = CDbl(TextBox12.Text)
        e = CDbl(TextBox9.Text)
        N = 10 '100

        dL = L / N
        A = Math.PI * 0.25 * (D ^ 2)
        G = mr / A

        '[2] Guess CTsat(CPsat)  => A2 A3 region

        ' start state 
        Dim hi As Double
        Dim InitialState As New Fluid(GlobalVar.Influid, "SI", "tp")
        InitialState.SatProp(CDbl(ETsat.Text))
        hi = (InitialState.iG - InitialState.iL) * xin + InitialState.iL




        ' guess end state
        Dim EndState As New Fluid(GlobalVar.Influid, "SI", "tp")
        EndState.SatProp(CDbl(CTsat.Text))

        Dim CapillaryEnd As String

        If hi > EndState.iL Then
            CapillaryEnd = "A2"
            xout = (hi - EndState.iL) / (EndState.ifg)
            TextBox17.Text = xout



        ElseIf hi < EndState.iL Then
            CapillaryEnd = "A3"
            xout = 0
            TextBox17.Text = xout



        End If



    End Sub

    Private Sub mnuOpenFile_Click(sender As Object, e As EventArgs) Handles mnuOpenFile.Click

        clear()

        Dim st As String
        Dim st1, st2, st3, st4, st5 As String
        Dim TB36, TB31, T41, QA4, QA5, QE, ET_sat, EP_sat, T34, TB10 As String
        Dim M_F_R, TB11, TB18 As String
        Dim T12, QA1, QA2, QA3, TB16, CP_sat, T23, TB17 As String


        dlgOpen.Filter = "文書檔 (*.txt)|*.txt|所有檔案 (*.*)|*.*"  ' 檔案類型清單
        dlgOpen.FilterIndex = 1           ' 檔案類型的預設值為清單中的第一個項目
        dlgOpen.RestoreDirectory = True   ' 指定開檔對話方塊為上一次作業
        ' 的資料夾路徑
        dlgOpen.DefaultExt = ".txt"       ' 指定開檔對話方塊預設開啟檔案的副檔名為「.txt」
        ' 檢查是否按 <確定> 鈕
        If dlgOpen.ShowDialog() = DialogResult.OK Then
            fileName = dlgOpen.FileName    ' 將開啟的檔名指定給fileName 變數
            fileNum = FreeFile()           ' 指定檔案代號以供程式中使用
            FileOpen(fileNum, fileName, OpenMode.Input) '開啟指定檔名為讀取模式

            'txtNote.Text = ""              ' 將表單上txtNote文字方塊控制項清成空白
            TextBox1.Text = ""
            TextBox2.Text = ""
            TextBox3.Text = ""
            TextBox4.Text = ""

            Do While Not EOF(fileNum)      ' 檔案指標是否指到檔案結束符號處
                'Input(fileNum, st)         ' 若未碰到EOF，將檔案指標所指的資料讀入存入st字串中
                'Input(fileNum, st1)
                'Input(fileNum, st2)
                'Input(fileNum, st3)
                'Input(fileNum, st4)
                Input(fileNum, st5)
                Input(fileNum, TB36)
                Input(fileNum, TB31)
                Input(fileNum, T41)
                Input(fileNum, QA4)
                Input(fileNum, QA5)
                Input(fileNum, QE)
                Input(fileNum, ET_sat)
                Input(fileNum, EP_sat)
                Input(fileNum, T34)
                Input(fileNum, TB10)
                Input(fileNum, M_F_R)
                Input(fileNum, TB11)
                Input(fileNum, TB18)
                Input(fileNum, T12)
                Input(fileNum, QA1)
                Input(fileNum, QA2)
                Input(fileNum, QA3)
                Input(fileNum, TB16)
                Input(fileNum, CP_sat)
                Input(fileNum, T23)
                Input(fileNum, TB17)

                'txtNote.Text = txtNote.Text + st ' 將st內的資料放在文字方塊目前插入點游標後面
                'TextBox1.Text = TextBox1.Text + st1
                'TextBox2.Text = TextBox2.Text + st2
                'TextBox3.Text = TextBox3.Text + st3
                'TextBox4.Text = TextBox4.Text + st4
                CTsat.Text = CTsat.Text + st5
                TextBox36.Text = TextBox36.Text + TB36
                TextBox31.Text = TextBox31.Text + TB31
                T4_1.Text = T4_1.Text + T41
                QA4txt.Text = QA4txt.Text + QA4
                QA5txt.Text = QA5txt.Text + QA5
                QEtxt.Text = QEtxt.Text + QE
                ETsat.Text = ETsat.Text + ET_sat
                EPsat.Text = EPsat.Text + EP_sat
                T3_4.Text = T3_4.Text + T34
                TextBox10.Text = TextBox10.Text + TB10
                MFR.Text = MFR.Text + M_F_R
                TextBox11.Text = TextBox11.Text + TB11
                TextBox18.Text = TextBox18.Text + TB18
                T1_2.Text = T1_2.Text + T12
                QA1txt.Text = QA1txt.Text + QA1
                QA2txt.Text = QA2txt.Text + QA2
                QA3txt.Text = QA3txt.Text + QA3
                TextBox16.Text = TextBox16.Text + TB16
                CPsat.Text = CPsat.Text + CP_sat
                T2_3.Text = T2_3.Text + T23
                TextBox17.Text = TextBox17.Text + TB17

            Loop
            Me.Text = fileName & "-記事本"
            FileClose(fileNum)              ' 關閉檔案
        End If
    End Sub

    Private Sub mnuSave_Click(sender As Object, e As EventArgs) Handles mnuSave.Click
        If fileName = "" Then
            Call mnuSaveAs_Click(sender, e)  ' 執行mnuAsSave_Click事件程序
            Return
        End If
        fileNum = FreeFile()
        FileOpen(fileNum, fileName, OpenMode.Output)

        'Write(fileNum, txtNote.Text)
        'Write(fileNum, TextBox1.Text)
        'Write(fileNum, TextBox2.Text)
        'Write(fileNum, TextBox3.Text)
        'Write(fileNum, TextBox4.Text)
        Write(fileNum, CTsat.Text)
        Write(fileNum, TextBox36.Text)
        Write(fileNum, TextBox31.Text)
        Write(fileNum, T4_1.Text)
        Write(fileNum, QA4txt.Text)
        Write(fileNum, QA5txt.Text)
        Write(fileNum, QEtxt.Text)
        Write(fileNum, ETsat.Text)
        Write(fileNum, EPsat.Text)
        Write(fileNum, T3_4.Text)
        Write(fileNum, TextBox10.Text)
        Write(fileNum, MFR.Text)
        Write(fileNum, TextBox11.Text)
        Write(fileNum, TextBox18.Text)
        Write(fileNum, T1_2.Text)
        Write(fileNum, QA1txt.Text)
        Write(fileNum, QA2txt.Text)
        Write(fileNum, QA3txt.Text)
        Write(fileNum, TextBox16.Text)
        Write(fileNum, CPsat.Text)
        Write(fileNum, T2_3.Text)
        Write(fileNum, TextBox17.Text)

        FileClose(fileNum)
    End Sub

    Private Sub mnuSaveAs_Click(sender As Object, e As EventArgs) Handles mnuSaveAs.Click
        dlgSave.Filter = "文書檔 (*.txt)|*.txt|所有檔案 (*.*)|*.*"
        dlgSave.FilterIndex = 1
        dlgSave.RestoreDirectory = True
        dlgSave.DefaultExt = ".txt"
        If dlgSave.ShowDialog() = DialogResult.OK Then
            fileName = dlgSave.FileName
            fileNum = FreeFile()
            FileOpen(fileNum, fileName, OpenMode.Output)

            'Write(fileNum, txtNote.Text)
            'Write(fileNum, TextBox1.Text)
            'Write(fileNum, TextBox2.Text)
            'Write(fileNum, TextBox3.Text)
            'Write(fileNum, TextBox4.Text)
            Write(fileNum, CTsat.Text)
            Write(fileNum, TextBox36.Text)
            Write(fileNum, TextBox31.Text)
            Write(fileNum, T4_1.Text)
            Write(fileNum, QA4txt.Text)
            Write(fileNum, QA5txt.Text)
            Write(fileNum, QEtxt.Text)
            Write(fileNum, ETsat.Text)
            Write(fileNum, ETsat.Text)
            Write(fileNum, T3_4.Text)
            Write(fileNum, TextBox10.Text)
            Write(fileNum, MFR.Text)
            Write(fileNum, TextBox11.Text)
            Write(fileNum, TextBox18.Text)
            Write(fileNum, T1_2.Text)
            Write(fileNum, QA1txt.Text)
            Write(fileNum, QA2txt.Text)
            Write(fileNum, QA3txt.Text)
            Write(fileNum, TextBox16.Text)
            Write(fileNum, CPsat.Text)
            Write(fileNum, T2_3.Text)
            Write(fileNum, TextBox17.Text)

            FileClose(fileNum)
            Me.Text = fileName & "-記事本"
        End If
    End Sub

    Private Sub mnuEnd_Click(sender As Object, e As EventArgs) Handles mnuEnd.Click
        Application.Exit()   ' 結束程式
    End Sub

    Sub clear()
        CTsat.Clear()
        TextBox36.Clear()
        TextBox31.Clear()
        T4_1.Clear()
        QA4txt.Clear()
        QA5txt.Clear()
        QEtxt.Clear()
        ETsat.Clear()
        EPsat.Clear()
        T3_4.Clear()
        TextBox10.Clear()
        MFR.Clear()
        TextBox11.Clear()
        TextBox18.Clear()
        T1_2.Clear()
        QA1txt.Clear()
        QA2txt.Clear()
        QA3txt.Clear()
        TextBox16.Clear()
        CPsat.Clear()
        T2_3.Clear()
        TextBox17.Clear()
    End Sub


End Class
