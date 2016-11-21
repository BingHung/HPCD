Module Expa

    Public Sub Expa()


        Dim L, Pin, D As Double
        Dim N, A, mr, dL, G, e As Double
        Dim Pout, dv, vout, vin, ftp, fsp, phi_tp, xin, xout, x, Pout_guess, h As Double
        Dim Resp, Retp As Double
        Dim Atp, Asp, Btp, Bsp As Double
        Dim Tin, Tout, Tpre As Double

        Pin = CDbl(Form1.CPsat.Text)
        mr = CDbl(Form1.MFR.Text)
        x = CDbl(Form1.TextBox17.Text)
        Tpre = CDbl(Form1.CTsat.Text) ' Tcond.sat for A2 expand '(K)

        L = CDbl(Form1.TextBox13.Text)
        D = CDbl(Form1.TextBox12.Text)
        e = CDbl(Form1.TextBox9.Text)
        N = 10 '100

        dL = L / N
        A = Math.PI * 0.25 * (D ^ 2)
        G = mr / A



        If State_A2 = 2 Then
            ' CTS Condensor A3 => calculate subcooled => two phase

            'XXXXXXXXXXXXXXXXXXXXXXXXXXXX   Subcooled Capillary XXXXXXXXXXXXXXXXXXXXXXXXXXXX

            Dim TA3 As Double
            TA3 = (CDbl((Form1.T2_3.Text))) '(K)

            Dim Tx, xzero, Px As Double
            Dim zeroL, zeroR As Double
            zeroL = CDbl(Form1.ETsat.Text)  '273.15 'CDbl(ETsat.Text)  '10 'T state1 guess (Low Pressure Saturation)
            zeroR = TA3  ' T initial state temperature
            Tx = (zeroL + zeroR) / 2

            Dim InitialState As New Fluid(Influid, "SI", "tp")
            InitialState.Properties(TA3, Pin)

            While (1)

                'Guess for state x = 0 

                Dim StateZero As New Fluid(Influid, "SI", "tp")
                StateZero.SatProp(Tx)

                xzero = (InitialState.i - StateZero.iL) / (StateZero.ifg)
                'MsgBox(xzero)

                If xzero < -0.001 Then
                    zeroR = Tx
                    Tx = (zeroL + zeroR) / 2
                ElseIf xzero > 0.001 Then
                    zeroL = Tx
                    Tx = (zeroL + zeroR) / 2
                End If

                'If Math.Abs(xzero) < 0.0001 Then Exit While
                If xzero > 0 And xzero < 0.001 Then Exit While
            End While
            'MsgBox(xzero)
            'MsgBox(Tx)

            ' Define stateZero , calaulte the tube length of one phase 
            ' Px = Pressure(Influid, "TS", "SI", CtoK(Tx), (InitialState.i / 1000))
            Px = Pressure(Influid, "Tliq", "SI", Tx)

            Dim dPzero, Re, u, f, Ls1, Ls2 As Double
            dPzero = Pin - Px

            u = mr / A / InitialState.rho
            Re = InitialState.rho * u * A / InitialState.Visc
            f = 0.0791 * Re ^ -0.25
            Ls1 = 2 * dPzero * D / (f * 4 * InitialState.rho * u ^ 2)
            Ls2 = L - Ls1

            'MsgBox(Ls1)
            ' what i want to get are the 'Ls2' and ''the new initial state' 

            'XXXXXXXXXXXXXXXXXXXXXXXXXXXX   Two Phase Capillary XXXXXXXXXXXXXXXXXXXXXXXXXXXX

            Dim previous As New Fluid(Influid, "SI", "tp")
            previous.SatProp(Tx)

            x = 0
            h = (previous.iG - previous.iL) * x + previous.iL

            'Dim E_sat As New Fluid(Influid, "si", "tp")
            'E_sat.SatProp(CtoK(ETsat.Text))

            Dim Lp, Rp, count As Double
            Lp = 0.101325 'E_sat.P '2
            Rp = Px 'CDbl(P2_3.Text) '2.13
            Pout = (Lp + Rp) / 2
            count = 1

            Pin = Px ' Re_state IC
            Tin = Tx ' Re_state IC

            Dim Cnum As Double
            'Stop
            While (count < N)

                While (1)

                    '***********************************************************************************************************************
                    'Dim start As New Fluid("r22", "SI", "PH")
                    'start.CapillaryProp(Pin, previous.i / 1000)
                    Tin = Temperature(Influid, "PLIQ", "SI", Pin)

                    Dim start As New Fluid(Influid, "SI", "tp")
                    start.SatProp(Tin)

                    'Dim out As New Fluid("r22", "SI", "PH")
                    'out.CapillaryProp(Pout, previous.i / 1000)
                    Tout = Temperature(Influid, "PLIQ", "SI", Pout)

                    Dim out As New Fluid(Influid, "SI", "tp")
                    out.SatProp(Tout)
                    '***********************************************************************************************************************
                    xin = (h - start.iL) / start.ifg
                    xout = (h - out.iL) / out.ifg
                    vin = 1 / ((1 - xin) * (1 / start.rhoL) + xin * (1 / start.rhoG))
                    vout = 1 / ((1 - xout) * (1 / out.rhoL) + xout * (1 / out.rhoG))
                    dv = vout - vin 'vin - vout 'Math.Abs(vout - vin)


                    Resp = G * D / start.viscL
                    fsp = 0.0791 * Resp ^ (-0.25)

                    Retp = G * D / ((1 - xin) / start.viscL + xin / start.viscG) ^ -1

                    Bsp = 37530 / Resp
                    Btp = 37530 / Retp
                    Asp = 2.457 * Math.Log(1 / ((7 / Resp) ^ 0.9 + 0.27 * e / D))
                    Atp = 2.457 * Math.Log(1 / ((7 / Retp) ^ 0.9 + 0.27 * e / D))


                    phi_tp = (((8 / Retp) ^ 12 + (Atp ^ 16 + Btp ^ 16) ^ (-3 / 2)) / ((8 / Resp) ^ 12 + (Asp ^ 16 + Bsp ^ 16) ^ (-3 / 2))) ^ (1 / 12) * (1 + xin * ((1 / start.rhoG) / (1 / start.rhoL) - 1))

                    ftp = phi_tp * fsp * ((1 / start.rhoL) / (1 / vin))

                    Pout_guess = -(G ^ 2 * (ftp * (vin / 2 / D) + (dv / dL))) * dL / (10 ^ 6) + Pin

                    Cnum = Cnum + 1
                    If Cnum > 300 Then
                        MsgBox("choke A3")
                    End If


                    If (Pout_guess - Pout) > 0.001 Then
                        Lp = Pout
                        Pout = (Lp + Rp) / 2
                    ElseIf (Pout - Pout_guess) > 0.001 Then
                        Rp = Pout
                        Pout = (Lp + Rp) / 2
                    End If

                    If Math.Abs(Pout_guess - Pout) < 0.001 Then Exit While

                End While
                ' MsgBox(Pout_guess)
                ' MsgBox(count)
                Pin = Pout_guess
                Lp = 0
                Rp = Pout_guess
                Pout = (Lp + Rp) / 2

                count = count + 1

            End While
            'MsgBox("from A3")
            '/****************************************************************************
            '** A performance Comparison Between Coiled and Straight Capillary Tubes Modification
            '**(mcoiled/mstraight) = 2.011(Dc/L)^0.0527*(di/L)^0.094

            '** di:         inside diameter of capillary tube (m)
            '** Dc:         coiled diameter(m)
            '** L = length Of the capillary tube
            '/****************************************************************************
            Dim Dc As Double = CDbl(Form1.TextBox42.Text) '(m)
            'MsgBox(Pout_guess)
            Pout_guess = Pout_guess * (2.011 * (Dc / L) ^ 0.0527 * (D / L) ^ 0.094)

            'MsgBox(vout)
            'MsgBox(count)
            'MsgBox(Pout_guess)
            Form1.EPsat.Text = Pout_guess.ToString("0.###")
            Form1.TextBox10.Text = xout.ToString("0.###")



        ElseIf State_A2 = 1 Then
            ' CTS Condensor A2 => two phase only

            Dim previous As New Fluid(Influid, "SI", "tp")
            previous.SatProp(Tpre)
            h = (previous.iG - previous.iL) * x + previous.iL

            'Dim E_sat As New Fluid(Influid, "si", "tp")
            ' E_sat.SatProp(CtoK(ETsat.Text))

            Dim Lp, Rp, count, cplus As Double
            Lp = 0.101325 'E_sat.P '2
            Rp = CDbl(Form1.CPsat.Text) '2.13
            Pout = (Lp + Rp) / 2
            count = 1

            cplus = 0
            While (count < N)

                While (1)

                    '***********************************************************************************************************************
                    'Dim start As New Fluid("r22", "SI", "PH")
                    'start.CapillaryProp(Pin, previous.i / 1000)
                    Tin = Temperature(Influid, "PLIQ", "SI", Pin)

                    Dim start As New Fluid(Influid, "SI", "tp")
                    start.SatProp(Tin)

                    'Dim out As New Fluid("r22", "SI", "PH")
                    'out.CapillaryProp(Pout, previous.i / 1000)
                    Tout = Temperature(Influid, "PLIQ", "SI", Pout)

                    Dim out As New Fluid(Influid, "SI", "tp")
                    out.SatProp(Tout)
                    '***********************************************************************************************************************
                    xin = (h - start.iL) / start.ifg
                    xout = (h - out.iL) / out.ifg
                    vin = 1 / ((1 - xin) * (1 / start.rhoL) + xin * (1 / start.rhoG))
                    vout = 1 / ((1 - xout) * (1 / out.rhoL) + xout * (1 / out.rhoG))
                    dv = vout - vin 'vin - vout 'Math.Abs(vout - vin)


                    Resp = G * D / start.viscL
                    fsp = 0.0791 * Resp ^ (-0.25)

                    Retp = G * D / ((1 - xin) / start.viscL + xin / start.viscG) ^ -1

                    Bsp = 37530 / Resp
                    Btp = 37530 / Retp
                    Asp = 2.457 * Math.Log(1 / ((7 / Resp) ^ 0.9 + 0.27 * e / D))
                    Atp = 2.457 * Math.Log(1 / ((7 / Retp) ^ 0.9 + 0.27 * e / D))


                    phi_tp = (((8 / Retp) ^ 12 + (Atp ^ 16 + Btp ^ 16) ^ (-3 / 2)) / ((8 / Resp) ^ 12 + (Asp ^ 16 + Bsp ^ 16) ^ (-3 / 2))) ^ (1 / 12) * (1 + xin * ((1 / start.rhoG) / (1 / start.rhoL) - 1))

                    ftp = phi_tp * fsp * ((1 / start.rhoL) / (1 / vin))

                    Pout_guess = -(G ^ 2 * (ftp * (vin / 2 / D) + (dv / dL))) * dL / (10 ^ 6) + Pin


                    If (Pout_guess - Pout) > 0.01 Then
                        Lp = Pout
                        Pout = (Lp + Rp) / 2
                    ElseIf (Pout - Pout_guess) > 0.01 Then
                        Rp = Pout
                        Pout = (Lp + Rp) / 2
                    End If

                    cplus = cplus + 1


                    If Math.Abs(Pout_guess - Pout) < 0.01 Then Exit While


                End While
                ' MsgBox(Pout_guess)
                ' MsgBox(count)
                Pin = Pout_guess
                Lp = 0
                Rp = Pout_guess
                Pout = (Lp + Rp) / 2

                ' If Math.Abs(Pout - 0.101325) < 0.1 Then
                'MsgBox("Error Capillary")
                'End If


                count = count + 1

            End While
            'MsgBox("from A2")
            'MsgBox(vout)
            'MsgBox(count)
            '/****************************************************************************
            '** A performance Comparison Between Coiled and Straight Capillary Tubes Modification
            '**(mcoiled/mstraight) = 2.011(Dc/L)^0.0527*(di/L)^0.094

            '** di:         inside diameter of capillary tube (m)
            '** Dc:         coiled diameter(m)
            '** L = length Of the capillary tube
            '/****************************************************************************
            Dim Dc As Double = CDbl(Form1.TextBox42.Text) '(m)

            Pout_guess = Pout_guess * (2.011 * (Dc / L) ^ 0.0527 * (D / L) ^ 0.094)

            ' MsgBox(Pout_guess)
            Form1.EPsat.Text = Pout_guess.ToString("0.###")
            Form1.TextBox10.Text = xout.ToString("0.###")

        End If


    End Sub

End Module
