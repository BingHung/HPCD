
Public Class Fluid
    Public name, units, inputCode As String
    ' Public info As String()
    Public Tcrit, Pcrit, rhoCrit, Ttriple, x, rho, Visc, k, Pr, cp, cv, i As Double
    Public iL, iG, ism, ifg, CpL, CpG, rhoL, rhoG, rhom, viscL, viscG, viscm, kL, kG, km, PrL, PrG, Prm, SurfTL, P As Double 'Sat Prop


    Public Sub New(ByVal name As String, ByVal units As String, ByVal inputCode As String)
        '   Set the property Value
        Me.name = name
        Me.inputCode = inputCode
        Me.units = units
   
        ' Me.info = {[name], [inputCode], [units]}
        'Me.info = String.Concat("""", name, ", ", inputCode, ", ", units, ", ")

        ' CritProp(name)
    End Sub
    Sub CritProp()
        '------------------PROPERTIES
        '--Critical Properties
        Me.Tcrit = Temperature(name, "crit", "SI")
        Me.Pcrit = Pressure(name, "crit", "SI")
        Me.rhoCrit = Density(name, "crit", "SI")
        Me.Ttriple = Temperature(name, "Trip", "SI")
    End Sub
    Sub Properties(ByVal T As Double, ByVal P As Double)
        ' Comment?
        Me.i = Enthalpy(name, "TP", "SI", T, P) * 1000 ' {J/kg]
        Me.Visc = Viscosity(name, "TP", "SI", T, P) / 1000000 '[Pa*s]
        Me.k = ThermalConductivity(name, "TP", "SI", T, P) / 1000 '[W/m*K]
        Me.cp = IsobaricHeatCapacity(name, "TP", "SI", T, P) * 1000 '[J/kg-K]
        Me.cv = IsochoricHeatCapacity(name, "TP", "SI", T, P) * 1000 '[J/kg-K]
        Me.rho = Density(name, "TP", "SI", T, P)   ' [kg/m^3]
        Me.Pr = Prandtl(name, "TP", "SI", T, P) ' [dimensionless]
    End Sub
    Function Psat(ByVal Tsat As Double)
        Return Pressure(name, "Tliq", "SI", Tsat)
    End Function


    Sub SatProp(ByVal Tsat As Double) 'receives saturarion temperature  - returns saturation properties
        iL = Enthalpy(name, "Tliq", "SI", Tsat) * 1000              '[J/kg]
        iG = Enthalpy(name, "Tvap", "SI", Tsat) * 1000              '[J/kg]
        ifg = iG - iL
        ism = (iG + iL) / 2                                         ''[J/kg] Mean saturation enthalpy
        CpL = IsobaricHeatCapacity(name, "Tliq", "SI", Tsat) * 1000 '[J/kg-K]
        CpG = IsobaricHeatCapacity(name, "Tvap", "SI", Tsat) * 1000 '[J/kg-K]
        rhoL = Density(name, "Tliq", "SI", Tsat)                    '[kg/m^3]
        rhoG = Density(name, "Tvap", "SI", Tsat)                    '[kg/m^3]
        rhom = (rhoL + rhoG) / 2
        viscL = Viscosity(name, "Tliq", "SI", Tsat) / 1000000       '[Pa*s]
        viscG = Viscosity(name, "Tvap", "SI", Tsat) / 1000000       '[Pa*s]
        viscm = (viscL + viscG) / 2
        kL = ThermalConductivity(name, "Tliq", "SI", Tsat) / 1000   '[W/m*K]
        kG = ThermalConductivity(name, "Tvap", "SI", Tsat) / 1000    '[W/m*K]
        km = (kL + kG) / 2
        PrL = Prandtl(name, "Tliq", "SI", Tsat)                     '[dimensionless]
        PrG = Prandtl(name, "Tvap", "SI", Tsat)                     '[dimensionless]
        Prm = (PrL + PrG) / 2
        SurfTL = SurfaceTension(name, "Tliq", "SI", Tsat) / 1000      '{N/m} 
        P = Pressure(name, "Tliq", "SI", Tsat) '20160323 add
    End Sub
    Sub Sat_iG(ByVal Tsat As Double)
        iG = Enthalpy(name, "Tvap", "SI", Tsat) * 1000
    End Sub
    Function Dens(ByVal T As Double, ByVal P As Double) As Double
        Return RefProp.Density(name, "TP", "SI", T, P)
    End Function
    Function Tsat(ByVal isat As Double) As Double
        Return Temperature(name, "Hvap", "SI", isat)
    End Function

    Function xQuality(ByVal T As Double, ByVal P As Double)
        Dim x, h, hl, hg As Double  'where x is the quality and the h is the enthalphy
        h = RefProp.Enthalpy(name, "TP", "SI", T, P)
        hl = RefProp.Enthalpy(name, "Tliq", "SI", T)
        hg = RefProp.Enthalpy(name, "Tvap", "SI", T)

        If h <= hl Then
            Return 0
        ElseIf h >= hg Then
            Return 1
        Else
            x = (h - hl) / (hg - hl)
        End If


        Return x
    End Function
    Function _i(ByVal T As Double, P As Double)
        Return Enthalpy(name, "TP", "SI", T, P) * 1000 ' {J/kg]
    End Function
    Function _rho(ByVal T As Double, P As Double)
        Return Density(name, "TP", "SI", T, P)  '[kg/m³]
    End Function
End Class
